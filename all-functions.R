# program: all-functions.R
# purpose: functions to process and analyze data
# author: max rubinstein
# date modified: july 25, 2021


# load libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
library(sandwich)
library(msm)
library(gt)
library(purrr)
library(tidyverse)
library(data.table)
library(gtools)
library(usmap)
library(assertthat)

################################################################################
######################### data processing functions ############################
################################################################################

#' data_process: takes microdata and processes the variables to create an
#' analytic dataset 
#' 
#' @param orig_data unprocessed microdata
#' @param codebook microdata to full descriptive names
#' @param fullvars cross-walk for variable labels
#' @param urban_xwalk cross-walk for FIPS code region time
#'
#' @return processed dataset
data_process <- function(orig_data, codebook, fullvars, urban_xwalk) {
  data <- orig_data %>%
    mutate(reasons = case_when(
      V3 == 2 ~ V5a,
      V3 == 3 ~ V5b,
      V3 == 4 ~ V5c
    ),
    mult_resp = ifelse(stringr::str_length(D7) > 1, 1, 0), # dummy for multiple race categories
    ethnicity_race = case_when(
         D6 == 1 ~ 1, # race/ethnicity recode; hispanic versus non-hispanic [race]
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 1 ~ 2,
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 2 ~ 3,
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 3 ~ 4,
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 4 ~ 5,
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 5 ~ 6,
         (is.na(D6) | D6 == 2) & mult_resp == 0 & D7 == 6 ~ 7,
         (is.na(D6) | D6 == 2) & mult_resp == 1 ~ 8),
       ethnicity_race = ifelse(is.na(ethnicity_race), 7, ethnicity_race),
       age_cat2 = case_when(
         D2 %in% c(1, 2) ~ 1,
         D2 %in% c(3, 4, 5) ~ 2,
         D2 %in% c(6, 7) ~ 3,
         is.na(D2) ~ 199),
       D8  = case_when(D8 == 1 ~ 2, TRUE ~ as.numeric(D8)), # combine less than HS with HS graduation
       D8  = case_when(D8 == 4 ~ 3, TRUE ~ as.numeric(D8)), # combine some college with college
       D10 = case_when(D9 == 2 ~ 3, TRUE ~ as.numeric(D10)), # add not working category for people employed outside of home
       Q66 = case_when( # recode educator variable 
         Q66 %in% c(5:7) ~ 5,
         is.na(Q66) ~ NA_real_,
         TRUE ~ as.numeric(Q66)
       ),
       Q64 = case_when(Q64 == 16 ~ 30 + Q80,
                       TRUE ~ as.numeric(Q64)),
       Q64 = case_when(
         is.na(Q64) & D9 == 2 ~ 20,
         is.na(Q64) & D9 == 1 ~ 198,
         is.na(Q64) & is.na(D9) ~ 199,
         TRUE ~ as.numeric(Q64)
       ),
       Q69 = case_when(Q69 %in% c(3:5, 7:11) ~ 3, TRUE ~ as.numeric(Q69)), # recode healthcare supp cats
       Q68 = case_when(Q64 == 5 ~ Q69 + 30, TRUE ~ as.numeric(Q68)), # add hc support subcategory
       Q66 = ifelse(is.na(Q66), 199, Q66), # add other/unkn/missing job category for educators
       Q68 = ifelse(is.na(Q68), 199, Q68) # add other/unkn/missing job category for hcpractitioners
)
  
  # rename variables and recode NA values to 199
  replace_vars <- codebook$variable_name
  names(replace_vars) <- paste0("^", codebook$variable, "$")
  
  nas <- rep(list(199), length(codebook$variable_name))
  names(nas) <- codebook$variable_name
  nas <- nas[-grep("start_date", names(nas))]

  # join urban xwalk and save to disk
  final <- data %>%
    mutate(fips = as.numeric(fips)) %>%
    set_names(stringr::str_replace_all(names(.), replace_vars)) %>%
    mutate(fips.length = stringr::str_length(fips) - 4,
           state = as.numeric(stringr::str_sub(fips, start = 1, end = 1 + fips.length))) %>%
    mutate(gender = ifelse(gender == 5, 199, gender)) %>% # combine prefer not to answer with no response
    left_join(urban_xwalk, by = "fips") %>%
    replace_na(nas) %>% 
    fastDummies::dummy_cols(select_columns = c("occ", "occ_hcprac", "occ_educ", "emp", "emp_out"))
  
  return(final)
}


#' generate_indicators: generate hesitancy reason indicators
#'
#' @param data analytic dataset
#' @param value value associated with a reason for hesitancy
#'
#' @return dataframe with indicator for a specified reason
generate_indicators <- function(data, value) {
  varname <- paste0("reason_", value)
  value_string <- paste0("^", value, "$|", "\\,", value, "\\,|\\,", value, "$|^", value, "\\,")
  data %>% 
    mutate(!!varname := ifelse(grepl(value_string, reasons), 1, 0))
}

#'indicator_loop: loops through generate_indicators
#'
#' @param data analytic dataset
#' @param values list of values for hesitancy reasons
#'
#' @return dataframe with indicators for all specified hesitancy reasons
indicator_loop <- function(data, values) {
  for(value in values) {
    data <- generate_indicators(data, value)
  }
  return(data)
}

################################################################################
######################### data analysis functions ##############################
################################################################################

#' intent_by: calculates the percent for a binary outcome (typically vaccine
#' intent) within all values of a stratifying variable defined by a group 
#'
#' @param data output from data_process
#' @param fullvars cross-walk for variable names and values
#' @param group specified group to examine 
#' @param outcome specified outcome variable
#'
#' @return dataframe containing the proportion, standard errors, and sample size
#' associated with each level of a grouping variable
intent_by <- function(data, fullvars, group, outcome) {
  gvars <- fullvars %>%
    filter(variable_name == !!group) 
  
  totals <- unlist(map(gvars$new_varname, ~nrow(filter(data, !!sym(.x) == 1))))
  
  formula <- map(gvars$new_varname, ~paste0(outcome, " ~ ", .x, "-1"))
  
  models <- map(formula, ~lm(as.formula(.x), data = data, weights = data$weight))
  robust <- map(models, ~vcovHC(.x, type = "HC0"))
  mcoefs <- unlist(map(models, ~coef(.x)))
  
  navals <- is.na(mcoefs)
  mcoefs <- mcoefs[!navals]
  robust <- robust[!navals]
  labels <- gvars$new_varname[!navals]
  totals <- totals[!navals]
  
  se_est <- unlist(map(robust, ~sqrt(.x[1,1])))
  
  tibble(
    props = mcoefs,
    se = se_est,
    lci = props - 1.96*se,
    uci = props + 1.96*se,
    total = totals
  ) %>%
    set_names(paste0(outcome, "_", names(.))) %>%
    mutate(label = labels)
}

#' calc_intent: calculates the percentage of a binary outcome within a specified strata,
#' or calculates the change in the outcome between two months (outcome typically vaccine
#' intent)
#'
#' @param data unprocessed microdata
#' @param group binary variable that species a subset of the dataframe
#' @param outcome binary outcome
#' @param change boolean indicating whether to calculate an intercept or a change
#' across two months
#'
#' @return dataframe containing proportion and standard errors associated with
#' the outcome within the specified group. if change is true also returns the 
#' estimated percent change and standard error

calc_intent <- function(data, group, outcome, change = FALSE) {
  data <- data %>%
    filter(!!sym(group) == 1)
  
  if (change == FALSE) {
    formula <- paste0(outcome, " ~ 1")
  }
  
  if (change == TRUE) {
    formula <- paste0(outcome, " ~ factor(month)")
  }
  
  model <- lm(as.formula(formula), data = data, weights = data$weight)
  robust <- vcovHC(model, type = "HC0")
  
  if (change == FALSE) {
    mcoefs <- coef(model)
    se_est <- sqrt(robust[1,1])
    res <-   tibble(
      props = mcoefs,
      se = se_est,
      lci = props - 1.96*se,
      uci = props + 1.96*se
    ) %>%
      set_names(paste0(outcome, "_", names(.))) %>%
      mutate(label = group)
  }
  
  if (change == TRUE) {
    mcoefs <- coef(model)[2]
    se_est <- sqrt(robust[2,2])
    
    calc_ratio <- function(model, vcov) {
      coef1 <- coef(model)[1]; coef2 <- coef(model)[2]
      ratio.est <- coef2/coef1
      ratio.var <- ratio.est^2 * (vcov[1, 1]/(coef1^2) + vcov[2, 2] / (coef2^2) - 2 * vcov[1, 2] / (coef1 * coef2))
      c(ratio.est = ratio.est, se.est = sqrt(ratio.var))
    }
    ratios <- calc_ratio(model, robust)
    res <- tibble(
      props = c(mcoefs, ratios[1]),
      se = c(se_est, ratios[2]),
      lci = props - 1.96*se,
      uci = props + 1.96*se
    ) %>%
      set_names(paste0(outcome, "_", names(.))) %>%
      mutate(label = group, month = c(6, 7))
  }
  return(res)
}

#' iter_intent: iterates calc_intent function for a list of variables
#'
#' @param data analytic dataset
#' @param var_list list of binary variables
#' @param outcome binary outcome
#'
#' @return list of dataframes of calc_intent output
iter_intent <- function(data, var_list, outcome) {
  map(var_list, ~calc_intent(data, .x, outcome))
}

#' rr_table: creates a table of risk ratios for a given categorical variable.
#' also allows adjusted risk ratios if adjustment variables specified
#'
#' @param data analytic dataset
#' @param fullvars variable label and description cross-walk
#' @param group categorical variable for desired risk ratios
#' @param outcome binary outcome
#' @param adj_vars variables to specify in adjustment model
#' @param prefix prefix to label output variable names
#'
#' @return dataframe containing risk ratios estimated from poisson regression model 
#' as well as standard errors and CIs in datafra
rr_table <- function(data, fullvars, group, outcome, adj_vars = "", prefix = "") {
  gvars <- fullvars %>%
    filter(variable_name == !!group) %>%
    filter(reference == 0)
  
  if (group != "all") {
    
    vars <- gvars$new_varname
    
    if (length(adj_vars) > 1) {
      adj_vars <- paste0("factor(", adj_vars, ")")
      vars <- c(vars, adj_vars)
    }
    
    formula <- paste0(outcome, " ~ ", paste0(vars, collapse = "+"))
    
    rr_model <- glm(as.formula(formula), data = data, weights = data$weight, family = poisson)
    rr_cov <- vcovHC(rr_model, type = "HC0")
    
    coefs <- coef(rr_model)
    coefs.na <- coefs[!is.na(coefs)]
    fin_coefs <- coefs[2:(length(gvars$new_varname) + 1)]
    
    coef_str <- map(sprintf("~exp(x%s)", 2:(length(fin_coefs) + 1)), as.formula)
    
    se_ests <- deltamethod(coef_str, coefs.na, rr_cov)
    
    final <- tibble(
      rr = exp(fin_coefs),
      se = se_ests,
      lci = rr - 1.96*se,
      uci = rr + 1.96*se
    ) %>%
      set_names(paste0(prefix, "_", names(.)))

    final <- final %>%
      mutate(label = names(fin_coefs))
      
  }
  
  if (group == "all") {
    final <- tibble(
      rr = NA,
      se = NA,
      lci = NA,
      uci = NA
    ) %>%
      set_names(paste0(prefix, "_", names(.))) %>%
      mutate(label = "All")
  }
  final
}

#' calc_xtabs: calculate percent hesitant, very hesitant, employed outside of home, 
#' vaccinated, as well as adjusted and unadjusted risk ratios for specified categorical 
#' variables and outputs to dataframe
#'
#' @param data analytic dataset
#' @param groups categorical variables of interest
#' @param fullvars variable label and description cross-walk
#' @param adj_vars variables to include in adjustment model
#'
#' @return dataframe containing all of the above as well as standard errors and
#' 95 percecnt confidence intervals for estimates
calc_xtabs <- function(data, groups, fullvars, adj_vars) {
  data <- mutate(data, vaccinated = case_when(vaccine_ever == 1 ~ 1, 
                                  vaccine_ever == 199 ~ NA_real_,
                                  TRUE ~ 0),
                 def_not = case_when(vaccine_intent == 4 ~ 1,
                                     vaccine_intent == 199 & vaccine_ever != 1 ~ NA_real_,
                                     TRUE ~ 0))
  
  rd_table <- map(groups, ~intent_by(data, fullvars, .x, "hesitant")) 
  rd_table1 <- map(groups, ~intent_by(data, fullvars, .x, "emp_out_1")) 
  rd_table2 <- map(groups, ~intent_by(data, fullvars, .x, "vaccinated")) 
  rd_table3 <- map(groups, ~intent_by(data, fullvars, .x, "def_not")) 
  rr_table <- map(groups, ~rr_table(data, fullvars, .x, "hesitant", adj_vars = "", prefix = "u"))
  rr_table_adj <- map(groups, ~rr_table(data, fullvars, .x, "hesitant", adj_vars = adj_vars, prefix = "a"))
  
  join_res <- function(rd_table, rd_table1, rd_table2, rd_table3, rr_table, rr_table_adj) {
    res <- list()
    for (i in 1:length(rd_table)) {
      res[[i]] <- rd_table[[i]] %>%
        left_join(rd_table1[[i]], by = c("label")) %>%
        left_join(rd_table2[[i]], by = "label") %>%
        left_join(rd_table3[[i]], by = "label") %>%
        dplyr::select(-emp_out_1_total, -vaccinated_total, -def_not_total) %>%
        left_join(rr_table[[i]], by = "label") %>%
        left_join(rr_table_adj[[i]], by = "label")
    }
    return(res)
  }
  final <- join_res(rd_table, rd_table1, rd_table2, rd_table3, rr_table, rr_table_adj) %>%
    map2(groups, ~mutate(.x, variable_name = .y)) %>%
    invoke(rbind, .)
  return(final)
}

#' hestiancy_reason_estimates: estimate percent with a specified hesitancy reason
#'
#' @param data analytic dataset
#' @param outcome hesitancy reason (binary)
#'
#' @return dataframe with percent with a reason and standard error
hesitancy_reason_estimates <- function(data, outcome) {
  my_formula <- paste0(outcome, " ~ 1")
  m1 <- lm(as.formula(my_formula), weight = weight, data = data)
  se <- sqrt(vcovHC(m1, type = "HC0")[1,1])
  tibble(
    estimate = 100*coef(m1),
    se = 100*se
  )
}

#' reasons_table: create a table of hesitancy reasons labelled by
#' the subgroup analyzed
#'
#' @param data analytic dataset (typically subsetted to an occupation 
#' category of interest)
#' @param label label for dataset
#'
#' @return formatted dataframe of hesitanc reasons
reasons_table <- function(data, label) {
  num = nrow(data)
  
  map(reason_values$varname, ~hesitancy_reason_estimates(data, .x)) %>%
    invoke(rbind, .) %>%
    mutate(Reason = reason_values$description) %>%  
    mutate(l95ci = estimate - 1.96*se, u95ci = estimate + 1.96*se) %>%
    mutate(intent_no = paste0(format(round(estimate, 1), nsmall = 1), 
                              " (", format(round(l95ci, 1), nsmall = 1), ", ", 
                              format(round(u95ci, 1), nsmall = 1), ")")) %>%
    arrange(-estimate) %>%
    select(Reason, intent_no) %>%
    rename(!!paste0(label, " \n N = ", num) := intent_no)
}

#' trend_tables: create time trends for specified list of binary variables,
#' (typically occupations), including both monthly estimates and changes
#' between specified months
#'
#' @param var_list list of binary variables (typically occupations)
#' @param monthly_data dataframe nested by month
#' @param change_data dataframe containing only two months to compare change 
#' over (typically January and May)
#' @param outcome binary outcome (typically vaccine hesitancy)
#'
#' @return dataframe containing the percent within the outcome for each month and the
#' change over the same time perios
trend_tables <- function(var_list, monthly_data, change_data, outcome = "hesitant") {
  percent_changes <- map(var_list, ~calc_intent(change_data, .x, outcome, change = TRUE)) %>%
    invoke(rbind, .) 
  
  percent_by_month <- monthly_data %>%
    mutate(hesitant = map(data, ~iter_intent(.x, var_list, outcome))) %>%
    select(month, hesitant) %>%
    mutate(hesitant = map(hesitant, ~invoke(rbind, .x))) %>%
    unnest(cols = c(hesitant)) %>%
    bind_rows(percent_changes) 
  
  return(percent_by_month)
}

################################################################################
######################### table formatting functions ###########################
################################################################################

#' format_cells: formats numeric values from a table to create cells with
#' an estimate (lower CI, upper CI) with specified number of digits and
#' possibly rescaled
#'
#' @param num point estimate
#' @param lci lower confidence interval
#' @param uci upper confidence interval
#' @param digits how many digits to round to
#' @param mult multiplicative scaling of original values
#'
#' @return formatted numbers 
format_cells <- function(num, lci, uci, digits, mult = 1) {
  paste0(format(round(mult*num, digits), nsmall = digits), " (", 
         format(round(mult*lci, digits), nsmall = digits), ", ", 
         format(round(mult*uci, digits), nsmall = digits), ")")
}

#' create_main_table: create Tables 1 and 2 from occupation paper
#'
#' @param results output from calc_xtabs
#' @param category_order specified order of categorical variables
#' to appear in the table
#'
#' @return formatted gt table version of calc_xtabs output
create_main_table <- function(results, category_order) {

  add_gt_rows <- function(gttable, group, row_ids) {
    final <- gttable %>%
      tab_row_group(label = group, rows = row_ids)
    final
  } 
  
  fin_tab <- results %>%
    select(variable_name, label, N = hesitant_total, emp_out_1_props, def_not_props, 
           emp_out_1_lci, emp_out_1_uci, def_not_lci, def_not_uci,
           vaccinated_props, vaccinated_lci, vaccinated_uci,
           hesitant_props, hesitant_lci, hesitant_uci, u_rr, a_rr, u_lci, a_lci, u_uci, a_uci) %>%
    arrange(variable_name, hesitant_props) %>%
    mutate(`Work outside home` = format_cells(emp_out_1_props, emp_out_1_lci, emp_out_1_uci, 1, 100),
           Vaccinated = format_cells(vaccinated_props, vaccinated_lci, vaccinated_uci, 1, 100),
           Hesitant = format_cells(hesitant_props, hesitant_lci, hesitant_uci, 1, 100),
           `Definitely not` = format_cells(def_not_props, def_not_lci, def_not_uci, 1, 100),
           `RR (95% CI) Hesitant` = format_cells(u_rr, u_lci, u_uci, 2, 1),
           `aRR (95% CI) Hesitant` = format_cells(a_rr, a_lci, a_uci, 2, 1)) %>%
    select(-contains("props"), -ends_with("ci"), -u_rr, -a_rr) %>%
    left_join(codebook %>% select(variable_name, variable_name_full), by = "variable_name") %>%
    left_join(fullvars %>% select(variable_name, label = new_varname, label_full), by = c("variable_name", "label")) %>%
    mutate_if(is.character, ~gsub("\\( ", "\\(", .)) #%>%
#    filter(!grepl("No response, not working, or other subcategory", label_full))
  
  varnames <- fin_tab$variable_name[!is.na(fin_tab$variable_name)]
  
  if (unique(varnames)[1] %in% c("occ_hcprac", "occ_educ")) {
    fin_tab <- fin_tab %>%
      mutate(variable_name_full = case_when(
        #grepl("_2[0-9]", label) ~ "Educator",
        grepl("_3[0-9]$", label) ~ "Occupation: HC support",
        TRUE ~ variable_name_full
    ))
  }
  
  fin_tab <- fin_tab %>%
    select(-variable_name, -label) %>%
    select(Category = variable_name_full, Description = label_full, everything()) %>%
    replace_na(list(Category = "All"))
  
  gt_tbl <- fin_tab %>%
    select(-Category) %>%
    gt(rowname_col = "Description") %>%
    tab_spanner(
      label = "% (95% CI)", 
      columns = vars( "Work outside home", "Vaccinated", "Definitely not", "Hesitant")
    ) 
  
  char.list <- category_order
  row.ids <- map(char.list, ~grep(.x, fin_tab$Category))

  for (i in 1:length(char.list)) {
    gt_tbl <- add_gt_rows(gt_tbl, group = char.list[i], row_ids = row.ids[[i]])
  }
  
  gt_tbl 
}

#' format_trend_table: formats the time trends tables
#'
#' @param trend_table output from trend_tables 
#'
#' @return rearranged and formatted version for to produce gt table version
format_trend_table <- function(trend_table) {
  hes_var <- grep("props", names(trend_table), value = TRUE)
  lci_var <- grep("lci", names(trend_table), value = TRUE)
  uci_var <- grep("uci", names(trend_table), value = TRUE)
  
  trend_table %>%
    mutate(stats = format_cells(!!sym(hes_var), !!sym(lci_var), !!sym(uci_var), 1, 100)) %>%
    select(month, stats, label) %>%
    spread(month, stats) %>%
    set_names(c("label", "January", "February", "March", "April", "May",
                "Difference (May - Jan)", "Percent change")) %>%
    left_join(fullvars %>% select(new_varname, label_full), by = c("label" = "new_varname")) %>%
    select(Description = label_full, everything(), -label) %>%
    mutate_if(is.character, ~gsub("\\( ", "\\(", .)) %>%
    mutate_if(is.character, ~gsub("\\,  ", "\\, ", .)) %>%
    mutate_if(is.character, trimws) %>%
    arrange(Description) 
}

#' full_reasons_table: takes a list of hesitancy reason tables and joins them to
#' compare reasons by different occupation categories
#'
#' @param intent_table_list list of hesitancy reason tables
#'
#' @return formatted dataframe with all reasons joined
full_reasons_table <- function(intent_table_list) {
  intent.tab <- reduce(intent_table_list, left_join, by = "Reason") 
  intent.tab <- intent.tab %>%
    mutate_all(~gsub("\\( ", "\\(", .)) %>%
    mutate_all(~gsub("  ", " ", .)) %>%
    mutate_all(trimws) %>%
    mutate_at("Reason", ~factor(., levels = intent_order)) %>%
    arrange(Reason)
  
  return(intent.tab)
}

################################################################################
####################### sample summary statistics ##############################
################################################################################

#' full_flow: takes analytic dataset and returns number associated with
#' various exclusions in the data analysis
#'
#' @param data analytic dataset
#'
#' @return dataframe containing various flow statistics
full_flow <- function(data) {
  age_dat <- filter(data, age_cat %in% c(1:5))
  main_dat <- filter(age_dat, !is.na(hesitant))
  hesitant <- filter(main_dat, hesitant == 1)
  
  # initial subset
  n_full <- nrow(data)
  age_65 <- nrow(filter(data, age_cat %in% c(6, 7)))
  age_na <- sum(data$age_cat == 199)
  age_18_64 <- nrow(age_dat)
  hes_na <- sum(is.na(age_dat$hesitant))
  n_main <- nrow(main_dat)
  
  # occupation info
  occ_dat <- filter(main_dat, occ != 199)
  occ_hcdat <- filter(occ_dat, occ %in% c(2, 4, 5))
  
  occ_na <- sum(main_dat$occ == 199)
  occ_n <- nrow(occ_dat)
  
  occ_nonhc <- sum(!(occ_dat$occ %in% c(2, 4, 5)))
  occ_hcna <- sum(occ_hcdat$occ_hcprac == 199)
  occ_hc_n <- nrow(occ_hcdat) - occ_hcna
  
  # reasons info
  nonhesitant <- sum(main_dat$hesitant == 0)
  reasons_na <- sum(is.na(hesitant$reasons))
  reasons <- filter(hesitant, !is.na(reasons))
  reasons_n <- nrow(reasons)
  reasons_oth <- reasons_n - sum(reasons$emp == 1)
  reasons_fin <- sum(reasons$emp == 1)
  
  # output
  res <- tibble(
    `Responded to survey` = n_full,
    `Age 65 and over` = age_65,
    `Did not report age` = age_na,
    `Age 18-64` = age_18_64,
    `Did not report hesitancy` = hes_na,
    `Report sample` = n_main,
    `Did not report an occupation in HC or education` = occ_nonhc,
    `Did not specify profession in HC or education` = occ_hcna,
    `HC and education subgroup` = occ_hc_n,
    `Not hesitant` = nonhesitant,
    `Did not provide reasons` = reasons_na,
    `Not employed or did not report employment status` = reasons_oth,
    `Reasons for hesitancy subsample` = reasons_fin
  ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("Statistic", "N"))
  
  gt(res)
}

#' summary_flow: takes analytic dataset and returns smaller subset
#' of flow statistics compared to full_flow
#'
#' @param data analytic dataset
#'
#' @return dataframe of flow statistics
summary_flow <- function(data) {
  age_dat <- filter(data, age_cat %in% c(1:5))
  main_dat <- filter(age_dat, !is.na(hesitant))
  hesitant <- filter(main_dat, hesitant == 1)
  
  # initial subset
  n_full <- nrow(data)
  age_65 <- nrow(filter(data, age_cat %in% c(6, 7)))
  age_na <- sum(data$age_cat == 199)
  age_18_64 <- nrow(age_dat)
  hes_na <- sum(is.na(age_dat$hesitant))
  n_main <- nrow(main_dat)
  
  # employment info
  emp_dat <- filter(main_dat, emp != 199)
  emp_na <- sum(main_dat$emp == 199)
  
  # output
  res <- tibble(
    `Responded to survey` = n_full,
    `Age 65 and over` = age_65,
    `Did not report age or hesitancy` = age_na + hes_na,
    `Report sample` = n_main,
    `Did not report employment` = emp_na
  ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("Statistic", "N"))
  
  return(res)
}

#' sample_characteristics: takes analytic file and outputs sample characteristics
#'
#' @param data analytic file
#'
#' @return dataframe containing relevant sample characteristics
sample_characteristics <- function(data) {
  # age stats
  age_dat <- filter(data, age_cat != 199)
  age <- matrixStats::weightedMedian(age_dat$age_cat, age_dat$weight)
  
  # gender stats
  gender_dat <- filter(data, gender != 199)
  male   <- weighted.mean(gender_dat$gender == 1, gender_dat$weight)
  female <- weighted.mean(gender_dat$gender == 2, gender_dat$weight)
  nonb   <- weighted.mean(gender_dat$gender == 3, gender_dat$weight)
  self   <- weighted.mean(gender_dat$gender == 4, gender_dat$weight)
  
  # race stats
  race_dat <- filter(data, ethnicity_race != 7)
  white <- weighted.mean(race_dat$ethnicity_race == 6, race_dat$weight) 
  hisp  <- weighted.mean(race_dat$ethnicity_race == 1, race_dat$weight) 
  black <- weighted.mean(race_dat$ethnicity_race == 4, race_dat$weight) 
  asian <- weighted.mean(race_dat$ethnicity_race == 3, race_dat$weight) 
  natam <- weighted.mean(race_dat$ethnicity_race == 2, race_dat$weight) 
  pacis <- weighted.mean(race_dat$ethnicity_race == 5, race_dat$weight) 
  mult   <- weighted.mean(race_dat$ethnicity_race == 8, race_dat$weight) 
  
  #educ stats
  educ_dat <- filter(data, educ != 199)
  lshs <- weighted.mean(educ_dat$educ %in% c(1, 2), educ_dat$weight)
  college <- weighted.mean(educ_dat$educ %in% c(5:8), educ_dat$weight)
  
  #emp dat
  emp_dat <- filter(data, emp != 199)
  worked <- weighted.mean(emp_dat$emp == 1, emp_dat$weight)
  
  emp_out_dat <- filter(data, emp_out != 199)
  worked_outside <- weighted.mean(emp_out_dat$emp_out == 1, emp_out_dat$weight)
  
  #urban/rural
  urban_dat <- filter(data, urban_fips != 199)
  noncore <- weighted.mean(urban_dat$urban_fips %in% c(5, 6), urban_dat$weight)
  large <- weighted.mean(urban_dat$urban_fips %in% c(1, 2), urban_dat$weight)
  
  tibble(
    `Median age` = age,
    `% male` = 100*male,
    `% female` = 100*female,
    `% non-binary` = 100*nonb,
    `% self` = 100*self,
    `% white` = 100*white,
    `% hispanic` = 100*hisp,
    `% black` = 100*black,
    `% asian` = 100*asian,
    `% native american` = 100*natam,
    `% pacific islander` = 100*pacis,
    `% multiracial` = 100*mult,
    `% less than HS` = 100*lshs,
    `% college or more` = 100*college,
    `% worked for pay` = 100*worked,
    `% worked outside home` = 100*worked_outside,
    `% noncore or micropolitan` = 100*noncore,
    `% large central or fringe` = 100*large  
  ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("Variable", "Value")) %>%
    mutate_at("Value", ~round(., 2))
}
