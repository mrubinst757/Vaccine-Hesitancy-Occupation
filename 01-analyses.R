source("02_AnalyzeData/occupation-paper/all-functions.R")

# load data/aux data -----------------------------------------------------------------------
vars <- read_csv("../00_RawData/codebook/discrete-vars.csv") %>%
  select(groups = variable_name, value, label_full) %>%
  mutate_at("value", as.character)

codebook <- read_csv("../00_RawData/codebook/codebook.csv") 

fullvars <- read_csv("../00_RawData/codebook/discrete-vars.csv") 

reason_values <- read_csv("../00_RawData/codebook/hesitancy-reasons.csv") %>%
  mutate(varname = paste0("reason_", value))

adj_vars <- c("age_cat", "gender", "urban_fips", "ethnicity_race", "educ")

all_proc <- readRDS("../00_RawData/processed-data/jan-may-microdata-proc.rds") %>%
  mutate(all = 1) %>%
  filter(age_cat %in% c(1:5), !is.na(hesitant))

out_dir <- "C:/Users/mdrub/Box/Vaccine-Intent-Project/occupation-paper"

# specify relevant data subsets from full data -------------------------------------------

# primary dataset: April 20 - May 19
may <- all_proc %>%
  filter(as.Date(start_date) >= "2021-04-20", as.Date(start_date) < "2021-05-20")

# reasons dataset (adding reasons indicators)
reason.dat <- may %>%
  indicator_loop(reason_values$value) %>%
  filter(hesitant == 1, !is.na(reasons), emp == 1)

# all data nested by month
trend_data <- all_proc %>%
  mutate(month = month(as.Date(start_date))) %>%
  nest(-month)

# only january and may data 
change_data <- trend_data %>%
  filter(month %in% c(1, 5)) %>%
  unnest(cols = c(data)) 

########################################################################################
################################# primary tables #######################################
########################################################################################
fullvars <- fullvars %>%
  bind_rows(tibble(orig_var = "emp_1", variable_name = "emp_1", value = 1, label_abbrev = "1",
                   create_dummy = 0, new_varname = "emp_1", reference = 0, label_full = "All working"))

codebook <- codebook %>%
  bind_rows(tibble(variable_name = "emp_1", variable = "emp_1", description = "All working",
                   variable_name_full = "All working"))

table_one_data <- calc_xtabs(may, c("all", "emp_1", "occ"), fullvars, adj_vars)
table_two_data <- calc_xtabs(may, c("occ_hcprac", "occ_educ"), fullvars, adj_vars)

saveRDS(list(table_one = table_one_data, table_two = table_two_data), "02_Output/occ-tables.rds")

table_data <- readRDS("02_Output/occ-tables.rds")
table_one_data <- table_data$table_one
table_two_data <- table_data$table_two

tab_one_order <- c("All", "All Working", "Occupation")
tab_two_order <- c("Occupation: HC practitioners", "Occupation: HC support",
                   "Educators")

table_one <- create_main_table(table_one_data, rev(tab_one_order))
table_two <- create_main_table(table_two_data, rev(tab_two_order))

gtsave(table_one, paste0(out_dir, "table-one.html"))
gtsave(table_two, paste0(out_dir, "table-two.html"))

########################################################################################
############################ hesitancy reason tables ###################################
########################################################################################

# intent tables -----------------------------------------------------------
intent.all <- reasons_table(reason.dat, "Total employed")
intent_order <- c(intent.all$Reason[-grep("^Other$", intent.all$Reason)], "Other")

#- table three variables
intent.hcp  <- reasons_table(filter(reason.dat, occ == 4), "HC Practitioners") 
intent.hcs  <- reasons_table(filter(reason.dat, occ == 5), "HC Support") 
intent.educ1 <- reasons_table(filter(reason.dat, occ_educ %in% c(1:5)), "Educators") 

#- table four variables
intent.constr <- reasons_table(filter(reason.dat, occ == 12), "Construction and extraction (oil, gas, mining, or quarrying)") 
intent.farm   <- reasons_table(filter(reason.dat, Q80 == 7),  "Farming, fishing, and forestry") 
intent.inst   <- reasons_table(filter(reason.dat, occ == 13), "Installation, maintenance, repair") 
intent.prot   <- reasons_table(filter(reason.dat, occ == 6),  "Protective service") 
intent.trans  <- reasons_table(filter(reason.dat, occ == 15), "Transportation and material moving (including delivery services)") 

#- table supp variables
intent.prod  <- reasons_table(filter(reason.dat, occ == 14), "Production (including food processing, meat packing, laundry, and dry clearning workers)") 
intent.milit <- reasons_table(filter(reason.dat, Q80 == 8), "Military") 
intent.pers <- reasons_table(filter(reason.dat, occ == 9), "Personal care and service (not healthcare)")
intent.food <- reasons_table(filter(reason.dat, occ == 7), "Food preparation and serving related")
intent.com <- reasons_table(filter(reason.dat, occ == 1), "Community and social service")

# output tables
table_three <- full_reasons_table(list(intent.all, intent.hcp, intent.hcs, intent.educ1)) %>%
  gt(rowname_col = "Reason")

gtsave(table_three, paste0(out_dir, "table-three.html"))

table_four <- full_reasons_table(list(intent.constr, intent.inst,
                                      intent.farm, intent.prot, intent.trans)) %>%
  gt(rowname_col = "Reason") 

gtsave(table_four, paste0(out_dir, "table-four.html"))

table_supp <- full_reasons_table(list(intent.milit, intent.prod, intent.pers,
                                      intent.food, intent.com)) %>%
  gt(rowname_col = "Reason") 

gtsave(table_supp, paste0(out_dir, "supplemental/supp-reasons.html"))

########################################################################################
############################### time trend tables ######################################
########################################################################################
educ_hc_occs <- c("occ_2", "occ_4", "occ_5", "occ_educ_1", "occ_educ_2", "occ_educ_3", "occ_educ_4", "occ_educ_5")
emp_out_occs <- c("all", "emp_out_1", "emp_out_2", "emp_out_3", "emp_out_199", "emp_1")

#- identify most hesitant occupations in january and may
hesitant_occs <- change_data %>%
  nest(-month) %>%
  mutate(occs = map(data, ~intent_by(.x, fullvars, "occ", "hesitant")))

most_hesitant_occs <- hesitant_occs %>%
  select(month, occs) %>%
  unnest(cols = c(occs)) %>%
  group_by(month) %>%
  arrange(month, -hesitant_props) %>%
  slice(1:5) %>%
  .$label %>%
  unique()

all_occs <- filter(fullvars, variable_name == "occ") %>%
  .$new_varname

# create time trend tables
occ_table1 <- trend_tables(most_hesitant_occs, trend_data, change_data) 
occ_table2 <- trend_tables(educ_hc_occs, trend_data, change_data)
occ_table3 <- trend_tables(emp_out_occs, trend_data, change_data)
occ_table4 <- trend_tables(all_occs, trend_data, change_data)

# note: this is percent in each occupation by time (not percent hesitant by time)
occ_changes <- map(emp_out_occs[-1], ~trend_tables("all", trend_data, change_data, outcome = .x))

# output tabels with gt
occ_table1 %>%
  arrange(month) %>%
  format_trend_table() %>%
  gt() %>%
  gtsave(paste0(out_dir, "supplemental/trends-by-most-hesitant-occs.html"))

occ_table2 %>%
  format_trend_table() %>%
  gt() %>%
  gtsave(paste0(out_dir, "supplemental/trends-by-occs-of-interest.html"))

occ_table3 %>%
  format_trend_table() %>%
  mutate_at("Description", ~factor(., levels = c("Total sample", "Worked for pay", "Work outside home",
                                                 "Work at home", "Does not work for pay", "No response"))) %>%
  dplyr::arrange(Description) %>%
  gt() %>%
  gtsave(paste0(out_dir, "supplemental/supp-table-c.html"))

occ_table4 %>%
  format_trend_table() %>%
  gt() %>%
  gtsave(paste0(out_dir, "supplemental/trends-by-all-occupations.html"))

map(occ_changes, format_trend_table) %>%
  invoke(rbind, .) %>%
  mutate(Description = c("Work outside home", "Work at home", "Does not work for pay", "No response", 
                         "Worked for pay")) %>%
  gt() %>%
  gtsave(paste0(out_dir, "supplemental/supp-table-b.html"))

# create plots
trend_table_dat1 <- occ_table3 %>%
  filter(month %in% c(1:5), label != "emp_out_199") %>%
  left_join(fullvars %>% select(new_varname, label_full), by = c("label" = "new_varname")) %>%
  mutate_at("label_full", ~gsub("All", "Total sample", .)) %>%
  mutate_at("label_full", ~factor(., levels = c("Total sample", "Work outside home",
                                                "Work at home", "Does not work for pay"))) 

trend_table1 <- trend_table_dat1 %>%
  mutate_at("month", ~lubridate::month(., label = TRUE, abbr = FALSE)) %>%
  rename(`Employment status` = label_full) %>%
  ggplot(aes(x = month, y = 100*hesitant_props, color = `Employment status`, group = `Employment status`)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  xlab("Month") +
  ylab("Percent Hesitant") +
  ylim(c(0, 50)) +
  scale_color_brewer(palette = "Set1")

ggsave(paste0(out_dir, "figure-one.png", trend_table1))

trend_table_dat2 <- occ_table4 %>%
  filter(month %in% c(1:5), label %in% c("occ_12", "occ_4", "occ_6", "occ_14", "occ_7", "occ_9")) %>%
  mutate(month = month.name[month]) %>%
  mutate_at("month", ~factor(., levels = c("January", "February", "March", "April", "May"))) %>%
  left_join(fullvars %>% select(new_varname, label_full), by = c("label" = "new_varname")) 

trend_table2 <- trend_table_dat2 %>%
  rename(Occupation = label_full) %>%
  mutate_at("Occupation", ~gsub("\\*\\*\\*", "", .)) %>%
  ggplot(aes(x = month, y = 100*hesitant_props, color = Occupation, group = Occupation)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  xlab("Month") +
  ylab("Percent Hesitant") +
  ylim(c(0, 50)) +
  scale_color_brewer(palette = "Dark2") 

ggsave(paste0(out_dir, "figure-two.png", trend_table2))

format_plot <- function(plot, letter) {
  library(grid)
  library(gridExtra)
  arrangeGrob(plot, top = textGrob(letter, x = unit(0, "npc"), 
                                   y = unit(1, "npc"), just=c("left","top")))
}

png(paste0(out_dir, "figure-three.png"),
    width = 12, height = 7, units = "in", res = 600)
gridExtra::grid.arrange(format_plot(trend_table1 + theme(legend.position = "bottom"), "A"), 
                        format_plot(trend_table2 + theme(legend.position = "bottom"), "B"),
                        ncol = 1, nrow = 2)
dev.off() # Close the file

trend_table_dat1 %>%
  mutate(panel = "A") %>%
  bind_rows(trend_table_dat2 %>% mutate(panel = "B")) %>%
  rename(Description = label_full) %>%
  ggplot(aes(x = month, y = 100*hesitant_props, color = Description, group = Description)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  xlab("Month") +
  ylab("Percent Hesitant") +
  facet_wrap(~panel, scales = "free_y")

########################################################################################
############################ sample characteristics ####################################
########################################################################################

sample_stats <- sample_characteristics(may)
gt(sample_stats) %>%
  gtsave(paste0(out_dir, "supplemental/sample-characteristics.html"))

