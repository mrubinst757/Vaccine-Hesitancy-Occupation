# load libraries and read external files
library(tidyverse)
source("01_RPrograms/occupation-paper/all-functions.R")

codebook <- read_csv("../00_RawData/codebook/codebook.csv") 
fullvars <- read_csv("../00_RawData/codebook/discrete-vars.csv") 

urban_xwalk <- readxl::read_excel("../00_RawData/other/NCHSURCodes2013.xlsx") %>%
  select(fips = `FIPS code`, urban_fips = `2013 code`) 

date_start <- c("2021-01-06", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")
date_end <- c("2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-05-20")
dargs <- transpose(list(date_start, date_end))

# read raw data and process
all_data <- list.files("../00_RawData/microdata/", pattern = "race", full.names = TRUE) %>%
  map(~data.table::fread(.x) %>%
        mutate(hesitant = case_when(
          V1 == 1 | V3 %in% c(1:2) ~ 0,
          V3 %in% c(3:4) ~ 1,
          TRUE ~ NA_real_))) %>%
  map2(dargs, ~filter(.x, as.Date(StartDatetime) >= .y[[1]], as.Date(StartDatetime) < .y[[2]]))

all_raw <- all_data %>%
  map(~select(.x, any_of(c("weight", "hesitant", "StartDatetime", "D1", "D2", "D6", "D7", "D8", "D9", "D10",
              "Q64", "Q66", "Q68", "Q69", "Q80", "V1", "V3", "V5a", "V5b", "V5c", "fips")))) %>%
  invoke(bind_rows, .) 

#saveRDS(all_raw, "../00_RawData/processed-data/jan-may-microdata-raw.rds")

all_raw <- readRDS("../00_RawData/processed-data/jan-may-microdata-raw.rds")
all_proc <- data_process(all_raw, codebook, fullvars, urban_xwalk)
saveRDS(all_proc, "../00_RawData/processed-data/jan-may-microdata-proc.rds")

all_proc <- readRDS("../00_RawData/processed-data/jan-may-microdata-proc.rds")

# survey flow statistics -------------------------------------------------------------------------
all_flow <- all_proc %>%
  mutate(month = month(start_date)) %>%
  nest(-month) %>%
  mutate(flow = map(data, summary_flow)) %>%
  arrange(month) %>%
  select(flow) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  select(-matches("Statistic[0-9]")) %>%
  set_names(c("Flow", "January", "February", "March", "April", "May"))

gtsave(gt(all_flow), "C:/Users/mdrub/Box/Vaccine-Intent-Project/jama-letter/supplemental/supp-table-a.html")

analysis_sample <- all_proc %>%
  filter(as.Date(start_date) >= "2021-04-20", as.Date(start_date) < "2021-05-20")

final_flow <- full_flow(analysis_sample)
gtsave(final_flow, "C:/Users/mdrub/Box/Vaccine-Intent-Project/jama-letter/supplemental/flow-numbers.html")

