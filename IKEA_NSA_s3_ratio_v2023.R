######################################################################################################################.

#                                                  IKEA NSA report series: Scope 3 ratio

######################################################################################################################.
# Description: This script calculates the MRY emissions ratio between scope 3 (upstream) to scope 1 + 2M + 3 (upstream), 
# scope 3 (downstream) to scope 1 + 2M + 3 (downstream), scope 3 (upstream and downstream) to scope 1 + 2M + 3 (upstream
# and downstream), scope 3 (upstream) to scope 1 + 2L + 3 (upstream), scope 3 (downstream) to scope 1 + 2L + 3 (downstream),
# scope 3 (upstream and downstream) to scope 1 + 2L + 3 (upstream and downstream). Include also the base year scope 1 figures
# and dates, scope 2L figures and dates, scope 2M figures and dates; MRY scope 1 figures and dates, scope 2L figures and dates,
# scope 2M figures and dates.
######################################################################################################################.

# 1. Preamble ============================================================================

t0 <- Sys.time()  # to calculate the time taken to pull the data using the script 
options(scipen=999)

# 2. Library =============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(stringdist)
library(fuzzyjoin)
library(openxlsx)
library("stringr")
library(zeallot)

# 3. Functions and variables =========
#====================================================

source('IKEA_NSA_s3_ratio_functions.R')

# SETTINGS
data_dir = "data/CDP_2023/"
overwrite_input_file <- TRUE # Boolean to express if excel files should be read in even if this was already done before

# 4. Data =============================================================================

#abs and int target types
# abs_er_2018 <- read.xlsx("input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C4.1a")
# abs_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1a")
# #int_er_2018 <- read.xlsx("input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")
# #int_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")
# #int_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")

# account period information
if (overwrite_input_file == TRUE)
{ acct_period_2018 <- read.xlsx(paste0(data_dir, "input/CDP_2018_Global_aggregation_raw_response.xlsx"), sheet = "C0.2")
  acct_period_2019 <- read.xlsx(paste0(data_dir, "/input/CDP_2019_Global_aggregation_raw_response.xlsx"), sheet = "C0.2")
  acct_period_2020 <- read.xlsx(paste0(data_dir, "/input/CDP_2020_Global_aggregation_raw_response.xlsx"), sheet = "C0.2")
  acct_period_2021 <- read.xlsx(paste0(data_dir, "/input/CDP_2021_Global_aggregation_raw_response.xlsx"), sheet = "C0.2")
  acct_period_2022 <- read.xlsx(paste0(data_dir, "/input/2023_NSA_report_CDP_2022_climate_raw_response.xlsx"), sheet = "C0.2")
  base_year_em_2018 <- read.xlsx(paste0(data_dir, "/input/CDP_2018_Global_aggregation_raw_response.xlsx"), sheet = "C5.1")
  base_year_em_2019 <- read.xlsx(paste0(data_dir, "/input/CDP_2019_Global_aggregation_raw_response.xlsx"), sheet = "C5.1")
  base_year_em_2020 <- read.xlsx(paste0(data_dir, "/input/CDP_2020_Global_aggregation_raw_response.xlsx"), sheet = "C5.1")
  base_year_em_2021 <- read.xlsx(paste0(data_dir, "/input/CDP_2021_Global_aggregation_raw_response.xlsx"), sheet = "C5.1")
  base_year_em_2022 <- read.xlsx(paste0(data_dir, "/input/2023_NSA_report_CDP_2022_climate_raw_response.xlsx"), sheet = "C5.2") %>%
    mutate(across('RowName', str_replace, 'xml:space="preserve">', ''))

  #emissions data
  mry_s1_em_2018_in <- read.xlsx(paste0(data_dir, "input/CDP_2018_Global_aggregation_raw_response.xlsx"), sheet = "C6.1")
  mry_s1_em_2019_in <- read.xlsx(paste0(data_dir, "input/CDP_2019_Global_aggregation_raw_response.xlsx"), sheet = "C6.1")
  mry_s1_em_2020_in <- read.xlsx(paste0(data_dir, "input/CDP_2020_Global_aggregation_raw_response.xlsx"), sheet = "C6.1")
  mry_s1_em_2021_in <- read.xlsx(paste0(data_dir, "input/CDP_2021_Global_aggregation_raw_response.xlsx"), sheet = "C6.1")
  mry_s1_em_2022_in <- read.xlsx(paste0(data_dir, "input/2023_NSA_report_CDP_2022_climate_raw_response.xlsx"), sheet = "C6.1")

  mry_s2_em_2018_in <- read.xlsx(paste0(data_dir, "input/CDP_2018_Global_aggregation_raw_response.xlsx"), sheet = "C6.3")
  mry_s2_em_2019_in <- read.xlsx(paste0(data_dir, "input/CDP_2019_Global_aggregation_raw_response.xlsx"), sheet = "C6.3")
  mry_s2_em_2020_in <- read.xlsx(paste0(data_dir, "input/CDP_2020_Global_aggregation_raw_response.xlsx"), sheet = "C6.3")
  mry_s2_em_2021_in <- read.xlsx(paste0(data_dir, "input/CDP_2021_Global_aggregation_raw_response.xlsx"), sheet = "C6.3")
  mry_s2_em_2022_in <- read.xlsx(paste0(data_dir, "input/2023_NSA_report_CDP_2022_climate_raw_response.xlsx"), sheet = "C6.3")

  mry_s3_em_2018_in <- read.xlsx(paste0(data_dir, "input/CDP_2018_Global_aggregation_raw_response.xlsx"), sheet = "C6.5")
  mry_s3_em_2019_in <- read.xlsx(paste0(data_dir, "input/CDP_2019_Global_aggregation_raw_response.xlsx"), sheet = "C6.5")
  mry_s3_em_2020_in <- read.xlsx(paste0(data_dir, "input/CDP_2020_Global_aggregation_raw_response.xlsx"), sheet = "C6.5")
  mry_s3_em_2021_in <- read.xlsx(paste0(data_dir, "input/CDP_2021_Global_aggregation_raw_response.xlsx"), sheet = "C6.5")
  mry_s3_em_2022_in <- read.xlsx(paste0(data_dir, "input/2023_NSA_report_CDP_2022_climate_raw_response.xlsx"), sheet = "C6.5")
}

# Clean column names
# 2018
mry_s1_em_2018 <- mry_s1_em_2018_in %>%
  CleanColumnNames_s3_ratio_mry_s1()
mry_s2_em_2018 <- mry_s2_em_2018_in %>% 
  CleanColumnNames_s3_ratio_mry_s2()
mry_s3_em_2018 <- mry_s3_em_2018_in %>%
  CleanColumnNames_s3_ratio_mry_s3()
# 2019
mry_s1_em_2019 <- mry_s1_em_2019_in %>%
  CleanColumnNames_s3_ratio_mry_s1()
mry_s2_em_2019 <- mry_s2_em_2019_in %>% 
  CleanColumnNames_s3_ratio_mry_s2()
mry_s3_em_2019 <- mry_s3_em_2019_in %>%
  CleanColumnNames_s3_ratio_mry_s3()
# 2020
mry_s1_em_2020 <- mry_s1_em_2020_in %>%
  CleanColumnNames_s3_ratio_mry_s1()
mry_s2_em_2020 <- mry_s2_em_2020_in %>% 
  CleanColumnNames_s3_ratio_mry_s2()
mry_s3_em_2020 <- mry_s3_em_2020_in %>%
  CleanColumnNames_s3_ratio_mry_s3()
# 2021
mry_s1_em_2021 <- mry_s1_em_2021_in %>%
  CleanColumnNames_s3_ratio_mry_s1()
mry_s2_em_2021 <- mry_s2_em_2021_in %>% 
  CleanColumnNames_s3_ratio_mry_s2()
mry_s3_em_2021 <- mry_s3_em_2021_in %>%
  CleanColumnNames_s3_ratio_mry_s3()
# 2022
mry_s1_em_2022 <- mry_s1_em_2022_in %>%
  CleanColumnNames_s3_ratio_mry_s1()
mry_s2_em_2022 <- mry_s2_em_2022_in %>% 
  CleanColumnNames_s3_ratio_mry_s2()
mry_s3_em_2022 <- mry_s3_em_2022_in %>%
  CleanColumnNames_s3_ratio_mry_s3()

# 5. Code =============================================================================

####################### 5.4 Prepare emissions and accounting year #############################################

####################### Using 0.2 #########################
acct_period_2018_form <- PrepareS3Ratios_account_info(acct_period_2018, 2018)
acct_period_2019_form <- PrepareS3Ratios_account_info(acct_period_2019, 2019)
acct_period_2020_form <- PrepareS3Ratios_account_info(acct_period_2020, 2020)
acct_period_2021_form <- PrepareS3Ratios_account_info(acct_period_2021, 2021)
acct_period_2022_form <- PrepareS3Ratios_account_info(acct_period_2022, 2022)

####################### Using 5.1 #########################
base_year_em_2018_form <- PrepareS3Ratios_by_emissions(base_year_em_2018, 2018)
base_year_em_2019_form <- PrepareS3Ratios_by_emissions(base_year_em_2019, 2019)
base_year_em_2020_form <- PrepareS3Ratios_by_emissions(base_year_em_2020, 2020)
base_year_em_2021_form <- PrepareS3Ratios_by_emissions(base_year_em_2021, 2021)
base_year_em_2022_form <- PrepareS3Ratios_by_emissions(base_year_em_2022, 2022)

############## Using 6.1, 6.3, 6.5 ########################
# Combine MRY and BY emissions into one dataset
c(mry_s1_em_2018_form, mry_s2_em_2018_form, mry_s3_em_2018_form) %<-%  PrepareS3Ratios_mry_emissions(mry_s1_em_2018, mry_s2_em_2018, mry_s3_em_2018, 2018)
by_mry_2018_final <- PrepareS3Ratios_combine_by_mry_emissions(acct_period_2018_form, base_year_em_2018_form, mry_s1_em_2018_form, mry_s2_em_2018_form, mry_s3_em_2018_form, 2018)

c(mry_s1_em_2019_form, mry_s2_em_2019_form, mry_s3_em_2019_form) %<-%  PrepareS3Ratios_mry_emissions(mry_s1_em_2019, mry_s2_em_2019, mry_s3_em_2019, 2019)
by_mry_2019_final <- PrepareS3Ratios_combine_by_mry_emissions(acct_period_2019_form, base_year_em_2019_form, mry_s1_em_2019_form, mry_s2_em_2019_form, mry_s3_em_2019_form, 2019)

c(mry_s1_em_2020_form, mry_s2_em_2020_form, mry_s3_em_2020_form) %<-%  PrepareS3Ratios_mry_emissions(mry_s1_em_2020, mry_s2_em_2020, mry_s3_em_2020, 2020)
by_mry_2020_final <- PrepareS3Ratios_combine_by_mry_emissions(acct_period_2020_form, base_year_em_2020_form, mry_s1_em_2020_form, mry_s2_em_2020_form, mry_s3_em_2020_form, 2020)

c(mry_s1_em_2021_form, mry_s2_em_2021_form, mry_s3_em_2021_form) %<-%  PrepareS3Ratios_mry_emissions(mry_s1_em_2021, mry_s2_em_2021, mry_s3_em_2021, 2021)
by_mry_2021_final <- PrepareS3Ratios_combine_by_mry_emissions(acct_period_2021_form, base_year_em_2021_form, mry_s1_em_2021_form, mry_s2_em_2021_form, mry_s3_em_2021_form, 2021)

c(mry_s1_em_2022_form, mry_s2_em_2022_form, mry_s3_em_2022_form) %<-%  PrepareS3Ratios_mry_emissions(mry_s1_em_2022, mry_s2_em_2022, mry_s3_em_2022, 2022)
by_mry_2022_final <- PrepareS3Ratios_combine_by_mry_emissions(acct_period_2022_form, base_year_em_2022_form, mry_s1_em_2022_form, mry_s2_em_2021_form, mry_s3_em_2021_form, 2022)

write.xlsx(by_mry_2018_final, file = paste0(data_dir, 'output/IKEA_NSA_2018_BY_MRY_s3_perc.xlsx'))
write.xlsx(by_mry_2019_final, file = paste0(data_dir, 'output/IKEA_NSA_2019_BY_MRY_s3_perc.xlsx'))
write.xlsx(by_mry_2020_final, file = paste0(data_dir, 'output/IKEA_NSA_2020_BY_MRY_s3_perc.xlsx'))
write.xlsx(by_mry_2021_final, file = paste0(data_dir, 'output/IKEA_NSA_2021_BY_MRY_s3_perc.xlsx')) 
write.xlsx(by_mry_2021_final, file = paste0(data_dir, 'output/IKEA_NSA_2022_BY_MRY_s3_perc.xlsx')) 


