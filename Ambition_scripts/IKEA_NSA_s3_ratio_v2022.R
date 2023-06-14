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

# 3. Functions and variables =============================================================



# 4. Data =============================================================================
curent_dir = getwd()
setwd('./Ambition_scripts')
#abs and int target types
# abs_er_2018 <- read.xlsx("input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C4.1a")
# abs_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1a")
# #int_er_2018 <- read.xlsx("input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")
# #int_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")

# account period information
acct_period_2018 <- read.xlsx("data/CDP_2022/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C0.2")
acct_period_2019 <- read.xlsx("data/CDP_2022/input/CDP_2019_Global_aggregation_raw_response.xlsx", sheet = "C0.2")
acct_period_2020 <- read.xlsx("data/CDP_2022/input/CDP_2020_Global_aggregation_raw_response.xlsx", sheet = "C0.2")
acct_period_2021 <- read.xlsx("data/CDP_2022/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C0.2")
base_year_em_2018 <- read.xlsx("data/CDP_2022/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C5.1")
base_year_em_2019 <- read.xlsx("data/CDP_2022/input/CDP_2019_Global_aggregation_raw_response.xlsx", sheet = "C5.1")
base_year_em_2020 <- read.xlsx("data/CDP_2022/input/CDP_2020_Global_aggregation_raw_response.xlsx", sheet = "C5.1")
base_year_em_2021 <- read.xlsx("data/CDP_2022/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C5.1")


#emissions data
mry_s1_em_2018_in <- read.xlsx("data/CDP_2022/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C6.1")
mry_s1_em_2019_in <- read.xlsx("data/CDP_2022/input/CDP_2019_Global_aggregation_raw_response.xlsx", sheet = "C6.1")
mry_s1_em_2020_in <- read.xlsx("data/CDP_2022/input/CDP_2020_Global_aggregation_raw_response.xlsx", sheet = "C6.1")
mry_s1_em_2021_in <- read.xlsx("data/CDP_2022/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C6.1")

mry_s2_em_2018_in <- read.xlsx("data/CDP_2022/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C6.3")
mry_s2_em_2019_in <- read.xlsx("data/CDP_2022/input/CDP_2019_Global_aggregation_raw_response.xlsx", sheet = "C6.3")
mry_s2_em_2020_in <- read.xlsx("data/CDP_2022/input/CDP_2020_Global_aggregation_raw_response.xlsx", sheet = "C6.3")
mry_s2_em_2021_in <- read.xlsx("data/CDP_2022/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C6.3")

mry_s3_em_2018_in <- read.xlsx("data/CDP_2022/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C6.5")
mry_s3_em_2019_in <- read.xlsx("data/CDP_2022/input/CDP_2019_Global_aggregation_raw_response.xlsx", sheet = "C6.5")
mry_s3_em_2020_in <- read.xlsx("data/CDP_2022/input/CDP_2020_Global_aggregation_raw_response.xlsx", sheet = "C6.5")
mry_s3_em_2021_in <- read.xlsx("data/CDP_2022/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C6.5")

# Clean column names 2018

mry_s1_em_2018 <- mry_s1_em_2018_in
names(mry_s1_em_2018) <- str_replace_all(names(mry_s1_em_2018), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.1\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s1_em_2018) <- str_replace_all(names(mry_s1_em_2018), "C6\\.1_C", "")
names(mry_s1_em_2018) <- str_replace_all(names(mry_s1_em_2018), "^[0123456789]", "")
names(mry_s1_em_2018) <- str_replace_all(names(mry_s1_em_2018), "^[0123456789]", "")
names(mry_s1_em_2018) <- str_replace(names(mry_s1_em_2018), "\\-\\.", "")
names(mry_s1_em_2018) <- str_replace(names(mry_s1_em_2018), "_", "")
names(mry_s1_em_2018) <- str_replace_all(names(mry_s1_em_2018), "\\.", " ")
names(mry_s1_em_2018) <- trimws(names(mry_s1_em_2018))

mry_s2_em_2018 <- mry_s2_em_2018_in
names(mry_s2_em_2018) <- str_replace_all(names(mry_s2_em_2018), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.2\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s2_em_2018) <- str_replace_all(names(mry_s2_em_2018), "C6\\.3_C", "")
names(mry_s2_em_2018) <- str_replace_all(names(mry_s2_em_2018), "^[0123456789]", "")
names(mry_s2_em_2018) <- str_replace_all(names(mry_s2_em_2018), "^[0123456789]", "")
names(mry_s2_em_2018) <- str_replace(names(mry_s2_em_2018), "\\-\\.", "")
names(mry_s2_em_2018) <- str_replace(names(mry_s2_em_2018), "_", "")
names(mry_s2_em_2018) <- str_replace_all(names(mry_s2_em_2018), "\\.", " ")
names(mry_s2_em_2018) <- trimws(names(mry_s2_em_2018))

mry_s3_em_2018 <- mry_s3_em_2018_in
names(mry_s3_em_2018) <- str_replace_all(names(mry_s3_em_2018), "Account\\.for\\.your\\.organization\\’s\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.-\\.", "")
names(mry_s3_em_2018) <- str_replace_all(names(mry_s3_em_2018), "C6\\.5_C", "")
names(mry_s3_em_2018) <- str_replace_all(names(mry_s3_em_2018), "^[0123456789]", "")
names(mry_s3_em_2018) <- str_replace_all(names(mry_s3_em_2018), "^[0123456789]", "")
names(mry_s3_em_2018) <- str_replace(names(mry_s3_em_2018), "\\-\\.", "")
names(mry_s3_em_2018) <- str_replace(names(mry_s3_em_2018), "_", "")
names(mry_s3_em_2018) <- str_replace_all(names(mry_s3_em_2018), "\\.", " ")
names(mry_s3_em_2018) <- trimws(names(mry_s3_em_2018))

# Clean column names 2019

mry_s1_em_2019 <- mry_s1_em_2019_in
names(mry_s1_em_2019) <- str_replace_all(names(mry_s1_em_2019), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.1\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s1_em_2019) <- str_replace_all(names(mry_s1_em_2019), "C6\\.1_C", "")
names(mry_s1_em_2019) <- str_replace_all(names(mry_s1_em_2019), "^[0123456789]", "")
names(mry_s1_em_2019) <- str_replace_all(names(mry_s1_em_2019), "^[0123456789]", "")
names(mry_s1_em_2019) <- str_replace(names(mry_s1_em_2019), "\\-\\.", "")
names(mry_s1_em_2019) <- str_replace(names(mry_s1_em_2019), "_", "")
names(mry_s1_em_2019) <- str_replace_all(names(mry_s1_em_2019), "\\.", " ")
names(mry_s1_em_2019) <- trimws(names(mry_s1_em_2019))

mry_s2_em_2019 <- mry_s2_em_2019_in
names(mry_s2_em_2019) <- str_replace_all(names(mry_s2_em_2019), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.2\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s2_em_2019) <- str_replace_all(names(mry_s2_em_2019), "C6\\.3_C", "")
names(mry_s2_em_2019) <- str_replace_all(names(mry_s2_em_2019), "^[0123456789]", "")
names(mry_s2_em_2019) <- str_replace_all(names(mry_s2_em_2019), "^[0123456789]", "")
names(mry_s2_em_2019) <- str_replace(names(mry_s2_em_2019), "\\-\\.", "")
names(mry_s2_em_2019) <- str_replace(names(mry_s2_em_2019), "_", "")
names(mry_s2_em_2019) <- str_replace_all(names(mry_s2_em_2019), "\\.", " ")
names(mry_s2_em_2019) <- trimws(names(mry_s2_em_2019))

mry_s3_em_2019 <- mry_s3_em_2019_in
names(mry_s3_em_2019) <- str_replace_all(names(mry_s3_em_2019), "Account\\.for\\.your\\.organization\\’s\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.-\\.", "")
names(mry_s3_em_2019) <- str_replace_all(names(mry_s3_em_2019), "C6\\.5_C", "")
names(mry_s3_em_2019) <- str_replace_all(names(mry_s3_em_2019), "^[0123456789]", "")
names(mry_s3_em_2019) <- str_replace_all(names(mry_s3_em_2019), "^[0123456789]", "")
names(mry_s3_em_2019) <- str_replace(names(mry_s3_em_2019), "\\-\\.", "")
names(mry_s3_em_2019) <- str_replace(names(mry_s3_em_2019), "_", "")
names(mry_s3_em_2019) <- str_replace_all(names(mry_s3_em_2019), "\\.", " ")
names(mry_s3_em_2019) <- trimws(names(mry_s3_em_2019))

# Clean column names 2020

mry_s1_em_2020 <- mry_s1_em_2020_in
names(mry_s1_em_2020) <- str_replace_all(names(mry_s1_em_2020), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.1\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s1_em_2020) <- str_replace_all(names(mry_s1_em_2020), "C6\\.1_C", "")
names(mry_s1_em_2020) <- str_replace_all(names(mry_s1_em_2020), "^[0123456789]", "")
names(mry_s1_em_2020) <- str_replace_all(names(mry_s1_em_2020), "^[0123456789]", "")
names(mry_s1_em_2020) <- str_replace(names(mry_s1_em_2020), "\\-\\.", "")
names(mry_s1_em_2020) <- str_replace(names(mry_s1_em_2020), "_", "")
names(mry_s1_em_2020) <- str_replace_all(names(mry_s1_em_2020), "\\.", " ")
names(mry_s1_em_2020) <- trimws(names(mry_s1_em_2020))

mry_s2_em_2020 <- mry_s2_em_2020_in
names(mry_s2_em_2020) <- str_replace_all(names(mry_s2_em_2020), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.2\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s2_em_2020) <- str_replace_all(names(mry_s2_em_2020), "C6\\.3_C", "")
names(mry_s2_em_2020) <- str_replace_all(names(mry_s2_em_2020), "^[0123456789]", "")
names(mry_s2_em_2020) <- str_replace_all(names(mry_s2_em_2020), "^[0123456789]", "")
names(mry_s2_em_2020) <- str_replace(names(mry_s2_em_2020), "\\-\\.", "")
names(mry_s2_em_2020) <- str_replace(names(mry_s2_em_2020), "_", "")
names(mry_s2_em_2020) <- str_replace_all(names(mry_s2_em_2020), "\\.", " ")
names(mry_s2_em_2020) <- trimws(names(mry_s2_em_2020))

mry_s3_em_2020 <- mry_s3_em_2020_in
names(mry_s3_em_2020) <- str_replace_all(names(mry_s3_em_2020), "Account\\.for\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.-\\.", "")
names(mry_s3_em_2020) <- str_replace_all(names(mry_s3_em_2020), "C6\\.5_C", "")
names(mry_s3_em_2020) <- str_replace_all(names(mry_s3_em_2020), "^[0123456789]", "")
names(mry_s3_em_2020) <- str_replace_all(names(mry_s3_em_2020), "^[0123456789]", "")
names(mry_s3_em_2020) <- str_replace(names(mry_s3_em_2020), "\\-\\.", "")
names(mry_s3_em_2020) <- str_replace(names(mry_s3_em_2020), "_", "")
names(mry_s3_em_2020) <- str_replace_all(names(mry_s3_em_2020), "\\.", " ")
names(mry_s3_em_2020) <- trimws(names(mry_s3_em_2020))

# Clean column names 2021

mry_s1_em_2021 <- mry_s1_em_2021_in
names(mry_s1_em_2021) <- str_replace_all(names(mry_s1_em_2021), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.1\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s1_em_2021) <- str_replace_all(names(mry_s1_em_2021), "C6\\.1_C", "")
names(mry_s1_em_2021) <- str_replace_all(names(mry_s1_em_2021), "^[0123456789]", "")
names(mry_s1_em_2021) <- str_replace_all(names(mry_s1_em_2021), "^[0123456789]", "")
names(mry_s1_em_2021) <- str_replace(names(mry_s1_em_2021), "\\-\\.", "")
names(mry_s1_em_2021) <- str_replace(names(mry_s1_em_2021), "_", "")
names(mry_s1_em_2021) <- str_replace_all(names(mry_s1_em_2021), "\\.", " ")
names(mry_s1_em_2021) <- trimws(names(mry_s1_em_2021))

mry_s2_em_2021 <- mry_s2_em_2021_in
names(mry_s2_em_2021) <- str_replace_all(names(mry_s2_em_2021), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.2\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
names(mry_s2_em_2021) <- str_replace_all(names(mry_s2_em_2021), "C6\\.3_C", "")
names(mry_s2_em_2021) <- str_replace_all(names(mry_s2_em_2021), "^[0123456789]", "")
names(mry_s2_em_2021) <- str_replace_all(names(mry_s2_em_2021), "^[0123456789]", "")
names(mry_s2_em_2021) <- str_replace(names(mry_s2_em_2021), "\\-\\.", "")
names(mry_s2_em_2021) <- str_replace(names(mry_s2_em_2021), "_", "")
names(mry_s2_em_2021) <- str_replace_all(names(mry_s2_em_2021), "\\.", " ")
names(mry_s2_em_2021) <- trimws(names(mry_s2_em_2021))

mry_s3_em_2021 <- mry_s3_em_2021_in
names(mry_s3_em_2021) <- str_replace_all(names(mry_s3_em_2021), "Account\\.for\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.\\-.", "")
names(mry_s3_em_2021) <- str_replace_all(names(mry_s3_em_2021), "C6\\.5_C", "")
names(mry_s3_em_2021) <- str_replace_all(names(mry_s3_em_2021), "^[0123456789]", "")
names(mry_s3_em_2021) <- str_replace_all(names(mry_s3_em_2021), "^[0123456789]", "")
names(mry_s3_em_2021) <- str_replace(names(mry_s3_em_2021), "\\-\\.", "")
names(mry_s3_em_2021) <- str_replace(names(mry_s3_em_2021), "_", "")
names(mry_s3_em_2021) <- str_replace_all(names(mry_s3_em_2021), "\\.", " ")
names(mry_s3_em_2021) <- trimws(names(mry_s3_em_2021))

# 5. Code =============================================================================

####################### 5.4 Prepare emissions and accounting year #############################################

####################### Using 0.2 #########################

##2018 account period info
acct_period_2018_form <- acct_period_2018 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date)) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
  group_by(account_id) %>%
  slice(which.max(acctprd_end_date))

##2019 account period info
acct_period_2019_form <- acct_period_2019 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date)) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
  group_by(account_id) %>%
  slice(which.max(acctprd_end_date))

##2020 account period info
acct_period_2020_form <- acct_period_2020 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date)) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
  group_by(account_id) %>%
  slice(which.max(acctprd_end_date))

## 2021 account period info
acct_period_2021_form <- acct_period_2021 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date))

####################### Using 5.1 #########################
##2018 Base year emissions data
base_year_em_2018_form <- base_year_em_2018 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         by_scope = `RowName`,
         by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
         by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
         by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")


##2019 account period info
acct_period_2019_form <- acct_period_2019 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date)) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
  group_by(account_id) %>%
  slice(which.max(acctprd_end_date))

##2020 account period info
acct_period_2020_form <- acct_period_2020 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date)) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
  group_by(account_id) %>%
  slice(which.max(acctprd_end_date))

## 2021 account period info
acct_period_2021_form <- acct_period_2021 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
         acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
  select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
  mutate(acctprd_start_date=as.Date(acctprd_start_date),
         acctprd_end_date=as.Date(acctprd_end_date))

####################### Using 5.1 #########################
##2018 Base year emissions data
base_year_em_2018_form <- base_year_em_2018 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         by_scope = `RowName`,
         by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
         by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
         by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")

##2019 Base year emissions data
base_year_em_2019_form <- base_year_em_2019 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         by_scope = `RowName`,
         by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
         by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
         by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")

##2020 Base year emissions data
base_year_em_2020_form <- base_year_em_2020 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         by_scope = `RowName`,
         by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
         by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
         by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")

##2021 Base year emissions data
base_year_em_2021_form <- base_year_em_2021 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         by_scope = `RowName`,
         by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
         by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
         by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")


############## Using 6.1, 6.3, 6.5 ########################
##2018 most recent year scope 1 emissions data

mry_s1_em_2018_form <- mry_s1_em_2018 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_end_year = `End-year of reporting period`,
         mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
  filter(!(mry_emissions_s1 == "Question not applicable")) %>%
  filter(row=="Row 1") %>%
  select(c(account_id, mry_emissions_s1)) 

##2018 most recent year scope 2 emissions data
mry_s2_em_2018_form <- mry_s2_em_2018 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_end_year = `End-year of reporting period`,
         mry_emissions_s2l = `Scope 2, location-based`,
         mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
  filter(row=="Row 1") %>%
  select(c(account_id, row,mry_emissions_s2l, mry_emissions_s2m)) %>%
  filter(!(mry_emissions_s2l == "Question not applicable") | !(mry_emissions_s2m == "Question not applicable"))


##2018 most recent year scope 3 emissions data
mry_s3_em_2018_form <- mry_s3_em_2018 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_emissions_s3 = `Metric tonnes CO2e`) %>%
  select(c(account_id, row,mry_emissions_s3)) %>%
  mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
  pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
  rowwise() %>%
  mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                `Capital goods`,
                                `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                `Upstream transportation and distribution`,
                                `Waste generated in operations`,
                                `Business travel`,
                                `Employee commuting`,
                                `Other (upstream)`,
                                `Upstream leased assets`,na.rm = TRUE),
         mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                    `Processing of sold products`,
                                    `Use of sold products`,
                                    `End of life treatment of sold products`,
                                    `Downstream leased assets`,
                                    `Franchises`,
                                    `Investments`,
                                    `Other (downstream)`,na.rm=TRUE),
         mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
  select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
         


by_mry_2018_final = base_year_em_2018_form %>%
  rename(by_emissions_s1 = `by_emissions_Scope 1`,
         by_start_dt_s1 = `by_start_date_Scope 1`,
         by_end_dt_s1 = `by_end_date_Scope 1`,
         by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
         by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
         by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
         by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
         by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
         by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
  full_join(mry_s1_em_2018_form) %>%
  full_join(mry_s2_em_2018_form) %>%
  full_join(mry_s3_em_2018_form) %>%
  full_join(acct_period_2018_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
  rename(mry_start_dt = acctprd_start_date,
         mry_end_dt = acctprd_end_date) %>%
  select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
           by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
           by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
           mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
           mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
  rowwise() %>%
  mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
         mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
         mry_emissions_s1 = as.numeric(mry_emissions_s1),
         mry_emissions_s2l = as.numeric(mry_emissions_s2l),
         mry_emissions_s2m = as.numeric(mry_emissions_s2m),
         mry_emissions_s3up = as.numeric(mry_emissions_s3up),
         mry_emissions_s3down = as.numeric(mry_emissions_s3down),
         mry_emissions_s3 = as.numeric(mry_emissions_s3),
         s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
         s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
         perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3up)
         ),
         
         perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3down)
         ),
         
         perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3up)
         ),
         
         perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3down)
         ),
         
         perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                        ~ (s12l)/
                                          (s12l + mry_emissions_s3)
         ),
         
         perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                        ~ (s12m)/
                                          (s12m + mry_emissions_s3)
         )
  ) %>%
  select(-mry_emissions_s3,-s12l,-s12m)



#######################################################################################
##2019 most recent year scope 1 emissions data
mry_s1_em_2019_form <- mry_s1_em_2019 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_start_date = `Start date`,
         mry_end_date = `End date`,
         mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
  filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
  select(c(account_id, mry_emissions_s1))


##2019 most recent year scope 2 emissions data
mry_s2_em_2019_form <- mry_s2_em_2019 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_start_date = `Start date`,
         mry_end_date = `End date`,
         mry_emissions_s2l = `Scope 2, location-based`,
         mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
  filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
  select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))




##2019 most recent year scope 3 emissions data
mry_s3_em_2019_form <- mry_s3_em_2019 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_emissions_s3 = `Metric tonnes CO2e`) %>%
  select(c(account_id, row,mry_emissions_s3)) %>%
  mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
  pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
  rowwise() %>%
  mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                  `Capital goods`,
                                  `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                  `Upstream transportation and distribution`,
                                  `Waste generated in operations`,
                                  `Business travel`,
                                  `Employee commuting`,
                                  `Other (upstream)`,
                                  `Upstream leased assets`,na.rm = TRUE),
         mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                    `Processing of sold products`,
                                    `Use of sold products`,
                                    `End of life treatment of sold products`,
                                    `Downstream leased assets`,
                                    `Franchises`,
                                    `Investments`,
                                    `Other (downstream)`,na.rm=TRUE),
         mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
  select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))


by_mry_2019_final = base_year_em_2019_form %>%
  rename(by_emissions_s1 = `by_emissions_Scope 1`,
         by_start_dt_s1 = `by_start_date_Scope 1`,
         by_end_dt_s1 = `by_end_date_Scope 1`,
         by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
         by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
         by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
         by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
         by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
         by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
  full_join(mry_s1_em_2019_form) %>%
  full_join(mry_s2_em_2019_form) %>%
  full_join(mry_s3_em_2019_form) %>%
  full_join(acct_period_2019_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
  rename(mry_start_dt = acctprd_start_date,
         mry_end_dt = acctprd_end_date) %>%
  select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
           by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
           by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
           mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
           mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
  rowwise() %>%
  mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
         mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
         mry_emissions_s1 = as.numeric(mry_emissions_s1),
         mry_emissions_s2l = as.numeric(mry_emissions_s2l),
         mry_emissions_s2m = as.numeric(mry_emissions_s2m),
         mry_emissions_s3up = as.numeric(mry_emissions_s3up),
         mry_emissions_s3down = as.numeric(mry_emissions_s3down),
         mry_emissions_s3 = as.numeric(mry_emissions_s3),
         s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
         s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
         perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3up)
         ),
         
         perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3down)
         ),
         
         perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3up)
         ),
         
         perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3down)
         ),
         
         perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                        ~ (s12l)/
                                          (s12l + mry_emissions_s3)
         ),
         
         perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                        ~ (s12m)/
                                          (s12m + mry_emissions_s3)
         )
  ) %>%
  select(-mry_emissions_s3,-s12l,-s12m)

#######################################################################################


#######################################################################################
##2020 most recent year scope 1 emissions data
mry_s1_em_2020_form <- mry_s1_em_2020 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_start_date = `Start date`,
         mry_end_date = `End date`,
         mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
  filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
  select(c(account_id, mry_emissions_s1))


##2020 most recent year scope 2 emissions data
mry_s2_em_2020_form <- mry_s2_em_2020 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_start_date = `Start date`,
         mry_end_date = `End date`,
         mry_emissions_s2l = `Scope 2, location-based`,
         mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
  filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
  select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))




##2020 most recent year scope 3 emissions data
mry_s3_em_2020_form <- mry_s3_em_2020 %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         mry_emissions_s3 = `Metric tonnes CO2e`) %>%
  select(c(account_id, row,mry_emissions_s3)) %>%
  mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
  pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
  rowwise() %>%
  mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                  `Capital goods`,
                                  `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                  `Upstream transportation and distribution`,
                                  `Waste generated in operations`,
                                  `Business travel`,
                                  `Employee commuting`,
                                  `Other (upstream)`,
                                  `Upstream leased assets`,na.rm = TRUE),
         mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                    `Processing of sold products`,
                                    `Use of sold products`,
                                    `End of life treatment of sold products`,
                                    `Downstream leased assets`,
                                    `Franchises`,
                                    `Investments`,
                                    `Other (downstream)`,na.rm=TRUE),
         mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
  select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))



#######################################################################################
by_mry_2021_final = base_year_em_2021_form %>%
  rename(by_emissions_s1 = `by_emissions_Scope 1`,
         by_start_dt_s1 = `by_start_date_Scope 1`,
         by_end_dt_s1 = `by_end_date_Scope 1`,
         by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
         by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
         by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
         by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
         by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
         by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
  full_join(mry_s1_em_2021_form) %>%
  full_join(mry_s2_em_2021_form) %>%
  full_join(mry_s3_em_2021_form) %>%
  full_join(acct_period_2021_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
  rename(mry_start_dt = acctprd_start_date,
         mry_end_dt = acctprd_end_date) %>%
  select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
           by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
           by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
           mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
           mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
  rowwise() %>%
  mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
         mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
         mry_emissions_s1 = as.numeric(mry_emissions_s1),
         mry_emissions_s2l = as.numeric(mry_emissions_s2l),
         mry_emissions_s2m = as.numeric(mry_emissions_s2m),
         mry_emissions_s3up = as.numeric(mry_emissions_s3up),
         mry_emissions_s3down = as.numeric(mry_emissions_s3down),
         mry_emissions_s3 = as.numeric(mry_emissions_s3),
         s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
         s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
         perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3up)
         ),
         
         perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                         ~ (s12l)/
                                           (s12l + mry_emissions_s3down)
         ),
         
         perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3up)
         ),
         
         perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                         ~ (s12m)/
                                           (s12m + mry_emissions_s3down)
         ),
         
         perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                        ~ (s12l)/
                                          (s12l + mry_emissions_s3)
         ),
         
         perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                        ~ (s12m)/
                                          (s12m + mry_emissions_s3)
         )
  ) %>%
  select(-mry_emissions_s3,-s12l,-s12m)


write.xlsx(by_mry_2018_final, file = 'data/CDP_2022/output/IKEA_NSA_2018_BY_MRY_s3_perc.xlsx') 
write.xlsx(by_mry_2019_final, file = 'data/CDP_2022/output/IKEA_NSA_2019_BY_MRY_s3_perc.xlsx')
write.xlsx(by_mry_2020_final, file = 'data/CDP_2022/output/IKEA_NSA_2020_BY_MRY_s3_perc.xlsx')
write.xlsx(by_mry_2021_final, file = 'data/CDP_2022/output/IKEA_NSA_2021_BY_MRY_s3_perc.xlsx') 

setwd(curent_dir)
