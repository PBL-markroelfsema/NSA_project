library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

options(scipen = 999, digits=2) 

# RAW DATA
CDP_data_targets_raw_2020 <- read_excel("data/CDP/input/CDP_2020_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_targets_raw_2020) <- str_replace_all(names(CDP_data_targets_raw_2020), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_targets_raw_2020) <- str_replace_all(names(CDP_data_targets_raw_2020), "C4\\.1a_C", "")
names(CDP_data_targets_raw_2020) <- str_replace_all(names(CDP_data_targets_raw_2020), "^[0123456789]", "")
names(CDP_data_targets_raw_2020) <- str_replace_all(names(CDP_data_targets_raw_2020), "^[0123456789]", "")
names(CDP_data_targets_raw_2020) <- str_replace_all(names(CDP_data_targets_raw_2020), "_", "")
names(CDP_data_targets_raw_2020) <- trimws(names(CDP_data_targets_raw_2020))

CDP_data_emissions_raw_2020 <- read_excel("data/CDP/input/CDP_2020_Global_Aggregation_raw_response.xlsx", sheet="C6.1")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "C6\\.1_C", "")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "^[0123456789]", "")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "^[0123456789]", "")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "_", "")
names(CDP_data_emissions_raw_2020) <- str_replace_all(names(CDP_data_emissions_raw_2020), "What were your organization’s gross global Scope 1 emissions in metric tons CO2e\\? - ", "")
names(CDP_data_emissions_raw_2020) <- trimws(names(CDP_data_emissions_raw_2020))
mutate(CDP_data_emissions_raw_2020, `Gross global Scope 1 emissions (metric tons CO2e)`=
       replace(`Gross global Scope 1 emissions (metric tons CO2e)`, `Gross global Scope 1 emissions (metric tons CO2e)`=="Question not applicable", NA))
CDP_data_emissions_raw_2020$`Gross global Scope 1 emissions (metric tons CO2e)` <- as.numeric(CDP_data_emissions_raw_2020$`Gross global Scope 1 emissions (metric tons CO2e)`)

CDP_data_emissions_raw_2020 <- group_by(CDP_data_emissions_raw_2020, `Account number`, Organization) %>% 
                               select(-`Start date`, -`End date`, -Row, -Comment) %>%
                               spread(key=RowName, value=`Gross global Scope 1 emissions (metric tons CO2e)`)
check_duplicates <- group_by(CDP_data_emissions_raw_2020, `Account number`) %>% summarise(count=n())
CDP_data_emissions_raw_2020 <- mutate(CDP_data_emissions_raw_2020, GHG=ifelse(is.na(`Reporting year`), ifelse(is.na(`Past year 1`), ifelse(is.na(`Past year 2`), ifelse(is.na(`Past year 3`), NA, `Past year 3`), `Past year 2`), `Past year 1`), `Reporting year`))
CDP_data_emissions_total_raw_2020 <- ungroup(CDP_data_emissions_raw_2020) %>% summarise(Total=(10^-6)*sum(GHG, na.rm=TRUE))

# CLEANED DATA
CDP_data_cleanded_2020 <- read_excel('data/CDP/input/2020_CDP_Country_Specific_Dataset_for_NSA_Report_v2_adjusted.xlsx', sheet = "Absolute ER")
CDP_data_cleanded_2020 <- as.data.frame(CDP_data_cleanded_2020)
CDP_data_cleanded_2020$`Target year` <- as.integer(CDP_data_cleanded_2020$`Target year`)
CDP_data_cleanded_2020$`Target status in reporting year` <- str_trim(CDP_data_cleanded_2020$`Target status in reporting year`)
CDP_data_cleanded_2020$`MRY emissions (100% of scope)` <- as.numeric(CDP_data_cleanded_2020$`MRY emissions (100% of scope)`)
CDP_data_cleanded_2020$`MRY emissions (100% of scope, excl. scope 3)` <- as.numeric(CDP_data_cleanded_2020$`MRY emissions (100% of scope, excl. scope 3)`)
CDP_data_emissions_cleanded_2020 <- select(CDP_data_cleanded_2020, account_id, company_name, `Country/Region`, `Target reference number`, `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`) %>%
                                    group_by(`account_id`, company_name, `Target reference number`) %>% 
                                    summarise(MRY_100=sum(`MRY emissions (100% of scope)`), MRY_100_excl_scope3=sum(`MRY emissions (100% of scope, excl. scope 3)`)) %>%
                                    mutate(MRY_target=ifelse(is.na(MRY_100_excl_scope3), MRY_100, MRY_100_excl_scope3)) %>%
                                    group_by(account_id, company_name) %>%
                                    summarise(MRY=max(MRY_target))
CDP_data_emissions_total_cleanded_2020 <- ungroup(CDP_data_emissions_cleanded_2020) %>% summarise(Total=(10^-6)*sum(MRY, na.rm=TRUE))
