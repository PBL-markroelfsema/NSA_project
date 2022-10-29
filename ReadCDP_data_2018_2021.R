library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

# CDP_data_raw_2018_TY
# CDP_data_raw_2021_TY
# CDP_data_raw_2018_MRY_S1
# CDP_data_raw_2021_MRY_S1
# CDP_data_raw_2018_MRY_S2
# CDP_data_raw_2021_MRY_S2

options(scipen = 999, digits=2) 

# RAW DATA

#--------------------------------------------------------------------------------------------------

# TARGET DATA (C4)

# 1a. Summary TARGETS data
col_names_summary <- c("Account number",	"Organization", "Country", "Primary sector", "Primary industry")
CDP_data_raw_2018_summary <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2021_summary <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2018_summary <- select(CDP_data_raw_2018_summary, all_of(col_names_summary))
CDP_data_raw_2021_summary <- select(CDP_data_raw_2021_summary, all_of(col_names_summary))

CDP_data_raw_target_2021_summary <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_target_2021_summary <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_target_2021_summary <- select(CDP_data_raw_target_2021_summary, all_of(col_names_summary))
CDP_data_raw_target_2021_summary <- select(CDP_data_raw_target_2021_summary, all_of(col_names_summary))

# 1b. Read target data for TY
CDP_data_raw_target_2018_TY <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_target_2018_TY) <- str_replace_all(names(CDP_data_raw_target_2018_TY), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_target_2018_TY) <- str_replace_all(names(CDP_data_raw_target_2018_TY), "C4\\.1a_C", "")
names(CDP_data_raw_target_2018_TY) <- str_replace_all(names(CDP_data_raw_target_2018_TY), "^[0123456789]", "")
names(CDP_data_raw_target_2018_TY) <- str_replace_all(names(CDP_data_raw_target_2018_TY), "^[0123456789]", "")
names(CDP_data_raw_target_2018_TY) <- str_replace_all(names(CDP_data_raw_target_2018_TY), "_", "")
names(CDP_data_raw_target_2018_TY) <- trimws(names(CDP_data_raw_target_2018_TY))

CDP_data_raw_target_2021_TY <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_target_2021_TY) <- str_replace_all(names(CDP_data_raw_target_2021_TY), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_target_2021_TY) <- str_replace_all(names(CDP_data_raw_target_2021_TY), "C4\\.1a_C", "")
names(CDP_data_raw_target_2021_TY) <- str_replace_all(names(CDP_data_raw_target_2021_TY), "^[0123456789]", "")
names(CDP_data_raw_target_2021_TY) <- str_replace_all(names(CDP_data_raw_target_2021_TY), "^[0123456789]", "")
names(CDP_data_raw_target_2021_TY) <- str_replace_all(names(CDP_data_raw_target_2021_TY), "_", "")
names(CDP_data_raw_target_2021_TY) <- trimws(names(CDP_data_raw_target_2021_TY))

# Combine 2018 and 2021 data and streamline column names from stored information on different CDP datasets
CDP_data_info <- read_excel("data/CDP/Process CDP data.xlsx", sheet="R-C4.1a")
col_2018_TY <- pull(CDP_data_info, `2018`)[1:12]
col_2021_TY <- pull(CDP_data_info, `2021`)[1:13]
col_names <- c("dataset_year", col_2021_TY)

# remove 'Question not applicable'
remove_TY <- c("Question not applicable")
CDP_data_raw_target_2018_TY_selection <-  filter(CDP_data_raw_target_2018_TY, !`Target year`%in%remove_TY & !is.na(`Target year`)) %>%
                                   mutate(`% achieved (emissions)`=ifelse(is.na(`% achieved (emissions)`), 0, `% achieved (emissions)`),
                                          `% reduction from base year`=ifelse(is.na(`% reduction from base year`), 0, `% reduction from base year`))
CDP_data_raw_target_2021_TY_selection <-  filter(CDP_data_raw_target_2021_TY, !`Target year`%in%remove_TY & !is.na(`Target year`))

# add reporting year data (2018 calculated based on % achieved emissions, included in 2021 dataset)
CDP_data_raw_target_2018_TY_selection$`Base year emissions covered by target (metric tons CO2e)` <- as.numeric(CDP_data_raw_target_2018_TY_selection$`Base year emissions covered by target (metric tons CO2e)`)
CDP_data_raw_target_2018_TY_selection$`% reduction from base year` <- as.numeric(CDP_data_raw_target_2018_TY_selection$`% reduction from base year`)
CDP_data_raw_target_2018_TY_selection$`% achieved (emissions)` <- as.numeric(CDP_data_raw_target_2018_TY_selection$`% achieved (emissions)`)

CDP_data_raw_target_2018_TY_selection <- mutate(CDP_data_raw_target_2018_TY_selection, dataset_year=2018) %>% select("dataset_year", all_of(col_2018_TY)) %>%
                                  mutate(`Covered emissions in reporting year (metric tons CO2e)`=
                                           `Base year emissions covered by target (metric tons CO2e)`*(1-(`% reduction from base year`/100)*(`% achieved (emissions)`/100)))
colnames(CDP_data_raw_target_2018_TY_selection) <- col_names

CDP_data_raw_target_2021_TY_selection <- mutate(CDP_data_raw_target_2021_TY_selection, dataset_year=2021) %>% select("dataset_year", all_of(col_2021_TY))
CDP_data_raw_target_2021_TY_selection$`Covered emissions in base year (metric tons CO2e)` <- as.numeric(CDP_data_raw_target_2021_TY_selection$`Covered emissions in base year (metric tons CO2e)`)
CDP_data_raw_target_2021_TY_selection$`Targeted reduction from base year (%)` <- as.numeric(CDP_data_raw_target_2021_TY_selection$`Targeted reduction from base year (%)`)
CDP_data_raw_target_2021_TY_selection$`% of target achieved [auto-calculated]` <- as.numeric(CDP_data_raw_target_2021_TY_selection$`% of target achieved [auto-calculated]`)
CDP_data_raw_target_2021_TY_selection$`Covered emissions in reporting year (metric tons CO2e)` <- as.numeric(CDP_data_raw_target_2021_TY_selection$`Covered emissions in reporting year (metric tons CO2e)`)

# make one raw target dataset
CDP_data_raw_target_selection <- rbind(CDP_data_raw_target_2018_TY_selection, CDP_data_raw_target_2021_TY_selection)
CDP_data_raw_target_selection$dataset_year <- as.factor(CDP_data_raw_target_selection$dataset_year)
CDP_data_raw_target_selection$`Target year` <- as.integer(CDP_data_raw_target_selection$`Target year`)
CDP_data_raw_target_selection <-  filter(CDP_data_raw_target_selection, `% of target achieved [auto-calculated]`!=0 & !is.na(`% of target achieved [auto-calculated]`))
CDP_data_raw_target_selection$`% of target achieved [auto-calculated]` <- as.numeric(CDP_data_raw_target_selection$`% of target achieved [auto-calculated]`)
names(CDP_data_raw_target_selection) <- str_replace_all(names(CDP_data_raw_target_selection), "\\[auto-calculated\\]", "")
write.table(CDP_data_raw_target_selection, file='data/CDP/output/selection_from_year.csv', sep=";", row.names=FALSE)

#--------------------------------------------------------------------------------------------------

# EMISSIONS DATA (C6)

# 2a. Read reporting year data from target data set
# 2a1. Scope 1
# 2018
CDP_data_raw_inventory_2018_MRY_S1 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.1")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "What were your organization’s gross global", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "C6\\.1_C", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "_", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S1), "\\?", "")
names(CDP_data_raw_inventory_2018_MRY_S1) <- trimws(names(CDP_data_raw_inventory_2018_MRY_S1))
CDP_data_raw_inventory_2018_MRY_S1 <- select(CDP_data_raw_inventory_2018_MRY_S1, -Row, 
                                   -`Scope 1 emissions in metric tons CO2e - Comment`,
                                   -`Scope 1 emissions in metric tons CO2e - End-year of reporting period`)
CDP_data_raw_inventory_2018_MRY_S1 <- spread(CDP_data_raw_inventory_2018_MRY_S1, key="RowName", value="Scope 1 emissions in metric tons CO2e - Gross global Scope 1 emissions (metric tons CO2e)")
CDP_data_raw_inventory_2018_MRY_S1 <- mutate(CDP_data_raw_inventory_2018_MRY_S1, 
                                #`Reporting period`=na_if(`Scope 1 emissions in metric tons CO2e - End-year of reporting period`, "Question not applicable"),
                                `Row 1`=na_if(`Row 1`, "Question not applicable"),
                                `Row 2`=na_if(`Row 2`, "Question not applicable"),
                                `Row 3`=na_if(`Row 3`, "Question not applicable"),
                                `Row 4`=na_if(`Row 4`, "Question not applicable"),
                                `Row 1` = as.numeric(`Row 1`),
                                `Row 2` = as.numeric(`Row 2`),
                                `Row 3` = as.numeric(`Row 3`),
                                `Row 4` = as.numeric(`Row 4`),
                                EM_MRY_S1=ifelse(!is.na(`Row 1`), `Row 1`, ifelse(!is.na(`Row 2`), `Row 2`, ifelse(!is.na(`Row 3`), `Row 3`, ifelse(!is.na(`Row 4`), `Row 4`, 0))))) %>%
                           select(-`Row 1`, -`Row 2`, -`Row 3`, -`Row 4`)

# 2021
CDP_data_raw_inventory_2021_MRY_S1 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.1")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "What were your organization’s gross global", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "C6\\.1_C", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "_", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S1), "\\?", "")
names(CDP_data_raw_inventory_2021_MRY_S1) <- trimws(names(CDP_data_raw_inventory_2021_MRY_S1))
CDP_data_raw_inventory_2021_MRY_S1 <- select(CDP_data_raw_inventory_2021_MRY_S1, -`Scope 1 emissions in metric tons CO2e - Start date`,
                                                             -`Scope 1 emissions in metric tons CO2e - End date`)
CDP_data_raw_inventory_2021_MRY_S1 <- select(CDP_data_raw_inventory_2021_MRY_S1, -Row, -`Scope 1 emissions in metric tons CO2e - Comment`)
CDP_data_raw_inventory_2021_MRY_S1 <- spread(CDP_data_raw_inventory_2021_MRY_S1, key="RowName", value="Scope 1 emissions in metric tons CO2e - Gross global Scope 1 emissions (metric tons CO2e)")
CDP_data_raw_inventory_2021_MRY_S1 <- mutate(CDP_data_raw_inventory_2021_MRY_S1, 
                                   #`Reporting period`=na_if(`Scope 1 emissions in metric tons CO2e - End date`, "Question not applicable"),
                                   `Reporting year`=na_if(`Reporting year`, "Question not applicable"),
                                   `Past year 1`=na_if(`Past year 1`, "Question not applicable"),
                                   `Past year 2`=na_if(`Past year 2`, "Question not applicable"),
                                   `Past year 3`=na_if(`Past year 3`, "Question not applicable"),
                                   `Reporting year` = as.numeric(`Reporting year`),
                                   `Past year 1` = as.numeric(`Past year 1`),
                                   `Past year 2` = as.numeric(`Past year 2`),
                                   `Past year 3` = as.numeric(`Past year 3`),
                                   EM_MRY_S1=ifelse(!is.na(`Reporting year`), `Reporting year`, ifelse(!is.na(`Past year 1`), `Past year 1`, ifelse(!is.na(`Past year 2`), `Past year 2`, ifelse(!is.na(`Past year 3`), `Past year 3`, 0))))) #%>%
CDP_data_raw_inventory_2021_MRY_S1 <- select(CDP_data_raw_inventory_2021_MRY_S1, -`Reporting year`, -`Past year 1`, -`Past year 2`, -`Past year 3`)

# 2a2. Scope 2
# 2018
CDP_data_raw_inventory_2018_MRY_S2 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.3")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "What were your organization’s gross global", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "C6\\.3_C", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "_", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S2), "\\?", "")
names(CDP_data_raw_inventory_2018_MRY_S2) <- trimws(names(CDP_data_raw_inventory_2018_MRY_S2))
CDP_data_raw_inventory_2018_MRY_S2 <- select(CDP_data_raw_inventory_2018_MRY_S2, -Row, 
                                   -`Scope 2 emissions in metric tons CO2e - Comment`,
                                   -`Scope 2 emissions in metric tons CO2e - End-year of reporting period`)
CDP_data_raw_inventory_2018_MRY_S2L <- select(CDP_data_raw_inventory_2018_MRY_S2, !contains("market-based"))
CDP_data_raw_inventory_2018_MRY_S2L <- spread(CDP_data_raw_inventory_2018_MRY_S2L, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, location-based")
CDP_data_raw_inventory_2018_MRY_S2M <- select(CDP_data_raw_inventory_2018_MRY_S2, !contains("location-based"))
CDP_data_raw_inventory_2018_MRY_S2M <- spread(CDP_data_raw_inventory_2018_MRY_S2M, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, market-based (if applicable)")
CDP_data_raw_inventory_2018_MRY_S2L <- mutate(CDP_data_raw_inventory_2018_MRY_S2L, 
                                   #`Reporting year`=na_if(`Scope 2 emissions in metric tons CO2e? - End-year of reporting period`, "Question not applicable"),
                                   `Row 1`=na_if(`Row 1`, "Question not applicable"),
                                   `Row 2`=na_if(`Row 2`, "Question not applicable"),
                                   `Row 3`=na_if(`Row 3`, "Question not applicable"),
                                   `Row 4`=na_if(`Row 4`, "Question not applicable"),
                                   `Row 1` = as.numeric(`Row 1`),
                                   `Row 2` = as.numeric(`Row 2`),
                                   `Row 3` = as.numeric(`Row 3`),
                                   `Row 4` = as.numeric(`Row 4`),
                                   EM_MRY_S2L=ifelse(!is.na(`Row 1`), `Row 1`, ifelse(!is.na(`Row 2`), `Row 2`, ifelse(!is.na(`Row 3`), `Row 3`, ifelse(!is.na(`Row 4`), `Row 4`, 0))))) %>%
                                   select(-`Row 1`, -`Row 2`, -`Row 3`, -`Row 4`)
CDP_data_raw_inventory_2018_MRY_S2M <- mutate(CDP_data_raw_inventory_2018_MRY_S2M, 
                                    #`Reporting year`=na_if(`Scope 2 emissions in metric tons CO2e? - End-year of reporting period`, "Question not applicable"),
                                    `Row 1`=na_if(`Row 1`, "Question not applicable"),
                                    `Row 2`=na_if(`Row 2`, "Question not applicable"),
                                    `Row 3`=na_if(`Row 3`, "Question not applicable"),
                                    `Row 4`=na_if(`Row 4`, "Question not applicable"),
                                    `Row 1` = as.numeric(`Row 1`),
                                    `Row 2` = as.numeric(`Row 2`),
                                    `Row 3` = as.numeric(`Row 3`),
                                    `Row 4` = as.numeric(`Row 4`),
                                    EM_MRY_S2M=ifelse(!is.na(`Row 1`), `Row 1`, ifelse(!is.na(`Row 2`), `Row 2`, ifelse(!is.na(`Row 3`), `Row 3`, ifelse(!is.na(`Row 4`), `Row 4`, 0))))) %>%
                                    select(-`Row 1`, -`Row 2`, -`Row 3`, -`Row 4`)
col_names = colnames(CDP_data_raw_inventory_2018_MRY_S2L)
col_names = col_names[!col_names=="EM_MRY_S2L"]
CDP_data_raw_inventory_2018_MRY_S2 <- full_join(CDP_data_raw_inventory_2018_MRY_S2L, CDP_data_raw_inventory_2018_MRY_S2M, by=col_names)
CDP_data_raw_inventory_2018_MRY_S2$EM_MRY_S2L <- as.numeric(CDP_data_raw_inventory_2018_MRY_S2$EM_MRY_S2L)
CDP_data_raw_inventory_2018_MRY_S2$EM_MRY_S2M <- as.numeric(CDP_data_raw_inventory_2018_MRY_S2$EM_MRY_S2M)

CDP_data_raw_inventory_2018_MRY_S2 <- mutate(CDP_data_raw_inventory_2018_MRY_S2, EM_MRY_S2=ifelse(EM_MRY_S2M %in% c(0, NA), EM_MRY_S2L, EM_MRY_S2M)) 

# 2021
CDP_data_raw_inventory_2021_MRY_S2 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.3")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "What were your organization’s gross global", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "C6\\.3_C", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "_", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S2), "\\?", "")
names(CDP_data_raw_inventory_2021_MRY_S2) <- trimws(names(CDP_data_raw_inventory_2021_MRY_S2))
CDP_data_raw_inventory_2021_MRY_S2 <- select(CDP_data_raw_inventory_2021_MRY_S2, -Row, -`Scope 2 emissions in metric tons CO2e - Comment`,
                                   -`Scope 2 emissions in metric tons CO2e - Start date`,
                                   -`Scope 2 emissions in metric tons CO2e - End date`)
CDP_data_raw_inventory_2021_MRY_S2L <- select(CDP_data_raw_inventory_2021_MRY_S2, !contains("market-based"))
CDP_data_raw_inventory_2021_MRY_S2L <- spread(CDP_data_raw_inventory_2021_MRY_S2L, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, location-based")
CDP_data_raw_inventory_2021_MRY_S2M <- select(CDP_data_raw_inventory_2021_MRY_S2, !contains("location-based"))
CDP_data_raw_inventory_2021_MRY_S2M <- spread(CDP_data_raw_inventory_2021_MRY_S2M, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, market-based (if applicable)")
CDP_data_raw_inventory_2021_MRY_S2L <- mutate(CDP_data_raw_inventory_2021_MRY_S2L, 
                                    `Reporting year`=na_if(`Reporting year`, "Question not applicable"),
                                    `Past year 1`=na_if(`Past year 1`, "Question not applicable"),
                                    `Past year 2`=na_if(`Past year 2`, "Question not applicable"),
                                    `Past year 3`=na_if(`Past year 3`, "Question not applicable"),
                                    `Reporting year` = as.numeric(`Reporting year`),
                                    `Past year 1` = as.numeric(`Past year 1`),
                                    `Past year 2` = as.numeric(`Past year 2`),
                                    `Past year 3` = as.numeric(`Past year 3`),
                                    EM_MRY_S2L=ifelse(!is.na(`Reporting year`), `Reporting year`, ifelse(!is.na(`Past year 1`), `Past year 1`, ifelse(!is.na(`Past year 2`), `Past year 2`, ifelse(!is.na(`Past year 3`), `Past year 3`, 0))))) %>%
                             select(-`Reporting year`, -`Past year 1`, -`Past year 2`, -`Past year 3`)
CDP_data_raw_inventory_2021_MRY_S2M <- mutate(CDP_data_raw_inventory_2021_MRY_S2M, 
                                    `Reporting year`=na_if(`Reporting year`, "Question not applicable"),
                                    `Past year 1`=na_if(`Past year 1`, "Question not applicable"),
                                    `Past year 2`=na_if(`Past year 2`, "Question not applicable"),
                                    `Past year 3`=na_if(`Past year 3`, "Question not applicable"),
                                    `Reporting year` = as.numeric(`Reporting year`),
                                    `Past year 1` = as.numeric(`Past year 1`),
                                    `Past year 2` = as.numeric(`Past year 2`),
                                    `Past year 3` = as.numeric(`Past year 3`),
                                    EM_MRY_S2M=ifelse(!is.na(`Reporting year`), `Reporting year`, ifelse(!is.na(`Past year 1`), `Past year 1`, ifelse(!is.na(`Past year 2`), `Past year 2`, ifelse(!is.na(`Past year 3`), `Past year 3`, 0))))) %>%
                             select(-`Reporting year`, -`Past year 1`, -`Past year 2`, -`Past year 3`)
col_names = colnames(CDP_data_raw_inventory_2021_MRY_S2L)
col_names = col_names[!col_names=="EM_MRY_S2L"]
CDP_data_raw_inventory_2021_MRY_S2 <- full_join(CDP_data_raw_inventory_2021_MRY_S2L, CDP_data_raw_inventory_2021_MRY_S2M, by=col_names)
CDP_data_raw_inventory_2021_MRY_S2$EM_MRY_S2L <- as.numeric(CDP_data_raw_inventory_2021_MRY_S2$EM_MRY_S2L)
CDP_data_raw_inventory_2021_MRY_S2$EM_MRY_S2M <- as.numeric(CDP_data_raw_inventory_2021_MRY_S2$EM_MRY_S2M)
CDP_data_raw_inventory_2021_MRY_S2 <- mutate(CDP_data_raw_inventory_2021_MRY_S2, EM_MRY_S2=ifelse(EM_MRY_S2M %in% c(0, NA), EM_MRY_S2L, EM_MRY_S2M))

# 2a3. Scope 3
#2018
CDP_data_raw_inventory_2018_MRY_S3 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.5")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "Account for your organization’s ", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "gross global ", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "C6\\.5_C", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "_", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2018_MRY_S3), "\\?", "")
names(CDP_data_raw_inventory_2018_MRY_S3) <- trimws(names(CDP_data_raw_inventory_2018_MRY_S3))
CDP_data_raw_inventory_2018_MRY_S3 <- select(CDP_data_raw_inventory_2018_MRY_S3, -Row)
col_names_S3 = unique(CDP_data_raw_inventory_2018_MRY_S3$RowName)
CDP_data_raw_inventory_2018_MRY_S3[CDP_data_raw_inventory_2018_MRY_S3=="Question not applicable"] <- "0"
CDP_data_raw_inventory_2018_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e` <- as.numeric(CDP_data_raw_inventory_2018_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e`)
CDP_data_raw_inventory_2018_MRY_S3 <- spread(CDP_data_raw_inventory_2018_MRY_S3, key="RowName", value="Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e")
CDP_data_raw_inventory_2018_MRY_S3 <- mutate(CDP_data_raw_inventory_2018_MRY_S3, EM_MRY_S3=rowSums(across(all_of(col_names_S3)))) %>%
  select(-all_of(col_names_S3))


# 2021
CDP_data_raw_inventory_2021_MRY_S3 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.5")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "Account for your organization’s ", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "gross global ", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "C6\\.5_C", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "_", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_inventory_2021_MRY_S3), "\\?", "")
names(CDP_data_raw_inventory_2021_MRY_S3) <- trimws(names(CDP_data_raw_inventory_2021_MRY_S3))
CDP_data_raw_inventory_2021_MRY_S3 <- select(CDP_data_raw_inventory_2021_MRY_S3, -Row)
col_names_S3 = unique(CDP_data_raw_inventory_2021_MRY_S3$RowName)
CDP_data_raw_inventory_2021_MRY_S3[CDP_data_raw_inventory_2021_MRY_S3=="Question not applicable"] <- "0"
CDP_data_raw_inventory_2021_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e` <- as.numeric(CDP_data_raw_inventory_2021_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e`)
CDP_data_raw_inventory_2021_MRY_S3 <- spread(CDP_data_raw_inventory_2021_MRY_S3, key="RowName", value="Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e")
CDP_data_raw_inventory_2021_MRY_S3 <- mutate(CDP_data_raw_inventory_2021_MRY_S3, EM_MRY_S3=rowSums(across(all_of(col_names_S3)))) %>%
                                      select(-all_of(col_names_S3)) 
#CHECK Scope 3 column names, makes upstream, downstream, upstream/downstream

# Combine raw emissions data
CDP_data_raw_inventory_2018_MRY_S1 <- select(CDP_data_raw_inventory_2018_MRY_S1, `Account number`,	Organization, EM_MRY_S1)
CDP_data_raw_inventory_2018_MRY_S2 <- select(CDP_data_raw_inventory_2018_MRY_S2, `Account number`,	Organization, EM_MRY_S2L, EM_MRY_S2M, EM_MRY_S2)
CDP_data_raw_inventory_2018_MRY_S3 <- select(CDP_data_raw_inventory_2018_MRY_S3, `Account number`,	Organization, EM_MRY_S3)
CDP_data_raw_inventory_2018 <- left_join(CDP_data_raw_2018_summary, CDP_data_raw_inventory_2018_MRY_S1, by=c("Account number", "Organization")) %>%
                               left_join(CDP_data_raw_inventory_2018_MRY_S2, by=c("Account number", "Organization")) %>%
                               left_join(CDP_data_raw_inventory_2018_MRY_S3, by=c("Account number", "Organization")) %>%
                               mutate(source_year=2018) %>%
                               select(source_year, everything())

CDP_data_raw_inventory_2021_MRY_S1 <- select(CDP_data_raw_inventory_2021_MRY_S1, `Account number`,	Organization, EM_MRY_S1)
CDP_data_raw_inventory_2021_MRY_S2 <- select(CDP_data_raw_inventory_2021_MRY_S2, `Account number`,	Organization, EM_MRY_S2L, EM_MRY_S2M, EM_MRY_S2)
CDP_data_raw_inventory_2021_MRY_S3 <- select(CDP_data_raw_inventory_2021_MRY_S3, `Account number`,	Organization, EM_MRY_S3)
CDP_data_raw_inventory_2021 <- left_join(CDP_data_raw_2021_summary, CDP_data_raw_inventory_2021_MRY_S1, by=c("Account number", "Organization")) %>%
                               left_join(CDP_data_raw_inventory_2021_MRY_S2, by=c("Account number", "Organization")) %>%
                               left_join(CDP_data_raw_inventory_2021_MRY_S3, by=c("Account number", "Organization")) %>%
                               mutate(source_year=2021) %>%
                               select(source_year, everything())

CDP_data_raw_inventory_2018_2021 <- rbind(CDP_data_raw_inventory_2018, CDP_data_raw_inventory_2021) 


CDP_data_raw_inventory_2018_2021_total <- group_by(CDP_data_raw_inventory_2018_2021, source_year) %>%
                                          mutate(EM_MRT_S1S2 = EM_MRY_S1+EM_MRY_S2) %>%
                                          summarise(value=(10^-6)*sum(EM_MRT_S1S2)) %>%
                                          mutate(Sector="Total",
                                                 Unit="MtCO2eq")
CDP_data_raw_inventory_2018_2021_sector <- group_by(CDP_data_raw_inventory_2018_2021, source_year, `Primary sector`) %>%
                                           mutate(EM_MRT_S1S2 = EM_MRY_S1+EM_MRY_S2, 
                                                  `Primary sector`=ifelse(is.na(`Primary sector`), 'Other', `Primary sector`)) %>%
                                           summarise(value=(10^-6)*sum(EM_MRT_S1S2)) %>%
                                           rename(Sector=`Primary sector`) %>%
                                           mutate(Unit="MtCO2eq")
CDP_data_raw_inventory_2018_2021_MRY_overview = rbind(CDP_data_raw_inventory_2018_2021_total, CDP_data_raw_inventory_2018_2021_sector)
write.table(CDP_data_raw_inventory_2018_2021_MRY_overview, file='data/CDP/output/GHG_emissions_raw.csv', sep=";", row.names=FALSE)

# 2. READ CLEANED TARGET DATA
CDP_data_cleanded_target_2018_original <- read_excel('data/CDP/input/CDP_CCTD_2018_abs_ER_public.xlsx', sheet = "absolute")
CDP_data_cleanded_target_2021_original <- read_excel('data/CDP/input/CDP_CCTD_2021_abs_ER_public.xlsx', sheet = "Absolute ER")

# Create dataset with quantifiable, scope 1+2 and active targets
# CHECK CDP_CCTD_2018_abs_ER_public_overview.xlsx and CDP_CCTD_2021_abs_ER_public_overview.xlsx to see selection (Needs to be transfered to R)
ActiveTargets = c('Underway', 'Achieved', 'New', 'Revised')
#2018
CDP_data_cleanded_target_2018 <- select(CDP_data_cleanded_target_2018_original, account_id, company_name, primary_industry, 
                                             Scope, `% emissions in Scope`,
                                            `Base year emissions covered by target (metric tons CO2e)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year emissions (100% of scope)`,
                                            `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`,
                                            `% reduction from base year`, `Target year`, `Target status`, `% achieved (emissions)`) %>%
                                     rename(BY_covered=`Base year emissions covered by target (metric tons CO2e)`, 
                                            BY_Scope_exclS3=`Base year emissions (100% of scope, excl. scope 3)`, 
                                            BY_Scope=`Base year emissions (100% of scope)`,
                                            MRY_Scope=`MRY emissions (100% of scope)`, 
                                            MRY_Scope_exclS3=`MRY emissions (100% of scope, excl. scope 3)`) %>%
                                     mutate(Scope=trimws(Scope), 
                                            MRY_inventory=(ifelse(is.na(`% emissions in Scope`), 100, `% emissions in Scope`)/100)*ifelse(MRY_Scope_exclS3%in%c(0, NA), ifelse(is.na(MRY_Scope), 0, MRY_Scope), MRY_Scope_exclS3),
                                            MRY_target=ifelse((is.na(BY_covered)|is.na(`% achieved (emissions)`)|is.na(`% reduction from base year`)), 0, BY_covered*(1-(`% reduction from base year`/100)*(`% achieved (emissions)`/100))),
                                            TargetActive=ifelse(`Target status`%in%ActiveTargets & (`Target year`>=2018), TRUE, FALSE),
                                            Quantifiable=ifelse((BY_covered%in%c(0, NA)&BY_Scope_exclS3%in%c(0, NA)&BY_Scope%in%c(0, NA))|(`% reduction from base year`%in%c(NA, "")), FALSE, TRUE),
                                            Scope1_2=ifelse(substr(Scope, 1, 7)!="Scope 3" & substr(Scope, 1, 5)!="Other", TRUE, FALSE),
                                            Include=ifelse(TargetActive==TRUE & Quantifiable==TRUE & Scope1_2==TRUE, TRUE, FALSE))
tmp1=filter(CDP_data_cleanded_target_2018, TargetActive==TRUE)
tmp2=filter(CDP_data_cleanded_target_2018, Quantifiable==TRUE)
tmp3=filter(CDP_data_cleanded_target_2018, Scope1_2==TRUE)
tmp4=filter(CDP_data_cleanded_target_2018, Include==TRUE)
write.table(tmp3, file='data/CDP/output/tmp2018.csv', sep=";", row.names=FALSE)

print(paste0("Total cleaned targets: ", nrow(CDP_data_cleanded_target_2018)))
print(paste0("Total active targets: ", nrow(tmp1)))
print(paste0("Total quantifiable targets: ", nrow(tmp2)))
print(paste0("Total scope1+2 targets: ", nrow(tmp3)))
print(paste0("Total included targets: ", nrow(tmp4)))

# take maximum emissions per company
CDP_data_cleanded_target_2018_MRY_included_max <- filter(CDP_data_cleanded_target_2018, Include==TRUE) %>%
                                                  group_by(account_id) %>%
                                                  summarise(MRY_inventory=max(MRY_inventory, na.rm=TRUE),
                                                            MRY_target=max(MRY_target, na.rm=TRUE))
# calculated total emissions in dataset with quantifiable, scope 1+2 and active targets
CDP_data_cleanded_target_2018_MRY_included_total <- summarise(CDP_data_cleanded_target_2018_MRY_included_max, 
                                                              MRY_inventory=sum(MRY_inventory),
                                                              MRY_target=sum(MRY_target)) %>%
                                                    mutate(primary_industry="Total",
                                                           source_year=2018)

account_sector <- select(CDP_data_raw_2018_summary, `Account number`, `Primary industry`) %>% 
                  rename(account_id=`Account number`,
                         primary_industry=`Primary industry`) %>%
                  distinct()
tmp = left_join(CDP_data_cleanded_target_2018_MRY_included_max, account_sector, by=c('account_id')) %>%
       mutate(primary_industry=ifelse(is.na(primary_industry), "Other", primary_industry))
CDP_data_cleanded_target_2018_MRY_included_sector <- group_by(tmp, primary_industry) %>%
                                                     summarise(MRY_inventory=sum(MRY_inventory, na.rm=TRUE),
                                                               MRY_target=sum(MRY_target, na.rm=TRUE)) %>%
                                                     mutate(source_year=2018)
CDP_data_cleanded_target_2018_MRY_included <- rbind(CDP_data_cleanded_target_2018_MRY_included_total, CDP_data_cleanded_target_2018_MRY_included_sector) %>%
                                              mutate(MRY_inventory=10^-6*MRY_inventory,
                                                     MRY_target=10^-6*MRY_target,
                                                     Unit="MtCO2eq")
write.table(CDP_data_cleanded_target_2018_MRY_included, file='data/CDP/output/CDP_data_cleanded_target_2018_MRY_included.csv', sep=";", row.names=FALSE)


#2021
# Add primary_industry
tmp_primary_industry=select(CDP_data_raw_2021_summary, `Account number`, `Primary industry`) %>%
                     rename(account_id=`Account number`,
                            primary_industry=`Primary industry`)
CDP_data_cleanded_target_2021 <- left_join(CDP_data_cleanded_target_2021_original, tmp_primary_industry, by=c('account_id')) %>%
                                 select(account_id, company_name, primary_industry, 
                                        `Scope(s) (or Scope 3 category)`, `Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
                                        `Covered emissions in base year (metric tons CO2e)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year emissions (100% of scope)`,
                                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Covered emissions in reporting year (metric tons CO2e)`,
                                        `Targeted reduction from base year (%)`, `Target year`, `Target status in reporting year`) %>%
  rename(Scope=`Scope(s) (or Scope 3 category)`,
         `% emissions in Scope`=`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
         BY_covered=`Covered emissions in base year (metric tons CO2e)`, 
         BY_Scope_exclS3=`Base year emissions (100% of scope, excl. scope 3)`, 
         BY_Scope=`Base year emissions (100% of scope)`,
         MRY_Scope=`MRY emissions (100% of scope)`, 
         MRY_Scope_exclS3=`MRY emissions (100% of scope, excl. scope 3)`,
         MRY_Covered=`Covered emissions in reporting year (metric tons CO2e)`,
         `% reduction from base year`=`Targeted reduction from base year (%)`,
         `Target status`=`Target status in reporting year`) %>%
  mutate(Scope=trimws(Scope), 
         MRY_inventory=(ifelse(is.na(`% emissions in Scope`), 100, `% emissions in Scope`)/100)*ifelse(MRY_Scope_exclS3%in%c(0, NA), ifelse(is.na(MRY_Scope), 0, MRY_Scope), MRY_Scope_exclS3),
         MRY_target=ifelse((is.na(MRY_Covered)), 0, MRY_Covered),
         TargetActive=ifelse(`Target status`%in%ActiveTargets & (`Target year`>=2021), TRUE, FALSE),
         Quantifiable=ifelse((BY_covered%in%c(0, NA)&BY_Scope_exclS3%in%c(0, NA)&BY_Scope%in%c(0, NA))|(`% reduction from base year`%in%c(NA, "")), FALSE, TRUE),
         Scope1_2=ifelse(substr(Scope, 1, 7)!="Scope 3" & substr(Scope, 1, 5)!="Other", TRUE, FALSE),
         Full_coverage=ifelse(`% emissions in Scope`>=90, TRUE, FALSE),
         Include=ifelse(TargetActive==TRUE & Quantifiable==TRUE & Scope1_2==TRUE & Full_coverage==TRUE, TRUE, FALSE))
tmp1=filter(CDP_data_cleanded_target_2021, TargetActive==TRUE)
tmp2=filter(CDP_data_cleanded_target_2021, Quantifiable==TRUE)
tmp3=filter(CDP_data_cleanded_target_2021, Scope1_2==TRUE)
tmp4=filter(CDP_data_cleanded_target_2021, Include==TRUE)
write.table(tmp3, file='data/CDP/output/tmp2021.csv', sep=";", row.names=FALSE)

print(paste0("Total cleaned targets: ", nrow(CDP_data_cleanded_target_2021)))
print(paste0("Total active targets: ", nrow(tmp1)))
print(paste0("Total quantifiable targets: ", nrow(tmp2)))
print(paste0("Total scope1+2 targets: ", nrow(tmp3)))
print(paste0("Total included targets: ", nrow(tmp4)))

# take maximum emissions per company
CDP_data_cleanded_target_2021_MRY_included_max <- filter(CDP_data_cleanded_target_2021, Include==TRUE) %>%
  group_by(account_id) %>%
  summarise(MRY_inventory=max(MRY_inventory, na.rm=TRUE),
            MRY_target=max(MRY_target, na.rm=TRUE))
# calculated total emissions in dataset with quantifiable, scope 1+2 and active targets
CDP_data_cleanded_target_2021_MRY_included_total <- summarise(CDP_data_cleanded_target_2021_MRY_included_max, 
                                                              MRY_inventory=sum(MRY_inventory),
                                                              MRY_target=sum(MRY_target)) %>%
                                                              mutate(primary_industry="Total",
                                                              source_year=2021)
account_sector <- select(CDP_data_raw_2021_summary, `Account number`, `Primary industry`) %>% 
                  rename(account_id=`Account number`,
                         primary_industry=`Primary industry`) %>%
                  distinct()
tmp = left_join(CDP_data_cleanded_target_2021_MRY_included_max, account_sector, by=c('account_id')) %>%
      mutate(primary_industry=ifelse(is.na(primary_industry), "Other", primary_industry))
CDP_data_cleanded_target_2021_MRY_included_sector <- group_by(tmp, primary_industry) %>%
                                                     summarise(MRY_inventory=sum(MRY_inventory, na.rm=TRUE),
                                                               MRY_target=sum(MRY_target, na.rm=TRUE)) %>%
                                                               mutate(source_year=2021)
CDP_data_cleanded_target_2021_MRY_included <- rbind(CDP_data_cleanded_target_2021_MRY_included_total, CDP_data_cleanded_target_2021_MRY_included_sector) %>%
                                              mutate(MRY_inventory=10^-6*MRY_inventory,
                                                     MRY_target=10^-6*MRY_target,
                                                     Unit="MtCO2eq")

write.table(CDP_data_cleanded_target_2021_MRY_included, file='data/CDP/output/CDP_data_cleanded_target_2021_MRY_included.csv', sep=";", row.names=FALSE)






