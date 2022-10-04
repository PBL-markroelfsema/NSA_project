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

# 0. Summary data
col_names_summary <- c("Account number",	"Organization", "Activities",	"Sectors",	"Industries",	"Primary activity",	"Primary sector",	"Primary industry")
CDP_data_raw_2018_summary <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2021_summary <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2018_summary <- select(CDP_data_raw_2018_summary, all_of(col_names_summary))
CDP_data_raw_2021_summary <- select(CDP_data_raw_2021_summary, all_of(col_names_summary))

# 1a. Read raw data for TY
CDP_data_raw_2018_TY <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2018_TY) <- str_replace_all(names(CDP_data_raw_2018_TY), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2018_TY) <- str_replace_all(names(CDP_data_raw_2018_TY), "C4\\.1a_C", "")
names(CDP_data_raw_2018_TY) <- str_replace_all(names(CDP_data_raw_2018_TY), "^[0123456789]", "")
names(CDP_data_raw_2018_TY) <- str_replace_all(names(CDP_data_raw_2018_TY), "^[0123456789]", "")
names(CDP_data_raw_2018_TY) <- str_replace_all(names(CDP_data_raw_2018_TY), "_", "")
names(CDP_data_raw_2018_TY) <- trimws(names(CDP_data_raw_2018_TY))


CDP_data_raw_2021_TY <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2021_TY) <- str_replace_all(names(CDP_data_raw_2021_TY), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2021_TY) <- str_replace_all(names(CDP_data_raw_2021_TY), "C4\\.1a_C", "")
names(CDP_data_raw_2021_TY) <- str_replace_all(names(CDP_data_raw_2021_TY), "^[0123456789]", "")
names(CDP_data_raw_2021_TY) <- str_replace_all(names(CDP_data_raw_2021_TY), "^[0123456789]", "")
names(CDP_data_raw_2021_TY) <- str_replace_all(names(CDP_data_raw_2021_TY), "_", "")
names(CDP_data_raw_2021_TY) <- trimws(names(CDP_data_raw_2021_TY))

CDP_data_info <- read_excel("data/CDP/Process CDP data.xlsx", sheet="R-C4.1a")
col_2018_TY <- pull(CDP_data_info, `2018`)
col_2021_TY <- pull(CDP_data_info, `2021`)
col_names <- c("dataset_year", col_2021_TY)

CDP_data_raw_2018_TY_selection <- mutate(CDP_data_raw_2018_TY, dataset_year=2018) %>% select("dataset_year", all_of(col_2018_TY))
colnames(CDP_data_raw_2018_TY_selection) <- col_names
CDP_data_raw_2021_TY_selection <- mutate(CDP_data_raw_2021_TY, dataset_year=2021) %>% select("dataset_year", all_of(col_2021_TY))
colnames(CDP_data_raw_2021_TY_selection) <- col_names

remove_TY <- c("Question not applicable")
CDP_data_raw_selection <- rbind(CDP_data_raw_2018_TY_selection, CDP_data_raw_2021_TY_selection)
CDP_data_raw_selection$dataset_year <- as.factor(CDP_data_raw_selection$dataset_year)
CDP_data_raw_selection <-  filter(CDP_data_raw_selection, !`Target year`%in%remove_TY & !is.na(`Target year`))
CDP_data_raw_selection$`Target year` <- as.integer(CDP_data_raw_selection$`Target year`)
CDP_data_raw_selection <-  filter(CDP_data_raw_selection, `% of target achieved [auto-calculated]`!=0 & !is.na(`% of target achieved [auto-calculated]`))
CDP_data_raw_selection$`% of target achieved [auto-calculated]` <- as.numeric(CDP_data_raw_selection$`% of target achieved [auto-calculated]`)
write.table(CDP_data_raw_selection, file='data/CDP/output/selection_from_year.csv', sep=";", row.names=FALSE)

# 1b. Read raw data for MRY
# 1b1. Scope 1
# 2018
CDP_data_raw_2018_MRY_S1 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.1")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "What were your organization’s gross global", "")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "C6\\.1_C", "")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "_", "")
names(CDP_data_raw_2018_MRY_S1) <- str_replace_all(names(CDP_data_raw_2018_MRY_S1), "\\?", "")
names(CDP_data_raw_2018_MRY_S1) <- trimws(names(CDP_data_raw_2018_MRY_S1))
CDP_data_raw_2018_MRY_S1 <- select(CDP_data_raw_2018_MRY_S1, -Row, 
                                   -`Scope 1 emissions in metric tons CO2e - Comment`,
                                   -`Scope 1 emissions in metric tons CO2e - End-year of reporting period`)
CDP_data_raw_2018_MRY_S1 <- spread(CDP_data_raw_2018_MRY_S1, key="RowName", value="Scope 1 emissions in metric tons CO2e - Gross global Scope 1 emissions (metric tons CO2e)")
CDP_data_raw_2018_MRY_S1 <- mutate(CDP_data_raw_2018_MRY_S1, 
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
CDP_data_raw_2021_MRY_S1 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.1")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "What were your organization’s gross global", "")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "C6\\.1_C", "")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "_", "")
names(CDP_data_raw_2021_MRY_S1) <- str_replace_all(names(CDP_data_raw_2021_MRY_S1), "\\?", "")
names(CDP_data_raw_2021_MRY_S1) <- trimws(names(CDP_data_raw_2021_MRY_S1))
CDP_data_raw_2021_MRY_S1 <- select(CDP_data_raw_2021_MRY_S1, -`Scope 1 emissions in metric tons CO2e - Start date`,
                                                             -`Scope 1 emissions in metric tons CO2e - End date`)
CDP_data_raw_2021_MRY_S1 <- select(CDP_data_raw_2021_MRY_S1, -Row, -`Scope 1 emissions in metric tons CO2e - Comment`)
CDP_data_raw_2021_MRY_S1 <- spread(CDP_data_raw_2021_MRY_S1, key="RowName", value="Scope 1 emissions in metric tons CO2e - Gross global Scope 1 emissions (metric tons CO2e)")
CDP_data_raw_2021_MRY_S1 <- mutate(CDP_data_raw_2021_MRY_S1, 
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
CDP_data_raw_2021_MRY_S1 <- select(CDP_data_raw_2021_MRY_S1, -`Reporting year`, -`Past year 1`, -`Past year 2`, -`Past year 3`)

# 1b2. Scope 2
# 2018
CDP_data_raw_2018_MRY_S2 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.3")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "What were your organization’s gross global", "")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "C6\\.3_C", "")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "_", "")
names(CDP_data_raw_2018_MRY_S2) <- str_replace_all(names(CDP_data_raw_2018_MRY_S2), "\\?", "")
names(CDP_data_raw_2018_MRY_S2) <- trimws(names(CDP_data_raw_2018_MRY_S2))
CDP_data_raw_2018_MRY_S2 <- select(CDP_data_raw_2018_MRY_S2, -Row, 
                                   -`Scope 2 emissions in metric tons CO2e - Comment`,
                                   -`Scope 2 emissions in metric tons CO2e - End-year of reporting period`)
CDP_data_raw_2018_MRY_S2L <- select(CDP_data_raw_2018_MRY_S2, !contains("market-based"))
CDP_data_raw_2018_MRY_S2L <- spread(CDP_data_raw_2018_MRY_S2L, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, location-based")
CDP_data_raw_2018_MRY_S2M <- select(CDP_data_raw_2018_MRY_S2, !contains("location-based"))
CDP_data_raw_2018_MRY_S2M <- spread(CDP_data_raw_2018_MRY_S2M, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, market-based (if applicable)")
CDP_data_raw_2018_MRY_S2L <- mutate(CDP_data_raw_2018_MRY_S2L, 
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
CDP_data_raw_2018_MRY_S2M <- mutate(CDP_data_raw_2018_MRY_S2M, 
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
col_names = colnames(CDP_data_raw_2018_MRY_S2L)
col_names = col_names[!col_names=="EM_MRY_S2L"]
CDP_data_raw_2018_MRY_S2 <- full_join(CDP_data_raw_2018_MRY_S2L, CDP_data_raw_2018_MRY_S2M, by=col_names)
CDP_data_raw_2018_MRY_S2$EM_MRY_S2L <- as.numeric(CDP_data_raw_2018_MRY_S2$EM_MRY_S2L)
CDP_data_raw_2018_MRY_S2$EM_MRY_S2M <- as.numeric(CDP_data_raw_2018_MRY_S2$EM_MRY_S2M)

CDP_data_raw_2018_MRY_S2 <- mutate(CDP_data_raw_2018_MRY_S2, EM_MRY_S2=pmax(EM_MRY_S2L, EM_MRY_S2M)) 

# 2021
CDP_data_raw_2021_MRY_S2 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.3")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "What were your organization’s gross global", "")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "C6\\.3_C", "")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "_", "")
names(CDP_data_raw_2021_MRY_S2) <- str_replace_all(names(CDP_data_raw_2021_MRY_S2), "\\?", "")
names(CDP_data_raw_2021_MRY_S2) <- trimws(names(CDP_data_raw_2021_MRY_S2))
CDP_data_raw_2021_MRY_S2 <- select(CDP_data_raw_2021_MRY_S2, -Row, -`Scope 2 emissions in metric tons CO2e - Comment`,
                                   -`Scope 2 emissions in metric tons CO2e - Start date`,
                                   -`Scope 2 emissions in metric tons CO2e - End date`)
CDP_data_raw_2021_MRY_S2L <- select(CDP_data_raw_2021_MRY_S2, !contains("market-based"))
CDP_data_raw_2021_MRY_S2L <- spread(CDP_data_raw_2021_MRY_S2L, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, location-based")
CDP_data_raw_2021_MRY_S2M <- select(CDP_data_raw_2021_MRY_S2, !contains("location-based"))
CDP_data_raw_2021_MRY_S2M <- spread(CDP_data_raw_2021_MRY_S2M, key="RowName", value="Scope 2 emissions in metric tons CO2e - Scope 2, market-based (if applicable)")
CDP_data_raw_2021_MRY_S2L <- mutate(CDP_data_raw_2021_MRY_S2L, 
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
CDP_data_raw_2021_MRY_S2M <- mutate(CDP_data_raw_2021_MRY_S2M, 
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
col_names = colnames(CDP_data_raw_2021_MRY_S2L)
col_names = col_names[!col_names=="EM_MRY_S2L"]
CDP_data_raw_2021_MRY_S2 <- full_join(CDP_data_raw_2021_MRY_S2L, CDP_data_raw_2021_MRY_S2M, by=col_names)
CDP_data_raw_2021_MRY_S2$EM_MRY_S2L <- as.numeric(CDP_data_raw_2021_MRY_S2$EM_MRY_S2L)
CDP_data_raw_2021_MRY_S2$EM_MRY_S2M <- as.numeric(CDP_data_raw_2021_MRY_S2$EM_MRY_S2M)
CDP_data_raw_2021_MRY_S2 <- mutate(CDP_data_raw_2021_MRY_S2, EM_MRY_S2=pmax(EM_MRY_S2L,EM_MRY_S2M))

# 1b3. Scope 3
#2018
CDP_data_raw_2018_MRY_S3 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C6.5")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "Account for your organization’s ", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "gross global ", "")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "C6\\.5_C", "")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "_", "")
names(CDP_data_raw_2018_MRY_S3) <- str_replace_all(names(CDP_data_raw_2018_MRY_S3), "\\?", "")
names(CDP_data_raw_2018_MRY_S3) <- trimws(names(CDP_data_raw_2018_MRY_S3))
CDP_data_raw_2018_MRY_S3 <- select(CDP_data_raw_2018_MRY_S3, -Row)
col_names_S3 = unique(CDP_data_raw_2018_MRY_S3$RowName)
CDP_data_raw_2018_MRY_S3[CDP_data_raw_2018_MRY_S3=="Question not applicable"] <- "0"
CDP_data_raw_2018_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e` <- as.numeric(CDP_data_raw_2018_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e`)
CDP_data_raw_2018_MRY_S3 <- spread(CDP_data_raw_2018_MRY_S3, key="RowName", value="Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e")
CDP_data_raw_2018_MRY_S3 <- mutate(CDP_data_raw_2018_MRY_S3, EM_MRY_S3=rowSums(across(all_of(col_names_S3)))) %>%
  select(-all_of(col_names_S3))


# 2021
CDP_data_raw_2021_MRY_S3 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C6.5")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "Account for your organization’s ", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "gross global ", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "C6\\.5_C", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "^[0123456789]", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "_", "")
names(CDP_data_raw_2021_MRY_S3) <- str_replace_all(names(CDP_data_raw_2021_MRY_S3), "\\?", "")
names(CDP_data_raw_2021_MRY_S3) <- trimws(names(CDP_data_raw_2021_MRY_S3))
CDP_data_raw_2021_MRY_S3 <- select(CDP_data_raw_2021_MRY_S3, -Row)
col_names_S3 = unique(CDP_data_raw_2021_MRY_S3$RowName)
CDP_data_raw_2021_MRY_S3[CDP_data_raw_2021_MRY_S3=="Question not applicable"] <- "0"
CDP_data_raw_2021_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e` <- as.numeric(CDP_data_raw_2021_MRY_S3$`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e`)
CDP_data_raw_2021_MRY_S3 <- spread(CDP_data_raw_2021_MRY_S3, key="RowName", value="Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e")
CDP_data_raw_2021_MRY_S3 <- mutate(CDP_data_raw_2021_MRY_S3, EM_MRY_S3=rowSums(across(all_of(col_names_S3)))) %>%
                            select(-all_of(col_names_S3))
# Combine raw data
# CDP_data_raw_2018_summary
# CDP_data_raw_2018_MRY_S1
# CDP_data_raw_2018_TY
# CDP_data_raw_2018_MRY_S2
CDP_data_raw_2018_MRY_S1 <- select(CDP_data_raw_2018_MRY_S1, `Account number`,	Organization, EM_MRY_S1)
CDP_data_raw_2018_MRY_S2 <- select(CDP_data_raw_2018_MRY_S2, `Account number`,	Organization, EM_MRY_S2L, EM_MRY_S2M, EM_MRY_S2)
CDP_data_raw_2018_MRY_S3 <- select(CDP_data_raw_2018_MRY_S3, `Account number`,	Organization, EM_MRY_S3)
CDP_data_raw_2018 <- left_join(CDP_data_raw_2018_summary, CDP_data_raw_2018_MRY_S1, by=c("Account number", "Organization")) %>%
                     left_join(CDP_data_raw_2018_MRY_S2, by=c("Account number", "Organization")) %>%
                     left_join(CDP_data_raw_2018_MRY_S3, by=c("Account number", "Organization"))

# CDP_data_raw_2021_summary
# CDP_data_raw_2021_TY
# CDP_data_raw_2021_MRY_S1
# CDP_data_raw_2021_MRY_S2


# 2. READ PROGRESS DATA

IKEA_NSA_abs_er_2018_prof1_v1.xlsx
IKEA_NSA_abs_er_2018_prof2_v1.xlsx
IKEA_NSA_abs_er_2018_prof2_v2.xlsx

IKEA_NSA_abs_er_2021_prof1_v1.xlsx
IKEA_NSA_abs_er_2021_prof2_v1.xlsx
IKEA_NSA_abs_er_2021_prof2_v2.xlsx

# COMPARE Raw response data and cleaned dataset
CDP_data_clean_2020 <- read_excel("data/CDP/input/2020_CDP_Country_Specific_Dataset_for_NSA_Report_v2_adjusted.xlsx", sheet="Absolute ER")

# aggreagate to company (remove country data)
CDP_data_clean_2020_aggregation <- filter(CDP_data_clean_2020, `Target year`>=2020) %>%
                                   group_by(account_id, company_name, incorporation_country, `Target year`, `Target reference number`, `Scope(s) (or Scope 3 category)`) %>%
                                   summarise(MRY_EM=sum(`Covered emissions in reporting year (metric tons CO2e)`)) %>% 
                                   filter(!substr(`Scope(s) (or Scope 3 category)`,1,7) == "Scope 3")

tmp1 <- filter(CDP_data_clean_2020_aggregation, `Scope(s) (or Scope 3 category)` %in% c('Scope 1', 'Scope 2 (location-based)', 'Scope 2 (market-based)')) %>%
        mutate(MRY_EM=ifelse(is.na(MRY_EM), 0, MRY_EM)) %>%     
        spread(key=`Scope(s) (or Scope 3 category)`, value=MRY_EM) %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(MRY_EM=`Scope 1`+ max(`Scope 2 (location-based)`, `Scope 2 (market-based)`)) %>%
        select(-`Scope 1`, -`Scope 2 (location-based)`, -`Scope 2 (market-based)`) %>%
        group_by(account_id, company_name, incorporation_country) %>%
        summarise(MRY_EM=max(MRY_EM)) %>%
        mutate(`Scope(s) (or Scope 3 category)`="Scope 1+2 [auto-calculated]") %>%
        as.data.frame()
tmp2 <- ungroup(CDP_data_clean_2020_aggregation) %>%
        filter(!(`Scope(s) (or Scope 3 category)` %in% c('Scope 1', 'Scope 2 (location-based)', 'Scope 2 (market-based)'))) %>%
        select(account_id, company_name, incorporation_country, MRY_EM, `Scope(s) (or Scope 3 category)`) %>%
        as.data.frame()
 
CDP_data_clean_2020_aggregation <- rbind(tmp1, tmp2)
 # WHAT TO DO WITH OTHERS in scope?
