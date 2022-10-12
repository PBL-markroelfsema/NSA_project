######################################################################################################################.

#                                                  IKEA NSA report series: target matching

######################################################################################################################.
# Description: This script compares and matched responses from different disclosure cycles for an analysis of 
# corporate progress tracking
######################################################################################################################.

# 1. Preamble ============================================================================

#setwd("~/CDP-progress-tracking")
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

output <- TRUE #Set to TRUE if new files should be output


# 4. Data =============================================================================

#abs and int target types

abs_er_2018 <- read.xlsx("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet = "C4.1a")
abs_er_2021 <- read.xlsx("data/CDP/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1a")
#int_er_2018 <- read.xlsx("input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")
#int_er_2021 <- read.xlsx("input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C4.1b")









# 5. Code =============================================================================

########### 5.1 Prepare dataframes for target comparison ###########

## Prepare abs targets dataframes 
## Rename and select key columns, remove rows that do not contain target information, i.e. row = 0,  "Question not applicable", target_year = NA

#2018 abs emission reduction targets
abs_er_2018_form <- abs_er_2018 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         target_id = `C4.1a_C1_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.reference.number`, 
         year_target_set = `C4.1a_C6_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Start.year`,
         scope = `C4.1a_C2_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Scope`,
         base_year = `C4.1a_C5_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Base.year`,
         emissions_base_year = `C4.1a_C7_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Base.year.emissions.covered.by.target.(metric.tons.CO2e)`,
         emissions_base_year_percent = `C4.1a_C3_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.%.emissions.in.Scope`,
         target_year = `C4.1a_C8_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.year`,
         targeted_reduction = `C4.1a_C4_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.%.reduction.from.base.year`,
         percent_achieved = `C4.1a_C10_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.%.achieved.(emissions)`,
         target_status = `C4.1a_C11_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.status`,
         SBTi_status = `C4.1a_C9_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Is.this.a.science-based.target?`,
         please_explain = `C4.1a_C12_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Please.explain`) %>%
  mutate(year_target_set = as.numeric(year_target_set),
         base_year = as.numeric(base_year),
         emissions_base_year = as.numeric(emissions_base_year),
         emissions_base_year_percent = as.numeric(emissions_base_year_percent),
         target_year = as.numeric(target_year),
         targeted_reduction = as.numeric(targeted_reduction),
         percent_achieved = as.numeric(percent_achieved),
         simple_scope=case_when(scope == "Scope 1" ~ "S1",
                                scope == "Scope 2 (location-based)" ~ "S2",
                                scope == "Scope 2 (market-based)" ~ "S2",
                                scope == "Scope 1+2 (location-based)" ~ "S1S2",
                                scope == "Scope 1 +2 (market-based)" ~ "S1S2",
                                scope == "Scope 1+2 (location-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 3 (upstream)" ~ "S3",
                                scope == "Scope 3 (downstream)" ~ "S3",
                                scope == "Scope 3 (upstream & downstream)" ~ "S3",
                                scope == "Scope 3: Purchased goods & services" ~ "S3",
                                scope == "Scope 3: Capital goods" ~ "S3",
                                scope == "Scope 3: Fuel and energy-related activities (not included in Scopes 1 or 2)" ~ "S3",
                                scope == "Scope 3: Upstream transportation & distribution" ~ "S3",
                                scope == "Scope 3: Waste generated in operations" ~ "S3",
                                scope == "Scope 3: Business travel" ~ "S3",
                                scope == "Scope 3: Employee commuting" ~ "S3",
                                scope == "Scope 3: Upstream leased assets" ~ "S3",
                                scope == "Scope 3: Downstream transportation and distribution" ~ "S3",
                                scope == "Scope 3: Processing of sold products" ~ "S3",
                                scope == "Scope 3: Use of sold products" ~ "S3",
                                scope == "Scope 3: End-of-life treatment of sold products" ~ "S3",
                                scope == "Scope 3: Downstream leased assets" ~ "S3",
                                scope == "Scope 3: Franchises" ~ "S3",
                                scope == "Scope 3: Investments" ~ "S3",
                                grepl('Other', `scope`) ~ "OTH")) %>%
  select(c(account_id, organization, country, access, row, target_id, year_target_set, scope, simple_scope,  base_year, emissions_base_year, emissions_base_year_percent,
           target_year, targeted_reduction, percent_achieved, target_status, SBTi_status, please_explain)) %>%
  filter(!(row == 0), !(target_id == "Question not applicable"), !is.na(target_year), target_status %in% c("New", "Underway", "Expired"),
         !(simple_scope %in% c("S3", "OTH")), emissions_base_year_percent >= 75)

#remove source dataset
rm(abs_er_2018)

#2021 abs emission reduction targets
abs_er_2021_form <- abs_er_2021 %>%
  rename(account_id = `Account.number`,
         organization = `Organization`,
         country = `Country`,
         access = `Public`,
         row = `Row`,
         target_id = `C4.1a_C1_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.reference.number`, 
         year_target_set = `C4.1a_C2_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Year.target.was.set`,
         target_coverage = `C4.1a_C3_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.coverage`,
         scope = `C4.1a_C4_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Scope(s).(or.Scope.3.category)`,
         base_year = `C4.1a_C5_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Base.year`,
         emissions_base_year = `C4.1a_C6_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Covered.emissions.in.base.year.(metric.tons.CO2e)`,
         emissions_base_year_percent = `C4.1a_C7_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Covered.emissions.in.base.year.as.%.of.total.base.year.emissions.in.selected.Scope(s).(or.Scope.3.category)`,
         target_year = `C4.1a_C8_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.year`,
         targeted_reduction = `C4.1a_C9_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Targeted.reduction.from.base.year.(%)`,
         emissions_target_year = `C4.1a_C10_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Covered.emissions.in.target.year.(metric.tons.CO2e).[auto-calculated]`,
         emissions_reporting_year = `C4.1a_C11_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Covered.emissions.in.reporting.year.(metric.tons.CO2e)`,
         percent_achieved = `C4.1a_C12_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.%.of.target.achieved.[auto-calculated]`,
         target_status = `C4.1a_C13_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.status.in.reporting.year`,
         SBTi_status = `C4.1a_C14_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Is.this.a.science-based.target?`,
         target_ambition = `C4.1a_C15_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Target.ambition`,
         please_explain = `C4.1a_C16_Provide.details.of.your.absolute.emissions.target(s).and.progress.made.against.those.targets..-.Please.explain.(including.target.coverage)`) %>%
  mutate(year_target_set = as.numeric(year_target_set),
         base_year = as.numeric(base_year),
         emissions_base_year = as.numeric(emissions_base_year),
         emissions_base_year_percent = as.numeric(emissions_base_year_percent),
         target_year = as.numeric(target_year),
         targeted_reduction = as.numeric(targeted_reduction),
         emissions_target_year = as.numeric(emissions_target_year),
         emissions_reporting_year = as.numeric(emissions_reporting_year),
         percent_achieved = as.numeric(percent_achieved),
         simple_scope=case_when(scope == "Scope 1" ~ "S1",
                                scope == "Scope 2 (location-based)" ~ "S2",
                                scope == "Scope 2 (market-based)" ~ "S2",
                                scope == "Scope 1+2 (location-based)" ~ "S1S2",
                                scope == "Scope 1+2 (market-based)" ~ "S1S2",
                                scope == "Scope 1+2 (location-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 3 (upstream)" ~ "S3",
                                scope == "Scope 3 (downstream)" ~ "S3",
                                scope == "Scope 3 (upstream & downstream)" ~ "S3",
                                scope == "Scope 3: Purchased goods & services" ~ "S3",
                                scope == "Scope 3: Capital goods" ~ "S3",
                                scope == "Scope 3: Fuel and energy-related activities (not included in Scopes 1 or 2)" ~ "S3",
                                scope == "Scope 3: Upstream transportation & distribution" ~ "S3",
                                scope == "Scope 3: Waste generated in operations" ~ "S3",
                                scope == "Scope 3: Business travel" ~ "S3",
                                scope == "Scope 3: Employee commuting" ~ "S3",
                                scope == "Scope 3: Upstream leased assets" ~ "S3",
                                scope == "Scope 3: Downstream transportation and distribution" ~ "S3",
                                scope == "Scope 3: Processing of sold products" ~ "S3",
                                scope == "Scope 3: Use of sold products" ~ "S3",
                                scope == "Scope 3: End-of-life treatment of sold products" ~ "S3",
                                scope == "Scope 3: Downstream leased assets" ~ "S3",
                                scope == "Scope 3: Franchises" ~ "S3",
                                scope == "Scope 3: Investments" ~ "S3",
                                grepl('Other', `scope`) ~ "OTH")) %>%
  select(c(account_id, organization, country, access, row, target_id, year_target_set, target_coverage, scope, simple_scope, base_year, emissions_base_year, emissions_base_year_percent,
           target_year, targeted_reduction, emissions_target_year, emissions_reporting_year, percent_achieved, target_status, SBTi_status, target_ambition, please_explain)) %>%
  filter(!(row == 0), !(target_id == "Question not applicable"), !is.na(target_year), target_status %in% c("New", "Underway", "Achieved", "Revised"),
         !(simple_scope %in% c("S3", "OTH")), emissions_base_year_percent >= 75)

#remove source dataset
rm(abs_er_2021)

#Add priority score and order
# 2018
score_df <- abs_er_2018_form %>%
  mutate(length_scope = nchar(simple_scope))
last_score <- score_df %>%
  mutate(scope_score = case_when(length_scope == "2" ~ 0.33,
                                 length_scope == "3" ~ 0.33,
                                 length_scope == "4" ~ 0.66,
                                 length_scope == "6" ~ 1)) %>%
  mutate(new_target_score = (target_year / 2050)) %>%
  mutate(emi_score = (emissions_base_year_percent / 100)) %>%
  mutate(reduction_score = (targeted_reduction / 100)) %>%
  mutate(base_emissions = emissions_base_year / 10000000000)
final <- last_score %>% rowwise() %>% mutate(priority_score = sum(scope_score, new_target_score, emi_score, reduction_score, base_emissions,na.rm = TRUE))
add_back <- final %>% select(-c(scope_score,new_target_score,emi_score,reduction_score,base_emissions,length_scope))

abs_er_2018_form <- add_back

# Target Rank  
#2018
test2 <- add_back %>% group_by(account_id) %>% mutate(priority_order = dense_rank(desc(priority_score)))
abs_er_2018_form <- test2


#Add priority score and order
# 2021
score_df <- abs_er_2021_form %>%
  mutate(length_scope = nchar(simple_scope))
last_score <- score_df %>%
  mutate(scope_score = case_when(length_scope == "2" ~ 0.33,
                                 length_scope == "3" ~ 0.33,
                                 length_scope == "4" ~ 0.66,
                                 length_scope == "6" ~ 1)) %>%
  mutate(new_target_score = (target_year / 2050)) %>%
  mutate(emi_score = (emissions_base_year_percent / 100)) %>%
  mutate(reduction_score = (targeted_reduction / 100)) %>%
  mutate(base_emissions = emissions_base_year / 10000000000)
final <- last_score %>% rowwise() %>% mutate(priority_score = sum(scope_score, new_target_score, emi_score, reduction_score, base_emissions,na.rm = TRUE))
add_back <- final %>% select(-c(scope_score,new_target_score,emi_score,reduction_score,base_emissions,length_scope))

abs_er_2021_form <- add_back

# Target Rank  
#2021
test2 <- add_back %>% group_by(account_id) %>% mutate(priority_order = dense_rank(desc(priority_score)))
abs_er_2021_form <- test2

rm("add_back", "final", "last_score", "score_df", "test2")


##2018 Base year emissions data
base_year_em_2018 <- read.xlsx("data/CDP/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C5.1")
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

#remove source dataset
rm(base_year_em_2018)

#Output dataset
if(output)  {write.xlsx(base_year_em_2018_form, "data/CDP/output/IKEA_NSA_BY_EM_2018.xlsx")
  print("Output 2018 base year emissions file")
  }


##2021 Base year emissions data
base_year_em_2021 <- read.xlsx("data/CDP/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C5.1")
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

#remove source dataset
rm(base_year_em_2021)

#Output dataset
if(output)  {write.xlsx(base_year_em_2021_form, "data/CDP/output/IKEA_NSA_BY_EM_2021.xlsx")
  print("Output 2021 base year emissions file")
}


##2018 most recent year scope 1 emissions data
mry_s1_em_2018 <- read.xlsx("data/CDP/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C6.1")
mry_s1_em_2018_form <- mry_s1_em_2018 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         row = `RowName`,
         #mry_end_year = `C6.1_C2_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.End-year.of.reporting.period`,
         mry_end_year = colnames(mry_s1_em_2018)[13],
         #mry_emissions = `C6.1_C1_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.Gross.global.Scope.1.emissions.(metric.tons.CO2e)`) %>%
         mry_emissions = colnames(mry_s1_em_2018)[12]) %>%
  mutate(mry_scope = "Scope 1",
         row = case_when(row == "Row 1" ~ "Reporting year",
                        row == "Row 2" ~ "Past year 1",
                        row == "Row 3" ~ "Past year 2",
                        row == "Row 4" ~ "Past year 3")) %>%
  select(c(account_id, access, row, mry_scope, mry_end_year, mry_emissions)) %>%
  filter(!(mry_emissions == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions, mry_end_year), names_vary = "slowest")

#remove source dataset
rm(mry_s1_em_2018)

#Output dataset
if(output)  {write.xlsx(mry_s1_em_2018_form, "data/CDP/output/IKEA_NSA_MRY_S1_EM_2018.xlsx")
  print("Output 2018 most recent year scope 1 emissions file")
}

         
##2021 most recent year scope 1 emissions data
mry_s1_em_2021 <- read.xlsx("data/CDP/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C6.1")
mry_s1_em_2021_form <- mry_s1_em_2021 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         row = `RowName`,
         #mry_start_date = `C6.1_C2_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.Start.date`,
         mry_start_date = colnames(mry_s1_em_2021)[13], 
         #mry_end_date = `C6.1_C3_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.End.date`,
         mry_end_date = colnames(mry_s1_em_2021)[14], 
         #mry_emissions = `C6.1_C1_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.Gross.global.Scope.1.emissions.(metric.tons.CO2e)`) %>%
         mry_emissions=colnames(mry_s1_em_2021)[12]) %>%
  mutate(mry_scope = "Scope 1") %>%
  select(c(account_id, access, row, mry_scope, mry_start_date, mry_end_date, mry_emissions)) %>%
  filter(!(mry_emissions == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions, mry_start_date, mry_end_date), names_vary = "slowest")
         
#remove source dataset
rm(mry_s1_em_2021)
         
#Output dataset
if(output)  {write.xlsx(mry_s1_em_2021_form, "data/CDP/output/IKEA_NSA_MRY_S1_EM_2021.xlsx")
  print("Output 2021 most recent year scope 1 emissions file")
}


##2018 most recent year scope 2 emissions data
mry_s2_em_2018 <- read.xlsx("data/CDP/input/CDP_2018_Global_aggregation_raw_response.xlsx", sheet = "C6.3")
mry_s2_em_2018_form <- mry_s2_em_2018 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         row = `RowName`,
         #mry_end_year = `C6.3_C3_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.End-year.of.reporting.period`,
         mry_end_year = colnames(mry_s2_em_2018)[14],
         #mry_emissions_s2l = `C6.3_C1_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.location-based`,
         mry_emissions_s2l = colnames(mry_s2_em_2018)[12],
         #mry_emissions_s2m = `C6.3_C2_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.market-based.(if.applicable)`) %>%
         mry_emissions_s2m = colnames(mry_s2_em_2018)[13]) %>%
  mutate(mry_scope = "Scope 2",
         row = case_when(row == "Row 1" ~ "Reporting year",
                         row == "Row 2" ~ "Past year 1",
                         row == "Row 3" ~ "Past year 2",
                         row == "Row 4" ~ "Past year 3")) %>%
  select(c(account_id, access, row, mry_scope, mry_end_year, mry_emissions_s2l, mry_emissions_s2m)) %>%
  filter(!(mry_emissions_s2l == "Question not applicable") | !(mry_emissions_s2m == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions_s2l, mry_emissions_s2m, mry_end_year), names_vary = "slowest")

#remove source dataset
rm(mry_s2_em_2018)

#Output dataset
if(output)  {write.xlsx(mry_s2_em_2018_form, "data/CDP/output/IKEA_NSA_MRY_S2_EM_2018.xlsx")
  print("Output 2018 most recent year scope 2 emissions file")
}

##2021 most recent year scope 2 emissions data
mry_s2_em_2021 <- read.xlsx("data/CDP/input/CDP_2021_Global_aggregation_raw_response.xlsx", sheet = "C6.3")
mry_s2_em_2021_form <- mry_s2_em_2021 %>%
  rename(account_id = `Account.number`,
         access = `Public`,
         row = `RowName`,
         #mry_start_date = `C6.3_C3_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Start.date`,
         mry_start_date=colnames(mry_s2_em_2021)[14],
         #mry_end_date = `C6.3_C4_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.End.date`,
         mry_end_date=colnames(mry_s2_em_2021)[15],
         #mry_emissions_s2l = `C6.3_C1_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.location-based`,
         mry_emissions_s2l=colnames(mry_s2_em_2021)[12],
         #mry_emissions_s2m = `C6.3_C2_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.market-based.(if.applicable)`) %>%
         mry_emissions_s2m=colnames(mry_s2_em_2021)[13]) %>%
  mutate(mry_scope = "Scope 2") %>%
  select(c(account_id, access, row, mry_scope, mry_start_date, mry_end_date, mry_emissions_s2l, mry_emissions_s2m)) %>%
  filter(!(mry_emissions_s2l == "Question not applicable") | !(mry_emissions_s2m == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions_s2l, mry_emissions_s2m, mry_start_date, mry_end_date), names_vary = "slowest")

#remove source dataset
rm(mry_s2_em_2021)

#Output dataset
if(output)  {write.xlsx(mry_s2_em_2021_form, "data/CDP/output/IKEA_NSA_MRY_S2_EM_2021.xlsx")
  print("Output 2021 most recent year scope 2 emissions file")
}


############ 5.2 Summary statistics  ##################################################################


# Count unique numbers of companies in each dataset
actid_uni_abs_2018 <- unique(abs_er_2018_form$account_id)
actid_uni_abs_2021 <- unique(abs_er_2021_form$account_id)
abs_act_overlap_log <- actid_uni_abs_2018 %in% actid_uni_abs_2021
abs_overlap_count <- sum(abs_act_overlap_log == TRUE)

# Identify companies that have a single target
#2018 and 2021
tar_count_2018_abs <- abs_er_2018_form %>%
  count(account_id)

tar_count_2021_abs <- abs_er_2021_form %>%
  count(account_id)

#subset of single target companies  
single_tar_abs_2018 <- filter(tar_count_2018_abs, n == 1) 
single_tar_accids_2018 <- single_tar_abs_2018$account_id

single_tar_abs_2021 <- filter(tar_count_2021_abs, n == 1)
single_tar_accids_2021 <- single_tar_abs_2021$account_id



########### 5.3 Target profiles  ################################################################

###### Profile 1 - Companies with only one target ####################
abs_er_2018_prof1 <- abs_er_2018_form %>%
  filter(account_id %in% single_tar_accids_2018) %>%
  mutate(target_profile = "prof1_single")

abs_er_2021_prof1 <- abs_er_2021_form %>%
  filter(account_id %in% single_tar_accids_2021) %>%
  mutate(target_profile = "prof1_single")


#Output profile 1 - single target companies
#2018
if(output)  {write.xlsx(abs_er_2018_prof1, "data/CDP/output/IKEA_NSA_abs_er_2018_prof1_vF.xlsx")
  print("Output 2018 absolute emissions reduction - profile 1")
}

if(output)  {write.xlsx(abs_er_2021_prof1, "data/CDP/output/IKEA_NSA_abs_er_2021_prof1_vF.xlsx")
  print("Output 2021 absolute emissions reduction - profile 1")
}


######### Profile 2 - 2021 - Same scope, same base year, different % reduction and target year ###################

# Create subset without single target companies
#2018
abs_er_2018_form_alt_post1 <- abs_er_2018_form %>%
  filter(!account_id %in% single_tar_accids_2018)

#2021
abs_er_2021_form_alt_post1 <- abs_er_2021_form %>%
  filter(!account_id %in% single_tar_accids_2021)


# Add root_id for sequential targets
#2018
abs_er_2018_form_alt_post1 <- abs_er_2018_form_alt_post1 %>%
  mutate(root_id = paste(account_id, scope, base_year, emissions_base_year, emissions_base_year_percent))

#2021
abs_er_2021_form_alt_post1 <- abs_er_2021_form_alt_post1 %>%
  mutate(root_id = paste(account_id, target_coverage, scope, base_year, emissions_base_year, emissions_base_year_percent))

#Counts of root_id matches
#2018
root_id_count_2018_abs <- abs_er_2018_form_alt_post1 %>%
  count(root_id)

#2021
root_id_count_2021_abs <- abs_er_2021_form_alt_post1 %>%
  count(root_id)

#Identify single vs multi occurrence root_id   
#2018
single_root_abs_2018 <- filter(root_id_count_2018_abs, n == 1)
multi_root_abs_2018 <- filter(root_id_count_2018_abs, n > 1)
multi_root_id_2018 <- multi_root_abs_2018$root_id

#2021
single_root_abs_2021 <- filter(root_id_count_2021_abs, n == 1) 
multi_root_abs_2021 <- filter(root_id_count_2021_abs, n > 1)
multi_root_id_2021 <- multi_root_abs_2021$root_id


#Subset dataset for multi root_id occurrence in 2018 and 2021
#2018
abs_er_2018_form_alt_post1 <- abs_er_2018_form_alt_post1 %>%
  filter(root_id %in% multi_root_id_2018)

#2021
abs_er_2021_form_alt_post1 <- abs_er_2021_form_alt_post1 %>%
  filter(root_id %in% multi_root_id_2021)


#Pivot dataset by root_id
#2018
abs_er_2018_prof2 <- abs_er_2018_form_alt_post1 %>%
  group_by(root_id) %>%
  arrange(target_year) %>%
  mutate(group_count = 1:n()) %>%
  pivot_wider(id_cols = c(account_id, organization, country, access, scope, base_year,
                          emissions_base_year, emissions_base_year_percent), 
              names_from = group_count, 
              names_vary = "slowest",
              values_from = c(target_id, target_year, targeted_reduction, percent_achieved, target_status, SBTi_status, please_explain), 
              values_fill = NA)

#2021
abs_er_2021_prof2 <- abs_er_2021_form_alt_post1 %>%
  group_by(root_id) %>%
  arrange(target_year) %>%
  mutate(group_count = 1:n()) %>%
  pivot_wider(id_cols = c(account_id, organization, country, access, target_coverage, scope, base_year,
                          emissions_base_year, emissions_base_year_percent), 
              names_from = group_count, 
              names_vary = "slowest",
              values_from = c(target_id, target_year, targeted_reduction, emissions_target_year, 
                              emissions_reporting_year, percent_achieved, target_status, SBTi_status,
                              target_ambition, please_explain), 
              values_fill = NA)


#Output profile 2 - sequential matched targets
#2018
if(output)  {write.xlsx(abs_er_2018_prof2, "data/CDP/output/IKEA_NSA_abs_er_2018_prof2_vF.xlsx")
  print("Output 2018 absolute emissions reduction - profile 2")
}

#2021
if(output)  {write.xlsx(abs_er_2021_prof2, "data/CDP/output/IKEA_NSA_abs_er_2021_prof2_vF.xlsx")
  print("Output 2021 absolute emissions reduction - profile 2")
}

###### Profile 3 - Multiple targets, separate scopes, same base and target year ####################
             #Join data sets based on account_id, base year, target year, different scopes

# Create list of unique sequential account ids
seq_tar_accids_2018 <- unique(abs_er_2018_prof2$account_id)
seq_tar_accids_2021 <- unique(abs_er_2021_prof2$account_id)


# Create subset without single target and sequential targets companies
#2018
abs_er_2018_form_alt_post2 <- abs_er_2018_form %>%
  filter(!account_id %in% single_tar_accids_2018 & !account_id %in% seq_tar_accids_2018,
         !(simple_scope %in% c("S1S2", "S1S2S3")))

#2021
abs_er_2021_form_alt_post2 <- abs_er_2021_form %>%
  filter(!account_id %in% single_tar_accids_2021 & !account_id %in% seq_tar_accids_2021,
         !(simple_scope %in% c("S1S2", "S1S2S3")))


# Add root_id for parallel targets
#2018
abs_er_2018_form_alt_post2 <- abs_er_2018_form_alt_post2 %>%
  mutate(root_id = paste(account_id, base_year, target_year))

#2021
abs_er_2021_form_alt_post2 <- abs_er_2021_form_alt_post2 %>%
  mutate(root_id = paste(account_id, target_coverage, base_year, target_year))

#Counts of root_ids 
#2018
prof3_root_id_count_2018_abs <- abs_er_2018_form_alt_post2 %>%
  count(root_id)

#2021
prof3_root_id_count_2021_abs <- abs_er_2021_form_alt_post2 %>%
  count(root_id)

#Identify single vs multi occurrence root_id   
#2018
prof3_single_root_abs_2018 <- filter(prof3_root_id_count_2018_abs, n == 1)
prof3_multi_root_abs_2018 <- filter(prof3_root_id_count_2018_abs, n > 1)
prof3_multi_root_id_2018 <- prof3_multi_root_abs_2018$root_id

#2021
prof3_single_root_abs_2021 <- filter(prof3_root_id_count_2021_abs, n == 1) 
prof3_multi_root_abs_2021 <- filter(prof3_root_id_count_2021_abs, n > 1)
prof3_multi_root_id_2021 <- prof3_multi_root_abs_2021$root_id


#Subset dataset for multi root_id occurrence in 2018 and 2021
#2018
abs_er_2018_form_alt_post2 <- abs_er_2018_form_alt_post2 %>%
  filter(root_id %in% prof3_multi_root_id_2018)

#2021
abs_er_2021_form_alt_post2 <- abs_er_2021_form_alt_post2 %>%
  filter(root_id %in% prof3_multi_root_id_2021)

#Pivot dataset by root_id
#2018
abs_er_2018_prof3 <- abs_er_2018_form_alt_post2 %>%
  group_by(root_id) %>%
  arrange(scope) %>%
  mutate(group_count = 1:n()) %>%
  pivot_wider(id_cols = c(account_id, organization, country, access, base_year, target_year), 
              names_from = group_count, 
              names_vary = "slowest",
              values_from = c(target_id, scope, emissions_base_year, emissions_base_year_percent, targeted_reduction, percent_achieved, target_status, SBTi_status, please_explain), 
              values_fill = NA)



#2021
abs_er_2021_prof3 <- abs_er_2021_form_alt_post2 %>%
  group_by(root_id) %>%
  arrange(scope) %>%
  mutate(group_count = 1:n()) %>%
  pivot_wider(id_cols = c(account_id, organization, country, access, target_coverage, base_year, target_year), 
              names_from = group_count, 
              names_vary = "slowest",
              values_from = c(target_id, scope, emissions_base_year, emissions_base_year_percent, targeted_reduction, emissions_target_year, 
                              emissions_reporting_year, percent_achieved, target_status, SBTi_status,
                              target_ambition, please_explain), 
              values_fill = NA)


#Output profile 3 - parallel matched targets
#2018
if(output)  {write.xlsx(abs_er_2018_prof3, "data/CDP/output/IKEA_NSA_abs_er_2018_prof3_vF.xlsx")
  print("Output 2018 absolute emissions reduction - profile 3")
}

#2021
if(output)  {write.xlsx(abs_er_2021_prof3, "data/CDP/output/IKEA_NSA_abs_er_2021_prof3_vF.xlsx")
  print("Output 2021 absolute emissions reduction - profile 3")
}

###### Profile 4 - Select from remaining targets with highest priority score ####################

# Create list of unique parallel target account ids
para_tar_accids_2018 <- unique(abs_er_2018_prof3$account_id)
para_tar_accids_2021 <- unique(abs_er_2021_prof3$account_id)


# Create subset without single, sequential, and parallel targets companies
#2018
abs_er_2018_prof4 <- abs_er_2018_form %>%
  filter(!account_id %in% single_tar_accids_2018 & !account_id %in% seq_tar_accids_2018 & !account_id %in% para_tar_accids_2018,
         priority_order == 1)

#2021
abs_er_2021_prof4 <- abs_er_2021_form %>%
  filter(!account_id %in% single_tar_accids_2021 & !account_id %in% seq_tar_accids_2021 & !account_id %in% para_tar_accids_2021,
         priority_order == 1)

#Output profile 4 - top priority score targets
#2018
if(output)  {write.xlsx(abs_er_2018_prof4, "data/CDP/output/IKEA_NSA_abs_er_2018_prof4_vF.xlsx")
  print("Output 2018 absolute emissions reduction - profile 4")
}

#2021
if(output)  {write.xlsx(abs_er_2021_prof4, "data/CDP/output/IKEA_NSA_abs_er_2021_prof4_vF.xlsx")
  print("Output 2021 absolute emissions reduction - profile 4")
}






