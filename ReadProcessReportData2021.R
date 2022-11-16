library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)
library(data.table)
library(patchwork)

#SETTNGS
choice_data_source = "response" # response or processed
choice_data_source_year = 2021
#choice_ambition = "MRY" # from "BY" or "MRY"
choice_progress = "BY" # relative to "BY" or "TYset"

# RAW DATA
col_names_summary <- c("Account number",	"Organization", "Country", "Primary sector", "Primary industry")
CDP_data_raw_2018_summary <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2021_summary <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="Summary Data")
CDP_data_raw_2018_summary <- select(CDP_data_raw_2018_summary, all_of(col_names_summary))
CDP_data_raw_2021_summary <- select(CDP_data_raw_2021_summary, all_of(col_names_summary))

if (choice_data_source_year == 2021)
{ CDP_data_raw_summary = CDP_data_raw_2021_summary
} else if (choice_data_source_year == 2018)
{ CDP_data_raw_summary = CDP_data_raw_2018_summary
}
# read in data (response or processed)
# RESPONSE DATA
data_response_original <- NA
data_response <- NA
if (choice_data_source_year == 2021)
{ data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2021_abs_ER_public.xlsx', sheet = "Absolute ER")
data_response <- select(data_response_original, account_id, incorporation_country, `Target reference number`, 
                        `Scope(s) (or Scope 3 category)`, `Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Covered emissions in base year (metric tons CO2e)`, `Base year`,
                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Covered emissions in reporting year (metric tons CO2e)`, accounting_year,
                        `Targeted reduction from base year (%)`, `Target year`, `Year target was set`, `Target status in reporting year`, `% of target achieved [auto-calculated]`) %>%
  rename(Scope=`Scope(s) (or Scope 3 category)`,
         `% emissions in Scope`=`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
         `% of target achieved`=`% of target achieved [auto-calculated]`) %>%
  mutate(source_year = choice_data_source_year)
#} else if (choice_data_source_year == 2020) 
#{      data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2020_abs_ER_public.xlsx', sheet = "Absolute ER")
#data_response <- select(data_response_original, account_id, incorporation_country, `Target reference number`, `Scope(s) (or Scope 3 category)`,
#                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year`,
#                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Most recent accounting year`,
#                        `Targeted reduction from base year (%)`, `Target year`, `Year target was set`) %>%
# mutate(source_year = choice_data_source_year) %>%
#  rename(accounting_year=`Most recent accounting year`)
} else if (choice_data_source_year == 2018) 
{      data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2018_abs_ER_public.xlsx', sheet = "absolute")
data_response <- select(data_response_original, account_id, incorporation_country, `Target reference number`, 
                        Scope, `% emissions in Scope`,
                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year emissions covered by target (metric tons CO2e)`, `Base year`,
                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Most recent accounting year`,
                        Scope,
                        `% reduction from base year`, `% achieved (emissions)`, `Target year`, `Target status`) %>%
  mutate(source_year = choice_data_source_year,
         `Year target was set`=NA,
         `Covered emissions in reporting year (metric tons CO2e)`=`Base year emissions covered by target (metric tons CO2e)`*(1-(`% reduction from base year`/100)*(`% achieved (emissions)`/100))) %>%
  rename(accounting_year=`Most recent accounting year`, 
         `Covered emissions in base year (metric tons CO2e)`= `Base year emissions covered by target (metric tons CO2e)`,
         `Target status in reporting year`=`Target status`,
         `Targeted reduction from base year (%)`=`% reduction from base year`,
         `% of target achieved`=`% achieved (emissions)` )
}
nrow(data_response)

data=data_response

cols_id_tmp <- c("account_id", "Target reference number")
data <- group_by_at(data,  cols_id_tmp) %>% 
  top_n(n=1, wt=`Targeted reduction from base year (%)`) 

ActiveTargets = c('Underway', 'Achieved', 'New', 'Revised')
data_progress <-  group_by(data, account_id, `Target reference number`) %>%
  rename(BY=`Base year`,
         MRY=`accounting_year`,
         TR=`Targeted reduction from base year (%)`, 
         TY=`Target year`,
         TYset=`Year target was set`) %>%
  mutate(BY_covered=`Covered emissions in base year (metric tons CO2e)`, 
         BY_Scope_exclS3=(`% emissions in Scope`/100)*`Base year emissions (100% of scope, excl. scope 3)`, 
         BY_Scope=(`% emissions in Scope`/100)*`Base year emissions (100% of scope)`,
         
         MRY_covered=`Covered emissions in reporting year (metric tons CO2e)`,
         MRY_Scope=(`% emissions in Scope`/100)*`MRY emissions (100% of scope)`, 
         MRY_Scope_exclS3=(`% emissions in Scope`/100)*`MRY emissions (100% of scope, excl. scope 3)`,
         
         # for BY the BY_covered is more reliable, 
         # Wathc out! For the 2018 dataset MRY_covered is calcualted based on '% target achieved' and therefore less reliable
         BY_EM=ifelse(!BY_covered%in%c(0, NA), BY_covered, ifelse(!BY_Scope_exclS3%in%c(0, NA), BY_Scope_exclS3, BY_Scope)),
         MRY_EM=ifelse(!MRY_covered%in%c(0, NA), MRY_covered, ifelse(!MRY_Scope_exclS3%in%c(0, NA), MRY_Scope_exclS3, MRY_Scope)),
         TYSet=max(TYset, BY), 
         
         NetZero = ifelse(TR>=80, TRUE, FALSE),
         TargetActive=ifelse(`Target status in reporting year`%in%ActiveTargets&(TY>=choice_data_source_year), TRUE, FALSE),
         Quantifiable=ifelse((BY_covered%in%c(0, NA)&BY_Scope_exclS3%in%c(0, NA)&BY_Scope%in%c(0, NA))|(TR%in%c(NA, "")), FALSE, TRUE),
         Scope1_2=ifelse(substr(Scope, 1, 7)!="Scope 3" & substr(Scope, 1, 5)!="Other", TRUE, FALSE),
         Full_coverage=ifelse(`% emissions in Scope`>=90, TRUE, FALSE),
         Include=ifelse(TargetActive==TRUE & Quantifiable==TRUE & Scope1_2 == TRUE & Full_coverage ==TRUE, TRUE, FALSE),
         
         TYset_EM=ifelse(TYset==BY, BY_EM, 
                         ifelse(TYset>MRY, MRY_EM, ifelse(BY_EM>0 & MRY_EM>0, BY_EM+((TYset-BY)/(MRY-BY))*(MRY_EM-BY_EM), NA)))) %>%
  select(-`% of target achieved`, -`Target status in reporting year`,
         -`Covered emissions in base year (metric tons CO2e)`, -`Base year emissions (100% of scope, excl. scope 3)`, -`Base year emissions (100% of scope)`,
         -`Covered emissions in reporting year (metric tons CO2e)`, -`MRY emissions (100% of scope)`, -`MRY emissions (100% of scope, excl. scope 3)`,
         -BY_covered, -BY_Scope, -BY_Scope_exclS3, -MRY_covered, -MRY_Scope, -MRY_Scope_exclS3)
nrow(data_progress)
data_progress$BY_EM <- as.numeric(data_progress$BY_EM)
data_progress$MRY_EM <- as.numeric(data_progress$MRY_EM)
data_progress$TYset_EM <- as.numeric(data_progress$TYset_EM)

# calculate statistics for database with all records
stats_data_progress_total <- ungroup(data_progress) %>%
  summarise(number_of_targets = n(),
            number_active = sum(TargetActive, na.rm=TRUE),
            number_quantifiable = sum(Quantifiable, na.rm=TRUE),
            number_scope1_2 = sum(Scope1_2, na.rm=TRUE),
            number_full_coverage = sum(Full_coverage, na.rm=TRUE),
            number_included = sum(Include, na.rm=TRUE))
stats_data_progress_total_net_zero <- ungroup(data_progress) %>%
  filter(NetZero==TRUE) %>%
  summarise(number_of_net_zero_targets=n())
stats_data_progress_total <- cbind(stats_data_progress_total, stats_data_progress_total_net_zero)

# calculate percentage of net-zero targets for companies instead of targets
net_zero_per_company <- group_by(data_progress, account_id) %>%
  summarise(value=sum(NetZero==TRUE)) %>%
  mutate(net_zero_company=ifelse(value>0, TRUE, FALSE))
stats_data_progress_total_companies = ungroup(net_zero_per_company) %>%
  summarise(nr_companies=n(),
            nr_net_zero_companies=sum(net_zero_company, na.rm=TRUE))
stats_data_progress_total <- cbind(stats_data_progress_total, stats_data_progress_total_companies) %>%
  mutate(Included="All companies") %>%
  select(Included, everything())

stats_data_progress_total
min(data_progress$TY, na.rm=TRUE)
max(data_progress$TY, na.rm=TRUE)
tst <- filter(data_progress, TY>2030)

# Annual reductions based on 2C and 1.5C pathways from IPCC WGIII AR6 report
# Technical summary, Table TS.3: This is referencing C1 and C3a scenarios (1.5 deg cel and 2 deg cel scenarios, respectively). 
# They can also be found in page 45 of the Technical Summary for the
TwoC_annual_2030 = 27/(2030-2019)
TwoC_annual_2040 = 47/(2040-2019)
TwoC_annual_2050 = 63/(2050-2019)
OneAndAHalfC_annual_2030 = 43/(2030-2019)
OneAndAHalfC_annual_2040 = 69/(2040-2019)
OneAndAHalfC_annual_2050 = 84/(2050-2019)
Potential_annual = 50/(2030-2019)

TwoC_annual_path <- data.frame(year=c(1, 2030-2019, 2040-2019, 2050-2019), 
                               annual_reduction=c(TwoC_annual_2030, TwoC_annual_2030, TwoC_annual_2040, TwoC_annual_2050),
                               type="2 °C")
OneAndAHalfC_annual_path <- data.frame(year=c(1, 2030-2019, 2040-2019, 2050-2019), 
                                       annual_reduction=c(OneAndAHalfC_annual_2030, OneAndAHalfC_annual_2030, OneAndAHalfC_annual_2040, OneAndAHalfC_annual_2050),
                                       type="1.5 °C")
Potential_annual_path <- data.frame(year=c(1, 2030-2019, 2040-2019, 2050-2019), 
                                    annual_reduction=c(Potential_annual, Potential_annual, NA, NA),
                                    type="Potential based on current cost-effective options")

IPCC_annual_path=rbind(TwoC_annual_path, OneAndAHalfC_annual_path, Potential_annual_path)
IPCC_annual_path$type = factor(IPCC_annual_path$type, levels=c("2 °C", "1.5 °C", "Potential based on current cost-effective options"))
# calculate progress rate and target completed

data_progress <- 
  group_by(data_progress, account_id, `Target reference number`) %>%
  mutate(TY_EM=(1-TR/100)*BY_EM,
         Timelapsed=(MRY-BY)/(TY-BY),
         reduction_required=(BY_EM-TY_EM)*Timelapsed,
         reduction_achieved=BY_EM-MRY_EM,
         target_year_group=ifelse(TY<=2020, 0, ifelse(TY<=2025, 1, ifelse(TY<=2035, 2, 3))),
         target_completed=100*(TY_EM/MRY_EM-1),
         progress_rate=100*(reduction_achieved/reduction_required),
         
         maturity=ifelse(TY-BY>=0, TY-BY, NA),
         maturity_expired_BY=ifelse(MRY-BY>=0, MRY-BY, 0),
         maturity_expired_TYSet=ifelse(MRY-TYSet>=0, MRY-TYSet, 0),
         maturity_expired=ifelse(choice_progress=="BY", maturity_expired_BY, maturity_expired_TYSet),
         maturity_remaining=ifelse(TY-MRY>=0, TY-MRY, 0),
         
         annual_ambition_MRY_g=ifelse(maturity_remaining>0 & MRY_EM>0 & TY_EM>=0,-100*((TY_EM/MRY_EM)^(1/(TY-MRY))-1), NA),
         annual_ambition_BY_g=ifelse(maturity_remaining>=0 & BY_EM>0 & TY_EM>=0,-100*((TY_EM/BY_EM)^(1/(TY-BY))-1), NA),
         #annual_ambition_g=ifelse(choice_ambition=="BY", annual_ambition_BY, annual_ambition_MRY), 
         
         annual_progress_g=ifelse(maturity_expired>0 & BY_EM>0 & MRY_EM>0,-100*((MRY_EM/BY_EM)^(1/(MRY-BY))-1), NA),
         
         annual_ambition_MRY_a=ifelse(maturity_remaining>=0 & MRY_EM>0 & TY_EM>=0,-100*((1/(TY-MRY))*((TY_EM-MRY_EM)/MRY_EM)), NA),
         annual_ambition_BY_a=ifelse(maturity_remaining>=0 & BY_EM>0 & TY_EM>=0,-100*((1/(TY-BY))*((TY_EM-BY_EM)/BY_EM)), NA),
         #annual_ambition_a=ifelse(choice_ambition=="BY", annual_ambition_BY_a, annual_ambition_MRY_a),
         
         annual_progress_BY_a=ifelse(maturity_expired>0 & BY_EM>0 & MRY_EM>0,-100*((1/(MRY-BY))*((MRY_EM-BY_EM)/BY_EM)), NA),
         annual_progress_TYset_a=ifelse(maturity_expired>0 & BY_EM>0 & MRY_EM>0, -100*((1/(MRY-TYset))*((MRY_EM-TYset_EM)/TYset_EM)), NA),
         annual_progress_a=ifelse(choice_progress=="BY", annual_progress_BY_a, annual_progress_TYset_a),
         
         diff_required_achieved_a=(annual_ambition_BY_a - annual_ambition_MRY_a),
         net_zero=ifelse(TR==100, TRUE, FALSE),
         
         on_track=ifelse(reduction_achieved>=reduction_required, TRUE, FALSE),
         
         ambition_BY_2C=ifelse(TY<=2030, ifelse(annual_ambition_BY_a>TwoC_annual_2030, TRUE, FALSE), 
                               ifelse(TY>2030&TY<=2040, ifelse(annual_ambition_BY_a>TwoC_annual_2040, TRUE, FALSE),
                                      ifelse(annual_ambition_BY_a>TwoC_annual_2050, TRUE, FALSE))),
         ambition_BY_1_5C=ifelse(TY<=2030, ifelse(annual_ambition_BY_a>OneAndAHalfC_annual_2030, TRUE, FALSE), 
                                 ifelse(TY>2030&TY<=2040, ifelse(annual_ambition_BY_a>OneAndAHalfC_annual_2040, TRUE, FALSE),
                                        ifelse(annual_ambition_BY_a>OneAndAHalfC_annual_2050, TRUE, FALSE))),
         ambition_MRY_2C=ifelse(TY<=2030, ifelse(annual_ambition_MRY_a>TwoC_annual_2030, TRUE, FALSE), 
                                ifelse(TY>2030&TY<=2040, ifelse(annual_ambition_MRY_a>TwoC_annual_2040, TRUE, FALSE),
                                       ifelse(annual_ambition_MRY_a>TwoC_annual_2050, TRUE, FALSE))),
         ambition_MRY_1_5C=ifelse(TY<=2030, ifelse(annual_ambition_MRY_a>OneAndAHalfC_annual_2030, TRUE, FALSE), 
                                  ifelse(TY>2030&TY<=2040, ifelse(annual_ambition_MRY_a>OneAndAHalfC_annual_2040, TRUE, FALSE),
                                         ifelse(annual_ambition_MRY_a>OneAndAHalfC_annual_2050, TRUE, FALSE))),
         ambition_BY_potential=ifelse(TY<=2030, ifelse(annual_ambition_BY_a>Potential_annual, TRUE, FALSE), NA),
         ambition_MRY_potential=ifelse(TY<=2030, ifelse(annual_ambition_MRY_a>Potential_annual, TRUE, FALSE), NA)                
  ) %>%
  filter(on_track%in%c(TRUE, FALSE),
         substr(Scope, 1, 7)!='Scope 3',
         !BY_EM%in%c(0,NA,""), 
         !MRY_EM%in%c(0,NA,""), 
         !TY%in%c(0,NA,""))
data_progress$target_year_group <- factor(data_progress$target_year_group, levels=c('0', '1', '2', '3'))
nrow(data_progress)
write.table(data_progress, paste0('data/CDP/output/data_progress_', choice_data_source_year, '.csv'), sep=";", row.names = F)

# only select occurences with quantifiable, scope1+2 targets
data_progress_include = filter(data_progress, Include==TRUE)

# add primary industry
account_sector <- select(CDP_data_raw_summary, `Account number`, `Primary industry`) %>% 
  rename(account_id=`Account number`,
         primary_industry=`Primary industry`) %>%
  distinct()

data_progress_include = left_join(data_progress_include, account_sector, by=c('account_id')) %>%
  mutate(primary_industry=ifelse(is.na(primary_industry), "Other", primary_industry)) %>%
  select(account_id, primary_industry, everything())

#print to screen
n_before_outliers=nrow(data_progress_include)
n_before_outliers
x_before<-filter(data_progress_include, NetZero==TRUE)
print(paste0("NetZero before: ", nrow(x_before)))

# Filter outliers
data_progress_include=filter(data_progress_include, target_year_group%in%c('1', '2', '3'),
                             annual_ambition_BY_a>-100, annual_ambition_BY_a<100,
                             annual_ambition_MRY_a>-100, annual_ambition_MRY_a<100,
                             annual_progress_a>-100, annual_progress_a<100)
#print to screen
n_after_outliers=nrow(data_progress_include)
n_after_outliers
x_after<-filter(data_progress_include, NetZero==TRUE)
print(paste0("NetZero after: ", nrow(x_after)))

# Calculate GHG MRY emissions
tmp <- group_by(data_progress_include, account_id) %>%
  summarise(max_GHG = max(MRY_EM),
            weight=1/n())
data_progress_include <- left_join(data_progress_include, tmp, by=c("account_id"))

# calculate statistics for database after  ing
stats_data_progress_after_filtering <- ungroup(data_progress_include) %>%
  summarise(number_of_targets = n(),
            number_active = sum(TargetActive, na.rm=TRUE),
            number_quantifiable = sum(Quantifiable, na.rm=TRUE),
            number_scope1_2 = sum(Scope1_2, na.rm=TRUE),
            number_full_coverage = sum(Full_coverage, na.rm=TRUE),
            number_included = sum(Include, na.rm=TRUE))
stats_data_progress_after_filtering_net_zero <- ungroup(data_progress_include) %>%
  filter(NetZero==TRUE) %>%
  summarise(number_of_net_zero_targets=n())
stats_data_progress_after_filtering <- cbind(stats_data_progress_after_filtering, stats_data_progress_after_filtering_net_zero)

# calculate percentage of net-zero targets for companies instead of targets
net_zero_per_company <- group_by(data_progress_include, account_id) %>%
  summarise(value=sum(NetZero==TRUE)) %>%
  mutate(net_zero_company=ifelse(value>0, TRUE, FALSE))
stats_data_progress_after_filtering_companies = ungroup(net_zero_per_company) %>%
  summarise(nr_companies=n(),
            nr_net_zero_companies=sum(net_zero_company, na.rm=TRUE))
stats_data_progress_after_filtering <- cbind(stats_data_progress_after_filtering, stats_data_progress_after_filtering_companies) %>%
  mutate(Included="Companies with quantifiable targets") %>%
  select(Included, everything())
stats_data_progress <- rbind(stats_data_progress_total, stats_data_progress_after_filtering)
write.table(stats_data_progress, paste0('data/CDP/output/stats_data_progress_from_', choice_progress, "_", choice_data_source_year, '.csv'), sep=";", row.names = F)

# Calculated indicators
data_progress_include[sapply(data_progress_include, is.infinite)] <- NA

stats_ambition_progress_total <- ungroup(data_progress_include) %>%
  summarise(number_of_targets = n(),
            number_of_companies = n_distinct(account_id),
            total_GHG_emissions = 10^-6*sum(max_GHG*weight),
            data_source_year=choice_data_source_year,
            
            on_track_perc=100*sum(on_track==TRUE)/n(),
            ambition_BY_2C_perc=100*sum(ambition_BY_2C==TRUE)/n(),
            ambition_BY_1_5C_perc=100*sum(ambition_BY_1_5C==TRUE)/n(),
            ambition_MRY_2C_perc=100*sum(ambition_MRY_2C==TRUE)/n(),
            ambition_MRY_1_5C_perc=100*sum(ambition_MRY_1_5C==TRUE)/n(),
            #ambition_BY_potential_perc=100*sum(ambition_BY_potential==TRUE, na.rm=TRUE)/n(),
            #ambition_MRY_potential=100*sum(ambition_MRY_potential==TRUE, na.rm=TRUE)/n(),
            ambition_BY_potential_perc=sum(ambition_BY_potential==TRUE, TY<=2030, na.rm=TRUE)/sum(TY<=2030, na.rm=TRUE),
            ambition_MRY_potential_perc=sum(ambition_MRY_potential==TRUE, TY<=2030, na.rm=TRUE)/sum(TY<=2030, na.rm=TRUE),
            
            progress_average=mean(annual_progress_a, na.rm=TRUE),
            progress_median=median(annual_progress_a, na.rm=TRUE),
            progress_5p=quantile(annual_progress_a, 0.05, na.rm=TRUE),
            progress_95p=quantile(annual_progress_a, 0.95, na.rm=TRUE),
            progress_min=min(annual_progress_a, na.rm=TRUE),
            progress_max=max(annual_progress_a, na.rm=TRUE),
            ambition_BY_average=mean(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_median=median(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_5p=quantile(annual_ambition_BY_a, 0.05, na.rm=TRUE),
            ambition_BY_95p=quantile(annual_ambition_BY_a, 0.95, na.rm=TRUE),
            ambition_BY_min=min(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_max=max(annual_ambition_BY_a, na.rm=TRUE),
            ambition_MRY_average=mean(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_median=median(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_5p=quantile(annual_ambition_MRY_a, 0.05, na.rm=TRUE),
            ambition_MRY_95p=quantile(annual_ambition_MRY_a, 0.95, na.rm=TRUE),
            ambition_MRY_min=min(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_max=max(annual_ambition_MRY_a, na.rm=TRUE),
            diff_required_achieved_median=median(diff_required_achieved_a, na.rm=TRUE),
            maturity_expired_BY_median=median(maturity_expired_BY, na.rm=TRUE)) %>%
  mutate(primary_industry="Total")
stats_ambition_progress_sector <- group_by(data_progress_include, primary_industry) %>%
  summarise(number_of_targets = n(),
            total_GHG_emissions = 10^-6*sum(max_GHG*weight),
            number_of_companies = n_distinct(account_id),
            data_source_year=choice_data_source_year,
            
            on_track_perc=100*sum(on_track==TRUE)/n(),
            ambition_BY_2C_perc=100*sum(ambition_BY_2C==TRUE)/n(),
            ambition_BY_1_5C_perc=100*sum(ambition_BY_1_5C==TRUE)/n(),
            ambition_MRY_2C_perc=100*sum(ambition_MRY_2C==TRUE)/n(),
            ambition_MRY_1_5C_perc=100*sum(ambition_MRY_1_5C==TRUE)/n(),
            #ambition_BY_potential_perc=100*sum(ambition_BY_potential==TRUE, na.rm=TRUE)/n(),
            #ambition_MRY_potential=100*sum(ambition_MRY_potential==TRUE, na.rm=TRUE)/n(),
            ambition_BY_potential_perc=sum(ambition_BY_potential==TRUE, TY<=2030, na.rm=TRUE)/sum(TY<=2030, na.rm=TRUE),
            ambition_MRY_potential_perc=sum(ambition_MRY_potential==TRUE, TY<=2030, na.rm=TRUE)/sum(TY<=2030, na.rm=TRUE),
            
            progress_average=mean(annual_progress_a, na.rm=TRUE),
            progress_median=median(annual_progress_a, na.rm=TRUE),
            progress_5p=quantile(annual_progress_a, 0.05, na.rm=TRUE),
            progress_95p=quantile(annual_progress_a, 0.95, na.rm=TRUE),
            progress_min=min(annual_progress_a, na.rm=TRUE),
            progress_max=max(annual_progress_a, na.rm=TRUE),
            ambition_BY_average=mean(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_median=median(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_5p=quantile(annual_ambition_BY_a, 0.05, na.rm=TRUE),
            ambition_BY_95p=quantile(annual_ambition_BY_a, 0.95, na.rm=TRUE),
            ambition_BY_min=min(annual_ambition_BY_a, na.rm=TRUE),
            ambition_BY_max=max(annual_ambition_BY_a, na.rm=TRUE),
            ambition_MRY_average=mean(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_median=median(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_5p=quantile(annual_ambition_MRY_a, 0.05, na.rm=TRUE),
            ambition_MRY_95p=quantile(annual_ambition_MRY_a, 0.95, na.rm=TRUE),
            ambition_MRY_min=min(annual_ambition_MRY_a, na.rm=TRUE),
            ambition_MRY_max=max(annual_ambition_MRY_a, na.rm=TRUE),
            diff_required_achieved_median=median(diff_required_achieved_a, na.rm=TRUE),
            maturity_expired_BY_median=median(maturity_expired_BY, na.rm=TRUE))
stats_ambition_progress <- rbind(stats_ambition_progress_total, stats_ambition_progress_sector) %>%
  select(primary_industry, everything())

# number of targets per target group
stats_ambition_progress_total_targets <- group_by(data_progress_include, target_year_group) %>%
  summarise(number_of_targets = n()) %>%
  spread(key=target_year_group,value=number_of_targets) %>%
  mutate(primary_industry="Total") %>%
  select(primary_industry, everything())
colnames(stats_ambition_progress_total_targets) <- paste("nr_targets_per_year_group", colnames(stats_ambition_progress_total_targets), sep="_")
colnames(stats_ambition_progress_total_targets)[1] <- "primary_industry"
stats_ambition_progress_sector_targets <- group_by(data_progress_include, target_year_group, primary_industry) %>%
  summarise(number_of_targets = n()) %>%
  spread(key=target_year_group,value=number_of_targets)
colnames(stats_ambition_progress_sector_targets) <- paste("nr_targets_per_year_group", sep="_", colnames(stats_ambition_progress_sector_targets))
colnames(stats_ambition_progress_sector_targets)[1] <- "primary_industry"
stats_ambition_progress_targets <- rbind(stats_ambition_progress_total_targets, stats_ambition_progress_sector_targets)
stats_ambition_progress <- left_join(stats_ambition_progress, stats_ambition_progress_targets, by=c("primary_industry"))

# progress per target group
stats_ambition_progress_total_per_group <- group_by(data_progress_include, target_year_group) %>%
  summarise(progress_median=median(annual_progress_a, na.rm=TRUE)) %>%
  spread(key=target_year_group,value=progress_median) %>%
  mutate(primary_industry="Total") %>%
  select(primary_industry, everything())
colnames(stats_ambition_progress_total_per_group) <- paste("median_progress_per_year_group", colnames(stats_ambition_progress_total_per_group), sep="_")
colnames(stats_ambition_progress_total_per_group)[1] <- "primary_industry"
stats_ambition_progress_sector_per_group <- group_by(data_progress_include, target_year_group, primary_industry) %>%
  summarise(progress_median=median(annual_progress_a, na.rm=TRUE)) %>%
  spread(key=target_year_group,value=progress_median)
colnames(stats_ambition_progress_sector_per_group) <- paste("median_progress_per_year_group", sep="_", colnames(stats_ambition_progress_sector_per_group))
colnames(stats_ambition_progress_sector_per_group)[1] <- "primary_industry"
stats_ambition_progress_per_group <- rbind(stats_ambition_progress_total_per_group, stats_ambition_progress_sector_per_group)

stats_ambition_progress <- left_join(stats_ambition_progress, stats_ambition_progress_per_group, by=c("primary_industry"))

# number of net zero targets
stats_data_ambition_progress_net_zero_total <- ungroup(data_progress_include) %>%
  filter(NetZero==TRUE) %>%
  summarise(number_of_net_zero_targets=n()) %>%
  mutate(primary_industry="Total") %>%
  select(primary_industry, everything())
stats_data_ambition_progress_net_zero_sector <- group_by(data_progress_include, primary_industry) %>%
  filter(NetZero==TRUE) %>%
  summarise(number_of_net_zero_targets=n())
stats_data_ambition_progress_net_zero <- rbind(stats_data_ambition_progress_net_zero_total, stats_data_ambition_progress_net_zero_sector)

stats_ambition_progress <- left_join(stats_ambition_progress, stats_data_ambition_progress_net_zero, by=c("primary_industry"))
write.table(stats_ambition_progress, paste0('data/CDP/output/stats_data_progress_indicators_from_', choice_progress, "_", choice_data_source_year, '.csv'), sep=";", row.names = F)
#---------------------------------