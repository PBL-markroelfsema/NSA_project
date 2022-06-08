library(stringr)
library(readxl)
library(tidyverse)

options(scipen = 999)
CurrentYear = 2020
absolute_data_excel <- read_excel('data/CDP/input/2020_CDP_Country_Specific_Dataset_for_NSA_Report_v2_adjusted.xlsx', sheet = "Absolute ER")

# 0 clean
# 0.a determine correct type
absolute_data_excel$`Covered emissions in base year (metric tons CO2e)` <- as.numeric(absolute_data_excel$`Covered emissions in base year (metric tons CO2e)`)
absolute_data_excel$`Base year emissions (100% of scope)` <- as.numeric(absolute_data_excel$`Base year emissions (100% of scope)`) 
absolute_data_excel$`Base year emissions (100% of scope, excl. scope 3)` <- as.numeric(absolute_data_excel$`Base year emissions (100% of scope, excl. scope 3)`)
absolute_data_excel$`MRY emissions (100% of scope)` <- as.numeric(absolute_data_excel$`MRY emissions (100% of scope)`)
absolute_data_excel$`MRY emissions (100% of scope, excl. scope 3)` <- as.numeric(absolute_data_excel$`MRY emissions (100% of scope, excl. scope 3)`)
absolute_data_excel$`Global S1` <- as.numeric(absolute_data_excel$`Global S1`)
absolute_data_excel$`Target year` <- as.integer(absolute_data_excel$`Target year`)
absolute_data_excel$`Target status in reporting year` <- str_trim(absolute_data_excel$`Target status in reporting year`)
absolute_data_excel$`Country S1` <- as.numeric(absolute_data_excel$`Country S1`)
absolute_data_excel$`Country S2L` <- as.numeric(absolute_data_excel$`Country S2L`)
absolute_data_excel$`Country S2M` <- as.numeric(absolute_data_excel$`Country S2M`)
absolute_data_excel$`Global S1` <- as.numeric(absolute_data_excel$`Global S1`)
absolute_data_excel$`Global S2L` <- as.numeric(absolute_data_excel$`Global S2L`)
absolute_data_excel$`Global S2M` <- as.numeric(absolute_data_excel$`Global S2M`)

#--------------------------------------------

# 1 CLEAN and PROCESS DATA
absolute_data_process <- absolute_data_excel
cat(paste0("absolute_data: ", nrow(absolute_data_process), "\n"))

# remove fields that are not necessary and rename
absolute_data_process <- select(absolute_data_process, -`primary_sector`, -`primary_industry`, -`primary_activity`,
                                -`Please explain (including target coverage)`, -`Possible invalid formulation`, -`Keyword matches`, -`Most recent accounting year end date`,
                                -`Base year emissions data source`, -`Base year emissions data source (excl. scope 3)`, -`MRY year emissions data source`,
                                -`access_datapoint_specific`, -`C0.3 - Country/Region`,
                                -`Estimation method`, -`flag`, -`flag_notes`) %>%
                         rename(Scope_long=`Scope(s) (or Scope 3 category)`,
                                Perc_EM_scope_BY=`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`)
# 1.a. remove target_year <- 2020 and target_status is 'expired' or 'retired'
# and % emissions in Scope, % reduction from base year, Base year, Target year, percent_alloc is NA
absolute_data_process <- filter(absolute_data_process, !`Target year`%in%c(0,NA)) %>% 
                              filter(`Target year`>= CurrentYear) %>% 
                              filter(!`Target status in reporting year`%in%c('Expired', 'Retired')) %>%
                              filter(!is.na(`Targeted reduction from base year (%)`)) %>% 
                              filter(!is.na(`Base year`)) %>%
                              filter(!(`Covered emissions in base year (metric tons CO2e)`%in%c(0, NA) &&
                              `Base year emissions (100% of scope)`%in%c(0, NA) &&
                              `Base year emissions (100% of scope, excl. scope 3)`%in%c(0, NA))) %>%
                              filter(!(`Most recent accounting year`==`Target year`))
cat(paste0("absolute_data after removing old or incomplete targets data: ", nrow(absolute_data_process), "\n"))

# 1.b. TEMP: for two companies emissions are extremely high, these are deleted
#absolute_data_excel <- filter(absolute_data_excel, !company_name%in%delete_companies_names)
#absolute_data_excel <- filter(absolute_data_excel, !account_id%in%delete_companies_account_ID)
#delete_companies_names = c("Sweco AB", "Techteam35")
#delete_companies_account_ID= c(829595, 829599, 829603, 829640, 830933, 846242, 847923, 847937, 847956, 847957, 848264)

# 1.c. Some `Country/Region` values are empty. 
# In 2020 dataset, this can be made equal to incorporation country
# ANDREW --> can you check if my change is correct?
check_CountryRegion <- filter(absolute_data_process, `Country/Region` %in% c("", NA)) %>% select(`Country/Region`, everything())
absolute_data_process<- mutate(absolute_data_process, `Country/Region`=ifelse(is.na(`Country/Region`), incorporation_country, `Country/Region`))
cat(paste0("absolute data after removing empty Country/Regions: ", nrow(absolute_data_process), "\n"))

# 1.d. determine short scope names based on long scope names
# ANDREW --> can you check if my changes are correct? 
Scopes <- c("Scope 1", "Scope 1+3", "Scope 1+2", "Scope 1+2+3", "Scope 2", "Scope 2+3")
Scopes_short <- c("S1", "S12L", "S12M", "S12L3",  "S12M3", "SX", "S12X", "S12X3", "S2L", "S2M", "S2X", "S2X3", "S3", "S13", "S2M3")
scope_info <- data.frame (Scope_short = c("S12M",      "S12X3",       "S1",      "S12M3",       "S12L",      "S2M",     "S12L3",       "S3",      "S2L",     "S12X",      "SX", "S2X3",       "S13",      "S2X"),
                          Scope       = c("Scope 1+2", "Scope 1+2+3", "Scope 1", "Scope 1+2+3", "Scope 1+2", "Scope 2", "Scope 1+2+3", "Scope 3", "Scope 2", "Scope 1+2",  "",  "Scope 2+3", "Scope 1+3", "Scope 2"),
                          Scope_3     = c(FALSE,       FALSE,         FALSE,     FALSE,        FALSE,       FALSE,     FALSE,         TRUE,      FALSE,     FALSE,        TRUE, FALSE,       FALSE,       FALSE),
                          Scope1      = c("S1",        "S1",           "S1",      "S1",         "S1",        "",        "S1",          "",        "",       "S1",          "",   "",          "S1",        ""),
                          Scope2      = c("S2M",        "max",         "",        "S2M",        "S2L",       "S2M",     "S2L",         "",        "S2L",    "max",         "",   "max",       "",          "max"))

absolute_data_process <- mutate(absolute_data_process, Scope_short = str_split(T_ID, "-") %>% map_chr(., 3))
check_Scopes <- filter(absolute_data_process,str_detect(Scope_short, "X")) %>% select(Scope_short, everything())

# Determine scope information 
# If scope contains 'X': 
#   SX: S1 if 'Country S1 > 0
#   S2X: S2M if 'Country 2M > 0
# ANDREW --> can we do that?
# Based on scope info in T_ID determine if there is a scope 1, 2 and/or 3, and which scope 2 (Market/local)
# Determine scope for companies with X in Scope)
absolute_data_process <- mutate(absolute_data_process, Scope_short=ifelse(!(Scope_short=="SX"), Scope_short, ifelse(Scope_short=="SX" & !(`Country S1`==0), "S1", ifelse(Scope_short=="SX" & !(`Country S2L`==0), "S2", ifelse(!(`Country S2M`==0), "S2M", Scope_short)))))
absolute_data_process <- mutate(absolute_data_process, Scope_short=ifelse(Scope_short=="S2X", "S2", Scope_short))
absolute_data_process <- mutate(absolute_data_process, Scope_short=ifelse(Scope_short%in%c("S12X", "S123X"), ifelse(!(`Country S2M`==0), "S2M", ifelse(!(`Country S2L`==0), "S2L", Scope_short)), Scope_short))
absolute_data_process <- mutate(absolute_data_process, Scope_short=ifelse(Scope_short=="S2X", "S2M", Scope_short))

# Determine scope (Scope 1+2, Scope 1 or Scope 2)
absolute_data_process <- left_join(absolute_data_process, scope_info, by=c('Scope_short')) %>% select(-Scope_3)
absolute_data_process$Scope <- factor(absolute_data_process$Scope, levels=Scopes)
absolute_data_process$Scope_short <- factor(absolute_data_process$Scope_short, levels=Scopes_short)
cat(paste0("Absolute data after processing Scopes: ", nrow(absolute_data_process), "\n"))

# 1.e. calculate (percent_alloc) for companies that have only one record the percent_alloc is often NA --> set to 1
# ANDREW --> can you check why percent_allocs are zero in some cases, and if my changes are correct?
check_percent_alloc <- filter(absolute_data_process, percent_alloc %in% c(0, NA)) %>% select(percent_alloc, everything())
cat("Zero alloc: ", nrow(check_percent_alloc), "-->try to calculate\n")
# set percent_alloc to 1 for company/target year/scope combination with only one Region/Country
absolute_data_process1 <- group_by(absolute_data_process, company_name, `Target year`, Scope_short) %>% 
                          filter(n()==1) %>% 
                          mutate(percent_alloc=1)
absolute_data_process2 <- group_by(absolute_data_process, company_name, `Target year`, Scope_short) %>% 
                          filter(n()>1)
absolute_data_process <- rbind(absolute_data_process1, absolute_data_process2)
cat(paste0("absolute data after processing percent_alloc (one country): ", nrow(absolute_data_process), "\n"))

# 1.f. calculate percent_alloc (if possible) for those that are missing (X in scope name)                           
# NOTE: this way most scope 3 targets are removed
absolute_data_process <- mutate(absolute_data_process, Scope_short_tmp=Scope_short)
absolute_data_process$Scope_short_tmp <- factor(absolute_data_process$Scope_short_tmp, levels=Scopes_short)
levels(absolute_data_process$Scope_short_tmp) <- list(S1 = c("S1", "SX", "S13"),
                                                      S2L = c("S2L", "S2X"),                                            
                                                      S2M = c("S2M", "S2M3"),
                                                      S12L = c("S12L", "S12L3", "S12X3", "S12X"),
                                                      S12M = c("S12M", "S12M3"))
absolute_data_process <- mutate(absolute_data_process, 
                                     percent_alloc_tmp = case_when(
                                        Scope_short=="S1" ~ (`Country S1`)/(`Global S1`),
                                        Scope_short=="S2L" ~ ifelse(!is.na(`Country S2L`), (`Country S2L`)/(`Global S2L`),(`Country S2M`)/(`Global S2M`)),
                                        Scope_short=="S2M" ~ ifelse(!is.na(`Country S2M`), (`Country S2M`)/(`Global S2M`), (`Country S2L`)/(`Global S2L`)),
                                        Scope_short=="S12L" ~ ifelse(!is.na(`Country S2L`), (`Country S1`+`Country S2L`)/(`Global S1`+`Global S2L`), (`Country S1`+`Country S2M`)/(`Global S1`+`Global S2M`)),
                                        Scope_short=="S12M" ~ ifelse(!is.na(`Country S2M`),(`Country S1`+`Country S2M`)/(`Global S1`+`Global S2M`), (`Country S1`+`Country S2L`)/(`Global S1`+`Global S2L`)),
                                        TRUE ~ 0))
absolute_data_process <- mutate(absolute_data_process, percent_alloc=ifelse(is.na(percent_alloc), percent_alloc_tmp, percent_alloc))
cat(paste0("absolute data after processing percent_alloc (scopes): ", nrow(absolute_data_process), "\n"))
# delete remaining percent_alloc that is zero or NA
check_percent_alloc2 <- filter(absolute_data_process, percent_alloc %in% c(0, NA)) %>% select(percent_alloc, everything())
write.table(check_percent_alloc2, 'data/CDP/output/check_percent_alloc2.csv', sep=";", row.names = FALSE)

# check again for zero percent_alloc
cat("Zero alloc2: ", nrow(check_percent_alloc2), "\n")
absolute_data_process <- filter(absolute_data_process, !percent_alloc%in% c(0, NA))
cat(paste0("absolute data after removing empty percent_alloc2: ", nrow(absolute_data_process), "\n"))

# END of cleaning and processing
cat("original: ", nrow(absolute_data_excel), "after cleaning and processing: ", nrow(absolute_data_process), "\n")
write.table(absolute_data_process, 'data/CDP/output/absolute_data_process.csv', sep=";", row.names = FALSE)

#--------------------------------------------

# 2. make selection for company --> remove duplicates
# 2.a. find duplicates
# ANDREW could you check why there are duplicates for company, country, scopy, target year, base year combinations?
absolute_data_select <- absolute_data_process
cols_id = c("account_id", "company_name", "Country/Region", "Scope_long", "Target year", "Base year")
check_duplicates1 <- select(absolute_data_select, all_of(cols_id)) %>%
                             group_by(`account_id`, `company_name`, `Country/Region`, Scope_long, `Target year`, `Base year`) %>%
                             summarise(count=n()) %>%
                             filter(count>1)
cat("duplicates: ", nrow(check_duplicates1), "\n")

# 2.b. remove duplicates (for private responses (with NA as Global S1))
absolute_data_select <- group_by(absolute_data_select, account_id, company_name, `Country/Region`, Scope_long, Scope_short, `Target year`, `Base year`) %>% 
                         mutate(`Global S1`=ifelse(is.na(`Global S1`),0,`Global S1`)) %>%
                         top_n(n=1, wt=`Global S1`) %>%
                         top_n(n=1, wt=`Year target was set`) %>%
                         top_n(n=1, wt=`Perc_EM_scope_BY`) %>%
                         top_n(n=1, wt=`Covered emissions in base year (metric tons CO2e)`) %>%
                         top_n(n=1, wt=`Targeted reduction from base year (%)`) %>% # Prairiesky has one target achieved, and one underway --> choose highest targets
                         # below are duplicates that only differ in S1/S2L
                         top_n(n=1, wt=`Global S2L`) %>% # for example Salesforce.com, Inc. - Australia
                         top_n(n=1, wt=`Country S2L`) %>% # for example Salesforce.com, Inc. - United States
                         top_n(n=1, wt=`Country S1`) %>% # for example Equinor. - Norway
  
                         select(account_id, company_name, Scope_long, Scope_short, `Target year`, `Base year`, everything())
write.table(absolute_data_select, 'data/CDP/output/absolute_data_select.csv', sep=";", row.names = FALSE)
cat(paste0("absolute data after removing duplicates: ", nrow(absolute_data_select), "\n"))

# 2.c. check again for duplicates
check_duplicates2 <- select(absolute_data_select, all_of(cols_id)) %>%
                             group_by(`account_id`, `company_name`, `Country/Region`, Scope_long, `Base year`, `Target year`) %>%
                             summarise(count=n()) %>%
                             filter(count>1)
cat(paste0("check duplicates again: ", nrow(check_duplicates2), " (SHOULD BE ZERO!)\n")) 

#------------------------------------------------

# Calculate EXCL_SCOPE3
absolute_data_target_data <- absolute_data_select

# 3a. Scope 3 emissions are excluded, so change "Scope 1+2+3" to "Scope 1+2" and  "Scope 1+3" to  "Scope 1" --> TO DO
# Delete occurrences where Scope is Scope 1 and Country S1=0 or Scope 2 and Country S2L+S2M=0 and percent_alloc is zero or NA --> no calculations of BY emissions possible
absolute_data_target_data <- filter(absolute_data_target_data, !(Scope=="Scope 1" & `Country S1`==0 & percent_alloc%in%c(0, NA)))
absolute_data_target_data <- filter(absolute_data_target_data, !(Scope=="Scope 2" & `Country S2L`+`Country S2M`==0 & percent_alloc%in%c(0, NA)))

# 3b. Calculate help variables (including renaming a few variables due to long names)
# Target only applies to "% emissions in Scope" for BY and MRY emissions and covered by country branch percentage allocation (percent_alloc). 
# Note that "Base year emissions covered by target" already applies to full target and does not need to be multiplied with `% emissions in Scope`
absolute_data_target_data <- rename(absolute_data_target_data, 
                                #BY_EM_target=`Base year emissions covered by target (metric tons CO2e)`,
                                BY_EM_target=`Covered emissions in base year (metric tons CO2e)`,
                                BY_EM_100=`Base year emissions (100% of scope)`, 
                                BY_EM_100_excl_scope3=`Base year emissions (100% of scope, excl. scope 3)`,
                                MRY_EM_100=`MRY emissions (100% of scope)`, 
                                MRY_EM_100_excl_scope3=`MRY emissions (100% of scope, excl. scope 3)`)
absolute_data_target_data <- mutate(absolute_data_target_data, BY_EM_target=BY_EM_target*percent_alloc,
                                    BY_EM_100=BY_EM_100*(1/100)*Perc_EM_scope_BY*percent_alloc, 
                                    BY_EM_100_excl_scope3=BY_EM_100_excl_scope3*(1/100)*Perc_EM_scope_BY*percent_alloc,
                                    MRY_EM_100=MRY_EM_100*(1/100)*Perc_EM_scope_BY*percent_alloc, 
                                    MRY_EM_100_excl_scope3=MRY_EM_100_excl_scope3*(1/100)*Perc_EM_scope_BY*percent_alloc)
absolute_data_target_data <- mutate(absolute_data_target_data, target_type="absolute",
                                    BY_tmp = ifelse(!BY_EM_target%in%c(0,NA), BY_EM_target, 
                                                ifelse(!BY_EM_100_excl_scope3%in%c(0,NA), BY_EM_100_excl_scope3*(1/100), BY_EM_100*(1/100))),
                                   `BaseYearEmissions_excl_scope3`=ifelse(`Country impact (excl. scope 3)`%in%c(0,NA), BY_tmp, 100*`Country impact (excl. scope 3)`/`Targeted reduction from base year (%)`), 
                                   S1=ifelse(Scope1=="S1", `Country S1`, 0),
                                   S2=ifelse(Scope2=="S2M", `Country S2M`, ifelse(Scope2=="S2L", `Country S2L`, 
                                                                               ifelse(Scope2=="max",ifelse(`Country S2M`>`Country S2L`,`Country S2M`,`Country S2L`),0))),
                                   perc_scope1 = ifelse(S1==0 & S2==0,0,S1/(S1+S2)),
                                   ratio_EM_excl_incl_scope3=ifelse(!(BY_EM_100%in%c(0,NA) | BY_EM_100_excl_scope3%in%c(0,NA)),BY_EM_100_excl_scope3/BY_EM_100,
                                                                 ifelse(!(MRY_EM_100%in%c(0,NA) | MRY_EM_100_excl_scope3%in%c(0,NA)),MRY_EM_100_excl_scope3/MRY_EM_100, 1)),
                                   TargetYearEmissions_excl_scope3 = (1-`Targeted reduction from base year (%)`/100)*BaseYearEmissions_excl_scope3,
                                   MRY_EM_excl_scope3_interpolated = ifelse(`Most recent accounting year`==`Base year`, BaseYearEmissions_excl_scope3, BaseYearEmissions_excl_scope3+((`Most recent accounting year`-`Base year`)/(`Target year`-`Base year`))*(TargetYearEmissions_excl_scope3-BaseYearEmissions_excl_scope3)),
                                   MRY_EM_excl_scope3=ifelse(MRY_EM_100%in%c(0,NA) & MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_excl_scope3_interpolated,
                                                          ifelse(!MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_100_excl_scope3,MRY_EM_100*ratio_EM_excl_incl_scope3))
)
absolute_data_target_data <- mutate(absolute_data_target_data, `BaseYearEmissions_excl_scope3`=ifelse(is.na(`BaseYearEmissions_excl_scope3`), 0, `BaseYearEmissions_excl_scope3`))
absolute_data_target_data <- mutate(absolute_data_target_data, `MRY_EM_excl_scope3`=ifelse(is.na(`MRY_EM_excl_scope3`), 0, `MRY_EM_excl_scope3`))
check_perc_scope1_zero <- filter(absolute_data_target_data, is.na(perc_scope1))
cat("records with NA in perc_scope1: ", nrow(check_perc_scope1_zero), "\n")

# 3.c. If perc_scope1 is NA change to 40% for Scope1+2, 100% for Scope 1 and 0% for Scope 2
absolute_data_target_data <- mutate(absolute_data_target_data, perc_scope1 = ifelse(!is.na(perc_scope1), perc_scope1, ifelse(Scope%in%c("Scope 1+2", "Scope 1+2+3"), 0.4, ifelse(Scope%in%c("Scope 1","Scope 1+3"), 1, 0))))
absolute_data_target_data$BaseYearEmissions_excl_scope3 <- as.numeric(absolute_data_target_data$BaseYearEmissions_excl_scope3)
absolute_data_target_data$TargetYearEmissions_excl_scope3 <- as.numeric(absolute_data_target_data$TargetYearEmissions_excl_scope3)
write.table(absolute_data_target_data, "data/CDP/output/absolute_data_target_data.csv", sep=";", col.names = TRUE, row.names=FALSE)
cat(paste0("number of records in target data: ", nrow(absolute_data_target_data), "\n")) 

#-----------------------------

# TO DO!! ##
# 4 calculate BY, MR emissions per company/scope combination
absolute_data_aggregated <- group_by(absolute_data_process, account_id, company_name, Scope_long, `Base year`, `Target year`, `Target coverage`,`Targeted reduction from base year (%)`) %>%
                                 summarise(BY_covered=sum(`Covered emissions in base year (metric tons CO2e)`, na.rm=TRUE),
                                           BY_100=sum(`Base year emissions (100% of scope)`, na.rm=TRUE),
                                           BY_100_excl_scope3=sum(`Base year emissions (100% of scope, excl. scope 3)`, na.rm=TRUE),
                                           MRY_100=sum(`MRY emissions (100% of scope)`, na.rm=TRUE),
                                           MRY_100_scope3=sum(`MRY emissions (100% of scope, excl. scope 3)`, na.rm=TRUE),
                                           nr_countries=n()) %>%
                                 select(nr_countries, everything())
write.table(absolute_data_aggregated, 'data/CDP/output/absolute_data_aggregated.csv', sep=";", row.names = FALSE)

# 3a. select scope
absolute_data_select_scope <- filter(absolute_data_aggregated, substr(Scope_long, 1, 7)!="Scope 3",
                                                                         substr(Scope_long, 1, 5)!="Other")
write.table(absolute_data_select_scope, 'data/CDP/output/absolute_data_select_scope.csv', sep=";", row.names = FALSE)

# 3b --> TO DO: calcualte scope1+2 if both scope1 and scope 2 exist and target same year with same reduction target

# 3c selection
