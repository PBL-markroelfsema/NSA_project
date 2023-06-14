library(stringr)
library(readxl)
library(tidyverse)

########################
# THIS DOES NOT WORK AS WE DO NOT HAVE THE COUNTRY CDP DATA FOR 2021
########################


# This script uses absolute targets data from CDP and processes it
# use for 2021 report Global Climate Action from cities, regions and businesses

source("statistics_CDP.R")

# Description of worksheet 'CDP_companies_commitments_abs1' that links to 'absolute_processed'. In brackets is column in 'absolute_processed', and description is also added how raw data was used for this
# add Industry_Group = organisation, Industry_Group, Country
# add ID = company name, sector, primary_sector	primary_industry	primary_activity
# Organisation = company_name
# Account.No = account_id
# Industry_Group = primary_sector	primary_industry	primary_activity 
# Headquarters = incorporation_country
# Country = Country/Region

# Commitment.Type = {"absolute emissions reduction"}
# Commitment.Scope = (AH) Scope_short {scope 1, scope 1+2, scope 1+2+3, scope 1+3, scope 2, scope 2+3, scope 3}
#        1) Determine (BM)Scope_fromID (based on (AD)T_ID) e.g. "44-abs-S12M-2017-2019-1" --> {S1, S12L, S12L3, S12M, S12M3, S12X, S12X3, S13, S2L, S2M, S2X3, S3, SX, <blanks>}
#        Where, S=scope, numbers = {1, 2, 3},  L=location based , M=market based, X=unkown
#        2) 'Scope short' = TABLE(Scope_fromID, 2) Based on this string determine 
            # Scope	Scope_short	Scope 3 Emissions_scope2
            # S12M	Scope 1+2	  FALSE   S2M
            # S12X3	Scope 1+2+3	FALSE   max
            # S1	  Scope 1	    FALSE
            # S12M3	Scope 1+2+3	FALSE   S2M
            # S12L	Scope 1+2	  FALSE   S2L
            # S2M	  Scope 2	    FALSE   S2M
            # S12L3	Scope 1+2+3	FALSE   S2L
            # S3	  Scope 3	    TRUE
            # S2L	  Scope 2	    FALSE   S2L
            # S12X	Scope 1+2	  FALSE   max
            # SX	  NA	        TRUE
            # S2X3	Scope 2+3	  FALSE   max
            # S13 	Scope 1+3	  FALSE
            # S2X	  Scope 2	    FALSE   max
# Percent.Emissions.In.Scope = (Q)`% emissions in Scope` 
#  --> Thus, 'Base year emissions covered by target (metric tons CO2e)' is linked to this percentage (while 'Base year emissions (100% of scope)' applies to the 100% scope (company-wide) emissions)
# Commitment.Base.Year = (T)Base year
# Renewable.Base.Year (=EMPTY)
# Base.Emissions. (tCO2e) (=EMPTY)
# Base.Emissions.Covered.by.Target (tCO2e) 
# --> these are the total emissions excluding scope 3 that are covered by the target
#     = (AF)'BY emissions (excl scope 3)'
#     = (absolute(BG)'Country impact (excl. scope 3)'0/(absolute(P)'% reduction from base year') --> Orange cells, but in many cases the 'Country impact (excl. scope 3)' is empty, as the scope 3 information
#                                                                                                    is not supplied by the company
#                                                                                                --> but should we leave these out? Or assume that scope 3 is zero?
#     (own calcualtions --> Green cells)
#     = Ratio_without_scope3 x 'Base year emissions covered by target (metric tons CO2e) x percent_alloc_own_calc --> Ratio_without_scope3 is based on 'Scope_from_ID' (S1, S12L, S12L3, S12M3, etc) --> see below
#                                                                                                                 --> percent_alloc_own_calc is not always equal to percent_alloc from CDP calculations (check)
#                                                                                                                 --> question: percent_alloc should be multiple of 1,2,3,4, but this is often not the case. Why?
#     (if zero then)
#     = Ratio_without_scope3 x 'Base year emissions (100% of scope)' x percent_alloc_own_calc
#     
# percent_alloc_own_calc is calculated from 'percent_alloc_scope1' and 'percent_alloc_scope2', which depend on 'EM_country_scope1+2', 'EM_country_scope1', 'EM_country_scope2'
# EM_country_scope1+2 = 'EM_country_scope1'+ 'EM_country_scope1' and 'EM_country_scope2_tmp'
# EM_country_scope1 = absolute(AR)'Country S1' --> IF scope_short="scope 1", "scope 1+2", "scope 1+2+3"
# EM_country_scope2 = EM_country_scope2_tmp  --> IF scope_short="scope 2", "scope 1+2", "scope 1+2+3"
# EM_country_scope2_tmp = IF TABLE(Scope_fromID, 5) ="S2L" THEN absolute(AT)'Country S2L'
#                         IF TABLE(Scope_fromID,5) = "S2M" THEN absolute(AU)'Country S2M', 
#                         IF TABLE(Scope_fromID,5) = "min" THEN minimum of the two above
#                         ELSE 0
# Country S2X
# Global S2X

#Sweco AB is a first time responder to CDP in 2021. They have not provided country level scope 1/2 emission breakdowns, and have also entered 99999999 into each of their global scope 1/2 inventory values, so I would not consider the information they have entered to be reliable for the analysis.
#Techteam35 should not be included in the analysis, as well as the following data test accounts that were accidentally included in the country-specific output:
#829595
#829599
#829603
#829640
#830933
#846242
#847923
#847937
#847956
#847957
#848264

# steps
# absolute_2021_data_excel
# absolute_2021_data
# absolute_2021_targets
# absolute_2021_targets_remove_duplicates

delete_companies_names = c("Sweco AB", "Techteam35")
delete_companies_account_ID= c(829595, 829599, 829603, 829640, 830933, 846242, 847923, 847937, 847956, 847957, 848264)

# 1 orginal data
options(scipen = 999)
# (X) Read in CDP data from Excel

#absolute_2021_data_excel <- read_excel('data/CDP/input/2021_CDP_Country_Specific_Dataset_for_NSA_Report_v2_adjusted.xlsx', sheet = "Absolute ER")
# In version v3 changes have been made for Global S1, S2M, S2L where the sum of the countries was higher than global 
# Based on check_percent_alloc which is calculated in this Scipt
absolute_2021_data_excel <- read_excel('data/CDP/input/CDP_CCTD_2021_abs_ER_public.xlsx', sheet = "Absolute ER")

absolute_2021_data_excel <- as.data.frame(absolute_2021_data_excel)
absolute_2021_data_excel$`Target year` <- as.integer(absolute_2021_data_excel$`Target year`)
absolute_2021_data_excel$`Target status in reporting year` <- str_trim(absolute_2021_data_excel$`Target status in reporting year`)

# (X) "private response' has already been removed from the original Excel file (and name now ends with 'adjusted') --> R reads them as text
absolute_2021_data_excel$`Country S1` <- as.numeric(absolute_2021_data_excel$`Country S1`)
absolute_2021_data_excel$`Country S2L` <- as.numeric(absolute_2021_data_excel$`Country S2L`)
absolute_2021_data_excel$`Country S2M` <- as.numeric(absolute_2021_data_excel$`Country S2M`)
absolute_2021_data_excel$`Global S1` <- as.numeric(absolute_2021_data_excel$`Global S1`)
absolute_2021_data_excel$`Global S2L` <- as.numeric(absolute_2021_data_excel$`Global S2L`)
absolute_2021_data_excel$`Global S2M` <- as.numeric(absolute_2021_data_excel$`Global S2M`)
absolute_2021_data_excel$`Base year emissions (100% of scope)` <- as.numeric(absolute_2021_data_excel$`Base year emissions (100% of scope)`)
absolute_2021_data_excel$`Base year emissions (100% of scope, excl. scope 3)` <- as.numeric(absolute_2021_data_excel$`Base year emissions (100% of scope, excl. scope 3)`)
absolute_2021_data_excel$`MRY emissions (100% of scope)` <- as.numeric(absolute_2021_data_excel$`MRY emissions (100% of scope)`)
absolute_2021_data_excel$`MRY emissions (100% of scope, excl. scope 3)` <- as.numeric(absolute_2021_data_excel$`MRY emissions (100% of scope, excl. scope 3)`)


# (X) TEMP: for two companies emissions are extremely high, these are deleted
absolute_2021_data_excel <- filter(absolute_2021_data_excel, !company_name%in%delete_companies_names)
absolute_2021_data_excel <- filter(absolute_2021_data_excel, !account_id%in%delete_companies_account_ID)

# TEMPORARILY DELETE
# SAP SE reduction = 100
absolute_2021_data_excel <- filter(absolute_2021_data_excel, !(account_id==16339 & `Is this a science-based target?`=="No, but we are reporting another target that is science-based"))

cat("absolute_2021_data_excel:\n")
nrow(absolute_2021_data_excel)
length(unique(absolute_2021_data_excel$account_id))

#StatCDP(absolute_2021_data_excel, "original_2021")

# 2. (X) clean data
Scopes <- c("Scope 1", "Scope 1+3", "Scope 1+2", "Scope 1+2+3", "Scope 2", "Scope 2+3")
Scopes_short      = c("S1", "S12L", "S12M", "S12L3",  "S12M3", "SX", "S12X", "S12X3", "S2L", "S2M", "S2X", "S2X3", "S3", "S13", "S2M3")


# (X) 2a. Some `Country/Region` values are empty. CHECK. But in 2021 dataset, this can be made equal to incorporation country
absolute_2021_data<- mutate(absolute_2021_data_excel, `Country/Region`=ifelse(is.na(`Country/Region`), incorporation_country, `Country/Region`))

# (X) 2b-e: remove data with target year < 2021 and target_status is 'expired' or 'retired'
# and % emissions in Scope, % reduction from base year, Base year, Target year, percent_alloc is NA
absolute_2021_data <- filter(absolute_2021_data, !`Target year`%in%c(0,NA)) %>% 
                      filter(`Target year`>=2021) %>% 
                      #filter(!str_detect(`Country/Region`, "^Other")) %>% # Country/Region starts with 'Other
                      filter(!`Target status in reporting year`%in%c('Expired', 'Retired')) %>%
                      #filter(!is.na(`% emissions in Scope`)) %>% 
                      filter(!is.na(`Targeted reduction from base year (%)`)) %>% 
                      filter(!is.na(`Base year`)) %>%
                      filter(!(`Covered emissions in base year (metric tons CO2e)`%in%c(0, NA) &
                              `Base year emissions (100% of scope)`%in%c(0, NA) &
                              `Base year emissions (100% of scope, excl. scope 3)`%in%c(0, NA))) %>%
                      filter(!(`Most recent accounting year`==`Target year`))
                      #filter(!percent_alloc%in%c(0,NA))
                      #filter(!is.na(`Target year`))

StatCDP(absolute_2021_data, "after_process_2021")

cat("absolute_2021_data:\n")
nrow(absolute_2021_data)
length(unique(absolute_2021_data$account_id))

# (X) 2c. (percent_alloc) for companies that have only one record the percent_alloc is often NA --> set to 1
# first add scope_short based on T_ID and delete scope 3
absolute_2021_data <- mutate(absolute_2021_data, Scope_short = str_split(T_ID, "-") %>% map_chr(., 3)) %>%
                      filter(!(Scope_short=="S3"))
# set percent_alloc to 1 for company/target year/scope combination with only one Region/Country
absolute_2021_data1 <- group_by(absolute_2021_data, company_name, `Target year`, Scope_short) %>% filter(n()==1) %>% 
                       mutate(percent_alloc=1) #), 
absolute_2021_data2 <- group_by(absolute_2021_data, company_name, `Target year`, Scope_short) %>% filter(n()>1)
absolute_2021_data <- rbind(absolute_2021_data1, absolute_2021_data2)

# (X) calculate percent_alloc for those that are missing (X in scope name)                           
absolute_2021_data <- mutate(absolute_2021_data, Scope_short_tmp=Scope_short)
absolute_2021_data$Scope_short_tmp <- factor(absolute_2021_data$Scope_short_tmp, levels=Scopes_short)
levels(absolute_2021_data$Scope_short_tmp) <- list(
  S1 = c("S1", "SX", "S13"),
  S2L = c("S2L", "S2X"),                                            
  S2M = c("S2M", "S2M3"),
  S12L = c("S12L", "S12L3", "S12X3", "S12X"),
  S12M = c("S12M", "S12M3")
                                               
)
write.table(absolute_2021_data, "data/CDP/output/absolute_2021_data_tmp1.csv", sep=";", col.names = TRUE, row.names=FALSE)

# There are many duplicates (one with Country/Global S1/S2M/S2L information, and one row without) --> they are removed further down the script
# But to do this in a more controlled way, this could be changed --> TO DO
cols_id_tmp <- c("account_id", "Country/Region", "Target reference number") # only one target should exist for this ID
absolute_2021_data_tmp <- group_by_at(absolute_2021_data, cols_id_tmp) %>% 
                          top_n(n=1, wt=`Global S1`) %>%
                          top_n(n=1, wt=`Global S2L`) %>%
                          top_n(n=1, wt=`Global S2M`)
write.table(absolute_2021_data_tmp, "data/CDP/output/absolute_2021_data_tmp.csv", sep=";", col.names = TRUE, row.names=FALSE) 
write.table(absolute_2021_data, "data/CDP/output/absolute_2021_data.csv", sep=";", col.names = TRUE, row.names=FALSE) 
check_percent_alloc <- group_by(absolute_2021_data, account_id, `Target reference number`, `Country/Region`) %>%
                          summarise(check_percent_alloc_S1=sum(`Country S1`, na.rm=T)/mean(`Global S1`, na.rm=T),
                                    check_percent_alloc_S2L=sum(`Country S2L`, na.rm=T)/mean(`Global S2L`, na.rm=T),
                                    check_percent_alloc_S2M=sum(`Country S2M`, na.rm=T)/mean(`Global S2M`, na.rm=T))
check_percent_alloc <- group_by(check_percent_alloc, account_id, `Target reference number`) %>%
                          summarise(check_percent_alloc_S1=sum(check_percent_alloc_S1),
                                    check_percent_alloc_S2L=sum(check_percent_alloc_S2L),
                                    check_percent_alloc_S2M=sum(check_percent_alloc_S2M))
check_percent_alloc <- filter(check_percent_alloc, (check_percent_alloc_S1!=0 & (check_percent_alloc_S1<0.9|check_percent_alloc_S1>1.1)) |
                                                         (check_percent_alloc_S2L!=0 & (check_percent_alloc_S2L<0.9|check_percent_alloc_S2L>1.1)) |
                                                         (check_percent_alloc_S2M!=0 & (check_percent_alloc_S2M<0.9|check_percent_alloc_S2M>1.1))) #%>%
write.table(check_percent_alloc, "data/CDP/output/check_percent_alloc.csv", sep=";", col.names = TRUE, row.names=FALSE)

absolute_2021_data <- mutate(absolute_2021_data, percent_alloc_tmp = case_when(
  Scope_short_tmp=="S1" ~ (`Country S1`)/(`Global S1`),
  Scope_short_tmp=="S2L" ~ ifelse(!is.na(`Country S2L`), (`Country S2L`)/(`Global S2L`),(`Country S2M`)/(`Global S2M`)),
  Scope_short_tmp=="S2M" ~ ifelse(!is.na(`Country S2M`), (`Country S2M`)/(`Global S2M`), (`Country S2L`)/(`Global S2L`)),
  Scope_short_tmp=="S12L" ~ ifelse(!is.na(`Country S2L`), (`Country S1`+`Country S2L`)/(`Global S1`+`Global S2L`), (`Country S1`+`Country S2M`)/(`Global S1`+`Global S2M`)),
  Scope_short_tmp=="S12M" ~ ifelse(!is.na(`Country S2M`),(`Country S1`+`Country S2M`)/(`Global S1`+`Global S2M`), (`Country S1`+`Country S2L`)/(`Global S1`+`Global S2L`)),
  TRUE ~ 0
  )
)
#write.table(absolute_2021_data, "data/CDP/output/tmp1.csv", sep=";", col.names = TRUE, row.names=FALSE)

absolute_2021_data <- mutate(absolute_2021_data, percent_alloc=ifelse(percent_alloc %in% c(0, NA), percent_alloc_tmp, percent_alloc))
write.table(absolute_2021_data, "data/CDP/output/absolute_2021_data_tmp2.csv", sep=";", col.names = TRUE, row.names=FALSE)

# (X) delete remaining percent_alloc that is zero or NA
tmp<-filter(absolute_2021_data, is.na(percent_alloc))
nrow(tmp)
tmp<-filter(absolute_2021_data, percent_alloc==0)
nrow(tmp)
tmp<-filter(absolute_2021_data, is.na(percent_alloc) | percent_alloc==0 | percent_alloc==Inf)
nrow(tmp)
absolute_2021_data <- filter(absolute_2021_data, !(is.na(percent_alloc) | percent_alloc==0 | percent_alloc==Inf))
nrow(absolute_2021_data)
# remove ; because this is inconvenient for csv files. TEMP --> does not work, so remove columsn
absolute_2021_data <- select(absolute_2021_data, -`Please explain (including target coverage)`)
write.table(absolute_2021_data, "data/CDP/output/absolute_2021_data.csv", sep=";", col.names = TRUE, row.names=FALSE)

#--------------------------------------------

# PROCESS DATA
absolute_2021_targets <- rename(absolute_2021_data, `% emissions in Scope`=`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
                                   Scope=`Scope(s) (or Scope 3 category)`,
                                   `Base year emissions covered by target (metric tons CO2e)`=`Covered emissions in base year (metric tons CO2e)`,
                                   `% reduction from base year`=`Targeted reduction from base year (%)`)
# (selection) select columns
cols_absolute_2021 <- #c("company_name", "incorporation_country", "Country/Region", "sectors", "industries", "activities",
                   c("account_id", "company_name", "incorporation_country", "Country/Region", "primary_sector", "primary_industry", "primary_activity",
                   "Base year", "% reduction from base year", "Target year",
                   "Base year emissions covered by target (metric tons CO2e)", "Base year emissions (100% of scope)", "Base year emissions (100% of scope, excl. scope 3)",
                   "Most recent accounting year", 'MRY emissions (100% of scope)', 'MRY emissions (100% of scope, excl. scope 3)',
                   "Country impact (excl. scope 3)", 
                   "percent_alloc", "% emissions in Scope", "Scope",
                   "Country S1", "Country S2L", "Country S2M", "Global S1", "Global S2L", "Global S2M", "T_ID", "Target coverage", "Year target was set", "Is this a science-based target?", "% of target achieved [auto-calculated]")

# (???) select and calculate necessary variables
absolute_2021_targets <- select(absolute_2021_targets, cols_absolute_2021) %>% rename(Scope_long=Scope)
tc1 <- unique(absolute_2021_targets$`Target coverage`)
tc2 <- c("Company-wide", "Business activity", "Business division", "Site/facility","Country/region")
tc1 <- tc1[!tc1%in%tc2]
tc <- c(tc2, tc1)
absolute_2021_targets$`Target coverage` <- factor(absolute_2021_targets$`Target coverage`, levels=tc)

# 3a. scope information
# Based on scope info in T_ID determine if there is a scope 1, 2 and/or 3, and which scope 2 (Market/local)
scope_info <- data.frame (
              Scope_short     = c("S12M",      "S12X3",       "S1",      "S12M3",       "S12L",      "S2M",     "S12L3",       "S3",      "S2L",     "S12X",      "SX", "S2X3",       "S13",      "S2X"),
              Scope           = c("Scope 1+2", "Scope 1+2+3", "Scope 1", "Scope 1+2+3", "Scope 1+2", "Scope 2", "Scope 1+2+3", "Scope 3", "Scope 2", "Scope 1+2",  "",  "Scope 2+3", "Scope 1+3", "Scope 2"),
              Scope_3         = c(FALSE,       FALSE,         FALSE,     FALSE,        FALSE,       FALSE,     FALSE,         TRUE,      FALSE,     FALSE,        TRUE, FALSE,       FALSE,       FALSE),
              Scope1          = c("S1",        "S1",           "S1",      "S1",         "S1",        "",        "S1",          "",        "",       "S1",          "",   "",          "S1",        ""),
              Scope2          = c("S2M",        "max",         "",        "S2M",        "S2L",       "S2M",     "S2L",         "",        "S2L",    "max",         "",   "max",       "",          "max"))

# Determine scope for companies with X in Scope
absolute_2021_targets <- mutate(absolute_2021_targets, Scope_short = str_split(T_ID, "-") %>% map_chr(., 3))
tmp <- filter(absolute_2021_targets, str_detect(Scope_short, "X"))
absolute_2021_targets <- mutate(absolute_2021_targets, Scope_short=ifelse(!(Scope_short=="SX"), Scope_short, ifelse(Scope_short=="SX" & !(`Country S1`==0), "S1", ifelse(Scope_short=="SX" & !(`Country S2L`==0), "S2", ifelse(!(`Country S2M`==0), "S2M", Scope_short)))))
absolute_2021_targets <- mutate(absolute_2021_targets, Scope_short=ifelse(Scope_short=="S2X", "S2", Scope_short))
absolute_2021_targets <- mutate(absolute_2021_targets, Scope_short=ifelse(Scope_short%in%c("S12X", "S123X"), ifelse(!(`Country S2L`==0), "S2L", ifelse(!(`Country S2M`==0), "S2M", Scope_short)), Scope_short))
absolute_2021_targets <- mutate(absolute_2021_targets, Scope_short=ifelse(Scope_short=="S2X", "S2", Scope_short))

# Determine scope (Scope 1+2, Scope 1 or Scope 2)
absolute_2021_targets <- left_join(absolute_2021_targets, scope_info, by=c('Scope_short')) %>% select(-Scope_3)
absolute_2021_targets$Scope <- factor(absolute_2021_targets$Scope, levels=Scopes)
absolute_2021_targets$Scope_short <- factor(absolute_2021_targets$Scope_short, levels=Scopes_short)

nrow(absolute_2021_targets)

## 3b. calculate percent_alloc for those occurences where this is zero/empty
#absolute_2021_targets <- mutate(absolute_2021_targets, `Country S2L` = ifelse(is.na(`Country S2L`), 0, `Country S2L`),
#                                                       `Global S2L` = ifelse(is.na(`Global S2L`), 0, `Global S2L`),
#                                                       `Country S2M` = ifelse(is.na(`Country S2M`), 0, `Country S2M`),
#                                                       `Global S2M` = ifelse(is.na(`Global S2M`), 0, `Global S2M`))
#absolute_2021_targets <- mutate(absolute_2021_targets, `Country S2L` = ifelse(`Country S2L`==0, `Country S2M`, `Country S2L`),
#                                                       `Global S2L` = ifelse(`Global S2L`==0, `Global S2M`, `Global S2L`),
#                                                       `Country S2M` = ifelse(`Country S2M`==0, `Country S2L`, `Country S2M`),
#                                                       `Global S2M` = ifelse(`Global S2M`==0, `Global S2L`, `Global S2M`),
#                                                        N1 = ifelse(Scope1=="S1",  `Country S1`,0),
#                                                        N2 = ifelse(Scope2=="S2L", `Country S2L`, ifelse(Scope2=="S2M", `Country S2M`,0)),
#                                                        D1 = ifelse(Scope1=="S1",  `Global S1`,0),
#                                                        D2 = ifelse(Scope2=="S2L", `Global S2L`, ifelse(Scope2=="S2M", `Global S2M`,0)),
#                                                        percent_alloc_tmp=(N1+N2)/(D1+D2))
#check <- select(absolute_2021_targets, company_name, `Country/Region`, percent_alloc, percent_alloc_tmp) %>% mutate(diff_perc=round(100*(percent_alloc_tmp/percent_alloc-1),1))
#absolute_2021_targets <- mutate(absolute_2021_targets, percent_alloc=ifelse(is.na(percent_alloc) | percent_alloc==0, percent_alloc_tmp, percent_alloc)) %>%
#                         select(-N1, -N2, -D1, -D2, -percent_alloc_tmp)
#write.table(absolute_2021_targets, "data/CDP/output/absolute_2021_check_percent_alloc.csv", sep=";", col.names = TRUE, row.names=FALSE)

# 4. Calculate EXCL_SCOPE3
# 4a. Scope 3 emissions are excluded, so change "Scope 1+2+3" to "Scope 1+2" and  "Scope 1+3" to  "Scope 1" --> TO DO
# Delete occurrences where Scope is Scope 1 and Country S1=0 or Scope 2 and Country S2L+S2M=0
nrow(absolute_2021_targets)
absolute_2021_targets <- filter(absolute_2021_targets, !(Scope=="Scope 1" & `Country S1`==0 & percent_alloc%in%c(0, NA)))
absolute_2021_targets <- filter(absolute_2021_targets, !(Scope=="Scope 2" & `Country S2L`+`Country S2M`==0 & percent_alloc%in%c(0, NA)))
nrow(absolute_2021_targets)

# (X) 4b. Calculate help variables (including renaming a few variables due to long names)
# Target only applies to "% emissions in Scope" for BY and MRY emissions and covered by country branch percentage allocation (percent_alloc). 
# Note that "Base year emissions covered by target" already applies to full target and does not need to be multiplied with `% emissions in Scope`
absolute_2021_targets <- rename(absolute_2021_targets, BY_EM_target=`Base year emissions covered by target (metric tons CO2e)`,
                                                       BY_EM_100=`Base year emissions (100% of scope)`, 
                                                       BY_EM_100_excl_scope3=`Base year emissions (100% of scope, excl. scope 3)`,
                                                       MRY_EM_100=`MRY emissions (100% of scope)`, 
                                                       MRY_EM_100_excl_scope3=`MRY emissions (100% of scope, excl. scope 3)`)
absolute_2021_targets <- mutate(absolute_2021_targets, BY_EM_target=BY_EM_target*percent_alloc,
                                                       BY_EM_100=BY_EM_100*(1/100)*`% emissions in Scope`*percent_alloc, 
                                                       BY_EM_100_excl_scope3=BY_EM_100_excl_scope3*(1/100)*`% emissions in Scope`*percent_alloc,
                                                       MRY_EM_100=MRY_EM_100*(1/100)*`% emissions in Scope`*percent_alloc, 
                                                       MRY_EM_100_excl_scope3=MRY_EM_100_excl_scope3*(1/100)*`% emissions in Scope`*percent_alloc)
absolute_2021_targets <- mutate(absolute_2021_targets, target_type="absolute",
                                BY_tmp = ifelse(!BY_EM_target%in%c(0,NA), BY_EM_target, 
                                               ifelse(!BY_EM_100_excl_scope3%in%c(0,NA), BY_EM_100_excl_scope3*(1/100), BY_EM_100*(1/100))),
                                BaseYearEmissions_excl_scope3=ifelse(`Country impact (excl. scope 3)`%in%c(0,NA), BY_tmp, 100*`Country impact (excl. scope 3)`/`% reduction from base year`), 
                                                       S1=ifelse(Scope1=="S1", `Country S1`, 0),
                                                       S2=ifelse(Scope2=="S2M", `Country S2M`, ifelse(Scope2=="S2L", `Country S2L`, 
                                                          ifelse(Scope2=="max",ifelse(`Country S2M`>`Country S2L`,`Country S2M`,`Country S2L`),0))),
                                perc_scope1 = ifelse(S1==0 & S2==0,0,S1/(S1+S2)),
                                ratio_EM_excl_incl_scope3=ifelse(!(BY_EM_100%in%c(0,NA) | BY_EM_100_excl_scope3%in%c(0,NA)),BY_EM_100_excl_scope3/BY_EM_100,
                                                                ifelse(!(MRY_EM_100%in%c(0,NA) | MRY_EM_100_excl_scope3%in%c(0,NA)),MRY_EM_100_excl_scope3/MRY_EM_100, 1)),
                                TargetYearEmissions_excl_scope3 = (1-`% reduction from base year`/100)*BaseYearEmissions_excl_scope3,
                                MRY_EM_excl_scope3_interpolated = ifelse(`Most recent accounting year`==`Base year`, BaseYearEmissions_excl_scope3, BaseYearEmissions_excl_scope3+((`Most recent accounting year`-`Base year`)/(`Target year`-`Base year`))*(TargetYearEmissions_excl_scope3-BaseYearEmissions_excl_scope3)),
                                MRY_EM_excl_scope3=ifelse(MRY_EM_100%in%c(0,NA) & MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_excl_scope3_interpolated,
                                                          ifelse(!MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_100_excl_scope3,MRY_EM_100*ratio_EM_excl_incl_scope3))
                               )
absolute_2021_targets <- mutate(absolute_2021_targets, `BaseYearEmissions_excl_scope3`=ifelse(is.na(`BaseYearEmissions_excl_scope3`), 0, `BaseYearEmissions_excl_scope3`))
absolute_2021_targets <- mutate(absolute_2021_targets, `MRY_EM_excl_scope3`=ifelse(is.na(`MRY_EM_excl_scope3`), 0, `MRY_EM_excl_scope3`))

# If perc_scope1 is NA change to 40% for Scope1+2, 100% for Scope 1 and 0% for Scope 2
avg_scope=0.4
absolute_2021_targets <- mutate(absolute_2021_targets, perc_scope1 = ifelse(!is.na(perc_scope1), perc_scope1, ifelse(Scope%in%c("Scope 1+2", "Scope 1+2+3"), avg_scope, ifelse(Scope%in%c("Scope 1","Scope 1+3"), 1, 0))))

absolute_2021_targets$BaseYearEmissions_excl_scope3 <- as.numeric(absolute_2021_targets$BaseYearEmissions_excl_scope3)
absolute_2021_targets$TargetYearEmissions_excl_scope3 <- as.numeric(absolute_2021_targets$TargetYearEmissions_excl_scope3)
write.table(absolute_2021_targets, "data/CDP/output/absolute_2021_targets.csv", sep=";", col.names = TRUE, row.names=FALSE)

cat("absolute_2021_targets:\n")
nrow(absolute_2021_targets)
length(unique(absolute_2021_targets$account_id))

# REMOVE DUPCLIATES
# CHECK NAs in `Most recent accounting year`, `% reduction from base year`, `% emissions in Scope`, Scope_long, `Country impact (excl. scope 3)`
tmp1 <- filter(absolute_2021_targets, is.na(`Most recent accounting year`))
tmp2 <- filter(absolute_2021_targets, is.na(`% reduction from base year`))
tmp3 <- filter(absolute_2021_targets, is.na(`% emissions in Scope`))
#tmp4 <- filter(absolute_2021_targets, is.na(Scope_long))
#tmp5 <- filter(absolute_2021_targets, is.na(`Country impact (excl. scope 3)`))
absolute_2021_remove <- rbind(tmp1, tmp2, tmp3)
write.table(absolute_2021_remove, "data/CDP/output/absolute_2021_removed.csv", sep=";", col.names = TRUE, row.names=FALSE)

nrow(absolute_2021_remove)

# some records have multiple occurences for base year, inventory year
# in some case there are still multiple records meeting this selection criteria, then we select the hightest 'Base year emissions covered by target (metric tons CO2e)'
# and if still duplicates exist, '% reduction from base year', 'Country S1', 'Country S2L', 'Country S2M'
# cols_id <- c("company_name", "Country/Region", "sectors",	"industries",	"activities", "Target year", "target_type", "Scope") # only one target should exist for this ID
# make sure `Country impact (excl. scope 3)` is not NA
absolute_2021_targets_remove_duplicates <- mutate(absolute_2021_targets, `Country impact (excl. scope 3)`=ifelse(is.na(`Country impact (excl. scope 3)`), 0, `Country impact (excl. scope 3)`))
nrow(absolute_2021_targets_remove_duplicates)
cols_id <- c("account_id", "company_name", "Country/Region", "primary_sector",	"primary_industry",	"primary_activity", "Target year", "target_type", "Scope") # only one target should exist for this ID
cols_other <- colnames(absolute_2021_targets)
cols_other <- cols_other[!cols_other%in%cols_id]
# count number of duplicates
test_duplicates <- group_by_at(absolute_2021_targets_remove_duplicates, all_of(cols_id)) %>% 
                   summarise(c=n()) %>%
                   filter(c>1)
cat(paste0("maximum duplicates is ", max(test_duplicates$c), "\n"))
count_duplicates <- group_by(test_duplicates, c) %>% summarise(count_count=n())

Science_based <- c("Yes, this target has been approved as science-based by the Science-Based Targets initiative",
                   "Yes, we consider this a science-based target, but this target has not been approved as science-based by the Science-Based Targets initiative", 
                   "No, but we anticipate setting one in the next 2 years" , 
                   "No, and we do not anticipate setting one in the next 2 years",
                   "No, but we are reporting another target that is science-based")
absolute_2021_targets_remove_duplicates$`Is this a science-based target?` <- factor(absolute_2021_targets_remove_duplicates$`Is this a science-based target?`, levels=Science_based)
absolute_2021_targets_remove_duplicates <- select(absolute_2021_targets_remove_duplicates, all_of(cols_id), everything()) %>%
                                           group_by_at(cols_id) %>% 
                                           #top_n(n=1, wt=`Is this a science-based target?`) %>% 
                                           top_n(n=1, wt=`Target coverage`) %>% 
                                           top_n(n=1, wt=`Year target was set`) %>% 
                                           top_n(n=1, wt=`Most recent accounting year`) %>% # year to which MRY emissions apply
                                           top_n(n=1, wt=`% emissions in Scope`) %>%
                                           top_n(n=1, wt=`% reduction from base year`) %>%
                                           top_n(n=1, wt=Scope_short) %>%
                                           top_n(n=1, wt=S1) %>%
                                           top_n(n=1, wt=`Country impact (excl. scope 3)`) #%>% if still duplicate exists, take the one with highest impact

cat("absolute_2021_targets_remove_duplicates:\n")
nrow(absolute_2021_targets_remove_duplicates)
length(unique(absolute_2021_targets_remove_duplicates$account_id))

write.table(absolute_2021_targets_remove_duplicates, "data/CDP/output/absolute_2021_targets_remove_duplicates.csv", sep=";", col.names = TRUE, row.names=FALSE)
absolute_2021_duplicates <- anti_join(absolute_2021_targets, absolute_2021_targets_remove_duplicates, by=cols_id)
write.table(absolute_2021_duplicates, "data/CDP/output/absolute_2021_duplicates.csv", sep=";", col.names = TRUE, row.names=FALSE)

cat("absolute_2021_duplicates:\n")
nrow(absolute_2021_duplicates)
length(unique(absolute_2021_duplicates$account_id))

