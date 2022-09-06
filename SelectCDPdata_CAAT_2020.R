library(stringr)
library(tidyverse)
library(plotly)

# This script used the processed CDP data from ReadProcessCDPData_2020 and translates it into a table that can be used in the CAAT tool

source('ReadProcessCDPData_2020.R')

#----------------------------------
# SELECT TWO TARGETS per company/country


T_select = 2035

countries_EU = c("Austria", "Baltic States", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Czechia", "Denmark", "Estonia",  "Finland", "France", "Germany", 
                 "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", 
                 "Sint Maarten (Dutch part)", "Greenland", "Slovakia", "Slovenia", "Spain",  "Sweden", 
                 "United Kingdom", "United Kingdom of Great Britain and Northern Ireland", "Benelux",
                 "EU12", "EU28", "EU25", "EU15", "Europe")
#countries_include = c(countries_EU, 
#                      "Argentina", "China", "China, Hong Kong Special Administrative Region", "China, Macao Special Administrative Region", "Macau", "Hong Kong", 
#                      "Brazil", "Canada", "India", "Indonesia", "Ireland", "Japan", "Mexico", "South Africa", "United States of America")
countries_include <- unique(absolute_2020_targets_remove_duplicates$`Country/Region`)                    
countries = unique(absolute_2020_targets_remove_duplicates$`Country/Region`)
countries_exclude = countries[!countries%in%countries_include] 
  
# 1. CHECKS. first check number of unique rows (company_name, `Country/Region`)
#test <- mutate(absolute_targets_per_row2, key=paste0(company_name, "-", `Country/Region`)) %>% group_by(key)
test <- mutate(absolute_2020_targets_remove_duplicates, key=paste0(company_name, "-", `Country/Region`)) %>% group_by(key)
n_test <- sum(!duplicated(test$key))

#data_select <- absolute_2020_targets_per_row2
data_select <-filter(absolute_2020_targets_remove_duplicates, `Country/Region`%in%countries_include)

# 2. CHANGE DATA. 
# Change country names
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="Czechia", "Czech Republic", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="Sint Maarten (Dutch part)", "Netherlands", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="China, Hong Kong Special Administrative Region", "China", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="China, Macao Special Administrative Region", "China", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="Macau", "China", `Country/Region`))
data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="Hong Kong", "China", `Country/Region`))
#data_select <- mutate(data_select, `Country/Region`=ifelse(`Country/Region`=="Côte d'Ivoire", "Cote d'Ivoire", `Country/Region`))


# 3. SELECT Targets
# 3.1 select target closes to T_select, if the result is two, smallest is chosen
data_select1a <- mutate(data_select, select1 = abs(`Target year`-T_select))
# order target year and scope in order of priority for selection
data_select1a$Scope <- factor(data_select1a$Scope, levels=Scopes)
data_select1a <- arrange(data_select1a, company_name, `Country/Region`, `Target year`, Scope) %>% 
                 group_by(company_name, `Country/Region`) %>%
                 slice_min(n=1, order_by=select1, with_ties=FALSE) %>%
                 select(-select1)
write.table(data_select1a, "data/CDP/output/data_select1a.csv", sep=";", col.names = TRUE, row.names=FALSE)
# check if number of rows is equal to number of unique keys 
nrow(data_select1a)==n_test

# 3.2 Select target with target year closest to first selection (Target year descending, because if selection criteria index is the same, we take the highest target year)
data_select1a_TY <- select(data_select1a, company_name, `Country/Region`, `Target year`)
data_select1b1<- left_join(data_select, data_select1a_TY, by=c("company_name", "Country/Region")) %>% 
                 mutate(select2=abs(`Target year.y`-`Target year.x`)) %>%
                 filter(select2 != 0) %>%
                 rename(`Target year`=`Target year.x`) %>% select(-`Target year.y`) %>%
                 arrange(company_name, `Country/Region`, desc(`Target year`), Scope) %>%
                 group_by(company_name, `Country/Region`) %>%
                 slice_min(n=1, order_by=select2, with_ties=FALSE) %>%
                 select(-select2)
# remove records for which Target 2 has different scope than scope 1 --> Target 1 is leading
data_select1b2 <- left_join(data_select1b1, data_select1a, by=c('company_name', 'Country/Region')) %>%
                  filter(Scope.x==Scope.y)
data_select1b2 <- select(data_select1b2, !contains(".y"))
colnames(data_select1b2) <- names(data_select1b2) %>% str_replace_all("\\.x", "")
write.table(data_select1b2, "data/CDP/output/data_select1b.csv", sep=";", col.names = TRUE, row.names=FALSE)

# bind target 1 and target 2 data frames
data_select2 <- rbind(data_select1a, data_select1b2)



# for some scope 1 targets the perc_scope1 is not 1 and for some scop2 targets it is not 0, change (perc is based on S1, S2M, S2L)
data_select2 <- mutate(data_select2, perc_scope1=ifelse(Scope=="Scope 1", 1, perc_scope1))
data_select2 <- mutate(data_select2, perc_scope1=ifelse(Scope=="Scope 1+3", 1, perc_scope1))
data_select2 <- mutate(data_select2, perc_scope1=ifelse(Scope=="Scope 2", 0, perc_scope1))
write.table(data_select2, "data/CDP/output/data_select2.csv", sep=";", col.names = TRUE, row.names=FALSE)

#----------------------------
# PUT TWO TARGETS ON SAME ROW

# Similar to ReadProcessCDPData.R, put the two selected targets and info (no in rows) to one row
# add company/country/scope with different targets to one row
# 1. add field with target number
cols_id_row <- cols_id[!(cols_id %in% c("Target year", "Scope"))]
data_select3 <- group_by_at(data_select2, cols_id_row) %>%
                arrange(`Base year`, `Target year`) %>%
                mutate(target_nr=paste0("Target_", row_number()))
max_nr_targets <- str_split(max(data_select3$target_nr), pattern="_", simplify = TRUE)
max_nr_targets <- as.integer(max_nr_targets[1,2])
cat(paste0("maximum number of targets per row is ", max_nr_targets, "\n"))
write.table(data_select3, "data/CDP/output/data_select3.csv", sep=";", col.names = TRUE, row.names=FALSE)

# 2. select necassary fields
data_select4 <- select(data_select3, all_of(cols_id_row), `Base year`, `Target year`, `Most recent accounting year`, 
                       `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`, target_nr, Scope, perc_scope1, 
                       `Is this a science-based target?`, `% of target achieved [auto-calculated]`)

# 3. add target information to same row per company/country/scope combination
data_select4 <- pivot_wider(data_select4, 
                            names_from=target_nr,
                            values_from=c(`Base year`, `Target year`, Scope, perc_scope1, `Most recent accounting year`, 
                                          `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`, 
                                          `Is this a science-based target?`, `% of target achieved [auto-calculated]`),
                                           values_fill=NA)
write.table(data_select4, "data/CDP/output/data_select4.csv", sep=";", col.names = TRUE, row.names=FALSE)

# 4. order columns keep target info (BY, MRY, %-reduction) next to each other
order_targets_empty <- c('Base year_Target_', 'Target year_Target_', 'Most recent accounting year_Target_', 'Scope_Target_', 'perc_scope1_Target_', 
                         'BaseYearEmissions_excl_scope3_Target_', 'MRY_EM_excl_scope3_Target_', '% reduction from base year_Target_')
order_targets <- NULL
for (i in 1:2)
{ tmp1 <- paste0(order_targets_empty[1], i)
  tmp2 <- paste0(order_targets_empty[2], i)
  tmp3 <- paste0(order_targets_empty[3], i)
  tmp4 <- paste0(order_targets_empty[4], i)
  tmp5 <- paste0(order_targets_empty[5], i)
  tmp6 <- paste0(order_targets_empty[6], i)
  tmp7 <- paste0(order_targets_empty[7], i)
  tmp8 <- paste0(order_targets_empty[8], i)
  cat(paste0(tmp1, " - ", tmp2, " - ", tmp3, " - ", tmp4, " - ", tmp5, " - ", tmp6, " - ", tmp7, " - ", tmp8, "\n"))
  order_targets <- c(order_targets, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)
}
cols_other <- colnames(data_select4)
cols_other <- cols_other[!(cols_other %in% order_targets)]
data_select5 <- select(data_select4, all_of(cols_other), all_of(order_targets))
data_select5 <- select(data_select5, cols_other, order_targets) %>%
                arrange(company_name, `Country/Region`)
write.table(data_select5, "data/CDP/output/absolute_2020_targets_selection_per_row.csv", sep=";", col.names = TRUE, row.names=FALSE)

#-----------------------------------------------

# Create CAAT tool table
# Add CAAT sector
Sectors_CDP_CAAT <- read_excel('data/CDP/input/Sectors CDP_CAAT.xlsx', sheet = "sectors")
# missing primary_sector, primary_activity, primary_industry data for Sumitomo Mitsui Financial Group
data_select6 <- mutate(data_select5, CDP_sector = paste0(primary_sector, "-", primary_industry, "-", primary_activity))
absolute_2020_targets_remove_duplicates <- left_join(data_select6, Sectors_CDP_CAAT, by=c("CDP_sector"))
# check if there are empty primary_sector, primary_industry, primary_activity fields
check_empty <- filter(data_select6, is.na(CDP_sector)) %>% select(`company_name`, `Country/Region`, `target_type`, primary_industry, primary_activity, contains("sector"))

cols_CDP <- colnames(data_select6)
#cols_CAAT <- c("account_id", "Scope_Target_1", "Scope_Target_2", "TwoTargets", "Country", "Actor name",	"Action description", "Included in aggregation", "Sufficient data", "Actor type", "Target type", "Action or initiative",	
#               "Sector targeted",	"Subsector", "Subsector 2",	"Smaller actor types included", "Geographic coverage", "Action status", 
#               "Base Year", "BY Scope 1 emissions (tCO2e)",	"BY Scope 2 emissions (tCO2e)",	
#              "MRY Year",	"MRY Scope 1 emissions (tCO2e)",	"MRY Scope 2 emissions (tCO2e)", 
#               "Target Year 1", "Target Year 1 Scope 1 emissions reduction (perc)",	"Target Year 1 Scope 2 emissions reduction (perc)", 
#               "Target Year 2",	"Target Year 2 Scope 1 emissions reduction (perc)",	"Target Year 2 Scope 2 emissions reduction (perc)",
#               "SBT_target1", "SBT_target2", "Perc_Target_Achieved_Target1",  "Perc_Target_Achieved_Target2", "primary_sector")

cols_CAAT <- c("account_id",	"Scope_Target_1",	"Scope_Target_2",	"TwoTargets",	"Country",	"Actor name",	"Sector targeted",	"Action description",	"Included in aggregation",	"Sufficient data",	"Actor type",	
                "Target type",	"Action or initiative",	"Subsector",	"Subsector 2",	"Smaller actor types included",	"Geographic coverage",	"Action status",	"TwoTargets",	
               "Base Year",	"BY Scope 1 emissions (tCO2e)",	"BY Scope 2 emissions (tCO2e)",	
               "MRY Year",	"MRY Scope 1 emissions (tCO2e)",	"MRY Scope 2 emissions (tCO2e)",	"Target Year 1",	"Target Year 1 Scope 1 emissions reduction (perc)",	"Target Year 1 Scope 2 emissions reduction (perc)",
               "Target Year 2",	"Target Year 2 Scope 1 emissions reduction (perc)",	"Target Year 2 Scope 2 emissions reduction (perc)",
               "SBT_target1",	"SBT_target2",	"Perc_Target_Achieved_Target1",	"Perc_Target_Achieved_Target2",	"primary_sector")

data_CAAT <- data_select6 %>% ungroup() %>%
             #rename(Country=`Country/Region`,
             #      `Actor name`= company_name,
             #       `Sector targeted` = CAAT_sector) %>%
             transmute( account_id=account_id,
                        Scope_Target_1=Scope_Target_1,
                        Scope_Target_2=Scope_Target_2,
                        Country=`Country/Region`,
                       `Actor name`= company_name,
                       `Sector targeted` = CDP_sector,
                       `Action description`="",

                       `Included in aggregation`="Yes",
                    `Sufficient data`="",
                    `Actor type`="Companies",
                    `Target type`="GHG emissions target",
                    `Action or initiative`="Individual action",
                    `Sector targeted`=CDP_sector,
                    `Subsector`="",
                    `Subsector 2`="",
                    `Smaller actor types included`="No",
                    `Geographic coverage`="National",
                    `Action status`="Non-binding commitment",
                     # CAAT only allows for one base year. In 2018 dataset the base year for target 2 is the most recent (or the same as target 1) --> only if there are two targets
                     TwoTargets = ifelse(is.na(BaseYearEmissions_excl_scope3_Target_2), FALSE, TRUE),
                     `Base Year`=ifelse(TwoTargets,`Base year_Target_2`,`Base year_Target_1`),
                     `BY Scope 1 emissions (tCO2e)`=ifelse(TwoTargets, 
                                                           perc_scope1_Target_2*BaseYearEmissions_excl_scope3_Target_2,
                                                           perc_scope1_Target_1*BaseYearEmissions_excl_scope3_Target_1),
                     `BY Scope 2 emissions (tCO2e)`=ifelse(TwoTargets, 
                                                           (1-perc_scope1_Target_2)*BaseYearEmissions_excl_scope3_Target_2,
                                                           (1-perc_scope1_Target_1)*BaseYearEmissions_excl_scope3_Target_1),
                     `MRY Year`=`Most recent accounting year_Target_1`,
                     `MRY Scope 1 emissions (tCO2e)`=perc_scope1_Target_1*MRY_EM_excl_scope3_Target_1,
                     `MRY Scope 2 emissions (tCO2e)`=(1-perc_scope1_Target_1)*MRY_EM_excl_scope3_Target_1,
                      # CAAT only allows for one base year. Base year 2 is (in 2018 dataset) always more recent
                      # Therefore, if the base year for target 1 is different form target 2, the percentage reduction needs to be recalculated to BY 2
                     `Target Year 1`=`Target year_Target_1`,
                     `Target Year 1 Scope 1 emissions reduction (perc)`=ifelse(TwoTargets, 
                                                                               ifelse(BaseYearEmissions_excl_scope3_Target_2!=0,(-1*((1-`% reduction from base year_Target_1`/100)*(BaseYearEmissions_excl_scope3_Target_1/BaseYearEmissions_excl_scope3_Target_2))+1),0),
                                                                               `% reduction from base year_Target_1`/100),
                     `Target Year 1 Scope 2 emissions reduction (perc)`=`Target Year 1 Scope 1 emissions reduction (perc)`,
                     `Target Year 2`=ifelse(TwoTargets, `Target year_Target_2`, ""),
                     `Target Year 2 Scope 1 emissions reduction (perc)`=ifelse(TwoTargets,`% reduction from base year_Target_2`/100,""),
                     `Target Year 2 Scope 2 emissions reduction (perc)`=ifelse(TwoTargets,`Target Year 2 Scope 1 emissions reduction (perc)`,""),
                      SBT_target1=`Is this a science-based target?_Target_1`, SBT_target2=`Is this a science-based target?_Target_2`,
                      Perc_Target_Achieved_Target1=`% of target achieved [auto-calculated]_Target_1`, Perc_Target_Achieved_Target2=`% of target achieved [auto-calculated]_Target_2`,
                      primary_sector=primary_sector)
data_CAAT$TwoTargets <- as.character(data_CAAT$TwoTargets)
data_CAAT_select <- select(data_CAAT,cols_CAAT)
write.table(data_CAAT_select, "data/CDP/output/data_2020_CAAT.csv", sep=";", col.names = TRUE, row.names=FALSE, fileEncoding="UTF-16LE")




