library(stringr)
library(tidyverse)
library(plotly)

# This script used processed CDP data from ReadProcessCDPData_2020.R and add targets for every company/country/scope combination to one row

source('ReadProcessCDPData_2020.R')

# add company/country/scope with different targets to one row
# 1. add field with target number
cols_id_row <- cols_id[!(cols_id %in% c("Target year"))] 
#cols_id_row <- cols_id[!(cols_id %in% c("Target year", "Scope"))] %>% as.data.frame()
cols_id_row_plus <- c(cols_id_row)
absolute_2020_targets_per_row1 <- filter(absolute_2020_targets_remove_duplicates, `Target coverage`=="Company-wide" & `% emissions in Scope`>0.75) %>%
                                  select(cols_id_row_plus, `Target year`, `Base year`, `% reduction from base year`) %>%
                                  group_by_at(cols_id_row_plus) %>%
                                  arrange(`Target year`, `Base year`) %>%  
                                  mutate(r=row_number(), target_nr=paste0("Target_", row_number()))
max_nr_targets <- str_split(max(absolute_2020_targets_per_row1$target_nr), pattern="_", simplify = TRUE)
max_nr_targets <- as.integer(max_nr_targets[1,2])
cat(paste0("maximum number of targets per row is ", max_nr_targets, "\n"))

# 2. select necessary fields
absolute_2020_targets_per_row2 <- select(absolute_2020_targets_per_row1, all_of(cols_id_row), `Base year`, `Target year`, `Most recent accounting year`, 
                                         `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`, target_nr, Scope, perc_scope1)

# 3. add target information to same row per company/country/scope combination
absolute_2020_targets_per_row3 <- pivot_wider(absolute_2020_targets_per_row2, 
                                              names_from=target_nr,
                                              values_from=c(`Base year`, `Target year`, `Most recent accounting year`, 
                                                            `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`),
                                              #values_from=c(`Base year`, `Target year`, Scope, perc_scope1, `Most recent accounting year`, 
                                              #              `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`),
                                              values_fill=NA)
# 4. order columns keep target info (BY, MRY, %-reduction) next to each other
order_targets_empty <- c('Base year_Target_', 'Target year_Target_', 'Most recent accounting year_Target_', #Inventory reporting year_Target_', 
                         'BaseYearEmissions_excl_scope3_Target_', 'MRY_EM_excl_scope3_Target_', '% reduction from base year_Target_')
#order_targets_empty <- c('Base year_Target_', 'Target year_Target_', 'Most recent accounting year_Target_',  'Scope_Target_', 'perc_scope1_Target_', #Inventory reporting year_Target_', 
#                         'BaseYearEmissions_excl_scope3_Target_', 'MRY_EM_excl_scope3_Target_', '% reduction from base year_Target_')
order_targets <- NULL
for (i in 1:max_nr_targets)
{ tmp1 <- paste0(order_targets_empty[1], i)
tmp2 <- paste0(order_targets_empty[2], i)
tmp3 <- paste0(order_targets_empty[3], i)
tmp4 <- paste0(order_targets_empty[4], i)
tmp5 <- paste0(order_targets_empty[5], i)
tmp6 <- paste0(order_targets_empty[6], i)
#tmp7 <- paste0(order_targets_empty[7], i)
#tmp8 <- paste0(order_targets_empty[8], i)
cat(paste0(tmp1, " - ", tmp2, " - ", tmp3, " - ", tmp4, " - ", tmp5, " - ", tmp6, " - ", " - ", "\n"))
#cat(paste0(tmp1, " - ", tmp2, " - ", tmp3, " - ", tmp4, " - ", tmp5, " - ", tmp6, " - ", tmp7, " - ", tmp8, "\n"))
order_targets <- c(order_targets, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
#order_targets <- c(order_targets, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)
}
cols_other <- colnames(absolute_2020_targets_per_row3)
cols_other <- cols_other[!(cols_other %in% order_targets)]
#absolute_2020_targets_per_row4 <- select(absolute_2020_targets_per_row3, all_of(cols_other), all_of(order_targets))
absolute_2020_targets_per_row4 <- select(absolute_2020_targets_per_row3, cols_other, order_targets) %>%
                                  arrange(company_name, `Country/Region`)
write.table(absolute_2020_targets_per_row4, "data/CDP/output/absolute_2020_targets_per_row.csv", sep=";", col.names = TRUE, row.names=FALSE)

# aggregate it to global scale
absolute_2020_targets_per_row_global <- group_by(absolute_2020_targets_per_row4, account_id, company_name, primary_sector, Scope) %>%
                                        summarise(BY_T1=median(`Base year_Target_1`, na.rm=TRUE),
                                                  TY_T1=median(`Target year_Target_1`, na.rm=TRUE),
                                                  MRY_T1=median(`Most recent accounting year_Target_1`, na.rm=TRUE),
                                                  BY_EM_T1=sum(BaseYearEmissions_excl_scope3_Target_1, na.rm=TRUE),
                                                  MRY_EM_T1=sum(MRY_EM_excl_scope3_Target_1, na.rm=TRUE),
                                                  Perc_T1=median(`% reduction from base year_Target_1`, na.rm=TRUE),
                                                  check_stdev_T1=sd(`% reduction from base year_Target_1`, na.rm=TRUE),
                                                  
                                                  BY_T2=median(`Base year_Target_2`, na.rm=TRUE),
                                                  TY_T2=median(`Target year_Target_2`, na.rm=TRUE),
                                                  MRY_T2=median(`Most recent accounting year_Target_2`, na.rm=TRUE),
                                                  BY_EM_T2=sum(BaseYearEmissions_excl_scope3_Target_2, na.rm=TRUE)	,
                                                  MRY_EM_T2=sum(MRY_EM_excl_scope3_Target_2, na.rm=TRUE),
                                                  Perc_T2=median(`% reduction from base year_Target_2`, na.rm=TRUE),
                                                  check_stdev_T2=sd(`% reduction from base year_Target_2`, na.rm=TRUE),
                                                  
                                                  BY_T3=median(`Base year_Target_3`, na.rm=TRUE),
                                                  TY_T3=median(`Target year_Target_3`, na.rm=TRUE),
                                                  MRY_T3=median(`Most recent accounting year_Target_3`, na.rm=TRUE),
                                                  BY_EM_T3=sum(BaseYearEmissions_excl_scope3_Target_3, na.rm=TRUE),	
                                                  MRY_EM_T3=sum(MRY_EM_excl_scope3_Target_3, na.rm=TRUE),
                                                  Perc_T3=median(`% reduction from base year_Target_3`, na.rm=TRUE),
                                                  check_stdev_T3=sd(`% reduction from base year_Target_3`, na.rm=TRUE),
                                                  
                                                  BY_T4=median(`Base year_Target_4`, na.rm=TRUE),
                                                  TY_T4=median(`Target year_Target_4`, na.rm=TRUE),
                                                  MRY_T4=median(`Most recent accounting year_Target_4`, na.rm=TRUE),
                                                  BY_EM_T4=sum(BaseYearEmissions_excl_scope3_Target_4, na.rm=TRUE),	
                                                  MRY_EM_T4=sum(MRY_EM_excl_scope3_Target_4, na.rm=TRUE),
                                                  Perc_T4=median(`% reduction from base year_Target_4`, na.rm=TRUE),
                                                  check_stdev_T4=sd(`% reduction from base year_Target_4`, na.rm=TRUE),
                                                  
                                                  #BY_T5=median(`Base year_Target_5`, na.rm=TRUE),
                                                  #TY_T5=median(`Target year_Target_5`, na.rm=TRUE),
                                                  #MRY_T5=median(`Most recent accounting year_Target_5`, na.rm=TRUE),
                                                  #BY_EM_T5=sum(BaseYearEmissions_excl_scope3_Target_5, na.rm=TRUE),	
                                                  #MRY_EM_T5=sum(MRY_EM_excl_scope3_Target_5, na.rm=TRUE),
                                                  #Perc_T5=median(`% reduction from base year_Target_5`, na.rm=TRUE),
                                                  #check_stdev_T5=sd(`% reduction from base year_Target_5`, na.rm=TRUE),
                                                  
                                                  #BY_T6=median(`Base year_Target_6`, na.rm=TRUE),
                                                  #TY_T6=median(`Target year_Target_6`, na.rm=TRUE),
                                                  #MRY_T6=median(`Most recent accounting year_Target_6`, na.rm=TRUE),
                                                  #BY_EM_T6=sum(BaseYearEmissions_excl_scope3_Target_6, na.rm=TRUE),	
                                                  #MRY_EM_T6=sum(MRY_EM_excl_scope3_Target_6, na.rm=TRUE),
                                                  #Perc_T6=median(`% reduction from base year_Target_6`, na.rm=TRUE),
                                                  #check_stdev_T6=sd(`% reduction from base year_Target_6`, na.rm=TRUE),
                                                  )
check_sd <- filter(absolute_2020_targets_per_row_global, check_stdev_T1>0 | check_stdev_T2 | check_stdev_T3 | check_stdev_T4)
#check_sd <- filter(absolute_2020_targets_per_row_global, check_stdev_T1>0 | check_stdev_T2 | check_stdev_T3 | check_stdev_T4 | check_stdev_T5 | check_stdev_T6)

