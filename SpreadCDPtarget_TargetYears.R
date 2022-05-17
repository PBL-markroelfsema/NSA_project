library(stringr)
library(tidyverse)
library(plotly)

# This script used processed CDP data from ReadProcessCDPData_2020.R and add targets for every company/country/scope combination to one row

source('ReadProcessCDPData_2020.R')

# add company/country/scope with different targets to one row
# 1. add field with target number
cols_id_row <- cols_id[!(cols_id %in% c("Target year", "Scope"))]
absolute_2020_targets_per_row1 <- group_by_at(absolute_2020_targets_remove_duplicates, cols_id_row) %>%
  arrange(`Base year`, `Target year`) %>%  
  mutate(target_nr=paste0("Target_", row_number()))
max_nr_targets <- str_split(max(absolute_2020_targets_per_row1$target_nr), pattern="_", simplify = TRUE)
max_nr_targets <- as.integer(max_nr_targets[1,2])
cat(paste0("maximum number of targets per row is ", max_nr_targets, "\n"))
# 2. select necassary fields
absolute_2020_targets_per_row2 <- select(absolute_2020_targets_per_row1, all_of(cols_id_row), `Base year`, `Target year`, `Most recent accounting year`, 
                                         `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`, target_nr, Scope, perc_scope1)
# 3. add target information to same row per company/country/scope combination
absolute_2020_targets_per_row3 <- pivot_wider(absolute_2020_targets_per_row2, 
                                              names_from=target_nr,
                                              values_from=c(`Base year`, `Target year`, Scope, perc_scope1, `Most recent accounting year`, 
                                                            `BaseYearEmissions_excl_scope3`, `MRY_EM_excl_scope3`, `% reduction from base year`),
                                              values_fill=NA)
# 4. order columns keep target info (BY, MRY, %-reduction) next to each other
order_targets_empty <- c('Base year_Target_', 'Target year_Target_', 'Most recent accounting year_Target_',  'Scope_Target_', 'perc_scope1_Target_', #Inventory reporting year_Target_', 
                         'BaseYearEmissions_excl_scope3_Target_', 'MRY_EM_excl_scope3_Target_', '% reduction from base year_Target_')
order_targets <- NULL
for (i in 1:max_nr_targets)
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
cols_other <- colnames(absolute_2020_targets_per_row3)
cols_other <- cols_other[!(cols_other %in% order_targets)]
#absolute_2020_targets_per_row4 <- select(absolute_2020_targets_per_row3, all_of(cols_other), all_of(order_targets))
absolute_2020_targets_per_row4 <- select(absolute_2020_targets_per_row3, cols_other, order_targets) %>%
  arrange(company_name, `Country/Region`)
write.table(absolute_2020_targets_per_row4, "data/CDP/output/absolute_2020_targets_per_row.csv", sep=";", col.names = TRUE, row.names=FALSE)
