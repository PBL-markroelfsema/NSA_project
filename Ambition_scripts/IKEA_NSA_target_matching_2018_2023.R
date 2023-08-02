
#                                                  IKEA NSA report series: target matching

######################################################################################################################.
# Description: This script compares and matched responses from different disclosure cycles for an analysis of 
# corporate progress tracking
######################################################################################################################.

# OUTPUT: IKEA_NSA_abs_er_2018_prof1_vF.xlsx
#         IKEA_NSA_abs_er_2018_prof2_vF.xlsx
#         IKEA_NSA_abs_er_2018_prof3_vF.xlsx
#         IKEA_NSA_abs_er_2018_prof4_vF.xlsx

# 1. Preamble ============================================================================

t0 <- Sys.time()  # to calculate the time taken to pull the data using the script 
options(scipen=999)

# 2. Library =============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(stringdist)
library(fuzzyjoin)
library(openxlsx)
library(stringr)
library(data.table)

#------------------------------------- 
# 3. Functions, variables and settings =============================================================

source('Ambition_scripts/IKEA_NSA_target_matching_functions.R')

# SETTINGS
data_dir = "Ambition_scripts/data/CDP_2023/"
# Target status options: New, Underway, Achieved, Revised, Expired, Replaced, Retired
# exclude 'Expired', 'Replaced', 'Retired'
target_status_include = c('New', 'Underway', 'Achieved', 'Revised')
target_status_exclude = c('Expired', 'Replaced', 'Retired')

output <- TRUE #Set to TRUE if new files should be output
overwrite_input_file <- FALSE # Boolean to express if excel files should be read in even if this was already done before

raw_response_files <- data.frame(year = c(2018, 2019, 2020, 2021, 2022),
                                 filename = c('CDP_2018_Global_Aggregation_raw_response.xlsx',
                                              'CDP_2019_Global_aggregation_raw_response.xlsx',
                                              'CDP_2020_Global_aggregation_raw_response.xlsx',
                                              'CDP_2021_Global_aggregation_raw_response.xlsx',
                                              '2023_NSA_report_CDP_2022_climate_raw_response.xlsx'))

#YEAR = 2022
years = c(2018, 2019, 2020, 2021, 2022)
#years = c(2022)


# dataframe to collect column names
abs_colnames <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(abs_colnames) <- c('reporting year', 'column_name', 'profile')

# dataframe to collect all data
abs_er_prof <- data.frame()

# dataframe to collect number of records
statistics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(statistics) <- c('stage', 'reporting_year', 'item', 'profile', 'value')
statistics$stage <- as.character(statistics$stage)
statistics$reporting_year <- as.numeric(statistics$reporting_year)
statistics$item <- as.character(statistics$item)
statistics$profile <- as.character(statistics$profile)
statistics$value <- as.numeric(statistics$value)

# dataframe to collect selection statistics
stat_selection_before <- NULL
stat_selection_after <- NULL

#-------------------------------------
# 4. Data =============================================================================

# Process datasets for each year
#abs and int target types
for (YEAR in years)
{ ## Read in files
  print(YEAR, sep="\n")
  filename <- filter(raw_response_files, year==YEAR)$filename
  var_target = paste0("abs_er_", YEAR)
  # https://www.r-bloggers.com/2010/12/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
  # target data
  if(exists(paste0("abs_er_", YEAR)) & !overwrite_input_file)
  { print(paste0(var_target, " already exists"))
    abs_er = eval(parse(text = var_target))
  } else{ print(paste("reading in ", var_target), sep="\n")
    abs_er <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C4.1a")
    assign(paste0("abs_er_", YEAR), abs_er) }
  
  # base year emissions
  var_by = paste0("base_year_em_", YEAR)
  if (YEAR %in% c(2018, 2019, 2020, 2021)) {s_by="C5.1"} else {s_by="C5.2"}
  if(exists(paste0("base_year_em_", YEAR)) & !overwrite_input_file)
  { print(paste0(var_by, " already exists"))
    base_year_em = eval(parse(text = var_by))
  } else{print(paste("reading in ", var_by), sep="\n")
    base_year_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = s_by)
    assign(paste0("base_year_em_", YEAR), base_year_em) }
  
  # mry1 emissions
  var_mry1 = paste0("mry_s1_em_", YEAR)
  if(exists(paste0("mry_s1_em_", YEAR)) & !overwrite_input_file)
  { print(paste0(var_mry1, " already exists"))
    mry_s1_em = eval(parse(text = var_mry1))
  } else{ print(paste("reading in ", var_mry1), sep="\n")
    mry_s1_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.1")
    assign(paste0("mry_s1_em_", YEAR), mry_s1_em) }
  
  # mry2 emissions
  var_mry2 = paste0("mry_s2_em_", YEAR)
  if(exists(paste0("mry_s2_em_", YEAR)) & !overwrite_input_file)
  { print(paste0(var_mry2, " already exists"))
    mry_s2_em = eval(parse(text = var_mry2))
  } else{ print(paste("reading in ", var_mry2), sep="\n")
    mry_s2_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.3")
    assign(paste0("mry_s2_em_", YEAR), mry_s2_em) }
  
  # Clean files
  abs_er <- CleanColumnNames_TargetMatching(abs_er)
  assign(paste0("abs_er_", YEAR), abs_er) 
  base_year_em <- CleanColumnNames_TargetMatching(base_year_em)
  mry_s1_em <- CleanColumnNames_TargetMatching(mry_s1_em)
  mry_s2_em <- CleanColumnNames_TargetMatching(mry_s2_em)

 source('Ambition_scripts/stats.R')
  
  # 5. Code =============================================================================

  ########### 5.1 Prepare dataframes for target comparison ###########

  scopes <- unique(abs_er$`Scope(s)`) %>% as.data.frame()
  assign(paste0("scopes_", YEAR), scopes)
  
  ## Prepare abs targets dataframes 
  ## Rename and select key columns, remove rows that do not contain target information, i.e. row = 0,  "Question not applicable", target_year = NA
  abs_er_form_prepared <- abs_er %>% mutate(id=row_number()) %>% select(id, everything()) %>% PrepareAbsTargets(YEAR) 
  assign(paste0("abs_er_form_prepared_", YEAR), abs_er_form_prepared)
  
  if (YEAR == 2018) {tss <- c(target_status_include, 'Expired')} else {tss <- target_status_include}
  abs_er_form_processed <- ProcessSelect_CDPData(abs_er_form_prepared, tss, YEAR)
  assign(paste0("abs_er_form_processed_", YEAR), abs_er_form_processed)
  
  # show records that have not been included
  abs_er_form_removed <- anti_join(abs_er_form_processed, abs_er_form_prepared, by=c('id'))
  assign(paste0("abs_er_form_removed_", YEAR), abs_er_form_removed)
  
  # Determine priority score, order and Target Rank 
  abs_er_form <- AddPriorityScore_Order(abs_er_form_processed) %>% group_by(account_id) %>% mutate(priority_order = dense_rank(desc(priority_score))) %>% as.data.frame
  assign(paste0("abs_er_form_", YEAR), abs_er_form)

  base_year_em_form <- Process_BY(base_year_em, YEAR)
  #rm(base_year_em)
  if(output)  {write.xlsx(base_year_em_form, paste0(data_dir, "output/IKEA_NSA_BY_EM_", YEAR, ".xlsx"))
    print(paste0("Output ", YEAR, " base year emissions file"))
    }

  if (YEAR == 2018) {change_row=TRUE} else {change_row=FALSE}
  col_numbers_mry <- read.table(paste0(data_dir, 'col_names_mry.csv'), sep=';', header=TRUE) # Process most recent year scope 1 emissions data
  mry_s1_em_form <- Process_MRY_s1(mry_s1_em, filter(col_numbers_mry, year==YEAR, variable=='mry_s1')$col1, filter(col_numbers_mry, year==YEAR, variable=='mry_s1')$col2, change_row)
  #rm(mry_s1_em)
  if(output)  {write.xlsx(mry_s1_em_form, paste0(data_dir, "output/IKEA_NSA_MRY_S1_EM_", YEAR, ".xlsx"))
    print(paste0("Output ", YEAR, " most recent year scope 1 emissions file"))
  }

  # Process most recent year scope 2 emissions data
  mry_s2_em_form <- Process_MRY_s2(mry_s2_em, filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col1, 
                                              filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col2,  
                                              filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col3,
                                              change_row)
  #rm(mry_s2_em)
  if(output)  {write.xlsx(mry_s2_em_form, paste0(data_dir, "output/IKEA_NSA_MRY_S2_EM_", YEAR, ".xlsx"))
    print(paste0("Output ", YEAR, " most recent year scope 2 emissions file"))
  }

  ############ 5.2 Summary statistics  ##################################################################

  # Identify companies that have a single target
  tar_count_abs <- abs_er_form %>% count(account_id)
  assign(paste0("tar_count_abs", YEAR), tar_count_abs) 
  
  #subset of single target companies  
  single_tar_abs <- filter(tar_count_abs, n == 1) 
  single_tar_accids <- single_tar_abs$account_id

  ########### 5.3 Target profiles  ################################################################
  
  ###### Profile 1 - Companies with only one target ####################
  abs_er_prof1 <- abs_er_form %>%
    filter(account_id %in% single_tar_accids) %>%
    mutate(target_profile = "prof1") %>%
    select(-row)

  #Output profile 1 - single target companies
  if(output)  {write.xlsx(abs_er_prof1, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof1_vF.xlsx"))
    print(paste0("Output ", YEAR, " absolute emissions reduction - profile 1"))
  }
  
  # check column names
  n = ncol(abs_er_prof1)
  abs_reporting <- rep(YEAR, n)
  abs_names <- colnames(abs_er_prof1)
  abs_profile <- rep(1, n) 
  abs_colnames_year <- cbind(abs_reporting, abs_names, abs_profile)
  colnames(abs_colnames_year) <- c('reporting year', 'column_name', 'profile')
  abs_colnames <- rbind(abs_colnames, abs_colnames_year)
  
  ######### Profile 2 - 2021 - Same scope, same base year, different % reduction and target year ###################

  # Create subset without single target companies
  abs_er_form_alt_post1 <- abs_er_form %>% filter(!account_id %in% single_tar_accids)

  # Add root_id for sequential targets
  abs_er_form_alt_post1 <- abs_er_form_alt_post1 %>%
    mutate(root_id = paste(account_id, target_coverage, scope, base_year, emissions_base_year, emissions_base_year_percent))

  #Counts of root_id matches
  root_id_count_abs <- abs_er_form_alt_post1 %>% count(root_id)

  #Identify single vs multi occurrence root_id   
  single_root_abs <- filter(root_id_count_abs, n == 1)
  multi_root_abs <- filter(root_id_count_abs, n > 1)
  multi_root_id <- multi_root_abs$root_id

  #Subset dataset for multi root_id occurrence in 2018 and 2021
  abs_er_form_alt_post1 <- abs_er_form_alt_post1 %>%  filter(root_id %in% multi_root_id) %>% as.data.frame()

  #Pivot dataset by root_id
  abs_er_prof2 <- PivotDatasetByRoot_id_prof2(abs_er_form_alt_post1) %>%
    mutate(target_profile = "prof2") 

  #Output profile 2 - sequential matched targets
  if(output)  {write.xlsx(abs_er_prof2, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof2_vF.xlsx"))
    print(paste0("Output ", YEAR, " absolute emissions reduction - profile 2"))
  }

  # check column names
  n = ncol(abs_er_prof2)
  abs_reporting <- rep(YEAR, n)
  abs_names <- colnames(abs_er_prof2)
  abs_profile <- rep(2, n) 
  abs_colnames_year <- cbind(abs_reporting, abs_names, abs_profile)
  colnames(abs_colnames_year) <- c('reporting year', 'column_name', 'profile')
  abs_colnames <- rbind(abs_colnames, abs_colnames_year)
  
  ###### Profile 3 - Multiple targets, separate scopes, same base and target year ####################
               #Join data sets based on account_id, base year, target year, different scopes

  # Create list of unique sequential account ids
  seq_tar_accids <- unique(abs_er_prof2$account_id)


  # Create subset without single target and sequential targets companies
  abs_er_form_alt_post2 <- abs_er_form %>%
    filter(!account_id %in% single_tar_accids & !account_id %in% seq_tar_accids,
           !(simple_scope %in% c("S1S2", "S1S2S3")))

  # Add root_id for parallel targets
  abs_er_form_alt_post2 <- abs_er_form_alt_post2 %>%
    mutate(root_id = paste(account_id, target_coverage, base_year, target_year))

  #Counts of root_ids 
  prof3_root_id_count_abs <- abs_er_form_alt_post2 %>%
    count(root_id)

  #Identify single vs multi occurrence root_id   
  prof3_single_root_abs <- filter(prof3_root_id_count_abs, n == 1) 
  prof3_multi_root_abs <- filter(prof3_root_id_count_abs, n > 1)
  prof3_multi_root_id <- prof3_multi_root_abs$root_id

  #Subset dataset for multi root_id occurrence in years
  abs_er_form_alt_post2 <- abs_er_form_alt_post2 %>%
    filter(root_id %in% prof3_multi_root_id) %>%
    mutate(target_profile = "prof3") %>%
    select(-row)

  #Pivot dataset by root_id
  if (nrow(abs_er_form_alt_post2) > 0)
  { abs_er_prof3 <- PivotDatasetByRoot_id_prof3(abs_er_form_alt_post2)
  } else 
  { abs_er_prof3 <- NULL
  }

  #Output profile 3 - parallel matched targets
  if(output)  {write.xlsx(abs_er_prof3, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof3_vF.xlsx"))
    print(paste0("Output ", YEAR, " absolute emissions reduction - profile 3"))
  }
  
  # check column names
  # NOT INCLUDED FOR NOW, AS THIS INCLUDES SCOPE_1 and SCOPE_2
  # --> make choice for one scope, and thus also one scope
  #n = ncol(abs_er_prof3)
  #abs_reporting <- rep(YEAR, n)
  #abs_names <- colnames(abs_er_prof3)
  #abs_profile <- rep(3, n) 
  #abs_colnames_year <- cbind(abs_reporting, abs_names, abs_profile)
  #colnames(abs_colnames_year) <- c('reporting year', 'column_name', 'profile')
  #abs_colnames <- rbind(abs_colnames, abs_colnames_year)
  
  ###### Profile 4 - Select from remaining targets with highest priority score ####################

  # Create list of unique parallel target account ids
  para_tar_accids <- unique(abs_er_prof3$account_id)

  # Create subset without single, sequential, and parallel targets companies
  abs_er_prof4 <- abs_er_form %>%
    filter(!account_id %in% single_tar_accids & !account_id %in% seq_tar_accids & !account_id %in% para_tar_accids,
           priority_order == 1) %>% 
    mutate(target_profile = "prof4") %>%
    select(-row)

  #Output profile 4 - top priority score targets
  if(output)  {write.xlsx(abs_er_prof4, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof4_vF.xlsx"))
    print(paste0("Output ", YEAR, " absolute emissions reduction - profile 4"))
  }

  # check column names
  n = ncol(abs_er_prof4)
  abs_reporting <- rep(YEAR, n)
  abs_names <- colnames(abs_er_prof4)
  abs_profile <- rep(4, n) 
  abs_colnames_year <- cbind(abs_reporting, abs_names, abs_profile)
  colnames(abs_colnames_year) <- c('reporting year', 'column_name', 'profile')
  abs_colnames <- rbind(abs_colnames, abs_colnames_year)
  
  # profile 2 has multiple profiles for which column names end with '_1', '_2' etc.
  # streamline profile 1, 3, and 4 by using '_1' for these column names
  multiple_col_name <- abs_colnames %>% filter(grepl("\\_1$", column_name)) %>%
    select(-`reporting year`, -profile) %>%
    unique() %>%
    mutate(column_name = str_replace(column_name, '_1', ''))
  multiple_col_name <- multiple_col_name$column_name
  
  abs_er_prof1_col_names <- colnames(abs_er_prof1)
  check_prof_1 <- abs_er_prof1_col_names[abs_er_prof1_col_names %in% multiple_col_name]
  abs_er_prof1 <- rename_with(abs_er_prof1, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_1) ) %>%
                  mutate(reporting_year = YEAR)
  abs_er_prof2 <- mutate(abs_er_prof2, reporting_year = YEAR) %>%
                  mutate(reporting_year = YEAR)
  abs_er_prof3_col_names <- colnames(abs_er_prof3)
  check_prof_3 <- abs_er_prof3_col_names[abs_er_prof3_col_names %in% multiple_col_name]
  if (is.null(dim(abs_er_prof3)[1])) 
  { print("empty profile 3")} else 
  {abs_er_prof3 <- rename_with(abs_er_prof3, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_3) ) %>%
                                    mutate(reporting_year = YEAR)
  }
  abs_er_prof4_col_names <- colnames(abs_er_prof4)
  check_prof_4 <- abs_er_prof4_col_names[abs_er_prof4_col_names %in% multiple_col_name]
  abs_er_prof4 <- rename_with(abs_er_prof4, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_4) ) %>%
    mutate(reporting_year = YEAR)

  abs_er_prof_year <- bind_rows(abs_er_prof1, abs_er_prof2) %>% bind_rows(abs_er_prof4)
  assign(paste0("abs_er_prof_", YEAR), abs_er_prof_year)
  abs_er_prof <- bind_rows(abs_er_prof, abs_er_prof_year)
  
  abs_er_prof <- select(abs_er_prof, reporting_year, target_profile, everything())
  
  #Output profiles top priority score targets
  if(output)  {write.xlsx(abs_er_prof, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof_vF.xlsx"))
    print(paste0("Output ", YEAR, " absolute emissions reduction - all profiles"))
  }
  
  # STATISTICS

  statistics <- add_row(statistics, stage='stage1_processed', reporting_year=YEAR, item='targets', profile='total', value=nrow(abs_er))
  statistics <- add_row(statistics, stage='stage1_processed', reporting_year=YEAR, item='companies', profile='total', value=length(unique(abs_er$`Account number`)))
  statistics <- add_row(statistics, stage='stage2_selected', reporting_year=YEAR, item='targets', profile='total', value=nrow(abs_er_form))
  statistics <- add_row(statistics, stage='stage2_selected', reporting_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_form$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='targets', profile='1', value=nrow(abs_er_prof1))
  statistics <- add_row(statistics, stage='stage3_read_in', reporting_year=YEAR, item='companies', profile='1', value=length(unique(abs_er_prof1$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='targets', profile='2', value=nrow(abs_er_prof2))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='companies', profile='2', value=length(unique(abs_er_prof2$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='targets', profile='3', value=nrow(abs_er_prof3))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='companies', profile='3', value=length(unique(abs_er_prof3$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='targets', profile='4', value=nrow(abs_er_prof4))
  statistics <- add_row(statistics, stage='stage3_profile', reporting_year=YEAR, item='companies', profile='4', value=length(unique(abs_er_prof4$`account_id`)))
  
  # # target reference number (not empty)
  nr_target_reference_numbers_empty_NA <- abs_er %>% filter(is.na(`Target reference number`)) %>% summarise(empty=n()) %>% as.numeric()
  nr_target_reference_numbers_empty_QNA <- abs_er %>% filter(`Target reference number`=='Question not applicable') %>% summarise(empty=n()) %>% as.numeric()
  nr_target_reference_numbers_empty = nr_target_reference_numbers_empty_NA + nr_target_reference_numbers_empty_QNA
  statistics <- add_row(statistics, stage='stage1_processed', reporting_year=YEAR, item='target reference number', profile='total', value=nr_target_reference_numbers_empty)
  
  stat_selection_year_before <- read.table(paste0('Ambition_scripts/data/CDP_2023/output/stats/stats_before_', YEAR, '.csv'), sep=';', header=T)
  stat_selection_before <- bind_rows(stat_selection_before, stat_selection_year_before)
  stat_selection_year_after <- read.table(paste0('Ambition_scripts/data/CDP_2023/output/stats/stats_after_', YEAR, '.csv'), sep=';', header=T)
  stat_selection_after <- bind_rows(stat_selection_after, stat_selection_year_after)

} # end for YEARS in years


stat_selection <- inner_join(stat_selection_before, stat_selection_after, by=c('reporting_year'))
write.table(stat_selection_before, 'Ambition_scripts/data/CDP_2023/output/stats/stats_before.csv', sep=';', row.names=F)
write.table(stat_selection_after, 'Ambition_scripts/data/CDP_2023/output/stats/stats_after.csv', sep=';', row.names=F)
write.table(stat_selection, 'Ambition_scripts/data/CDP_2023/output/stats/stats.csv', sep=';', row.names=F)

source('Ambition_scripts/stats_summary.R')
