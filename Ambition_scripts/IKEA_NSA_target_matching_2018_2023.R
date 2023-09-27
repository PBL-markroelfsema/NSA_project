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
options(dplyr.summarise.inform = FALSE)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 500L)

# 2. Library =============================================================================

#library(dplyr)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(stringdist)
library(fuzzyjoin)
library(openxlsx)
library(stringr)
library(data.table)
library(stringr)
library(ggplot2)
library(logr)
library(scatterpie)
library(zeallot)

#------------------------------------- 
# 3. Functions, variables and settings =============================================================

source('Ambition_scripts/IKEA_NSA_target_matching_functions.R')

# Create logfile file location
# https://cran.r-project.org/web/packages/logr/vignettes/logr-globals.html
data_dir = "Ambition_scripts/data/CDP_2023/"
log_file_location <- file.path(data_dir, "target_matching.log")
log_file <- log_open(log_file_location)
options("logr.notes" = FALSE)

# SETTINGS
# Target status options: New, Underway, Achieved, Revised, Expired, Replaced, Retired
# exclude 'Expired', 'Replaced', 'Retired'
target_status_include = c('New', 'Underway', 'Revised', 'Achieved')
target_status_exclude = c('Expired', 'Replaced', 'Retired')
target_status_order = c(target_status_include, target_status_exclude)
target_coverage_order = c("Company-wide", "Business activity","Business division", "Country/region", "Site/facility", "Product-level", "Other", "Unknown", "", NA)
scope_order <- c("Scope 1| Scope 2", "Scope 1| Scope 2| Scope 3", "Scope 1", "Scope 2", "Scope 2| Scope 3", "Scope 1| Scope 3")
scope_accounting_method_order <- c("Market-based", "Location-based", "", NA)
scope3_categories_order <- c("Upstream & downstream", "Downstream", "Upstream", "", NA)

output <- TRUE #Set to TRUE if new files should be output
overwrite_input_file <- FALSE # Boolean to express if excel files should be read in even if this was already done before

raw_response_files <- data.frame(year = c(2018, 2019, 2020, 2021, 2022),
                                 filename = c('CDP_2018_Global_Aggregation_raw_response.xlsx',
                                              'CDP_2019_Global_aggregation_raw_response.xlsx',
                                              'CDP_2020_Global_aggregation_raw_response.xlsx',
                                              'CDP_2021_Global_aggregation_raw_response.xlsx',
                                              '2023_NSA_report_CDP_2022_climate_raw_response.xlsx'))

YEAR = 2021
years = c(2018, 2019, 2020, 2021, 2022)
#years = c(2022)

# dataframe to collect column names
abs_colnames <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(abs_colnames) <- c('reporting year', 'column_name', 'profile')

# dataframe to collect all data
abs_er_prof_year <- data.frame()
abs_er_prof <- data.frame()

# dataframe to collect number of records
statistics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(statistics) <- c('stage', 'disclosure_year', 'item', 'profile', 'value')
statistics$stage <- as.character(statistics$stage)
statistics$disclosure_year <- as.numeric(statistics$disclosure_year)
statistics$item <- as.character(statistics$item)
statistics$profile <- as.character(statistics$profile)
statistics$value <- as.numeric(statistics$value)

# dataframe to collect selection statistics
stat_selection_before <- NULL
stat_selection_after <- NULL
# dataframe to collect base year emissions

#base_year_em_total <- select(base_year_em_form, account_id, `by_emissions_Scope 1`, `by_emissions_Scope 2 (location-based) `, `by_emissions_Scope 2 (market-based) `) 
base_year_em_total <- data.frame(0, 0, 0, 0, 0, 0, 0)

colnames(base_year_em_total) <- c("account_id", "by_emissions_Scope 1", "by_emissions_Scope 2 (location-based) ", "by_emissions_Scope 2 (market-based) ", "disclosure_year", "by_emissions_scope1_2_location", "by_emissions_scope1_2_market")

outliers <- read.table(paste0(data_dir, "input/outliers.csv"), header=T, sep=";") 

stat_years <- NULL

#-------------------------------------
# 4. Data =============================================================================

# Process datasets for each year
#abs and int target types
for (YEAR in years)
{ ## READ
  
  log_print("----------------------------------------------------", sep="\n", hide_notes = T)
  log_print(YEAR)
  filename <- filter(raw_response_files, year==YEAR)$filename
  # https://www.r-bloggers.com/2010/12/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
  
  # target data
  
  # companies
  var_target = paste0("companies_", YEAR)
  if(exists(paste0("companies_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_target, " already exists"))
    companies = eval(parse(text = var_target))
  } else { log_print(paste("Reading in ", var_target), sep="\n")
    Sheetname = "Summary Data"
    companies <- read.xlsx(paste0(data_dir, "input/", filename), sheet = Sheetname)
    assign(paste0("companies_", YEAR), companies) 
  }
  
  #  targets
  var_target = paste0("targets_", YEAR)
  if(exists(paste0("targets_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_target, " already exists"))
    targets = eval(parse(text = var_target))
  } else { log_print(paste("Reading in ", var_target), sep="\n")
    if (YEAR<2022) Sheetname = "C4 - Targets and Performance" else Sheetname = "C4.1"
    targets <- read.xlsx(paste0(data_dir, "input/", filename), sheet = Sheetname)
    assign(paste0("targets_", YEAR), targets) 
  }
  
  # absolute emissions reduction targets
  var_target = paste0("abs_er_", YEAR)
  if(exists(paste0("abs_er_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_target, " already exists"))
    abs_er = eval(parse(text = var_target))
  } else{ log_print(paste("Reading in ", var_target), sep="\n")
    abs_er <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C4.1a")
    assign(paste0("abs_er_", YEAR), abs_er) 
  }
  
  # intensity targets
  var_target = paste0("int_", YEAR)
  if(exists(paste0("int_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_target, " already exists"))
     int = eval(parse(text = var_target))
  } else{ log_print(paste("Reading in ", var_target), sep="\n")
    int <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C4.1b")
    assign(paste0("int_", YEAR), int) 
  }
  
  # net-zero targets
  var_target = paste0("nz_", YEAR)
  nz = NULL
  if (YEAR>2020)
  {  if(exists(paste0("nz_", YEAR)) & !overwrite_input_file)
    { log_print(paste0(var_target, " already exists"))
      nz = eval(parse(text = var_target))
    } else { log_print(paste("Reading in ", var_target), sep="\n")
      nz <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C4.2c")
      assign(paste0("nz_", YEAR), nz) 
    }
  } # if
  
  # base year emissions
  var_by = paste0("base_year_em_", YEAR)
  if (YEAR %in% c(2018, 2019, 2020, 2021)) {s_by="C5.1"} else {s_by="C5.2"}
  if(exists(paste0("base_year_em_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_by, " already exists"))
    base_year_em = eval(parse(text = var_by))
  } else{log_print(paste("reading in ", var_by), sep="\n")
    base_year_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = s_by)
    assign(paste0("base_year_em_", YEAR), base_year_em) }
  
  # mry1 emissions
  var_mry1 = paste0("mry_s1_em_", YEAR)
  if(exists(paste0("mry_s1_em_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_mry1, " already exists"))
    mry_s1_em = eval(parse(text = var_mry1))
  } else{ log_print(paste("reading in ", var_mry1), sep="\n")
    mry_s1_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.1")
    assign(paste0("mry_s1_em_", YEAR), mry_s1_em) }
  
  # mry2 emissions
  var_mry2 = paste0("mry_s2_em_", YEAR)
  if(exists(paste0("mry_s2_em_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_mry2, " already exists"))
    mry_s2_em = eval(parse(text = var_mry2))
  } else{ log_print(paste("reading in ", var_mry2), sep="\n")
    mry_s2_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.3")
    assign(paste0("mry_s2_em_", YEAR), mry_s2_em) }
  
  # mry3 emissions
  var_mry3 = paste0("mry_s3_em_", YEAR)
  if(exists(paste0("mry_s3_em_", YEAR)) & !overwrite_input_file)
  { log_print(paste0(var_mry3, " already exists"))
    mry_s3_em = eval(parse(text = var_mry3))
  } else{ log_print(paste("reading in ", var_mry3), sep="\n")
    mry_s3_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.5")
    assign(paste0("mry_s3_em_", YEAR), mry_s3_em) }
  
  # 5. Code =============================================================================
  
  # CLEAN
  
  companies <- CleanColumnNames_TargetMatching(companies)
  abs_er <- CleanColumnNames_TargetMatching(abs_er)
  targets <- CleanColumnNames_TargetMatching(targets)
  int <- CleanColumnNames_TargetMatching(int)
  if (YEAR>2020)
  { nz <- CleanColumnNames_TargetMatching(nz)
    assign(paste0("nz_", YEAR), nz) 
    write.table(nz, paste0(data_dir, "processed/nz_clean_", YEAR, ".csv"), row.names=FALSE, sep=";")
  }

  assign(paste0("abs_er_", YEAR), abs_er) 
  assign(paste0("targets_", YEAR), targets) 
  assign(paste0("int_", YEAR), int) 
  assign(paste0("nz_", YEAR), nz) 
  #
  base_year_em <- CleanColumnNames_TargetMatching(base_year_em)
  mry_s1_em <- CleanColumnNames_TargetMatching(mry_s1_em)
  mry_s2_em <- CleanColumnNames_TargetMatching(mry_s2_em)
  mry_s3_em <- CleanColumnNames_TargetMatching(mry_s3_em)
  
  abs_er <- mutate_all(abs_er, funs(str_replace_all(., ";", "|")))
  abs_er <- mutate_all(abs_er, funs(str_replace_all(., ",", " ")))
  abs_er <- mutate_all(abs_er, funs(str_replace_all(., "\\\\", " ")))
  write.table(abs_er, paste0(data_dir, "processed/abs_er_clean_", YEAR, ".csv"), row.names=FALSE, sep=";")


  ########### 5.1 Prepare dataframes for target comparison ###########

  # TO DO --> integrate in stats.R
  stat_year <- CalculateStatsCDPData(companies, targets, abs_er, int, nz, base_year_em, mry_s1_em, mry_s2_em, mry_s3_em, YEAR) %>%
               mutate(disclosure_year=YEAR) %>%
               select(disclosure_year, everything())
  stat_years <- rbind(stat_years, stat_year)  
  
  # PREPARE
  
  ## Prepare abs targets dataframes (rename columns, adjust datatypes)
  targets_form_prepared <- PrepareTargets(targets, year)
  companies_form_prepared <- PrepareCompanies(companies, year)
  abs_er_form_prepared <- abs_er %>% mutate(id=row_number()) %>% select(id, everything()) %>% PrepareAbsTargets(YEAR) 
  assign(paste0("abs_er_form_prepared_", YEAR), abs_er_form_prepared)
  if (YEAR>2020) nz_form_prepared <- PrepareNZTargets(nz, YEAR)
  
  # write to file
  write.table(abs_er_form_prepared, paste0(data_dir, "processed/abs_er_form_prepared_", YEAR, ".csv"), row.names=FALSE, sep=";")
  
  source('Ambition_scripts/stats.R')
  
  # PROCESS
  
  abs_er_form_processed <- abs_er_form_prepared
  # Process data
  if (YEAR == 2018) {tss <- c(target_status_include, 'Expired')} else {tss <- target_status_include}
  abs_er_form_processed <- ProcessSelect_CDPData(abs_er_form_processed, tss, YEAR)
  
  if(YEAR>=2022) CheckEmissions2022(abs_er_form_processed)
  
  # Process scopes 
  abs_er_form_processed$account_id <- as.numeric(abs_er_form_processed$account_id)
  abs_er_form_processed <- ProcessScopes(abs_er_form_processed, YEAR)
  
  # Remove outliers
  log_print(paste0("Before removing outliers: ", nrow(abs_er_form_processed)))
  treshold_MRY  = 300000000
  outliers_year <- filter(outliers, disclosure_year==YEAR)
  outliers_year_remove <- filter(outliers_year, factor=="X")
  outliers_year_scale <- filter(outliers_year, factor!="X") %>% select(-organization)
  outliers_year_scale$factor <- as.numeric(outliers_year_scale$factor)
  #outliers_year_scale_BY <- filter(outliers_year_scale, EM_type=="BY")
  #outliers_year_scale_MRY <- filter(outliers_year_scale, EM_type=="MRY")
  #outliers_year_scale_TY <- filter(outliers_year_scale, EM_type=="TY")
  outliers_year_scale <- spread(outliers_year_scale, key=EM_type, value=factor)
  if ("BY" %in% colnames(outliers_year_scale))
  { outliers_year_scale=rename(outliers_year_scale, factor_BY=BY)
  } else
  { outliers_year_scale = mutate(outliers_year_scale, factor_BY=NA)
  }
  if ("MRY" %in% colnames(outliers_year_scale))
  { outliers_year_scale=rename(outliers_year_scale, factor_MRY=MRY)
  } else
  { outliers_year_scale = mutate(outliers_year_scale, factor_MRY=NA)
  }
  if ("TY" %in% colnames(outliers_year_scale))
  { outliers_year_scale=rename(outliers_year_scale, factor_TY=TY)
  } else
  { outliers_year_scale = mutate(outliers_year_scale, factor_TY = NA)
  }
  abs_er_form_processed <- anti_join(abs_er_form_processed, outliers_year_remove, by=c("account_id", "scope_def_2022")) %>%
                           left_join(outliers_year_scale, by=c("account_id", "scope_def_2022")) %>%
                           mutate(emissions_base_year=ifelse(!is.na(factor_BY), factor_BY*emissions_base_year, emissions_base_year)) %>%
                           mutate(emissions_reporting_year=ifelse(!is.na(factor_MRY), factor_MRY*emissions_reporting_year,
                                                           ifelse(emissions_reporting_year>treshold_MRY, NA, emissions_reporting_year))) %>%
                           mutate(emissions_target_year=ifelse(!is.na(factor_TY), factor_TY*emissions_target_year, emissions_target_year)) %>%
                           select(-one_of(c("factor_BY", "factor_MRY", "factor_TY")))
  log_print(paste0("After removing outliers: ", nrow(abs_er_form_processed)))
  
  # write file to processed
  abs_er_form_processed <- select(abs_er_form_processed, -please_explain)
  assign(paste0("abs_er_form_processed_", YEAR), abs_er_form_processed)
  write.table(abs_er_form_processed, paste0(data_dir, "processed/abs_er_form_processed_", YEAR, ".csv"), row.names=FALSE, sep=";")
  
  SaveFactors(abs_er_form_processed, "abs_er_form_processed", YEAR)
  
  # save unique account ids
  account_ids_year <- unique(abs_er_form_processed$account_id) %>% as.data.frame %>% mutate(disclosure_year=YEAR) %>%
                      select(disclosure_year, everything())
  colnames(account_ids_year)[2]="account_id"
  write.table(account_ids_year, paste0(data_dir, "processed/account_ids_year_", YEAR, ".csv"), row.names=FALSE, sep=";")
  
  ############ INVENTORY EMISSIONS  ##################################################################
  
  # BY/MRY EMISSIONS
  base_year_em_form <- Process_BY(base_year_em, YEAR)
  #rm(base_year_em)
  if(output)  {write.xlsx(base_year_em_form, paste0(data_dir, "output/IKEA_NSA_BY_EM_", YEAR, ".xlsx"))
    log_print(paste0("Output ", YEAR, " base year emissions file"))
  }
  
  # add BY emissions to one file
  base_year_em_form$`by_emissions_Scope 1` = as.numeric(base_year_em_form$`by_emissions_Scope 1`)
  base_year_em_form$`by_emissions_Scope 2 (location-based) ` = as.numeric(base_year_em_form$`by_emissions_Scope 2 (location-based) `)
  base_year_em_form$`by_emissions_Scope 2 (market-based) ` = as.numeric(base_year_em_form$`by_emissions_Scope 2 (market-based) `)
  base_year_em_total_tmp <- select(base_year_em_form, account_id, `by_emissions_Scope 1`, `by_emissions_Scope 2 (location-based) `, `by_emissions_Scope 2 (market-based) `) %>%
    mutate(`by_emissions_Scope 1`=replace(`by_emissions_Scope 1`, is.na(`by_emissions_Scope 1`),0),
           `by_emissions_Scope 2 (location-based) `=replace(`by_emissions_Scope 2 (location-based) `, is.na(`by_emissions_Scope 2 (location-based) `),0),
           `by_emissions_Scope 2 (market-based) `=replace(`by_emissions_Scope 2 (market-based) `, is.na(`by_emissions_Scope 2 (market-based) `),0),
           disclosure_year=YEAR, 
           by_emissions_scope1_2_location=`by_emissions_Scope 1`+`by_emissions_Scope 2 (location-based) `,
           by_emissions_scope1_2_market=`by_emissions_Scope 1`+`by_emissions_Scope 2 (market-based) `)
  base_year_em_total <- rbind(base_year_em_total, base_year_em_total_tmp)
  
  if (YEAR == 2018) {change_row=TRUE} else {change_row=FALSE}
  col_numbers_mry <- read.table(paste0(data_dir, 'col_names_mry.csv'), sep=';', header=TRUE) # Process most recent year scope 1 emissions data
  mry_s1_em_form <- Process_MRY_s1(mry_s1_em, filter(col_numbers_mry, year==YEAR, variable=='mry_s1')$col1, filter(col_numbers_mry, year==YEAR, variable=='mry_s1')$col2, change_row)
  #rm(mry_s1_em)
  if(output)  {write.xlsx(mry_s1_em_form, paste0(data_dir, "output/IKEA_NSA_MRY_S1_EM_", YEAR, ".xlsx"))
    log_print(paste0("Output ", YEAR, " most recent year scope 1 emissions file"))
  }
  
  # Process most recent year scope 2 emissions data
  mry_s2_em_form <- Process_MRY_s2(mry_s2_em, filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col1, 
                                   filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col2,  
                                   filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col3,
                                   change_row)
  #rm(mry_s2_em)
  if(output)  {write.xlsx(mry_s2_em_form, paste0(data_dir, "output/IKEA_NSA_MRY_S2_EM_", YEAR, ".xlsx"))
    log_print(paste0("Output ", YEAR, " most recent year scope 2 emissions file"))
  }
  
  # Process most recent year scope 3 emissions data
  mry_s3_em_form <- Process_MRY_s3(mry_s3_em, 
                                   "", 
                                   filter(col_numbers_mry, year==YEAR, variable=='mry_s2')$col2,
                                   change_row)
  #rm(mry_s2_em)
  if(output)  {write.xlsx(mry_s2_em_form, paste0(data_dir, "output/IKEA_NSA_MRY_S2_EM_", YEAR, ".xlsx"))
    log_print(paste0("Output ", YEAR, " most recent year scope 2 emissions file"))
  }
  
  # stats for report
  stats_report <- StatsReport(abs_er_form_processed, YEAR)
  log_print("REPORT")
  log_print(stats_report)
  
  #---------------------------------------------------------------------------------------------------------------------------------
  
  ############ Collect unique scope 1+2 targets for report figure  ##################################################################
  
  log_print("-----------------------------------------------")
  log_print("COLLECT SCOPE 1+2")
  
  abs_er_scope12 <- abs_er_form_processed
  # filter scopes, exclude scope 3 and unidentifiable scopes
  abs_er_scope12 <- filter(abs_er_scope12, !(simple_scope %in% c("S3", "Other", "", "No scope", "Question not applicable", NA))) %>%
                    select(-scope3_categories)
  
  # process duplicates for each company based on scope_def_2022, scope_accounting_method, target_year
  abs_er_scope12 <- group_by(abs_er_scope12, account_id, scope_def_2022, scope_accounting_method, target_year) %>%
                    mutate(duplicate=n()>1) %>%
                    select(duplicate, everything())
  abs_er_scope12_no_duplicates <- filter(abs_er_scope12, !(target_coverage=="Company-wide") | duplicate==F)
  abs_er_scope12_duplicates <- filter(abs_er_scope12, target_coverage=="Company-wide" & duplicate==T)
  write.table(abs_er_scope12_duplicates, paste0(data_dir, "processed/abs_er_scope12_duplicates_", YEAR, ".csv"), row.names=FALSE, sep=";")
  log_print("", sep="\n")
  log_print(paste0("Total rows for scope 1+2 is ", nrow(abs_er_scope12)))
  log_print(paste0(nrow(abs_er_scope12_no_duplicates), " withtout duplicates, and ", nrow(abs_er_scope12_duplicates), " with duplicates"))

  abs_er_scope12 <- mutate(abs_er_scope12, scope_accounting_method=ifelse(is.na(scope_accounting_method), "", scope_accounting_method))
  
  # factorize variables to enable selection in case of duplicate
  # 1. check factors
  log_print("Factors not included")
  tmp1 <- filter(abs_er_scope12, !scope_def_2022%in%scope_order); log_print(paste0("Scope: ", nrow(tmp1)))
  tmp2 <- filter(abs_er_scope12, !scope_accounting_method%in%scope_accounting_method_order); log_print(paste0("Accounting method: ", nrow(tmp2)))
  tmp3 <- filter(abs_er_scope12, !target_status%in%target_status_order); log_print(paste0("Target status: ", nrow(tmp3)))
  tmp4 <- filter(abs_er_scope12, !target_coverage%in%target_coverage_order); log_print(paste0("Target coverage: ", nrow(tmp4)))
  # 2. factorize
  abs_er_scope12$scope_def_2022 <- factor(abs_er_scope12$scope_def_2022, levels=scope_order)
  abs_er_scope12$scope_accounting_method <- factor(abs_er_scope12$scope_accounting_method, levels=scope_accounting_method_order)
  abs_er_scope12$target_status <- factor(abs_er_scope12$target_status, levels=target_status_order)
  abs_er_scope12$target_coverage <- factor(abs_er_scope12$target_coverage, levels=target_coverage_order)
  check_duplicates_before <- group_by(abs_er_scope12, account_id, scope_def_2022, scope_accounting_method, target_year, target_coverage) %>%
                             mutate(duplicate=n()>1) %>%
                             select(duplicate, everything()) %>%
                             filter(duplicate==T)
  log_print(paste0("Total rows before removing duplicates ", nrow(abs_er_scope12)))
  abs_er_scope12 <- group_by(abs_er_scope12, account_id, scope_def_2022, scope_accounting_method, target_year, target_coverage) %>% 
                    top_n(n=1, wt=target_status) %>%
                    top_n(n=1, wt=emissions_base_year_percent) %>%
                    top_n(n=1, wt=base_year) %>%
                    top_n(n=1, wt=year_target_set) %>%
                    top_n(n=1, wt=targeted_reduction) %>%
                    top_n(n=1, wt=scope_accounting_method) %>%
                    top_n(n=1, wt=emissions_base_year)
  log_print(paste0("Total rows after removing duplicates ", nrow(abs_er_scope12)))
  check_duplicates_after <- group_by(abs_er_scope12, account_id, scope_def_2022, scope_accounting_method, target_year, target_coverage) %>%
                            mutate(duplicate=n()>1) %>%
                            select(duplicate, everything()) %>%
                            filter(duplicate==T)
  log_print(paste0("CHECK - Duplicates remaining: ", nrow(check_duplicates_after)))
  log_print("", sep="\n")
  
  # calculate target year emissions if NA
  abs_er_scope12 <- mutate(abs_er_scope12, emissions_target_year=ifelse(is.na(emissions_target_year), (1-targeted_reduction/100)*emissions_base_year, emissions_target_year))
  
  # correct emissions for scope 3 emissions (only scope 1+2)
  # apply to emissions_base_year, emissions_target_year, emissions_reporting_year
  if (YEAR<2022)
  { abs_er_scope12 <- mutate(abs_er_scope12, 
                                        emissions_base_year_before=emissions_base_year,
                                        emissions_base_year_s12=perc_s1s2*emissions_base_year,
                                        emissions_target_year_s12=ifelse(is.na(emissions_target_year), NA, perc_s1s2*emissions_target_year),
                                        emissions_reporting_year_s12=ifelse(is.na(emissions_reporting_year), NA, perc_s1s2*emissions_reporting_year))
  } else
  {  #calculate scope 1 and 2 emissions for reporting years >= 2022
    abs_er_scope12 <- mutate(abs_er_scope12, 
                                        emissions_base_year_before=emissions_base_year,
                                        emissions_base_year_s12=ifelse(is.na(emissions_base_year_s1), 0, emissions_base_year_s1)+ifelse(is.na(emissions_base_year_s2), 0, emissions_base_year_s2),
                                        emissions_reporting_year_s12=ifelse(is.na(emissions_reporting_year_s1), 0, emissions_reporting_year_s1)+ifelse(is.na(emissions_reporting_year_s2), 0, emissions_reporting_year_s2),
                                        emissions_target_year_s12=(1-targeted_reduction/100)*emissions_base_year)
    
  } # if
  
  # add interpolated emissions for reporting year
  abs_er_scope12 <- mutate(abs_er_scope12, disclosure_year=YEAR)
  abs_er_scope12 <- mutate(abs_er_scope12, emissions_reporting_year_interpolated_scope12=ifelse(target_year==YEAR, emissions_target_year_s12,
                                                                                               emissions_base_year_s12+((YEAR-base_year)/(target_year-base_year))*(emissions_target_year_s12-emissions_base_year_s12)))
  abs_er_scope12 <- abs_er_scope12 %>% as.data.frame()
  
  assign(paste0("abs_er_scope12_processed_", YEAR), abs_er_scope12)
  write.table(abs_er_scope12, paste0(data_dir, "processed/abs_er_scope12_processed_", YEAR, ".csv"), row.names=FALSE, sep=";")
  SaveFactors(abs_er_scope12, "abs_er_scope12_", YEAR)
  
  ############ Collect unique scope 3 targets and process emissions for report figure  ##################################################################
  
  log_print("-----------------------------------------------")
  log_print("COLLECT SCOPE 3")
  
  abs_er_scope3 <- abs_er_form_processed
  
  # select scope 3 targets
  test1_scope3 <- filter(abs_er_scope3, grepl("Scope 3|\\+3", scope_def_2018))
  test1_scope3 <- mutate(test1_scope3, across('scope', str_replace, "Scope 3 ", "Scope 3:"), test="test1") %>% select(test, everything(), -scope_def_2022)
  write.table(test1_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/test1_scope3_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  test2_scope3 <- filter(abs_er_scope3, grepl("Scope 3", scope_def_2022)) %>% mutate(test="test2") %>% select(test, everything(), -scope_def_2018)
  write.table(test2_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/test2_scope3_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  abs_er_scope3 <- filter(abs_er_scope3, grepl("Scope 3", scope_def_2022))
  
  # calculate target year emissions if NA
  abs_er_scope3 <- mutate(abs_er_scope3, emissions_target_year=ifelse(is.na(emissions_target_year), (1-targeted_reduction/100)*emissions_base_year, emissions_target_year))
  
  # determine scope 3 emissions
  if (YEAR<2022)
  { abs_er_scope3 <- mutate(abs_er_scope3, emissions_base_year_scope3=(1-perc_s1s2)*emissions_base_year,
                            emissions_target_year_scope3=ifelse(is.na(emissions_target_year), NA, (1-perc_s1s2)*emissions_target_year),
                            emissions_reporting_year_scope3=ifelse(is.na(emissions_reporting_year), NA, (1-perc_s1s2)*emissions_reporting_year))
  } else
  {  #calculate scope 1 and 2 emissions for reporting years >= 2022
    abs_er_scope3 <- mutate(abs_er_scope3, emissions_base_year_scope3=emissions_base_year_s3,
                            emissions_reporting_year_scope3=emissions_reporting_year_s3,
                            emissions_target_year_scope3=(1-targeted_reduction/100)*emissions_base_year_s3)
  } # if
  
  # remove records with no emissions_base_year_scope3
  tmp <- filter(abs_er_scope3, is.na(emissions_base_year_scope3))
  log_print(paste0("Number of rows with NA BY emissions for scope 3: ", nrow(tmp), " from ", nrow(abs_er_scope3), " rows"))
  
  abs_er_scope3 <- mutate(abs_er_scope3, scope = scope_def_2022)
  abs_er_scope3 <- select(abs_er_scope3, "account_id", "country", "target_id", "target_coverage",
                                         "scope", "scope3_categories", "base_year", "emissions_base_year_scope3", 
                                         "target_year", "targeted_reduction", "emissions_target_year_scope3", "emissions_reporting_year_scope3")
  #abs_er_scope3 <- mutate(abs_er_scope3, target_coverage=ifelse(str_starts(target_coverage, "Other"), "Other", target_coverage))
  
  # set variable types
  abs_er_scope3$emissions_base_year_scope3 <- as.numeric(abs_er_scope3$emissions_base_year_scope3)
  abs_er_scope3$emissions_target_year_scope3 <- as.numeric(abs_er_scope3$emissions_target_year_scope3)
  abs_er_scope3$emissions_reporting_year_scope3 <- as.numeric(abs_er_scope3$emissions_reporting_year_scope3)
  abs_er_scope3$base_year <- as.numeric(abs_er_scope3$base_year)
  abs_er_scope3$target_year <- as.numeric(abs_er_scope3$target_year)
  abs_er_scope3$targeted_reduction <- as.numeric(abs_er_scope3$targeted_reduction)
  
  # calculate interpolate MRY emissions (between BY and TY)
  abs_er_scope3 <- mutate(abs_er_scope3, emissions_target_year_scope3=ifelse(is.na(emissions_target_year_scope3), 
                                                                             (1-targeted_reduction/100)*emissions_base_year_scope3, 
                                                                             emissions_target_year_scope3))
  write.table(abs_er_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/abs_er_scope3_emissions_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  # disaggregate scope 3 targets for reporting years >= 2022
  if (YEAR < 2022)
  { abs_er_scope3_per_category <- abs_er_scope3
  } else { 
    abs_er_scope3_per_category <- separate_rows(abs_er_scope3, scope3_categories, sep="\\|")
    tmp <- group_by(abs_er_scope3_per_category, account_id, target_id) %>%
            summarise(count=n())
    abs_er_scope3_per_category <- left_join(abs_er_scope3_per_category, tmp, by=c("account_id", "target_id")) %>%
            mutate(emissions_base_year_scope3=emissions_base_year_scope3/count,
                   emissions_target_year_scope3=emissions_target_year_scope3/count,
                   emissions_reporting_year_scope3=emissions_reporting_year_scope3/count)
  }
  write.table(abs_er_scope3_per_category, paste0("Ambition_scripts/data/CDP_2023/processed/abs_er_scope3_per_category_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  # aggregate scope 3 targets for reporting years <= 2021
  if (YEAR < 2022)
  { abs_er_scope3 <- group_by(abs_er_scope3, account_id, country, target_coverage, scope, base_year, target_year) %>%
    summarise(targeted_reduction=weighted.mean(targeted_reduction, emissions_base_year_scope3),
              emissions_base_year_scope3=sum(emissions_base_year_scope3),
              emissions_target_year_scope3=sum(emissions_target_year_scope3),
              emissions_reporting_year_scope3=sum(emissions_reporting_year_scope3))
  } else 
  { abs_er_scope3 <- select(abs_er_scope3, -scope3_categories, -target_id)
  }
  
  # add interpolated emissions for reporting year
  abs_er_scope3 <- mutate(abs_er_scope3, disclosure_year=YEAR)
  abs_er_scope3 <- mutate(abs_er_scope3, emissions_reporting_year_interpolated_scope3=ifelse(target_year==YEAR, emissions_target_year_scope3,
                                                                                             emissions_base_year_scope3+((YEAR-base_year)/(target_year-base_year))*(emissions_target_year_scope3-emissions_base_year_scope3)))
  abs_er_scope3 <- abs_er_scope3 %>% as.data.frame()
  
  # save
  assign(paste0("abs_er_scope3_processed_", YEAR), abs_er_scope3)
  write.table(abs_er_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/abs_er_scope3_processed_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  ############ Collect NZ targets for report figure  ##################################################################
  
  log_print("-----------------------------------------------")
  log_print("COLLECT NZ targets")
  
  if (YEAR>2020)
  { nz_processed <- nz_form_prepared #%>%
    #select(-starts_with("Plan for"), -starts_with("List"))
    nz_processed <- filter(nz_processed, !target_id=="Question not applicable") %>%
                           select(-please_explain)
    assign(paste0("nz_processed_", YEAR), nz_processed)
    write.table(nz_processed, paste0(data_dir, "processed/nz_processed_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  }
  ############ SCOPE 1+2 SPECIFIC for ambition pathways ################################################################################################
  
  log_print("-----------------------------------------------")
  log_print("Process scope 1+2 for ambition pathways")
  
  #abs_er_form_pathways <- abs_er_scope12
  abs_er_form_pathways <- abs_er_form_processed
  abs_er_form_pathways <- mutate(abs_er_form_pathways, disclosure_year=YEAR)

  # only include targets that cover more than 75% of company emissions
  abs_er_form_pathways <- filter(abs_er_form_pathways, emissions_base_year_percent >= 75)
  
  # filter scopes, exclude scope 3 and unidentifiable scopes
  abs_er_form_pathways <- filter(abs_er_form_pathways, !(simple_scope %in% c("S3", "Other", "", "No scope", "Question not applicable", NA)))
  
  # correct emissions for scope 3 emissions (only scope 1+2)
  # apply to emissions_base_year, emissions_target_year, emissions_reporting_year
  if (YEAR<2022)
  { abs_er_form_pathways <- mutate(abs_er_form_pathways, 
                                   emissions_base_year_before=emissions_base_year,
                                   emissions_base_year_s12=perc_s1s2*emissions_base_year,
                                   emissions_target_year_s12=ifelse(is.na(emissions_target_year), NA, perc_s1s2*emissions_target_year),
                                   emissions_reporting_year_s12=ifelse(is.na(emissions_reporting_year), NA, perc_s1s2*emissions_reporting_year))
  } else
  {  #calculate scope 1 and 2 emissions for reporting years >= 2022
    abs_er_form_pathways <- mutate(abs_er_form_pathways, 
                                    emissions_base_year_before=emissions_base_year,
                                    emissions_base_year_s1=ifelse(is.na(emissions_base_year_s1), 0, emissions_base_year_s1),
                                    emissions_base_year_s2=ifelse(is.na(emissions_base_year_s2), 0, emissions_base_year_s2),
                                    emissions_base_year_s3=ifelse(is.na(emissions_base_year_s3), 0, emissions_base_year_s3),
                                    emissions_base_year_s12=ifelse(emissions_base_year_s1+emissions_base_year_s2>0, emissions_base_year_s1+emissions_base_year_s2,emissions_base_year-emissions_base_year_s3),
                                    emissions_reporting_year_s1=ifelse(is.na(emissions_reporting_year_s1), 0, emissions_reporting_year_s1),
                                    emissions_reporting_year_s2=ifelse(is.na(emissions_reporting_year_s2), 0, emissions_reporting_year_s2),
                                    emissions_reporting_year_s3=ifelse(is.na(emissions_reporting_year_s3), 0, emissions_reporting_year_s3),
                                    emissions_reporting_year_s12=ifelse(emissions_reporting_year_s1+emissions_reporting_year_s2>0, emissions_reporting_year_s1+emissions_reporting_year_s2,emissions_reporting_year-emissions_reporting_year_s3),
                                    emissions_target_year_s12=(1-targeted_reduction/100)*emissions_base_year_s12)

  } # if
  abs_er_form_pathways <- mutate(abs_er_form_pathways, 
                                      emissions_base_year=emissions_base_year_s12, 
                                      emissions_reporting_year=emissions_reporting_year_s12, 
                                      emissions_target_year=emissions_target_year_s12) %>%
                               select(-emissions_base_year_s12, -emissions_reporting_year_s12, -emissions_target_year_s12)
  assign(paste0("abs_er_form_pathways_", YEAR), abs_er_form_pathways)
  write.table(abs_er_form_pathways, paste0(data_dir, "processed/abs_er_form_pathways_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  # Determine priority score, order and target Rank 
  abs_er_form <- select(abs_er_form_pathways, -emissions_base_year_before)
  abs_er_form <- AddPriorityScore_Order(abs_er_form) %>% group_by(account_id) %>% mutate(priority_order = dense_rank(desc(priority_score))) %>% as.data.frame
  write.table(abs_er_form, paste0(data_dir, "processed/abs_er_form_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  SaveFactors(abs_er_form, "abs_er_form", YEAR)
  
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

  abs_er_prof1 <- select(abs_er_prof1, -year_target_set, -priority_score, -priority_order, -perc_s1s2, -scope3_categories, -scope_accounting_method, -scope_def_2018, -scope_def_2022)
  #Output profile 1 - single target companies
  if(output)  {write.xlsx(abs_er_prof1, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof1_vF.xlsx"))
    log_print(paste0("Output ", YEAR, " absolute emissions reduction - profile 1"))
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

  # check if there are 5 target years
  abs_er_prof2 <- AddEmptyTargets(abs_er_prof2)
  
  #abs_er_prof2 <- select(abs_er_prof2, -priority_score, -priority_order, -perc_s1s2, -scope3_categories, -scope_accounting_method, -scope_def_2018, -scope_def_2022)
  #Output profile 2 - sequential matched targets
  if(output)  {write.xlsx(abs_er_prof2, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof2_vF.xlsx"))
    log_print(paste0("Output ", YEAR, " absolute emissions reduction - profile 2"))
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

  # check if there are 5 target years
  #abs_er_prof3 <- AddEmptyTargets(abs_er_prof3)
  
  #Output profile 3 - parallel matched targets
  if(output)  {write.xlsx(abs_er_prof3, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof3_vF.xlsx"))
    log_print(paste0("Output ", YEAR, " absolute emissions reduction - profile 3"))
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
  
  abs_er_prof4 <- select(abs_er_prof4, -year_target_set, -priority_score, -priority_order, -perc_s1s2, -scope3_categories, -scope_accounting_method, -scope_def_2018, -scope_def_2022)
  # Output profile 4 - top priority score targets
  if(output)  {write.xlsx(abs_er_prof4, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof4_vF.xlsx"))
    log_print(paste0("Output ", YEAR, " absolute emissions reduction - profile 4"))
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
  
  ###### Aggregate profiles ####################
  
  abs_er_prof1_col_names <- colnames(abs_er_prof1)
  check_prof_1 <- abs_er_prof1_col_names[abs_er_prof1_col_names %in% multiple_col_name]
  abs_er_prof1 <- rename_with(abs_er_prof1, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_1) ) #%>%
                  #mutate(disclosure_year = YEAR)
  #abs_er_prof2 <- mutate(abs_er_prof2, disclosure_year = YEAR)
  abs_er_prof3_col_names <- colnames(abs_er_prof3)
  check_prof_3 <- abs_er_prof3_col_names[abs_er_prof3_col_names %in% multiple_col_name]
  if (is.null(dim(abs_er_prof3)[1])) 
  { log_print("empty profile 3")} else 
  { abs_er_prof3 <- rename_with(abs_er_prof3, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_3) ) #%>%
                   #mutate(disclosure_year = YEAR)
  }
  abs_er_prof4_col_names <- colnames(abs_er_prof4)
  check_prof_4 <- abs_er_prof4_col_names[abs_er_prof4_col_names %in% multiple_col_name]
  abs_er_prof4 <- rename_with(abs_er_prof4, .fn = ~paste0(., '_1'), .cols = all_of(check_prof_4) ) #%>%
                  #mutate(disclosure_year = YEAR)

  abs_er_prof_year <- bind_rows(abs_er_prof1, abs_er_prof2) %>% bind_rows(abs_er_prof4)
  abs_er_prof_year <- select(abs_er_prof_year, disclosure_year, everything())
  assign(paste0("abs_er_prof_", YEAR), abs_er_prof_year)
  
  #abs_er_prof <- bind_rows(abs_er_prof, abs_er_prof_year)
  #abs_er_prof <- select(abs_er_prof, disclosure_year, target_profile, everything())
  
  #Output profiles top priority score targets
  if(output)  {write.xlsx(abs_er_prof_year, paste0(data_dir, "output/IKEA_NSA_abs_er_", YEAR, "_prof_vF.xlsx"))
    log_print(paste0("Output ", YEAR, " absolute emissions reduction - all profiles"))
  }
  #Output profiles to one file top priority score targets
  #if(output)  {write.xlsx(abs_er_prof_year, paste0(data_dir, "output/IKEA_NSA_abs_er_prof_vF.xlsx"))
  #  log_print(paste0("Output ", YEAR, " absolute emissions reduction to one file - all profiles"))
  #} 
  
  # STATISTICS
  statistics <- add_row(statistics, stage='stage1a_cleaned', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er))
  statistics <- add_row(statistics, stage='stage1a_cleaned', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er$`Account number`)))
  statistics <- add_row(statistics, stage='stage1b_prepared', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_form_prepared))
  statistics <- add_row(statistics, stage='stage1b_prepared', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_form_prepared$`account_id`)))
  abs_er_prepared_check <- filter(abs_er_form_prepared, !target_id=="Question not applicable", !is.na(target_id))
  statistics <- add_row(statistics, stage='stage1b_prepared_Q', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_prepared_check))
  statistics <- add_row(statistics, stage='stage1b_prepared_Q', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_prepared_check$`account_id`)))
  statistics <- add_row(statistics, stage='stage1c_processed', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_form_processed))
  statistics <- add_row(statistics, stage='stage1c_processed', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_form_processed$`account_id`)))
  abs_er_processed_check <- filter(abs_er_form_processed, target_coverage=="Company-wide")
  statistics <- add_row(statistics, stage='stage1c_processed_company_wide', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_processed_check))
  statistics <- add_row(statistics, stage='stage1c_processed_company_wide', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_processed_check$`account_id`)))
  
  statistics <- add_row(statistics, stage='stage1d_processed_s12', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_form_pathways))
  statistics <- add_row(statistics, stage='stage1d_processed_s12', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_form_pathways$`account_id`)))
  statistics <- add_row(statistics, stage='stage2a_selected', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_form))
  statistics <- add_row(statistics, stage='stage2a_selected', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_form$`account_id`)))
  statistics <- add_row(statistics, stage='stage2b_report', disclosure_year=YEAR, item='targets', profile='total', value=nrow(abs_er_scope12))
  statistics <- add_row(statistics, stage='stage2b_report', disclosure_year=YEAR, item='companies', profile='total', value=length(unique(abs_er_scope12$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='targets', profile='1', value=nrow(abs_er_prof1))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='companies', profile='1', value=length(unique(abs_er_prof1$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='targets', profile='2', value=nrow(abs_er_prof2))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='companies', profile='2', value=length(unique(abs_er_prof2$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='targets', profile='3', value=nrow(abs_er_prof3))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='companies', profile='3', value=length(unique(abs_er_prof3$`account_id`)))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='targets', profile='4', value=nrow(abs_er_prof4))
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='companies', profile='4', value=length(unique(abs_er_prof4$`account_id`)))
  
  statistics_profile <- filter(statistics, stage=="stage3_profile", disclosure_year==YEAR) %>% group_by(item) %>% summarise(value=sum(value)) %>% as.data.frame()
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='targets', profile='total', value=statistics_profile$value[2])
  statistics <- add_row(statistics, stage='stage3_profile', disclosure_year=YEAR, item='companies', profile='total', value=statistics_profile$value[1])
  
  # # target reference number (not empty)
  nr_target_reference_numbers_empty_NA <- abs_er %>% filter(is.na(`Target reference number`)) %>% summarise(empty=n()) %>% as.numeric()
  nr_target_reference_numbers_empty_QNA <- abs_er %>% filter(`Target reference number`=='Question not applicable') %>% summarise(empty=n()) %>% as.numeric()
  nr_target_reference_numbers_empty = nr_target_reference_numbers_empty_NA + nr_target_reference_numbers_empty_QNA
  statistics <- add_row(statistics, stage='stage1_processed', disclosure_year=YEAR, item='target reference number', profile='total', value=nr_target_reference_numbers_empty)

} # end for YEARS in years
statistics_output<- spread(statistics, key=disclosure_year, value=value)
write.table(statistics_output, paste0(data_dir, "output/statistics.csv"), row.names=FALSE, sep=";")
write.table(base_year_em_total, paste0(data_dir, "processed/base_year_em_total.csv"), row.names=FALSE, sep=";")

source('Ambition_scripts/stats_summary.R')

# TO DO --> finish overview headquarters
G20_included <- c("Argentina", "Australia", "Brazil", "Canada", "China", "the European Union (EU)", "India", "Indonesia", "South Korea", "Japan", "Mexico", "Russia", "Saudi Arabia", 
                  "South Africa", "Türkiye", "the United Kingdom", "the United States")
headquarter_countries <- NULL
for (t in years)
{ headquarter_countries_years <- unique(abs_er_form_processed$country) %>% as.data.frame()
  headquarter_countries <- rbind(headquarter_countries, headquarter_countries_years)
}
colnames(headquarter_countries)[1] <- "Countr_Region_Headquarter"
headquarter_countries <- distinct(headquarter_countries, Countr_Region_Headquarter, .keep_all = TRUE)
write.table(headquarter_countries, paste0(data_dir, "processed/headquarters.csv"), sep=";", row.names=FALSE)

###### Check outliers ####################

df_EM_total <- data.frame(2017, 0, "", "", "", 0, 0, 0, 0, 0, 0)
colnames(df_EM_total) <- c("disclosure_year", "account_id", "organization", "scope_def_2022", "scope_accounting_method", "emissions_base_year_percent", "emissions_base_year", "emissions_base_year_before", "emissions_reporting_year", "emissions_target_year", "perc_s1s2")
#y=2020
for (y in years)
{ log_print(y)
  abs_er_BY <- eval(parse(text = paste0("abs_er_form_pathways_", y)))
  if(exists(paste0("abs_er_form_pathways_", y)))
  { BY_EM <- select(abs_er_BY, account_id, organization, scope_def_2022, scope_accounting_method, emissions_base_year_percent, emissions_base_year, emissions_base_year_before, emissions_reporting_year, emissions_target_year, perc_s1s2) %>% mutate(disclosure_year=y) %>%
            select(disclosure_year, everything())
   df_EM_total <- rbind(df_EM_total, BY_EM)
  } # if
}
df_EM_total <- filter(df_EM_total, disclosure_year>=2018)
write.table(df_EM_total, paste0(data_dir, "output/df_emissions.csv"), row.names=FALSE, sep=";") 

outlier_treshold = 100000000
outliers_BY <- filter(df_EM_total, emissions_base_year>outlier_treshold | emissions_base_year<0) %>%
               arrange(disclosure_year, desc(emissions_base_year)) %>%
               left_join(base_year_em_total, by=c("account_id", "disclosure_year"))
write.table(outliers_BY, paste0(data_dir, "processed/outliers.csv"), row.names=FALSE, sep=";") 

# outliers sent to Andrew/CDP earlier
outliers_sent_before <- read.table(paste0(data_dir, "outliers_sent_to_Andrew.csv"), header=T, sep=";") %>%
                        rename(disclosure_year=source_year)
outliers_sent_before <- left_join(outliers_sent_before, df_EM_total, by=c("disclosure_year", "account_id", "scope_def_2022")) %>%
                         left_join(base_year_em_total, by=c("account_id", "disclosure_year"))
write.table(outliers_sent_before, paste0(data_dir, "processed/outliers_sent_before.csv"), row.names=FALSE, sep=";") 

# Close log
current_time = str_replace_all(Sys.time(), " ", "_")
current_time = str_replace_all(current_time, ":", "-")
log_file_name <- paste0(data_dir, "log/", "target_matching.log")
log_copy_file_name <- paste0(data_dir, "log/", "target_matching_", current_time, ".log")
file.copy(from = log_file_name, to = log_copy_file_name)
log_close()


  