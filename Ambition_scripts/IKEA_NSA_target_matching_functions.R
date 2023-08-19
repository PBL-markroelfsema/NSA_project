library(stringr)

target_status_include = c('New', 'Underway', 'Achieved', 'Revised')
scope3_categories <- c("Scope1| Scope 2| Scope 3", "Scope 3", "Other")
scope3_cat_categories <- c("Purchased goods and services",
                           "Capital goods",
                           "Fuel-and-energy-related activities (not included in Scopes 1 or 2)",
                           "Upstream transportation and distribution",
                           "Waste generated in operations",
                           "Business travel",
                           "Employee commuting",
                           "Upstream leased assets",
                           "Downstream transportation and distribution",
                           "Processing of sold products",
                           "Use of sold products",
                           "End-of-life treatment of sold products",
                           "Downstream leased assets",
                           "Franchises",
                           "Investments",
                           "Other",
                           "Upstream",
                           "Downstream",
                           "Upstream & downstream")
scope_priority_order <- c("Scope 1| Scope 2", "Scope 1| Scope 2| Scope 3", "Scope 1", "Scope 2", "Scope 2| Scope 3", "Scope 1| Scope 3")


capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

CleanColumnNames_TargetMatching <- function(abs_er)
{ abs_er_clean <- abs_er
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "Provide\\.details\\.of\\.your\\.absolute\\.emissions\\.target\\(s\\)\\.and\\.progress\\.made\\.against\\.those\\.targets\\.\\.", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C4\\.1a_C", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.1\\_C1", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.1\\_C2", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.1\\_C3", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.1\\_C4", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.2\\_C1", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.2\\_C2", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.2\\_C3", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C5\\.2\\_C4", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.1\\_C1", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.1\\_C2", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.1\\_C3", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.1\\_C4", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.3\\_C1", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.3\\_C2", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.3\\_C3", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.3\\_C4", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.5\\_C1", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.5\\_C2", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.5\\_C3", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.5\\_C4", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "C6\\.5\\_C5", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "^[0123456789]", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "^[0123456789]", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "_\\-\\.", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "_", "")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "\\.", " ")
  names(abs_er_clean) <- str_replace_all(names(abs_er_clean), "  ", " ")
  names(abs_er_clean) <- trimws(names(abs_er_clean))
  
  return(abs_er_clean)
}

PrepareAbsTargets <- function(abs_er, year)
{ #2018 abs emission reduction targets
  if (year == 2018)
  { abs_er_form <- abs_er %>%
    rename(account_id = `Account number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           target_id = `Target reference number`, 
           year_target_set = `Start year`,
           scope = `Scope`,
           base_year = `Base year`,
           emissions_base_year = `Base year emissions covered by target (metric tons CO2e)`,
           emissions_base_year_percent = `% emissions in Scope`,
           target_year = `Target year`,
           targeted_reduction = `% reduction from base year`,
           percent_achieved = `% achieved (emissions)`,
           target_status = `Target status`,
           SBTi_status = `Is this a science-based target?`,
           please_explain = `Please explain`) %>%
    mutate(#scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
           target_coverage = NA,
           emissions_target_year = ifelse(is.numeric(targeted_reduction), emissions_base_year*(1-targeted_reduction/100), NA), 
           emissions_reporting_year = ifelse(is.numeric(emissions_target_year), (emissions_base_year-emissions_target_year)*(percent_achieved/100), NA),
           target_ambition = NA)
  }
  
  #2019 abs emission reduction targets
  if (year == 2019)
  { abs_er_form <- abs_er %>%
    rename(account_id = `Account number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           target_id = `Target reference number`, 
           year_target_set = `Start year`,
           scope = `Scope`,
           base_year = `Base year`,
           emissions_base_year = `Base year emissions covered by target (metric tons CO2e)`,
           emissions_base_year_percent = `% emissions in Scope`,
           target_year = `Target year`,
           targeted_reduction = `Targeted % reduction from base year`,
           percent_achieved = `% of target achieved`,
           target_status = `Target status`,
           SBTi_status = `Is this a science-based target?`,
           please_explain = `Please explain`) %>%
    mutate(#scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
           target_coverage = NA,
           emissions_target_year = ifelse(is.numeric(targeted_reduction), emissions_base_year*(1-targeted_reduction/100), NA), 
           emissions_reporting_year = ifelse(is.numeric(emissions_target_year), (emissions_base_year-emissions_target_year)*(percent_achieved/100), NA),
           target_ambition = NA,
           scope=replace(scope, scope=='Scope 2 (location-based) : Redução de Perdas Totais, em GWh', 'Scope 2 (location-based)')
           )
  }
  
  #2020 abs emission reduction targets
  if (year == 2020)
  { abs_er_form <- abs_er %>%
    rename(account_id = `Account number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           target_id = `Target reference number`, 
           year_target_set = `Year target was set`,
           target_coverage = `Target coverage`,
           scope = `Scope(s) (or Scope 3 category)`,
           base_year = `Base year`,
           emissions_base_year = `Covered emissions in base year (metric tons CO2e)`,
           emissions_base_year_percent = `Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
           target_year = `Target year`,
           targeted_reduction = `Targeted reduction from base year (%)`,
           emissions_target_year = `Covered emissions in target year (metric tons CO2e) [auto-calculated]`,
           emissions_reporting_year = `Covered emissions in reporting year (metric tons CO2e)`,
           percent_achieved = `% of target achieved [auto-calculated]`,
           target_status = `Target status in reporting year`,
           SBTi_status = `Is this a science-based target?`,
           please_explain = `Please explain (including target coverage)`) %>%
    mutate(#scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
           target_ambition = NA)
  }
  
  #2021 abs emission reduction targets
  if (year == 2021)
  { abs_er_form <- abs_er %>%
    rename(account_id = `Account number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           target_id = `Target reference number`, 
           year_target_set = `Year target was set`,
           target_coverage = `Target coverage`,
           scope = `Scope(s) (or Scope 3 category)`,
           base_year = `Base year`,
           emissions_base_year = `Covered emissions in base year (metric tons CO2e)`,
           emissions_base_year_percent = `Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`,
           target_year = `Target year`,
           targeted_reduction = `Targeted reduction from base year (%)`,
           emissions_target_year = `Covered emissions in target year (metric tons CO2e) [auto-calculated]`,
           emissions_reporting_year = `Covered emissions in reporting year (metric tons CO2e)`,
           percent_achieved = `% of target achieved [auto-calculated]`,
           target_status = `Target status in reporting year`,
           SBTi_status = `Is this a science-based target?`,
           target_ambition = `Target ambition`,
           please_explain = `Please explain (including target coverage)`) #%>%
    #mutate(scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")))
  }
  
  #2022 abs emission reduction targets
  if (year == 2022)
  # possible scopes:   Scope 1, Scope 1| Scope 2, Scope 1| Scope 2| Scope 3, Scope 1| Scope 3, Scope 2, Scope 2| Scope 3, Scope 3, Question not applicable, NA
  { abs_er_form <- abs_er
    abs_er_form$`Base year Scope 1 emissions covered by target (metric tons CO2e)` <- as.numeric(abs_er_form$`Base year Scope 1 emissions covered by target (metric tons CO2e)`)
    abs_er_form$`Base year Scope 2 emissions covered by target (metric tons CO2e)` <- as.numeric(abs_er_form$`Base year Scope 2 emissions covered by target (metric tons CO2e)`)
    abs_er_form$`Total base year emissions covered by target in all selected Scopes (metric tons CO2e)` <- as.numeric(abs_er_form$`Total base year emissions covered by target in all selected Scopes (metric tons CO2e)`)
    abs_er_form$`Base year Scope 1 emissions covered by target as % of total base year emissions in Scope 1` <- as.numeric(abs_er_form$`Base year Scope 1 emissions covered by target as % of total base year emissions in Scope 1`)
    abs_er_form$`Base year Scope 2 emissions covered by target as % of total base year emissions in Scope 2` <- as.numeric(abs_er_form$`Base year Scope 2 emissions covered by target as % of total base year emissions in Scope 2`)
    abs_er_form$`Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes` <- as.numeric(abs_er_form$`Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes`)
    abs_er_form$`Targeted reduction from base year (%)`<- as.numeric(abs_er_form$`Targeted reduction from base year (%)`)
  
    abs_er_form <- 
    rename(abs_er_form, account_id = `Account number`,
           organization = `Organization`,
           country = `Country/Areas`,
           access = `Public`,
           row = `Row`,
           target_id = `Target reference number`, 
           year_target_set = `Year target was set`,
           target_coverage = `Target coverage`,
           scope = `Scope(s)`,
           #scope_accounting_method = `Scope 2 accounting method`,
           base_year = `Base year`,
           emissions_base_year_s1 = `Base year Scope 1 emissions covered by target (metric tons CO2e)`,
           emissions_base_year_s2 = `Base year Scope 2 emissions covered by target (metric tons CO2e)`,
           emissions_base_year_s3 = `Base year Scope 3 emissions covered by target (metric tons CO2e)`,
           emissions_base_year = `Total base year emissions covered by target in all selected Scopes (metric tons CO2e)`,
           emissions_base_year_percent_s1 = `Base year Scope 1 emissions covered by target as % of total base year emissions in Scope 1`,  
           emissions_base_year_percent_s2 = `Base year Scope 2 emissions covered by target as % of total base year emissions in Scope 2`,
           emissions_base_year_percent_s3 = `Base year Scope 3 emissions covered by target as % of total base year emissions in Scope 3 (in all Scope 3 categories)`,
           emissions_base_year_percent = `Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes`,
           target_year = `Target year`,
           targeted_reduction = `Targeted reduction from base year (%)`,
           emissions_target_year = `Total emissions in target year covered by target in all selected Scopes (metric tons CO2e) [auto-calculated]`,
           emissions_reporting_year_s1 = `Scope 1 emissions in reporting year covered by target (metric tons CO2e)`,
           emissions_reporting_year_s2 = `Scope 2 emissions in reporting year covered by target (metric tons CO2e)`,
           emissions_reporting_year_s3 = `Scope 3 emissions in reporting year covered by target (metric tons CO2e)`,
           emissions_reporting_year = `Total emissions in reporting year covered by target in all selected scopes (metric tons CO2e)`,
           percent_achieved = `% of target achieved relative to base year [auto-calculated]`,
           target_status = `Target status in reporting year`,
           SBTi_status = `Is this a science-based target?`,
           target_ambition = `Target ambition`,
           please_explain = `Please explain target coverage and identify any exclusions`) %>%
    mutate(emissions_base_year=as.numeric(emissions_base_year),
           emissions_base_year_s1=as.numeric(emissions_base_year_s1), 
           emissions_base_year_s2=as.numeric(emissions_base_year_s2), 
           emissions_base_year_s3=as.numeric(emissions_base_year_s3), 
           emissions_base_year_percent_s1=as.numeric(emissions_base_year_percent_s1), 
           emissions_base_year_percent_s2=as.numeric(emissions_base_year_percent_s2), 
           emissions_base_year_percent_s3=as.numeric(emissions_base_year_percent_s3), 
           emissions_reporting_year=as.numeric(emissions_reporting_year), 
           emissions_reporting_year_s1=as.numeric(emissions_reporting_year_s1), 
           emissions_reporting_year_s2=as.numeric(emissions_reporting_year_s2),
           emissions_reporting_year_s3=as.numeric(emissions_reporting_year_s3)) #%>%
  } # if
  
  abs_er_form <- mutate(abs_er_form, year_target_set = as.numeric(year_target_set),
                        base_year = as.numeric(base_year),
                        emissions_base_year = as.numeric(emissions_base_year),
                        emissions_base_year_percent = as.numeric(emissions_base_year_percent),
                        target_year = as.numeric(target_year),
                        targeted_reduction = as.numeric(targeted_reduction),
                        percent_achieved = as.numeric(percent_achieved),
                        emissions_target_year = as.numeric(emissions_target_year),
                        emissions_reporting_year =as.numeric(emissions_reporting_year)) %>%
    
  return(abs_er_form)
}

ProcessSelect_CDPData <- function(abs_er, t_status_include, year)
{ abs_er_processed <- abs_er %>%
  select(account_id, organization, country, access, row, target_id, year_target_set, target_coverage, scope, 
         #scope_accounting_method, 
         any_of(c("Scope 3 category(ies)","Scope 2 accounting method")),
         any_of(c("emissions_base_year_s1", "emissions_base_year_s2", "emissions_base_year_s3", 
                  "emissions_reporting_year_s1", "emissions_reporting_year_s2", "emissions_reporting_year_s3", 
                  "emissions_base_year_s12", "emissions_base_year_percent_s12", "emissions_reporting_year_s12", "emissions_target_year_s12")),
         base_year, emissions_base_year, emissions_base_year_percent,
         target_year, targeted_reduction, emissions_target_year, emissions_reporting_year, percent_achieved, target_status, SBTi_status, target_ambition, please_explain) 
    abs_er_processed_before <- abs_er_processed
    
    # FILTER: improve by also excluding NA for target_id, target_status, emission_base_year_percent       
    abs_er_processed <- filter(abs_er_processed, !(row == 0), 
                               !(target_id%in%c("Question not applicable")), 
                               !is.na(target_year), 
                               target_year>=YEAR, 
                               target_status %in% t_status_include, 
                               !is.na(targeted_reduction),
                               !is.na(emissions_base_year_percent), 
                               !is.na(emissions_base_year), 
                               !is.na(base_year))
    # clean target coverage
    abs_er_processed <- mutate(abs_er_processed, target_coverage=ifelse(str_starts(target_coverage, "Other"), "Other", target_coverage))
  
    # select removed records
    account_ids_before_processing <- select(abs_er, account_id, target_id, row)
    account_ids_after_processing <- select(abs_er_processed, account_id, target_id, row)
    abs_er_removed <- anti_join(abs_er_processed_before, account_ids_after_processing, by=c("account_id", "target_id", "row")) 
    abs_er_removed_no_target <- filter(abs_er_removed, target_id%in%c("Question not applicable"))
    abs_er_removed_missing_data <- filter(abs_er_removed, !target_id%in%c("Question not applicable"))
    write.table(abs_er_processed, paste0(data_dir, "/", "processed/abs_er_used_", YEAR, ".csv"), sep=";", row.names = F)
    write.table(abs_er_removed_no_target, paste0(data_dir, "/", "processed/abs_er_removed_no_target_", YEAR, ".csv"), sep=";", row.names = F)
    write.table(abs_er_removed_missing_data, paste0(data_dir, "/", "processed/abs_er_removed_missing_data_", YEAR, ".csv"), sep=";", row.names = F)
   
    # calculate stats
    n_total = abs_er %>% nrow()
    n_used_data = abs_er_processed %>% nrow()
    n_no_target = abs_er_removed_no_target %>% nrow()
    n_missing_data = abs_er_removed_missing_data %>% nrow()
    
    cat("\n")
    print("+++++++++++++++++++ ProcessSelect_CDPData")
    print(paste0("Total: ", n_total, ", Used: ", n_used_data, ", No target: ", n_no_target, ", Missing data: ", n_missing_data))
    print("+++++++++++++++++++ ProcessSelect_CDPData")
    cat("\n")
    
    stats_process <- data.frame(year=year)
    stats_process <- mutate(stats_process, total_data=abs_er %>% nrow())     
    stats_process <- mutate(stats_process, used_data=abs_er_processed %>% nrow())     
    stats_process <- mutate(stats_process, no_target=abs_er_removed_no_target %>% nrow())
    stats_process <- mutate(stats_process, target_excluded=filter(abs_er_processed_before, target_year<year) %>% nrow())
    stats_process <- mutate(stats_process, missing_data=abs_er_removed_missing_data %>% nrow()) %>%
                     mutate(missing_data=missing_data-target_excluded)
    stats_process <- mutate(stats_process, check=total_data-used_data-no_target-target_excluded-missing_data)  
    stats_process <- mutate(stats_process, row_0=filter(abs_er_processed_before, row==0) %>% nrow())
    stats_process <- mutate(stats_process, target_year_NA=filter(abs_er_processed_before, is.na(target_year)) %>% nrow())
    stats_process <- mutate(stats_process, target_status_status=filter(abs_er_processed_before, !target_status%in%t_status_include) %>% nrow())
    stats_process <- mutate(stats_process, targeted_reduction_NA=filter(abs_er_processed_before, is.na(targeted_reduction)) %>% nrow())
    stats_process <- mutate(stats_process, emissions_base_year_percent=filter(abs_er_processed_before, is.na(emissions_base_year_percent)) %>% nrow())
    stats_process <- mutate(stats_process, emissions_base_yea_NAr=filter(abs_er_processed_before, is.na(emissions_base_year)) %>% nrow())
    stats_process <- mutate(stats_process, base_year_NA=filter(abs_er_processed_before, is.na(base_year)) %>% nrow())
    stats_process <- mutate(stats_process, total_removed=summarise(abs_er_removed, n()) %>% nrow())
    write.table(stats_process, paste0(data_dir, "/", "processed/stats_ProcessSelect_CDPData_", YEAR, ".csv"), sep=";", row.names = F)
    
    return(abs_er_processed)
}

ProcessScopes <- function(abs_er, year)
{ # 1. scope3_categories
  # ImproveScope3Categories
  if (YEAR < 2022)
  { # 1. scope 3 categories
    abs_er <- mutate(abs_er,  scope3_categories=case_when(
                                                  str_starts(scope, "Scope 3") & grepl(":", scope) ~ str_trim(str_split_i(scope, ":", 2)),
                                                  scope%in%c("Scope 3 (downstream)", "Scope 3 (upstream)", "Scope 3 (upstream & downstream)") ~ "Scope 3",
                                                  str_starts(scope, "Other, please specify") ~ "Other",
                                                  grepl("Scope 3|\\+3", scope) ~ str_trim(str_split_i(scope, "\\+3", 2)),
                                                  TRUE ~ ""))
    abs_er <- mutate(abs_er, across('scope3_categories', str_replace, "\\(", ""))
    abs_er <- mutate(abs_er, across('scope3_categories', str_replace, "\\)", ""))
    abs_er <- mutate(abs_er, scope3_categories=capFirst(scope3_categories))
    abs_er <- ImproveScope3Categories(abs_er)
    # check scope 3 categories
    scope3_cat_unknown <- filter(abs_er, !scope3_categories %in% scope3_cat_categories)
    scope3_cat_missing <- unique(scope3_cat_unknown$scope3_categories)
    if (!is.null(nrow(scope3_cat_missing))) {print(paste0("Unknown scope 3 categories: ", scope3_cat_missing))}
  } else 
  { abs_er <- rename(abs_er, scope3_categories=`Scope 3 category(ies)`) %>%
              mutate(scope3_categories=ifelse(scope3_categories=="Question not applicable", NA, scope3_categories))
  abs_er <- ImproveScope3Categories(abs_er)
  
  }
  # 2. scope2_accounting_method
  if (year<2022)
  { abs_er <- mutate(abs_er, scope_accounting_method=ifelse(str_starts(scope, "Other"), NA, str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")))) %>%
              mutate(scope_accounting_method=ifelse(scope_accounting_method%in%c("Question not applicable", "Downstream","Upstream & Downstream","Upstream", "Not Included In Scopes 1 Or 2"), "", scope_accounting_method))
  } else { 
    abs_er <- rename(abs_er, scope_accounting_method=`Scope 2 accounting method`) %>%
              mutate(scope_accounting_method=ifelse(grepl("scope 2", scope, ignore.case=TRUE) & is.na(scope_accounting_method), "Location-based", scope_accounting_method),
                     scope_accounting_method=ifelse(scope_accounting_method=="scope_accounting_method", NA, scope_accounting_method))
  }
  
  # 3. scope_def_2018
  if (year<2022)
  { abs_er <- mutate(abs_er, scope_def_2018=ifelse(str_starts(scope, "Other"), "Other", scope))
  } else
  { abs_er <- mutate(abs_er, scope_def_2018=case_when(trimws(scope) == "Scope 1" ~ "Scope 1",
                                                       trimws(scope) == "Scope 1| Scope 2" & scope_accounting_method == "Location-based" ~ "Scope 1+2 (location-based)",
                                                       trimws(scope) == "Scope 1| Scope 2" & scope_accounting_method == "Market-based" ~ "Scope 1+2 (market-based)",
                                                       trimws(scope) == "Scope 1| Scope 2| Scope 3" & scope_accounting_method == "Location-based" ~ "Scope 1+2 (location-based) +3",
                                                       trimws(scope) == "Scope 1| Scope 2| Scope 3" & scope_accounting_method == "Market-based" ~ "Scope 1+2 (market-based) +3",
                                                       trimws(scope) == "Scope 1| Scope 3|" ~ "Scope 1+3",
                                                       trimws(scope) == "Scope 1| Scope 3" ~ "Scope 1+3",
                                                       trimws(scope) == "Scope 2" & scope_accounting_method == "Location-based" ~ "Scope 2 (location-based)",
                                                       trimws(scope) == "Scope 2" & scope_accounting_method == "Market-based" ~ "Scope 2 (market-based)",
                                                       trimws(scope) == "Scope 2| Scope 3" & scope_accounting_method == "Location-based" ~ "Scope 2 (location-based) +3",
                                                       trimws(scope) == "Scope 2| Scope 3" & scope_accounting_method == "Market-based" ~ "Scope 2 (market-based) +3",
                                                       trimws(scope) == "Scope 3" ~ "Scope 3",
                                                       TRUE ~ "No scope"))
    
  }
  # 4. scope_def_2022
  if (year<2022)
  { scope_1_2_3 = c("Scope 1+2 (location-based) +3 (upstream)", "Scope 1+2 (location-based) +3 (downstream)", "Scope 1+2 (location-based) +3 (upstream & downstream)",
                        "Scope 1+2 (market-based) +3 (upstream)", "Scope 1+2 (market-based) +3 (downstream)", "Scope 1+2 (market-based) +3 (upstream & downstream)")
    abs_er <- mutate(abs_er,  scope_def_2022=case_when(
                                               # scope 1 and 2
                                               scope=="Scope 1" ~ "Scope 1",
                                               scope%in%c("Scope 2 (market-based)", "Scope 2 (location-based)") ~ "Scope 2",
                                               scope%in%c("Scope 1 +2 (market-based)", "Scope 1+2 (market-based)", "Scope 1+2 (location-based)") ~ "Scope 1| Scope 2",
                                               # scope 3
                                               str_starts(scope, "Scope 3") ~ "Scope 3",
                                               scope %in% scope_1_2_3 ~ "Scope 1| Scope 2| Scope 3",
                                               str_starts(scope, "Other, please specify") ~ "Other",
                                               TRUE ~ "")
  )
  } else
  { abs_er <- mutate(abs_er, scope_def_2022 = scope) 
  }
  
  # 5. simple_scope
  abs_er <- AddSimpleScope(abs_er)
  
  # 6. perc_s1s2
  abs_er <- CalculatePercentagecope3(abs_er, year)
  
} # ProcessScopes

AddSimpleScope <- function(abs_er_scope)
{ abs_er_scope <- mutate(abs_er_scope, simple_scope=case_when(scope_def_2018 == "Scope 1" ~ "S1",
                                                              scope_def_2018 == "Scope 2 (location-based)" ~ "S2",
                                                              scope_def_2018 == "Scope 2 (market-based)" ~ "S2",
                                                              scope_def_2018 == "Scope 1+2 (location-based)" ~ "S1S2",
                                                              scope_def_2018 == "Scope 1+2 (market-based)" ~ "S1S2",
                                                              scope_def_2018 == "Scope 1 +2 (market-based)" ~ "S1S2",
                                                              scope_def_2018 == "Scope 1+2 (location-based) +3 (upstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (location-based) +3 (downstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (market-based) +3 (upstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (market-based) +3 (downstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (location-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (market-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                                              scope_def_2018 == "Scope 1+2 (location-based) +3" ~ "S1S2S3", # for 2022 data
                                                              scope_def_2018 == "Scope 1+2 (market-based) +3" ~ "S1S2S3",   # for 2022 data
                                                              scope_def_2018 == "Scope 1+3" ~ "S1S3",
                                                              scope_def_2018 == "Scope 2 (location-based) +3" ~ "S2S3",     # for 2022 data
                                                              scope_def_2018 == "Scope 2 (market-based) +3" ~ "S2S3",       # for, s 2022 data
                                                              grepl('^scope 3', `scope_def_2018`, ignore.case=TRUE) ~ "S3",
                                                              grepl('^other', `scope_def_2018`, ignore.case=TRUE) ~ "Other",
                                                              TRUE ~ 'No scope'))
   
  check_scopes <- filter(abs_er_scope, simple_scope=="No scope", !scope%in%c("No scope", "Question not applicable")) %>%
                  select(account_id, scope, simple_scope)
  warning_scopes_process <- unique(check_scopes$scope)
  if (length(warning_scopes_process)>1)
  { cat("WARNING, not all scopes are included (ProcessSelect_CDPData)\n")
    print(warning_scopes_process)
    cat("\n")
  }

  return(abs_er_scope)
}

CalculatePercentagecope3 <- function(abs_er_scope3, year)
{ # calculate scope 3 emissions for scope 1+2+3 targets
  # 1. read scope 3 percentages
  if (year<2022)
  { filename_scope3_perc = paste0("IKEA_NSA_", YEAR, "_BY_MRY_s3_perc.xlsx")
    var_scope3_perc = paste0("scope3_perc_", YEAR)
    if(exists(var_scope3_perc))
    { print(paste0(var_scope3_perc, " already exists"))
      scope3_perc = eval(parse(text = var_scope3_perc))
    } else{ print(paste("reading in ", var_scope3_perc), sep="\n")
      scope3_perc <- read.xlsx(paste0(data_dir, "output/", filename_scope3_perc), sheet = "Sheet 1")
      scope3_perc <- select(scope3_perc, account_id, starts_with("perc"))
      assign(paste0("scope3_perc_", YEAR), scope3_perc) }
    # 2. calculate scope 3 percentages
    abs_er_scope3 <- left_join(abs_er_scope3, scope3_perc, by=c("account_id")) %>%
                     mutate(perc_s1s2=ifelse(!scope_def_2022%in%c("Scope 1| Scope 2| Scope 3"), 1, 
                                     ifelse(scope_accounting_method=="Location-Based" & `scope3_categories`=="Downstream", perc_S1S2L_S1S2LS3D, 
                                     ifelse(scope_accounting_method=="Location-Based" & `scope3_categories`=="Upstream", perc_S1S2L_S1S2LS3U, 
                                     ifelse(scope_accounting_method=="Location-Based" & `scope3_categories`=="Upstream & downstream", perc_S1S2L_S1S2LS3, 
                                     ifelse(scope_accounting_method=="Market-Based" & `scope3_categories`=="Downstream", perc_S1S2M_S1S2MS3D, 
                                     ifelse(scope_accounting_method=="Market-Based" & `scope3_categories`=="Upstream", perc_S1S2M_S1S2MS3U, 
                                     ifelse(scope_accounting_method=="Market-Based" & `scope3_categories`=="Upstream & downstream", perc_S1S2M_S1S2MS3, "X")))))))) %>%
                     select(-perc_S1S2L_S1S2LS3D, -perc_S1S2L_S1S2LS3U, -perc_S1S2L_S1S2LS3, -perc_S1S2M_S1S2MS3D, -perc_S1S2M_S1S2MS3U, -perc_S1S2M_S1S2MS3)
  } else
  { abs_er_scope3 <- mutate(abs_er_scope3, perc_s1s2=0)
  }
  
  abs_er_scope3$perc_s1s2 <- as.numeric(abs_er_scope3$perc_s1s2)
    
    return(abs_er_scope3)
}

# Scope 3 functions
ImproveScope3Categories <- function(abs_er_scope3)
{ abs_er_scope3 <- mutate(abs_er_scope3, across(scope3_categories, str_replace, "Purchased goods & services", "Purchased goods and services"))
  abs_er_scope3 <- mutate(abs_er_scope3, across(scope3_categories, str_replace, "Upstream transportation & distribution", "Upstream transportation and distribution"))
  abs_er_scope3 <- mutate(abs_er_scope3, across(scope3_categories, str_replace, "Fuel and energy-related activities not included in Scopes 1 or 2", "Fuel-and-energy-related activities (not included in Scopes 1 or 2)"))

return(abs_er_scope3)
}

ImproveScope3Categories_ <- function(abs_er_scope3)
{ #abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Purchased goods & services", "Purchased goods and services"))
  #abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Upstream transportation & distribution", "Upstream transportation and distribution"))
  #abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Fuel and energy-related activities not included in Scopes 1 or 2", "Fuel-and-energy-related activities (not included in Scopes 1 or 2)"))
  
  return(abs_er_scope3)
}
AddPriorityScore_Order <- function(abs_er)
{ last_score <- mutate(abs_er, scope_score=case_when(
                                                 scope_def_2022 == "Scope 1| Scope 2" ~ 1,
                                                 scope_def_2022 == "Scope 1| Scope 2| Scope 3"  ~ 0.9, 
                                                 scope_def_2022 == "Scope 1" ~ 0.8, 
                                                 scope_def_2022 == "Scope 2"  ~ 0.7,
                                                 scope_def_2022 == "Scope 2| Scope 3"  ~ 0.6,
                                                 scope_def_2022 == "Scope 1| Scope 3"  ~ 0.5,
                                                 TRUE ~ 0)) %>%
                mutate(new_target_score = (target_year / 2050)) %>%
                mutate(emi_score = (emissions_base_year_percent / 100)) %>%
                mutate(reduction_score = (targeted_reduction / 100)) %>%
                mutate(base_emissions = emissions_base_year / 10000000000)
  final <- last_score %>% rowwise() %>% mutate(priority_score = sum(scope_score, new_target_score, emi_score, reduction_score, base_emissions,na.rm = TRUE))
  add_back <- final %>% select(-c(scope_score,new_target_score,emi_score,reduction_score,base_emissions))

  return(add_back)
}

Process_BY <- function(base_year_em, year)
{ # in 2022 columns got different name
  if (year %in% c(2018, 2019, 2020, 2021)) 
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `Provide your base year and base year emissions (Scopes 1 and 2) - Base year start`,
           by_end_date = `Provide your base year and base year emissions (Scopes 1 and 2) - Base year end`,
           by_emissions = `Provide your base year and base year emissions (Scopes 1 and 2) - Base year emissions (metric tons CO2e)`) %>%
  select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
  filter(!(by_start_date == "Question not applicable")) %>%
  pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  } else # 2022
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `Provide your base year and base year emissions - Base year start`,
           by_end_date = `Provide your base year and base year emissions - Base year end`,
           by_emissions = `Provide your base year and base year emissions - Base year emissions (metric tons CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
    
  }
  return(base_year_em_form)
}

Process_MRY_s1 <- function(mry_s1_em, col1, col2, change_row = FALSE)
{ 
  mry_s1_em_form <- mry_s1_em %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         #mry_end_year = `C6.1_C2_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.End-year.of.reporting.period`,
         mry_end_year = colnames(mry_s1_em)[col1],
         #mry_emissions = `C6.1_C1_What.were.your.organization's.gross.global.Scope.1.emissions.in.metric.tons.CO2e?.-.Gross.global.Scope.1.emissions.(metric.tons.CO2e)`) %>%
         mry_emissions = colnames(mry_s1_em)[col2]) %>%
  mutate(mry_scope = "Scope 1", 
         row = case_when(change_row & row == "Row 1" ~ "Reporting year",
                         change_row & row == "Row 2" ~ "Past year 1",
                         change_row & row == "Row 3" ~ "Past year 2",
                         change_row & row == "Row 4" ~ "Past year 3",
                         TRUE ~ row)) %>%
  select(c(account_id, access, row, mry_scope, mry_end_year, mry_emissions)) %>%
  filter(!(mry_emissions == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions, mry_end_year), names_vary = "slowest")

  print(colnames(mry_s1_em)[col1])
  print(colnames(mry_s1_em)[col2])
  
  return(mry_s1_em_form)
}  

Process_MRY_s2 <- function(mry_s2_em, col1, col2, col3, change_row = FALSE)
{ mry_s2_em_form <- mry_s2_em %>%
  rename(account_id = `Account number`,
         access = `Public`,
         row = `RowName`,
         #mry_end_year = `C6.3_C3_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.End-year.of.reporting.period`,
         mry_end_year = colnames(mry_s2_em)[col1],
         #mry_emissions_s2l = `C6.3_C1_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.location-based`,
         mry_emissions_s2l = colnames(mry_s2_em)[col2],
         #mry_emissions_s2m = `C6.3_C2_What.were.your.organization's.gross.global.Scope.2.emissions.in.metric.tons.CO2e?.-.Scope.2,.market-based.(if.applicable)`) %>%
         mry_emissions_s2m = colnames(mry_s2_em)[col3]) %>%
  mutate(mry_scope = "Scope 2",
         row = case_when(change_row & row == "Row 1" ~ "Reporting year",
                         change_row & row == "Row 2" ~ "Past year 1",
                         change_row & row == "Row 3" ~ "Past year 2",
                         change_row & row == "Row 4" ~ "Past year 3",
                         TRUE ~ row)) %>%
  select(c(account_id, access, row, mry_scope, mry_end_year, mry_emissions_s2l, mry_emissions_s2m)) %>%
  filter(!(mry_emissions_s2l == "Question not applicable") | !(mry_emissions_s2m == "Question not applicable")) %>%
  pivot_wider(names_from = row, values_from = c(mry_emissions_s2l, mry_emissions_s2m, mry_end_year), names_vary = "slowest") 

  print(colnames(mry_s2_em)[col1])
  print(colnames(mry_s2_em)[col2])
  print(colnames(mry_s2_em)[col3])

  return(mry_s2_em_form)
}

PivotDatasetByRoot_id_prof2 <- function(abs_er_form_alt_post1)
{ abs_er_prof2 <- abs_er_form_alt_post1 %>%
  group_by(root_id) %>%
  arrange(target_year) %>%
  mutate(group_count = 1:n()) %>%
  pivot_wider(id_cols = c(account_id, organization, country, access, target_coverage, simple_scope, scope, base_year,
                          emissions_base_year, emissions_base_year_percent), 
              names_from = group_count, 
              names_vary = "slowest",
              values_from = c(target_id, target_year, targeted_reduction, emissions_target_year, 
                              emissions_reporting_year, percent_achieved, target_status, SBTi_status,
                              target_ambition), 
              values_fill = NA) %>%
  mutate(target_profile = "prof2") %>%

  return(abs_er_prof2)
  
}

PivotDatasetByRoot_id_prof3 <- function(abs_er_form_alt_post2)
{ abs_er_prof3 <- abs_er_form_alt_post2 %>%
    group_by(root_id) %>%
    arrange(scope) %>%
    mutate(group_count = 1:n()) %>%
    pivot_wider(id_cols = c(account_id, organization, country, access, target_coverage, base_year, target_year), 
                names_from = group_count, 
                names_vary = "slowest",
                values_from = c(target_id, scope, emissions_base_year, emissions_base_year_percent, targeted_reduction, emissions_target_year, 
                                emissions_reporting_year, percent_achieved, target_status, SBTi_status,
                                target_ambition), 
                values_fill = NA) %>%
    mutate(target_profile = "prof3") %>%

    return(abs_er_prof3)
}
CheckEmissions2022 <- function(abs_er_check)
{ # check if emissions are adding up
  check_emissions_2022 <- select(abs_er_check, account_id, target_id, row, 
                                 emissions_base_year, emissions_base_year_s1, emissions_base_year_s2, emissions_base_year_s3,
                                 emissions_reporting_year, emissions_reporting_year_s1, emissions_reporting_year_s2, emissions_reporting_year_s3) %>%
                          mutate(emissions_base_year=ifelse(is.na(emissions_base_year), 0, emissions_base_year),
                                 emissions_base_year_s1=ifelse(is.na(emissions_base_year_s1), 0, emissions_base_year_s1),
                                 emissions_base_year_s2=ifelse(is.na(emissions_base_year_s2), 0, emissions_base_year_s2),
                                 emissions_base_year_s3=ifelse(is.na(emissions_base_year_s3), 0, emissions_base_year_s3),
                                 emissions_reporting_year=ifelse(is.na(emissions_reporting_year), 0,emissions_reporting_year),
                                 emissions_reporting_year_s1=ifelse(is.na(emissions_reporting_year_s1), 0,emissions_reporting_year_s1),
                                 emissions_reporting_year_s2=ifelse(is.na(emissions_reporting_year_s2), 0,emissions_reporting_year_s2),
                                 emissions_reporting_year_s3=ifelse(is.na(emissions_reporting_year_s3), 0, emissions_reporting_year_s3)) %>%
                          mutate(diff_base_year = emissions_base_year-emissions_base_year_s1-emissions_base_year_s2-emissions_base_year_s3,
                                 diff_reporting_year = emissions_reporting_year-emissions_reporting_year_s1-emissions_reporting_year_s2-emissions_reporting_year_s3,
                                 diff_base_year_perc = emissions_base_year/(emissions_base_year_s1+emissions_base_year_s2+emissions_base_year_s3),
                                 diff_reporting_year_perc = emissions_reporting_year/(emissions_reporting_year_s1+emissions_reporting_year_s2+emissions_reporting_year_s3))
   write.table(check_emissions_2022, paste0(data_dir, 'processed/check_emissions_2022.csv'), sep=";", row.names = F)
}

ListValues <- function(abs_er_form, y)
{ tmp1 <- unique(abs_er_form$target_coverage)
  tmp2 <- unique(abs_er_form$scope)
  tmp3 <- unique(abs_er_form$target_status)
  tmp4 <- unique(abs_er_form$scope3_categories)
  tmp5 <- unique(abs_er_form$scope_accounting_method)
  tmp6 <- unique(abs_er_form$scope_def_2018)
  tmp7 <- unique(abs_er_form$scope_def_2022)
  
  write.table(tmp1, paste0(data_dir, "processed/factors/target_coverage_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp2, paste0(data_dir, "processed/factors/scope_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp3, paste0(data_dir, "processed/factors/target_status_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp4, paste0(data_dir, "processed/factors/scope3_categories_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp5, paste0(data_dir, "processed/factors/scope_accounting_method_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp6, paste0(data_dir, "processed/factors/scope_def_2018_", y, ".csv"), sep=";", row.names = F)
  write.table(tmp7, paste0(data_dir, "processed/factors/scope_def_2022_", y, ".csv"), sep=";", row.names = F)
}