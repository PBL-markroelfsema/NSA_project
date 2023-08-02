library(stringr)

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
    mutate(scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
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
    mutate(scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
           target_coverage = NA,
           emissions_target_year = ifelse(is.numeric(targeted_reduction), emissions_base_year*(1-targeted_reduction/100), NA), 
           emissions_reporting_year = ifelse(is.numeric(emissions_target_year), (emissions_base_year-emissions_target_year)*(percent_achieved/100), NA),
           target_ambition = NA,
           #Scope=replace(Scope, Scope=='Scope 2 (location-based) : Redução de Perdas Totais, em GWh', 'Scope 2 (location-based)')
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
    mutate(scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")),
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
           please_explain = `Please explain (including target coverage)`) %>%
    mutate(scope_accounting_method = str_to_title(str_replace_all(str_extract(scope, "\\([^()]+\\)"), "[()]", "")))
  }
  
  #2022 abs emission reduction targets
  if (year == 2022)
  # possible scopes:   Scope 1, Scope 1; Scope 2, Scope 1; Scope 2; Scope 3, Scope 1; Scope 3, Scope 2, Scope 2; Scope 3, Scope 3, Question not applicable, NA
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
           scope_ = `Scope(s)`,
           scope_accounting_method = `Scope 2 accounting method`,
           base_year = `Base year`,
           emissions_base_year_s1 = `Base year Scope 1 emissions covered by target (metric tons CO2e)`,
           emissions_base_year_s2 = `Base year Scope 2 emissions covered by target (metric tons CO2e)`,
           emissions_base_year = `Total base year emissions covered by target in all selected Scopes (metric tons CO2e)`,
           emissions_base_year_percent_s1 = `Base year Scope 1 emissions covered by target as % of total base year emissions in Scope 1`,  
           emissions_base_year_percent_s2 = `Base year Scope 2 emissions covered by target as % of total base year emissions in Scope 2`,
           emissions_base_year_percent = `Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes`,
           target_year = `Target year`,
           targeted_reduction = `Targeted reduction from base year (%)`,
           emissions_target_year = `Total emissions in target year covered by target in all selected Scopes (metric tons CO2e) [auto-calculated]`,
           emissions_reporting_year_s1 = `Scope 1 emissions in reporting year covered by target (metric tons CO2e)`,
           emissions_reporting_year_s2 = `Scope 2 emissions in reporting year covered by target (metric tons CO2e)`,
           emissions_reporting_year = `Total emissions in reporting year covered by target in all selected scopes (metric tons CO2e)`,
           percent_achieved = `% of target achieved relative to base year [auto-calculated]`,
           target_status = `Target status in reporting year`,
           SBTi_status = `Is this a science-based target?`,
           target_ambition = `Target ambition`,
           please_explain = `Please explain target coverage and identify any exclusions`) %>%
    mutate(scope_accounting_method = ifelse(grepl("scope 2", scope_, ignore.case=TRUE) & is.na(scope_accounting_method), "Location-based", scope_accounting_method),
           scope=case_when(trimws(scope_) == "Scope 1" ~ "Scope 1",
                           trimws(scope_) == "Scope 1; Scope 2" & scope_accounting_method == "Location-based" ~ "Scope 1+2 (location-based)",
                           trimws(scope_) == "Scope 1; Scope 2" & scope_accounting_method == "Market-based" ~ "Scope 1+2 (market-based)",
                           trimws(scope_) == "Scope 1; Scope 2; Scope 3" & scope_accounting_method == "Location-based" ~ "Scope 1+2 (location-based) +3",
                           trimws(scope_) == "Scope 1; Scope 2; Scope 3" & scope_accounting_method == "Market-based" ~ "Scope 1+2 (market-based) +3",
                           trimws(scope_) == "Scope 1; Scope 3;" ~ "Scope 1+3",
                           trimws(scope_) == "Scope 1; Scope 3" ~ "Scope 1+3",
                           trimws(scope_) == "Scope 2" & scope_accounting_method == "Location-based" ~ "Scope 2 (location-based)",
                           trimws(scope_) == "Scope 2" & scope_accounting_method == "Market-based" ~ "Scope 2 (market-based)",
                           trimws(scope_) == "Scope 2; Scope 3" & scope_accounting_method == "Location-based" ~ "Scope 2 (location-based) +3",
                           trimws(scope_) == "Scope 2; Scope 3" & scope_accounting_method == "Market-based" ~ "Scope 2 (market-based) +3",
                           trimws(scope_) == "Scope 3" ~ "Scope 3",
                           TRUE ~ "No scope"),
           emissions_base_year_s12 = emissions_base_year_s1 + emissions_base_year_s2,
           emissions_base_year_percent_s12 = (emissions_base_year_percent_s1*emissions_base_year_s1 + emissions_base_year_percent_s2*emissions_base_year_s2)/emissions_base_year,
           emissions_target_year_s12 = emissions_base_year_s12*(1-targeted_reduction))
    
    check_scopes <- filter(abs_er_form, scope=="No scope", scope!='Question not applicable') %>%
                    select(account_id, scope_, scope, scope_accounting_method)
    warning_scopes_select <- unique(check_scopes$scope_)
    for (i in 1:(length(warning_scopes_select)))
    { if (!warning_scopes_select[i]%in%c(NA, "Question not applicable"))
      { cat("WARNING, not all scopes are included (PrepareAbsTargets)\n")
        cat(warning_scopes_select[i])
        cat("\n")
      } #if
    } # for
  } # if

  return(abs_er_form)
}

ProcessSelect_CDPData <- function(abs_er, t_status_include, year)
{ abs_er_processed <- abs_er %>%
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
                                scope == "Scope 1+2 (market-based)" ~ "S1S2",
                                scope == "Scope 1 +2 (market-based)" ~ "S1S2",
                                scope == "Scope 1+2 (location-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (market-based) +3 (upstream & downstream)" ~ "S1S2S3",
                                scope == "Scope 1+2 (location-based) +3" ~ "S1S2S3", # for 2022 data
                                scope == "Scope 1+2 (market-based) +3" ~ "S1S2S3",   # for 2022 data
                                scope == "Scope 1+3" ~ "S1S3",
                                scope == "Scope 2 (location-based) +3" ~ "S2S3",     # for 2022 data
                                scope == "Scope 2 (market-based) +3" ~ "S2S3",       # for, s 2022 data
                                grepl('^scope 3', `scope`, ignore.case=TRUE) ~ "S3",
                                grepl('^other', `scope`, ignore.case=TRUE) ~ "Other",
                                TRUE ~ 'No scope')) %>%
    select(id, c(account_id, organization, country, access, row, target_id, year_target_set, target_coverage, scope, scope_accounting_method, simple_scope,  base_year, emissions_base_year, emissions_base_year_percent,
             target_year, targeted_reduction, emissions_target_year, emissions_reporting_year, percent_achieved, target_status, SBTi_status, target_ambition, please_explain)) #%>%
    
    check_scopes <- filter(abs_er_processed, simple_scope=="No scope", !scope%in%c("No scope", "Question not applicable")) %>%
                    select(account_id, scope, simple_scope)
    warning_scopes_process <- unique(check_scopes$scope)
    if (length(warning_scopes_process)>1)
    { cat("WARNING, not all scopes are included (ProcessSelect_CDPData)\n")
      print(warning_scopes_process)
      cat("\n")
    }
    
    stat_before <- abs_er_processed %>% summarise(total_targets_before = n(),
                                           total_companies_before =n_distinct(account_id), 
                                           total_deselected = sum(is.na(row) | is.na(target_id) | target_id=='Question not applicable' | is.na(target_year) | !(target_status %in% t_status_include) | simple_scope=="S3" | simple_scope=="Other" | emissions_base_year_percent<75, na.rm=T),
                                           row_empty = sum(is.na(row)),
                                           target_id_empty = sum(target_id=='Question not applicable', na.rm=T) ,
                                           target_year_empty = sum(is.na(target_year)),
                                           target_status_exclude = sum(!target_status%in%t_status_include),
                                           scope_exclude = sum(simple_scope=="S3", na.rm=TRUE)+sum(simple_scope=="Other", na.rm=TRUE)+sum(simple_scope=="No scope", na.rm=TRUE),
                                           
                                           #tmp_scope_exclude_S3 = sum(simple_scope=="S3", na.rm=TRUE),
                                           #tmp_scope_exclude_Other = sum(simple_scope=="Other", na.rm=TRUE),
                                           #tmp_scope_exclude_NA = sum(simple_scope=="No scope", na.rm=TRUE),
                                           
                                           by_perc_smaller_75 = sum(emissions_base_year_percent<75, na.rm=TRUE)) %>%
                                           mutate(reporting_year=year) %>%
                                           select(reporting_year, everything())
    write.table(stat_before, paste0('Ambition_scripts/data/CDP_2023/output/stats/stats_before_', year, ".csv"), sep=";", row.names=F)
         
    # improve by also excluding NA for target_id, target_status, emission_base_year_percent       
    abs_er_processed <- filter(abs_er_processed, !(row == 0), !(target_id == "Question not applicable"), !is.na(target_year), target_status %in% t_status_include,
                               !(simple_scope %in% c("S3", "Other", "", "No scope", "Question not applicable", NA)), emissions_base_year_percent >= 75)
  
    stat_after <- abs_er_processed %>% summarise(total_targets_after = n(),
                                                 total_companies_after =n_distinct(account_id)) %>%
                                                  mutate(reporting_year=year) %>%
                                                  select(reporting_year, everything())
    write.table(stat_after, paste0('Ambition_scripts/data/CDP_2023/output/stats/stats_after_', year, ".csv"), sep=";", row.names=F)
                                                  
    return(abs_er_processed)
}

AddPriorityScore_Order <- function(abs_er)
{ score_df <- abs_er %>%
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
                              target_ambition, please_explain), 
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
                                target_ambition, please_explain), 
                values_fill = NA) %>%
    mutate(target_profile = "prof3") %>%

    return(abs_er_prof3)
}

# Scope 3 functions
ImproveScope3Categories <- function(abs_er_scope3)
{ abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Purchased goods & services", "Purchased goods and services"))
  abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Upstream transportation & distribution", "Upstream transportation and distribution"))
  abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Fuel and energy-related activities not included in Scopes 1 or 2", "Fuel-and-energy-related activities (not included in Scopes 1 or 2)"))
  #abs_er_scope3 <- mutate(abs_er_scope3, across(`Scope 3 categories`, str_replace, "Investments", "Investments"))
  
  return(abs_er_scope3)
}