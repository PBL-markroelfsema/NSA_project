# stats
companies_check <- companies_form_prepared
targets_check <- targets_form_prepared 

abs_er_check <- filter(abs_er_form_prepared, !target_id=="Question not applicable", !is.na(target_id))
nr_companies_with_no_targets <- nrow(filter(targets_check, active_targets%in%c("No target", NA)))
nr_companies <- length(unique(companies_check$account_id))
nr_companies_with_absolute_targets <- length(unique(abs_er_check$account_id))
nr_absolute_targets <- nrow(abs_er_check)
log_print(paste0("REPORT - Number of companies: ", nr_companies))
log_print(paste0("REPORT - Number of companies with no targets: ", nr_companies_with_no_targets))
log_print(paste0("REPORT - Number of companies with absolute targets: ", nr_companies_with_absolute_targets))
log_print(paste0("REPORT - Number of absolute targets: ", nr_absolute_targets))

nr_processed_targets <- nrow(abs_er_form_processed)
nr_processed_companies <- length(unique(abs_er_form_processed$account_id))
nr_current_year_targets <- nrow(filter(abs_er_form_processed, target_year==YEAR))
nr_current_year_companies <- length(unique(filter(abs_er_form_processed, target_year==YEAR)))
perc_current_year_targets <- round(100*nr_current_year_targets/nr_processed_targets, 1)
perc_current_year_companies <- round(100*nr_current_year_companies/nr_processed_companies, 1)

log_print(paste0("REPORT - Number of targets in disclosure year: ", nr_current_year_targets))
log_print(paste0("         Percentage of total targets: ", perc_current_year_targets))
log_print(paste0("REPORT - Number of companies with targets in disclosure year: ", nr_current_year_targets))
log_print(paste0("         Percentage of total companies: ", perc_current_year_companies))