# STATISTICS

# Count unique numbers of companies in each dataset
#actid_uni_abs_2018 <- unique(abs_er_2018$`Account.number`)
#actid_uni_abs_2019 <- unique(abs_er_2019$`Account.number`)
#actid_uni_abs_2020 <- unique(abs_er_2020$`Account.number`)
#actid_uni_abs_2021 <- unique(abs_er_2021$`Account.number`)
#actid_uni_abs_2022 <- unique(abs_er_2022$`Account.number`)
#rint(paste0("2018: ", length(actid_uni_abs_2018))); print(paste0("2019: ", length(actid_uni_abs_2019))); print(paste0("2020: ", length(actid_uni_abs_2020))); print(paste0("2021: ", length(actid_uni_abs_2021))); print(paste0("2022: ", length(actid_uni_abs_2022)))
#abs_act_overlap_log <- actid_uni_abs_2018 %in% actid_uni_abs_2021
#abs_overlap_count <- sum(abs_act_overlap_log == TRUE)

#prof_uni_abs_2018 <- unique(abs_er_prof_2018$`account_id`)
#prof_uni_abs_2019 <- unique(abs_er_prof_2019$`account_id`)
#rof_uni_abs_2020 <- unique(abs_er_prof_2020$`account_id`)
#prof_uni_abs_2021 <- unique(abs_er_prof_2021$`account_id`)
#prof_uni_abs_2022 <- unique(abs_er_prof_2022$`account_id`)
#print(paste0("2018: ", length(prof_uni_abs_2018))); print(paste0("2019: ", length(prof_uni_abs_2019))); print(paste0("2020: ", length(prof_uni_abs_2020))); print(paste0("2021: ", length(prof_uni_abs_2021))); print(paste0("2022: ", length(prof_uni_abs_2022)))

# Statistics for companies included in the different profiles
#remaining_tar_accids_2018 <- unique(abs_er_2018_prof4$account_id)
#remaining_tar_accids_2019 <- unique(abs_er_2019_prof4$account_id)
#remaining_tar_accids_2020 <- unique(abs_er_2020_prof4$account_id)
#remaining_tar_accids_2021 <- unique(abs_er_2021_prof4$account_id)

#companies_per_profile1_2018 <- single_tar_accids_2018 %>% as.data.frame() %>% mutate(profile_2018 = 1)
#companies_per_profile2_2018 <- seq_tar_accids_2018 %>% as.data.frame() %>% mutate(profile_2018 = 2)
#ompanies_per_profile3_2018 <- para_tar_accids_2018 %>% as.data.frame() %>% mutate(profile_2018 = 3)
#companies_per_profile4_2018 <- remaining_tar_accids_2018 %>% as.data.frame() %>% mutate(profile_2018 = 4)
#companies_per_profle_2018 <- rbind(companies_per_profile1_2018, companies_per_profile2_2018) %>% rbind(companies_per_profile3_2018) %>% rbind(companies_per_profile4_2018)
#colnames(companies_per_profle_2018) <- c('account_id', 'profile_2018')
##rm(companies_per_profile1_2018); rm(companies_per_profile2_2018); rm(companies_per_profile3_2018); rm(companies_per_profile4_2018)

#companies_per_profile1_2019 <- single_tar_accids_2019 %>% as.data.frame() %>% mutate(profile_2019 = 1)
#companies_per_profile2_2019 <- seq_tar_accids_2019 %>% as.data.frame() %>% mutate(profile_2019 = 2)
#companies_per_profile3_2019 <- para_tar_accids_2019 %>% as.data.frame() %>% mutate(profile_2019 = 3)
#companies_per_profile4_2019 <- remaining_tar_accids_2019 %>% as.data.frame() %>% mutate(profile_2019 = 4)
#ompanies_per_profle_2019 <- rbind(companies_per_profile1_2019, companies_per_profile2_2019) %>% rbind(companies_per_profile3_2019) %>% rbind(companies_per_profile4_2019)
#colnames(companies_per_profle_2019) <- c('account_id', 'profile_2019')
##rm(companies_per_profile1_2019); rm(companies_per_profile2_2019); rm(companies_per_profile3_2019); rm(companies_per_profile4_2019)

#companies_per_profile1_2020 <- single_tar_accids_2020 %>% as.data.frame() %>% mutate(profile_2020 = 1)
#companies_per_profile2_2020 <- seq_tar_accids_2020 %>% as.data.frame() %>% mutate(profile_2020 = 2)
#companies_per_profile3_2020 <- para_tar_accids_2020 %>% as.data.frame() %>% mutate(profile_2020 = 3)
#companies_per_profile4_2020 <- remaining_tar_accids_2020 %>% as.data.frame() %>% mutate(profile_2020 = 4)
#companies_per_profle_2020 <- rbind(companies_per_profile1_2020, companies_per_profile2_2020) %>% rbind(companies_per_profile3_2020) %>% rbind(companies_per_profile4_2020)
#colnames(companies_per_profle_2020) <- c('account_id', 'profile_2020')
#rm(companies_per_profile1_2020); rm(companies_per_profile2_2020); rm(companies_per_profile3_2020); rm(companies_per_profile4_2020)

#companies_per_profile1_2021 <- single_tar_accids_2021 %>% as.data.frame() %>% mutate(profile_2021 = 1)
#companies_per_profile2_2021 <- seq_tar_accids_2021 %>% as.data.frame() %>% mutate(profile_2021 = 2)
#companies_per_profile3_2021 <- para_tar_accids_2021 %>% as.data.frame() %>% mutate(profile_2021 = 3)
#companies_per_profile4_2021 <- remaining_tar_accids_2021 %>% as.data.frame() %>% mutate(profile_2021 = 4)
#companies_per_profle_2021 <- rbind(companies_per_profile1_2021, companies_per_profile2_2021) %>% rbind(companies_per_profile3_2021) %>% rbind(companies_per_profile4_2021)
#colnames(companies_per_profle_2021) <- c('account_id', 'profile_2021')
##rm(companies_per_profile1_2021); rm(companies_per_profile2_2021); rm(companies_per_profile3_2021); rm(companies_per_profile4_2021)

#companies_per_profile <- full_join(companies_per_profle_2018, companies_per_profle_2019, by=c('account_id')) %>% full_join(companies_per_profle_2020, by=c('account_id')) %>% full_join(companies_per_profle_2021, by=c('account_id')) %>%
#                         mutate(profile=TRUE)
#check1 <- left_join(companies_in_full_target_sets, companies_per_profile, by=c('account_id')) %>%
#          mutate(profile=replace(profile, is.na(profile), FALSE))
#check2 <- left_join(companies_full_target_set, check1, by=c('account_id')) %>% 
#          distinct() %>%
#          mutate(profile=replace(profile, is.na(profile), FALSE))

statistics_table <- spread(statistics, key=reporting_year, value=value)
s1 <- filter(statistics_table, item=='targets')
s2 <- filter(statistics_table, item=='companies')
s3 <- filter(statistics_table, item=='target reference number')
print("Number of targets")
print(s1)
print("Number of companies")
print(s2)
print("Nr of empty target rreference numbers")
print(s3)
print("Scopes")
print("2018: ", )
print(unique(abs_er_form_2018$simple_scope))
print("2019: ", )
print(unique(abs_er_form_2019$simple_scope))
print("2020: ", )
print(unique(abs_er_form_2020$simple_scope))
print("2021: ", )
print(unique(abs_er_form_2021$simple_scope))
print("2022: ", )
print(unique(abs_er_form_2022$simple_scope))

