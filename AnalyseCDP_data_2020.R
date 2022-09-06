library(stringr)
library(readxl)

# 2.b. remove duplicates (for private responses (with NA as Global S1))
absolute_data_select <- group_by(absolute_data_select, account_id, company_name, `Country/Region`, Scope_long, Scope_short, `Target year`, `Base year`) %>% 
                         mutate(`Global S1`=ifelse(is.na(`Global S1`),0,`Global S1`)) %>%
                         top_n(n=1, wt=`Global S1`) %>%
                         top_n(n=1, wt=`Year target was set`) %>%
                         top_n(n=1, wt=`Perc_EM_scope_BY`) %>%
                         top_n(n=1, wt=`Covered emissions in base year (metric tons CO2e)`) %>%
                         top_n(n=1, wt=`Targeted reduction from base year (%)`) %>% # Prairiesky has one target achieved, and one underway --> choose highest targets
                         # below are duplicates that only differ in S1/S2L
                         top_n(n=1, wt=`Global S2L`) %>% # for example c, Inc. - Australia
                         top_n(n=1, wt=`Country S2L`) %>% # for example Salesforce.com, Inc. - United States
                         top_n(n=1, wt=`Country S1`) %>% # for example Equinor. - Norway
  
                         select(account_id, company_name, Scope_long, Scope_short, `Target year`, `Base year`, everything())
write.table(absolute_data_select, 'data/CDP/output/absolute_data_select.csv', sep=";", row.names = FALSE)
cat(paste0("absolute data after removing duplicates: ", nrow(absolute_data_select), "\n"))

# 2.c. check again for duplicates
check_duplicates2 <- select(absolute_data_select, all_of(cols_id)) %>%
                             group_by(`account_id`, `company_name`, `Country/Region`, Scope_long, `Base year`, `Target year`) %>%
                             summarise(count=n()) %>%
                             filter(count>1)
cat(paste0("check duplicates again: ", nrow(check_duplicates2), " (SHOULD BE ZERO!)\n")) 

#------------------------------------------------

# Calculate EXCL_SCOPE3
absolute_data_target_data <- absolute_data_select

# 3a. Scope 3 emissions are excluded, so change "Scope 1+2+3" to "Scope 1+2" and  "Scope 1+3" to  "Scope 1" --> TO DO
# Delete occurrences where Scope is Scope 1 and Country S1=0 or Scope 2 and Country S2L+S2M=0 and percent_alloc is zero or NA --> no calculations of BY emissions possible
absolute_data_target_data <- filter(absolute_data_target_data, !(Scope=="Scope 1" & `Country S1`==0 & percent_alloc%in%c(0, NA)))
absolute_data_target_data <- filter(absolute_data_target_data, !(Scope=="Scope 2" & `Country S2L`+`Country S2M`==0 & percent_alloc%in%c(0, NA)))

# 3b. Calculate help variables (including renaming a few variables due to long names)
# Target only applies to "% emissions in Scope" for BY and MRY emissions and covered by country branch percentage allocation (percent_alloc). 
# Note that "Base year emissions covered by target" already applies to full target and does not need to be multiplied with `% emissions in Scope`
absolute_data_target_data <- rename(absolute_data_target_data, 
                                #BY_EM_target=`Base year emissions covered by target (metric tons CO2e)`,
                                BY_EM_target=`Covered emissions in base year (metric tons CO2e)`,
                                BY_EM_100=`Base year emissions (100% of scope)`, 
                                BY_EM_100_excl_scope3=`Base year emissions (100% of scope, excl. scope 3)`,
                                MRY_EM_100=`MRY emissions (100% of scope)`, 
                                MRY_EM_100_excl_scope3=`MRY emissions (100% of scope, excl. scope 3)`)
absolute_data_target_data <- mutate(absolute_data_target_data, BY_EM_target=BY_EM_target*percent_alloc,
                                    BY_EM_100=BY_EM_100*(1/100)*Perc_EM_scope_BY*percent_alloc, 
                                    BY_EM_100_excl_scope3=BY_EM_100_excl_scope3*(1/100)*Perc_EM_scope_BY*percent_alloc,
                                    MRY_EM_100=MRY_EM_100*(1/100)*Perc_EM_scope_BY*percent_alloc, 
                                    MRY_EM_100_excl_scope3=MRY_EM_100_excl_scope3*(1/100)*Perc_EM_scope_BY*percent_alloc)
absolute_data_target_data <- mutate(absolute_data_target_data, target_type="absolute",
                                    BY_tmp = ifelse(!BY_EM_target%in%c(0,NA), BY_EM_target, 
                                                ifelse(!BY_EM_100_excl_scope3%in%c(0,NA), BY_EM_100_excl_scope3*(1/100), BY_EM_100*(1/100))),
                                   `BaseYearEmissions_excl_scope3`=ifelse(`Country impact (excl. scope 3)`%in%c(0,NA), BY_tmp, 100*`Country impact (excl. scope 3)`/`Targeted reduction from base year (%)`), 
                                   S1=ifelse(Scope1=="S1", `Country S1`, 0),
                                   S2=ifelse(Scope2=="S2M", `Country S2M`, ifelse(Scope2=="S2L", `Country S2L`, 
                                                                               ifelse(Scope2=="max",ifelse(`Country S2M`>`Country S2L`,`Country S2M`,`Country S2L`),0))),
                                   perc_scope1 = ifelse(S1==0 & S2==0,0,S1/(S1+S2)),
                                   ratio_EM_excl_incl_scope3=ifelse(!(BY_EM_100%in%c(0,NA) | BY_EM_100_excl_scope3%in%c(0,NA)),BY_EM_100_excl_scope3/BY_EM_100,
                                                                 ifelse(!(MRY_EM_100%in%c(0,NA) | MRY_EM_100_excl_scope3%in%c(0,NA)),MRY_EM_100_excl_scope3/MRY_EM_100, 1)),
                                   TargetYearEmissions_excl_scope3 = (1-`Targeted reduction from base year (%)`/100)*BaseYearEmissions_excl_scope3,
                                   MRY_EM_excl_scope3_interpolated = ifelse(`Most recent accounting year`==`Base year`, BaseYearEmissions_excl_scope3, BaseYearEmissions_excl_scope3+((`Most recent accounting year`-`Base year`)/(`Target year`-`Base year`))*(TargetYearEmissions_excl_scope3-BaseYearEmissions_excl_scope3)),
                                   MRY_EM_excl_scope3=ifelse(MRY_EM_100%in%c(0,NA) & MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_excl_scope3_interpolated,
                                                          ifelse(!MRY_EM_100_excl_scope3%in%c(0,NA),MRY_EM_100_excl_scope3,MRY_EM_100*ratio_EM_excl_incl_scope3))
)
absolute_data_target_data <- mutate(absolute_data_target_data, `BaseYearEmissions_excl_scope3`=ifelse(is.na(`BaseYearEmissions_excl_scope3`), 0, `BaseYearEmissions_excl_scope3`))
absolute_data_target_data <- mutate(absolute_data_target_data, `MRY_EM_excl_scope3`=ifelse(is.na(`MRY_EM_excl_scope3`), 0, `MRY_EM_excl_scope3`))
check_perc_scope1_zero <- filter(absolute_data_target_data, is.na(perc_scope1))
cat("records with NA in perc_scope1: ", nrow(check_perc_scope1_zero), "\n")

# 3.c. If perc_scope1 is NA change to 40% for Scope1+2, 100% for Scope 1 and 0% for Scope 2
absolute_data_target_data <- mutate(absolute_data_target_data, perc_scope1 = ifelse(!is.na(perc_scope1), perc_scope1, ifelse(Scope%in%c("Scope 1+2", "Scope 1+2+3"), 0.4, ifelse(Scope%in%c("Scope 1","Scope 1+3"), 1, 0))))
absolute_data_target_data$BaseYearEmissions_excl_scope3 <- as.numeric(absolute_data_target_data$BaseYearEmissions_excl_scope3)
absolute_data_target_data$TargetYearEmissions_excl_scope3 <- as.numeric(absolute_data_target_data$TargetYearEmissions_excl_scope3)
write.table(absolute_data_target_data, "data/CDP/output/absolute_data_target_data.csv", sep=";", col.names = TRUE, row.names=FALSE)
cat(paste0("number of records in target data: ", nrow(absolute_data_target_data), "\n")) 

#-----------------------------

# TO DO!! ##
# 4 calculate BY, MR emissions per company/scope combination
absolute_data_aggregated <- group_by(absolute_data_process, account_id, company_name, Scope_long, `Base year`, `Target year`, `Target coverage`,`Targeted reduction from base year (%)`) %>%
                                 summarise(BY_covered=sum(`Covered emissions in base year (metric tons CO2e)`, na.rm=TRUE),
                                           BY_100=sum(`Base year emissions (100% of scope)`, na.rm=TRUE),
                                           BY_100_excl_scope3=sum(`Base year emissions (100% of scope, excl. scope 3)`, na.rm=TRUE),
                                           MRY_100=sum(`MRY emissions (100% of scope)`, na.rm=TRUE),
                                           MRY_100_scope3=sum(`MRY emissions (100% of scope, excl. scope 3)`, na.rm=TRUE),
                                           nr_countries=n()) %>%
                                 select(nr_countries, everything())
write.table(absolute_data_aggregated, 'data/CDP/output/absolute_data_aggregated.csv', sep=";", row.names = FALSE)

# 3a. select scope
absolute_data_select_scope <- filter(absolute_data_aggregated, substr(Scope_long, 1, 7)!="Scope 3",
                                                                         substr(Scope_long, 1, 5)!="Other")
write.table(absolute_data_select_scope, 'data/CDP/output/absolute_data_select_scope.csv', sep=";", row.names = FALSE)

# 3b --> TO DO: calcualte scope1+2 if both scope1 and scope 2 exist and target same year with same reduction target

# 3c selection
