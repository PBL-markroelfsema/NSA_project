library(zeallot)

# function to add columns
add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) !=0 ) df[add] <- NA
  return(df)
}

CheckScopes <- function(d)
{ #Scope 1| Scope 2",  "Scope 1| Scope 2| Scope 3", "Scope 1", "Scope 1| Scope 3", "Scope 2", "Scope 2| Scope 3"
  print(paste0("CHECK: ", nrow(d)))
  d_separate_s12 <- filter(d, scope%in%c("Scope 1", "Scope 2") & !scope%in%c("Scope 1| Scope 2", "Scope 1| Scope 2| Scope 3"))
  d_combined_s12 <- filter(d, !scope%in%c("Scope 1", "Scope 2"))
  print(paste0("CHECK: ", nrow(d_separate_s12), ", ", nrow(d_combined_s12), " = ", nrow(d_separate_s12)+nrow(d_combined_s12)))
  
  return(list(d_separate_s12, d_combined_s12))
}

CalculateMaxSeparateScope12 <- function(d)
{ # determine scope 1+2 emissions for each company based on separate scope 1 and scope 2 target BY/emissions
  # 1. first determine max for scope 1 and scope 2 emissions separately
  # check scopes
  print("++++++++++++++")
  print(unique(d$scope))
  print(paste0("Number of rows before: ", nrow(d)))
  d <- group_by(d, account_id, scope) %>%
       summarise(emissions_base_year=max(emissions_base_year, na.rm=T),
                 emissions_reporting_year_interpolated_scope12=max(emissions_reporting_year_interpolated_scope12, na.rm=T),
                 emissions_target_year=max(emissions_target_year, na.rm=T))
  print(paste0("Number of rows after: ", nrow(d)))
  check_count <- group_by(d, account_id) %>% summarise(count=n())
  print(paste0("Min counts: ", min(check_count$count)))
  print(paste0("Max counts: ", max(check_count$count)))
  print("++++++++++++++")
  # sum scope 1 and scope 2 emissions
  d <- group_by(d, account_id) %>%
       summarise(emissions_base_year=sum(emissions_base_year, na.rm=T),
                 emissions_reporting_year_interpolated_scope12=max(emissions_reporting_year_interpolated_scope12, na.rm=T),
                 emissions_target_year=sum(emissions_target_year, na.rm=T))
  return(d)
  
}