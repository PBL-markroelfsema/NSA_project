StatCDP <- function(data, label="absolute_2020")
{ cat(paste0("# rows: ", nrow(data), "\n"))
  # STATISTICS
  f=paste0("Aggregation_scripts/data/", label, ".txt") 
  if(file.exists(f))
  { file.remove(f)
  }
  file.create(f)
  tmp<-length(data$account_id)
  cat(paste0("# records: ",tmp), sep="\n", file=f, append=TRUE)
  tmp<-length(unique(data$account_id))
  cat(paste0("# companies: ",tmp), sep="\n", file=f, append=TRUE)
  data <- mutate(data, company_branch = paste0(company_name, "-", `Country/Region`))
  tmp<-length(unique(data$company_branch))
  cat(paste0("# company branches: ", tmp), sep="\n", file=f, append=TRUE)
  tmp<-nrow(filter(data, `Target year`<2020))
  cat(paste0("# target year <2020: ", tmp), sep="\n", file=f, append=TRUE)

  # STATISTICS: number of company branches
  tmp <- select(data, company_name, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise(count_companies = n_distinct(company_name))
  write.table(tmp, paste0("Aggregation_scripts/data/stat_", label, "_1.csv"), sep=";", row.names=FALSE)

  # STATISTICS: number of company branches and target year combinations --> identify targets with same target year
  tmp <- select(data, company_name, `Country/Region`, `Target year`, `Scope(s) (or Scope 3 category)`) %>% group_by(company_name, `Country/Region`, `Scope(s) (or Scope 3 category)`) %>% summarise(count_TY = n_distinct(`Target year`))
  write.table(tmp, paste0("Aggregation_scripts/data/stat_", label, "_2.csv"), sep=";", row.names=FALSE)

  # STATISTIC: number of occurences per scope
  data <- mutate(data, Scope_short = str_split(T_ID, "-") %>% map_chr(., 3))
  tmp <- select(data,Scope_short) %>% group_by(Scope_short) %>% summarise(count_scopes = n())
  write.table(tmp, paste0("Aggregation_scripts/data/stat_", label, "_3.csv"), sep=";", row.names=FALSE)
  
  # Calculate sum(percent_alloc) for each company/country/target year/base year combination
  tmp <- select(data, company_name, `Country/Region`, Scope_short, `Target year`, `Base year`, percent_alloc) %>% group_by(company_name, Scope_short, `Target year`, `Base year`) %>% 
         summarise(count_branches = n(), sum_percent_alloc = sum(percent_alloc, na.rm=TRUE), remaing = sum_percent_alloc %% 1, perc_from_closest_number=abs(round(sum_percent_alloc)/sum_percent_alloc-1))
  write.table(tmp, paste0("Aggregation_scripts/data/stat_", label, "_4.csv"), sep=";", row.names=FALSE)
  write.table(filter(tmp, remaing==0 & sum_percent_alloc>1), paste0("Aggregation_scripts/data/duplicates_", label, ".csv"), sep=";", row.names=FALSE)
  write.table(filter(tmp, remaing!=0, perc_from_closest_number>0.1), paste0("Aggregation_scripts/data/percent_alloc_", label, ".csv"), sep=";", row.names=FALSE)
  
  # Check companies with only one branch/country, but percent_alloc is NA
  tmp <- group_by(data, company_name) %>% filter(n()==1) %>% select(company_name, `Country/Region`, percent_alloc) %>% filter(is.na(percent_alloc))
  write.table(tmp, paste0("Aggregation_scripts/data/percent_alloc_one_branch_na_", label, ".csv"), sep=";", row.names=FALSE)
  }