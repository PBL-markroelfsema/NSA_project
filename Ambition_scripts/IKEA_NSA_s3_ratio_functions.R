library(zeallot)

CleanColumnNames_s3_ratio_mry_s1 <- function(mry_s1_em)
{ names(mry_s1_em) <- str_replace_all(names(mry_s1_em), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.1\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
  names(mry_s1_em) <- str_replace_all(names(mry_s1_em), "C6\\.1_C", "")
  names(mry_s1_em) <- str_replace_all(names(mry_s1_em), "^[0123456789]", "")
  names(mry_s1_em) <- str_replace_all(names(mry_s1_em), "^[0123456789]", "")
  names(mry_s1_em) <- str_replace(names(mry_s1_em), "\\-\\.", "")
  names(mry_s1_em) <- str_replace(names(mry_s1_em), "_", "")
  names(mry_s1_em) <- str_replace_all(names(mry_s1_em), "\\.", " ")
  names(mry_s1_em) <- trimws(names(mry_s1_em))
  return(mry_s1_em) 
}

CleanColumnNames_s3_ratio_mry_s2 <- function(mry_s2_em)
{ names(mry_s2_em) <- str_replace_all(names(mry_s2_em), "What\\.were\\.your\\.organization\\’s\\.gross\\.global\\.Scope\\.2\\.emissions\\.in\\.metric\\.tons\\.CO2e\\?\\.-\\.", "")
  names(mry_s2_em) <- str_replace_all(names(mry_s2_em), "C6\\.3_C", "")
  names(mry_s2_em) <- str_replace_all(names(mry_s2_em), "^[0123456789]", "")
  names(mry_s2_em) <- str_replace_all(names(mry_s2_em), "^[0123456789]", "")
  names(mry_s2_em) <- str_replace(names(mry_s2_em), "\\-\\.", "")
  names(mry_s2_em) <- str_replace(names(mry_s2_em), "_", "")
  names(mry_s2_em) <- str_replace_all(names(mry_s2_em), "\\.", " ")
  names(mry_s2_em) <- trimws(names(mry_s2_em))
  return(mry_s2_em) 
}

CleanColumnNames_s3_ratio_mry_s3 <- function(mry_s3_em)
{ names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "Account\\.for\\.your\\.organization\\’s\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.-\\.", "")
  names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "Account\\.for\\.your\\.organization’s\\.gross\\.global\\.Scope\\.3\\.emissions,\\.disclosing\\.and\\.explaining\\.any\\.exclusions\\.\\.-\\.", "")
  names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "C6\\.5_C", "")
  names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "^[0123456789]", "")
  names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "^[0123456789]", "")
  names(mry_s3_em) <- str_replace(names(mry_s3_em), "\\-\\.", "")
  names(mry_s3_em) <- str_replace(names(mry_s3_em), "_", "")
  names(mry_s3_em) <- str_replace_all(names(mry_s3_em), "\\.", " ")
  names(mry_s3_em) <- trimws(names(mry_s3_em))
  return(mry_s3_em) 
}

PrepareS3Ratios_account_info <- function(acct_period, year)
{
  ##2018 account period info
  if (year == 2018)
  { acct_period_form <- acct_period %>%
    rename(account_id = `Account.number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
           acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
    mutate(acctprd_start_date=as.Date(acctprd_start_date),
           acctprd_end_date=as.Date(acctprd_end_date)) %>%
    select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
    filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
    group_by(account_id) %>%
    slice(which.max(acctprd_end_date))
  }

  ##2019 account period info
  if (year == 2019)
  { acct_period_form <- acct_period %>%
    rename(account_id = `Account.number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
           acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
    mutate(acctprd_start_date=as.Date(acctprd_start_date),
           acctprd_end_date=as.Date(acctprd_end_date)) %>%
    select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
    filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
    group_by(account_id) %>%
    slice(which.max(acctprd_end_date))
  }
  ##2020 account period info
  if (year == 2020)
  { acct_period_form <- acct_period %>%
    rename(account_id = `Account.number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
           acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
    mutate(acctprd_start_date=as.Date(acctprd_start_date),
           acctprd_end_date=as.Date(acctprd_end_date)) %>%
    select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
    filter((!is.na(acctprd_start_date) | !is.na(acctprd_end_date))) %>%
    group_by(account_id) %>%
    slice(which.max(acctprd_end_date))
  }
  ## 2021 account period info
  if (year == 2021)
  { acct_period_form <- acct_period %>%
    rename(account_id = `Account.number`,
           organization = `Organization`,
           country = `Country`,
           access = `Public`,
           row = `Row`,
           acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
           acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
    select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
    mutate(acctprd_start_date=as.Date(acctprd_start_date),
           acctprd_end_date=as.Date(acctprd_end_date))
  }
  ## 2022 account period info
  if (year == 2022)
  { acct_period_form <- acct_period %>%
    rename(account_id = `Account.number`,
           organization = `Organization`,
           country = `Country/Areas`,
           access = `Public`,
           row = `Row`,
           acctprd_start_date = `C0.2_C1_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.Start.date`,
           acctprd_end_date = `C0.2_C2_State.the.start.and.end.date.of.the.year.for.which.you.are.reporting.data..-.End.date`) %>%
    select(c(account_id, organization, country, access, row, acctprd_start_date, acctprd_end_date)) %>%
    mutate(acctprd_start_date=as.Date(acctprd_start_date),
           acctprd_end_date=as.Date(acctprd_end_date))
  }
  
  return(acct_period_form)
}

PrepareS3Ratios_by_emissions <- function(base_year_em, year)
{
##2018 Base year emissions data
  if (year == 2018)
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account.number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
           by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
           by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  }
  
  ##2019 Base year emissions data
  if (year == 2019)
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account.number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
           by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
           by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  }
  
##2020 Base year emissions data
  if (year == 2020)
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account.number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
           by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
           by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  }
  
  ##2021 Base year emissions data
  if (year == 2021)
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account.number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `C5.1_C1_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.start`,
           by_end_date = `C5.1_C2_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.end`,
           by_emissions = `C5.1_C3_Provide.your.base.year.and.base.year.emissions.(Scopes.1.and.2)..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  }

  ##2022 Base year emissions data
  if (year == 2022)
  { base_year_em_form <- base_year_em %>%
    rename(account_id = `Account.number`,
           access = `Public`,
           by_scope = `RowName`,
           by_start_date = `C5.2_C1_Provide.your.base.year.and.base.year.emissions..-.Base.year.start`,
           by_end_date = `C5.2_C2_Provide.your.base.year.and.base.year.emissions..-.Base.year.end`,
           by_emissions = `C5.2_C3_Provide.your.base.year.and.base.year.emissions..-.Base.year.emissions.(metric.tons.CO2e)`) %>%
    select(c(account_id, access, by_scope, by_start_date, by_end_date, by_emissions)) %>%
    filter(!(by_start_date == "Question not applicable")) %>%
    pivot_wider(names_from = by_scope, values_from = c(by_emissions, by_start_date, by_end_date), names_vary = "slowest")
  }
  
  return(base_year_em_form)
}

PrepareS3Ratios_mry_emissions <- function(mry_s1_em, mry_s2_em, mry_s3_em, year)
{ 
  if (year == 2018)
  { ##2018 most recent year scope 1 emissions data
    mry_s1_em_form <- mry_s1_em %>%
    rename(account_id = `Account number`,
           access = `Public`,
           row = `RowName`,
           mry_end_year = `End-year of reporting period`,
           mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
    filter(!(mry_emissions_s1 == "Question not applicable")) %>%
    filter(row=="Row 1") %>%
    select(c(account_id, mry_emissions_s1)) 

    ##2018 most recent year scope 2 emissions data
    mry_s2_em_form <- mry_s2_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_end_year = `End-year of reporting period`,
             mry_emissions_s2l = `Scope 2, location-based`,
             mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
      filter(row=="Row 1") %>%
      select(c(account_id, row,mry_emissions_s2l, mry_emissions_s2m)) %>%
      filter(!(mry_emissions_s2l == "Question not applicable") | !(mry_emissions_s2m == "Question not applicable"))


    ##2018 most recent year scope 3 emissions data
    mry_s3_em_form <- mry_s3_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_emissions_s3 = `Metric tonnes CO2e`) %>%
      select(c(account_id, row,mry_emissions_s3)) %>%
      mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
      pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
      rowwise() %>%
      mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                      `Capital goods`,
                                      `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                      `Upstream transportation and distribution`,
                                      `Waste generated in operations`,
                                      `Business travel`,
                                      `Employee commuting`,
                                      `Other (upstream)`,
                                      `Upstream leased assets`,na.rm = TRUE),
             mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                        `Processing of sold products`,
                                        `Use of sold products`,
                                        `End of life treatment of sold products`,
                                        `Downstream leased assets`,
                                        `Franchises`,
                                        `Investments`,
                                        `Other (downstream)`,na.rm=TRUE),
             mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
      select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
  } # if 2018
  if (year == 2019)
  { ##2019 most recent year scope 1 emissions data
    mry_s1_em_form <- mry_s1_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
      filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s1))
    
    ##2019 most recent year scope 2 emissions data
    mry_s2_em_form <- mry_s2_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s2l = `Scope 2, location-based`,
             mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
      filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))
    
    ##2019 most recent year scope 3 emissions data
    mry_s3_em_form <- mry_s3_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_emissions_s3 = `Metric tonnes CO2e`) %>%
      select(c(account_id, row,mry_emissions_s3)) %>%
      mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
      pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
      rowwise() %>%
      mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                      `Capital goods`,
                                      `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                      `Upstream transportation and distribution`,
                                      `Waste generated in operations`,
                                      `Business travel`,
                                      `Employee commuting`,
                                      `Other (upstream)`,
                                      `Upstream leased assets`,na.rm = TRUE),
             mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                        `Processing of sold products`,
                                        `Use of sold products`,
                                        `End of life treatment of sold products`,
                                        `Downstream leased assets`,
                                        `Franchises`,
                                        `Investments`,
                                        `Other (downstream)`,na.rm=TRUE),
             mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
      select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
  } # if 2019
  if (year == 2020)
  { ##2020 most recent year scope 1 emissions data
    mry_s1_em_form <- mry_s1_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
      filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s1))
    
    ##2020 most recent year scope 2 emissions data
    mry_s2_em_form <- mry_s2_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s2l = `Scope 2, location-based`,
             mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
      filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))
    
    ##2020 most recent year scope 3 emissions data
    mry_s3_em_form <- mry_s3_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_emissions_s3 = `Metric tonnes CO2e`) %>%
      select(c(account_id, row,mry_emissions_s3)) %>%
      mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
      pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
      rowwise() %>%
      mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                      `Capital goods`,
                                      `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                      `Upstream transportation and distribution`,
                                      `Waste generated in operations`,
                                      `Business travel`,
                                      `Employee commuting`,
                                      `Other (upstream)`,
                                      `Upstream leased assets`,na.rm = TRUE),
             mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                        `Processing of sold products`,
                                        `Use of sold products`,
                                        `End of life treatment of sold products`,
                                        `Downstream leased assets`,
                                        `Franchises`,
                                        `Investments`,
                                        `Other (downstream)`,na.rm=TRUE),
             mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
      select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
  } # if 2020
  
  if (year == 2021)
  { ##2021 most recent year scope 1 emissions data
    mry_s1_em_form <- mry_s1_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
      filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s1))
    
    ##2021 most recent year scope 2 emissions data
    mry_s2_em_form <- mry_s2_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s2l = `Scope 2, location-based`,
             mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
      filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))
    
    ##2021 most recent year scope 3 emissions data
    mry_s3_em_form <- mry_s3_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_emissions_s3 = `Metric tonnes CO2e`) %>%
      select(c(account_id, row,mry_emissions_s3)) %>%
      mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
      pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
      rowwise() %>%
      mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                      `Capital goods`,
                                      `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                      `Upstream transportation and distribution`,
                                      `Waste generated in operations`,
                                      `Business travel`,
                                      `Employee commuting`,
                                      `Other (upstream)`,
                                      `Upstream leased assets`,na.rm = TRUE),
             mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                        `Processing of sold products`,
                                        `Use of sold products`,
                                        `End of life treatment of sold products`,
                                        `Downstream leased assets`,
                                        `Franchises`,
                                        `Investments`,
                                        `Other (downstream)`,na.rm=TRUE),
             mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
      select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
  } # if 2021
  if (year == 2022)
  { ##2022 most recent year scope 1 emissions data
    mry_s1_em_form <- mry_s1_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s1 = `Gross global Scope 1 emissions (metric tons CO2e)`) %>%
      filter(mry_emissions_s1 != "Question not applicable",row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s1))
    
    ##2022 most recent year scope 2 emissions data
    mry_s2_em_form <- mry_s2_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_start_date = `Start date`,
             mry_end_date = `End date`,
             mry_emissions_s2l = `Scope 2, location-based`,
             mry_emissions_s2m = `Scope 2, market-based (if applicable)`) %>%
      filter((mry_emissions_s2l != "Question not applicable" | mry_emissions_s2m != "Question not applicable"),row=="Reporting year") %>%
      select(c(account_id, mry_emissions_s2l, mry_emissions_s2m))
    
    ##2022 most recent year scope 3 emissions data
    mry_s3_em_form <- mry_s3_em %>%
      rename(account_id = `Account number`,
             access = `Public`,
             row = `RowName`,
             mry_emissions_s3 = `Emissions in reporting year (metric tons CO2e)`) %>%
      select(c(account_id, row,mry_emissions_s3)) %>%
      mutate(mry_emissions_s3 = as.numeric(mry_emissions_s3)) %>%
      pivot_wider(names_from = row, values_from = mry_emissions_s3) %>%
      rowwise() %>%
      mutate(mry_emissions_s3up = sum(`Purchased goods and services`,
                                      `Capital goods`,
                                      `Fuel-and-energy-related activities (not included in Scope 1 or 2)`,
                                      `Upstream transportation and distribution`,
                                      `Waste generated in operations`,
                                      `Business travel`,
                                      `Employee commuting`,
                                      `Other (upstream)`,
                                      `Upstream leased assets`,na.rm = TRUE),
             mry_emissions_s3down = sum(`Downstream transportation and distribution`,
                                        `Processing of sold products`,
                                        `Use of sold products`,
                                        `End of life treatment of sold products`,
                                        `Downstream leased assets`,
                                        `Franchises`,
                                        `Investments`,
                                        `Other (downstream)`,na.rm=TRUE),
             mry_emissions_s3 = sum(mry_emissions_s3up,mry_emissions_s3down,na.rm=TRUE)) %>%
      select(c(account_id,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3))
  } # if 2022
  
  return(list(mry_s1_em_form, mry_s2_em_form, mry_s3_em_form))
}

PrepareS3Ratios_combine_by_mry_emissions <- function(acct_period_form, base_year_em_form, mry_s1_em_form, mry_s2_em_form, mry_s3_em_form, year)
{ # combine information that were collected from different sheets from the raw dataset
  if (year == 2018)
  { by_mry_final = base_year_em_form %>%
    rename(by_emissions_s1 = `by_emissions_Scope 1`,
           by_start_dt_s1 = `by_start_date_Scope 1`,
           by_end_dt_s1 = `by_end_date_Scope 1`,
           by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
           by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
           by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
           by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
           by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
           by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
    full_join(mry_s1_em_form) %>%
    full_join(mry_s2_em_form) %>%
    full_join(mry_s3_em_form) %>%
    full_join(acct_period_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
    rename(mry_start_dt = acctprd_start_date,
           mry_end_dt = acctprd_end_date) %>%
    select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
             by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
             by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
             mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
             mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
    rowwise() %>%
    mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
           mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
           mry_emissions_s1 = as.numeric(mry_emissions_s1),
           mry_emissions_s2l = as.numeric(mry_emissions_s2l),
           mry_emissions_s2m = as.numeric(mry_emissions_s2m),
           mry_emissions_s3up = as.numeric(mry_emissions_s3up),
           mry_emissions_s3down = as.numeric(mry_emissions_s3down),
           mry_emissions_s3 = as.numeric(mry_emissions_s3),
           s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
           s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
           perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3up)
           ),
         
           perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3down)
           ),
          
           perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3up)
           ), 
         
           perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3down)
           ), 
         
           perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                          ~ (s12l)/
                                            (s12l + mry_emissions_s3)
           ),
         
           perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                          ~ (s12m)/
                                            (s12m + mry_emissions_s3)
           )
    ) %>%
    select(-mry_emissions_s3,-s12l,-s12m)
  } # if 2018
  if (year == 2019)
  {by_mry_final = base_year_em_form %>%
    rename(by_emissions_s1 = `by_emissions_Scope 1`,
           by_start_dt_s1 = `by_start_date_Scope 1`,
           by_end_dt_s1 = `by_end_date_Scope 1`,
           by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
           by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
           by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
           by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
           by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
           by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
    full_join(mry_s1_em_form) %>%
    full_join(mry_s2_em_form) %>%
    full_join(mry_s3_em_form) %>%
    full_join(acct_period_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
    rename(mry_start_dt = acctprd_start_date,
           mry_end_dt = acctprd_end_date) %>%
    select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
             by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
             by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
             mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
             mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
    rowwise() %>%
    mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
           mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
           mry_emissions_s1 = as.numeric(mry_emissions_s1),
           mry_emissions_s2l = as.numeric(mry_emissions_s2l),
           mry_emissions_s2m = as.numeric(mry_emissions_s2m),
           mry_emissions_s3up = as.numeric(mry_emissions_s3up),
           mry_emissions_s3down = as.numeric(mry_emissions_s3down),
           mry_emissions_s3 = as.numeric(mry_emissions_s3),
           s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
           s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
           perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3up)
           ),
           
           perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3down)
           ),
           
           perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3up)
           ),
           
           perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3down)
           ),
           
           perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                          ~ (s12l)/
                                            (s12l + mry_emissions_s3)
           ),
           
           perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                          ~ (s12m)/
                                            (s12m + mry_emissions_s3)
           )
    ) %>%
    select(-mry_emissions_s3,-s12l,-s12m)
  } # if 2019
  if (year == 2020)
  { by_mry_final = base_year_em_form %>%
    rename(by_emissions_s1 = `by_emissions_Scope 1`,
           by_start_dt_s1 = `by_start_date_Scope 1`,
           by_end_dt_s1 = `by_end_date_Scope 1`,
           by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
           by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
           by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
           by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
           by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
           by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
    full_join(mry_s1_em_form) %>%
    full_join(mry_s2_em_form) %>%
    full_join(mry_s3_em_form) %>%
    full_join(acct_period_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
    rename(mry_start_dt = acctprd_start_date,
           mry_end_dt = acctprd_end_date) %>%
    select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
             by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
             by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
             mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
             mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
    rowwise() %>%
    mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
           mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
           mry_emissions_s1 = as.numeric(mry_emissions_s1),
           mry_emissions_s2l = as.numeric(mry_emissions_s2l),
           mry_emissions_s2m = as.numeric(mry_emissions_s2m),
           mry_emissions_s3up = as.numeric(mry_emissions_s3up),
           mry_emissions_s3down = as.numeric(mry_emissions_s3down),
           mry_emissions_s3 = as.numeric(mry_emissions_s3),
           s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
           s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
           perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3up)
           ),
             
           perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3down)
           ),
          
           perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3up)
          ),
            
           perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3down)
           ),
            
           perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                          ~ (s12l)/
                                            (s12l + mry_emissions_s3)
           ),
             
           perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                          ~ (s12m)/
                                            (s12m + mry_emissions_s3)
           )
    ) %>%
    select(-mry_emissions_s3,-s12l,-s12m)
   } # if 2020
   if (year == 2021)
   { by_mry_final = base_year_em_form %>%
    rename(by_emissions_s1 = `by_emissions_Scope 1`,
           by_start_dt_s1 = `by_start_date_Scope 1`,
           by_end_dt_s1 = `by_end_date_Scope 1`,
           by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
           by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
           by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
           by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
           by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
           by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
    full_join(mry_s1_em_form) %>%
    full_join(mry_s2_em_form) %>%
    full_join(mry_s3_em_form) %>%
    full_join(acct_period_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
    rename(mry_start_dt = acctprd_start_date,
           mry_end_dt = acctprd_end_date) %>%
    select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
             by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
             by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
             mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
             mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
    rowwise() %>%
    mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
           mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
           mry_emissions_s1 = as.numeric(mry_emissions_s1),
           mry_emissions_s2l = as.numeric(mry_emissions_s2l),
           mry_emissions_s2m = as.numeric(mry_emissions_s2m),
           mry_emissions_s3up = as.numeric(mry_emissions_s3up),
           mry_emissions_s3down = as.numeric(mry_emissions_s3down),
           mry_emissions_s3 = as.numeric(mry_emissions_s3),
           s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
           s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
           perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3up)
           ),
         
           perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3down)
           ), 
         
           perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3up)
           ),
         
           perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3down)
           ),
         
           perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                          ~ (s12l)/
                                            (s12l + mry_emissions_s3)
           ),
         
           perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                          ~ (s12m)/
                                            (s12m + mry_emissions_s3)
           )
    ) %>%
    select(-mry_emissions_s3,-s12l,-s12m)
   } # if 2021
  if (year == 2022)
  { by_mry_final = base_year_em_form %>%
    rename(by_emissions_s1 = `by_emissions_Scope 1`,
           by_start_dt_s1 = `by_start_date_Scope 1`,
           by_end_dt_s1 = `by_end_date_Scope 1`,
           by_emissions_s2l = `by_emissions_Scope 2 (location-based) `,
           by_start_dt_s2l = `by_start_date_Scope 2 (location-based) `,
           by_end_dt_s2l = `by_end_date_Scope 2 (location-based) `,
           by_emissions_s2m = `by_emissions_Scope 2 (market-based) `,
           by_start_dt_s2m = `by_start_date_Scope 2 (market-based) `,
           by_end_dt_s2m = `by_end_date_Scope 2 (market-based) `) %>%
    full_join(mry_s1_em_form) %>%
    full_join(mry_s2_em_form) %>%
    full_join(mry_s3_em_form) %>%
    full_join(acct_period_form[,c("account_id","organization","country","acctprd_start_date","acctprd_end_date")],by="account_id") %>%
    rename(mry_start_dt = acctprd_start_date,
           mry_end_dt = acctprd_end_date) %>%
    select(c(account_id,organization,country,access,by_emissions_s1,by_start_dt_s1,by_end_dt_s1,
             by_emissions_s2l,by_start_dt_s2l,by_end_dt_s2l,
             by_emissions_s2m,by_start_dt_s2m,by_end_dt_s2m,
             mry_start_dt,mry_end_dt,mry_emissions_s1,mry_emissions_s2l,
             mry_emissions_s2m,mry_emissions_s3up,mry_emissions_s3down,mry_emissions_s3)) %>%
    rowwise() %>%
    mutate(mry_emissions_s2l = case_when(mry_emissions_s2l != "Question not applicable" ~ mry_emissions_s2l),
           mry_emissions_s2m = case_when(mry_emissions_s2m != "Question not applicable" ~ mry_emissions_s2m),
           mry_emissions_s1 = as.numeric(mry_emissions_s1),
           mry_emissions_s2l = as.numeric(mry_emissions_s2l),
           mry_emissions_s2m = as.numeric(mry_emissions_s2m),
           mry_emissions_s3up = as.numeric(mry_emissions_s3up),
           mry_emissions_s3down = as.numeric(mry_emissions_s3down),
           mry_emissions_s3 = as.numeric(mry_emissions_s3),
           s12l = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2l) ~ sum(mry_emissions_s1,mry_emissions_s2l,na.rm=T)),
           s12m = case_when(!is.na(mry_emissions_s1) | !is.na(mry_emissions_s2m) ~ sum(mry_emissions_s1,mry_emissions_s2m,na.rm=T)),
           perc_S1S2L_S1S2LS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12l) & (s12l + mry_emissions_s3up) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3up)
           ),
           
           perc_S1S2L_S1S2LS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12l) & (s12l + mry_emissions_s3down) > 0
                                           ~ (s12l)/
                                             (s12l + mry_emissions_s3down)
           ), 
           
           perc_S1S2M_S1S2MS3U = case_when(!is.na(mry_emissions_s3up) & !is.na(s12m) & (s12m + mry_emissions_s3up) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3up)
           ),
           
           perc_S1S2M_S1S2MS3D = case_when(!is.na(mry_emissions_s3down) & !is.na(s12m) & (s12m + mry_emissions_s3down) > 0
                                           ~ (s12m)/
                                             (s12m + mry_emissions_s3down)
           ),
           
           perc_S1S2L_S1S2LS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12l) & (s12l + mry_emissions_s3) > 0
                                          ~ (s12l)/
                                            (s12l + mry_emissions_s3)
           ),
           
           perc_S1S2M_S1S2MS3 = case_when(!is.na(mry_emissions_s3) & !is.na(s12m) & (s12m + mry_emissions_s3) > 0
                                          ~ (s12m)/
                                            (s12m + mry_emissions_s3)
           )
    ) %>%
    select(-mry_emissions_s3,-s12l,-s12m)
  } # if 2022
  
   return(by_mry_final)
  
}