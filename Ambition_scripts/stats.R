# stats
abs_er_check <- abs_er
x1 <- filter(abs_er_check, `Target reference number`=='Question not applicable')
x2 <- filter(abs_er_check, `Target year`=='Question not applicable' | is.na(`Target year`))
if (YEAR%in%c(2018, 2019))
{ if (YEAR==2018) 
  { tss_tmp <- c(target_status_include, 'Expired')
} else {
    tss_tmp <- target_status_include
  }
  x3 <- filter(abs_er_check, !(`Target status`%in%tss_tmp))
} else {
  x3 <- filter(abs_er_check, !(`Target status in reporting year`%in%target_status_include))
}
if (YEAR%in%c(2018, 2019))
{ x4 <- filter(abs_er_check, grepl("^scope 3", Scope, ignore.case = T) | grepl("^other", Scope, ignore.case = T) | Scope=="Question not applicable" | is.na(Scope))
} else if (YEAR%in%c(2020, 2021))
{ x4 <- filter(abs_er_check, grepl("^scope 3", `Scope(s) (or Scope 3 category)`, ignore.case = T) | grepl("^other", `Scope(s) (or Scope 3 category)`, ignore.case = T) | `Scope(s) (or Scope 3 category)`=="Question not applicable" | is.na(`Scope(s) (or Scope 3 category)`))
} else
{ x4 <- filter(abs_er_check, grepl("^scope 3", `Scope(s)`, ignore.case = T) | grepl("^other", `Scope(s)`, ignore.case = T) | `Scope(s)`=="Question not applicable" | is.na(`Scope(s)`))
}
if (YEAR%in%c(2018, 2019))
{ abs_er_check$`% emissions in Scope` <- as.numeric(abs_er_check$`% emissions in Scope`)
  x5 <- filter(abs_er_check, `% emissions in Scope`<75)
} else if (YEAR%in%c(2020, 2021))
{ abs_er_check$`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)` <- as.numeric(abs_er_check$`Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`)
  x5 <- filter(abs_er_check, `Covered emissions in base year as % of total base year emissions in selected Scope(s) (or Scope 3 category)`<75)
} else
{ abs_er_check$`Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes` <- as.numeric(abs_er_check$`Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes`)
  x5 <- filter(abs_er_check, `Base year emissions covered by target in all selected Scopes as % of total base year emissions in all selected Scopes`<75)
}

cat("\n")
print("+++++++++++++++++++")
print(paste0("STATS ", YEAR))
print(paste0("target_id_empty: ", nrow(x1), " target_year_empty: ", nrow(x2), " target_status_not_used: ", nrow(x3), " scope3: ", nrow(x4), " by_perc_smaller_75: ", nrow(x5)))
cat("\n")
print("+++++++++++++++++++")