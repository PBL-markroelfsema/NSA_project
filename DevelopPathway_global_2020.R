library(stringr)
library(tidyverse)
library(plotly)

source('SpreadCDPtarget_global_TargetYears_v2.R')

Exclude_Scope3 = TRUE

Scopes_unique <- unique(absolute_global_2020_targets_per_row4$Scope)
Scopes_short_unique <- unique(absolute_global_2020_targets_per_row4$Scope_short)
Scopes_select <- c('Scope 1+2', 'Scope 1+2+3', 'Scope 1', 'Scope 1+3', 'Scope 2', 'Scope 2+3')

absolute_global_2020_targets_per_row4$Scope <- as.character(absolute_global_2020_targets_per_row4$Scope)
if (Exclude_Scope3)
{ tst0 <- ungroup(absolute_global_2020_targets_per_row4) %>%
          mutate(Scope_tmp = ifelse(Scope=="Scope 1+2+3", "Scope 1+2",
                                                                    ifelse(Scope=="Scope 1+3", "Scope 1",
                                                                    ifelse(Scope=="Scope 2+3", "Scope 2", Scope)))) %>%
          #select(-Scope) %>%
          rename(Scope_old=Scope, Scope=Scope_tmp) %>%
          select(account_id, company_name, starts_with("primary"), target_type, Scope, Scope_old, everything())
 
} else
{ tst0 <- absolute_global_2020_targets_per_row4
}
# add scopes to columns
tst1 <- ungroup(tst0) %>%
        mutate(tmp_Scope=TRUE) %>%
        spread(key=Scope, value=tmp_Scope) %>%
        #select(account_id, `Scope 1`, `Scope 1+3`, `Scope 1+2`, `Scope 1+2+3`, `Scope 2`)
        select(account_id, starts_with("Scope"))
# make one row for each company, step 1: calculate number of targets
tst2 <- group_by(tst1, account_id) %>%
        #summarise(nr_targets=sum(`Scope 1`, `Scope 1+3`, `Scope 1+2`, `Scope 1+2+3`, `Scope 2`, na.rm=TRUE))
        mutate(nr_targets = sum(c_across(where(is.logical)), na.rm = T))
tst3 <- left_join(tst1, tst2, by=(c("account_id")))
tst3 <- tst3 %>% replace(is.na(.), FALSE)
tst4 <- group_by(tst3, account_id) %>%
        summarise(`Scope 1` = max(`Scope 1`),
                  `Scope 1+3` = max(`Scope 1+3`),
                  `Scope 1+2` = max(`Scope 1+2`),
                  `Scope 1+2+3` = max(`Scope 1+2+3`),
                  `Scope 2` = max(`Scope 2`))
tst4$`Scope 1` <- as.logical(tst4$`Scope 1`)
tst4$`Scope 1+3` <- as.logical(tst4$`Scope 1+3`)
tst4$`Scope 1+2` <- as.logical(tst4$`Scope 1+2`)
tst4$`Scope 1+2+3` <- as.logical(tst4$`Scope 1+2+3`)
tst4$`Scope 2` <- as.logical(tst4$`Scope 2`)
tst5<- select(tst4, -account_id) %>%
       distinct()
tst6<-mutate(tst5, x1=ifelse(`Scope 1`==TRUE, "Scope 1/", ""),
                   x2=ifelse(`Scope 1+3`==TRUE, "Scope 1+3/", ""),
                   x3=ifelse(`Scope 1+2`==TRUE, "Scope 1+2/", ""),
                   x4=ifelse(`Scope 1+2+3`==TRUE, "Scope 1+2+3/", ""),
                   x5=ifelse(`Scope 2`==TRUE, "Scope 2/", ""),
                   tmp=paste0(x1, x2, x3, x4, x5), 
                   Scope=substr(tmp, 1, nchar(tmp)-1)) %>% 
      select(-x1, -x2, -x3, -x4, -x5, -tmp)
