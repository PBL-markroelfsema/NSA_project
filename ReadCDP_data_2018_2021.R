library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

options(scipen = 999, digits=2) 

CDP_data_raw_2018 <- read_excel("data/CDP/input/CDP_2018_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2018) <- str_replace_all(names(CDP_data_raw_2018), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2018) <- str_replace_all(names(CDP_data_raw_2018), "C4\\.1a_C", "")
names(CDP_data_raw_2018) <- str_replace_all(names(CDP_data_raw_2018), "^[0123456789]", "")
names(CDP_data_raw_2018) <- str_replace_all(names(CDP_data_raw_2018), "^[0123456789]", "")
names(CDP_data_raw_2018) <- str_replace_all(names(CDP_data_raw_2018), "_", "")
names(CDP_data_raw_2018) <- trimws(names(CDP_data_raw_2018))


CDP_data_raw_2019 <- read_excel("data/CDP/input/CDP_2019_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2019) <- str_replace_all(names(CDP_data_raw_2019), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2019) <- str_replace_all(names(CDP_data_raw_2019), "C4\\.1a_C", "")
names(CDP_data_raw_2019) <- str_replace_all(names(CDP_data_raw_2019), "^[0123456789]", "")
names(CDP_data_raw_2019) <- str_replace_all(names(CDP_data_raw_2019), "^[0123456789]", "")
names(CDP_data_raw_2019) <- str_replace_all(names(CDP_data_raw_2019), "_", "")
names(CDP_data_raw_2019) <- trimws(names(CDP_data_raw_2019))


CDP_data_raw_2020 <- read_excel("data/CDP/input/CDP_2020_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2020) <- str_replace_all(names(CDP_data_raw_2020), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2020) <- str_replace_all(names(CDP_data_raw_2020), "C4\\.1a_C", "")
names(CDP_data_raw_2020) <- str_replace_all(names(CDP_data_raw_2020), "^[0123456789]", "")
names(CDP_data_raw_2020) <- str_replace_all(names(CDP_data_raw_2020), "^[0123456789]", "")
names(CDP_data_raw_2020) <- str_replace_all(names(CDP_data_raw_2020), "_", "")
names(CDP_data_raw_2020) <- trimws(names(CDP_data_raw_2020))

CDP_data_raw_2021 <- read_excel("data/CDP/input/CDP_2021_Global_Aggregation_raw_response.xlsx", sheet="C4.1a")
names(CDP_data_raw_2021) <- str_replace_all(names(CDP_data_raw_2021), "Provide details of your absolute emissions target\\(s\\) and progress made against those targets. \\-", "")
names(CDP_data_raw_2021) <- str_replace_all(names(CDP_data_raw_2021), "C4\\.1a_C", "")
names(CDP_data_raw_2021) <- str_replace_all(names(CDP_data_raw_2021), "^[0123456789]", "")
names(CDP_data_raw_2021) <- str_replace_all(names(CDP_data_raw_2021), "^[0123456789]", "")
names(CDP_data_raw_2021) <- str_replace_all(names(CDP_data_raw_2021), "_", "")
names(CDP_data_raw_2021) <- trimws(names(CDP_data_raw_2021))

CDP_data_info <- read_excel("data/CDP/Process CDP data.xlsx", sheet="R-C4.1a")
col_2018 <- pull(CDP_data_info, `2018`)
col_2019 <- pull(CDP_data_info, `2019`)
col_2020 <- pull(CDP_data_info, `2020`)
col_2021 <- pull(CDP_data_info, `2021`)
col_names <- c("dataset_year", col_2021)

CDP_data_raw_2018_selection <- mutate(CDP_data_raw_2018, dataset_year=2018) %>% select("dataset_year", col_2018)
colnames(CDP_data_raw_2018_selection) <- col_names
CDP_data_raw_2019_selection <- mutate(CDP_data_raw_2019, dataset_year=2019) %>% select("dataset_year", col_2019)
colnames(CDP_data_raw_2019_selection) <- col_names
CDP_data_raw_2020_selection <- mutate(CDP_data_raw_2020, dataset_year=2020) %>% select("dataset_year", col_2020)
colnames(CDP_data_raw_2020_selection) <- col_names
CDP_data_raw_2021_selection <- mutate(CDP_data_raw_2021, dataset_year=2021) %>% select("dataset_year", col_2021)
colnames(CDP_data_raw_2021_selection) <- col_names

remove_TY <- c("Question not applicable")
CDP_data_raw_selection <- rbind(CDP_data_raw_2018_selection, CDP_data_raw_2019_selection) %>% rbind(CDP_data_raw_2020_selection) %>% rbind(CDP_data_raw_2021_selection)
CDP_data_raw_selection$dataset_year <- as.factor(CDP_data_raw_selection$dataset_year)
CDP_data_raw_selection <-  filter(CDP_data_raw_selection, !`Target year`%in%remove_TY & !is.na(`Target year`))
CDP_data_raw_selection$`Target year` <- as.integer(CDP_data_raw_selection$`Target year`)
CDP_data_raw_selection <-  filter(CDP_data_raw_selection, `% of target achieved [auto-calculated]`!=0 & !is.na(`% of target achieved [auto-calculated]`))
CDP_data_raw_selection$`% of target achieved [auto-calculated]` <- as.numeric(CDP_data_raw_selection$`% of target achieved [auto-calculated]`)
write.table(CDP_data_raw_selection, file='data/CDP/output/selection_from_year.csv', sep=";", row.names=FALSE)

data_progress_1 <- select(CDP_data_raw_selection, dataset_year, `Account number`, `Target year`, `% of target achieved [auto-calculated]`)
ggplot(data=filter(data_progress_1, `Target year`> 2018, `Target year`<= 2050,`% of target achieved [auto-calculated]`>-100, `% of target achieved [auto-calculated]`<100),
       aes(x=`Target year`, y=`% of target achieved [auto-calculated]`, colour=dataset_year)) +
  geom_point() +
  geom_smooth(method = "lm",  formula = y ~ x, size=2) +
  theme_bw()

data_progress_2 <- select(CDP_data_raw_selection, dataset_year, `Account number`, `Target year`, `% of target achieved [auto-calculated]`) %>%
                   filter(`% of target achieved [auto-calculated]`>-150, `% of target achieved [auto-calculated]`<150) %>%
  group_by(dataset_year, `Target year`) %>%
  summarise(mean_target_achieved=mean(`% of target achieved [auto-calculated]`, na.rm=TRUE),
            count_value=n())
fig_1a <- ggplot(data=filter(data_progress_2, dataset_year==2021, `Target year`> 2018, `Target year`<= 2050, mean_target_achieved>-100, mean_target_achieved<100),
       aes(x=`Target year`, y=mean_target_achieved, colour=dataset_year, size=count_value)) +
  geom_point(show.legend = FALSE) +     
  geom_smooth(method = "lm",  formula = y ~ x, size=2, show.legend = FALSE) +
  theme_bw()
fig_1b <- ggplot(data=filter(data_progress_2, `Target year`> 2018, `Target year`<= 2050, mean_target_achieved>-100, mean_target_achieved<100),
                 aes(x=`Target year`, y=mean_target_achieved, colour=dataset_year, size=count_value)) +
  geom_point() +     
  geom_smooth(method = "lm",  formula = y ~ x, size=2) +
  theme_bw()
fig_1 <- fig_1a + fig_1b
plot(fig_1)

data_progress_3a <- select(CDP_data_raw_selection, dataset_year, `Account number`, `Target year`, `% of target achieved [auto-calculated]`) %>%
  pivot_wider(names_from=dataset_year,
              values_from=c(`Target year`, `% of target achieved [auto-calculated]`),
              values_fill=NA)

CDP_data_raw_selection_dataset2021_year2050 <- filter(CDP_data_raw_selection, dataset_year==2021, `Target year`==2050,
                                                      `% of target achieved [auto-calculated]`>-150, `% of target achieved [auto-calculated]`<150) %>%
                                               rename(Perc_achieved=`% of target achieved [auto-calculated]`)
CDP_data_raw_selection_dataset2021_year2050$Perc_achieved<-as.double(CDP_data_raw_selection_dataset2021_year2050$Perc_achieved)
ggplot(data=CDP_data_raw_selection_dataset2021_year2050) +
  geom_histogram(aes(x=Perc_achieved), binwidth = 10)


# COMPARE Raw response data and cleaned dataset
CDP_data_clean_2020 <- read_excel("data/CDP/input/2020_CDP_Country_Specific_Dataset_for_NSA_Report_v2_adjusted.xlsx", sheet="Absolute ER")

# aggreagate to company (remove country data)
CDP_data_clean_2020_aggregation <- filter(CDP_data_clean_2020, `Target year`>=2020) %>%
                                   group_by(account_id, company_name, incorporation_country, `Target year`, `Target reference number`, `Scope(s) (or Scope 3 category)`) %>%
                                   summarise(MRY_EM=sum(`Covered emissions in reporting year (metric tons CO2e)`)) %>% 
                                   filter(!substr(`Scope(s) (or Scope 3 category)`,1,7) == "Scope 3")

tmp1 <- filter(CDP_data_clean_2020_aggregation, `Scope(s) (or Scope 3 category)` %in% c('Scope 1', 'Scope 2 (location-based)', 'Scope 2 (market-based)')) %>%
        mutate(MRY_EM=ifelse(is.na(MRY_EM), 0, MRY_EM)) %>%     
        spread(key=`Scope(s) (or Scope 3 category)`, value=MRY_EM) %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(MRY_EM=`Scope 1`+ max(`Scope 2 (location-based)`, `Scope 2 (market-based)`)) %>%
        select(-`Scope 1`, -`Scope 2 (location-based)`, -`Scope 2 (market-based)`) %>%
        group_by(account_id, company_name, incorporation_country) %>%
        summarise(MRY_EM=max(MRY_EM)) %>%
        mutate(`Scope(s) (or Scope 3 category)`="Scope 1+2 [auto-calculated]") %>%
        as.data.frame()
tmp2 <- ungroup(CDP_data_clean_2020_aggregation) %>%
        filter(!(`Scope(s) (or Scope 3 category)` %in% c('Scope 1', 'Scope 2 (location-based)', 'Scope 2 (market-based)'))) %>%
        select(account_id, company_name, incorporation_country, MRY_EM, `Scope(s) (or Scope 3 category)`) %>%
        as.data.frame()
 
CDP_data_clean_2020_aggregation <- rbind(tmp1, tmp2)
 # WHAT TO DO WITH OTHERS in scope?
