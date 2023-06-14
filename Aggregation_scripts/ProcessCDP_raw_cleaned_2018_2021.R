library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

source("ReadCDP_raw_cleaned_data_2018_2021.R")

# plot histogram of target coverage
d_TargetCoverage <- select(CDP_data_cleanded_target_2021, account_id, Scope, `Target coverage`) %>%
                    mutate(`Target coverage`=ifelse(substr(`Target coverage`, 1, 5)=="Other", "Other", `Target coverage`))
d_TargetCoverage_count <- group_by(d_TargetCoverage, `Target coverage`) %>%
                          summarise(count=n()) %>%
                          mutate(perc=100*count/sum(count))
# plot histogram annual ambitions 
g_TargetCoverage <- ggplot(data=d_TargetCoverage) +
  geom_bar(aes(x=`Target coverage`), stat="count") +
  geom_text(data=d_TargetCoverage_count, aes(x=`Target coverage`, y=count+100, label=paste0(round(perc,1), "%")), colour="darkgreen") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Target Coverage")
plot(g_TargetCoverage)

# Select companies with Dutch headquarters
Scope3_emissions_Ducht_Corporates <- left_join(CDP_data_raw_2021_summary, CDP_data_raw_inventory_2021_MRY_S3_categories, by=c('Account number', 'Organization', 'Country', 'Primary sector', 'Primary industry')) %>%
                                     rename(scope3_emisssions=`Scope 3 emissions, disclosing and explaining any exclusions. - Metric tonnes CO2e`, Scope3Category=RowName) %>%
                                     filter(Country=='Netherlands', scope3_emisssions!=0)
Scope3_emissions_Ducht_Corporates_count_companies <- select(Scope3_emissions_Ducht_Corporates, `Account number`) %>% n_distinct()
Scope3_emissions_Ducht_Corporates_count_categories <- group_by(Scope3_emissions_Ducht_Corporates, Scope3Category) %>% 
                                                      summarise( count=n(), scope3_emisssions=sum(scope3_emisssions)) %>%
                                                      arrange(desc(count))

scope3_dutch_hist <- ggplot(data=Scope3_emissions_Ducht_Corporates) +
                     geom_bar(aes(x=Scope3Category),stat="count") +
                     theme_bw() +
                     theme(axis.text.x = element_text(angle = 90))
plot(scope3_dutch_hist)
