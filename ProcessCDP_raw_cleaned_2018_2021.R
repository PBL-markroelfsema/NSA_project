library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

source("ReadCDP_raw_cleaned_data_2018_2021.R")

# polot histogram of target coverage
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
