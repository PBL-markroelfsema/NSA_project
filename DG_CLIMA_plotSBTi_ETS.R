library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

scenarios = c("History", "CPS", "CPS+NSA")
scenario_names = c("History", "ETS/ESR", "ETS/ESR + SBTi")

results_SBTi_ETS <- read.table('data/results_SBTi_ETS.csv', sep=";", header=TRUE)
results_SBTi_ETS <- gather(results_SBTi_ETS, `scope.1`, `scope.2`, total, key="emissions_type", value=value)
results_SBTi_ETS$Scenario <- factor(results_SBTi_ETS$Scenario, levels=scenarios)

g1 <- ggplot(data=results_SBTi_ETS) +
  geom_bar(aes(x=Scenario, y=value, fill=`Policy.instrument`), stat='identity') +
  geom_text(aes(x="CPS", y=1250, label="regulation", angle=45)) +
  geom_text(aes(x="CPS+NSA", y=1250, label="regulationR+SBTi", angle=44, hjust=0.65)) +
  scale_fill_brewer(name="Policy instrument", palette="Set1") +
  scale_x_discrete(labels=c("History"="2019", "CPS"="2030","CPS+NSA"="2030")) +
  labs(y="MtCO2eq") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size=10))
plot(g1)

g2 <- ggplot(data=results_SBTi_ETS) +
  geom_bar(aes(x=Scenario, y=value, fill=`Company.type`), stat='identity') +
  geom_text(aes(x="CPS", y=1250, label="regulation", angle=45)) +
  geom_text(aes(x="CPS+NSA", y=1250, label="regulation+SBTi", angle=45, hjust=0.65)) +
  scale_fill_brewer(name="Company type", palette="Set2") +
  scale_x_discrete(labels=c("History"="2019", "CPS"="2030","CPS+NSA"="2030")) +
  labs(y="") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size=10))
plot(g2)

g <- g1+g2+ plot_layout(guides="collect") 
plot(g)
ggsave(paste0("graphs/PBL CLIMA/figure_EU_SBTi_ETS.jpg"), g, scale=1)
