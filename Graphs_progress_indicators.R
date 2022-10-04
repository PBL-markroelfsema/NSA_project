library(stringr)
library(readxl)
library(tidyverse)
library(data.table)
library(patchwork)

choice_data_source = "response" # response or processed
choice_data_source_year = 2021

# read in data (response or processed)
# RESPONSE DATA
data_response_original <- NA
data_response <- NA
if (choice_data_source_year == 2021)
{ data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2021_abs_ER_public.xlsx', sheet = "Absolute ER")
data_response <- select(data_response_original, account_id, `Target reference number`,
                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year`,
                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, accounting_year,
                        `Targeted reduction from base year (%)`, `Target year`) %>%
  mutate(source_year = choice_data_source_year)
} else if (choice_data_source_year == 2020) 
{      data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2020_abs_ER_public.xlsx', sheet = "Absolute ER")
data_response <- select(data_response_original, account_id, `Target reference number`,
                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year`,
                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Most recent accounting year`,
                        `Targeted reduction from base year (%)`, `Target year`) %>%
  mutate(source_year = choice_data_source_year) %>%
  rename(accounting_year=`Most recent accounting year`)
} else if (choice_data_source_year == 2018) 
{      data_response_original <- read_excel('data/CDP/input/CDP_CCTD_2018_abs_ER_public.xlsx', sheet = "absolute")
data_response <- select(data_response_original, account_id, `Target reference number`,
                        `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year`,
                        `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `Most recent accounting year`,
                        `% reduction from base year`, `Target year`) %>%
                 mutate(source_year = choice_data_source_year) %>%
                 rename(accounting_year=`Most recent accounting year`, 
                        `Targeted reduction from base year (%)`=`% reduction from base year`)
}
nrow(data_response)

# PROCESSED DATA (CAAT)
data_CAAT <- read.table('data/CDP/output/data_CAAT.csv', sep=";", header = TRUE)
nrow(data_CAAT)
data_processed <- mutate(data_CAAT, selection=ifelse(is.na(`Target.Year.2`), 1, 2),
                             `Target year`=ifelse(selection==1, `Target.Year.1`, `Target.Year.2`),
                             `Targeted reduction from base year (%)`=ifelse(selection==1, `Target.Year.1.Scope.1.emissions.reduction..perc.`, `Target.Year.2.Scope.1.emissions.reduction..perc.`))
data_processed <- mutate(data_processed, account_id=`Actor.name`,
                                                 `Base year emissions (100% of scope)`=NA, 
                                                 `Base year emissions (100% of scope, excl. scope 3)`=`BY.Scope.1.emissions..tCO2e.`+`BY.Scope.2.emissions..tCO2e.`,
                                                 `Base year`=`Base.Year`,
                                                 `MRY emissions (100% of scope)`=NA, 
                                                 `MRY emissions (100% of scope, excl. scope 3)`=`MRY.Scope.1.emissions..tCO2e.`+`MRY.Scope.2.emissions..tCO2e.`, 
                                                 `accounting_year`=`MRY.Year`) %>%
                     select(account_id, 
                            `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`, `Base year`,
                            `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`, `accounting_year`,
                            `Targeted reduction from base year (%)`, `Target year`)
nrow(data_processed)


if (choice_data_source=="response") {data=data_response} else {data=data_response}

   
data_progress <-  mutate(data,
                         BY_EM=ifelse(is.na(`Base year emissions (100% of scope, excl. scope 3)`), `Base year emissions (100% of scope)`, `Base year emissions (100% of scope, excl. scope 3)`),
                         MRY_EM=ifelse(is.na(`MRY emissions (100% of scope, excl. scope 3)`), `MRY emissions (100% of scope)`, `MRY emissions (100% of scope, excl. scope 3)`)) %>%
                  rename(BY=`Base year`,
                         MRY=`accounting_year`,
                         TR=`Targeted reduction from base year (%)`, TY=`Target year`) %>%
                  filter(!is.na(BY_EM), !(BY_EM=="private_response"), 
                         !is.na(MRY_EM), MRY_EM>0, !(MRY_EM=="private_response"), 
                         !is.na(TR), 
                         !is.na(TY)) %>%
                  select(-`Base year emissions (100% of scope)`, -`Base year emissions (100% of scope, excl. scope 3)`,
                         -`MRY emissions (100% of scope)`, -`MRY emissions (100% of scope, excl. scope 3)`)
nrow(data_progress)
data_progress$BY_EM <- as.numeric(data_progress$BY_EM)
data_progress$MRY_EM <- as.numeric(data_progress$MRY_EM)

min(data_progress$TY, na.rm=TRUE)
max(data_progress$TY, na.rm=TRUE)
tst <- filter(data_progress, TY>2030)

# calculate progress rate and target completed
data_progress <- mutate(data_progress, TY_EM=(1-TR/100)*BY_EM,
                                       reduction_achieved=BY_EM-MRY_EM,
                                       Timelapsed=(MRY-BY)/(TY-BY),
                                       reduction_required=BY_EM*(TR/100)*Timelapsed,
                                       reduction_achieved=BY_EM-MRY_EM,
                                       on_track=ifelse(reduction_achieved>=reduction_required, TRUE, FALSE),
                                       target_year_group=ifelse(TY<=2020, 0, ifelse(TY<=2025, 1, ifelse(TY<2035, 2, 3))),
                                       target_completed=100*(TY_EM/MRY_EM-1),
                                       progress_rate=100*(reduction_achieved/reduction_required),
                                       maturity=ifelse(TY-BY>=0, TY-BY, NA),
                                       maturity_remaining=ifelse(TY-MRY>=0, TY-MRY, NA),
                                       annual_ambition=ifelse(maturity_remaining>=0 & BY_EM>0 & MRY_EM>0,-100*((TY_EM/MRY_EM)^(1/(MRY-BY))-1), NA),
                                       annual_progress=ifelse(maturity_remaining>=0 & BY_EM>0 & MRY_EM>0,-100*((MRY_EM/BY_EM)^(1/(MRY-BY))-1), NA),
                                       annual_ambition_a=ifelse(maturity_remaining>=0 & BY_EM>0 & MRY_EM>0,-100*((1/(TY-MRY))*((TY_EM-MRY_EM)/MRY_EM)), NA),
                                       annual_progress_a=ifelse(maturity_remaining>=0 & BY_EM>0 & MRY_EM>0,-100*((1/(MRY-BY))*((MRY_EM-BY_EM)/BY_EM)), NA)
                        )
data_progress$target_year_group <- factor(data_progress$target_year_group, levels=c('0', '1', '2', '3'))
nrow(data_progress)
write.table(data_progress, paste0('data/data_progress_', choice_data_source_year, '.csv'), sep=";")

TwoC_annual = 27/(2030-2019)
OneAndAHalfC_annual = 43/(2030-2019)
Potential_annual = 50/(2030-2019)

stats_ambition_progress <- ungroup(data_progress) %>%
                                                    summarise(number_of_targets = n(),
                                                    number_of_companies = n_distinct(account_id),
                                                    data_source_year=choice_data_source_year,
                                                    ambition_2C=100*sum(annual_ambition_a>TwoC_annual, na.rm=TRUE)/n(),
                                                    ambition_1_5C=100*sum(annual_ambition_a>OneAndAHalfC_annual, na.rm=TRUE)/n(),
                                                    on_track=100*sum(on_track==TRUE, na.rm=TRUE)/n())

#---------------------------------

# PLOT PROGRESS, AMBITION
d_ambition_progress=filter(data_progress, target_year_group%in%c('1', '2', '3'))
progress_TY_labels=c("2021-2025", "2026-2035", ">2035")
nrow(d_ambition_progress)

# 1. PLOT Maturity against annual progress
g_maturity_progress <- ggplot(data=filter(d_ambition_progress, maturity_remaining>0, maturity_remaining<40, annual_progress_a>-100, annual_progress_a<100),
                              aes(x=maturity_remaining, y=annual_progress_a)) +
  geom_point(aes(size=on_track), colour="grey20") +
  geom_point(aes(colour=target_year_group)) +
  geom_hline(yintercept=TwoC_annual, colour="black", linetype="dashed", size=1) +
  geom_hline(yintercept=OneAndAHalfC_annual, colour="black", linetype="dashed", size=1) +
  geom_hline(yintercept=Potential_annual, colour="black", linetype="dashed", size=1) +
  #geom_smooth(aes(x=maturity_remaining, y=annual_progress_a)) +
  scale_colour_discrete(name="Target year", labels=progress_TY_labels) +
  scale_size_manual(name="On track", values=c("TRUE"=5, "FALSE"=0.0)) +
  xlab("remaining maturity (years)") +
  ylab("annualised progress between base year and accounting_year (%)") +
  #xlim(0, 40) +
  #ylim(-100, 100) +
  theme_bw()
plot(g_maturity_progress)

# 2. PLOT Maturity against annual ambition
g_maturity_ambition <- ggplot(data=filter(d_ambition_progress, maturity_remaining>0, maturity_remaining<40, annual_ambition_a>-100, annual_ambition_a<100),
                              aes(x=maturity_remaining, y=annual_ambition_a)) +
  geom_point(aes(size=on_track), colour="grey20") +
  geom_point(aes(colour=target_year_group)) +
  geom_hline(yintercept=TwoC_annual, colour="black", linetype="dashed", size=1) +
  geom_hline(yintercept=OneAndAHalfC_annual, colour="black", linetype="dashed", size=1) +
  geom_hline(yintercept=Potential_annual, colour="black", linetype="dashed", size=1) +
  #geom_smooth() +
  scale_colour_discrete(name="Target year", labels=progress_TY_labels) +
  scale_size_manual(name="On track", values=c("TRUE"=5, "FALSE"=0.0)) +
  xlab("remaining maturity (years)") +
  ylab("annualised ambition between base year and target year (%)") +
  #xlim(0, 40) +
  #ylim(-30, 0) +
  theme_bw()
plot(g_maturity_ambition)

# 3. PLOT annual ambition against annual progress
TwoC_progress = data.frame(annual_ambition_a=TwoC_annual, annual_progress_a=TwoC_annual, temperature="2 °C")
OneAndAHalfC_progress = data.frame(annual_ambition_a=OneAndAHalfC_annual, annual_progress_a=OneAndAHalfC_annual, temperature="1.5 °C")
g_ambition_progress <- ggplot(data=filter(d_ambition_progress, annual_ambition_a>-100, annual_ambition_a<100, annual_progress_a>-100, annual_progress_a<100),
                              aes(x=annual_ambition_a, y=annual_progress_a)) +
  geom_point(aes(size=on_track), colour="grey20") +
  geom_point(aes(colour=target_year_group)) +
  geom_point(data=TwoC_progress, aes(x=annual_ambition_a, y=annual_progress_a, shape=temperature), colour="yellow", size=4) +
  geom_point(data=OneAndAHalfC_progress, aes(x=annual_ambition_a, y=annual_progress_a, shape=temperature), colour="yellow", size=4) +
  geom_vline(xintercept=TwoC_annual, colour="black", linetype="dashed", size=1) +
  geom_vline(xintercept=OneAndAHalfC_annual, colour="black", linetype="dashed", size=1) +
  geom_vline(xintercept=Potential_annual, colour="black", linetype="dashed", size=1) +
  #geom_smooth(aes()) +
  scale_colour_discrete(name="Target year", labels=progress_TY_labels) +
  scale_size_manual(name="On track", values=c("TRUE"=5, "FALSE"=0.0)) +
  scale_shape_manual(name="Paris goals (2019-2030)", values=c(15, 17)) +
  xlab("annualised ambition (%)") +
  ylab("annualised progress (%)") +
  xlim(0, 50) +
  #ylim(-100, 100) +
  theme_bw()
plot(g_ambition_progress)

layout <- "
AABB
CCC#
"


layout <- c(
  area(1,1,2,4),
  area(1,3,2,4),
  area(3,1,4,3)
)
g_total <- ((g_maturity_ambition + theme(legend.position = "none") + ggtitle("Ambition")) + 
           (g_maturity_progress + theme(legend.position = "none") +  ggtitle("Progress"))) /
           ((g_ambition_progress + ggtitle("Compare ambition and progress")))  +
           #plot_layout(design=layout) #+
           plot_annotation(paste0("Ambition and progress for source data year ", choice_data_source_year)) &
           ylim(-100, 100)
plot(g_total)
#jpeg(paste0("graphs/ambition_progress_", choice_data_source_year, ".jpg"))
#print(g_total)
#dev.off()
ggsave(paste0("graphs/ambition_progress_", choice_data_source_year, ".jpg"), g_total)

#---------------------------------
#TRY ALTERNATIVE PROGRESS AND AMBITION

# plot histogram annual ambitions
d_annual_ambition=filter(data_progress, annual_ambition_a>-100, annual_ambition_a<100)
g_annual_ambition <- ggplot(data=d_annual_ambition) +
  geom_histogram(aes(x=annual_ambition_a)) +
  theme_bw() +
  ggtitle("Ambition")
plot(g_annual_ambition)

# plot histogram annual progress
d_annual_progress=filter(data_progress, annual_progress_a>-100, annual_progress_a<100)
g_annual_progress <- ggplot(data=d_annual_progress) +
  geom_histogram(aes(x=annual_progress_a)) +
  theme_bw() +
  ggtitle("Progress")
plot(g_annual_progress)

g_ambition_progress = g_annual_ambition + g_annual_progress
plot(g_ambition_progress)
ggsave(paste0("graphs/hist_ambition_progress_", choice_data_source_year, ".jpg"), g_ambition_progress)

# plot histogram progress rate
g_progress_rate <- ggplot(data=data_progress) +
  geom_histogram(aes(x=progress_rate)) +
  theme_bw()
plot(g_progress_rate)

# plot histogram target_completed
g_target_completed <- ggplot(data=data_progress) +
  geom_histogram(aes(x=target_completed)) +
  theme_bw()
plot(g_target_completed)

# plot histogram progress rate
g_progress_rate_annual <- ggplot(data=data_progress) +
  geom_histogram(aes(x=annual_progress)) +
  theme_bw()
plot(g_progress_rate_annual)

# plot histogram target_completed
g_ambition_annual <- ggplot(data=data_progress) +
  geom_histogram(aes(x=annual_ambition)) +
  theme_bw()
plot(g_ambition_annual)

d_progress=filter(data_progress, target_year_group%in%c('0', '1', '2', '3'))
progress_TY_labels=c("<=2020", "2021-2025", "2026-2035", ">2035")
nrow(d_progress)
g_progress <- ggplot(data=d_progress) +
  geom_point(aes(x=target_completed, y=progress_rate, colour=target_year_group, shape=target_year_group)) +
  geom_smooth(aes(x=target_completed, y=progress_rate)) +
  scale_colour_discrete(name="Target year", labels=progress_TY_labels) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  xlim(-100, 25) +
  ylim(-400,400) +
  theme_bw()
plot(g_progress)

# CHANGE
d_progress_annual=filter(data_progress, target_year_group%in%c('1', '2', '3'))
progress_TY_labels=c("2021-2025", "2026-2035", ">2035")
nrow(d_progress_annual)
g_progress_annual <- ggplot(data=d_progress_annual) +
  geom_point(aes(x=annual_ambition, y=annual_progress, colour=target_year_group, shape=target_year_group)) +
  geom_smooth(aes(x=annual_ambition, y=annual_progress)) +
  scale_colour_discrete(name="Target year", labels=progress_TY_labels) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw()
plot(g_progress_annual)

#--------------------------------

# COMPARE TO LAST YEARS DATA
#data_progress_DDL <- read.csv('data/companies_progress_tracking_2021_DDL.csv', sep=",", header=TRUE, encoding = "UTF-8") 
data_progress_DDL <- fread('data/companies_progress_tracking_2021_DDL_semicolumn.csv', sep=";", header=TRUE) 
data_progress_DDL <- as.data.frame(data_progress_DDL) %>%
                     mutate(target_completed=gsub('\\%', '', `Target emission change rate vs 2019`), 
                            progress_rate=gsub('\\%', '', `Pro-rated achievement rate (BY-TY)`))
data_progress_DDL$target_completed <- as.numeric(data_progress_DDL$target_completed)
data_progress_DDL$progress_rate <- as.numeric(data_progress_DDL$progress_rate)
nrow(data_progress_DDL)
g_progress_DDL <- ggplot(data=data_progress_DDL) +
                  geom_point(aes(x=target_completed, y=progress_rate)) +
                  geom_smooth(aes(x=target_completed, y=progress_rate)) +
                  xlim(-100, 25) +
                  ylim(-400,400) +
                  theme_bw()
plot(g_progress_DDL)
