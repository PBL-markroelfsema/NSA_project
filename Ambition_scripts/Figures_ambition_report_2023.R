library(patchwork)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringdist)
library(fuzzyjoin)
library(openxlsx)
library(stringr)
library(data.table)
library(stringr)
library(ggplot2)
library(logr)
library(scatterpie)
library(zeallot)
library(scales)
library(ggrepel)
library(ggbreak)
library(ggtext)
library(readxl)

source('Ambition_scripts/IKEA_NSA_target_matching_2018_2023.R')

#------------------------------------------

source('Ambition_scripts/Figures_ambition_report_2023_functions.R')

data_dir = "Ambition_scripts/data/CDP_2023/"

primary_industries <- c("Manufacturing", "Services", "Materials", "Retail", "Infrastructure", "Food, beverage & agriculture", 
                        "Biotech, health care & pharma", "Transportation services", "Apparel", "Fossil Fuels", "Hospitality", "Power generation") 

###### APPENDIX: BY and MRY emissions per source year ####################
d_EM <- read.table(paste0(data_dir, "output/df_emissions.csv"), header=T, sep=";") 

treshold_BY_figure  = 300000000
treshold_MRY_figure = 300000000
threshold_TY_Figure = 300000000

text_size_outliers = 20
d1 <- d_EM
d_text_BY <- filter(d1, emissions_base_year > treshold_BY_figure)
d_text_MRY <- filter(d1, emissions_reporting_year > treshold_MRY_figure)
d_text_TY <- filter(d1, emissions_target_year > threshold_TY_Figure)
g1_check_emissions <- ggplot(data=d1) +
      geom_point(aes(x=disclosure_year, y=emissions_base_year, colour="Base year emissions")) +
      geom_point(aes(x=disclosure_year+0.33, y=emissions_reporting_year, colour="Reporting year emissions")) +
      geom_point(aes(x=disclosure_year+0.66, y=emissions_target_year, colour="Target year emissions")) +
      #geom_text(data=d_text_BY, aes(x=disclosure_year, y=emissions_base_year, label=account_id)) +
      #geom_text(data=d_text_MRY, aes(x=disclosure_year+0.33, y=emissions_reporting_year, label=account_id)) +
      #geom_text(data=d_text_TY, aes(x=disclosure_year+0.66, y=emissions_target_year, label=account_id)) +
      scale_x_continuous(breaks=seq(2018, 2022, 1)) +
      scale_y_continuous(name="GHG emissions", labels = comma_format(big.mark = ",", decimal.mark = ".")) +
      scale_colour_brewer(name="Emissions", palette="Set1") +
      theme_bw() +
      ggtitle("Not adjusted for outliers") +
      theme(axis.text=element_text(size=text_size_outliers),
        axis.title=element_text(size=text_size_outliers),
        legend.text=element_text(size=text_size_outliers),
        legend.title=element_text(size=text_size_outliers))
g1_check_emissions

d2 <- filter(d_EM, emissions_base_year<treshold_BY_figure & emissions_reporting_year<treshold_MRY_figure & emissions_target_year<threshold_TY_Figure)
g2_check_emissions <- ggplot(data=d2) +
  geom_point(aes(x=disclosure_year, y=emissions_base_year, colour="Base year emissions")) +
  geom_point(aes(x=disclosure_year+0.33, y=emissions_reporting_year, colour="Reporting year emissions")) +
  geom_point(aes(x=disclosure_year+0.66, y=emissions_target_year, colour="Target year emissions")) +
  #geom_text(data=subset(d2, emissions_base_year>treshold_BY_figure | emissions_reporting_year>treshold_MRY_figure), aes(disclosure_year,emissions_base_year,label=account_id)) +
  #geom_point(aes(x=disclosure_year+0.66, y=emissions_target_year, colour="Target year emissions")) +
  scale_x_continuous(breaks=seq(2018, 2022, 1)) +
  scale_y_continuous(name="", labels = comma_format(big.mark = ",", decimal.mark = ".")) +
  scale_colour_brewer(name="Emissions", palette="Set1") +
  theme_bw() +
  ggtitle("Adjusted for outliers") +
  theme(axis.text=element_text(size=text_size_outliers),
        axis.title=element_text(size=text_size_outliers),
        legend.text=element_text(size=text_size_outliers),
        legend.title=element_text(size=text_size_outliers))
g2_check_emissions

g_check_emissions <- g1_check_emissions + theme(legend.position="none") + g2_check_emissions
g_check_emissions
ggsave("Ambition_scripts/graphs/figure_check_emissions.png", g_check_emissions, dpi=300, units="cm", width=50, height=40)

###### APPENDIX: Annual reductions ####################

for (t in years)
{ annual_change_proccesed <- filter(abs_er_form_processed, account_id, emissions_target_year, emissions_base_year, target_year, base_year, targeted_reduction, scope_def_2022) %>%
                             mutate(annual_change = ifelse(targeted_reduction < 100, (emissions_target_year/emissions_base_year)^(1/(target_year-base_year)), "NZ"), disclosure_year = t)
}

check_annual_chagne <- filter(annual_change_proccesed, annual_change>50)
ggplot(data=annual_change_proccesed) +
  geom_point(aes(x=disclosure_year, y=annual_change, colour=scope_def_2022)) +
  theme_bw()

###### APPENDIX: Sensitivity analaysis ambtion pathways ####################

ambition_pathways_CPS <- read_xlsx(paste0(data_dir, "input/AmbitionPathways_PBL_IMAGE.xlsx"), sheet = "All Companies") #%>% mutate(extrapolation="CPS trends")
ambition_pathways_CPS <- ambition_pathways_CPS %>% slice(-1)
colnames(ambition_pathways_CPS)[1] = "year"
ambition_pathways_constant <- read_excel(paste0(data_dir, "input/AmbitionPathways_constant.xlsx"), sheet = "All Companies") #%>% mutate(extrapolation="constant")
ambition_pathways_constant <- ambition_pathways_constant %>% slice(-1)
colnames(ambition_pathways_constant)[1] = "year"
ambition_pathways_CPS <- as.data.frame(ambition_pathways_CPS)
ambition_pathways_constant <- as.data.frame(ambition_pathways_constant)
ambition_pathways_CPS <- gather(ambition_pathways_CPS, starts_with("2"), key="disclosure_year", value="value") %>%
                         mutate(extrapolation_method="CPS")
ambition_pathways_constant <- gather(ambition_pathways_constant, starts_with("2"), key="disclosure_year", value="value") %>%
                              mutate(extrapolation_method="constant")
ambition_pathways1 <- rbind(ambition_pathways_CPS, ambition_pathways_constant)
ambition_pathways2 <- inner_join(ambition_pathways_CPS, ambition_pathways_constant, by=c("year", "disclosure_year")) %>%
                      mutate(min=pmin(value.x, value.y), max=pmax(value.x, value.y)) %>%
                      select(-value.x, -value.y, -extrapolation_method.x, -extrapolation_method.y)
text_size_pathways=20
g_ambtion_pathways <- ggplot() +
                       geom_line(data=ambition_pathways1, aes(x=year, y=value, colour=disclosure_year, linetype=extrapolation_method)) + 
                       geom_ribbon(data=ambition_pathways2, aes(x=year,ymin=min,ymax=max,fill=disclosure_year),alpha=.15) +
                       scale_colour_brewer(name="Disclosure year", palette="Set1") +
                       scale_fill_brewer(name="Disclosure year", palette="Set1") +
                       scale_linetype_discrete(name="Extrapolation method", labels=c("Constant", "Current policies scenario trends")) +
                       scale_x_continuous(name="Year") +
                       scale_y_continuous(name="MtCO2e", limits=c(0, NA)) +
                       theme_bw() +
                       theme(axis.text=element_text(size=text_size_pathways),
                             axis.title=element_text(size=text_size_pathways),
                             legend.text=element_text(size=text_size_pathways),
                             legend.title=element_text(size=text_size_pathways))
g_ambtion_pathways
ggsave("Ambition_scripts/graphs/figure_ambtion_pathways_sensitivity.png", g_ambtion_pathways, dpi=300, units="cm", width=40, height=30)

###### Figure Country emissions breakdown ####################

term_order <- c("Short", "Mid", "Long")
emissions_country_2018 <- read.xlsx(paste0(data_dir, "input/2018_IKEA_NSA_landscape_breakdown_counts_updated.xlsx"), sheet="Country Count Target-Level") %>%
                          as.data.frame() %>%
                          mutate(disclosure_year=2018)
emissions_country_2019 <- read.xlsx(paste0(data_dir, "input/2019_IKEA_NSA_landscape_breakdown_counts_updated.xlsx"), sheet="Country Count Target-Level") %>%
                          as.data.frame() %>%
                          mutate(disclosure_year=2019)
emissions_country_2020 <- read.xlsx(paste0(data_dir, "input/2020_IKEA_NSA_landscape_breakdown_counts_updated.xlsx"), sheet="Country Count Target-Level") %>%
                          as.data.frame() %>%
                          mutate(disclosure_year=2020)
emissions_country_2021 <- read.xlsx(paste0(data_dir, "input/2021_IKEA_NSA_landscape_breakdown_counts_updated.xlsx"), sheet="Country Count Target-Level") %>%
                          as.data.frame() %>%
                          mutate(disclosure_year=2021)
emissions_country_2022 <- read.xlsx(paste0(data_dir, "input/2022_IKEA_NSA_landscape_breakdown_counts_updated.xlsx"), sheet="Country Count Target-Level") %>%
                          as.data.frame() %>%
                          mutate(disclosure_year=2022)
d_emissions_country <- rbind(emissions_country_2018, emissions_country_2019) %>% rbind(emissions_country_2020) %>% rbind(emissions_country_2021) %>% rbind(emissions_country_2022) %>%
                       select(disclosure_year, everything())
d_emissions_country <- mutate_all(d_emissions_country, funs(str_replace_all(., "United Kingdom of Great Britain and Northern Ireland", "UK")))
d_emissions_country <- mutate_all(d_emissions_country, funs(str_replace_all(., "United States of America", "USA")))
d_emissions_country$disclosure_year <- factor(d_emissions_country$disclosure_year)
d_emissions_country$n <- as.numeric(d_emissions_country$n)
d_emissions_country$term <- factor(d_emissions_country$term, levels=term_order)

n_max = max(d_emissions_country$n)
year_terms_order = c("2018-Short", "2019-Short", "2020-Short", "2021-Short", "2022-Short", "2018-Mid", "2019-Mid", "2020-Mid", "2021-Mid", "2022-Mid", "2018-Long", "2019-Long", "2020-Long", "2021-Long", "2022-Long")
d_emissions_country <- mutate(d_emissions_country, label=paste0(disclosure_year, "-", term))
d_emissions_country$label <- factor(d_emissions_country$label, levels=rev(year_terms_order))
#d_emissions_country$label <- factor(d_emissions_country$label, levels=year_terms_order)

n_max = max(d_emissions_country$n)
# break symbol
# https://english.stackexchange.com/questions/196138/what-to-call-the-symbol-where-there-is-a-break-in-content-or-a-break-in-a-graph
g_country_breakdown <- ggplot(data=d_emissions_country) +
                        geom_bar(aes(x=country2, y=n, fill=label, alpha=disclosure_year), position="dodge", stat="identity") +
                        #geom_label(aes(x=6, y=1990, label = "/ /", angle=90)) +
  geom_richtext(aes(x=5.9, y=1850, label = ".   /   /   ."), angle=270) +
  geom_richtext(aes(x=5.9, y=1950, label = n_max), angle=-45) +
  scale_x_discrete(name="Economy", expand = c(0, 0)) +
                        scale_y_continuous(name="Number of targets", breaks=seq(0, n_max, 500), expand = c(0, 0)) +
                        scale_fill_manual(name="Term target", values=c("2018-Short"="blue", "2019-Short"="blue", "2020-Short"="blue", "2021-Short"="blue", "2022-Short"="blue", 
                                                                       "2018-Mid"="red", "2019-Mid"="red", "2020-Mid"="red", "2021-Mid"="red", "2022-Mid"='red', 
                                                                       "2018-Long"="darkgreen", "2019-Long"="darkgreen", "2020-Long"="darkgreen", "2021-Long"="darkgreen", "2022-Long"="darkgreen"),
                                                              breaks=c("2022-Long", "2022-Mid", "2022-Short"),
                                                              labels=c("Long", "Medium", "Short")) +
                        scale_alpha_discrete(name="Disclosure year", range = c(0.27, 0.75),) +
                        theme_bw() + 
                        theme(strip.background = element_blank(),
                              strip.text = element_text(face="bold", size=15),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.text=element_text(size=15),
                              legend.text=element_text(size=15),
                              legend.title=element_text(size=15)) + 
                        guides(fill=guide_legend(reverse=T)) +
                        coord_flip(ylim=c(0, 2000))
g_country_breakdown 
ggsave("Ambition_scripts/graphs/figure_scope_emissions_breakdown_appendix.png", g_country_breakdown, dpi=300, units="cm", width=30, height=40)
ggsave("Ambition_scripts/graphs/figure_country_emissions_breakdown_appendix.eps", g_country_breakdown, dpi=300, units="cm", width=30, height=40)
ggsave("Ambition_scripts/graphs/figure_country_emissions_breakdown_appendix.svg", g_country_breakdown, dpi=300, units="cm", width=30, height=40)
ggsave("Ambition_scripts/graphs/figure_country_emissions_breakdown_appendix.pdf", g_country_breakdown, dpi=300, units="cm", width=30, height=40)
write.table(d_emissions_country, "Ambition_scripts/graphs/data/figure_breakdown_emissions_appendix.csv", sep=";", row.names=FALSE)

#-------------------
# Only China, US, EU, India
d_emissions_country_selection <- filter(d_emissions_country, country2%in%c("EU-27", "USA", "China", "India"))
d_emissions_country_selection$label <- factor(d_emissions_country_selection$label, levels=year_terms_order)
n_max = max(d_emissions_country_selection$n)
# break symbol
# https://english.stackexchange.com/questions/196138/what-to-call-the-symbol-where-there-is-a-break-in-content-or-a-break-in-a-graph
g_country_breakdown <- ggplot(data=d_emissions_country_selection) +
  geom_bar(aes(x=country2, y=n, fill=label, alpha=disclosure_year), position="dodge", stat="identity") +
  geom_richtext(aes(x=2.1, y=1850, label = ".   /   /   ."), angle=0) +
  geom_richtext(aes(x=2.1, y=1950, label = n_max), angle=-45) +
  scale_x_discrete(name="Economy", expand = c(0, 0)) +
  scale_y_continuous(name="Number of targets", breaks=seq(0, n_max, 500), expand = c(0, 0)) +
  scale_fill_manual(name="Term target", values=c("2018-Short"="blue", "2019-Short"="blue", "2020-Short"="blue", "2021-Short"="blue", "2022-Short"="blue", 
                                                 "2018-Mid"="red", "2019-Mid"="red", "2020-Mid"="red", "2021-Mid"="red", "2022-Mid"='red', 
                                                 "2018-Long"="darkgreen", "2019-Long"="darkgreen", "2020-Long"="darkgreen", "2021-Long"="darkgreen", "2022-Long"="darkgreen"),
                    breaks=c("2022-Long", "2022-Mid", "2022-Short"),
                    labels=c("Long", "Medium", "Short")) +
  scale_alpha_discrete(name="Disclosure year", range = c(0.27, 0.75),) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face="bold", size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=20),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15)) + 
  guides(fill=guide_legend(reverse=T)) +
  coord_cartesian(ylim=c(0, 2000))
g_country_breakdown 
ggsave("Ambition_scripts/graphs/Figure7_scope_emissions_breakdown.png", g_country_breakdown, dpi=300, units="cm", width=40, height=30)
ggsave("Ambition_scripts/graphs/Figure7_country_emissions_breakdown.eps", g_country_breakdown, dpi=300, units="cm", width=40, height=30, device=cairo_ps)
ggsave("Ambition_scripts/graphs/Figure7_country_emissions_breakdown.svg", g_country_breakdown, dpi=300, units="cm", width=40, height=30)
ggsave("Ambition_scripts/graphs/Figure7_country_emissions_breakdown.pdf", g_country_breakdown, dpi=300, units="cm", width=40, height=30)
write.table(d_emissions_country, "Ambition_scripts/graphs/data/figure7_breakdown_emissions.csv", sep=";", row.names=FALSE)

###### Figure Scope 1+2 emissions covered by targets ############################################################

g_scope12 <- c("Scope 1| Scope 2",  "Scope 1| Scope 2| Scope 3", "Scope 1", "Scope 1| Scope 3", "Scope 2", "Scope 2| Scope 3")
for (t in c(2018, 2019, 2020, 2021, 2022))
{ d_scope12 <- read.table(paste0(data_dir, "processed/abs_er_scope12_processed_", t, ".csv"), header=T, sep=";") 
  assign(paste0("d_scope12_", t), d_scope12)
  print(paste0(t, ": "))
  print(unique(d_scope12$scope_def_2022))
}

# targets
d_scope12_targets_companies <- mutate(d_scope12_2018, X=1, scope=scope_def_2022) %>%
                               select(-scope_def_2018, -scope_def_2022) %>%
                               group_by(scope, disclosure_year) %>%
                               summarise(nr_scope_targets=n()) %>%
                               spread(key=scope, value=nr_scope_targets) %>%
                               mutate(nr_targets = sum(c_across(starts_with("Scope")), na.rm = T),
                                      nr_companies=length(unique(d_scope12_2018$account_id)))
d_scope12_targets_companies <- add_cols(d_scope12_targets_companies, g_scope12)

for (y in years[2:length(years)])
{ d_scope12 = eval(parse(text = paste0("d_scope12_", y)))
  tmp <- mutate(d_scope12, X=1, scope=scope_def_2022) %>%
         select(-scope_def_2018, -scope_def_2022) %>%
         group_by(scope, disclosure_year) %>%
         summarise(nr_scope_targets=n()) %>%
         spread(key=scope, value=nr_scope_targets) %>%
         mutate(nr_targets = sum(c_across(starts_with("Scope")), na.rm = T),
                nr_companies=length(unique(d_scope12$account_id)))
  tmp <- add_cols(tmp, g_scope12)
  d_scope12_targets_companies <- rbind(d_scope12_targets_companies, tmp)
} # for
d_scope12_targets_companies <- select(d_scope12_targets_companies, disclosure_year, nr_targets, nr_companies, everything())
d_scope12_targets_companies <- d_scope12_targets_companies %>% replace(is.na(.), 0)

source('Ambition_scripts/Figures_ambition_report_2023_functions.R')

# emissions
d_scope12 = d_scope12_2018 %>%
            mutate(scope=scope_def_2022) %>%
            select(-scope_def_2018, -scope_def_2022)
d_scope12 <- group_by(d_scope12, account_id, disclosure_year) %>%
             summarise(emissions_reporting_year_interpolated_scope12=max(emissions_reporting_year_interpolated_scope12))
d_scope12$emissions_reporting_year_interpolated_scope12 <- 10^-9*d_scope12$emissions_reporting_year_interpolated_scope12
# use max emissions per account_id
d_scope12_years_emissions <- group_by(d_scope12, disclosure_year) %>%
                             summarise(emissions_reporting_year_interpolated_scope12=sum(emissions_reporting_year_interpolated_scope12))
for (y in years[2:length(years)]) 
{ d_scope12 = eval(parse(text = paste0("d_scope12_", y)))
  d_scope12 = d_scope12 %>%
              mutate(scope=scope_def_2022) %>%
              select(-scope_def_2018, -scope_def_2022)
  d_scope12 <- group_by(d_scope12, account_id, disclosure_year) %>%
               summarise(emissions_reporting_year_interpolated_scope12=max(emissions_reporting_year_interpolated_scope12))
  d_scope12$emissions_reporting_year_interpolated_scope12 <- 10^-9*d_scope12$emissions_reporting_year_interpolated_scope12
  tmp <- group_by(d_scope12, disclosure_year) %>%
         summarise(emissions_reporting_year_interpolated_scope12=sum(emissions_reporting_year_interpolated_scope12, na.rm=T))
  d_scope12_years_emissions <- rbind(d_scope12_years_emissions, tmp)
} # for
d_scope12_years_emissions <- select(d_scope12_years_emissions, disclosure_year, emissions_reporting_year_interpolated_scope12, everything())
d_scope12_years_emissions <- d_scope12_years_emissions %>% replace(is.na(.), 0)

#------------------
g_scope12_fig <- c("Scope 1+2",  "Scope 1+2+3", "Scope 1", "Scope 1+3", "Scope 2", "Scope 2+3")
d_scope12_targets_companies_adj <- d_scope12_targets_companies %>% mutate(Companies="Companies")
d_scope12_targets_companies_adj$Companies <- as.factor(d_scope12_targets_companies_adj$Companies)
scale_fig_s12 = 375
d_scope12_targets_companies_adj$disclosure_year <- scale_fig_s12*d_scope12_targets_companies_adj$disclosure_year %>% as.integer()
trans_x <- function(x)(x/scale_fig_s12)
g_scope12_targets_companies<-ggplot(data=d_scope12_targets_companies_adj) + 
                             geom_scatterpie(aes(x=disclosure_year, y=nr_targets), data=d_scope12_targets_companies_adj,
                                             cols=g_scope12, pie_scale = 2.5) +
                             geom_point(aes(x=disclosure_year, y=nr_companies, shape=Companies), size=5) +
                             scale_y_continuous(name="Number of targets/companies", limits=c(0, NA)) +
                             scale_x_continuous(name="Disclosure year", labels=trans_x, breaks=seq(2018*scale_fig_s12, 2022*scale_fig_s12, 1*scale_fig_s12)) +
                             scale_fill_brewer(name="Disclosure year", palette="Set1", labels=g_scope12_fig) +
                             scale_shape_discrete(name="") +
                             theme_bw(base_size=15) +
                             guides(fill=guide_legend(title="Targets (scope)", reverse=F, size=15, order=1),
                                    shape=guide_legend(legend.title=element_blank()), 
                                    size=15, order=2) +
                             theme(legend.text=element_text(size=15)) +
                             coord_fixed() 
g_scope12_targets_companies

g_scope12_emissions <- ggplot(data=d_scope12_years_emissions) +
                        geom_line(aes(x=disclosure_year, y=emissions_reporting_year_interpolated_scope12), linetype="dotted") +
                        geom_point(aes(x=disclosure_year, y=emissions_reporting_year_interpolated_scope12), size=2.5, colour="grey") +
                        scale_x_continuous(name="Disclosure year") +
                        scale_y_continuous(name="Emissions in disclosure year\n(interpolated (GtCO2eq)", limits=c(0, NA), labels=scales::comma) +
                        theme_bw(base_size=15)
g_scope12_emissions

#https://stackoverflow.com/questions/17073772/ggplot2-legend-on-top-and-margin
#g_scope12 = g_scope12_targets_companies + theme(legend.position="top") + guides(fill=guide_legend(title.position="top")) + g_scope12_emissions
g_scope12 = g_scope12_targets_companies + g_scope12_emissions
g_scope12
ggsave("Ambition_scripts/graphs/Figure9_scope12.svg", g_scope12, dpi=300, units="cm", width=30, height=20)
ggsave("Ambition_scripts/graphs/Figure9_scope12.pdf", g_scope12, dpi=300, units="cm", width=30, height=20)
ggsave("Ambition_scripts/graphs/Figure9_scope12.eps", g_scope12, dpi=300, units="cm", width=30, height=20)
ggsave("Ambition_scripts/graphs/Figure9_scope12.png", g_scope12, dpi=300, units="cm", width=30, height=20)

write.table(d_scope12_targets_companies, "Ambition_scripts/graphs/data/figure9_scope12_a.csv", sep=";", row.names=FALSE)
write.table(d_scope12_years_emissions, "Ambition_scripts/graphs/data/figure9_scope12_b.csv", sep=";", row.names=FALSE)


###### Figure Scope 3 ################################################################################

# read data
for (t in c(2018, 2019, 2020, 2021, 2022))
{ d_scope3 <- read.table(paste0(data_dir, "processed/abs_er_scope3_processed_", t, ".csv"), header=T, sep=";") %>%
  as.data.frame() %>%
  mutate(disclosure_year=t) %>%
  select(disclosure_year, everything())
assign(paste0("d_scope3_", t), d_scope3)
}

# check outliers
check_2018 <- d_scope3_2018 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2019 <- d_scope3_2019 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2020 <- d_scope3_2020 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2021 <- d_scope3_2022 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2022 <- d_scope3_2022 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
# remove outlier
d_scope3_2019 <- filter(d_scope3_2019, account_id !=10629)

# SCATTER PIE CHARTS

# Create one figure with pie charts for each reporting year
# 1. create dataset with number of targets, companies and number of targets per scope type    
g_scope3 <- c("Scope 3", "Scope 1| Scope 2| Scope 3","Other")
# Number of targets and companies data
# OTHER?
d_scope3_targets_companies <- mutate(d_scope3_2018, X=1) %>%
  group_by(scope, disclosure_year) %>%
  summarise(nr_scope_targets=n()) %>%
  spread(key=scope, value=nr_scope_targets) %>%
  mutate(nr_targets=`Scope 3`+`Scope 1| Scope 2| Scope 3`,
         nr_companies=length(unique(d_scope3_2018$account_id)),
         Other=0)
for (y in years[2:length(years)])
{ d_scope3 = eval(parse(text = paste0("d_scope3_", y)))
if (y>=2022) {d_scope3=mutate(d_scope3, scope=ifelse(scope%in%c("Scope 2| Scope 3", "Scope 1| Scope 3"), "Other", scope))}
tmp <- mutate(d_scope3, X=1) %>%
  group_by(scope, disclosure_year) %>%
  summarise(nr_scope_targets=n()) %>%
  spread(key=scope, value=nr_scope_targets) %>%
  mutate(nr_targets=`Scope 3`+`Scope 1| Scope 2| Scope 3`,
         nr_companies=length(unique(d_scope3$account_id)))
if (y<2022) {tmp=mutate(tmp, Other=0)}
d_scope3_targets_companies <- rbind(d_scope3_targets_companies, tmp)
} # for

d_scope3 = d_scope3_2018
d_scope3$emissions_reporting_year_interpolated_scope3 <- 10^-9*d_scope3$emissions_reporting_year_interpolated_scope3
d_scope3_years_emissions <- group_by(d_scope3, disclosure_year, account_id) %>%
  summarise(emissions_reporting_year_interpolated_scope3=max(emissions_reporting_year_interpolated_scope3))
d_scope3_years_emissions <- group_by(d_scope3, disclosure_year) %>%
  summarise(emissions_reporting_year_interpolated_scope3=sum(emissions_reporting_year_interpolated_scope3))
for (y in years[2:length(years)]) 
{ d_scope3 = eval(parse(text = paste0("d_scope3_", y)))
d_scope3$emissions_reporting_year_interpolated_scope3 <- 10^-9*d_scope3$emissions_reporting_year_interpolated_scope3
tmp <- group_by(d_scope3, disclosure_year, account_id) %>%
  summarise(emissions_reporting_year_interpolated_scope3=max(emissions_reporting_year_interpolated_scope3))
tmp <- group_by(tmp, disclosure_year) %>%
  summarise(emissions_reporting_year_interpolated_scope3=sum(emissions_reporting_year_interpolated_scope3, na.rm=T))
d_scope3_years_emissions <- rbind(d_scope3_years_emissions, tmp)
} # for

d_scope3_targets_companies <- select(d_scope3_targets_companies, disclosure_year, nr_targets, nr_companies, `Scope 3`, `Scope 1| Scope 2| Scope 3`,`Other`)
d_scope3_years_emissions <- select(d_scope3_years_emissions, disclosure_year, emissions_reporting_year_interpolated_scope3)

#------------------
d_fig_s3 <- d_scope3_targets_companies %>% mutate(Companies="Companies")
d_fig_s3$Companies <- as.factor(d_fig_s3$Companies)
scale_fig_3 = 375
d_fig_s3$disclosure_year <- scale_fig_3*d_fig_s3$disclosure_year %>% as.integer()
trans_x <- function(x)(x/scale_fig_3)
g_scope3_targets_companies<-ggplot() + 
  geom_scatterpie(aes(x=disclosure_year, y=nr_targets), data=d_fig_s3,
                  cols=c("Scope 1| Scope 2| Scope 3", "Scope 3", "Other"), pie_scale = 2.5) +
  geom_point(data=d_fig_s3, aes(x=disclosure_year, y=nr_companies, shape=Companies), size=5) +
  scale_y_continuous(name="Number of targets/companies", limits=c(0, NA)) +
  scale_x_continuous(name="Disclosure year", labels=trans_x, breaks=seq(2018*scale_fig_3, 2022*scale_fig_3, 1*scale_fig_3)) +
  #scale_fill_discrete(labels=c("Scope 1+2+3", "Scope 3", "Other")) +
  scale_fill_brewer(name="disclosure year", palette="Set1", labels=c("Scope 1+2+3", "Scope 3", "Scope 2+3")) +
  scale_shape_discrete(name="") +
  theme_bw(base_size=15) +
  guides(fill=guide_legend(title="Targets (scope)", reverse=F, size=15, order=1),
         shape=guide_legend(legend.title=element_blank()), 
         size=15, order=2) +
  theme(legend.text=element_text(size=15)) +
  coord_fixed() 
g_scope3_targets_companies

g_scope3_emissions <- ggplot(data=d_scope3_years_emissions) +
  geom_line(aes(x=disclosure_year, y=emissions_reporting_year_interpolated_scope3), linetype="dotted") +
  geom_point(aes(x=disclosure_year, y=emissions_reporting_year_interpolated_scope3), size=2.5, colour="grey") +
  scale_x_continuous(name="Disclosure year") +
  scale_y_continuous(name="Scope 3 emissions in disclosure year\n(interpolated (GtCO2eq)", limits=c(0, NA), labels=scales::comma) +
  theme_bw(base_size=15)
g_scope3_emissions

#https://stackoverflow.com/questions/17073772/ggplot2-legend-on-top-and-margin
#g_scope3 = g_scope3_targets_companies + theme(legend.position="top") + guides(fill=guide_legend(title.position="top")) + g_scope3_emissions
g_scope3 = g_scope3_targets_companies + g_scope3_emissions
g_scope3
ggsave("Ambition_scripts/graphs/Figure10_scope3.svg", g_scope3, dpi=300,width = 30, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure10_scope3.eps", g_scope3, dpi=300,width = 30, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure10_scope3.png", g_scope3, dpi=300,width = 30, height = 20, units = "cm")

write.table(d_scope3_targets_companies, "Ambition_scripts/graphs/data/figure10_scope3_a.csv", sep=";", row.names=FALSE)
write.table(d_scope3_years_emissions, "Ambition_scripts/graphs/data/figure10_scope3_b.csv", sep=";", row.names=FALSE)

###### Figure scope 1+2 targets primary industry ################################################################################


years <- c(2018, 2019, 2020, 2021, 2022)
y=2021
abs_vector <- paste0("Abs ", 1:250)
for (y in years)
{ #d_industry <- read.table(paste0(data_dir, "processed/abs_er_form_processed_", y, ".csv"), header=T, sep=";")
  d_industry <- eval(parse(text = paste0("abs_er_form_processed_", y)))
  d_industry$target_id <- factor(d_industry$target_id, levels=abs_vector)
  d_industry <- mutate(d_industry, primary_industry=ifelse(primary_industry=="Food  beverage & agriculture", "Food, beverage & agriculture", primary_industry)) %>%
                mutate(primary_industry=ifelse(primary_industry=="Biotech  health care & pharma", "Biotech, health care & pharma", primary_industry))
  d_industry <- group_by(d_industry, account_id) %>%
                top_n(n=1, wt=target_id) %>%
                top_n(n=1, wt=row)
                #top_n(n=1, wt=targeted_reduction) %>%
                # top_n(n=1, wt=percent_achieved) %>%
                #top_n(n=1, wt=scope_def_2018) %>%
                #top_n(n=1, wt=target_status)
  check <- group_by(d_industry, account_id) %>% filter(n()>1)
  print(paste0("Duplicated industry: ", nrow(check)))
  assign(paste0("d_industry_", y), d_industry)
}

d_industry_count <- group_by(d_industry_2018, primary_industry) %>%
                    summarise(count=n()) %>%
                    mutate(disclosure_year=2018) %>%
                    select(disclosure_year, everything())
for (y in years[2:length(years)])
{ d_industry = eval(parse(text = paste0("d_industry_", y)))
  tmp <- group_by(d_industry, primary_industry) %>%
         summarise(count=n()) %>%
         mutate(disclosure_year=y)
  d_industry_count <- rbind(d_industry_count, tmp)
}

d_industry_count <- filter(d_industry_count, !is.na(primary_industry), !primary_industry=="International bodies", !primary_industry=="Corporate Tags")

d_industry_count$disclosure_year <- factor(d_industry_count$disclosure_year)
d_industry_count$primary_industry <- factor(d_industry_count$primary_industry, levels=rev(primary_industries))
d_industry_count$count <- as.numeric(d_industry_count$count)
g_industry <- ggplot(data=d_industry_count) +
              geom_bar(aes(x=primary_industry, y=count, fill=disclosure_year), position="dodge", stat="identity", width=0.75) +
              scale_fill_brewer(name="Disclosure year", palette="Set2") +
              scale_y_continuous(name="Number of companies") +
              scale_x_discrete(name="Industry") +
              theme_bw() +
              theme(legend.text=element_text(size=15)) +
              theme(axis.text=element_text(size=12)) +
              coord_flip()
g_industry
ggsave("Ambition_scripts/graphs/Figure8_industry_breakdown.svg", g_industry, dpi=300,width = 30, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure8_industry_breakdown.eps", g_industry, dpi=300,width = 30, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure8_industry_breakdown.png", g_industry, dpi=300,width = 30, height = 20, units = "cm")

write.table(d_industry_count, "Ambition_scripts/graphs/data/figure8_industry.csv", sep=";", row.names=FALSE)

###### Figure net-zero targets ################################################################################

years_nz = c(2021, 2022)
for (t in years_nz)
{ d_nz <- read.table(paste0(data_dir, "processed/abs_er_nz_processed_", t, ".csv"), header=T, sep=";") %>%
  as.data.frame() %>%
  mutate(disclosure_year=t) %>%
  select(disclosure_year, everything())
  assign(paste0("d_nz_", t), d_nz)
  print(paste0("Number of rows for year ", t, " : ", nrow(d_nz)))
  print(paste0("Number of companies for year ", t, " : ", length(unique(d_nz$account_id))))
}

d_nz_2021 = eval(parse(text = paste0("d_nz_", 2021)))
d_nz_2021 <- select(d_nz_2021, disclosure_year, account_id, primary_industry, target_coverage, target_year, targets_linked)
d_nz_2022 = eval(parse(text = paste0("d_nz_", 2022)))
d_nz_2022 <- select(d_nz_2022, disclosure_year, account_id, primary_industry, target_coverage, target_year, targets_linked)
d_nz <- rbind(d_nz_2021, d_nz_2022)
d_nz <- filter(d_nz, !is.na(primary_industry), !primary_industry=="International bodies", !primary_industry=="Corporate Tags")
d_nz <- mutate(d_nz, primary_industry=ifelse(primary_industry=="Food  beverage & agriculture", "Food, beverage & agriculture", primary_industry)) %>%
        mutate(primary_industry=ifelse(primary_industry=="Biotech  health care & pharma", "Biotech, health care & pharma", primary_industry))

# factorize
d_nz$disclosure_year <- factor(d_nz$disclosure_year)
d_nz$primary_industry <- factor(d_nz$primary_industry, level=rev(primary_industries))
d_nz$disclosure_year <- factor(d_nz$disclosure_year, levels=years_nz)
# calculate number of linked targets
target_types=c("absolute", "intensity", "portfolio", "other")
d_nz_industry_linked_targets <- mutate(d_nz, absolute=ifelse(is.na(targets_linked), 0, str_count(targets_linked, "Abs")),
                                             intensity=ifelse(is.na(targets_linked), 0, str_count(targets_linked, "Int")),
                                             portfolio=ifelse(is.na(targets_linked), 0, str_count(targets_linked, "Por")),
                                             other=ifelse(is.na(targets_linked), 0, str_count(targets_linked, "\\|")+1-absolute-intensity-portfolio)) %>%
                                gather(absolute, intensity, portfolio, other, key=target_type, value=count)
d_nz_industry_linked_targets <- group_by(d_nz_industry_linked_targets, disclosure_year, target_type) %>%
                                summarise(count=sum(count)) %>%
                                mutate(share=count/sum(count)) %>%
                                filter(!(disclosure_year==2021 & target_type=="portfolio"))
d_nz_industry_linked_targets$target_type <- factor(d_nz_industry_linked_targets$target_type, levels=target_types)    

# calulate nr of companies --> CHANGE!
d_nz_industry <- select(d_nz, disclosure_year, account_id, primary_industry, target_year) %>%
                  group_by(disclosure_year, primary_industry) %>%
                  summarise(count=n_distinct(account_id), ty=round(mean(target_year, na.rm=T),0))
check_total <- select(d_nz, disclosure_year, account_id, target_year) %>%
               group_by(disclosure_year) %>%
               summarise(count=n_distinct(account_id), ty=round(mean(target_year, na.rm=T),0))
count_max = max(d_nz_industry$count)

d_nz_industry$count <- as.numeric(d_nz_industry$count)
d_nz_industry$disclosure_year <- factor(d_nz_industry$disclosure_year, levels=rev(years_nz))
g_nz_industry <- ggplot(data=d_nz_industry) +
                 geom_bar(aes(x=primary_industry, y=count, fill=disclosure_year), position="dodge", stat="identity") +
                 geom_text(data=filter(d_nz_industry, disclosure_year==2022), aes(x=primary_industry, y=490, label=ty), nudge_x=-0.25, colour="red") +
                 geom_text(data=filter(d_nz_industry, disclosure_year==2021), aes(x=primary_industry, y=510, label=ty), nudge_x=0.25, colour="deepskyblue4") +
                 geom_text(data=filter(d_nz_industry, disclosure_year==2021), aes(x=13, y=510, label="Average target\nyear")) +
                 scale_fill_brewer(name="Industry", palette="Set1") +
                 scale_x_discrete(name="Industry") +
                 scale_y_continuous(name="Number of companies", breaks=seq(0, count_max, 50)) +
                 theme_bw() +
                 theme(legend.text=element_text(size=15), 
                       axis.text=element_text(size=12),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       legend.position="bottom",
                       #plot.margin = unit(c(0, 25, 0, 450), "lines")
                       ) +
                 guides(fill = guide_legend(reverse=TRUE)) +
                 coord_flip(clip = "off")
g_nz_industry

# https://stackoverflow.com/questions/62173825/ggplot2-geom-text-position-in-pie-chart
g_nz_linked_targets_2021 <- ggplot(data=filter(d_nz_industry_linked_targets, disclosure_year==2021)) +
                       geom_bar(aes(x="", y=share, fill=target_type), stat="identity", colour="black") +
                       geom_label_repel(aes(x=1.7, y=share, label = scales::percent(share, accuracy = .1)), vjust = 0.5, direction="y") +
                       coord_polar(theta="y") +
                       #facet_wrap(~disclosure_year, nrow=2) +
                       scale_fill_brewer(name="Target types", palette="Set1") +
                       theme_void() +
                       theme(legend.position="none")
g_nz_linked_targets_2022 <- ggplot(data=filter(d_nz_industry_linked_targets,disclosure_year==2022)) +
  geom_bar(aes(x="", y=share, fill=target_type), stat="identity", colour="black") +
  geom_label_repel(aes(x=1.7, y=share, label = scales::percent(share, accuracy = .1)), vjust = 0.5, direction="y") +
  coord_polar(theta="y") +
  #facet_wrap(~disclosure_year, nrow=2) +
  scale_fill_brewer(name="Target types", palette="Set1") +
  theme_void() +
  theme(legend.position="bottom", legend.text=element_text(size=15))

g_nz_linked_targets_2021 / g_nz_linked_targets_2022

g_nz <- (g_nz_industry + ggtitle("Number of companies/average target year") & theme(plot.title = element_text(size=18))) + 
  ((g_nz_linked_targets_2021 + ggtitle("Underlying targets\nlinked to net-zero commitments") & theme(plot.title = element_text(size=18))) / 
    g_nz_linked_targets_2022) + 
  plot_annotation(tag_levels = 'a')
g_nz

ggsave("Ambition_scripts/graphs/Figure11_nz.svg", g_nz, dpi=300,width = 40, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure11.eps", g_nz, dpi=300,width = 40, height = 20, units = "cm")
ggsave("Ambition_scripts/graphs/Figure11.png", g_nz, dpi=300,width = 40, height = 20, units = "cm")

write.table(d_nz_industry, "Ambition_scripts/graphs/data/figure11_nz_a.csv", sep=";", row.names=FALSE)
write.table(d_nz_industry_linked_targets, "Ambition_scripts/graphs/data/figure11_nz_b.csv", sep=";", row.names=FALSE)
