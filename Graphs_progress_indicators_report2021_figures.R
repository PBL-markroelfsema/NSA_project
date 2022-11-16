library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)
library(data.table)
library(patchwork)

source("ReadProcessReportData2021.R")
options(scipen = 100)

# PLOT PROGRESS, AMBITION
d_ambition_progress=data_progress_include
d_ambition_progress$on_track <- factor(d_ambition_progress$on_track, levels=c(FALSE, TRUE))
levels(d_ambition_progress$on_track) <- list("On track" = TRUE,
                                             "Not on track" = FALSE)
progress_TY_labels=c("2021-2025", "2026-2035", ">2035")
nrow(d_ambition_progress)

#---------------------------------
# 1. PLOT PROGRESS vs AMBITION
progress_start <- ifelse(choice_progress=="BY", "base year", "target year set")

g_ambition_progress_BY <- ggplot(data=d_ambition_progress, aes(x=annual_ambition_BY_a, y=annual_progress_a)) +
  geom_point(aes(colour=on_track, shape=target_year_group), size=2) +
  geom_smooth(se=FALSE, method=lm, colour="black") +
  scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  labs(x="annualised ambition\nbetween base and target year\n(%)", 
       y="annualised progress from base year(%)") +
  #xlim(-100, 60) +
  theme_bw() +
  guides(colour=guide_legend(order=1), shape=guide_legend(order=2)) +
  ggtitle("Progress vs ambition")
plot(g_ambition_progress_BY)
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_1_ambition_progress.jpg"), g_ambition_progress_BY, width = 20, height = 10, units = "cm")
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_1_ambition_progress.pdf"), g_ambition_progress_BY, width = 20, height = 10, units = "cm")

g_ambition_progress_MRY <- ggplot(data=d_ambition_progress, aes(x=annual_ambition_MRY_a, y=annual_progress_a)) +
  geom_point(aes(colour=on_track, shape=target_year_group), size=2) +
  geom_smooth(se=FALSE, method=lm, colour="black") +
  scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  labs(x="annualised ambition\nbetween reporting and target year\n(%)", 
       y=paste0("annualised progress from ", progress_start, " (%)")) +
  xlim(-100, 60) +
  theme_bw()
plot(g_ambition_progress_MRY)

g_ambition_progress= (g_ambition_progress_BY + theme(legend.position="none"))+g_ambition_progress_MRY + guides(colour=guide_legend(order=1), shape=guide_legend(order=2)) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag=element_text(face="bold"))
plot(g_ambition_progress)
ggsave(paste0("graphs/figure_companies_progress_from_", choice_progress, "_1_ambition_progress.jpg"), g_ambition_progress,dpi=300, width = 55, height = 30, units = "cm")
ggsave(paste0("graphs/figure_companies_progress_from_", choice_progress, "_1_ambition_progress.pdf"), g_ambition_progress, dpi=300, width = 55, height = 30, units = "cm")

# 2. PLOT Maturity against annual progress
q_progress <- data.frame(x=c(quantile(d_ambition_progress$annual_progress_a, 0.05),quantile(d_ambition_progress$annual_progress_a, 0.95)),
                        q=c("5%", "95%"))
                        
# plot PROGRESS
g_annual_progress <- ggplot(data=d_ambition_progress) +
  geom_histogram(aes(x=annual_progress_a, fill=on_track, alpha=on_track), binwidth = 2) +
  geom_vline(data=q_progress, aes(xintercept=x, linetype=q)) +
  scale_fill_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_linetype_manual(name="Percentile", values=c("dashed", "dashed")) +
  scale_alpha_manual(values=c(0.5, 0.5)) +
  theme_bw() +
  guides(alpha="none", fill=guide_legend(order=1), linetype=guide_legend(order=2)) +
  labs(x=paste0("Annualised progress\nbetween ", progress_start, " and\n reporting year (%)"), y="Number of targets") +
  ggtitle("Progress")
plot(g_annual_progress)

max_maturity=max(d_ambition_progress$maturity_remaining)
g_maturity_progress1 <- ggplot(data=d_ambition_progress, aes(x=maturity_expired, y=annual_progress_a)) +
  geom_point(aes(colour=on_track, shape=target_year_group), size=2) +
  scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  labs(x="Years since base year\n(years)", y=paste0("Annualised progress\n between ", progress_start, " and reporting year (%)")) +
  xlim(0, max_maturity) +
  theme_bw()
plot(g_maturity_progress1)
g_maturity_progress2 <- ggplot(data=d_ambition_progress,
                               aes(x=maturity_remaining, y=annual_progress_a)) +
  geom_point(aes(colour=on_track, shape=target_year_group), size=2) +
  scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  labs(x="Remaining time to maturity\n(years)", y=NULL) +
  xlim(0, max_maturity) +
  theme_bw()
plot(g_maturity_progress2)

g_progress=(g_annual_progress + guides(fill=guide_legend(order=1), shape=guide_legend(order=2))) + 
           (g_maturity_progress1 + guides(colour="none")) + 
           (g_maturity_progress2  + guides(colour="none")) +
           plot_annotation(tag_levels = 'a') + plot_layout(guides="collect") & theme(plot.tag=element_text(face="bold"))

plot(g_progress)
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_2_progress.jpg"), g_progress, width = 25, height = 12.5, units = "cm")
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_2_progress.pdf"), g_progress, width = 25, height = 12.5, units = "cm")

#---------------------------------

# 3. PLOT AMBITION
q_ambition <- data.frame(x=c(quantile(d_ambition_progress$annual_ambition_BY_a, 0.05),quantile(d_ambition_progress$annual_ambition_BY_a, 0.95)),
                         Percentiles=c("5%", "95%"))
# 1. plot ambition between BY and TY vs remaining time to maturity
min_ambition=min(d_ambition_progress$annual_ambition_MRY_a)
max_ambition=max(d_ambition_progress$annual_ambition_MRY_a)
d_ambition = filter(d_ambition_progress, 
                    annual_ambition_BY_a>-100, annual_ambition_BY_a<100,
                    annual_ambition_MRY_a>-100, annual_ambition_MRY_a<100)
g_maturity_ambition_BY <- ggplot(data=d_ambition, aes(x=maturity_remaining, y=annual_ambition_BY_a)) +
                          geom_point(aes(colour=on_track, shape=target_year_group, colour=target_year_group), size=2) +
                          geom_line(data=IPCC_annual_path, aes(x=year, y=annual_reduction, linetype=type)) +
                          scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
                          scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
                          scale_linetype_manual(name="Benchmark IPCC annual reduction", values=c("dashed", "dotted", "longdash")) +
                          labs(x="remaining time to maturity\n(years)", y="Annualised ambition\nbetween base and target year\n(%)") +
                          #ylim(min_ambition, max_ambition) +
                          ylim(-5,35) +
                          theme_bw() +
                          ggtitle("Ambition")
plot(g_maturity_ambition_BY)

# 2. plot histogram annual ambitions
g_annual_ambition <- ggplot(data=d_ambition) +
  geom_histogram(aes(x=annual_ambition_BY_a, fill=on_track, alpha=on_track), binwidth = 1) +
  geom_vline(data=q_ambition, aes(xintercept=x, linetype=Percentiles)) +
  scale_fill_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_alpha_manual(values=c(0.5, 0.5)) +
  theme_bw() +
  guides(alpha="none") +
  labs(x="Annualised ambition\nbetween base year and\n reporting year (%)", y="Number of targets")
  #ggtitle("Ambition")
plot(g_annual_ambition)

# 3. plot ambition between MRY and TY vs remaining time to maturity
g_maturity_ambition_MRY <- ggplot(data=d_ambition,
                                  aes(x=maturity_remaining, y=annual_ambition_MRY_a)) +
  geom_point(aes(colour=on_track, shape=target_year_group), size=2) +
  geom_line(data=IPCC_annual_path, aes(x=year, y=annual_reduction, linetype=type)) +
  theme_bw() +
  scale_colour_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_shape_discrete(name="Target year", labels=progress_TY_labels) +
  scale_linetype_manual(name="Benchmark IPCC annual reduction", values=c("dashed", "dotted", "longdash")) +
  labs(x="remaining time to maturity\n(years)", y="Annualised ambition\nbetween reporting and target year\n(%)") +
  ylim(min_ambition, max_ambition)
plot(g_maturity_ambition_MRY)

# 4. difference between two ambitions
d_gap=filter(d_ambition, diff_required_achieved_a>-50, diff_required_achieved_a<50)
g_gap <- ggplot(data=d_gap) +
  geom_histogram(aes(x=diff_required_achieved_a, fill=on_track, alpha=on_track), binwidth = 2) +
  scale_fill_brewer(name="Progress towards target", palette="Set1", direction=-1) +
  scale_alpha_manual(values=c(0.5, 0.5)) +
  theme_bw() +
  guides(alpha="none") +
  labs(x="Difference between original \nand current ambition\n(%)", y="Number of targets")
  #ggtitle("Differences between original and ambition")
plot(g_gap)
ggsave(paste0("graphs/figure_companies_ambition_gap.jpg"), g_gap, scale=1)

g_ambition1 <- (g_annual_ambition + theme(legend.position="none")) + (g_maturity_ambition_BY + theme(legend.position="none")) + g_maturity_ambition_MRY
plot(g_ambition1)
ggsave(paste0("graphs/figure_companies_ambition1.jpg"), g_ambition1, scale=1)

g_ambition2 <- (g_maturity_ambition_BY + guides(colour=guide_legend(order=1), shape=guide_legend(order=2)) + g_annual_ambition + guides(fill="none")) /
               (g_maturity_ambition_MRY  + g_gap + guides(fill="none")) + 
               plot_annotation(tag_levels = 'a')  + plot_layout(guides="collect") & theme(plot.tag=element_text(face="bold"))

plot(g_ambition2)
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_3_ambition2.jpg"), g_ambition2, width = 25, height = 12.5, units = "cm")
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_3_ambition2.pdf"), g_ambition2, width = 25, height = 12.5, units = "cm")

#---------------------------------
# 4 AMBITION PAHTWAYS
Pal_ambition_pathways="Paired"
Col1="chartreuse3"
Col2="blue3"
ambition_pathways <- read_excel('data/CDP/input/Ambitionpathways_v4.xlsx', sheet = "pathways", range="A1:AZ11")
colnames(ambition_pathways)[1] <- "var_name year"
ambition_pathways <- separate(ambition_pathways, `var_name year`, into=c("variable_name", "source_year"), sep=" ") %>%
                     gather(starts_with("2"), key=year, value="value")
ambition_pathways$year <- as.numeric(ambition_pathways$year)

ambition_pathways_sectors <- read_excel('data/CDP/input/Ambitionpathways_v4.xlsx', sheet = "pathways", range="A52:AZ80")
colnames(ambition_pathways_sectors)[1] <- "sector"
ambition_pathways_sectors <- separate(ambition_pathways_sectors, `sector`, into=c("sector", "source_year"), sep="-" ) %>%
                             gather(starts_with("2"), key=year, value="value")
ambition_pathways_sectors$year <- as.numeric(ambition_pathways_sectors$year)

d_companies_all_2018 <- filter(ambition_pathways, year>2018, year<2030, variable_name=="emissions", source_year=="2018")
d_companies_all_2021 <- filter(ambition_pathways, year>2020, year<2030, variable_name=="emissions", source_year=="2021")
d_companies_all <- rbind(d_companies_all_2018, d_companies_all_2021)
g_companies_all <- ggplot(data=d_companies_all) +
                   geom_line(aes(x=year, y=value, colour=source_year), size=1.5) +
                   scale_x_continuous(breaks=seq(2018, 2030, 2)) +
                   #scale_colour_brewer(name="Source data", palette=Pal_ambition_pathways) +
                   scale_colour_manual(name="Source data", values=c(Col1, Col2)) +
                   ylim(0, 5000) +
                   labs(x="Year", y="MtCO2eq") +
                   theme_bw() +
                   ggtitle("Companies with targets\neither or both in 2018 and 2021")
plot(g_companies_all)  

d_companies_overlap_2018 <- filter(ambition_pathways, year>=2018, year<=2030, variable_name=="emissions_overlapping", source_year=="2018")
d_companies_overlap_2021 <- filter(ambition_pathways, year>=2021, year<=2030, variable_name=="emissions_overlapping", source_year=="2021")
d_companies_overlap <- rbind(d_companies_overlap_2018, d_companies_overlap_2021)
g_companies_overlap <- ggplot(data=d_companies_overlap) +
  geom_line(aes(x=year, y=value, colour=source_year), size=1.5) +
  scale_x_continuous(breaks=seq(2018, 2030, 2)) +
  #scale_colour_brewer(name="Source data", palette=Pal_ambition_pathways) +
  scale_colour_manual(name="Source data", values=c(Col1, Col2)) +
  ylim(0, 2000) +
  labs(x="Year", y="MtCO2eq") +
  theme_bw() +
  ggtitle("Companies with targets\nboth in 2018 and 2021")
plot(g_companies_overlap)  

g_companies_total = (g_companies_all + theme(legend.position="none")) + g_companies_overlap +
              plot_annotation(title="Ambition pathways")
plot(g_companies_total)
ggsave(paste0("graphs/figure_companies_ambition_pathways_total.jpg"), g_companies_total, scale=1)

sectors = c("Materials", "Infrastructure", "Power generation")
d_companies_sectors_2018 <- filter(ambition_pathways_sectors, sector%in%sectors, year>=2018, year<=2030)
d_companies_sectors_2021 <- filter(ambition_pathways_sectors, sector%in%sectors, year>=2021, year<=2030)
d_companies_sectors <- rbind(d_companies_sectors_2018, d_companies_sectors_2021)
g_companies_sectors <- ggplot(data=d_companies_sectors) +
                       geom_bar(aes(x=year, y=value, fill=source_year), stat="identity", position="dodge") +
                       facet_wrap(~sector,  strip.position="bottom") +
                       scale_x_continuous(breaks=seq(2018, 2030, 2)) +
                       scale_y_continuous(breaks=seq(0, 900, 100)) +
                       labs(y="MtCO2eq") +
                       theme_bw() +
                       #scale_fill_brewer(name="Source data", palette=Pal_ambition_pathways) +
                       scale_fill_manual(name="Source data", values=c(Col1, Col2)) +
                       theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(colour="black", fill="white"))
plot(g_companies_sectors)
ggsave(paste0("graphs/figure_companies_ambition_pathways_sectors.jpg"), g_companies_sectors, scale=1)

g_companies = ((g_companies_total + theme(legend.position="none"))/ g_companies_sectors) + plot_layout(guides="collect") + plot_annotation(tag_levels = 'a') & theme(plot.tag=element_text(face="bold"))
plot(g_companies)
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_5_ambition_pathways.jpg"), g_companies, width = 20, height = 20, units = "cm")
ggsave(paste0("graphs/report/figure_companies_progress_from_", choice_progress, "_5_ambition_pathways.pdf"), g_companies, width = 20, height = 20, units = "cm")

