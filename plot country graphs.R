library(reshape2)
library(data.table)    
library(ggplot2)
library(tidyverse)

data_NSA <- invisible(fread("data/NSA.csv", sep=";", header=TRUE))
data_NSA <- gather(data_NSA, '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025', 
                      '2026', '2027', '2028', '2029', '2030', key="year", value="emissions")
data_NSA$year <- as.numeric(data_NSA$year)
data_NSA <- spread(data_NSA, key="scenario", value=emissions)

data_history <- invisible(fread("data/historical data.csv", sep=";", header=TRUE))
data_history <- gather(data_history, '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', 
                                     '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', 
                                      '2010', '2011', '2012', '2013', '2014', '2015', 
                                      key="year", value="emissions")
data_history$year <- as.numeric(data_history$year)
data_history <- spread(data_history, key="scenario", value=emissions)

data_NDC_point <- filter(data_NSA, year==2030) %>% select(country, year, NDC_max, NDC_min)

fills <- c("Current policies"="navy", "Current + NSA policies"="seagreen3")
cols <- c("NDC"="black")

for (i in c("Brazil", "China", "EU", "India", "Japan", "Mexico", "USA", "Indonesia", "South Africa")) {
#for (i in c("China")) {
cat(paste(i, "\n"))
country_selec <- i
CPS_2030_min <- filter(data_NSA, country==country_selec, year==2030) %>% select(CPS_min)
CPS_2030_max <- filter(data_NSA, country==country_selec, year==2030) %>% select(CPS_max)
CPS_NSA_2030_min <- filter(data_NSA, country==country_selec, year==2030) %>% select(CPS_NSA_min)
CPS_NSA_2030_max <- filter(data_NSA, country==country_selec, year==2030) %>% select(CPS_NSA_max)

p <- ggplot(data=filter(data_NSA, country==country_selec), aes(x=year)) + 
     # history
     geom_line(data=filter(data_history, country==country_selec), aes(x=year, y=history), colour="black", size=3) +
     
     # CPS
     geom_ribbon(aes(ymin=CPS_min, ymax=CPS_max, fill="Current policies"), alpha=0.5) +
     geom_line(aes(y=CPS_min), size=1) + 
     geom_line(aes(y=CPS_max), size=1)+            
     geom_rect(mapping=aes(xmin=2031, xmax=2031, ymin=CPS_2030_min, ymax=CPS_2030_max), colour="navy", size=3) +
              
     # CPS + NSA
     geom_ribbon(aes(ymin=CPS_NSA_min, ymax=CPS_NSA_max, fill="Current + NSA policies"), alpha=0.6) +
     geom_line(aes(y=CPS_NSA_min), size=1) + 
     geom_line(aes(y=CPS_NSA_max), size=1) + 
     geom_rect(mapping=aes(xmin=2030.5, xmax=2030.5, ymin=CPS_NSA_2030_min, ymax=CPS_NSA_2030_max), colour="seagreen3", size=3) +
  
     scale_fill_manual(name="Scenarios", values=fills) +
  
     #NDCs
     geom_point(data=filter(data_NDC_point, country==country_selec), aes(x=year, y=NDC_max, colour = "NDC"), size=5) +
     geom_point(data=filter(data_NDC_point, country==country_selec), aes(x=year, y=NDC_min, colour = "NDC"), size=5) +
     
     scale_colour_manual(name="2030 level", values=cols) +
     
     xlab("year") +
     ylab(bquote(' ' ~MtCO[2]~'e')) +
  
     theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
     theme(axis.text = element_text(face="bold", size=30)) +
     theme(axis.title = element_text(face="bold", size=35)) +
     theme(legend.text = element_text(face="bold", size=50)) +
     theme(legend.title = element_text(face="bold", size=50)) +
     ylim(0, NA)

     ggsave(file=paste("graphs/Figure_",country_selec,".jpg"),p,width=24,height=14,dpi=400)
     ggsave(file=paste("graphs/Figure_",country_selec,"_small.jpg"),p,width=24,height=14,dpi=200)
}

