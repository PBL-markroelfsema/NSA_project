# Data processing ---------------------------------------------------------
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob
library(xtable)
library(grid)
library(scales)
library(readxl)
# data from IMAGE
currentdir <- getwd()
setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer/NSA/6_R/TIMER_output")
Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer", sep="")
Project=paste("NSA")
TIMERGeneration = 'TIMER_2015'
# Source scripts (after setting working directory)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')
# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy_update','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
NoPolicy_indicators <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=TRUE)

NPi <- ImportTimerScenario('NPi_update','NPi_update', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi_indicators <- ProcessTimerScenario(NPi, Rundir, Project, Policy=TRUE)

ICI_GFEI <- ImportTimerScenario('ICI_GFEI','NPi_update', Rundir, Project, TIMERGeneration, Policy=TRUE)
ICI_GFEI_indicators <- ProcessTimerScenario(ICI_GFEI, Rundir, Project, Policy=TRUE)

setwd(currentdir)