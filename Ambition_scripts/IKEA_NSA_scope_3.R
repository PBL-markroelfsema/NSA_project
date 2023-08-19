# 1. Preamble ============================================================================

t0 <- Sys.time()  # to calculate the time taken to pull the data using the script 
options(scipen=999)
options(dplyr.summarise.inform = FALSE)

# 2. Library =============================================================================

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
library(patchwork)
library(scatterpie)

#------------------------------------- 
# 3. Functions, variables and settings =============================================================

source('Ambition_scripts/IKEA_NSA_target_matching_functions.R')

# SETTINGS
data_dir = "Ambition_scripts/data/CDP_2023/"
# Target status options: New, Underway, Achieved, Revised, Expired, Replaced, Retired
# exclude 'Expired', 'Replaced', 'Retired'
target_status_include = c('New', 'Underway', 'Revised', 'Achieved')
target_status_exclude = c('Expired', 'Replaced', 'Retired')
target_status_order = c(target_status_include, target_status_exclude)
target_coverage_order = c("Company-wide", "Business activity","Business division", "Country/region", "Site/facility", "Product-level", "Other", "", NA)

output <- TRUE #Set to TRUE if new files should be output
overwrite_input_file <- FALSE # Boolean to express if excel files should be read in even if this was already done before

raw_response_files <- data.frame(year = c(2018, 2019, 2020, 2021, 2022),
                                 filename = c('CDP_2018_Global_Aggregation_raw_response.xlsx',
                                              'CDP_2019_Global_aggregation_raw_response.xlsx',
                                              'CDP_2020_Global_aggregation_raw_response.xlsx',
                                              'CDP_2021_Global_aggregation_raw_response.xlsx',
                                              '2023_NSA_report_CDP_2022_climate_raw_response.xlsx'))

YEAR = 2020
years = c(2018, 2019, 2020, 2021, 2022)
#years = c(2018)


# dataframe to collect column names
abs_colnames <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(abs_colnames) <- c('reporting year', 'column_name', 'profile')

# dataframe to collect all data
abs_er_prof <- data.frame()

# dataframe to collect number of records
statistics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(statistics) <- c('stage', 'reporting_year', 'item', 'profile', 'value')
statistics$stage <- as.character(statistics$stage)
statistics$reporting_year <- as.numeric(statistics$reporting_year)
statistics$item <- as.character(statistics$item)
statistics$profile <- as.character(statistics$profile)
statistics$value <- as.numeric(statistics$value)

# dataframe to collect selection statistics
stat_selection_before <- NULL
stat_selection_after <- NULL

scope3_categories <- c("Scope 3", "Scope1; Scope 2; Scope 3")
scope3_cat_categories <- c("Purchased goods and services",
                          "Capital goods",
                          "Fuel-and-energy-related activities (not included in Scopes 1 or 2)",
                          "Upstream transportation and distribution",
                          "Waste generated in operations",
                          "Business travel",
                          "Employee commuting",
                          "Upstream leased assets",
                          "Downstream transportation and distribution",
                          "Processing of sold products",
                          "Use of sold products",
                          "End-of-life treatment of sold products",
                          "Downstream leased assets",
                          "Franchises",
                          "Investments",
                          "Other",
                          "Upstream",
                          "Downstream",
                          "Upstream & downstream")
scope_accounting_methods <- c("Location-Based", "Market-Based", "", NA)


#-------------------------------------
# 4. Data =============================================================================

# Process datasets for each year
#abs and int target types
for (YEAR in years)
{ ## Read in files
  print(YEAR, sep="\n")
  filename <- filter(raw_response_files, year==YEAR)$filename
  var_target = paste0("abs_er_scope3_", YEAR)
  # https://www.r-bloggers.com/2010/12/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
  # target data
  if(exists(var_target) & !overwrite_input_file)
  { print(paste0(var_target, " already exists"))
    abs_er_scope3 = eval(parse(text = var_target))
  } else{ print(paste("reading in ", var_target), sep="\n")
    abs_er_scope3 <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C4.1a")
    assign(paste0("abs_er_scope3_", YEAR), abs_er_scope3) }
  
  # mry3 emissions
  var_mry3 = paste0("mry_s3_em_", YEAR)
  if(exists(paste0("mry_s3_em_", YEAR)) & !overwrite_input_file)
  { print(paste0(var_mry3, " already exists"))
    mry_s3_em = eval(parse(text = var_mry3))
  } else{ print(paste("reading in ", var_mry3), sep="\n")
    mry_s3_em <- read.xlsx(paste0(data_dir, "input/", filename), sheet = "C6.5")
    assign(paste0("mry_s3_em_", YEAR), mry_s3_em) }
  
  # CLEAN
  
  # Clean files
  abs_er_scope3 <- CleanColumnNames_TargetMatching(abs_er_scope3)
  mry_s3_em <- CleanColumnNames_TargetMatching(mry_s3_em)

  # PREPARE
  
  ## Prepare abs targets dataframes 
  ## Rename and select key columns, remove rows that do not contain target information, i.e. row = 0,  "Question not applicable", target_year = NA
  abs_er_scope3 <- PrepareAbsTargets(abs_er_scope3, YEAR)
  
  # PROCESS
  
  if (YEAR == 2018) {tss <- c(target_status_include, 'Expired')} else {tss <- target_status_include}
  abs_er_scope3 <- ProcessSelect_CDPData(abs_er_scope3, tss, YEAR)
  
  # Process scopes 
  abs_er_scope3 <- ProcessScopes(abs_er_scope3, YEAR)
  
  # SELECT
  
  # unique targets: account_id, target_coverage, scope (scope_accounting_method, scope3_categories, scope_def_2018/scope_def_2022)
  # selection: year_target_set, 
  
  abs_er_scope3_tmp = abs_er_scope3
  # factorize variables
  abs_er_scope3$target_coverage <- factor(abs_er_scope3$target_coverage, level=target_coverage_order) 
  abs_er_scope3$scope_accounting_method <- factor(abs_er_scope3$scope_accounting_method, level=scope_accounting_methods) 
  abs_er_scope3$target_status <- factor(abs_er_scope3$target_status, level=target_status_order)
  # check
  check_factors_1 <- select(abs_er_scope3_tmp, account_id, scope, target_coverage, scope_accounting_method, target_status)
  check_factors_3 <- select(abs_er_scope3, account_id, scope, target_coverage, scope_accounting_method, target_status)
  check_factors <- left_join(check_factors_1,check_factors_3, by=c("account_id", "scope"))
  write.table(check_factors, paste0("Ambition_scripts/data/CDP_2023/processed/check_factors_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  # CHECK duplicates
  # scope, target_year, scope_3_categories may vary
  # selection on target_status, year_target_set, base_year, scope_accounting_method, targeted_reduction
  check_duplicates <- abs_er_scope3$account_id[duplicated(abs_er_scope3$account_id)] %>% as.data.frame() 
  check_duplicates <- unique(check_duplicates$`.`) %>% as.data.frame()
  colnames(check_duplicates) <- c("account_id")
  check_duplicates <- left_join(check_duplicates, abs_er_scope3, by=c("account_id"))
  select_duplicates <- group_by(check_duplicates, account_id, target_year, scope_def_2022, scope3_categories) %>% 
    top_n(n=1, wt=target_status) %>%
    top_n(n=1, wt=year_target_set) %>%
    top_n(n=1, wt=base_year) %>%
    top_n(n=1, wt=scope_accounting_method) %>%
    top_n(n=1, wt=targeted_reduction)
  
  #-------------------------------
  # SCOPE 3 specific
  
  # select scope 3 targets
  test1_scope3 <- filter(abs_er_scope3, grepl("Scope 3|\\+3", scope_def_2018))
  test1_scope3 <- mutate(test1_scope3, across('scope', str_replace, "Scope 3 ", "Scope 3:"), test="test1") %>% select(test, everything(), -scope_def_2022)
  write.table(test1_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/test1_scope3_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  test2_scope3 <- filter(abs_er_scope3, grepl("Scope 3", scope_def_2022)) %>% mutate(test="test2") %>% select(test, everything(), -scope_def_2018)
  write.table(test2_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/test2_scope3_", YEAR, ".csv"), row.names=FALSE, sep=";") 
  
  abs_er_scope3 <- filter(abs_er_scope3, grepl("Scope 3", scope_def_2022))
  
  # determine scope 3 emissions
  if (YEAR<2022)
  { abs_er_scope3 <- mutate(abs_er_scope3, emissions_base_year_scope3=(1-perc_s1s2)*emissions_base_year,
                                           emissions_target_year_scope3=ifelse(is.na(emissions_target_year), NA, (1-perc_s1s2)*emissions_target_year),
                                           emissions_reporting_year_scope3=ifelse(is.na(emissions_reporting_year), NA, (1-perc_s1s2)*emissions_reporting_year))
  } else
  {  #calculate scope 1 and 2 emissions for reporting years >= 2022
    abs_er_scope3 <- mutate(abs_er_scope3, emissions_base_year_scope3=emissions_base_year_s3,
                                           emissions_reporting_year_scope3=emissions_reporting_year_s3,
                                           emissions_target_year_scope3=(1-targeted_reduction/100)*emissions_base_year_s3)
  } # if
 
  abs_er_scope3 <- mutate(abs_er_scope3, scope = scope_def_2022)
  abs_er_scope3 <- select(abs_er_scope3, "account_id", "country", "target_id", "target_coverage",
                                         "scope", "scope3_categories", "base_year", "emissions_base_year_scope3", 
                                         "target_year", "targeted_reduction", "emissions_target_year_scope3", "emissions_reporting_year_scope3")
  
  abs_er_scope3 <- mutate(abs_er_scope3, target_coverage=ifelse(str_starts(target_coverage, "Other"), "Other", target_coverage))
  
  # set variable types
  abs_er_scope3$emissions_base_year_scope3 <- as.numeric(abs_er_scope3$emissions_base_year_scope3)
  abs_er_scope3$emissions_target_year_scope3 <- as.numeric(abs_er_scope3$emissions_target_year_scope3)
  abs_er_scope3$emissions_reporting_year_scope3 <- as.numeric(abs_er_scope3$emissions_reporting_year_scope3)
  abs_er_scope3$base_year <- as.numeric(abs_er_scope3$base_year)
  abs_er_scope3$target_year <- as.numeric(abs_er_scope3$target_year)
  abs_er_scope3$targeted_reduction <- as.numeric(abs_er_scope3$targeted_reduction)

  # calculate interpolate MRY emissions (between BY and TY)
  abs_er_scope3 <- mutate(abs_er_scope3, emissions_target_year_scope3=ifelse(is.na(emissions_target_year_scope3), (1-targeted_reduction/100)*emissions_base_year_scope3, emissions_target_year_scope3))
    
  # aggregate scope 3 targets for reporting years <= 2021
  if (YEAR <= 2021)
  { abs_er_scope3 <- group_by(abs_er_scope3, account_id, country, target_coverage, scope, base_year, target_year) %>%
                     summarise(targeted_reduction=weighted.mean(targeted_reduction, emissions_base_year_scope3),
                               emissions_base_year_scope3=sum(emissions_base_year_scope3),
                               emissions_target_year_scope3=sum(emissions_target_year_scope3),
                               emissions_reporting_year_scope3=sum(emissions_reporting_year_scope3))
  } else 
  { abs_er_scope3 <- select(abs_er_scope3, -scope3_categories, -target_id)
  }
  
  # add interpolated emissions for reporting year
  abs_er_scope3 <- mutate(abs_er_scope3, source_year = YEAR)
  abs_er_scope3 <- mutate(abs_er_scope3, emissions_reporting_year_interpolated_scope3=ifelse(target_year==source_year, emissions_target_year_scope3,
                                                                                      emissions_base_year_scope3+((source_year-base_year)/(target_year-base_year))*(emissions_target_year_scope3-emissions_base_year_scope3)))
  abs_er_scope3 <- abs_er_scope3 %>% as.data.frame()
  
  # save
  assign(paste0("abs_er_scope3_processed_", YEAR), abs_er_scope3)
  write.table(abs_er_scope3, paste0("Ambition_scripts/data/CDP_2023/processed/abs_er_scope3_processed_", YEAR, ".csv"), row.names=FALSE, sep=";") 
} # for

#--------------------------

# DUPLICATES

check_2018_duplicates <- abs_er_scope3_processed_2018$account_id[duplicated(abs_er_scope3_processed_2018$account_id)] %>% as.data.frame() 
colnames(check_2018_duplicates) <- c("account_id")
check_2018_duplicates <- left_join(check_2018_duplicates, abs_er_scope3, by=c("account_id"))
check_2018_duplicates$scope <- factor(check_2018_duplicates$scope, level=scope3_categories)
select_2018_duplicates <- group_by(check_2018_duplicates, account_id) %>% 
                          top_n(n=1, wt=abs(target_year-2030)) %>%
                          top_n(n=1, wt=scope)
## WHY SAME TARGET YEAR AND MULTIPLE RECORDS???? --> market/location-based

# CHECK VALUES
check_2018 <- abs_er_scope3_processed_2018 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2019 <- abs_er_scope3_processed_2019 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2020 <- abs_er_scope3_processed_2020 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2021 <- abs_er_scope3_processed_2022 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
check_2022 <- abs_er_scope3_processed_2022 %>% arrange(desc(emissions_reporting_year_interpolated_scope3)) %>% slice(1:10)
# remove
abs_er_scope3_processed_2019 <- filter(abs_er_scope3_processed_2019, account_id !=10629)
abs_er_scope3_processed_2020 <- filter(abs_er_scope3_processed_2020, account_id !=23126)


#--------------------------
# CREATE PIE CHARTS

# Create separate pie charts for each reporting year
y = 2018
for (y in years)
{ scopes <- c("Scope 3", "Scope 1; Scope 2; Scope 3", "Scope 2; Scope 3", "Scope 1; Scope 3", "Other")
  data_scope3 = eval(parse(text = paste0("abs_er_scope3_processed_", y)))
  if (y>=2022) {data_scope3=mutate(data_scope3, scope=ifelse(scope%in%c("Scope 2; Scope 3", "Scope 1; Scope 3"), "Other", scope))}
  data_scope3$scope <- factor(data_scope3$scope, levels=scopes)
  g_data <- group_by(data_scope3, scope) %>% summarise(nr_targets = n())
  g <- ggplot(data=g_data) +
        geom_bar(aes(x="", y=nr_targets, fill=scope), stat="identity", width=1, color="white") +
        scale_fill_manual(values=c("Scope 3"="blue", "Scope 1; Scope 2; Scope 3"="grey", "Scope 2; Scope 3"="darkgreen", "Scope 1; Scope 3"="darkgoldenrod")) + 
        coord_polar("y", start=0) +
        theme_void()
  assign(paste0("g_scope3_", y), g)
}

g <- g_scope3_2018 + g_scope3_2019 + g_scope3_2020 + g_scope3_2021 + g_scope3_2022
g

#--------------------------
# CREATE SCATTER PIE CHARTS

# Create one figure with pie charts for each reporting year
# 1. create dataset with number of targets, companies and number of targets per scope type    
g_scopes <- c("Scope 3", "Scope 1; Scope 2; Scope 3","Other")
# Number of targets and companies data
data_scope3_years_targets_companies <- mutate(abs_er_scope3_processed_2018, X=1) %>%
                                       group_by(scope, source_year) %>%
                                       summarise(nr_scope_targets=n()) %>%
                                       spread(key=scope, value=nr_scope_targets) %>%
                                       mutate(nr_targets=`Scope 3`+`Scope 1; Scope 2; Scope 3`,
                                              nr_companies=length(unique(abs_er_scope3_processed_2018$account_id)),
                                              Other=0)
for (y in years[2:length(years)])
{ data_scope3_processed = eval(parse(text = paste0("abs_er_scope3_processed_", y)))
  if (y>=2022) {data_scope3_processed=mutate(data_scope3_processed, scope=ifelse(scope%in%c("Scope 2; Scope 3", "Scope 1; Scope 3"), "Other", scope))}
  tmp <- mutate(data_scope3_processed, X=1) %>%
         group_by(scope, source_year) %>%
         summarise(nr_scope_targets=n()) %>%
         spread(key=scope, value=nr_scope_targets) %>%
         mutate(nr_targets=`Scope 3`+`Scope 1; Scope 2; Scope 3`,
                nr_companies=length(unique(data_scope3_processed$account_id)))
  if (y<2022) {tmp=mutate(tmp, Other=0)}
  print(y)
  str(data_scope3_years_targets_companies)
  str(tmp)
  data_scope3_years_targets_companies <- rbind(data_scope3_years_targets_companies, tmp)
}

# Emissions data
# FIRST make sure emissions are not double counted (one company can have multiple targets)
# check_duplicates$scope <- factor(check_duplicates$scope, level=scope3_categories)
#top_n(n=1, wt=abs(target_year-2030)) %>%
#top_n(n=1, wt=scope) %>%
  
  
data_scope3_processed = abs_er_scope3_processed_2018
data_scope3_processed$emissions_reporting_year_interpolated_scope3 <- 10^-6*data_scope3_processed$emissions_reporting_year_interpolated_scope3
data_scope3_years_emissions <- group_by(data_scope3_processed, scope, source_year) %>%
                               summarise(emissions_reporting_year_interpolated_scope3=sum(emissions_reporting_year_interpolated_scope3)) %>%
                               spread(key=scope, value=emissions_reporting_year_interpolated_scope3) %>%
                               mutate(emissions_reporting_year_interpolated_scope3=`Scope 3`+`Scope 1; Scope 2; Scope 3`,
                               Other=0)
y=2018
for (y in years[2:length(years)]) 
{ data_scope3_processed = eval(parse(text = paste0("abs_er_scope3_processed_", y)))
  data_scope3_processed$emissions_reporting_year_interpolated_scope3 <- 10^-6*data_scope3_processed$emissions_reporting_year_interpolated_scope3
  if (y>=2022) {data_scope3_processed=mutate(data_scope3_processed, scope=ifelse(scope%in%c("Scope 2; Scope 3", "Scope 1; Scope 3"), "Other", scope))}
  tmp <- group_by(data_scope3_processed, scope, source_year) %>%
         summarise(emissions_reporting_year_interpolated_scope3=sum(emissions_reporting_year_interpolated_scope3)) %>%
         spread(key=scope, value=emissions_reporting_year_interpolated_scope3) %>%
         mutate(emissions_reporting_year_interpolated_scope3=`Scope 3`+`Scope 1; Scope 2; Scope 3`)
  if (y<2022) {tmp=mutate(tmp, Other=0)}
  data_scope3_years_emissions <- rbind(data_scope3_years_emissions, tmp)
}
data_scope3_years_targets_companies <- select(data_scope3_years_targets_companies, source_year, nr_targets, nr_companies, `Scope 3`, `Scope 1; Scope 2; Scope 3`,`Other`)
data_scope3_years_emissions <- select(data_scope3_years_emissions, source_year, emissions_reporting_year_interpolated_scope3, `Scope 3`, `Scope 1; Scope 2; Scope 3`,`Other`)
write.table(data_scope3_years_targets_companies, paste0("Ambition_scripts/data/CDP_2023/processed/data_scope3_years_targets_companies.csv"), row.names=FALSE, sep=";") 
write.table(data_scope3_years_emissions, paste0("Ambition_scripts/data/CDP_2023/processed/data_scope3_years_emissions.csv"), row.names=FALSE, sep=";") 

#------------------
d_fig <- data_scope3_years_targets_companies %>% mutate(Companies="Companies")
d_fig$Companies <- as.factor(d_fig$Companies)
scale_fig = 375
d_fig$source_year <- scale_fig*d_fig$source_year %>% as.integer()
trans_x <- function(x)(x/scale_fig)
g_scope3_targets_companies<-ggplot() + 
                     geom_scatterpie(aes(x=source_year, y=nr_targets), data=d_fig,
                                     cols=c("Scope 1; Scope 2; Scope 3", "Scope 3"), pie_scale = 2.5) +
                     geom_point(data=d_fig, aes(x=source_year, y=nr_companies, shape=Companies), size=5) +
                     
                     scale_y_continuous(name="Number of targets/companies", limits=c(0, NA)) +
                     scale_x_continuous(name="Reporting year", labels=trans_x, breaks=seq(2018*scale_fig, 2022*scale_fig, 1*scale_fig)) +
                     scale_fill_discrete(labels=c("Scope 1+2+3", "Scope 3")) +
                     scale_shape_discrete(name="") +
                     theme_bw(base_size=15) +
                     guides(fill=guide_legend(title="Targets (scope)", reverse=TRUE, size=15, order=1),
                            shape=guide_legend(legend.title=element_blank()), 
                            size=15, order=2) +
                     theme(legend.text=element_text(size=15)) +
                     coord_fixed() 
g_scope3_targets_companies

#g_scope3_emissions <- ggplot(data=data_scope3_years_emissions) +
#                      geom_line(aes(x=source_year, y=emissions_reporting_year_interpolated), linetype="dotted") +
#                      geom_point(aes(x=source_year, y=emissions_reporting_year_interpolated), size=2.5) +
#                     scale_x_continuous(name="Reporting year") +
#                     scale_y_continuous(name="Interpolated emissions in reporting year (MtCO2eq)", limits=c(0, NA), labels=scales::comma) +
#                      theme_bw(base_size=15)
#g_scope3_emissions

#g_scope3 = g_scope3_targets_companies + g_scope3_emissions
#g_scope3
