# STATISTICS

statistics_table <- spread(statistics, key=disclosure_year, value=value)
s1 <- filter(statistics_table, item=='targets')
s2 <- filter(statistics_table, item=='companies')
s3 <- filter(statistics_table, item=='target reference number')
log_print("Number of targets")
log_print(s1)
log_print("Number of companies")
log_print(s2)
log_print("Nr of empty target reference numbers")
log_print(s3)
log_print("Scopes")
log_print("2018: ", )
if (exists("abs_er_form_processed_2018")) log_print(unique(abs_er_form_processed_2018$simple_scope))
log_print("2019: ", )
if (exists("abs_er_form_processed_2019")) log_print(unique(abs_er_form_processed_2019$simple_scope))
log_print("2020: ", )
if (exists("abs_er_form_processed_2020")) log_print(unique(abs_er_form_processed_2020$simple_scope))
log_print("2021: ", )
if (exists("abs_er_form_processed_2021")) log_print(unique(abs_er_form_processed_2021$simple_scope))
log_print("2022: ", )
if (exists("abs_er_form_processed_2022")) log_print(unique(abs_er_form_processed_2022$simple_scope))

log_print("SBTi")

log_print("2019: ", )
sbti_2018 <- filter(abs_er_form_processed_2018, SBTi_status=="Yes  this target has been approved as science-based by the Science-Based Targets initiative")
sbti_perc_2018 = round(100*nrow(sbti_2018)/nrow(abs_er_form_processed_2018),2)
if (exists("abs_er_form_processed_2018")) log_print(sbti_perc_2018)

log_print("2019: ", )
sbti_2019 <- filter(abs_er_form_processed_2019, SBTi_status=="Yes  this target has been approved as science-based by the Science-Based Targets initiative")
sbti_perc_2019 = round(100*nrow(sbti_2019)/nrow(abs_er_form_processed_2019),2)
if (exists("abs_er_form_processed_2019")) log_print(sbti_perc_2019)

log_print("2020: ", )
sbti_2020 <- filter(abs_er_form_processed_2020, SBTi_status=="Yes  this target has been approved as science-based by the Science-Based Targets initiative")
sbti_perc_2020 = round(100*nrow(sbti_2020)/nrow(abs_er_form_processed_2020),2)
if (exists("abs_er_form_processed_2020")) log_print(sbti_perc_2020)

log_print("2021: ", )
sbti_2021 <- filter(abs_er_form_processed_2021, SBTi_status=="Yes  and this target has been approved  by the Science-Based Targets initiative")
sbti_perc_2021 = round(100*nrow(sbti_2021)/nrow(abs_er_form_processed_2021),2)
if (exists("abs_er_form_processed_2021")) log_print(sbti_perc_2021)

log_print("2022: ", )
sbti_2022 <- filter(abs_er_form_processed_2022, SBTi_status=="Yes  we consider this a science-based target  and the target is currently being reviewed by the Science Based Targets initiative"  )
sbti_perc_2022 = round(100*nrow(sbti_2022)/nrow(abs_er_form_processed_2022),2)
if (exists("abs_er_form_processed_2022")) log_print(sbti_perc_2022)



