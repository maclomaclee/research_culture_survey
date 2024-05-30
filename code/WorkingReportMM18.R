source("code/utils/DataCleaning.R")
source("code/utils/analysisfunctions.R")
library(forcats)
library(MASS)

employ_dat<- cleandata_factor

demographics <- c("Academic", "Q3", "Carer", "Dis2022", "Ethnicity", "Race", "FirstGen", "Gender", "GenderAssignedAtBirth", "Sexuality")

survey_results18_2_a <- analyze_demographics(employ_dat, demographics, "Q18.2.a")
survey_results18_4_a <- analyze_demographics(employ_dat, demographics, "Q18.4.a")
survey_results18_5_a <- analyze_demographics(employ_dat, demographics, "Q18.5.a")
survey_results18_8_a <- analyze_demographics(employ_dat, demographics, "Q18.8.a")
survey_results18_10_a <- analyze_demographics(employ_dat, demographics, "Q18.10.a")
survey_results18_11_a <- analyze_demographics(employ_dat, demographics, "Q18.11.a")
survey_results18_12_a <- analyze_demographics(employ_dat, demographics, "Q18.12.a")
survey_results18_12_aa <- analyze_demographics(employ_dat, demographics, "Q18.12.a.a")
survey_results18_13_a <- analyze_demographics(employ_dat, demographics, "Q18.13.a")

