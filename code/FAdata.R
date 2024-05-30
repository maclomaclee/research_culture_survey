#generates single combined dataset EdWelcome_dat

#packages

#install.packages("pillar")
#install.packages("foreign")
#install.packages("MASS")
#install.packages("reshape2")
#install.packages("Hmisc")
#install.packages("skimr")
#install.packages("summarytools")
library(Hmisc)
library(pillar)
library(skimr)
library(summarytools)
library(readr)
library(rmarkdown)
library(kableExtra)
library(formatR)
library(knitr)
library(ggplot2)
library(pwr)
library(RColorBrewer)
library(cowplot)
library(foreign)
library(MASS)
library(reshape2)
library(forcats)
library(dplyr)
library(tidyr)
library(foreign)
library(nnet)
library(stargazer)
require(plyr)
source("analysisfunctions.R")

###################################################################################
###################################################################################
########################## Edinburgh Research Survey Data##########################
###################################################################################
###################################################################################

#Reading Edinburgh Research Survey Data
dat<- read.csv('AnalysisSet2.csv', header = T, check.names = F) 


#change column names
cleandat<-dat
colnames(cleandat)<- c("UniqueResponseNumber", "Q1", "Q2", "Q2.a", "Q2.b", "Q3", "Q3.a", 
                       "Q4", "Q4.a", "Q5", "Q6", "Q6.a", "Q7", "Q8", "Q8.a", "Q9", "Q9.a", 
                       "Q10", "Q11", "Q12", "Q13", "Q13.1.a", "Q13.2.a", "Q13.3.a", "Q13.4.a", 
                       "Q13.5.a", "Q14", "Q14.1.a", "Q14.2.a", "Q14.3.a", "Q15", "Q15.1.a", 
                       "Q15.2.a", "Q15.3.a", "Q15.4.a", "Q16", "Q16.1.a", "Q16.2.a", "Q16.3.a", 
                       "Q16.4.a", "Q17", "Q17.1.a", "Q17.2.a", "Q17.3.a", "Q17.4.a", "Q18", 
                       "Q18.1.a", "Q18.2.a", "Q18.3.a", "Q18.4.a", "Q18.5.a", "Q18.6.a", 
                       "Q18.7.a", "Q18.8.a", "Q18.9.a", "Q18.10.a", "Q18.11.a", "Q18.12.a", 
                       "Q18.13.a", "Q19", "Q20", "Q20.1.a", "Q20.2.a", "Q20.3.a", "Q20.4.a", 
                       "Q20.5.a", "Q20.6.a", "Q20.7.a", "Q21", "Q21.1.a", "Q21.2.a", "Q21.3.a", 
                       "Q21.4.a", "Q21.5.a", "Q21.6.a", "Q21.7.a", "Q21.8.a", "Q21.9.a", 
                       "Q21.10.a", "Q21.11.a", "Q22", "Q23", "Q23.a", "Q24", "Q24.a", 
                       "Q25", "Q25.1.a", "Q25.2.a", "Q25.3.a", "Q25.4.a", "Q25.5.a", "Q26", 
                       "Q26.1.a", "Q26.2.a", "Q26.3.a", "Q27", "Q27.1.a", "Q27.2.a", "Q27.3.a", 
                       "Q27.4.a", "Q27.5.a", "Q27.6.a", "Q27.7.a", "Q27.8.a", "Q27.9.a", 
                       "Q27.10.a", "Q27.11.a", "Q28", "Q29", "Q29.a", "Q29.a.i", "Q30", 
                       "Q30.a", "Q30.a.i", "Q31", "Q31.a", "Q31.a.i", "Q32", "Q32.a", 
                       "Q32.a.i", "Q33", "Q34", "Q35", "Q36", "Q36.1.a", "Q36.2.a", 
                       "Q37", "Q37.1.a", "Q37.2.a", "Q37.3.a", "Q37.4.a", "Q37.5.a", 
                       "Q37.6.a", "Q37.7.a", "Q37.8.a", "Q37.9.a", "Q37.10.a", "Q37.11.a", 
                       "Q37.12.a", "Q37.13.a", "Q37.14.a", "Q37.15.a", "Q38", "Q38.1", 
                       "Q39", "Q40", "Q40.1.a", "Q40.2.a", "Q40.3.a", "Q40.4.a", "Q40.5.a", "Q41", 
                       "Q41.1.a", "Q41.2.a", "Q41.3.a", "Q41.4.a", "Q41.5.a", "Q41.6.a", 
                       "Q41.7.a", "Q41.8.a", "Q41.9.a", "Q41.10.a", "Q42", "Q42.1", "Q42.2", 
                       "Q42.3", "Q43", "Q43.1.a", "Q43.2.a", "Q43.3.a", "Q43.4.a", "Q43.5.a", 
                       "Q43.6.a", "Q44", "Q44.1.a", "Q44.2.a", "Q44.3.a", "Q44.4.a", "Q44.5.a", 
                       "Q44.6.a", "Q44.7.a", "Q45", "Q46")

#Where empty row replace with NA
# mutate(cleandat, across(everything(), replace("", NA)))
                
                #               vars(colnames(.)),
                #          .funs = list(ifelse(.=="", NA, as.character(.))))

#Remove rows where Q1 is "No" (question for informed consent)
cleandat<-cleandat[!(cleandat$Q1=="No"),]

#Converting all the data into factors
cleandata_factor <- cleandat %>% 
  mutate_if(is.character,as.factor)%>%
  mutate_if(is.numeric,as.factor)
str(cleandata_factor)

#relabel on ordinal scale
vsa_pos <- cleandat[c(1,22:24,26,28:30,47:49,50,54,56,57,59,62,64:67,70:80,87:91,97,99,102,103,105,106,107,128:131,135,137:141,148,149,151,153:155,159:161)]
vsa_neg <- cleandat[c(1,25,51,52,53,55,58,63,68,98,100,101,104,132:134,136,142,147,150,156,157,158,162)]
important_positive <- cleandat[c(1,32:35)]
successful_positive <- cleandat[c(1,37:40,42:45)] #done to here
satisfied_positive <- cleandat[c(1,81)]
positive_positive <- cleandat[c(1,93:95)]
yes_no_positive <- cleandat[c(1,109,112,115,118)]
yes_no_unsure_positive <- cleandat[c(1,121:123)]
polar38.1 <- cleandat[c(1,144)]
performs <- cleandat[c(1,108)]

vsa_pos[vsa_pos == "Very strongly agree"] <- 7
vsa_pos[vsa_pos == "Strongly agree"] <- 6
vsa_pos[vsa_pos == "Agree"] <- 5
vsa_pos[vsa_pos == "Neither agree nor disagree"] <- 4
vsa_pos[vsa_pos == "Disagree"] <- 3
vsa_pos[vsa_pos == "Strongly disagree"] <- 2
vsa_pos[vsa_pos == "Very strongly disagree"] <- 1
vsa_pos[vsa_pos == "Not applicable"] <- NA

vsa_neg[vsa_neg == "Very strongly agree"] <- 1
vsa_neg[vsa_neg == "Strongly agree"] <- 2
vsa_neg[vsa_neg == "Agree"] <- 3
vsa_neg[vsa_neg == "Neither agree nor disagree"] <- 4
vsa_neg[vsa_neg == "Disagree"] <- 5
vsa_neg[vsa_neg == "Strongly disagree"] <- 6
vsa_neg[vsa_neg == "Very strongly disagree"] <- 7
vsa_neg[vsa_neg == "Not applicable"] <- NA

important_positive[important_positive == "Extremely Important"] <- 5
important_positive[important_positive == "Somewhat Important"] <- 4
important_positive[important_positive == "Neutral"] <- 3
important_positive[important_positive == "Somewhat Unimportant"] <- 2
important_positive[important_positive == "Extremely Unimportant"] <- 1
important_positive[important_positive == "Not applicable"] <- NA

successful_positive[successful_positive == "Extremely successful"] <- 5
successful_positive[successful_positive == "Somewhat successful"] <- 4
successful_positive[successful_positive == "Neutral"] <- 3
successful_positive[successful_positive == "Somewhat unsuccessful"] <- 2
successful_positive[successful_positive == "Extremely Unsuccessful"] <- 1
successful_positive[successful_positive == "Not applicable"] <- NA

satisfied_positive[satisfied_positive == "Extremely satisfied"] <- 7
satisfied_positive[satisfied_positive == "Satisfied"] <- 6
satisfied_positive[satisfied_positive == "Quite satisfied"] <- 5
satisfied_positive[satisfied_positive == "Neither satisfied nor unsatisfied"] <- 4
satisfied_positive[satisfied_positive == "Quite unsatisfied"] <- 3
satisfied_positive[satisfied_positive == "Unsatisfied"] <- 2
satisfied_positive[satisfied_positive == "Extremely unsatisfied"] <- 1
satisfied_positive[satisfied_positive == "Not applicable"] <- NA

positive_positive[positive_positive == "Very positive"] <- 5
positive_positive[positive_positive == "Positive"] <- 4
positive_positive[positive_positive == "Neutral"] <- 3
positive_positive[positive_positive == "Negative"] <- 2
positive_positive[positive_positive == "Very Negative"] <- 1
positive_positive[positive_positive == "Not applicable"] <- NA

yes_no_positive[yes_no_positive == "Yes"] <- 2
yes_no_positive[yes_no_positive == "No"] <- 1
yes_no_positive[yes_no_positive == "Not applicable"] <- NA
yes_no_positive[yes_no_positive == "Prefer not to say"] <- NA

yes_no_unsure_positive[yes_no_unsure_positive == "Yes"] <- 3
yes_no_unsure_positive[yes_no_unsure_positive == "Unsure"] <- 2
yes_no_unsure_positive[yes_no_unsure_positive == "No"] <- 1
yes_no_unsure_positive[yes_no_unsure_positive == "Not applicable"] <- NA
yes_no_unsure_positive[yes_no_unsure_positive == "Prefer not to say"] <- NA

performs[performs == "Performs much better"] <- 7
performs[performs == "Performs quite a bit better"] <- 6
performs[performs == "Performs a little better"] <- 5
performs[performs == "Performs the same"] <- 4
performs[performs == "Performs a little worse"] <- 3
performs[performs == "Performs quite a bit worse"] <- 2
performs[performs == "Performs much worse"] <- 1
performs[performs == "Not applicable"] <- NA

polarset <- cbind(vsa_neg,vsa_pos, important_positive, successful_positive, satisfied_positive, positive_positive, yes_no_positive, yes_no_unsure_positive, polar38.1, performs)

polarset[polarset == ""] <- NA
polarset <- polarset[,-25]
polarset <- polarset[,-87]
polarset <- polarset[,-91]
polarset <- polarset[,-99]
polarset <- polarset[,-100]
polarset <- polarset[,-103]
polarset <- polarset[,-107]
polarset <- polarset[,-110]
polarset <- polarset[,-111]
###################################################################################
###################################################################################
########################## Welcome Trust Data######################################
###################################################################################
###################################################################################
#Reading data

library(readr)
wdata<- read.csv('wdata_cleanstructure.csv', header = T, stringsAsFactors = F)

#Create a new factor Academic to indicate Academic 
#If Q5 is Academia / university and Q55 is UK then "UK-Academic" otherwise "other"
wdata$Academic<-ifelse (wdata$Q5..single. == "Academia / university", "Academic", "Other")

wdata$Academic<-as.factor(wdata$Academic)

#Create a new factor UK_Academic to indicate UK-Academic 
#If Q5 is Academia / university and Q55 is UK then "UK-Academic" otherwise "other"
wdata$UK_Academic<-ifelse (wdata$Q5..single. == "Academia / university" & wdata$Q55..single.== "UK", "UK-Academic", "Other")

wdata$UK_Academic<-as.factor(wdata$UK_Academic)

#relabel on ordinal scale
wvsa_pos <- wdata[c(1,12, 13, 14, 16, 17, 18, 19, 32, 33, 34, 35, 39, 41, 42, 44, 46, 48, 49, 50, 51, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 67, 68, 69, 70, 71, 75, 77, 80, 81, 83, 84, 85, 100, 101, 102, 103, 107, 109, 110, 111, 112, 113, 118, 119, 121, 122, 123, 124, 128, 129, 130)]
wvsa_neg <- wdata[c(1,15, 36, 37, 38, 40, 43, 47, 52, 76, 78, 79, 82, 104, 105, 106, 108, 114, 117, 120, 125, 126, 127, 131)]
wimportant_positive <- wdata[c(1,20:23)]
wsuccessful_positive <- wdata[c(1,24:31)] #done to here
wsatisfied_positive <- wdata[c(1,64)]
wpositive_positive <- wdata[c(1,72:74)]
wyes_no_positive <- wdata[c(1,87,88,91,92)]
wyes_no_unsure_positive <- wdata[c(1,95:97)]
wpolar38.1 <- wdata[c(1,115)]
wperforms <- wdata[c(1,86)]
wacademic <- wdata[c(1,160,161)]

wvsa_pos[wvsa_pos == "7"] <- 7
wvsa_pos[wvsa_pos == "6"] <- 6
wvsa_pos[wvsa_pos == "5"] <- 5
wvsa_pos[wvsa_pos == "4"] <- 4
wvsa_pos[wvsa_pos == "3"] <- 3
wvsa_pos[wvsa_pos == "2"] <- 2
wvsa_pos[wvsa_pos == "1"] <- 1
wvsa_pos[wvsa_pos == "8"] <- NA

wvsa_neg[wvsa_neg == "7"] <- 17
wvsa_neg[wvsa_neg == "6"] <- 16
wvsa_neg[wvsa_neg == "5"] <- 15
wvsa_neg[wvsa_neg == "4"] <- 14
wvsa_neg[wvsa_neg == "3"] <- 13
wvsa_neg[wvsa_neg == "2"] <- 12
wvsa_neg[wvsa_neg == "1"] <- 11
wvsa_neg[wvsa_neg == "8"] <- NA
wvsa_neg[wvsa_neg == "17"] <- 1
wvsa_neg[wvsa_neg == "16"] <- 2
wvsa_neg[wvsa_neg == "15"] <- 3
wvsa_neg[wvsa_neg == "14"] <- 4
wvsa_neg[wvsa_neg == "13"] <- 5
wvsa_neg[wvsa_neg == "12"] <- 6
wvsa_neg[wvsa_neg == "11"] <- 7


wsuccessful_positive[wsuccessful_positive == "Extremely successful"] <- 5
wsuccessful_positive[wsuccessful_positive == "Somewhat successful"] <- 4
wsuccessful_positive[wsuccessful_positive == "Neutral"] <- 3
wsuccessful_positive[wsuccessful_positive == "Somewhat unsuccesful"] <- 2
wsuccessful_positive[wsuccessful_positive == "Extemely unsuccesful"] <- 1
wsuccessful_positive[wsuccessful_positive == "Not applicable"] <- NA
wsuccessful_positive[wsuccessful_positive == "NULL"] <- NA
wsuccessful_positive[wsuccessful_positive == "N/A"] <- NA
wsuccessful_positive[wsuccessful_positive == "I don't know"] <- NA

wimportant_positive[wimportant_positive == "Extremely important"] <- 5
wimportant_positive[wimportant_positive == "Somewhat important"] <- 4
wimportant_positive[wimportant_positive == "Neutral"] <- 3
wimportant_positive[wimportant_positive == "Somewhat unimportant"] <- 2
wimportant_positive[wimportant_positive == "Not important at all"] <- 1
wimportant_positive[wimportant_positive == "Not applicable"] <- NA
wimportant_positive[wimportant_positive == "NULL"] <- NA
wimportant_positive[wimportant_positive == "N/A"] <- NA
wimportant_positive[wimportant_positive == "I don't know"] <- NA


wpositive_positive[wpositive_positive == "Very Positive"] <- 5
wpositive_positive[wpositive_positive == "Positive"] <- 4
wpositive_positive[wpositive_positive == "Neutral"] <- 3
wpositive_positive[wpositive_positive == "Negative"] <- 2
wpositive_positive[wpositive_positive == "Very Negative"] <- 1
wpositive_positive[wpositive_positive == "NULL"] <- NA

wyes_no_positive[wyes_no_positive == "Yes"] <- 2
wyes_no_positive[wyes_no_positive == "No"] <- 1
wyes_no_positive[wyes_no_positive == "N/A"] <- NA
wyes_no_positive[wyes_no_positive == "Prefer not to say"] <- NA
wyes_no_positive[wyes_no_positive == "NULL"] <- NA

wyes_no_unsure_positive[wyes_no_unsure_positive == "Yes"] <- 3
wyes_no_unsure_positive[wyes_no_unsure_positive == "Unsure"] <- 2
wyes_no_unsure_positive[wyes_no_unsure_positive == "No"] <- 1
wyes_no_unsure_positive[wyes_no_unsure_positive == "NULL"] <- NA
wyes_no_unsure_positive[wyes_no_unsure_positive == "Prefer not to say"] <- NA

wpolar38.1[wpolar38.1 == "7"] <- 7
wpolar38.1[wpolar38.1 == "6"] <- 6
wpolar38.1[wpolar38.1 == "5"] <- 5
wpolar38.1[wpolar38.1 == "4"] <- 4
wpolar38.1[wpolar38.1 == "3"] <- 3
wpolar38.1[wpolar38.1 == "2"] <- 2
wpolar38.1[wpolar38.1 == "1"] <- 1
wpolar38.1[wpolar38.1 == "8"] <- NA

wperforms[wperforms == "N/A"] <- NA
wperforms[wperforms == "NULL"] <- NA

wpolarset <- cbind(wvsa_neg,wvsa_pos, wimportant_positive, wsuccessful_positive, wsatisfied_positive, wpositive_positive, wyes_no_positive, wyes_no_unsure_positive, wpolar38.1, wperforms, wacademic)

wpolarset[wpolarset == ""] <- NA
wpolarset <- wpolarset[,-25]
wpolarset <- wpolarset[,-87]
wpolarset <- wpolarset[,-91]
wpolarset <- wpolarset[,-99]
wpolarset <- wpolarset[,-100]
wpolarset <- wpolarset[,-103]
wpolarset <- wpolarset[,-107]
wpolarset <- wpolarset[,-110]
wpolarset <- wpolarset[,-111]
wpolarset <- wpolarset[,-112]



#Dataset with only UK-Academic
UKAcademicData<- subset(wpolarset, UK_Academic=="UK-Academic")
AcademicData<- subset(wpolarset, Academic=="Academic")

#Delete unnecessary columns
UKAcademicData <- UKAcademicData[ -c(1,112:113) ]
AcademicData <- AcademicData[ -c(1,112:113) ]
wpolarset <- wpolarset[ -c(1,112:113) ]
polarset <- polarset[ -c(1) ]

#now we have 4 datasets in common structure: 
#Edinburgh (polarset) n = 1491
#Wellcome (wpolarset) n = 4267
#Wellcome | academic (AcademicData) n = 2660
#Wellcome | UK academic (UKAcademicData) n = 2169

alldata <- rbind(polarset, wpolarset)
allacademicdata <- rbind(polarset, AcademicData)
allUKAcademic <- rbind(polarset, UKAcademicData)

f_UKAcademicData <- as.factor(UKAcademicData)
f_AcademicData <- as.factor(AcademicData)
f_wpolarset <- as.factor(wpolarset)
f_polarset <- as.factor(polarset)
f_alldata <- as.factor(alldata)
f_allacademicdata <- as.factor(allacademicdata)
f_allUKAcademic <- as.factor(allUKAcademic)




