###
#title: "Edinburgh Research Culture Project-Data Analysis & Visualisation"
#author: "Ezgi Tanriver-Ayder & Kaitlyn Hair@
#date: "09/07/2020"
#adapted for 2022 by Malcolm Macleod
###

#packages

# install.packages("pillar")
# install.packages("foreign")
# install.packages("MASS")
# install.packages("reshape2")
# install.packages("Hmisc")
# install.packages("skimr")
# install.packages("summarytools")
# install.packages("remotes")
# remotes::install_github("wgaul/wgutil")
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
library(hrbrthemes)
library(wgutil)
library(wgaul)
require(plyr)
source("code/analysisfunctions.R")

###################################################################################
###################################################################################
########################## Edinburgh Research Survey Data##########################
###################################################################################
###################################################################################

#Reading 2022 Edinburgh Research Survey Data
dat<- read.csv('results-for-university-of-2022-11-23-1222.csv', header = T, check.names = F)




#change column names
#non continuous to match with 2020
cleandat<-dat[,-161]
colnames(cleandat)<- c("UniqueResponseNumber", "Q1", "Q2", "Q2.a","Q2.b","Q3", "Q3.a", "Q4", "Q4.a","Q5", "Q6", "Q6.a",
                       "Q7", "Q8", "Q8.a", "Q9", "Q9.a", "Q10", "Q11", "Q12", "Q13", "Q13.3.a", "Q13.4.a", "Q13.6.a", "Q13.7.a",
                       "Q14", "Q14.2.a", "Q14.3.a", "Q15", "Q15.1.a", 
                       "Q15.3.a", "Q15.4.a", "Q16", "Q16.1.a", "Q16.2.a", "Q16.3.a", 
                       "Q16.4.a", "Q17", "Q17.1.a", "Q17.2.a", "Q17.3.a", "Q17.4.a", "Q18", 
                       "Q18.2.a", "Q18.4.a", "Q18.5.a", 
                       "Q18.8.a", "Q18.10.a", "Q18.11.a", "Q18.12.a", "Q18.12.a.a", 
                       "Q18.13.a", "Q19", "Q20", "Q20.1.a", "Q20.2.a", "Q20.4.a", 
                       "Q20.7.a", "Q21", "Q21.3.a", 
                       "Q21.4.a", "Q21.5.a", "Q21.6.a", "Q21.7.a", "Q21.8.a",  
                       "Q21.10.a", "Q22", "Q24", "Q24.a",
                       "Q25", "Q25.1.a", "Q25.2.a", "Q25.3.a", "Q25.4.a", "Q25.5.a", "Q27", "Q27.1.a", "Q27.2.a", "Q27.3.a", 
                       "Q27.6.a", "Q27.7.a", "Q27.8.a", 
                       "Q27.10.a", "Q27.11.a", "Q28", "Q29be", "Q29be.a","Q29be.b", "Q30bw", 
                       "Q30bw.a", "Q30bw.b","Q29he", "Q29he.a", "Q29he.b", "Q30hw", "Q30hw.a", "Q30hw.b",
                        "Q31", "Q31.a",  "Q31.a.i", "Q32", "Q32.a", "Q32.a.i",
                        "Q33", "Q34", "Q35",  
                       "Q37", "Q37.1.a", "Q37.2.a", "Q37.3.a", "Q37.4.a",  
                       "Q37.6.a", "Q37.7.a", "Q37.8.a", "Q37.10.a", 
                       "Q37.12.a", "Q37.13.a", "Q37.14.a", "Q38", "Q38.1", 
                        "Q40", "Q40.3.a", "Q40.5.a", "Q41", 
                       "Q41.1.a", "Q41.2.a", "Q41.3.a", "Q41.4.a",  
                       "Q41.7.a", "Q41.9.a", "Q41.10.a", "Q40a", "Q42", "Q42.1.a", "Q42.2.a", "Q42.3.a",
                       "Q43", "Q43.1.a","Q43.2.a", "Q43.3.a", "Q43.4.a",
                       "Q43.5.a", "Q43.6.a", "Age","Ethnicity","Q59.a", "Race", "Qrace.a","Sexuality", "Q60.a",  "Gender", 
                       "Q61.a", "GenderAssignedAtBirth", "Qtrans.a",  "Carer", "FirstGen", "Dis2022", "Dis.1",
                       "QDis1.a","Dis.2", "timestamp")

# make bullying harassment to single field for comparison with 2020


cleandat$Q29w <- as.character(paste(cleandat$Q29be, cleandat$Q29he))
cleandat$Q30w <- paste(cleandat$Q30bw, cleandat$Q30hw)

cleandat$Dis.2 <- gsub("Yes, a little","Yes", cleandat$Dis.2)
cleandat$Dis.2 <- gsub("Yes, a lot","Yes", cleandat$Dis.2)
cleandat$Dis.2 <- gsub("Not at all","No", cleandat$Dis.2)
cleandat$Dis.2 <- gsub("Prefer not to say","No", cleandat$Dis.2)
cleandat$Dis.2[cleandat$Dis.2==""] <- "No"


# first set every mention of No to No; then over-write with prefer not to say; then overwrite with yes 
#implementation checked again 231122
for(i in 1:nrow(cleandat)){
  if(grepl("No", cleandat[i,162])){cleandat[i,164] = "No"}
  if(grepl("Prefer", cleandat[i,162])){cleandat[i,164] = NA}
  if(grepl("Yes", cleandat[i,162])){cleandat[i,164] = "Yes"}
  if(grepl("No", cleandat[i,163])){cleandat[i,165] = "No"}
  if(grepl("Prefer", cleandat[i,163])){cleandat[i,165] = NA}
  if(grepl("Yes", cleandat[i,163])){cleandat[i,165] = "Yes"}
}
  
names(cleandat)[names(cleandat) == 'V164'] <- 'Q29'
names(cleandat)[names(cleandat) == 'V165'] <- 'Q30'

            

#Where empty row replace with NA
cleandat<-cleandat %>%
  mutate_at(vars(colnames(.)),
            .funs = funs(ifelse(.=="", NA, as.character(.))))
#Where "empty row"Not Applicable" replace with NA
cleandat<-cleandat %>%
  mutate_at(vars(colnames(.)),
            .funs = funs(ifelse(.=="Not applicable", NA, as.character(.))))


#Remove rows where Q1 is "No" (question for informed consent)
cleandat<-cleandat[!(cleandat$Q1=="No"),]

#Converting all the data into factors
cleandata_factor <- cleandat %>% 
  mutate_if(is.character,as.factor)%>%
  mutate_if(is.numeric,as.factor)
str(cleandata_factor)


#Q2.a is answered if Q2 was other. Following step is to clean the data where other category was specified by each respondent uniquely
unique(cleandata_factor$Q2.a)

#Q3 clean the quotes observed
cleandata_factor$Q3<-gsub("College of Arts\\, Humanities and Social Sciences", "CAHSS", cleandata_factor$Q3)
cleandata_factor$Q3<-gsub("College of Science and Engineering", "CSE", cleandata_factor$Q3)
cleandata_factor$Q3<-gsub("College of Medicine and Veterinary Medicine", "CMVM", cleandata_factor$Q3)

#Q.2.a_redefined variable is created to define which respondents are academic, other or student (-we do this since some people selected other and defined what they are but actually they still belong to academic, student or other)

cleandata_factor$Q2.a_redefined<-NA

cleandata_factor$Q2.a_redefined[(cleandata_factor$Q2.a == "Professional Services who used to have Research in JD")|
                                (cleandata_factor$Q2.a == "participant, bystander?") |
                                (cleandata_factor$Q2.a == "Infrastructure") |
                                (cleandata_factor$Q2.a == "technical services manager") |
                                (cleandata_factor$Q2.a == "technician ") |
                                (cleandata_factor$Q2.a == "Stores manager")|
                                (cleandata_factor$Q2.a == "Technician")|
                                (cleandata_factor$Q2.a == "Research Support Staff")|
                                (cleandata_factor$Q2.a == "Professional Services")|
                                (cleandata_factor$Q2.a == "support scientist, specialist services shared facility")|
                                (cleandata_factor$Q2.a == "Research administrator")|
                                (cleandata_factor$Q2.a == "Lab Manager")|
                                (cleandata_factor$Q2.a == "lab manager")|
                                (cleandata_factor$Q2.a == "Research support ERO team")|
                                (cleandata_factor$Q2.a == "Research Support")|
                                (cleandata_factor$Q2.a == "technician team managing research facilities")|
                                (cleandata_factor$Q2.a =="support scientist, specialist services shared facility")| 
                                (cleandata_factor$Q2.a == "Senior Clinical Trial Manager")| 
                                (cleandata_factor$Q2.a == "Professional Services")| 
                                (cleandata_factor$Q2.a == "Professional Services/PE Impact ")| 
                                (cleandata_factor$Q2.a == "Project Manager - is that Research Professional?")| 
                                (cleandata_factor$Q2.a == "Project Manager")|
                                (cleandata_factor$Q2.a == "Facility Manager")| 
                                (cleandata_factor$Q2.a == "administration")| 
                                (cleandata_factor$Q2.a == "Research Communication")| 
                                (cleandata_factor$Q2.a == "Public engagement professional")| 
                                (cleandata_factor$Q2.a == "Laboratory Manager")| 
                                (cleandata_factor$Q2.a == "Professional services manager")| 
                                (cleandata_factor$Q2.a == "Professional services manager")| 
                                (cleandata_factor$Q2.a == "Edinburgh Research Office")]= "Other"

cleandata_factor$Q2.a_redefined[(cleandata_factor$Q2.a == "Lecturer")| 
                                (cleandata_factor$Q2.a == "University Teacher") | 
                                (cleandata_factor$Q2.a == "Im a Chancellors Felllow ") | 
                                (cleandata_factor$Q2.a == "Teaching fellow, researching independently")| 
                                (cleandata_factor$Q2.a == "Senior Lecturer") | 
                                (cleandata_factor$Q2.a == "Research Fellow (beyond post-doc stage) ")  | 
                                (cleandata_factor$Q2.a == "Teacher") | 
                                (cleandata_factor$Q2.a == "Senior Lecturer ") | 
                                (cleandata_factor$Q2.a == "Research Fellow") | 
                                (cleandata_factor$Q2.a == "Lecturer with small proportion of time available for research (<5%)") | 
                                (cleandata_factor$Q2.a == "Teaching Fellow") | 
                                (cleandata_factor$Q2.a == "Researcher") | 
                                (cleandata_factor$Q2.a == "Sr. Lecturer") | 
                                (cleandata_factor$Q2.a == "Teaching and Research") |
                                (cleandata_factor$Q2.a == "Research Fellow ") | 
                                (cleandata_factor$Q2.a == "Lecturer (research-active)") | 
                                (cleandata_factor$Q2.a == "Senior lecturer who researches as part of post") | 
                                (cleandata_factor$Q2.a == "The list is WEIRD! I am a normal academic - and hence one of a large group of staff with research as part (!) of their role - so: Academic (Research and Teaching). This list is a bad start to this survey!!!!") | 
                                (cleandata_factor$Q2.a == "Lecturer with combined research and teaching contract") | 
                                (cleandata_factor$Q2.a == "senior lecturer") | 
                                (cleandata_factor$Q2.a == "Research Fellow") | 
                                (cleandata_factor$Q2.a == "Teaching focused lecturer") | 
                                (cleandata_factor$Q2.a == "Mid-Career Researcher")| 
                                (cleandata_factor$Q2.a == "Knowledge Exchange Officer and lecturer (separate contracts)") | 
                                (cleandata_factor$Q2.a == "Lecturer (40% research)") | 
                                (cleandata_factor$Q2.a == "Senior Lecturer (sorry, I thought none of the terms really conveyed that I do research and teaching)") | 
                                (cleandata_factor$Q2.a == "Reader") | 
                                (cleandata_factor$Q2.a == "academic with research responsibilities")| 
                                (cleandata_factor$Q2.a == "researcher on grade 9 contract")| 
                                (cleandata_factor$Q2.a == "Teacher-researcher")| 
                                (cleandata_factor$Q2.a == "Academic staff on research and teaching contract")| 
                                (cleandata_factor$Q2.a == "academic faculty? ")| 
                                (cleandata_factor$Q2.a == "Lecturer with no current research grants")| 
                                (cleandata_factor$Q2.a == "Independent Research Fellow")| 
                                (cleandata_factor$Q2.a == "Research is 30% of my academic post")| 
                                (cleandata_factor$Q2.a == "Tutor (teaching staff); unpaid researcher")| 
                                (cleandata_factor$Q2.a =="- the term current complicates matters as I have been a PI/CI on £millions of research council funding in multuple countries but not at the present - so how should I describe my position in a way that helps you understand my perspective?") | 
                                (cleandata_factor$Q2.a == "Lecturer, PI, Co-I")| 
                                (cleandata_factor$Q2.a == " just a regular academic") | 
                                (cleandata_factor$Q2.a == "Clinician/ lecturer in Veterinary Oncology with clinical and laboratory research access. ")| 
                                (cleandata_factor$Q2.a =="Senior Lecturer (you might have thought to put lecturers into your list rather than just professors or PIs, or do you really feel that lecturers who are not PIs do not form a part of the research community at UoE?!)") | (cleandata_factor$Q2.a == "Regular academic contract but not technically a professor")| 
                                (cleandata_factor$Q2.a == "Academic ")| 
                                (cleandata_factor$Q2.a == "Knowledge Exchange Officer and lecturer (separate contracts")| 
                                (cleandata_factor$Q2.a == "Lecturer with Research in contract, and Co-I")]= "Academic"


cleandata_factor$Q2.a_redefined[(cleandata_factor$Q2.a == "Recent PhD graduate (July 2020)") | 
                                (cleandata_factor$Q2.a == "MSc Student ") | 
                                (cleandata_factor$Q2.a == "Part time PhD student, part time study coordinator") | 
                                (cleandata_factor$Q2.a == "Research Masters Student") | 
                                (cleandata_factor$Q2.a == "Masters by Research Student ") | 
                                (cleandata_factor$Q2.a == "Clinical Research Fellow and PhD student") | 
                                (cleandata_factor$Q2.a == "Masters Research Student") | 
                                (cleandata_factor$Q2.a == "PGR Student") | 
                                (cleandata_factor$Q2.a == "MScR student") | 
                                (cleandata_factor$Q2.a == "MSc by Research Student") | 
                                (cleandata_factor$Q2.a == "Post graduate student") | 
                                (cleandata_factor$Q2.a == "Masters by Research Student") | 
                                (cleandata_factor$Q2.a == "Part time PhD student, part time study coordinator") | 
                                (cleandata_factor$Q2.a == "RA & PhD student (Sept 2020)") | 
                                (cleandata_factor$Q2.a == "Master of research") |
                                (cleandata_factor$Q2.a == "Masters by Research Student") | 
                                (cleandata_factor$Q2.a == "Master by Research Student")|
                                (cleandata_factor$Q2.a == "MscR Student") | 
                                (cleandata_factor$Q2.a == "MScR researcher") | 
                                (cleandata_factor$Q2.a == "Post-grad student")| 
                                (cleandata_factor$Q2.a == "Clinical lecturer (80% clinical 20% academic time)")]="Student"

#Variable with Student, Other and Academic
cleandata_factor$Academic[(cleandata_factor$Q2== "Other")| (cleandata_factor$Q2 =="Research Professional")  |(cleandata_factor$Q2.a_redefined =="Other")]="Other"

cleandata_factor$Academic[(cleandata_factor$Q2== "Research Technician")|(cleandata_factor$Q2== "Post doctoral Researcher") | (cleandata_factor$Q2 == "Principal Investigator")| (cleandata_factor$Q2 == "Professor")| (cleandata_factor$Q2.a_redefined =="Academic")]="Academic"

cleandata_factor$Academic[(cleandata_factor$Q2.a_redefined == "Student") | (cleandata_factor$Q2 =="PhD Student")]="Student"

cleandata_factor$Academic<-as.factor(cleandata_factor$Academic)

####Data Cleaning for Disability, Gender Assigned at Birth, Gender, Sexuality, Carer, FirstGen, Ethnicity based on the data

#Disability
cleandata_factor$Dis2022[(cleandata_factor$Dis2022 == "Prefer not to say")]=NA
cleandata_factor$Dis2022 <- droplevels(cleandata_factor$Dis2022)

#GenderAssignedAtBirth (cannot look at this factor as there are only a few observation in No category)
cleandata_factor$GenderAssignedAtBirth[(cleandata_factor$GenderAssignedAtBirth == "Prefer not to say")]=NA
cleandata_factor$GenderAssignedAtBirth <- droplevels(cleandata_factor$GenderAssignedAtBirth)

#Gender (dropped nonbinary and other as there were not enough observations)
cleandata_factor$Gender[(cleandata_factor$Gender == "Prefer not to say")]=NA
#cleandata_factor$Gender[(cleandata_factor$Gender == "Non binary")]="Other"
#cleandata_factor$Gender[(cleandata_factor$Gender == "Other")]=NA
#cleandata_factor$Gender <- droplevels(cleandata_factor$Gender)

#Sexuality (bi, gay man, gay woman were all combined into other category due to small number of observations)
### ***not required for 2022 data ***
cleandata_factor$Sexuality[(cleandata_factor$Sexuality == "Prefer not to say")]=NA
#cleandata_factor$Sexuality[(cleandata_factor$Sexuality == "Bi")]="Other"
#cleandata_factor$Sexuality[(cleandata_factor$Sexuality == "Gay Man")]="Other"
#cleandata_factor$Sexuality[(cleandata_factor$Sexuality == "Gay Woman/ Lesbian")]="Other"
#cleandata_factor$Sexuality <- droplevels(cleandata_factor$Sexuality)


#Carer (all different yes options combined into "yes" due to small number of observations in subcategories)
cleandata_factor$Carer[(cleandata_factor$Carer == "Prefer not to say")]=NA
cleandata_factor$Carer<-revalue(cleandata_factor$Carer, c("Yes - equal carer"="Yes", "Yes - main carer"="Yes","Yes - secondary carer (someone else is the main carer)"="Yes" ))
cleandata_factor$Carer <- droplevels(cleandata_factor$Carer)

#FirstGen (all different yes options combined into "yes" due to small number of observations in subcategories)
cleandata_factor$FirstGen[(cleandata_factor$FirstGen == "Prefer not to say")]=NA
cleandata_factor$FirstGen[(cleandata_factor$FirstGen == "Don't know")]=NA
cleandata_factor$FirstGen <- droplevels(cleandata_factor$FirstGen)

#Ethinicity (recategorised due to the small number of observations in subcategories)
cleandata_factor$Ethnicity <- gsub("Western Europe \\(e\\.g\\. Greece\\, Sweden\\, United Kingdom\\)", "European",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Eastern Europe \\(e\\.g\\. Hungary\\, Poland\\, Russia\\)", "European",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("South America \\(e\\.g\\. Brazil\\, Chile\\, Colombia\\)", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("North Africa \\(e\\.g\\. Egypt\\, Morocco\\, Sudan\\)", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Sub\\-Saharan Africa \\(e\\.g\\. Kenya\\, Nigeria\\, South Africa\\)", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("West Asia \\/ Middle East \\(e\\.g\\. Iran\\, Israel\\, Saudi Arabia\\,\\)", "Asian",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("South and Southeast Asia \\(e\\.g\\. India\\, Indonesia\\, Singapore\\)", "Asian",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("East and Central Asia \\(e\\.g\\. China\\, Japan\\, Uzbekistan\\)", "Asian",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Pacific \\/ Oceania \\(e\\.g\\. Australia\\, Papua New Guinea\\, Fiji\\)", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("North America \\(Canada\\, United States\\)", "North American",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Central America and Caribbean \\(e\\.g\\. Jamaica\\, Mexico\\, Panama\\)", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Self described", "Other",cleandata_factor$Ethnicity)
cleandata_factor$Ethnicity <- gsub("Prefer not to say", "Other",cleandata_factor$Ethnicity)

#Race: need to collapse responses with n = 1
cleandata_factor$Race <- gsub("Indigenous \\(e\\.g\\. North American Indian Navajo\\, South American Indian Quechua\\, Aboriginal or Torres Strait Islander\\)","Self described",cleandata_factor$Race)
cleandata_factor$Race <- gsub("Middle Eastern or North African","Self described",cleandata_factor$Race)
cleandata_factor$Race <- gsub("Black","Black Hispanic or Latino a x",cleandata_factor$Race)
cleandata_factor$Race <- gsub("Self described","Black Hispanic or Latino a x",cleandata_factor$Race)
cleandata_factor$Race <- gsub("Hispanic or Latino\\/a\\/x","Black Hispanic or Latino a x",cleandata_factor$Race)


#Creating the Workhourdiff for difference between Q10 and Q11 
cleandata_factor$Q10_newcat<-cleandata_factor$Q10
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("Less than 10 hours"="Less than 30 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("More than 80 hours"="More than 40 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("10-20 hours"="Less than 30 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("20-30 hours"="Less than 30 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("30-40 hours"="30-40 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("40-50 hours"="More than 40 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("50-60 hours"="More than 40 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("60-70 hours"="More than 40 hours"))
cleandata_factor$Q10_newcat <- revalue(cleandata_factor$Q10_newcat, c("70-80 hours"="More than 40 hours"))


cleandata_factor$Q11_newcat<-cleandata_factor$Q11
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("Less than 10 hours"="Less than 30 hours"))
#cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("More than 80 hours"="More than 40 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("10-20 hours"="Less than 30 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("20-30 hours"="Less than 30 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("30-40 hours"="30-40 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("40-50 hours"="More than 40 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("50-60 hours"="More than 40 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("60-70 hours"="More than 40 hours"))
cleandata_factor$Q11_newcat <- revalue(cleandata_factor$Q11_newcat, c("70-80 hours"="More than 40 hours"))
cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "Less than 30 hours" &
                                 Q11_newcat %in% c("More than 40 hours","30-40 hours"), 
                               "Works less", ""))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30 - 40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works less", paste(WorkHourDiff)))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works less",  paste(WorkHourDiff)))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("Less than 30 hours"), 
                               "Works more",  paste(WorkHourDiff)))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "More than 40 hours" &
                                 Q11_newcat %in% c("Less than 30 hours", "30-40 hours"), 
                               "Works more",  paste(WorkHourDiff)))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "More than 40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works the same",  paste(WorkHourDiff)))
cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("30-40 hours"), 
                               "Works the same",  paste(WorkHourDiff)))

cleandata_factor <- cleandata_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "Less than 30 hours" &
                                 Q11_newcat %in% c("Less than 30 hours"), 
                               "Works the same",  paste(WorkHourDiff)))

cleandata_factor$WorkHourDiff[(cleandata_factor$WorkHourDiff == "")]=NA
cleandata_factor$WorkHourDiff[(cleandata_factor$WorkHourDiff == "NA")]=NA
cleandata_factor$WorkHourDiff<-as.factor(cleandata_factor$WorkHourDiff)
cleandata_factor$WorkHourDiff <- droplevels(cleandata_factor$WorkHourDiff)


####Separation of the Edinburgh Survey dataset according to Academic, Other and Student

EdinOtherData<-cleandata_factor%>%
  filter( Q2== "Research Professional" | Q2.a_redefined =="Other")

EdinAcademicData <- cleandata_factor %>%
  filter( Q2== "Post doctoral Researcher" | Q2 == "Principal Investigator"| Q2 == "Professor"| Q2 =="Research Technician"| Q2.a_redefined =="Academic")

EdinStudentData <- cleandata_factor %>%
  filter(Q2.a_redefined == "Student" | Q2 =="PhD Student")

# Applicable to UK Student + Employed Academics
EdinStudent_AcademicData <- rbind(EdinAcademicData, EdinStudentData)
EdinOther_AcademicData <- rbind(EdinAcademicData, EdinOtherData)

###################################################################################
###################################################################################
########################## 2020 Data######################################
###################################################################################
###################################################################################

#reading 2020 Edinburgh Research culture survey 

dat2020<- read.csv("AnalysisSet2.csv", header = T, check.names = F) 


#change column names
cleandat2020<-dat2020
colnames(cleandat2020)<- c("UniqueResponseNumber", "Q1", "Q2", "Q2.a", "Q2.b", "Q3", "Q3.a", 
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
                       "Q41.7.a", "Q41.8.a", "Q41.9.a", "Q41.10.a", "Q42", "Q42.1.a", "Q42.2.a", 
                       "Q42.3.a", "Q43", "Q43.1.a", "Q43.2.a", "Q43.3.a", "Q43.4.a", "Q43.5.a", 
                       "Q43.6.a", "Q44", "Q44.1.a", "Q44.2.a", "Q44.3.a", "Q44.4.a", "Q44.5.a", 
                       "Q44.6.a", "Q44.7.a", "Q45", "Q46", "Q47", "Q47.1", "Q48", "Q48.1.a", 
                       "Q48.2.a", "Q49", "Q50", "Q50.1.a", "Q50.2.a", "Q50.3.a", "Q50.4.a", 
                       "Q50.5.a", "Q50.6.a", "Q50.7.a", "Q51", "Q51.1", "Q51.2", "Q51.3", 
                       "Q52", "Q52.1.a", "Q52.2.a", "Q53", "Q54", "Q54.1", "Q54.2", "Q54.3", 
                       "Q54.4", "Q54.5", "Q54.6", "Q55", "Q56", "Q56.1.a", "Q56.2.a", "Q56.3.a", 
                       "Q56.4.a", "Q56.5.a", "Q56.6.a", "Q56.7.a", "Q56.8.a", "Age", "Q58",
                       "Ethnicity", "Q59.a", "Sexuality", "Q60.a", "Gender", "Q61.a", 
                       "GenderAssignedAtBirth", "Carer", "FirstGen", "Dis2020", "Q66", 
                       "Dis.2", "Q68")

#Where empty row replace with NA
cleandat2020<-cleandat2020 %>%
  mutate_at(vars(colnames(.)),
            .funs = funs(ifelse(.=="", NA, as.character(.))))

#Where "empty row"Not applicable" replace with NA
cleandat2020<-cleandat2020 %>%
  mutate_at(vars(colnames(.)),
            .funs = funs(ifelse(.=="Not applicable", NA, as.character(.))))

#Remove rows where Q1 is "No" (question for informed consent)
cleandat2020<-cleandat2020[!(cleandat2020$Q1=="No"),]

#Sexuality (bi, gay man, gay woman were all combined into other category due to small number of observations)
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Prefer not to say")]=NA
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Bi")]="Bisexual"
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Gay Man")]="Gay or Lesbian"
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Gay Woman/ Lesbian")]="Gay or Lesbian"
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Heterosexual/ Straight")]="Straight / Heterosexual"
cleandat2020$Sexuality[(cleandat2020$Sexuality == "Other")]=NA

cleandat2020$Dis.2 <- gsub("Prefer not to say","No", cleandat2020$Dis.2)
cleandat2020$Dis.2[cleandat2020$Dis.2==""] <- "No"


#Converting all the data into factors
cleandat2020a_factor <- cleandat2020 %>% 
  mutate_if(is.character,as.factor)%>%
  mutate_if(is.numeric,as.factor)
str(cleandat2020a_factor)


#Q2.a is answered if Q2 was other. Following step is to clean the data where other category was specified by each respondent uniquely
unique(cleandat2020a_factor$Q2.a)

#Q2.a clean the quotes observed
cleandat2020a_factor$Q2.a<-gsub("'", "", cleandat2020a_factor$Q2.a)
cleandat2020a_factor$Q2.a <- gsub("\"", "", cleandat2020a_factor$Q2.a)

#Q.2.a_redefined variable is created to define which respondents are academic, other or student (-we do this since some people selected other and defined what they are but actually they still belong to academic, student or other)

cleandat2020a_factor$Q2.a_redefined<-NA

cleandat2020a_factor$Q2.a_redefined[(cleandat2020a_factor$Q2.a == "Professional Services who used to have Research in JD")|
                                  (cleandat2020a_factor$Q2.a == "participant, bystander?") |
                                  (cleandat2020a_factor$Q2.a == "Infrastructure") |
                                  (cleandat2020a_factor$Q2.a == "technical services manager") |
                                  (cleandat2020a_factor$Q2.a == "technician ") |
                                  (cleandat2020a_factor$Q2.a == "Stores manager")|
                                  (cleandat2020a_factor$Q2.a == "Technician")|
                                  (cleandat2020a_factor$Q2.a == "Research Support Staff")|
                                  (cleandat2020a_factor$Q2.a == "Professional Services")|
                                  (cleandat2020a_factor$Q2.a == "support scientist, specialist services shared facility")|
                                  (cleandat2020a_factor$Q2.a == "Research administrator")|
                                  (cleandat2020a_factor$Q2.a == "Lab Manager")|
                                  (cleandat2020a_factor$Q2.a == "lab manager")|
                                  (cleandat2020a_factor$Q2.a == "Research support ERO team")|
                                  (cleandat2020a_factor$Q2.a == "Research Support")|
                                  (cleandat2020a_factor$Q2.a == "technician team managing research facilities")|
                                  (cleandat2020a_factor$Q2.a =="support scientist, specialist services shared facility")| 
                                  (cleandat2020a_factor$Q2.a == "Senior Clinical Trial Manager")| 
                                  (cleandat2020a_factor$Q2.a == "Professional Services")| 
                                  (cleandat2020a_factor$Q2.a == "Professional Services/PE Impact ")| 
                                  (cleandat2020a_factor$Q2.a == "Project Manager - is that Research Professional?")| 
                                  (cleandat2020a_factor$Q2.a == "Project Manager")|
                                  (cleandat2020a_factor$Q2.a == "Facility Manager")| 
                                  (cleandat2020a_factor$Q2.a == "administration")| 
                                  (cleandat2020a_factor$Q2.a == "Research Communication")| 
                                  (cleandat2020a_factor$Q2.a == "Public engagement professional")| 
                                  (cleandat2020a_factor$Q2.a == "Laboratory Manager")| 
                                  (cleandat2020a_factor$Q2.a == "Professional services manager")| 
                                  (cleandat2020a_factor$Q2.a == "Professional services manager")| 
                                  (cleandat2020a_factor$Q2.a == "Edinburgh Research Office")]= "Other"

cleandat2020a_factor$Q2.a_redefined[(cleandat2020a_factor$Q2.a == "Lecturer")| 
                                  (cleandat2020a_factor$Q2.a == "University Teacher") | 
                                  (cleandat2020a_factor$Q2.a == "Im a Chancellors Felllow ") | 
                                  (cleandat2020a_factor$Q2.a == "Teaching fellow, researching independently")| 
                                  (cleandat2020a_factor$Q2.a == "Senior Lecturer") | 
                                  (cleandat2020a_factor$Q2.a == "Research Fellow (beyond post-doc stage) ")  | 
                                  (cleandat2020a_factor$Q2.a == "Teacher") | 
                                  (cleandat2020a_factor$Q2.a == "Senior Lecturer ") | 
                                  (cleandat2020a_factor$Q2.a == "Research Fellow") | 
                                  (cleandat2020a_factor$Q2.a == "Lecturer with small proportion of time available for research (<5%)") | 
                                  (cleandat2020a_factor$Q2.a == "Teaching Fellow") | 
                                  (cleandat2020a_factor$Q2.a == "Researcher") | 
                                  (cleandat2020a_factor$Q2.a == "Sr. Lecturer") | 
                                  (cleandat2020a_factor$Q2.a == "Teaching and Research") |
                                  (cleandat2020a_factor$Q2.a == "Research Fellow ") | 
                                  (cleandat2020a_factor$Q2.a == "Lecturer (research-active)") | 
                                  (cleandat2020a_factor$Q2.a == "Senior lecturer who researches as part of post") | 
                                  (cleandat2020a_factor$Q2.a == "The list is WEIRD! I am a normal academic - and hence one of a large group of staff with research as part (!) of their role - so: Academic (Research and Teaching). This list is a bad start to this survey!!!!") | 
                                  (cleandat2020a_factor$Q2.a == "Lecturer with combined research and teaching contract") | 
                                  (cleandat2020a_factor$Q2.a == "senior lecturer") | 
                                  (cleandat2020a_factor$Q2.a == "Research Fellow") | 
                                  (cleandat2020a_factor$Q2.a == "Teaching focused lecturer") | 
                                  (cleandat2020a_factor$Q2.a == "Mid-Career Researcher")| 
                                  (cleandat2020a_factor$Q2.a == "Knowledge Exchange Officer and lecturer (separate contracts)") | 
                                  (cleandat2020a_factor$Q2.a == "Lecturer (40% research)") | 
                                  (cleandat2020a_factor$Q2.a == "Senior Lecturer (sorry, I thought none of the terms really conveyed that I do research and teaching)") | 
                                  (cleandat2020a_factor$Q2.a == "Reader") | 
                                  (cleandat2020a_factor$Q2.a == "academic with research responsibilities")| 
                                  (cleandat2020a_factor$Q2.a == "researcher on grade 9 contract")| 
                                  (cleandat2020a_factor$Q2.a == "Teacher-researcher")| 
                                  (cleandat2020a_factor$Q2.a == "Academic staff on research and teaching contract")| 
                                  (cleandat2020a_factor$Q2.a == "academic faculty? ")| 
                                  (cleandat2020a_factor$Q2.a == "Lecturer with no current research grants")| 
                                  (cleandat2020a_factor$Q2.a == "Independent Research Fellow")| 
                                  (cleandat2020a_factor$Q2.a == "Research is 30% of my academic post")| 
                                  (cleandat2020a_factor$Q2.a == "Tutor (teaching staff); unpaid researcher")| 
                                  (cleandat2020a_factor$Q2.a =="- the term current complicates matters as I have been a PI/CI on £millions of research council funding in multuple countries but not at the present - so how should I describe my position in a way that helps you understand my perspective?") | 
                                  (cleandat2020a_factor$Q2.a == "Lecturer, PI, Co-I")| 
                                  (cleandat2020a_factor$Q2.a == " just a regular academic") | 
                                  (cleandat2020a_factor$Q2.a == "Clinician/ lecturer in Veterinary Oncology with clinical and laboratory research access. ")| 
                                  (cleandat2020a_factor$Q2.a =="Senior Lecturer (you might have thought to put lecturers into your list rather than just professors or PIs, or do you really feel that lecturers who are not PIs do not form a part of the research community at UoE?!)") | (cleandat2020a_factor$Q2.a == "Regular academic contract but not technically a professor")| 
                                  (cleandat2020a_factor$Q2.a == "Academic ")| 
                                  (cleandat2020a_factor$Q2.a == "Knowledge Exchange Officer and lecturer (separate contracts")| 
                                  (cleandat2020a_factor$Q2.a == "Lecturer with Research in contract, and Co-I")]= "Academic"


cleandat2020a_factor$Q2.a_redefined[(cleandat2020a_factor$Q2.a == "Recent PhD graduate (July 2020)") | 
                                  (cleandat2020a_factor$Q2.a == "MSc Student ") | 
                                  (cleandat2020a_factor$Q2.a == "Part time PhD student, part time study coordinator") | 
                                  (cleandat2020a_factor$Q2.a == "Research Masters Student") | 
                                  (cleandat2020a_factor$Q2.a == "Masters by Research Student ") | 
                                  (cleandat2020a_factor$Q2.a == "Clinical Research Fellow and PhD student") | 
                                  (cleandat2020a_factor$Q2.a == "Masters Research Student") | 
                                  (cleandat2020a_factor$Q2.a == "PGR Student") | 
                                  (cleandat2020a_factor$Q2.a == "MScR student") | 
                                  (cleandat2020a_factor$Q2.a == "MSc by Research Student") | 
                                  (cleandat2020a_factor$Q2.a == "Post graduate student") | 
                                  (cleandat2020a_factor$Q2.a == "Masters by Research Student") | 
                                  (cleandat2020a_factor$Q2.a == "Part time PhD student, part time study coordinator") | 
                                  (cleandat2020a_factor$Q2.a == "RA & PhD student (Sept 2020)") | 
                                  (cleandat2020a_factor$Q2.a == "Master of research") |
                                  (cleandat2020a_factor$Q2.a == "Masters by Research Student") | 
                                  (cleandat2020a_factor$Q2.a == "Master by Research Student")|
                                  (cleandat2020a_factor$Q2.a == "MscR Student") | 
                                  (cleandat2020a_factor$Q2.a == "MScR researcher") | 
                                  (cleandat2020a_factor$Q2.a == "Post-grad student")| 
                                  (cleandat2020a_factor$Q2.a == "Clinical lecturer (80% clinical 20% academic time)")]="Student"

#Variable with Student, Other and Academic
cleandat2020a_factor$Academic[(cleandat2020a_factor$Q2== "Other")| (cleandat2020a_factor$Q2 =="Research Professional")  |(cleandat2020a_factor$Q2.a_redefined =="Other")]="Other"

cleandat2020a_factor$Academic[(cleandat2020a_factor$Q2== "Research Technician")|(cleandat2020a_factor$Q2== "Post doctoral Researcher") | (cleandat2020a_factor$Q2 == "Principal Investigator")| (cleandat2020a_factor$Q2 == "Professor")| (cleandat2020a_factor$Q2.a_redefined =="Academic")]="Academic"

cleandat2020a_factor$Academic[(cleandat2020a_factor$Q2.a_redefined == "Student") | (cleandat2020a_factor$Q2 =="PhD Student")]="Student"

cleandat2020a_factor$Academic<-as.factor(cleandat2020a_factor$Academic)

####Data Cleaning for Disability, Gender Assigned at Birth, Gender, Sexuality, Carer, FirstGen, Ethnicity based on the data

#Disability
cleandat2020a_factor$Dis2020[(cleandat2020a_factor$Dis2020 == "Prefer not to say")]=NA
cleandat2020a_factor$Dis2020 <- droplevels(cleandat2020a_factor$Dis2020)

#GenderAssignedAtBirth (cannot look at this factor as there are only a few observation in No category)
cleandat2020a_factor$GenderAssignedAtBirth[(cleandat2020a_factor$GenderAssignedAtBirth == "Prefer not to say")]=NA
cleandat2020a_factor$GenderAssignedAtBirth <- droplevels(cleandat2020a_factor$GenderAssignedAtBirth)

#Gender (dropped nonbinary and other as there were not enough observations)
cleandat2020a_factor$Gender[(cleandat2020a_factor$Gender == "Prefer not to say")]=NA
cleandat2020a_factor$Gender[(cleandat2020a_factor$Gender == "Non binary")]="Other"
cleandat2020a_factor$Gender[(cleandat2020a_factor$Gender == "Other")]=NA
cleandat2020a_factor$Gender <- droplevels(cleandat2020a_factor$Gender)


#Carer (all different yes options combined into "yes" due to small number of observations in subcategories)
cleandat2020a_factor$Carer[(cleandat2020a_factor$Carer == "Prefer not to say")]=NA
cleandat2020a_factor$Carer<-revalue(cleandat2020a_factor$Carer, c("Yes - equal carer"="Yes", "Yes - main carer"="Yes","Yes - secondary carer (someone else is the main carer)"="Yes" ))
cleandat2020a_factor$Carer <- droplevels(cleandat2020a_factor$Carer)

#FirstGen (all different yes options combined into "yes" due to small number of observations in subcategories)
cleandat2020a_factor$FirstGen[(cleandat2020a_factor$FirstGen == "Prefer not to say")]=NA
cleandat2020a_factor$FirstGen[(cleandat2020a_factor$FirstGen == "Don't know")]=NA
cleandat2020a_factor$FirstGen <- droplevels(cleandat2020a_factor$FirstGen)

#Ethinicity (recategorised due to the small number of observations in subcategories)-- new covariate:EthnicityCleaned
cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(EthnicityCleaned = ifelse(grepl("white", Q59.a), "White non British", paste(Ethnicity))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("White", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("whie", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("while", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("withe", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("caucasian", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(grepl("Caucasian", Q59.a), "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "White Scottish", "White British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "White Irish", "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "White Polish", "White non British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "Prefer not to say", "No response", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "White non British British", "White British", paste(EthnicityCleaned))) %>%
  mutate(EthnicityCleaned = ifelse(EthnicityCleaned %in% c("White British", "White non British", "No response", NA), paste(EthnicityCleaned), "BAME")) %>%
  mutate(EthnicityCleaned = ifelse(Ethnicity == "NA", "No response", paste(EthnicityCleaned)))

#Creating the Workhourdiff for difference between Q10 and Q11 
cleandat2020a_factor$Q10_newcat<-cleandat2020a_factor$Q10
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("Less than 10 hours"="Less than 30 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("More than 80 hours"="More than 40 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("10-20 hours"="Less than 30 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("20-30 hours"="Less than 30 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("30-40 hours"="30-40 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("40-50 hours"="More than 40 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("50-60 hours"="More than 40 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("60-70 hours"="More than 40 hours"))
cleandat2020a_factor$Q10_newcat <- revalue(cleandat2020a_factor$Q10_newcat, c("70-80 hours"="More than 40 hours"))


cleandat2020a_factor$Q11_newcat<-cleandat2020a_factor$Q11
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("Less than 10 hours"="Less than 30 hours"))
#cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("More than 80 hours"="More than 40 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("10-20 hours"="Less than 30 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("20-30 hours"="Less than 30 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("30-40 hours"="30-40 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("40-50 hours"="More than 40 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("50-60 hours"="More than 40 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("60-70 hours"="More than 40 hours"))
cleandat2020a_factor$Q11_newcat <- revalue(cleandat2020a_factor$Q11_newcat, c("70-80 hours"="More than 40 hours"))
cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "Less than 30 hours" &
                                 Q11_newcat %in% c("More than 40 hours","30-40 hours"), 
                               "Works less", ""))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30 - 40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works less", paste(WorkHourDiff)))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works less",  paste(WorkHourDiff)))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("Less than 30 hours"), 
                               "Works more",  paste(WorkHourDiff)))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "More than 40 hours" &
                                 Q11_newcat %in% c("Less than 30 hours", "30-40 hours"), 
                               "Works more",  paste(WorkHourDiff)))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "More than 40 hours" &
                                 Q11_newcat %in% c("More than 40 hours"), 
                               "Works the same",  paste(WorkHourDiff)))
cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "30-40 hours" &
                                 Q11_newcat %in% c("30-40 hours"), 
                               "Works the same",  paste(WorkHourDiff)))

cleandat2020a_factor <- cleandat2020a_factor %>%
  mutate(WorkHourDiff = ifelse(Q10_newcat == "Less than 30 hours" &
                                 Q11_newcat %in% c("Less than 30 hours"), 
                               "Works the same",  paste(WorkHourDiff)))

cleandat2020a_factor$GenderAssignedAtBirth <- gsub("Yes", "temp", cleandat2020a_factor$GenderAssignedAtBirth)
cleandat2020a_factor$GenderAssignedAtBirth <- gsub("No", "Yes", cleandat2020a_factor$GenderAssignedAtBirth)
cleandat2020a_factor$GenderAssignedAtBirth <- gsub("temp", "No", cleandat2020a_factor$GenderAssignedAtBirth)

cleandat2020a_factor$WorkHourDiff[(cleandat2020a_factor$WorkHourDiff == "")]=NA
cleandat2020a_factor$WorkHourDiff[(cleandat2020a_factor$WorkHourDiff == "NA")]=NA
cleandat2020a_factor$WorkHourDiff<-as.factor(cleandat2020a_factor$WorkHourDiff)
cleandat2020a_factor$WorkHourDiff <- droplevels(cleandat2020a_factor$WorkHourDiff)


####Separation of the Edinburgh Survey dataset according to Academic, Other and Student

EdinOtherData<-cleandat2020a_factor%>%
  filter( Q2== "Research Professional" | Q2.a_redefined =="Other")

EdinAcademicData <- cleandat2020a_factor %>%
  filter( Q2== "Post doctoral Researcher" | Q2 == "Principal Investigator"| Q2 == "Professor"| Q2 =="Research Technician"| Q2.a_redefined =="Academic")

EdinStudentData <- cleandat2020a_factor %>%
  filter(Q2.a_redefined == "Student" | Q2 =="PhD Student")

# Applicable to UK Student + Employed Academics
EdinStudent_AcademicData <- rbind(EdinAcademicData, EdinStudentData)
EdinOther_AcademicData <- rbind(EdinAcademicData, EdinOtherData)

####combined set for 2020 v 2022
cleandata_factor$surv <- "2022"
cleandat2020a_factor$surv <- "2020"
InBoth = intersect(colnames(cleandata_factor), colnames(cleandat2020a_factor))
Comb_dat=rbind(cleandata_factor[,InBoth], cleandat2020a_factor[,InBoth])

Comb_dat$Q3[(Comb_dat$Q3 == "CAHSS,CMVM")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CAHSS,CMVM,CSE")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CAHSS,CSE")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CMVM,CSE")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CAHSS,Research Professional Staff")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CSE,Other")]="Other"
Comb_dat$Q3[(Comb_dat$Q3 == "CSE,Research Professional Staff")]="Other"


Comb_dat_CMVM <- subset(Comb_dat, Comb_dat$Q3 == "CMVM")
Comb_dat_CSE <- subset(Comb_dat, Comb_dat$Q3 == "CSE")
Comb_dat_CAHSS <- subset(Comb_dat, Comb_dat$Q3 == "CAHSS")

