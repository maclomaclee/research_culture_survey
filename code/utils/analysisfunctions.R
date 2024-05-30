###
#title: "Edinburgh Research Culture Project-AnalysisFunctions"
#author: "Ezgi Tanriver-Ayder"
#updated for 2022 survey by malcolm.macleod@ed.ac.uk
#date: "09/07/2020"
###

library(plyr)
library(brant)
library(stringr)
library(dplyr)
library(tidyr)
library(ellipsis)

# Data Preparation for analysis ----------------------------------------------

analysisPrep <- function(outcome, cov, data) {
  attach(data)
  outcome[outcome =='NULL']<-NA    #Convert the NULL data into NAs so that it is not assessed as a category in answers
  outcome = droplevels(outcome)
  summary<-summary(outcome)
  summary <- summary[names(summary) != "NA's"]
  conTable <- xtabs(~outcome + cov, data = data)
  ref<-names(summary)[which.max(summary)]
  data$outcome<-relevel(outcome, ref=ref)
  result <- list(summary = summary, contTable = conTable, ref =  ref)
  return(result)
}

#Data Cleaning for multi-nominal outcomes ----------------------------------------------

##### Data cleaning process for multiple answered questions. When the respondents select multiple options (separated by ";" in the data) 
##### then separate that observation into multiple rows and count it as a unique answer. 

multidatClean<-function(outcome, cov, id, data){

      dat_long<-data %>%
      dplyr::select({{outcome}}, {{cov}}, {{id}}) %>%
      separate_rows({{outcome}}, sep = ",", convert = TRUE) %>%
      separate_rows({{cov}}, sep = ",", convert = TRUE) %>%
      separate_rows({{id}}, sep = ",", convert = TRUE) 
  dat_long<-data.frame(dat_long)
  attach(dat_long)
  dat_long[,1]<-as.factor(dat_long[,1])
  dat_long <- na.omit(dat_long)
  return(dat_long)
}


multiQuestion <- function(data) {
  data$ID<-rownames(data)
  data$new<-data[,1]
  data <- spread(data, new, names(data)[1])
  #data<-data[,1:18]
  data[] <- lapply(data, as.character)
  for(i in 3:ncol(data)) {
    data[,i] <- str_replace(data[,i], names(data)[i], "Agree")
    data[,i] <- replace_na(data[,i], "Disagree")
  }
  return(data)
}

#Data Cleaning for ordinal outcomes ----------------------------------------------

 # Very strongly agree-7   
 # Strongly agree-6
 # Agree-5
 # Neither agree nor disagree-4
 # Disagree-3                       
 # Strongly disagree-2               
 # Very strongly disagree-1
 # Not applicable-0

ordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
  case_when(x %in% c("Very strongly agree") ~7,
            x %in% c("Strongly agree") ~6,
            x %in% c("Agree") ~5,
            x %in% c("Neither agree nor disagree") ~4,
            x %in% c("Disagree") ~3,
            x %in% c("Strongly disagree") ~2,
            x %in% c("Very strongly disagree") ~1,
            x %in% c("Not applicable") ~0)
  }
CatOutcome <- as.factor(sapply(outcome, factorise))
CatOutcome <- cbind(CatOutcome,data)
CatOutcome <- CatOutcome[CatOutcome$CatOutcome!= "0", , drop=FALSE]
drops <- c("0","Not applicable")
CatOutcome <-CatOutcome[ , !(names(CatOutcome) %in% drops)]
CatOutcome[,1]<- factor(CatOutcome$CatOutcome)
CatOutcome<-na.omit(CatOutcome) 
return(CatOutcome)
}

ordinaldatCleanbin<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("No") ~2,
              x %in% c("Yes") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<- CatOutcome[CatOutcome$CatOutcome!= "0", , drop=FALSE]
  drops <- c("0","Not applicable")
  CatOutcome <-CatOutcome[ , !(names(CatOutcome) %in% drops)]
  CatOutcome[,1]<- factor(CatOutcome$CatOutcome)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

ordinaldatCleanbinTF<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c(FALSE) ~2,
              x %in% c(TRUE) ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<- CatOutcome[CatOutcome$CatOutcome!= "0", , drop=FALSE]
  drops <- c("0","Not applicable")
  CatOutcome <-CatOutcome[ , !(names(CatOutcome) %in% drops)]
  CatOutcome[,1]<- factor(CatOutcome$CatOutcome)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

ordinaldatCleanNegative<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Very strongly agree") ~1,
              x %in% c("Strongly agree") ~2,
              x %in% c("Agree") ~3,
              x %in% c("Neither agree nor disagree") ~4,
              x %in% c("Disagree") ~5,
              x %in% c("Strongly disagree") ~6,
              x %in% c("Very strongly disagree") ~7,
              x %in% c("Not applicable") ~0)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<- CatOutcome[CatOutcome$CatOutcome!= "0", , drop=FALSE]
  drops <- c("0","Not applicable")
  CatOutcome <-CatOutcome[ , !(names(CatOutcome) %in% drops)]
  CatOutcome[,1]<- factor(CatOutcome$CatOutcome)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

# Extremely Important-5   
# Somewhat Important-4
# Neutral-3
# Somewhat Unimportant-2
# Extremely Unimportant-1 

ImpordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Extremely Important") ~5,
              x %in% c("Somewhat Important") ~4,
              x %in% c("Neutral") ~3,
              x %in% c("Somewhat Unimportant") ~2,
              x %in% c("Extremely Unimportant") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}





SucordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Extremely successful") ~5,
              x %in% c("Somewhat successful") ~4,
              x %in% c("Neutral") ~3,
              x %in% c("Somewhat unsuccessful") ~2,
              x %in% c("Extremely unsuccessful") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

UnsureordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Yes") ~3,
              x %in% c("Unsure") ~2,
              x %in% c("No") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

NotsureordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Yes") ~3,
              x %in% c("Not sure") ~2,
              x %in% c("No") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

SatisfacordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Extremely satisfied") ~7,
              x %in% c("Quite satisfied") ~6,
              x %in% c("Satisfied") ~5,
              x %in% c("Neither satisfied nor unsatisfied") ~4,
              x %in% c("Unsatisfied") ~3,
              x %in% c("Quite unsatisfied") ~2,
              x %in% c("Extremely unsatisfied") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}


PositordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Very positive") ~5,
              x %in% c("Positive ") ~4,
              x %in% c("Neutral") ~3,
              x %in% c("Negative") ~2,
              x %in% c("Very Negative") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

ImpactordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Very high positive impact") ~7,
              x %in% c("High positive impact") ~6,
              x %in% c("Positive impact ") ~5,
              x %in% c("Neutral") ~4,
              x %in% c("Some negative impact") ~3,
              x %in% c("High negative impact") ~2,
              x %in% c("Very high negative impact") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}


ResponsibilityordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("High responsibility") ~3,
              x %in% c("Medium responsibility") ~2,
              x %in% c("Low responsibility") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}


PriorityordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("1 [highest priority]") ~3,
              x %in% c("2") ~2,
              x %in% c("3 [lowest priority]") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}


# Performs much better-7   
# Performs quite a bit better -6
# Performs a little better-5
# Performs the same-4
# Performs a little worse-3  
# Performs quite a bit worse-2
# Performs much worse-1  

PerformordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Performs much better") ~7,
              x %in% c("Performs quite a bit better") ~6,
              x %in% c("Performs a little better") ~5,
              x %in% c("Performs the same") ~4,
              x %in% c("Performs a little worse") ~3,
              x %in% c("Performs quite a bit worse") ~2,
              x %in% c("Performs much worse") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

StressedordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("7") ~7,
              x %in% c("6") ~6,
              x %in% c("5") ~5,
              x %in% c("4") ~4,
              x %in% c("3") ~3,
              x %in% c("2") ~2,
              x %in% c("1") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

WorkflatordinaldatClean<-function(outcome,data){
factorise <- function(x) {
    case_when(x %in% c("I feel overstretched and it can be unmanageable") ~1,
              x %in% c("I feel stretched but it's manageable") ~2,
              x %in% c("My workload is appropriately manageable") ~3,
              x %in% c("I don't have enough to do") ~4)
}
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

WorksensordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("I feel overstretched and it can be unmanageable") ~1,
              x %in% c("I feel stretched but it's manageable") ~2,
              x %in% c("My workload is appropriately manageable") ~3,
              x %in% c("I don't have enough to do") ~2)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

# Very high positive impact-7   
# High positive impact-6
# Some positive impact-5
# Neutral-4
# Some negative impact-3
# High negative impact-2 
# Very high negative impact-1

ImpactordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("Very high positive impact") ~7,
              x %in% c("High positive impact") ~6,
              x %in% c("Some positive impact") ~5,
              x %in% c("Neutral") ~4,
              x %in% c("Some negative impact") ~3,
              x %in% c("High negative impact") ~2,
              x %in% c("Very high negative impact") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

HelpordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("a lot more helpful than usual") ~5,
              x %in% c("more helpful than usual") ~4,
              x %in% c("as helpful as usual") ~3,
              x %in% c("less helpful than usual") ~2,
              x %in% c("a lot less helpful than usual") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}

LikeordinaldatClean<-function(outcome,data){
  factorise <- function(x) {
    case_when(x %in% c("I would like this a lot") ~5,
              x %in% c("I would like this") ~4,
              x %in% c("I'm neutral") ~3,
              x %in% c("I wouldn't like this") ~2,
              x %in% c("I really wouldn't like this") ~1)
  }
  CatOutcome<-as.factor(sapply(outcome, factorise))
  CatOutcome<-cbind(CatOutcome,data)
  CatOutcome<-na.omit(CatOutcome) 
  return(CatOutcome)
}



# Analysis pipeline for ordinal(1-7) outcomes ----------------------------------------------

ordinal <- function(outcome, cov, data) {
  # attach(data)
  prep <- analysisPrep(outcome, cov, data)
  analysis <- polr(outcome ~ cov, data=data, Hess=TRUE)
  assumption<-brant(analysis)
  analysisSummary <- summary(analysis)
  ctable <- coef(summary(analysis))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  ci <- confint(analysis) # default method gives profiled CIs
  confint.default(analysis) # CIs assuming normality
  exp(coef(analysis))
  OR <- exp(cbind(OR = coef(analysis), ci))
  
  result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
  
  return(result)
}


ordinal55 <- function(outcome, cov, data) {
  # attach(data)
  prep <- analysisPrep(outcome, cov, data)
  analysis <- polr(outcome ~ cov, data=data, Hess=TRUE)
  #assumption<-brant(analysis)
  analysisSummary <- summary(analysis)
  ctable <- coef(summary(analysis))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  ci <- confint(analysis) # default method gives profiled CIs
  confint.default(analysis) # CIs assuming normality
  exp(coef(analysis))
  OR <- exp(cbind(OR = coef(analysis), ci))
  
  result <- list(prep=prep, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
  
  return(result)
}

# Analysis pipeline for nominal(categorical) outcomes ----------------------------------------------

nominal <- function(outcome, cov, data) {
  attach(data)
  prep <- analysisPrep(outcome, cov, data)
  outcome <- relevel(outcome, ref = prep$ref)
  analysis<-multinom(outcome ~ cov, data=data)
  analysisSummary<-summary(analysis)
  analysisExp<-exp(coef(analysis))
  z <- summary(analysis)$coefficients/summary(analysis)$standard.errors
  p<- (1 - pnorm(abs(z), 0, 1)) * 2
  
  result<-list(prep=prep, analysisSummary = analysisSummary, analysisExp = analysisExp, analysisTest = z, analysispval = p)
  
  return(result)
}

graph_prep <- function (OR_outcomes) {
prepOR_outcomes <- data.frame(matrix(1.00, nrow=30, ncol=6))
colnames(prepOR_outcomes) <- (c("Question", "Dimension", "Item", "OR", "l95", "u95"))
prepOR_outcomes[,1] <- OR_Outcomes[1,1]
prepOR_outcomes[1:3,2] <- "Status"                              
prepOR_outcomes[4:7,2] <- "College"
prepOR_outcomes[8:9,2] <- "Caring Responsibility"
prepOR_outcomes[10:11,2] <- "Disability"
prepOR_outcomes[12:15,2] <- "Ethnicity"
prepOR_outcomes[16:19,2] <- "Race"
prepOR_outcomes[20:21,2] <- "First generation to go to University"
prepOR_outcomes[22:23,2] <- "Gender"
prepOR_outcomes[24:26,2] <- "Sexuality"
prepOR_outcomes[27:28,2] <- "Gender Assigned at Birth"
prepOR_outcomes[29:30,2] <- "Survey Year"
prepOR_outcomes[1,3] <- "Academic"
prepOR_outcomes[2,3] <- "Other role"
prepOR_outcomes[3,3] <- "Student"
prepOR_outcomes[4,3] <- "CAHSS"
prepOR_outcomes[5,3] <- "CMVM"
prepOR_outcomes[6,3] <- "CSE"
prepOR_outcomes[7,3] <- "No affiliation"
prepOR_outcomes[8,3] <- "No caring responsibility"
prepOR_outcomes[9,3] <- "Caring responsibility"
prepOR_outcomes[10,3] <- "No disability"
prepOR_outcomes[11,3] <- "Disability"
prepOR_outcomes[12,3] <- "European"
prepOR_outcomes[13,3] <- "Asian"
prepOR_outcomes[14,3] <- "North American"
prepOR_outcomes[15,3] <- "Other"
prepOR_outcomes[16,3] <- "White"
prepOR_outcomes[17,3] <- "Asian or Pacific Islander"
prepOR_outcomes[18,3] <- "Black, Hispanic or Latino/a/x"
prepOR_outcomes[19,3] <- "Prefer not to disclose"
prepOR_outcomes[20,3] <- "Not first generation"
prepOR_outcomes[21,3] <- "First generation"
prepOR_outcomes[22,3] <- "Female"
prepOR_outcomes[23,3] <- "Male"
prepOR_outcomes[24,3] <- "Bisexual"
prepOR_outcomes[25,3] <- "Gay or Lesbian"
prepOR_outcomes[26,3] <- "Straight"
prepOR_outcomes[27,3] <- "Gender as assigned at birth"
prepOR_outcomes[28,3] <- "Gender not as assigned at birth"
prepOR_outcomes[29,3] <- "2020 survey"
prepOR_outcomes[30,3] <- "2022 survey"
prepOR_outcomes[1,4:6] <- c(1,1,1)
prepOR_outcomes[2,4:6] <- OR_Outcomes[1,3:5]
prepOR_outcomes[3,4:6] <- OR_Outcomes[2,3:5]
prepOR_outcomes[4,4:6] <- c(1,1,1)
prepOR_outcomes[5,4:6] <- OR_Outcomes[3,3:5]
prepOR_outcomes[6,4:6] <- OR_Outcomes[4,3:5]
prepOR_outcomes[7,4:6] <- OR_Outcomes[5,3:5]
prepOR_outcomes[8,4:6] <- c(1,1,1)
prepOR_outcomes[9,4:6] <- OR_Outcomes[6,3:5]
prepOR_outcomes[10,4:6] <- c(1,1,1)
prepOR_outcomes[11,4:6] <- OR_Outcomes[7,3:5]
prepOR_outcomes[12,4:6] <- c(1,1,1)
prepOR_outcomes[13,4:6] <- OR_Outcomes[8,3:5]
prepOR_outcomes[14,4:6] <- OR_Outcomes[9,3:5]
prepOR_outcomes[15,4:6] <- OR_Outcomes[10,3:5]
prepOR_outcomes[16,4:6] <- c(1,1,1)
prepOR_outcomes[17,4:6] <- OR_Outcomes[11,3:5]
prepOR_outcomes[18,4:6] <- OR_Outcomes[12,3:5]
prepOR_outcomes[19,4:6] <- OR_Outcomes[13,3:5]
prepOR_outcomes[20,4:6] <- c(1,1,1)
prepOR_outcomes[21,4:6] <- OR_Outcomes[14,3:5]
prepOR_outcomes[22,4:6] <- c(1,1,1)
prepOR_outcomes[23,4:6] <- OR_Outcomes[15,3:5]
prepOR_outcomes[24,4:6] <- c(1,1,1)
prepOR_outcomes[25,4:6] <- OR_Outcomes[16,3:5]
prepOR_outcomes[26,4:6] <- OR_Outcomes[17,3:5]
prepOR_outcomes[27,4:6] <- c(1,1,1)
prepOR_outcomes[28,4:6] <- OR_Outcomes[18,3:5]
prepOR_outcomes[29,4:6] <- c(1,1,1)
prepOR_outcomes[30,4:6] <- OR_Outcomes[19,3:5]

prepOR_outcomes$Dimension_f = factor(prepOR_outcomes$Dimension, levels=c("Status","College","Caring Responsibility", "Disability", 
                                                                         "Ethnicity", "Race", "First generation to go to University", 
                                                                         "Gender", "Sexuality","Gender Assigned at birth", "Year of Survey"))
prepOR_outcomes[27,7] <- factor("Gender Assigned at birth")
prepOR_outcomes[28,7] <- factor("Gender Assigned at birth")
prepOR_outcomes[29,7] <- factor("Year of Survey")
prepOR_outcomes[30,7] <- factor("Year of Survey")
prepOR_outcomes$Item_f = factor(prepOR_outcomes$Item, levels = c("Other role", "Student","Academic", "No affiliation", 
              "CMVM", "CSE", "CAHSS", "Caring responsibility", "No caring responsibility", "Disability", "No disability", 
              "Other","North American","Asian","European",
              "Prefer not to disclose","Black, Hispanic or Latino/a/x","Asian or Pacific Islander","White",
              "First generation", "Not first generation", "Male","Female","Straight","Gay or Lesbian","Bisexual",
              "Gender not as assigned at birth", "Gender as assigned at birth", "2022 survey","2020 survey"))


fig <- prepOR_outcomes%>%   group_by(Dimension)%>%
  ggplot(aes(x = Item_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio", limits=(c(0.1,10)))+
  expand_limits(y=c(0.1,10))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  coord_flip()+
  facet_wrap(~Dimension_f, strip.position="left", labeller = label_wrap_gen(width=20), nrow=11, scales = "free_y")+
  geom_rect(aes(fill = Dimension),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  geom_point(shape=1)+
  labs(title = question)+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y.left = element_text(hjust=0,vjust = 0.5,angle=0,face="bold", size=10),
        strip.placement = "outside")
return(fig)
}

graph_prep_binary <- function (OR_outcomes) {
  remove(prepOR_outcomes)
  prepOR_outcomes <- data.frame(matrix(1.00, nrow=20, ncol=6))
  colnames(prepOR_outcomes) <- (c("Question", "Dimension", "Item", "OR", "l95", "u95"))
  prepOR_outcomes[,1] <- OR_Outcomes[1,1]
  prepOR_outcomes[1:3,2] <- "Status"                              
  prepOR_outcomes[4:7,2] <- "College"
  prepOR_outcomes[8:9,2] <- "Caring Responsibility"
  prepOR_outcomes[10:11,2] <- "Disability"
  prepOR_outcomes[12:15,2] <- "Ethnicity"
  prepOR_outcomes[16:17,2] <- "First generation to go to University"
  prepOR_outcomes[18:19,2] <- "Gender"
  prepOR_outcomes[20:21,2] <- "Sexuality"
  prepOR_outcomes[1,3] <- "Academic"
  prepOR_outcomes[2,3] <- "Other role"
  prepOR_outcomes[3,3] <- "Student"
  prepOR_outcomes[4,3] <- "CAHSS"
  prepOR_outcomes[5,3] <- "CMVM"
  prepOR_outcomes[6,3] <- "CSE"
  prepOR_outcomes[7,3] <- "No affiliation"
  prepOR_outcomes[8,3] <- "No caring responsibility"
  prepOR_outcomes[9,3] <- "Caring responsibility"
  prepOR_outcomes[10,3] <- "No disability"
  prepOR_outcomes[11,3] <- "Disability"
  prepOR_outcomes[12,3] <- "BAME"
  prepOR_outcomes[13,3] <- "No response"
  prepOR_outcomes[14,3] <- "White British"
  prepOR_outcomes[15,3] <- "White non British"
  prepOR_outcomes[16,3] <- "Not first generation"
  prepOR_outcomes[17,3] <- "First generation"
  prepOR_outcomes[18,3] <- "Female"
  prepOR_outcomes[19,3] <- "Male"
  prepOR_outcomes[20,3] <- "Straight"
  prepOR_outcomes[21,3] <- "LGBTIQ"
  prepOR_outcomes[1,4:6] <- OR_Outcomes[1,3:5]
  prepOR_outcomes[2,4:6] <- OR_Outcomes[2,3:5]
  prepOR_outcomes[3,4:6] <- OR_Outcomes[3,3:5]
  prepOR_outcomes[4,4:6] <- OR_Outcomes[4,3:5]
  prepOR_outcomes[5,4:6] <- OR_Outcomes[5,3:5]
  prepOR_outcomes[6,4:6] <- OR_Outcomes[6,3:5]
  prepOR_outcomes[7,4:6] <- OR_Outcomes[7,3:5]
  prepOR_outcomes[8,4:6] <- OR_Outcomes[8,3:5]
  prepOR_outcomes[9,4:6] <- OR_Outcomes[9,3:5]
  prepOR_outcomes[10,4:6] <- OR_Outcomes[10,3:5]
  prepOR_outcomes[11,4:6] <- OR_Outcomes[11,3:5]
  prepOR_outcomes[12,4:6] <- OR_Outcomes[12,3:5]
  prepOR_outcomes[13,4:6] <- OR_Outcomes[13,3:5]
  prepOR_outcomes[14,4:6] <- OR_Outcomes[14,3:5]
  prepOR_outcomes[15,4:6] <- OR_Outcomes[15,3:5]
  prepOR_outcomes[16,4:6] <- OR_Outcomes[16,3:5]
  prepOR_outcomes[17,4:6] <- OR_Outcomes[17,3:5]
  prepOR_outcomes[18,4:6] <- OR_Outcomes[18,3:5]
  prepOR_outcomes[19,4:6] <- OR_Outcomes[19,3:5]
  prepOR_outcomes[20,4:6] <- OR_Outcomes[20,3:5]
  prepOR_outcomes[21,4:6] <- OR_Outcomes[21,3:5]
  
  prepOR_outcomes$Dimension_f = factor(prepOR_outcomes$Dimension, levels=c("Status","College","Caring Responsibility", "Disability", "Ethnicity", "First generation to go to University", "Gender", "Sexuality"))
  prepOR_outcomes$Item_f = factor(prepOR_outcomes$Item, levels = c("Other role", "Student","Academic", "No affiliation", "CMVM", "CSE", "CAHSS", "Caring responsibility", "No caring responsibility", "Disability", "No disability", "White non British","White British","No response","BAME", "First generation","Not first generation", "Male","Female","LGBTIQ","Straight"))
  prepOR_outcomes%>%   group_by(Dimension)%>%
    ggplot(aes(x = Item_f, y=OR, ymin=l95, ymax=u95))+
    scale_y_log10(name="Odds Ratio", limits=(c(0.1,10)))+
    expand_limits(y=c(0.1,10))+
    geom_pointrange()+
    geom_hline(yintercept =1, linetype=2)+
    geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
    theme_minimal()+
    facet_wrap(~Dimension_f, strip.position="left", labeller = label_wrap_gen(width=20), nrow=9, scales = "free_y")+
    geom_rect(aes(fill = Dimension),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.1) +
    coord_flip()+
    geom_point(shape=1)+
    labs(title = Question)+
    theme(plot.title=element_text(size=16,face="bold"),
          legend.position = "none",
          axis.text.y=element_text(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(face="bold", ),
          axis.title.x =element_text(size=10,face="bold"),
          axis.title.y = element_blank(),
          strip.text.y.left = element_text(hjust=0,vjust = 0.5,angle=0,face="bold", size=10),
          strip.placement = "outside")
  plotname <- chartr(prepOR_outcomes[1,1], old = ".", new = "_")
  plotname <- paste(plotname, ".jpg", sep = "")
  ggsave(plotname)
}




#ordinal(employment_wdata_factor$Q14..grid., employment_wdata_factor$UK_Academic, employment_wdata_factor )
#nominal(employment_wdata_factor$Q12..single., employment_wdata_factor$UK_Academic, employment_wdata_factor)

# run ll demographoc analyses for a questions
analyze_demographics <- function(data, demographics, response_column, unique_id = "UniqueResponseNumber") {
  results <- list()
  
  for(demo in demographics) {
    cat("Analyzing for demographic:", demo, "\n")
    
    # Ensure the demographic variable and response columns are factors
    data[[demo]] <- factor(data[[demo]])
    data[[response_column]] <- as.character(data[[response_column]])
    
    # Data preprocessing and cleaning
    temp_data <- multidatClean(response_column, demo, unique_id, data)
    
    # Creating contingency table
    con_table <- xtabs(~ get(response_column) + get(demo), data = temp_data)
    print(con_table)
    
    # Secondary cleaning, if specific to your processing needs
    cleaned_data <- ordinaldatClean(temp_data[[response_column]], temp_data)
    
    # Performing ordinal logistic regression
    model_result <- ordinal(cleaned_data$CatOutcome, cleaned_data[[demo]], cleaned_data)
    
    # Collecting results in a list to return
    results[[demo]] <- list(
      contingency_table = con_table,
      model = model_result
    )
  }
  
  return(results)
}



