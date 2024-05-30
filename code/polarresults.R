library(nFactors)
library(ggpubr)
library(FactoMineR)
library(readr)
library(psych)

#change column names
dat<- read.csv('AnalysisSet.csv', header = T, check.names = F) 
cleandat <- dat
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
                       "Q44.6.a", "Q44.7.a", "Q45", "Q46", "Q47", "Q47.1", "Q48", "Q48.1.a", 
                       "Q48.2.a", "Q49", "Q50", "Q50.1.a", "Q50.2.a", "Q50.3.a", "Q50.4.a", 
                       "Q50.5.a", "Q50.6.a", "Q50.7.a", "Q51", "Q51.1", "Q51.2", "Q51.3", 
                       "Q52", "Q52.1.a", "Q52.2.a", "Q53", "Q54", "Q54.1", "Q54.2", "Q54.3", 
                       "Q54.4", "Q54.5", "Q54.6", "Q55", "Q56", "Q56.1.a", "Q56.2.a", "Q56.3.a", 
                       "Q56.4.a", "Q56.5.a", "Q56.6.a", "Q56.7.a", "Q56.8.a", "Q57", "Q58",
                       "Ethnicity", "Q59.a", "Sexuality", "Q60.a", "Gender", "Q61.a", 
                       "GenderAssignedAtBirth", "Carer", "FirstGen", "Disability", "Q66", 
                       "Q67", "Q68")
verystronglyagree_positive <- cleandat[c(1,22:24,26,28:30,47:50,54,56,57,59,62,64:67,70:80,87:91,97,99,102,103,105,106,107,128:131,135,137:141,148,149,151,153:155,159:161)]
verystronglyagree_negative <- cleandat[c(1,25,51,52,53,55,58,63,68,98,100,101,104,132:134,136,142,147,150,156,157,158,162)]
important_positive <- cleandat[c(1,32:35)]
successful_positive <- cleandat[c(1,37:40,42:45)]
satisfied_positive <- cleandat[c(1,81)]
positive_positive <- cleandat[c(1,93:95)]
yes_no_positive <- cleandat[c(1,109,112,115,118)]
yes_no_unsure_positive <- cleandat[c(1,121:123)]
polar38.1 <- cleandat[c(1,144)]

verystronglyagree_positive[verystronglyagree_positive == "Very strongly agree"] <- 7
verystronglyagree_positive[verystronglyagree_positive == "Strongly agree"] <- 6
verystronglyagree_positive[verystronglyagree_positive == "Agree"] <- 5
verystronglyagree_positive[verystronglyagree_positive == "Neither agree nor disagree"] <- 4
verystronglyagree_positive[verystronglyagree_positive == "Disagree"] <- 3
verystronglyagree_positive[verystronglyagree_positive == "Strongly disagree"] <- 2
verystronglyagree_positive[verystronglyagree_positive == "Very strongly disagree"] <- 1
verystronglyagree_positive[verystronglyagree_positive == "Not applicable"] <- NA

verystronglyagree_negative[verystronglyagree_negative == "Very strongly agree"] <- 1
verystronglyagree_negative[verystronglyagree_negative == "Strongly agree"] <- 2
verystronglyagree_negative[verystronglyagree_negative == "Agree"] <- 3
verystronglyagree_negative[verystronglyagree_negative == "Neither agree nor disagree"] <- 4
verystronglyagree_negative[verystronglyagree_negative == "Disagree"] <- 5
verystronglyagree_negative[verystronglyagree_negative == "Strongly disagree"] <- 6
verystronglyagree_negative[verystronglyagree_negative == "Very strongly disagree"] <- 7
verystronglyagree_negative[verystronglyagree_negative == "Not applicable"] <- NA

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

polarset <- cbind(verystronglyagree_negative,verystronglyagree_positive, important_positive, successful_positive, satisfied_positive, positive_positive, yes_no_positive, yes_no_unsure_positive, polar38.1)

polarset[polarset == ""] <- NA
polarset <- polarset[,-25]
polarset <- polarset[,-87]
polarset <- polarset[,-91]
polarset <- polarset[,-99]
polarset <- polarset[,-100]
polarset <- polarset[,-103]
polarset <- polarset[,-107]
polarset <- polarset[,-110]

for(i in 1:ncol(polarset)){
  polarset[,i] <- as.numeric(polarset[,i], na.rm = TRUE)
  }

for(i in 2:ncol(polarset)){
  polarset[is.na(polarset[,i]), i] <- median(polarset[,i], na.rm = TRUE)
}

dataFA<-polarmice5[,2:107]
PCA_dat<-princomp(na.omit(dataFA))
summary(PCA_dat) 
plot(PCA_dat)
screeplot(PCA_dat, type="line", main="Scree plot")

FA1<-factanal(na.omit(dataFA), factors=62, rotation="varimax") 
FA1

FA2<-factanal(na.omit(dataFA), factors=36, rotation="varimax", scores="regression") 
FA2
head(FA2$scores)

library(polycor)               # for hetcor()
pc <- hetcor(dataFA, ML=TRUE)   # polychoric corr matrix

ev <- eigen(pc) # get eigenvalues
ap <- parallel(subject=nrow(dataFA),var=ncol(dataFA),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

ordNum <- data.matrix(dataFA) 


faPC <- fa(r=pc$correlations, nfactors=15, n.obs=nrow(dataFA), rotate="varimax", cor="poly")
faPC$loadings

write.csv(faPC$loadings, "naomitload.csv")
write.csv(faPC$uniquenesses, "naomitunique.csv")
write.csv(faPC$score.cor, "naomitcor.csv")
