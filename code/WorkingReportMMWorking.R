source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor
employ_dat$Q3 <- as.character(employ_dat$Q3)

### Q13 - To what extent do you agree or disagree with the following statements regarding the management of your work
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q13.3. My supervisor values negative results that don't meet an expected hypothesis"
"Status"
employ_datQ13.3.a<-multidatClean(Q13.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.3.a + Academic, data = employ_datQ13.3.a)
detach(dat_long)
employ_datQ13.3.a<-ordinaldatClean(employ_datQ13.3.a$Q13.3.a, employ_datQ13.3.a)
#ordinal(employ_datQ13.3.a$CatOutcome, employ_datQ13.3.a$Academic, employ_datQ13.3.a)
prep <- analysisPrep(employ_datQ13.3.a$CatOutcome, employ_datQ13.3.a$Academic, employ_datQ13.3.a)
analysis <- polr(employ_datQ13.3.a$CatOutcome ~ employ_datQ13.3.a$Academic, data=employ_datQ13.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ13.3.aCollege<-multidatClean(Q13.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.3.a + Q3, data = employ_datQ13.3.aCollege)
conTable
detach(dat_long)
employ_datQ13.3.aCollege$Q3[(employ_datQ13.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.3.aCollege$Q3<- factor(employ_datQ13.3.aCollege$Q3)
employ_datQ13.3.aCollege<-ordinaldatClean(employ_datQ13.3.aCollege$Q13.3.a, employ_datQ13.3.aCollege)
#ordinal(employ_datQ13.3.aCollege$CatOutcome, employ_datQ13.3.aCollege$Q3, employ_datQ13.3.aCollege)
prep <- analysisPrep(employ_datQ13.3.aCollege$CatOutcome, employ_datQ13.3.aCollege$Q3, employ_datQ13.3.aCollege)
analysis <- polr(employ_datQ13.3.aCollege$CatOutcome ~ employ_datQ13.3.aCollege$Q3, data=employ_datQ13.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ13.3.aCarer<-multidatClean(Q13.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aCarer$Carer<- factor(employ_datQ13.3.aCarer$Carer)
employ_datQ13.3.aCarer<-ordinaldatClean(employ_datQ13.3.aCarer$Q13.3.a, employ_datQ13.3.aCarer)
#ordinal(employ_datQ13.3.aCarer$CatOutcome, employ_datQ13.3.aCarer$Carer, employ_datQ13.3.aCarer)
prep <- analysisPrep(employ_datQ13.3.aCarer$CatOutcome, employ_datQ13.3.aCarer$Carer, employ_datQ13.3.aCarer)
analysis <- polr(employ_datQ13.3.aCarer$CatOutcome ~ employ_datQ13.3.aCarer$Carer, data=employ_datQ13.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ13.3.aDisability<-multidatClean(Q13.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aDisability$Disability<- factor(employ_datQ13.3.aDisability$Disability)
employ_datQ13.3.aDisability<-ordinaldatClean(employ_datQ13.3.aDisability$Q13.3.a, employ_datQ13.3.aDisability)
conTable <- xtabs(~Q13.3.a + Disability, data = employ_datQ13.3.aDisability)
#ordinal(employ_datQ13.3.aDisability$CatOutcome, employ_datQ13.3.aDisability$Disability, employ_datQ13.3.aDisability)
prep <- analysisPrep(employ_datQ13.3.aDisability$CatOutcome, employ_datQ13.3.aDisability$Disability, employ_datQ13.3.aDisability)
analysis <- polr(employ_datQ13.3.aDisability$CatOutcome ~ employ_datQ13.3.aDisability$Disability, data=employ_datQ13.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ13.3.aEthnicity<-multidatClean(Q13.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aEthnicity$Ethnicity<- factor(employ_datQ13.3.aEthnicity$EthnicityCleaned)
employ_datQ13.3.aEthnicity<-ordinaldatClean(employ_datQ13.3.aEthnicity$Q13.3.a, employ_datQ13.3.aEthnicity)
conTable <- xtabs(~Q13.3.a + EthnicityCleaned, data = employ_datQ13.3.aEthnicity)
conTable
#ordinal(employ_datQ13.3.aEthnicity$CatOutcome, employ_datQ13.3.aEthnicity$EthnicityCleaned, employ_datQ13.3.aEthnicity)
prep <- analysisPrep(employ_datQ13.3.aEthnicity$CatOutcome, employ_datQ13.3.aEthnicity$EthnicityCleaned, employ_datQ13.3.aEthnicity)
analysis <- polr(employ_datQ13.3.aEthnicity$CatOutcome ~ employ_datQ13.3.aEthnicity$EthnicityCleaned, data=employ_datQ13.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ13.3.aFirstGen<-multidatClean(Q13.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aFirstGen$FirstGen<-factor(employ_datQ13.3.aFirstGen$FirstGen)
employ_datQ13.3.aFirstGen<-ordinaldatClean(employ_datQ13.3.aFirstGen$Q13.3.a, employ_datQ13.3.aFirstGen)
#ordinal(employ_datQ13.3.aFirstGen$CatOutcome, employ_datQ13.3.aFirstGen$FirstGen, employ_datQ13.3.aFirstGen)
prep <- analysisPrep(employ_datQ13.3.aFirstGen$CatOutcome, employ_datQ13.3.aFirstGen$FirstGen, employ_datQ13.3.aFirstGen)
analysis <- polr(employ_datQ13.3.aFirstGen$CatOutcome ~ employ_datQ13.3.aFirstGen$FirstGen, data=employ_datQ13.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ13.3.aGender<-multidatClean(Q13.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aGender$Gender<-factor(employ_datQ13.3.aGender$Gender)
employ_datQ13.3.aGender<-ordinaldatClean(employ_datQ13.3.aGender$Q13.3.a, employ_datQ13.3.aGender)
#ordinal(employ_datQ13.3.aGender$CatOutcome, employ_datQ13.3.aGender$Gender, employ_datQ13.3.aGender)
prep <- analysisPrep(employ_datQ13.3.aGender$CatOutcome, employ_datQ13.3.aGender$Gender, employ_datQ13.3.aGender)
analysis <- polr(employ_datQ13.3.aGender$CatOutcome ~ employ_datQ13.3.aGender$Gender, data=employ_datQ13.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ13.3.aSexuality<-multidatClean(Q13.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.3.aSexuality$Sexuality<-factor(employ_datQ13.3.aSexuality$Sexuality)
employ_datQ13.3.aSexuality<-ordinaldatClean(employ_datQ13.3.aSexuality$Q13.3.a, employ_datQ13.3.aSexuality)
#ordinal(employ_datQ13.3.aSexuality$CatOutcome, employ_datQ13.3.aSexuality$Sexuality, employ_datQ13.3.aSexuality)
prep <- analysisPrep(employ_datQ13.3.aSexuality$CatOutcome, employ_datQ13.3.aSexuality$Sexuality, employ_datQ13.3.aSexuality)
analysis <- polr(employ_datQ13.3.aSexuality$CatOutcome ~ employ_datQ13.3.aSexuality$Sexuality, data=employ_datQ13.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q13.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)



### Q13 - To what extent do you agree or disagree with the following statements regarding the management of your work
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q13.4. I have felt pressured by my supervisor to produce a particular result"
"Status"
employ_datQ13.4.a<-multidatClean(Q13.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.4.a + Academic, data = employ_datQ13.4.a)
detach(dat_long)
employ_datQ13.4.a<-ordinaldatCleanNegative(employ_datQ13.4.a$Q13.4.a, employ_datQ13.4.a)
#ordinal(employ_datQ13.4.a$CatOutcome, employ_datQ13.4.a$Academic, employ_datQ13.4.a)
prep <- analysisPrep(employ_datQ13.4.a$CatOutcome, employ_datQ13.4.a$Academic, employ_datQ13.4.a)
analysis <- polr(employ_datQ13.4.a$CatOutcome ~ employ_datQ13.4.a$Academic, data=employ_datQ13.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ13.4.aCollege<-multidatClean(Q13.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.4.a + Q3, data = employ_datQ13.4.aCollege)
conTable
detach(dat_long)
employ_datQ13.4.aCollege$Q3[(employ_datQ13.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.4.aCollege$Q3<- factor(employ_datQ13.4.aCollege$Q3)
employ_datQ13.4.aCollege<-ordinaldatCleanNegative(employ_datQ13.4.aCollege$Q13.4.a, employ_datQ13.4.aCollege)
#ordinal(employ_datQ13.4.aCollege$CatOutcome, employ_datQ13.4.aCollege$Q3, employ_datQ13.4.aCollege)
prep <- analysisPrep(employ_datQ13.4.aCollege$CatOutcome, employ_datQ13.4.aCollege$Q3, employ_datQ13.4.aCollege)
analysis <- polr(employ_datQ13.4.aCollege$CatOutcome ~ employ_datQ13.4.aCollege$Q3, data=employ_datQ13.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ13.4.aCarer<-multidatClean(Q13.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aCarer$Carer<- factor(employ_datQ13.4.aCarer$Carer)
employ_datQ13.4.aCarer<-ordinaldatCleanNegative(employ_datQ13.4.aCarer$Q13.4.a, employ_datQ13.4.aCarer)
#ordinal(employ_datQ13.4.aCarer$CatOutcome, employ_datQ13.4.aCarer$Carer, employ_datQ13.4.aCarer)
prep <- analysisPrep(employ_datQ13.4.aCarer$CatOutcome, employ_datQ13.4.aCarer$Carer, employ_datQ13.4.aCarer)
analysis <- polr(employ_datQ13.4.aCarer$CatOutcome ~ employ_datQ13.4.aCarer$Carer, data=employ_datQ13.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ13.4.aDisability<-multidatClean(Q13.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aDisability$Disability<- factor(employ_datQ13.4.aDisability$Disability)
employ_datQ13.4.aDisability<-ordinaldatCleanNegative(employ_datQ13.4.aDisability$Q13.4.a, employ_datQ13.4.aDisability)
conTable <- xtabs(~Q13.4.a + Disability, data = employ_datQ13.4.aDisability)
#ordinal(employ_datQ13.4.aDisability$CatOutcome, employ_datQ13.4.aDisability$Disability, employ_datQ13.4.aDisability)
prep <- analysisPrep(employ_datQ13.4.aDisability$CatOutcome, employ_datQ13.4.aDisability$Disability, employ_datQ13.4.aDisability)
analysis <- polr(employ_datQ13.4.aDisability$CatOutcome ~ employ_datQ13.4.aDisability$Disability, data=employ_datQ13.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ13.4.aEthnicity<-multidatClean(Q13.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aEthnicity$Ethnicity<- factor(employ_datQ13.4.aEthnicity$EthnicityCleaned)
employ_datQ13.4.aEthnicity<-ordinaldatCleanNegative(employ_datQ13.4.aEthnicity$Q13.4.a, employ_datQ13.4.aEthnicity)
conTable <- xtabs(~Q13.4.a + EthnicityCleaned, data = employ_datQ13.4.aEthnicity)
conTable
#ordinal(employ_datQ13.4.aEthnicity$CatOutcome, employ_datQ13.4.aEthnicity$EthnicityCleaned, employ_datQ13.4.aEthnicity)
prep <- analysisPrep(employ_datQ13.4.aEthnicity$CatOutcome, employ_datQ13.4.aEthnicity$EthnicityCleaned, employ_datQ13.4.aEthnicity)
analysis <- polr(employ_datQ13.4.aEthnicity$CatOutcome ~ employ_datQ13.4.aEthnicity$EthnicityCleaned, data=employ_datQ13.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ13.4.aFirstGen<-multidatClean(Q13.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aFirstGen$FirstGen<-factor(employ_datQ13.4.aFirstGen$FirstGen)
employ_datQ13.4.aFirstGen<-ordinaldatCleanNegative(employ_datQ13.4.aFirstGen$Q13.4.a, employ_datQ13.4.aFirstGen)
#ordinal(employ_datQ13.4.aFirstGen$CatOutcome, employ_datQ13.4.aFirstGen$FirstGen, employ_datQ13.4.aFirstGen)
prep <- analysisPrep(employ_datQ13.4.aFirstGen$CatOutcome, employ_datQ13.4.aFirstGen$FirstGen, employ_datQ13.4.aFirstGen)
analysis <- polr(employ_datQ13.4.aFirstGen$CatOutcome ~ employ_datQ13.4.aFirstGen$FirstGen, data=employ_datQ13.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ13.4.aGender<-multidatClean(Q13.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aGender$Gender<-factor(employ_datQ13.4.aGender$Gender)
employ_datQ13.4.aGender<-ordinaldatCleanNegative(employ_datQ13.4.aGender$Q13.4.a, employ_datQ13.4.aGender)
#ordinal(employ_datQ13.4.aGender$CatOutcome, employ_datQ13.4.aGender$Gender, employ_datQ13.4.aGender)
prep <- analysisPrep(employ_datQ13.4.aGender$CatOutcome, employ_datQ13.4.aGender$Gender, employ_datQ13.4.aGender)
analysis <- polr(employ_datQ13.4.aGender$CatOutcome ~ employ_datQ13.4.aGender$Gender, data=employ_datQ13.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ13.4.aSexuality<-multidatClean(Q13.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.4.aSexuality$Sexuality<-factor(employ_datQ13.4.aSexuality$Sexuality)
employ_datQ13.4.aSexuality<-ordinaldatCleanNegative(employ_datQ13.4.aSexuality$Q13.4.a, employ_datQ13.4.aSexuality)
#ordinal(employ_datQ13.4.aSexuality$CatOutcome, employ_datQ13.4.aSexuality$Sexuality, employ_datQ13.4.aSexuality)
prep <- analysisPrep(employ_datQ13.4.aSexuality$CatOutcome, employ_datQ13.4.aSexuality$Sexuality, employ_datQ13.4.aSexuality)
analysis <- polr(employ_datQ13.4.aSexuality$CatOutcome ~ employ_datQ13.4.aSexuality$Sexuality, data=employ_datQ13.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q13.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q13 - To what extent do you agree or disagree with the following statements regarding the management of your work
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q13.7. I have felt pressured by my supervisor to produce a particular result"
"Status"
employ_datQ13.7.a<-multidatClean(Q13.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.7.a + Academic, data = employ_datQ13.7.a)
detach(dat_long)
employ_datQ13.7.a<-ordinaldatClean(employ_datQ13.7.a$Q13.7.a, employ_datQ13.7.a)
#ordinal(employ_datQ13.7.a$CatOutcome, employ_datQ13.7.a$Academic, employ_datQ13.7.a)
prep <- analysisPrep(employ_datQ13.7.a$CatOutcome, employ_datQ13.7.a$Academic, employ_datQ13.7.a)
analysis <- polr(employ_datQ13.7.a$CatOutcome ~ employ_datQ13.7.a$Academic, data=employ_datQ13.7.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ13.7.aCollege<-multidatClean(Q13.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.7.a + Q3, data = employ_datQ13.7.aCollege)
conTable
detach(dat_long)
employ_datQ13.7.aCollege$Q3[(employ_datQ13.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.7.aCollege$Q3<- factor(employ_datQ13.7.aCollege$Q3)
employ_datQ13.7.aCollege<-ordinaldatClean(employ_datQ13.7.aCollege$Q13.7.a, employ_datQ13.7.aCollege)
#ordinal(employ_datQ13.7.aCollege$CatOutcome, employ_datQ13.7.aCollege$Q3, employ_datQ13.7.aCollege)
prep <- analysisPrep(employ_datQ13.7.aCollege$CatOutcome, employ_datQ13.7.aCollege$Q3, employ_datQ13.7.aCollege)
analysis <- polr(employ_datQ13.7.aCollege$CatOutcome ~ employ_datQ13.7.aCollege$Q3, data=employ_datQ13.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ13.7.aCarer<-multidatClean(Q13.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aCarer$Carer<- factor(employ_datQ13.7.aCarer$Carer)
employ_datQ13.7.aCarer<-ordinaldatClean(employ_datQ13.7.aCarer$Q13.7.a, employ_datQ13.7.aCarer)
#ordinal(employ_datQ13.7.aCarer$CatOutcome, employ_datQ13.7.aCarer$Carer, employ_datQ13.7.aCarer)
prep <- analysisPrep(employ_datQ13.7.aCarer$CatOutcome, employ_datQ13.7.aCarer$Carer, employ_datQ13.7.aCarer)
analysis <- polr(employ_datQ13.7.aCarer$CatOutcome ~ employ_datQ13.7.aCarer$Carer, data=employ_datQ13.7.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ13.7.aDisability<-multidatClean(Q13.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aDisability$Disability<- factor(employ_datQ13.7.aDisability$Disability)
employ_datQ13.7.aDisability<-ordinaldatClean(employ_datQ13.7.aDisability$Q13.7.a, employ_datQ13.7.aDisability)
conTable <- xtabs(~Q13.7.a + Disability, data = employ_datQ13.7.aDisability)
#ordinal(employ_datQ13.7.aDisability$CatOutcome, employ_datQ13.7.aDisability$Disability, employ_datQ13.7.aDisability)
prep <- analysisPrep(employ_datQ13.7.aDisability$CatOutcome, employ_datQ13.7.aDisability$Disability, employ_datQ13.7.aDisability)
analysis <- polr(employ_datQ13.7.aDisability$CatOutcome ~ employ_datQ13.7.aDisability$Disability, data=employ_datQ13.7.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ13.7.aEthnicity<-multidatClean(Q13.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aEthnicity$Ethnicity<- factor(employ_datQ13.7.aEthnicity$EthnicityCleaned)
employ_datQ13.7.aEthnicity<-ordinaldatClean(employ_datQ13.7.aEthnicity$Q13.7.a, employ_datQ13.7.aEthnicity)
conTable <- xtabs(~Q13.7.a + EthnicityCleaned, data = employ_datQ13.7.aEthnicity)
conTable
#ordinal(employ_datQ13.7.aEthnicity$CatOutcome, employ_datQ13.7.aEthnicity$EthnicityCleaned, employ_datQ13.7.aEthnicity)
prep <- analysisPrep(employ_datQ13.7.aEthnicity$CatOutcome, employ_datQ13.7.aEthnicity$EthnicityCleaned, employ_datQ13.7.aEthnicity)
analysis <- polr(employ_datQ13.7.aEthnicity$CatOutcome ~ employ_datQ13.7.aEthnicity$EthnicityCleaned, data=employ_datQ13.7.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ13.7.aFirstGen<-multidatClean(Q13.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aFirstGen$FirstGen<-factor(employ_datQ13.7.aFirstGen$FirstGen)
employ_datQ13.7.aFirstGen<-ordinaldatClean(employ_datQ13.7.aFirstGen$Q13.7.a, employ_datQ13.7.aFirstGen)
#ordinal(employ_datQ13.7.aFirstGen$CatOutcome, employ_datQ13.7.aFirstGen$FirstGen, employ_datQ13.7.aFirstGen)
prep <- analysisPrep(employ_datQ13.7.aFirstGen$CatOutcome, employ_datQ13.7.aFirstGen$FirstGen, employ_datQ13.7.aFirstGen)
analysis <- polr(employ_datQ13.7.aFirstGen$CatOutcome ~ employ_datQ13.7.aFirstGen$FirstGen, data=employ_datQ13.7.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ13.7.aGender<-multidatClean(Q13.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aGender$Gender<-factor(employ_datQ13.7.aGender$Gender)
employ_datQ13.7.aGender<-ordinaldatClean(employ_datQ13.7.aGender$Q13.7.a, employ_datQ13.7.aGender)
#ordinal(employ_datQ13.7.aGender$CatOutcome, employ_datQ13.7.aGender$Gender, employ_datQ13.7.aGender)
prep <- analysisPrep(employ_datQ13.7.aGender$CatOutcome, employ_datQ13.7.aGender$Gender, employ_datQ13.7.aGender)
analysis <- polr(employ_datQ13.7.aGender$CatOutcome ~ employ_datQ13.7.aGender$Gender, data=employ_datQ13.7.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ13.7.aSexuality<-multidatClean(Q13.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.7.aSexuality$Sexuality<-factor(employ_datQ13.7.aSexuality$Sexuality)
employ_datQ13.7.aSexuality<-ordinaldatClean(employ_datQ13.7.aSexuality$Q13.7.a, employ_datQ13.7.aSexuality)
#ordinal(employ_datQ13.7.aSexuality$CatOutcome, employ_datQ13.7.aSexuality$Sexuality, employ_datQ13.7.aSexuality)
prep <- analysisPrep(employ_datQ13.7.aSexuality$CatOutcome, employ_datQ13.7.aSexuality$Sexuality, employ_datQ13.7.aSexuality)
analysis <- polr(employ_datQ13.7.aSexuality$CatOutcome ~ employ_datQ13.7.aSexuality$Sexuality, data=employ_datQ13.7.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q13.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q13 - To what extent do you agree or disagree with the following statements regarding the management of your work
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q13.8. I have felt pressured by my supervisor to produce a particular result"
"Status"
employ_datQ13.8.a<-multidatClean(Q13.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.8.a + Academic, data = employ_datQ13.8.a)
detach(dat_long)
employ_datQ13.8.a<-ordinaldatClean(employ_datQ13.8.a$Q13.8.a, employ_datQ13.8.a)
#ordinal(employ_datQ13.8.a$CatOutcome, employ_datQ13.8.a$Academic, employ_datQ13.8.a)
prep <- analysisPrep(employ_datQ13.8.a$CatOutcome, employ_datQ13.8.a$Academic, employ_datQ13.8.a)
analysis <- polr(employ_datQ13.8.a$CatOutcome ~ employ_datQ13.8.a$Academic, data=employ_datQ13.8.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ13.8.aCollege<-multidatClean(Q13.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.8.a + Q3, data = employ_datQ13.8.aCollege)
conTable
detach(dat_long)
employ_datQ13.8.aCollege$Q3[(employ_datQ13.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.8.aCollege$Q3<- factor(employ_datQ13.8.aCollege$Q3)
employ_datQ13.8.aCollege<-ordinaldatClean(employ_datQ13.8.aCollege$Q13.8.a, employ_datQ13.8.aCollege)
#ordinal(employ_datQ13.8.aCollege$CatOutcome, employ_datQ13.8.aCollege$Q3, employ_datQ13.8.aCollege)
prep <- analysisPrep(employ_datQ13.8.aCollege$CatOutcome, employ_datQ13.8.aCollege$Q3, employ_datQ13.8.aCollege)
analysis <- polr(employ_datQ13.8.aCollege$CatOutcome ~ employ_datQ13.8.aCollege$Q3, data=employ_datQ13.8.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ13.8.aCarer<-multidatClean(Q13.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aCarer$Carer<- factor(employ_datQ13.8.aCarer$Carer)
employ_datQ13.8.aCarer<-ordinaldatClean(employ_datQ13.8.aCarer$Q13.8.a, employ_datQ13.8.aCarer)
#ordinal(employ_datQ13.8.aCarer$CatOutcome, employ_datQ13.8.aCarer$Carer, employ_datQ13.8.aCarer)
prep <- analysisPrep(employ_datQ13.8.aCarer$CatOutcome, employ_datQ13.8.aCarer$Carer, employ_datQ13.8.aCarer)
analysis <- polr(employ_datQ13.8.aCarer$CatOutcome ~ employ_datQ13.8.aCarer$Carer, data=employ_datQ13.8.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ13.8.aDisability<-multidatClean(Q13.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aDisability$Disability<- factor(employ_datQ13.8.aDisability$Disability)
employ_datQ13.8.aDisability<-ordinaldatClean(employ_datQ13.8.aDisability$Q13.8.a, employ_datQ13.8.aDisability)
conTable <- xtabs(~Q13.8.a + Disability, data = employ_datQ13.8.aDisability)
#ordinal(employ_datQ13.8.aDisability$CatOutcome, employ_datQ13.8.aDisability$Disability, employ_datQ13.8.aDisability)
prep <- analysisPrep(employ_datQ13.8.aDisability$CatOutcome, employ_datQ13.8.aDisability$Disability, employ_datQ13.8.aDisability)
analysis <- polr(employ_datQ13.8.aDisability$CatOutcome ~ employ_datQ13.8.aDisability$Disability, data=employ_datQ13.8.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ13.8.aEthnicity<-multidatClean(Q13.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aEthnicity$Ethnicity<- factor(employ_datQ13.8.aEthnicity$EthnicityCleaned)
employ_datQ13.8.aEthnicity<-ordinaldatClean(employ_datQ13.8.aEthnicity$Q13.8.a, employ_datQ13.8.aEthnicity)
conTable <- xtabs(~Q13.8.a + EthnicityCleaned, data = employ_datQ13.8.aEthnicity)
conTable
#ordinal(employ_datQ13.8.aEthnicity$CatOutcome, employ_datQ13.8.aEthnicity$EthnicityCleaned, employ_datQ13.8.aEthnicity)
prep <- analysisPrep(employ_datQ13.8.aEthnicity$CatOutcome, employ_datQ13.8.aEthnicity$EthnicityCleaned, employ_datQ13.8.aEthnicity)
analysis <- polr(employ_datQ13.8.aEthnicity$CatOutcome ~ employ_datQ13.8.aEthnicity$EthnicityCleaned, data=employ_datQ13.8.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ13.8.aFirstGen<-multidatClean(Q13.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aFirstGen$FirstGen<-factor(employ_datQ13.8.aFirstGen$FirstGen)
employ_datQ13.8.aFirstGen<-ordinaldatClean(employ_datQ13.8.aFirstGen$Q13.8.a, employ_datQ13.8.aFirstGen)
#ordinal(employ_datQ13.8.aFirstGen$CatOutcome, employ_datQ13.8.aFirstGen$FirstGen, employ_datQ13.8.aFirstGen)
prep <- analysisPrep(employ_datQ13.8.aFirstGen$CatOutcome, employ_datQ13.8.aFirstGen$FirstGen, employ_datQ13.8.aFirstGen)
analysis <- polr(employ_datQ13.8.aFirstGen$CatOutcome ~ employ_datQ13.8.aFirstGen$FirstGen, data=employ_datQ13.8.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ13.8.aGender<-multidatClean(Q13.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aGender$Gender<-factor(employ_datQ13.8.aGender$Gender)
employ_datQ13.8.aGender<-ordinaldatClean(employ_datQ13.8.aGender$Q13.8.a, employ_datQ13.8.aGender)
#ordinal(employ_datQ13.8.aGender$CatOutcome, employ_datQ13.8.aGender$Gender, employ_datQ13.8.aGender)
prep <- analysisPrep(employ_datQ13.8.aGender$CatOutcome, employ_datQ13.8.aGender$Gender, employ_datQ13.8.aGender)
analysis <- polr(employ_datQ13.8.aGender$CatOutcome ~ employ_datQ13.8.aGender$Gender, data=employ_datQ13.8.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ13.8.aSexuality<-multidatClean(Q13.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ13.8.aSexuality$Sexuality<-factor(employ_datQ13.8.aSexuality$Sexuality)
employ_datQ13.8.aSexuality<-ordinaldatClean(employ_datQ13.8.aSexuality$Q13.8.a, employ_datQ13.8.aSexuality)
#ordinal(employ_datQ13.8.aSexuality$CatOutcome, employ_datQ13.8.aSexuality$Sexuality, employ_datQ13.8.aSexuality)
prep <- analysisPrep(employ_datQ13.8.aSexuality$CatOutcome, employ_datQ13.8.aSexuality$Sexuality, employ_datQ13.8.aSexuality)
analysis <- polr(employ_datQ13.8.aSexuality$CatOutcome ~ employ_datQ13.8.aSexuality$Sexuality, data=employ_datQ13.8.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.8.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q13.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q14 - To what extent do you agree or disagree with the following statements regarding your institutional senior management?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
### Q14 - To what extent do you agree or disagree with the following statements regarding your institutional senior management?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q14.2. I am satisfied with the way the University of Edinburgh handles performance reviews"
"Status"
employ_datQ14.2.a<-multidatClean(Q14.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q14.2.a + Academic, data = employ_datQ14.2.a)
detach(dat_long)
employ_datQ14.2.a<-ordinaldatClean(employ_datQ14.2.a$Q14.2.a, employ_datQ14.2.a)
#ordinal(employ_datQ14.2.a$CatOutcome, employ_datQ14.2.a$Academic, employ_datQ14.2.a)
prep <- analysisPrep(employ_datQ14.2.a$CatOutcome, employ_datQ14.2.a$Academic, employ_datQ14.2.a)
analysis <- polr(employ_datQ14.2.a$CatOutcome ~ employ_datQ14.2.a$Academic, data=employ_datQ14.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q14.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ14.2.aCollege<-multidatClean(Q14.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q14.2.a + Q3, data = employ_datQ14.2.aCollege)
conTable
detach(dat_long)
employ_datQ14.2.aCollege$Q3[(employ_datQ14.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ14.2.aCollege$Q3<- factor(employ_datQ14.2.aCollege$Q3)
employ_datQ14.2.aCollege<-ordinaldatClean(employ_datQ14.2.aCollege$Q14.2.a, employ_datQ14.2.aCollege)
#ordinal(employ_datQ14.2.aCollege$CatOutcome, employ_datQ14.2.aCollege$Q3, employ_datQ14.2.aCollege)
prep <- analysisPrep(employ_datQ14.2.aCollege$CatOutcome, employ_datQ14.2.aCollege$Q3, employ_datQ14.2.aCollege)
analysis <- polr(employ_datQ14.2.aCollege$CatOutcome ~ employ_datQ14.2.aCollege$Q3, data=employ_datQ14.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ14.2.aCarer<-multidatClean(Q14.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aCarer$Carer<- factor(employ_datQ14.2.aCarer$Carer)
employ_datQ14.2.aCarer<-ordinaldatClean(employ_datQ14.2.aCarer$Q14.2.a, employ_datQ14.2.aCarer)
#ordinal(employ_datQ14.2.aCarer$CatOutcome, employ_datQ14.2.aCarer$Carer, employ_datQ14.2.aCarer)
prep <- analysisPrep(employ_datQ14.2.aCarer$CatOutcome, employ_datQ14.2.aCarer$Carer, employ_datQ14.2.aCarer)
analysis <- polr(employ_datQ14.2.aCarer$CatOutcome ~ employ_datQ14.2.aCarer$Carer, data=employ_datQ14.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ14.2.aDisability<-multidatClean(Q14.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aDisability$Disability<- factor(employ_datQ14.2.aDisability$Disability)
employ_datQ14.2.aDisability<-ordinaldatClean(employ_datQ14.2.aDisability$Q14.2.a, employ_datQ14.2.aDisability)
conTable <- xtabs(~Q14.2.a + Disability, data = employ_datQ14.2.aDisability)
#ordinal(employ_datQ14.2.aDisability$CatOutcome, employ_datQ14.2.aDisability$Disability, employ_datQ14.2.aDisability)
prep <- analysisPrep(employ_datQ14.2.aDisability$CatOutcome, employ_datQ14.2.aDisability$Disability, employ_datQ14.2.aDisability)
analysis <- polr(employ_datQ14.2.aDisability$CatOutcome ~ employ_datQ14.2.aDisability$Disability, data=employ_datQ14.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ14.2.aEthnicity<-multidatClean(Q14.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aEthnicity$Ethnicity<- factor(employ_datQ14.2.aEthnicity$EthnicityCleaned)
employ_datQ14.2.aEthnicity<-ordinaldatClean(employ_datQ14.2.aEthnicity$Q14.2.a, employ_datQ14.2.aEthnicity)
conTable <- xtabs(~Q14.2.a + EthnicityCleaned, data = employ_datQ14.2.aEthnicity)
conTable
#ordinal(employ_datQ14.2.aEthnicity$CatOutcome, employ_datQ14.2.aEthnicity$EthnicityCleaned, employ_datQ14.2.aEthnicity)
prep <- analysisPrep(employ_datQ14.2.aEthnicity$CatOutcome, employ_datQ14.2.aEthnicity$EthnicityCleaned, employ_datQ14.2.aEthnicity)
analysis <- polr(employ_datQ14.2.aEthnicity$CatOutcome ~ employ_datQ14.2.aEthnicity$EthnicityCleaned, data=employ_datQ14.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ14.2.aFirstGen<-multidatClean(Q14.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aFirstGen$FirstGen<-factor(employ_datQ14.2.aFirstGen$FirstGen)
employ_datQ14.2.aFirstGen<-ordinaldatClean(employ_datQ14.2.aFirstGen$Q14.2.a, employ_datQ14.2.aFirstGen)
#ordinal(employ_datQ14.2.aFirstGen$CatOutcome, employ_datQ14.2.aFirstGen$FirstGen, employ_datQ14.2.aFirstGen)
prep <- analysisPrep(employ_datQ14.2.aFirstGen$CatOutcome, employ_datQ14.2.aFirstGen$FirstGen, employ_datQ14.2.aFirstGen)
analysis <- polr(employ_datQ14.2.aFirstGen$CatOutcome ~ employ_datQ14.2.aFirstGen$FirstGen, data=employ_datQ14.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ14.2.aGender<-multidatClean(Q14.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aGender$Gender<-factor(employ_datQ14.2.aGender$Gender)
employ_datQ14.2.aGender<-ordinaldatClean(employ_datQ14.2.aGender$Q14.2.a, employ_datQ14.2.aGender)
#ordinal(employ_datQ14.2.aGender$CatOutcome, employ_datQ14.2.aGender$Gender, employ_datQ14.2.aGender)
prep <- analysisPrep(employ_datQ14.2.aGender$CatOutcome, employ_datQ14.2.aGender$Gender, employ_datQ14.2.aGender)
analysis <- polr(employ_datQ14.2.aGender$CatOutcome ~ employ_datQ14.2.aGender$Gender, data=employ_datQ14.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ14.2.aSexuality<-multidatClean(Q14.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.2.aSexuality$Sexuality<-factor(employ_datQ14.2.aSexuality$Sexuality)
employ_datQ14.2.aSexuality<-ordinaldatClean(employ_datQ14.2.aSexuality$Q14.2.a, employ_datQ14.2.aSexuality)
#ordinal(employ_datQ14.2.aSexuality$CatOutcome, employ_datQ14.2.aSexuality$Sexuality, employ_datQ14.2.aSexuality)
prep <- analysisPrep(employ_datQ14.2.aSexuality$CatOutcome, employ_datQ14.2.aSexuality$Sexuality, employ_datQ14.2.aSexuality)
analysis <- polr(employ_datQ14.2.aSexuality$CatOutcome ~ employ_datQ14.2.aSexuality$Sexuality, data=employ_datQ14.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q14.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q14 - To what extent do you agree or disagree with the following statements regarding your institutional senior management?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q14.3. Leaders communicate clear expectations regarding behaviours and/or culture in my working environment"
"Status"
employ_datQ14.3.a<-multidatClean(Q14.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q14.3.a + Academic, data = employ_datQ14.3.a)
detach(dat_long)
employ_datQ14.3.a<-ordinaldatClean(employ_datQ14.3.a$Q14.3.a, employ_datQ14.3.a)
#ordinal(employ_datQ14.3.a$CatOutcome, employ_datQ14.3.a$Academic, employ_datQ14.3.a)
prep <- analysisPrep(employ_datQ14.3.a$CatOutcome, employ_datQ14.3.a$Academic, employ_datQ14.3.a)
analysis <- polr(employ_datQ14.3.a$CatOutcome ~ employ_datQ14.3.a$Academic, data=employ_datQ14.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q14.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ14.3.aCollege<-multidatClean(Q14.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q14.3.a + Q3, data = employ_datQ14.3.aCollege)
conTable
detach(dat_long)
employ_datQ14.3.aCollege$Q3[(employ_datQ14.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ14.3.aCollege$Q3<- factor(employ_datQ14.3.aCollege$Q3)
employ_datQ14.3.aCollege<-ordinaldatClean(employ_datQ14.3.aCollege$Q14.3.a, employ_datQ14.3.aCollege)
#ordinal(employ_datQ14.3.aCollege$CatOutcome, employ_datQ14.3.aCollege$Q3, employ_datQ14.3.aCollege)
prep <- analysisPrep(employ_datQ14.3.aCollege$CatOutcome, employ_datQ14.3.aCollege$Q3, employ_datQ14.3.aCollege)
analysis <- polr(employ_datQ14.3.aCollege$CatOutcome ~ employ_datQ14.3.aCollege$Q3, data=employ_datQ14.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ14.3.aCarer<-multidatClean(Q14.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aCarer$Carer<- factor(employ_datQ14.3.aCarer$Carer)
employ_datQ14.3.aCarer<-ordinaldatClean(employ_datQ14.3.aCarer$Q14.3.a, employ_datQ14.3.aCarer)
#ordinal(employ_datQ14.3.aCarer$CatOutcome, employ_datQ14.3.aCarer$Carer, employ_datQ14.3.aCarer)
prep <- analysisPrep(employ_datQ14.3.aCarer$CatOutcome, employ_datQ14.3.aCarer$Carer, employ_datQ14.3.aCarer)
analysis <- polr(employ_datQ14.3.aCarer$CatOutcome ~ employ_datQ14.3.aCarer$Carer, data=employ_datQ14.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ14.3.aDisability<-multidatClean(Q14.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aDisability$Disability<- factor(employ_datQ14.3.aDisability$Disability)
employ_datQ14.3.aDisability<-ordinaldatClean(employ_datQ14.3.aDisability$Q14.3.a, employ_datQ14.3.aDisability)
conTable <- xtabs(~Q14.3.a + Disability, data = employ_datQ14.3.aDisability)
#ordinal(employ_datQ14.3.aDisability$CatOutcome, employ_datQ14.3.aDisability$Disability, employ_datQ14.3.aDisability)
prep <- analysisPrep(employ_datQ14.3.aDisability$CatOutcome, employ_datQ14.3.aDisability$Disability, employ_datQ14.3.aDisability)
analysis <- polr(employ_datQ14.3.aDisability$CatOutcome ~ employ_datQ14.3.aDisability$Disability, data=employ_datQ14.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ14.3.aEthnicity<-multidatClean(Q14.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aEthnicity$Ethnicity<- factor(employ_datQ14.3.aEthnicity$EthnicityCleaned)
employ_datQ14.3.aEthnicity<-ordinaldatClean(employ_datQ14.3.aEthnicity$Q14.3.a, employ_datQ14.3.aEthnicity)
conTable <- xtabs(~Q14.3.a + EthnicityCleaned, data = employ_datQ14.3.aEthnicity)
conTable
#ordinal(employ_datQ14.3.aEthnicity$CatOutcome, employ_datQ14.3.aEthnicity$EthnicityCleaned, employ_datQ14.3.aEthnicity)
prep <- analysisPrep(employ_datQ14.3.aEthnicity$CatOutcome, employ_datQ14.3.aEthnicity$EthnicityCleaned, employ_datQ14.3.aEthnicity)
analysis <- polr(employ_datQ14.3.aEthnicity$CatOutcome ~ employ_datQ14.3.aEthnicity$EthnicityCleaned, data=employ_datQ14.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ14.3.aFirstGen<-multidatClean(Q14.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aFirstGen$FirstGen<-factor(employ_datQ14.3.aFirstGen$FirstGen)
employ_datQ14.3.aFirstGen<-ordinaldatClean(employ_datQ14.3.aFirstGen$Q14.3.a, employ_datQ14.3.aFirstGen)
#ordinal(employ_datQ14.3.aFirstGen$CatOutcome, employ_datQ14.3.aFirstGen$FirstGen, employ_datQ14.3.aFirstGen)
prep <- analysisPrep(employ_datQ14.3.aFirstGen$CatOutcome, employ_datQ14.3.aFirstGen$FirstGen, employ_datQ14.3.aFirstGen)
analysis <- polr(employ_datQ14.3.aFirstGen$CatOutcome ~ employ_datQ14.3.aFirstGen$FirstGen, data=employ_datQ14.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ14.3.aGender<-multidatClean(Q14.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aGender$Gender<-factor(employ_datQ14.3.aGender$Gender)
employ_datQ14.3.aGender<-ordinaldatClean(employ_datQ14.3.aGender$Q14.3.a, employ_datQ14.3.aGender)
#ordinal(employ_datQ14.3.aGender$CatOutcome, employ_datQ14.3.aGender$Gender, employ_datQ14.3.aGender)
prep <- analysisPrep(employ_datQ14.3.aGender$CatOutcome, employ_datQ14.3.aGender$Gender, employ_datQ14.3.aGender)
analysis <- polr(employ_datQ14.3.aGender$CatOutcome ~ employ_datQ14.3.aGender$Gender, data=employ_datQ14.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ14.3.aSexuality<-multidatClean(Q14.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ14.3.aSexuality$Sexuality<-factor(employ_datQ14.3.aSexuality$Sexuality)
employ_datQ14.3.aSexuality<-ordinaldatClean(employ_datQ14.3.aSexuality$Q14.3.a, employ_datQ14.3.aSexuality)
#ordinal(employ_datQ14.3.aSexuality$CatOutcome, employ_datQ14.3.aSexuality$Sexuality, employ_datQ14.3.aSexuality)
prep <- analysisPrep(employ_datQ14.3.aSexuality$CatOutcome, employ_datQ14.3.aSexuality$Sexuality, employ_datQ14.3.aSexuality)
analysis <- polr(employ_datQ14.3.aSexuality$CatOutcome ~ employ_datQ14.3.aSexuality$Sexuality, data=employ_datQ14.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q14.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q15 - How important do you think the following research leadership characteristics are?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q15.1. Setting the direction for research and creating the plans and systems to achieve it"
"Status"
employ_datQ15.1.a<-multidatClean(Q15.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.1.a + Academic, data = employ_datQ15.1.a)
detach(dat_long)
employ_datQ15.1.a<-ImpordinaldatClean(employ_datQ15.1.a$Q15.1.a, employ_datQ15.1.a)
#ordinal(employ_datQ15.1.a$CatOutcome, employ_datQ15.1.a$Academic, employ_datQ15.1.a)
prep <- analysisPrep(employ_datQ15.1.a$CatOutcome, employ_datQ15.1.a$Academic, employ_datQ15.1.a)
analysis <- polr(employ_datQ15.1.a$CatOutcome ~ employ_datQ15.1.a$Academic, data=employ_datQ15.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ15.1.aCollege<-multidatClean(Q15.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.1.a + Q3, data = employ_datQ15.1.aCollege)
conTable
detach(dat_long)
employ_datQ15.1.aCollege$Q3[(employ_datQ15.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ15.1.aCollege$Q3<- factor(employ_datQ15.1.aCollege$Q3)
employ_datQ15.1.aCollege<-ImpordinaldatClean(employ_datQ15.1.aCollege$Q15.1.a, employ_datQ15.1.aCollege)
#ordinal(employ_datQ15.1.aCollege$CatOutcome, employ_datQ15.1.aCollege$Q3, employ_datQ15.1.aCollege)
prep <- analysisPrep(employ_datQ15.1.aCollege$CatOutcome, employ_datQ15.1.aCollege$Q3, employ_datQ15.1.aCollege)
analysis <- polr(employ_datQ15.1.aCollege$CatOutcome ~ employ_datQ15.1.aCollege$Q3, data=employ_datQ15.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ15.1.aCarer<-multidatClean(Q15.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aCarer$Carer<- factor(employ_datQ15.1.aCarer$Carer)
employ_datQ15.1.aCarer<-ImpordinaldatClean(employ_datQ15.1.aCarer$Q15.1.a, employ_datQ15.1.aCarer)
#ordinal(employ_datQ15.1.aCarer$CatOutcome, employ_datQ15.1.aCarer$Carer, employ_datQ15.1.aCarer)
prep <- analysisPrep(employ_datQ15.1.aCarer$CatOutcome, employ_datQ15.1.aCarer$Carer, employ_datQ15.1.aCarer)
analysis <- polr(employ_datQ15.1.aCarer$CatOutcome ~ employ_datQ15.1.aCarer$Carer, data=employ_datQ15.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ15.1.aDisability<-multidatClean(Q15.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aDisability$Disability<- factor(employ_datQ15.1.aDisability$Disability)
employ_datQ15.1.aDisability<-ImpordinaldatClean(employ_datQ15.1.aDisability$Q15.1.a, employ_datQ15.1.aDisability)
conTable <- xtabs(~Q15.1.a + Disability, data = employ_datQ15.1.aDisability)
#ordinal(employ_datQ15.1.aDisability$CatOutcome, employ_datQ15.1.aDisability$Disability, employ_datQ15.1.aDisability)
prep <- analysisPrep(employ_datQ15.1.aDisability$CatOutcome, employ_datQ15.1.aDisability$Disability, employ_datQ15.1.aDisability)
analysis <- polr(employ_datQ15.1.aDisability$CatOutcome ~ employ_datQ15.1.aDisability$Disability, data=employ_datQ15.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ15.1.aEthnicity<-multidatClean(Q15.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aEthnicity$Ethnicity<- factor(employ_datQ15.1.aEthnicity$EthnicityCleaned)
employ_datQ15.1.aEthnicity<-ImpordinaldatClean(employ_datQ15.1.aEthnicity$Q15.1.a, employ_datQ15.1.aEthnicity)
conTable <- xtabs(~Q15.1.a + EthnicityCleaned, data = employ_datQ15.1.aEthnicity)
conTable
#ordinal(employ_datQ15.1.aEthnicity$CatOutcome, employ_datQ15.1.aEthnicity$EthnicityCleaned, employ_datQ15.1.aEthnicity)
prep <- analysisPrep(employ_datQ15.1.aEthnicity$CatOutcome, employ_datQ15.1.aEthnicity$EthnicityCleaned, employ_datQ15.1.aEthnicity)
analysis <- polr(employ_datQ15.1.aEthnicity$CatOutcome ~ employ_datQ15.1.aEthnicity$EthnicityCleaned, data=employ_datQ15.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ15.1.aFirstGen<-multidatClean(Q15.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aFirstGen$FirstGen<-factor(employ_datQ15.1.aFirstGen$FirstGen)
employ_datQ15.1.aFirstGen<-ImpordinaldatClean(employ_datQ15.1.aFirstGen$Q15.1.a, employ_datQ15.1.aFirstGen)
#ordinal(employ_datQ15.1.aFirstGen$CatOutcome, employ_datQ15.1.aFirstGen$FirstGen, employ_datQ15.1.aFirstGen)
prep <- analysisPrep(employ_datQ15.1.aFirstGen$CatOutcome, employ_datQ15.1.aFirstGen$FirstGen, employ_datQ15.1.aFirstGen)
analysis <- polr(employ_datQ15.1.aFirstGen$CatOutcome ~ employ_datQ15.1.aFirstGen$FirstGen, data=employ_datQ15.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ15.1.aGender<-multidatClean(Q15.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aGender$Gender<-factor(employ_datQ15.1.aGender$Gender)
employ_datQ15.1.aGender<-ImpordinaldatClean(employ_datQ15.1.aGender$Q15.1.a, employ_datQ15.1.aGender)
#ordinal(employ_datQ15.1.aGender$CatOutcome, employ_datQ15.1.aGender$Gender, employ_datQ15.1.aGender)
prep <- analysisPrep(employ_datQ15.1.aGender$CatOutcome, employ_datQ15.1.aGender$Gender, employ_datQ15.1.aGender)
analysis <- polr(employ_datQ15.1.aGender$CatOutcome ~ employ_datQ15.1.aGender$Gender, data=employ_datQ15.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ15.1.aSexuality<-multidatClean(Q15.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.1.aSexuality$Sexuality<-factor(employ_datQ15.1.aSexuality$Sexuality)
employ_datQ15.1.aSexuality<-ImpordinaldatClean(employ_datQ15.1.aSexuality$Q15.1.a, employ_datQ15.1.aSexuality)
#ordinal(employ_datQ15.1.aSexuality$CatOutcome, employ_datQ15.1.aSexuality$Sexuality, employ_datQ15.1.aSexuality)
prep <- analysisPrep(employ_datQ15.1.aSexuality$CatOutcome, employ_datQ15.1.aSexuality$Sexuality, employ_datQ15.1.aSexuality)
analysis <- polr(employ_datQ15.1.aSexuality$CatOutcome ~ employ_datQ15.1.aSexuality$Sexuality, data=employ_datQ15.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q15.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q15 - How important do you think the following research leadership characteristics are?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other

### Q15 - How important do you think the following research leadership characteristics are?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q15.3. Setting and upholding standards in the conduct of research and its application"
"Status"
employ_datQ15.3.a<-multidatClean(Q15.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.3.a + Academic, data = employ_datQ15.3.a)
detach(dat_long)
employ_datQ15.3.a<-ImpordinaldatClean(employ_datQ15.3.a$Q15.3.a, employ_datQ15.3.a)
#ordinal(employ_datQ15.3.a$CatOutcome, employ_datQ15.3.a$Academic, employ_datQ15.3.a)
prep <- analysisPrep(employ_datQ15.3.a$CatOutcome, employ_datQ15.3.a$Academic, employ_datQ15.3.a)
analysis <- polr(employ_datQ15.3.a$CatOutcome ~ employ_datQ15.3.a$Academic, data=employ_datQ15.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ15.3.aCollege<-multidatClean(Q15.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.3.a + Q3, data = employ_datQ15.3.aCollege)
conTable
detach(dat_long)
employ_datQ15.3.aCollege$Q3[(employ_datQ15.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ15.3.aCollege$Q3<- factor(employ_datQ15.3.aCollege$Q3)
employ_datQ15.3.aCollege<-ImpordinaldatClean(employ_datQ15.3.aCollege$Q15.3.a, employ_datQ15.3.aCollege)
#ordinal(employ_datQ15.3.aCollege$CatOutcome, employ_datQ15.3.aCollege$Q3, employ_datQ15.3.aCollege)
prep <- analysisPrep(employ_datQ15.3.aCollege$CatOutcome, employ_datQ15.3.aCollege$Q3, employ_datQ15.3.aCollege)
analysis <- polr(employ_datQ15.3.aCollege$CatOutcome ~ employ_datQ15.3.aCollege$Q3, data=employ_datQ15.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ15.3.aCarer<-multidatClean(Q15.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aCarer$Carer<- factor(employ_datQ15.3.aCarer$Carer)
employ_datQ15.3.aCarer<-ImpordinaldatClean(employ_datQ15.3.aCarer$Q15.3.a, employ_datQ15.3.aCarer)
#ordinal(employ_datQ15.3.aCarer$CatOutcome, employ_datQ15.3.aCarer$Carer, employ_datQ15.3.aCarer)
prep <- analysisPrep(employ_datQ15.3.aCarer$CatOutcome, employ_datQ15.3.aCarer$Carer, employ_datQ15.3.aCarer)
analysis <- polr(employ_datQ15.3.aCarer$CatOutcome ~ employ_datQ15.3.aCarer$Carer, data=employ_datQ15.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ15.3.aDisability<-multidatClean(Q15.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aDisability$Disability<- factor(employ_datQ15.3.aDisability$Disability)
employ_datQ15.3.aDisability<-ImpordinaldatClean(employ_datQ15.3.aDisability$Q15.3.a, employ_datQ15.3.aDisability)
conTable <- xtabs(~Q15.3.a + Disability, data = employ_datQ15.3.aDisability)
#ordinal(employ_datQ15.3.aDisability$CatOutcome, employ_datQ15.3.aDisability$Disability, employ_datQ15.3.aDisability)
prep <- analysisPrep(employ_datQ15.3.aDisability$CatOutcome, employ_datQ15.3.aDisability$Disability, employ_datQ15.3.aDisability)
analysis <- polr(employ_datQ15.3.aDisability$CatOutcome ~ employ_datQ15.3.aDisability$Disability, data=employ_datQ15.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ15.3.aEthnicity<-multidatClean(Q15.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aEthnicity$Ethnicity<- factor(employ_datQ15.3.aEthnicity$EthnicityCleaned)
employ_datQ15.3.aEthnicity<-ImpordinaldatClean(employ_datQ15.3.aEthnicity$Q15.3.a, employ_datQ15.3.aEthnicity)
conTable <- xtabs(~Q15.3.a + EthnicityCleaned, data = employ_datQ15.3.aEthnicity)
conTable
#ordinal(employ_datQ15.3.aEthnicity$CatOutcome, employ_datQ15.3.aEthnicity$EthnicityCleaned, employ_datQ15.3.aEthnicity)
prep <- analysisPrep(employ_datQ15.3.aEthnicity$CatOutcome, employ_datQ15.3.aEthnicity$EthnicityCleaned, employ_datQ15.3.aEthnicity)
analysis <- polr(employ_datQ15.3.aEthnicity$CatOutcome ~ employ_datQ15.3.aEthnicity$EthnicityCleaned, data=employ_datQ15.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ15.3.aFirstGen<-multidatClean(Q15.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aFirstGen$FirstGen<-factor(employ_datQ15.3.aFirstGen$FirstGen)
employ_datQ15.3.aFirstGen<-ImpordinaldatClean(employ_datQ15.3.aFirstGen$Q15.3.a, employ_datQ15.3.aFirstGen)
#ordinal(employ_datQ15.3.aFirstGen$CatOutcome, employ_datQ15.3.aFirstGen$FirstGen, employ_datQ15.3.aFirstGen)
prep <- analysisPrep(employ_datQ15.3.aFirstGen$CatOutcome, employ_datQ15.3.aFirstGen$FirstGen, employ_datQ15.3.aFirstGen)
analysis <- polr(employ_datQ15.3.aFirstGen$CatOutcome ~ employ_datQ15.3.aFirstGen$FirstGen, data=employ_datQ15.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ15.3.aGender<-multidatClean(Q15.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aGender$Gender<-factor(employ_datQ15.3.aGender$Gender)
employ_datQ15.3.aGender<-ImpordinaldatClean(employ_datQ15.3.aGender$Q15.3.a, employ_datQ15.3.aGender)
#ordinal(employ_datQ15.3.aGender$CatOutcome, employ_datQ15.3.aGender$Gender, employ_datQ15.3.aGender)
prep <- analysisPrep(employ_datQ15.3.aGender$CatOutcome, employ_datQ15.3.aGender$Gender, employ_datQ15.3.aGender)
analysis <- polr(employ_datQ15.3.aGender$CatOutcome ~ employ_datQ15.3.aGender$Gender, data=employ_datQ15.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ15.3.aSexuality<-multidatClean(Q15.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.3.aSexuality$Sexuality<-factor(employ_datQ15.3.aSexuality$Sexuality)
employ_datQ15.3.aSexuality<-ImpordinaldatClean(employ_datQ15.3.aSexuality$Q15.3.a, employ_datQ15.3.aSexuality)
#ordinal(employ_datQ15.3.aSexuality$CatOutcome, employ_datQ15.3.aSexuality$Sexuality, employ_datQ15.3.aSexuality)
prep <- analysisPrep(employ_datQ15.3.aSexuality$CatOutcome, employ_datQ15.3.aSexuality$Sexuality, employ_datQ15.3.aSexuality)
analysis <- polr(employ_datQ15.3.aSexuality$CatOutcome ~ employ_datQ15.3.aSexuality$Sexuality, data=employ_datQ15.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q15.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q15 - How important do you think the following research leadership characteristics are?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q15.4. Creating development and career opportunities"
"Status"
employ_datQ15.4.a<-multidatClean(Q15.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.4.a + Academic, data = employ_datQ15.4.a)
detach(dat_long)
employ_datQ15.4.a<-ImpordinaldatClean(employ_datQ15.4.a$Q15.4.a, employ_datQ15.4.a)
#ordinal(employ_datQ15.4.a$CatOutcome, employ_datQ15.4.a$Academic, employ_datQ15.4.a)
prep <- analysisPrep(employ_datQ15.4.a$CatOutcome, employ_datQ15.4.a$Academic, employ_datQ15.4.a)
analysis <- polr(employ_datQ15.4.a$CatOutcome ~ employ_datQ15.4.a$Academic, data=employ_datQ15.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ15.4.aCollege<-multidatClean(Q15.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.4.a + Q3, data = employ_datQ15.4.aCollege)
conTable
detach(dat_long)
employ_datQ15.4.aCollege$Q3[(employ_datQ15.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ15.4.aCollege$Q3<- factor(employ_datQ15.4.aCollege$Q3)
employ_datQ15.4.aCollege<-ImpordinaldatClean(employ_datQ15.4.aCollege$Q15.4.a, employ_datQ15.4.aCollege)
#ordinal(employ_datQ15.4.aCollege$CatOutcome, employ_datQ15.4.aCollege$Q3, employ_datQ15.4.aCollege)
prep <- analysisPrep(employ_datQ15.4.aCollege$CatOutcome, employ_datQ15.4.aCollege$Q3, employ_datQ15.4.aCollege)
analysis <- polr(employ_datQ15.4.aCollege$CatOutcome ~ employ_datQ15.4.aCollege$Q3, data=employ_datQ15.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ15.4.aCarer<-multidatClean(Q15.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aCarer$Carer<- factor(employ_datQ15.4.aCarer$Carer)
employ_datQ15.4.aCarer<-ImpordinaldatClean(employ_datQ15.4.aCarer$Q15.4.a, employ_datQ15.4.aCarer)
#ordinal(employ_datQ15.4.aCarer$CatOutcome, employ_datQ15.4.aCarer$Carer, employ_datQ15.4.aCarer)
prep <- analysisPrep(employ_datQ15.4.aCarer$CatOutcome, employ_datQ15.4.aCarer$Carer, employ_datQ15.4.aCarer)
analysis <- polr(employ_datQ15.4.aCarer$CatOutcome ~ employ_datQ15.4.aCarer$Carer, data=employ_datQ15.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ15.4.aDisability<-multidatClean(Q15.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aDisability$Disability<- factor(employ_datQ15.4.aDisability$Disability)
employ_datQ15.4.aDisability<-ImpordinaldatClean(employ_datQ15.4.aDisability$Q15.4.a, employ_datQ15.4.aDisability)
conTable <- xtabs(~Q15.4.a + Disability, data = employ_datQ15.4.aDisability)
#ordinal(employ_datQ15.4.aDisability$CatOutcome, employ_datQ15.4.aDisability$Disability, employ_datQ15.4.aDisability)
prep <- analysisPrep(employ_datQ15.4.aDisability$CatOutcome, employ_datQ15.4.aDisability$Disability, employ_datQ15.4.aDisability)
analysis <- polr(employ_datQ15.4.aDisability$CatOutcome ~ employ_datQ15.4.aDisability$Disability, data=employ_datQ15.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ15.4.aEthnicity<-multidatClean(Q15.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aEthnicity$Ethnicity<- factor(employ_datQ15.4.aEthnicity$EthnicityCleaned)
employ_datQ15.4.aEthnicity<-ImpordinaldatClean(employ_datQ15.4.aEthnicity$Q15.4.a, employ_datQ15.4.aEthnicity)
conTable <- xtabs(~Q15.4.a + EthnicityCleaned, data = employ_datQ15.4.aEthnicity)
conTable
#ordinal(employ_datQ15.4.aEthnicity$CatOutcome, employ_datQ15.4.aEthnicity$EthnicityCleaned, employ_datQ15.4.aEthnicity)
prep <- analysisPrep(employ_datQ15.4.aEthnicity$CatOutcome, employ_datQ15.4.aEthnicity$EthnicityCleaned, employ_datQ15.4.aEthnicity)
analysis <- polr(employ_datQ15.4.aEthnicity$CatOutcome ~ employ_datQ15.4.aEthnicity$EthnicityCleaned, data=employ_datQ15.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ15.4.aFirstGen<-multidatClean(Q15.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aFirstGen$FirstGen<-factor(employ_datQ15.4.aFirstGen$FirstGen)
employ_datQ15.4.aFirstGen<-ImpordinaldatClean(employ_datQ15.4.aFirstGen$Q15.4.a, employ_datQ15.4.aFirstGen)
#ordinal(employ_datQ15.4.aFirstGen$CatOutcome, employ_datQ15.4.aFirstGen$FirstGen, employ_datQ15.4.aFirstGen)
prep <- analysisPrep(employ_datQ15.4.aFirstGen$CatOutcome, employ_datQ15.4.aFirstGen$FirstGen, employ_datQ15.4.aFirstGen)
analysis <- polr(employ_datQ15.4.aFirstGen$CatOutcome ~ employ_datQ15.4.aFirstGen$FirstGen, data=employ_datQ15.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ15.4.aGender<-multidatClean(Q15.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aGender$Gender<-factor(employ_datQ15.4.aGender$Gender)
employ_datQ15.4.aGender<-ImpordinaldatClean(employ_datQ15.4.aGender$Q15.4.a, employ_datQ15.4.aGender)
#ordinal(employ_datQ15.4.aGender$CatOutcome, employ_datQ15.4.aGender$Gender, employ_datQ15.4.aGender)
prep <- analysisPrep(employ_datQ15.4.aGender$CatOutcome, employ_datQ15.4.aGender$Gender, employ_datQ15.4.aGender)
analysis <- polr(employ_datQ15.4.aGender$CatOutcome ~ employ_datQ15.4.aGender$Gender, data=employ_datQ15.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ15.4.aSexuality<-multidatClean(Q15.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ15.4.aSexuality$Sexuality<-factor(employ_datQ15.4.aSexuality$Sexuality)
employ_datQ15.4.aSexuality<-ImpordinaldatClean(employ_datQ15.4.aSexuality$Q15.4.a, employ_datQ15.4.aSexuality)
#ordinal(employ_datQ15.4.aSexuality$CatOutcome, employ_datQ15.4.aSexuality$Sexuality, employ_datQ15.4.aSexuality)
prep <- analysisPrep(employ_datQ15.4.aSexuality$CatOutcome, employ_datQ15.4.aSexuality$Sexuality, employ_datQ15.4.aSexuality)
analysis <- polr(employ_datQ15.4.aSexuality$CatOutcome ~ employ_datQ15.4.aSexuality$Sexuality, data=employ_datQ15.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q15.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q16 - How successful is your workplace team in demonstrating each leadership characteristic??
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q16.1. Setting the direction for research and creating the plans and systems to achieve it"
"Status"
employ_datQ16.1.a<-multidatClean(Q16.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.1.a + Academic, data = employ_datQ16.1.a)
detach(dat_long)
employ_datQ16.1.a<-SucordinaldatClean(employ_datQ16.1.a$Q16.1.a, employ_datQ16.1.a)
#ordinal(employ_datQ16.1.a$CatOutcome, employ_datQ16.1.a$Academic, employ_datQ16.1.a)
prep <- analysisPrep(employ_datQ16.1.a$CatOutcome, employ_datQ16.1.a$Academic, employ_datQ16.1.a)
analysis <- polr(employ_datQ16.1.a$CatOutcome ~ employ_datQ16.1.a$Academic, data=employ_datQ16.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ16.1.aCollege<-multidatClean(Q16.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.1.a + Q3, data = employ_datQ16.1.aCollege)
conTable
detach(dat_long)
employ_datQ16.1.aCollege$Q3[(employ_datQ16.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ16.1.aCollege$Q3<- factor(employ_datQ16.1.aCollege$Q3)
employ_datQ16.1.aCollege<-SucordinaldatClean(employ_datQ16.1.aCollege$Q16.1.a, employ_datQ16.1.aCollege)
#ordinal(employ_datQ16.1.aCollege$CatOutcome, employ_datQ16.1.aCollege$Q3, employ_datQ16.1.aCollege)
prep <- analysisPrep(employ_datQ16.1.aCollege$CatOutcome, employ_datQ16.1.aCollege$Q3, employ_datQ16.1.aCollege)
analysis <- polr(employ_datQ16.1.aCollege$CatOutcome ~ employ_datQ16.1.aCollege$Q3, data=employ_datQ16.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ16.1.aCarer<-multidatClean(Q16.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aCarer$Carer<- factor(employ_datQ16.1.aCarer$Carer)
employ_datQ16.1.aCarer<-SucordinaldatClean(employ_datQ16.1.aCarer$Q16.1.a, employ_datQ16.1.aCarer)
#ordinal(employ_datQ16.1.aCarer$CatOutcome, employ_datQ16.1.aCarer$Carer, employ_datQ16.1.aCarer)
prep <- analysisPrep(employ_datQ16.1.aCarer$CatOutcome, employ_datQ16.1.aCarer$Carer, employ_datQ16.1.aCarer)
analysis <- polr(employ_datQ16.1.aCarer$CatOutcome ~ employ_datQ16.1.aCarer$Carer, data=employ_datQ16.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ16.1.aDisability<-multidatClean(Q16.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aDisability$Disability<- factor(employ_datQ16.1.aDisability$Disability)
employ_datQ16.1.aDisability<-SucordinaldatClean(employ_datQ16.1.aDisability$Q16.1.a, employ_datQ16.1.aDisability)
conTable <- xtabs(~Q16.1.a + Disability, data = employ_datQ16.1.aDisability)
#ordinal(employ_datQ16.1.aDisability$CatOutcome, employ_datQ16.1.aDisability$Disability, employ_datQ16.1.aDisability)
prep <- analysisPrep(employ_datQ16.1.aDisability$CatOutcome, employ_datQ16.1.aDisability$Disability, employ_datQ16.1.aDisability)
analysis <- polr(employ_datQ16.1.aDisability$CatOutcome ~ employ_datQ16.1.aDisability$Disability, data=employ_datQ16.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ16.1.aEthnicity<-multidatClean(Q16.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aEthnicity$Ethnicity<- factor(employ_datQ16.1.aEthnicity$EthnicityCleaned)
employ_datQ16.1.aEthnicity<-SucordinaldatClean(employ_datQ16.1.aEthnicity$Q16.1.a, employ_datQ16.1.aEthnicity)
conTable <- xtabs(~Q16.1.a + EthnicityCleaned, data = employ_datQ16.1.aEthnicity)
conTable
#ordinal(employ_datQ16.1.aEthnicity$CatOutcome, employ_datQ16.1.aEthnicity$EthnicityCleaned, employ_datQ16.1.aEthnicity)
prep <- analysisPrep(employ_datQ16.1.aEthnicity$CatOutcome, employ_datQ16.1.aEthnicity$EthnicityCleaned, employ_datQ16.1.aEthnicity)
analysis <- polr(employ_datQ16.1.aEthnicity$CatOutcome ~ employ_datQ16.1.aEthnicity$EthnicityCleaned, data=employ_datQ16.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ16.1.aFirstGen<-multidatClean(Q16.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aFirstGen$FirstGen<-factor(employ_datQ16.1.aFirstGen$FirstGen)
employ_datQ16.1.aFirstGen<-SucordinaldatClean(employ_datQ16.1.aFirstGen$Q16.1.a, employ_datQ16.1.aFirstGen)
#ordinal(employ_datQ16.1.aFirstGen$CatOutcome, employ_datQ16.1.aFirstGen$FirstGen, employ_datQ16.1.aFirstGen)
prep <- analysisPrep(employ_datQ16.1.aFirstGen$CatOutcome, employ_datQ16.1.aFirstGen$FirstGen, employ_datQ16.1.aFirstGen)
analysis <- polr(employ_datQ16.1.aFirstGen$CatOutcome ~ employ_datQ16.1.aFirstGen$FirstGen, data=employ_datQ16.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ16.1.aGender<-multidatClean(Q16.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aGender$Gender<-factor(employ_datQ16.1.aGender$Gender)
employ_datQ16.1.aGender<-SucordinaldatClean(employ_datQ16.1.aGender$Q16.1.a, employ_datQ16.1.aGender)
#ordinal(employ_datQ16.1.aGender$CatOutcome, employ_datQ16.1.aGender$Gender, employ_datQ16.1.aGender)
prep <- analysisPrep(employ_datQ16.1.aGender$CatOutcome, employ_datQ16.1.aGender$Gender, employ_datQ16.1.aGender)
analysis <- polr(employ_datQ16.1.aGender$CatOutcome ~ employ_datQ16.1.aGender$Gender, data=employ_datQ16.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ16.1.aSexuality<-multidatClean(Q16.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.1.aSexuality$Sexuality<-factor(employ_datQ16.1.aSexuality$Sexuality)
employ_datQ16.1.aSexuality<-SucordinaldatClean(employ_datQ16.1.aSexuality$Q16.1.a, employ_datQ16.1.aSexuality)
#ordinal(employ_datQ16.1.aSexuality$CatOutcome, employ_datQ16.1.aSexuality$Sexuality, employ_datQ16.1.aSexuality)
prep <- analysisPrep(employ_datQ16.1.aSexuality$CatOutcome, employ_datQ16.1.aSexuality$Sexuality, employ_datQ16.1.aSexuality)
analysis <- polr(employ_datQ16.1.aSexuality$CatOutcome ~ employ_datQ16.1.aSexuality$Sexuality, data=employ_datQ16.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q16.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q16 - How successful is your workplace team in demonstrating each leadership characteristic??
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q16.2. Leading and supporting teams of diverse individuals"
"Status"
employ_datQ16.2.a<-multidatClean(Q16.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.2.a + Academic, data = employ_datQ16.2.a)
detach(dat_long)
employ_datQ16.2.a<-SucordinaldatClean(employ_datQ16.2.a$Q16.2.a, employ_datQ16.2.a)
#ordinal(employ_datQ16.2.a$CatOutcome, employ_datQ16.2.a$Academic, employ_datQ16.2.a)
prep <- analysisPrep(employ_datQ16.2.a$CatOutcome, employ_datQ16.2.a$Academic, employ_datQ16.2.a)
analysis <- polr(employ_datQ16.2.a$CatOutcome ~ employ_datQ16.2.a$Academic, data=employ_datQ16.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ16.2.aCollege<-multidatClean(Q16.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.2.a + Q3, data = employ_datQ16.2.aCollege)
conTable
detach(dat_long)
employ_datQ16.2.aCollege$Q3[(employ_datQ16.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ16.2.aCollege$Q3<- factor(employ_datQ16.2.aCollege$Q3)
employ_datQ16.2.aCollege<-SucordinaldatClean(employ_datQ16.2.aCollege$Q16.2.a, employ_datQ16.2.aCollege)
#ordinal(employ_datQ16.2.aCollege$CatOutcome, employ_datQ16.2.aCollege$Q3, employ_datQ16.2.aCollege)
prep <- analysisPrep(employ_datQ16.2.aCollege$CatOutcome, employ_datQ16.2.aCollege$Q3, employ_datQ16.2.aCollege)
analysis <- polr(employ_datQ16.2.aCollege$CatOutcome ~ employ_datQ16.2.aCollege$Q3, data=employ_datQ16.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ16.2.aCarer<-multidatClean(Q16.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aCarer$Carer<- factor(employ_datQ16.2.aCarer$Carer)
employ_datQ16.2.aCarer<-SucordinaldatClean(employ_datQ16.2.aCarer$Q16.2.a, employ_datQ16.2.aCarer)
#ordinal(employ_datQ16.2.aCarer$CatOutcome, employ_datQ16.2.aCarer$Carer, employ_datQ16.2.aCarer)
prep <- analysisPrep(employ_datQ16.2.aCarer$CatOutcome, employ_datQ16.2.aCarer$Carer, employ_datQ16.2.aCarer)
analysis <- polr(employ_datQ16.2.aCarer$CatOutcome ~ employ_datQ16.2.aCarer$Carer, data=employ_datQ16.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ16.2.aDisability<-multidatClean(Q16.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aDisability$Disability<- factor(employ_datQ16.2.aDisability$Disability)
employ_datQ16.2.aDisability<-SucordinaldatClean(employ_datQ16.2.aDisability$Q16.2.a, employ_datQ16.2.aDisability)
conTable <- xtabs(~Q16.2.a + Disability, data = employ_datQ16.2.aDisability)
#ordinal(employ_datQ16.2.aDisability$CatOutcome, employ_datQ16.2.aDisability$Disability, employ_datQ16.2.aDisability)
prep <- analysisPrep(employ_datQ16.2.aDisability$CatOutcome, employ_datQ16.2.aDisability$Disability, employ_datQ16.2.aDisability)
analysis <- polr(employ_datQ16.2.aDisability$CatOutcome ~ employ_datQ16.2.aDisability$Disability, data=employ_datQ16.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ16.2.aEthnicity<-multidatClean(Q16.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aEthnicity$Ethnicity<- factor(employ_datQ16.2.aEthnicity$EthnicityCleaned)
employ_datQ16.2.aEthnicity<-SucordinaldatClean(employ_datQ16.2.aEthnicity$Q16.2.a, employ_datQ16.2.aEthnicity)
conTable <- xtabs(~Q16.2.a + EthnicityCleaned, data = employ_datQ16.2.aEthnicity)
conTable
#ordinal(employ_datQ16.2.aEthnicity$CatOutcome, employ_datQ16.2.aEthnicity$EthnicityCleaned, employ_datQ16.2.aEthnicity)
prep <- analysisPrep(employ_datQ16.2.aEthnicity$CatOutcome, employ_datQ16.2.aEthnicity$EthnicityCleaned, employ_datQ16.2.aEthnicity)
analysis <- polr(employ_datQ16.2.aEthnicity$CatOutcome ~ employ_datQ16.2.aEthnicity$EthnicityCleaned, data=employ_datQ16.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ16.2.aFirstGen<-multidatClean(Q16.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aFirstGen$FirstGen<-factor(employ_datQ16.2.aFirstGen$FirstGen)
employ_datQ16.2.aFirstGen<-SucordinaldatClean(employ_datQ16.2.aFirstGen$Q16.2.a, employ_datQ16.2.aFirstGen)
#ordinal(employ_datQ16.2.aFirstGen$CatOutcome, employ_datQ16.2.aFirstGen$FirstGen, employ_datQ16.2.aFirstGen)
prep <- analysisPrep(employ_datQ16.2.aFirstGen$CatOutcome, employ_datQ16.2.aFirstGen$FirstGen, employ_datQ16.2.aFirstGen)
analysis <- polr(employ_datQ16.2.aFirstGen$CatOutcome ~ employ_datQ16.2.aFirstGen$FirstGen, data=employ_datQ16.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ16.2.aGender<-multidatClean(Q16.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aGender$Gender<-factor(employ_datQ16.2.aGender$Gender)
employ_datQ16.2.aGender<-SucordinaldatClean(employ_datQ16.2.aGender$Q16.2.a, employ_datQ16.2.aGender)
#ordinal(employ_datQ16.2.aGender$CatOutcome, employ_datQ16.2.aGender$Gender, employ_datQ16.2.aGender)
prep <- analysisPrep(employ_datQ16.2.aGender$CatOutcome, employ_datQ16.2.aGender$Gender, employ_datQ16.2.aGender)
analysis <- polr(employ_datQ16.2.aGender$CatOutcome ~ employ_datQ16.2.aGender$Gender, data=employ_datQ16.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ16.2.aSexuality<-multidatClean(Q16.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.2.aSexuality$Sexuality<-factor(employ_datQ16.2.aSexuality$Sexuality)
employ_datQ16.2.aSexuality<-SucordinaldatClean(employ_datQ16.2.aSexuality$Q16.2.a, employ_datQ16.2.aSexuality)
#ordinal(employ_datQ16.2.aSexuality$CatOutcome, employ_datQ16.2.aSexuality$Sexuality, employ_datQ16.2.aSexuality)
prep <- analysisPrep(employ_datQ16.2.aSexuality$CatOutcome, employ_datQ16.2.aSexuality$Sexuality, employ_datQ16.2.aSexuality)
analysis <- polr(employ_datQ16.2.aSexuality$CatOutcome ~ employ_datQ16.2.aSexuality$Sexuality, data=employ_datQ16.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q16.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q16 - How successful is your workplace team in demonstrating each leadership characteristic??
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q16.3. Setting and upholding standards in the conduct of research and its application"
"Status"
employ_datQ16.3.a<-multidatClean(Q16.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.3.a + Academic, data = employ_datQ16.3.a)
detach(dat_long)
employ_datQ16.3.a<-SucordinaldatClean(employ_datQ16.3.a$Q16.3.a, employ_datQ16.3.a)
#ordinal(employ_datQ16.3.a$CatOutcome, employ_datQ16.3.a$Academic, employ_datQ16.3.a)
prep <- analysisPrep(employ_datQ16.3.a$CatOutcome, employ_datQ16.3.a$Academic, employ_datQ16.3.a)
analysis <- polr(employ_datQ16.3.a$CatOutcome ~ employ_datQ16.3.a$Academic, data=employ_datQ16.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ16.3.aCollege<-multidatClean(Q16.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.3.a + Q3, data = employ_datQ16.3.aCollege)
conTable
detach(dat_long)
employ_datQ16.3.aCollege$Q3[(employ_datQ16.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ16.3.aCollege$Q3<- factor(employ_datQ16.3.aCollege$Q3)
employ_datQ16.3.aCollege<-SucordinaldatClean(employ_datQ16.3.aCollege$Q16.3.a, employ_datQ16.3.aCollege)
#ordinal(employ_datQ16.3.aCollege$CatOutcome, employ_datQ16.3.aCollege$Q3, employ_datQ16.3.aCollege)
prep <- analysisPrep(employ_datQ16.3.aCollege$CatOutcome, employ_datQ16.3.aCollege$Q3, employ_datQ16.3.aCollege)
analysis <- polr(employ_datQ16.3.aCollege$CatOutcome ~ employ_datQ16.3.aCollege$Q3, data=employ_datQ16.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ16.3.aCarer<-multidatClean(Q16.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aCarer$Carer<- factor(employ_datQ16.3.aCarer$Carer)
employ_datQ16.3.aCarer<-SucordinaldatClean(employ_datQ16.3.aCarer$Q16.3.a, employ_datQ16.3.aCarer)
#ordinal(employ_datQ16.3.aCarer$CatOutcome, employ_datQ16.3.aCarer$Carer, employ_datQ16.3.aCarer)
prep <- analysisPrep(employ_datQ16.3.aCarer$CatOutcome, employ_datQ16.3.aCarer$Carer, employ_datQ16.3.aCarer)
analysis <- polr(employ_datQ16.3.aCarer$CatOutcome ~ employ_datQ16.3.aCarer$Carer, data=employ_datQ16.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ16.3.aDisability<-multidatClean(Q16.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aDisability$Disability<- factor(employ_datQ16.3.aDisability$Disability)
employ_datQ16.3.aDisability<-SucordinaldatClean(employ_datQ16.3.aDisability$Q16.3.a, employ_datQ16.3.aDisability)
conTable <- xtabs(~Q16.3.a + Disability, data = employ_datQ16.3.aDisability)
#ordinal(employ_datQ16.3.aDisability$CatOutcome, employ_datQ16.3.aDisability$Disability, employ_datQ16.3.aDisability)
prep <- analysisPrep(employ_datQ16.3.aDisability$CatOutcome, employ_datQ16.3.aDisability$Disability, employ_datQ16.3.aDisability)
analysis <- polr(employ_datQ16.3.aDisability$CatOutcome ~ employ_datQ16.3.aDisability$Disability, data=employ_datQ16.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ16.3.aEthnicity<-multidatClean(Q16.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aEthnicity$Ethnicity<- factor(employ_datQ16.3.aEthnicity$EthnicityCleaned)
employ_datQ16.3.aEthnicity<-SucordinaldatClean(employ_datQ16.3.aEthnicity$Q16.3.a, employ_datQ16.3.aEthnicity)
conTable <- xtabs(~Q16.3.a + EthnicityCleaned, data = employ_datQ16.3.aEthnicity)
conTable
#ordinal(employ_datQ16.3.aEthnicity$CatOutcome, employ_datQ16.3.aEthnicity$EthnicityCleaned, employ_datQ16.3.aEthnicity)
prep <- analysisPrep(employ_datQ16.3.aEthnicity$CatOutcome, employ_datQ16.3.aEthnicity$EthnicityCleaned, employ_datQ16.3.aEthnicity)
analysis <- polr(employ_datQ16.3.aEthnicity$CatOutcome ~ employ_datQ16.3.aEthnicity$EthnicityCleaned, data=employ_datQ16.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ16.3.aFirstGen<-multidatClean(Q16.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aFirstGen$FirstGen<-factor(employ_datQ16.3.aFirstGen$FirstGen)
employ_datQ16.3.aFirstGen<-SucordinaldatClean(employ_datQ16.3.aFirstGen$Q16.3.a, employ_datQ16.3.aFirstGen)
#ordinal(employ_datQ16.3.aFirstGen$CatOutcome, employ_datQ16.3.aFirstGen$FirstGen, employ_datQ16.3.aFirstGen)
prep <- analysisPrep(employ_datQ16.3.aFirstGen$CatOutcome, employ_datQ16.3.aFirstGen$FirstGen, employ_datQ16.3.aFirstGen)
analysis <- polr(employ_datQ16.3.aFirstGen$CatOutcome ~ employ_datQ16.3.aFirstGen$FirstGen, data=employ_datQ16.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ16.3.aGender<-multidatClean(Q16.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aGender$Gender<-factor(employ_datQ16.3.aGender$Gender)
employ_datQ16.3.aGender<-SucordinaldatClean(employ_datQ16.3.aGender$Q16.3.a, employ_datQ16.3.aGender)
#ordinal(employ_datQ16.3.aGender$CatOutcome, employ_datQ16.3.aGender$Gender, employ_datQ16.3.aGender)
prep <- analysisPrep(employ_datQ16.3.aGender$CatOutcome, employ_datQ16.3.aGender$Gender, employ_datQ16.3.aGender)
analysis <- polr(employ_datQ16.3.aGender$CatOutcome ~ employ_datQ16.3.aGender$Gender, data=employ_datQ16.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ16.3.aSexuality<-multidatClean(Q16.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.3.aSexuality$Sexuality<-factor(employ_datQ16.3.aSexuality$Sexuality)
employ_datQ16.3.aSexuality<-SucordinaldatClean(employ_datQ16.3.aSexuality$Q16.3.a, employ_datQ16.3.aSexuality)
#ordinal(employ_datQ16.3.aSexuality$CatOutcome, employ_datQ16.3.aSexuality$Sexuality, employ_datQ16.3.aSexuality)
prep <- analysisPrep(employ_datQ16.3.aSexuality$CatOutcome, employ_datQ16.3.aSexuality$Sexuality, employ_datQ16.3.aSexuality)
analysis <- polr(employ_datQ16.3.aSexuality$CatOutcome ~ employ_datQ16.3.aSexuality$Sexuality, data=employ_datQ16.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q16.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q16 - How successful is your workplace team in demonstrating each leadership characteristic??
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q16.4. Creating development and career opportunities"
"Status"
employ_datQ16.4.a<-multidatClean(Q16.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.4.a + Academic, data = employ_datQ16.4.a)
detach(dat_long)
employ_datQ16.4.a<-SucordinaldatClean(employ_datQ16.4.a$Q16.4.a, employ_datQ16.4.a)
#ordinal(employ_datQ16.4.a$CatOutcome, employ_datQ16.4.a$Academic, employ_datQ16.4.a)
prep <- analysisPrep(employ_datQ16.4.a$CatOutcome, employ_datQ16.4.a$Academic, employ_datQ16.4.a)
analysis <- polr(employ_datQ16.4.a$CatOutcome ~ employ_datQ16.4.a$Academic, data=employ_datQ16.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ16.4.aCollege<-multidatClean(Q16.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q16.4.a + Q3, data = employ_datQ16.4.aCollege)
conTable
detach(dat_long)
employ_datQ16.4.aCollege$Q3[(employ_datQ16.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ16.4.aCollege$Q3<- factor(employ_datQ16.4.aCollege$Q3)
employ_datQ16.4.aCollege<-SucordinaldatClean(employ_datQ16.4.aCollege$Q16.4.a, employ_datQ16.4.aCollege)
#ordinal(employ_datQ16.4.aCollege$CatOutcome, employ_datQ16.4.aCollege$Q3, employ_datQ16.4.aCollege)
prep <- analysisPrep(employ_datQ16.4.aCollege$CatOutcome, employ_datQ16.4.aCollege$Q3, employ_datQ16.4.aCollege)
analysis <- polr(employ_datQ16.4.aCollege$CatOutcome ~ employ_datQ16.4.aCollege$Q3, data=employ_datQ16.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ16.4.aCarer<-multidatClean(Q16.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aCarer$Carer<- factor(employ_datQ16.4.aCarer$Carer)
employ_datQ16.4.aCarer<-SucordinaldatClean(employ_datQ16.4.aCarer$Q16.4.a, employ_datQ16.4.aCarer)
#ordinal(employ_datQ16.4.aCarer$CatOutcome, employ_datQ16.4.aCarer$Carer, employ_datQ16.4.aCarer)
prep <- analysisPrep(employ_datQ16.4.aCarer$CatOutcome, employ_datQ16.4.aCarer$Carer, employ_datQ16.4.aCarer)
analysis <- polr(employ_datQ16.4.aCarer$CatOutcome ~ employ_datQ16.4.aCarer$Carer, data=employ_datQ16.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ16.4.aDisability<-multidatClean(Q16.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aDisability$Disability<- factor(employ_datQ16.4.aDisability$Disability)
employ_datQ16.4.aDisability<-SucordinaldatClean(employ_datQ16.4.aDisability$Q16.4.a, employ_datQ16.4.aDisability)
conTable <- xtabs(~Q16.4.a + Disability, data = employ_datQ16.4.aDisability)
#ordinal(employ_datQ16.4.aDisability$CatOutcome, employ_datQ16.4.aDisability$Disability, employ_datQ16.4.aDisability)
prep <- analysisPrep(employ_datQ16.4.aDisability$CatOutcome, employ_datQ16.4.aDisability$Disability, employ_datQ16.4.aDisability)
analysis <- polr(employ_datQ16.4.aDisability$CatOutcome ~ employ_datQ16.4.aDisability$Disability, data=employ_datQ16.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ16.4.aEthnicity<-multidatClean(Q16.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aEthnicity$Ethnicity<- factor(employ_datQ16.4.aEthnicity$EthnicityCleaned)
employ_datQ16.4.aEthnicity<-SucordinaldatClean(employ_datQ16.4.aEthnicity$Q16.4.a, employ_datQ16.4.aEthnicity)
conTable <- xtabs(~Q16.4.a + EthnicityCleaned, data = employ_datQ16.4.aEthnicity)
conTable
#ordinal(employ_datQ16.4.aEthnicity$CatOutcome, employ_datQ16.4.aEthnicity$EthnicityCleaned, employ_datQ16.4.aEthnicity)
prep <- analysisPrep(employ_datQ16.4.aEthnicity$CatOutcome, employ_datQ16.4.aEthnicity$EthnicityCleaned, employ_datQ16.4.aEthnicity)
analysis <- polr(employ_datQ16.4.aEthnicity$CatOutcome ~ employ_datQ16.4.aEthnicity$EthnicityCleaned, data=employ_datQ16.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ16.4.aFirstGen<-multidatClean(Q16.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aFirstGen$FirstGen<-factor(employ_datQ16.4.aFirstGen$FirstGen)
employ_datQ16.4.aFirstGen<-SucordinaldatClean(employ_datQ16.4.aFirstGen$Q16.4.a, employ_datQ16.4.aFirstGen)
#ordinal(employ_datQ16.4.aFirstGen$CatOutcome, employ_datQ16.4.aFirstGen$FirstGen, employ_datQ16.4.aFirstGen)
prep <- analysisPrep(employ_datQ16.4.aFirstGen$CatOutcome, employ_datQ16.4.aFirstGen$FirstGen, employ_datQ16.4.aFirstGen)
analysis <- polr(employ_datQ16.4.aFirstGen$CatOutcome ~ employ_datQ16.4.aFirstGen$FirstGen, data=employ_datQ16.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ16.4.aGender<-multidatClean(Q16.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aGender$Gender<-factor(employ_datQ16.4.aGender$Gender)
employ_datQ16.4.aGender<-SucordinaldatClean(employ_datQ16.4.aGender$Q16.4.a, employ_datQ16.4.aGender)
#ordinal(employ_datQ16.4.aGender$CatOutcome, employ_datQ16.4.aGender$Gender, employ_datQ16.4.aGender)
prep <- analysisPrep(employ_datQ16.4.aGender$CatOutcome, employ_datQ16.4.aGender$Gender, employ_datQ16.4.aGender)
analysis <- polr(employ_datQ16.4.aGender$CatOutcome ~ employ_datQ16.4.aGender$Gender, data=employ_datQ16.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ16.4.aSexuality<-multidatClean(Q16.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ16.4.aSexuality$Sexuality<-factor(employ_datQ16.4.aSexuality$Sexuality)
employ_datQ16.4.aSexuality<-SucordinaldatClean(employ_datQ16.4.aSexuality$Q16.4.a, employ_datQ16.4.aSexuality)
#ordinal(employ_datQ16.4.aSexuality$CatOutcome, employ_datQ16.4.aSexuality$Sexuality, employ_datQ16.4.aSexuality)
prep <- analysisPrep(employ_datQ16.4.aSexuality$CatOutcome, employ_datQ16.4.aSexuality$Sexuality, employ_datQ16.4.aSexuality)
analysis <- polr(employ_datQ16.4.aSexuality$CatOutcome ~ employ_datQ16.4.aSexuality$Sexuality, data=employ_datQ16.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q16.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q16.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q17 - How successful is the University of Edinburgh as a whole in demonstrating each leadership characteristic?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q17.1. How successful is the University of Edinburgh as a whole in demonstrating each leadership
characteristic?"
"Status"
employ_datQ17.1.a<-multidatClean(Q17.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.1.a + Academic, data = employ_datQ17.1.a)
detach(dat_long)
employ_datQ17.1.a<-SucordinaldatClean(employ_datQ17.1.a$Q17.1.a, employ_datQ17.1.a)
#ordinal(employ_datQ17.1.a$CatOutcome, employ_datQ17.1.a$Academic, employ_datQ17.1.a)
prep <- analysisPrep(employ_datQ17.1.a$CatOutcome, employ_datQ17.1.a$Academic, employ_datQ17.1.a)
analysis <- polr(employ_datQ17.1.a$CatOutcome ~ employ_datQ17.1.a$Academic, data=employ_datQ17.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ17.1.aCollege<-multidatClean(Q17.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.1.a + Q3, data = employ_datQ17.1.aCollege)
conTable
detach(dat_long)
employ_datQ17.1.aCollege$Q3[(employ_datQ17.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ17.1.aCollege$Q3<- factor(employ_datQ17.1.aCollege$Q3)
employ_datQ17.1.aCollege<-SucordinaldatClean(employ_datQ17.1.aCollege$Q17.1.a, employ_datQ17.1.aCollege)
#ordinal(employ_datQ17.1.aCollege$CatOutcome, employ_datQ17.1.aCollege$Q3, employ_datQ17.1.aCollege)
prep <- analysisPrep(employ_datQ17.1.aCollege$CatOutcome, employ_datQ17.1.aCollege$Q3, employ_datQ17.1.aCollege)
analysis <- polr(employ_datQ17.1.aCollege$CatOutcome ~ employ_datQ17.1.aCollege$Q3, data=employ_datQ17.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ17.1.aCarer<-multidatClean(Q17.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aCarer$Carer<- factor(employ_datQ17.1.aCarer$Carer)
employ_datQ17.1.aCarer<-SucordinaldatClean(employ_datQ17.1.aCarer$Q17.1.a, employ_datQ17.1.aCarer)
#ordinal(employ_datQ17.1.aCarer$CatOutcome, employ_datQ17.1.aCarer$Carer, employ_datQ17.1.aCarer)
prep <- analysisPrep(employ_datQ17.1.aCarer$CatOutcome, employ_datQ17.1.aCarer$Carer, employ_datQ17.1.aCarer)
analysis <- polr(employ_datQ17.1.aCarer$CatOutcome ~ employ_datQ17.1.aCarer$Carer, data=employ_datQ17.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ17.1.aDisability<-multidatClean(Q17.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aDisability$Disability<- factor(employ_datQ17.1.aDisability$Disability)
employ_datQ17.1.aDisability<-SucordinaldatClean(employ_datQ17.1.aDisability$Q17.1.a, employ_datQ17.1.aDisability)
conTable <- xtabs(~Q17.1.a + Disability, data = employ_datQ17.1.aDisability)
#ordinal(employ_datQ17.1.aDisability$CatOutcome, employ_datQ17.1.aDisability$Disability, employ_datQ17.1.aDisability)
prep <- analysisPrep(employ_datQ17.1.aDisability$CatOutcome, employ_datQ17.1.aDisability$Disability, employ_datQ17.1.aDisability)
analysis <- polr(employ_datQ17.1.aDisability$CatOutcome ~ employ_datQ17.1.aDisability$Disability, data=employ_datQ17.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ17.1.aEthnicity<-multidatClean(Q17.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aEthnicity$Ethnicity<- factor(employ_datQ17.1.aEthnicity$EthnicityCleaned)
employ_datQ17.1.aEthnicity<-SucordinaldatClean(employ_datQ17.1.aEthnicity$Q17.1.a, employ_datQ17.1.aEthnicity)
conTable <- xtabs(~Q17.1.a + EthnicityCleaned, data = employ_datQ17.1.aEthnicity)
conTable
#ordinal(employ_datQ17.1.aEthnicity$CatOutcome, employ_datQ17.1.aEthnicity$EthnicityCleaned, employ_datQ17.1.aEthnicity)
prep <- analysisPrep(employ_datQ17.1.aEthnicity$CatOutcome, employ_datQ17.1.aEthnicity$EthnicityCleaned, employ_datQ17.1.aEthnicity)
analysis <- polr(employ_datQ17.1.aEthnicity$CatOutcome ~ employ_datQ17.1.aEthnicity$EthnicityCleaned, data=employ_datQ17.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ17.1.aFirstGen<-multidatClean(Q17.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aFirstGen$FirstGen<-factor(employ_datQ17.1.aFirstGen$FirstGen)
employ_datQ17.1.aFirstGen<-SucordinaldatClean(employ_datQ17.1.aFirstGen$Q17.1.a, employ_datQ17.1.aFirstGen)
#ordinal(employ_datQ17.1.aFirstGen$CatOutcome, employ_datQ17.1.aFirstGen$FirstGen, employ_datQ17.1.aFirstGen)
prep <- analysisPrep(employ_datQ17.1.aFirstGen$CatOutcome, employ_datQ17.1.aFirstGen$FirstGen, employ_datQ17.1.aFirstGen)
analysis <- polr(employ_datQ17.1.aFirstGen$CatOutcome ~ employ_datQ17.1.aFirstGen$FirstGen, data=employ_datQ17.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ17.1.aGender<-multidatClean(Q17.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aGender$Gender<-factor(employ_datQ17.1.aGender$Gender)
employ_datQ17.1.aGender<-SucordinaldatClean(employ_datQ17.1.aGender$Q17.1.a, employ_datQ17.1.aGender)
#ordinal(employ_datQ17.1.aGender$CatOutcome, employ_datQ17.1.aGender$Gender, employ_datQ17.1.aGender)
prep <- analysisPrep(employ_datQ17.1.aGender$CatOutcome, employ_datQ17.1.aGender$Gender, employ_datQ17.1.aGender)
analysis <- polr(employ_datQ17.1.aGender$CatOutcome ~ employ_datQ17.1.aGender$Gender, data=employ_datQ17.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ17.1.aSexuality<-multidatClean(Q17.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.1.aSexuality$Sexuality<-factor(employ_datQ17.1.aSexuality$Sexuality)
employ_datQ17.1.aSexuality<-SucordinaldatClean(employ_datQ17.1.aSexuality$Q17.1.a, employ_datQ17.1.aSexuality)
#ordinal(employ_datQ17.1.aSexuality$CatOutcome, employ_datQ17.1.aSexuality$Sexuality, employ_datQ17.1.aSexuality)
prep <- analysisPrep(employ_datQ17.1.aSexuality$CatOutcome, employ_datQ17.1.aSexuality$Sexuality, employ_datQ17.1.aSexuality)
analysis <- polr(employ_datQ17.1.aSexuality$CatOutcome ~ employ_datQ17.1.aSexuality$Sexuality, data=employ_datQ17.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q17.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q17 - How successful is the University of Edinburgh as a whole in demonstrating each leadership characteristic?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q17.2. Leading and supporting teams of diverse individuals"
"Status"
employ_datQ17.2.a<-multidatClean(Q17.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.2.a + Academic, data = employ_datQ17.2.a)
detach(dat_long)
employ_datQ17.2.a<-SucordinaldatClean(employ_datQ17.2.a$Q17.2.a, employ_datQ17.2.a)
#ordinal(employ_datQ17.2.a$CatOutcome, employ_datQ17.2.a$Academic, employ_datQ17.2.a)
prep <- analysisPrep(employ_datQ17.2.a$CatOutcome, employ_datQ17.2.a$Academic, employ_datQ17.2.a)
analysis <- polr(employ_datQ17.2.a$CatOutcome ~ employ_datQ17.2.a$Academic, data=employ_datQ17.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ17.2.aCollege<-multidatClean(Q17.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.2.a + Q3, data = employ_datQ17.2.aCollege)
conTable
detach(dat_long)
employ_datQ17.2.aCollege$Q3[(employ_datQ17.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ17.2.aCollege$Q3<- factor(employ_datQ17.2.aCollege$Q3)
employ_datQ17.2.aCollege<-SucordinaldatClean(employ_datQ17.2.aCollege$Q17.2.a, employ_datQ17.2.aCollege)
#ordinal(employ_datQ17.2.aCollege$CatOutcome, employ_datQ17.2.aCollege$Q3, employ_datQ17.2.aCollege)
prep <- analysisPrep(employ_datQ17.2.aCollege$CatOutcome, employ_datQ17.2.aCollege$Q3, employ_datQ17.2.aCollege)
analysis <- polr(employ_datQ17.2.aCollege$CatOutcome ~ employ_datQ17.2.aCollege$Q3, data=employ_datQ17.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ17.2.aCarer<-multidatClean(Q17.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aCarer$Carer<- factor(employ_datQ17.2.aCarer$Carer)
employ_datQ17.2.aCarer<-SucordinaldatClean(employ_datQ17.2.aCarer$Q17.2.a, employ_datQ17.2.aCarer)
#ordinal(employ_datQ17.2.aCarer$CatOutcome, employ_datQ17.2.aCarer$Carer, employ_datQ17.2.aCarer)
prep <- analysisPrep(employ_datQ17.2.aCarer$CatOutcome, employ_datQ17.2.aCarer$Carer, employ_datQ17.2.aCarer)
analysis <- polr(employ_datQ17.2.aCarer$CatOutcome ~ employ_datQ17.2.aCarer$Carer, data=employ_datQ17.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ17.2.aDisability<-multidatClean(Q17.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aDisability$Disability<- factor(employ_datQ17.2.aDisability$Disability)
employ_datQ17.2.aDisability<-SucordinaldatClean(employ_datQ17.2.aDisability$Q17.2.a, employ_datQ17.2.aDisability)
conTable <- xtabs(~Q17.2.a + Disability, data = employ_datQ17.2.aDisability)
#ordinal(employ_datQ17.2.aDisability$CatOutcome, employ_datQ17.2.aDisability$Disability, employ_datQ17.2.aDisability)
prep <- analysisPrep(employ_datQ17.2.aDisability$CatOutcome, employ_datQ17.2.aDisability$Disability, employ_datQ17.2.aDisability)
analysis <- polr(employ_datQ17.2.aDisability$CatOutcome ~ employ_datQ17.2.aDisability$Disability, data=employ_datQ17.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ17.2.aEthnicity<-multidatClean(Q17.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aEthnicity$Ethnicity<- factor(employ_datQ17.2.aEthnicity$EthnicityCleaned)
employ_datQ17.2.aEthnicity<-SucordinaldatClean(employ_datQ17.2.aEthnicity$Q17.2.a, employ_datQ17.2.aEthnicity)
conTable <- xtabs(~Q17.2.a + EthnicityCleaned, data = employ_datQ17.2.aEthnicity)
conTable
#ordinal(employ_datQ17.2.aEthnicity$CatOutcome, employ_datQ17.2.aEthnicity$EthnicityCleaned, employ_datQ17.2.aEthnicity)
prep <- analysisPrep(employ_datQ17.2.aEthnicity$CatOutcome, employ_datQ17.2.aEthnicity$EthnicityCleaned, employ_datQ17.2.aEthnicity)
analysis <- polr(employ_datQ17.2.aEthnicity$CatOutcome ~ employ_datQ17.2.aEthnicity$EthnicityCleaned, data=employ_datQ17.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ17.2.aFirstGen<-multidatClean(Q17.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aFirstGen$FirstGen<-factor(employ_datQ17.2.aFirstGen$FirstGen)
employ_datQ17.2.aFirstGen<-SucordinaldatClean(employ_datQ17.2.aFirstGen$Q17.2.a, employ_datQ17.2.aFirstGen)
#ordinal(employ_datQ17.2.aFirstGen$CatOutcome, employ_datQ17.2.aFirstGen$FirstGen, employ_datQ17.2.aFirstGen)
prep <- analysisPrep(employ_datQ17.2.aFirstGen$CatOutcome, employ_datQ17.2.aFirstGen$FirstGen, employ_datQ17.2.aFirstGen)
analysis <- polr(employ_datQ17.2.aFirstGen$CatOutcome ~ employ_datQ17.2.aFirstGen$FirstGen, data=employ_datQ17.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ17.2.aGender<-multidatClean(Q17.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aGender$Gender<-factor(employ_datQ17.2.aGender$Gender)
employ_datQ17.2.aGender<-SucordinaldatClean(employ_datQ17.2.aGender$Q17.2.a, employ_datQ17.2.aGender)
#ordinal(employ_datQ17.2.aGender$CatOutcome, employ_datQ17.2.aGender$Gender, employ_datQ17.2.aGender)
prep <- analysisPrep(employ_datQ17.2.aGender$CatOutcome, employ_datQ17.2.aGender$Gender, employ_datQ17.2.aGender)
analysis <- polr(employ_datQ17.2.aGender$CatOutcome ~ employ_datQ17.2.aGender$Gender, data=employ_datQ17.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ17.2.aSexuality<-multidatClean(Q17.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.2.aSexuality$Sexuality<-factor(employ_datQ17.2.aSexuality$Sexuality)
employ_datQ17.2.aSexuality<-SucordinaldatClean(employ_datQ17.2.aSexuality$Q17.2.a, employ_datQ17.2.aSexuality)
#ordinal(employ_datQ17.2.aSexuality$CatOutcome, employ_datQ17.2.aSexuality$Sexuality, employ_datQ17.2.aSexuality)
prep <- analysisPrep(employ_datQ17.2.aSexuality$CatOutcome, employ_datQ17.2.aSexuality$Sexuality, employ_datQ17.2.aSexuality)
analysis <- polr(employ_datQ17.2.aSexuality$CatOutcome ~ employ_datQ17.2.aSexuality$Sexuality, data=employ_datQ17.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q17.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q17 - How successful is the University of Edinburgh as a whole in demonstrating each leadership characteristic?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q17.3. Setting and upholding standards in the conduct of research and its application"
"Status"
employ_datQ17.3.a<-multidatClean(Q17.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.3.a + Academic, data = employ_datQ17.3.a)
detach(dat_long)
employ_datQ17.3.a<-SucordinaldatClean(employ_datQ17.3.a$Q17.3.a, employ_datQ17.3.a)
#ordinal(employ_datQ17.3.a$CatOutcome, employ_datQ17.3.a$Academic, employ_datQ17.3.a)
prep <- analysisPrep(employ_datQ17.3.a$CatOutcome, employ_datQ17.3.a$Academic, employ_datQ17.3.a)
analysis <- polr(employ_datQ17.3.a$CatOutcome ~ employ_datQ17.3.a$Academic, data=employ_datQ17.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ17.3.aCollege<-multidatClean(Q17.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.3.a + Q3, data = employ_datQ17.3.aCollege)
conTable
detach(dat_long)
employ_datQ17.3.aCollege$Q3[(employ_datQ17.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ17.3.aCollege$Q3<- factor(employ_datQ17.3.aCollege$Q3)
employ_datQ17.3.aCollege<-SucordinaldatClean(employ_datQ17.3.aCollege$Q17.3.a, employ_datQ17.3.aCollege)
#ordinal(employ_datQ17.3.aCollege$CatOutcome, employ_datQ17.3.aCollege$Q3, employ_datQ17.3.aCollege)
prep <- analysisPrep(employ_datQ17.3.aCollege$CatOutcome, employ_datQ17.3.aCollege$Q3, employ_datQ17.3.aCollege)
analysis <- polr(employ_datQ17.3.aCollege$CatOutcome ~ employ_datQ17.3.aCollege$Q3, data=employ_datQ17.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ17.3.aCarer<-multidatClean(Q17.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aCarer$Carer<- factor(employ_datQ17.3.aCarer$Carer)
employ_datQ17.3.aCarer<-SucordinaldatClean(employ_datQ17.3.aCarer$Q17.3.a, employ_datQ17.3.aCarer)
#ordinal(employ_datQ17.3.aCarer$CatOutcome, employ_datQ17.3.aCarer$Carer, employ_datQ17.3.aCarer)
prep <- analysisPrep(employ_datQ17.3.aCarer$CatOutcome, employ_datQ17.3.aCarer$Carer, employ_datQ17.3.aCarer)
analysis <- polr(employ_datQ17.3.aCarer$CatOutcome ~ employ_datQ17.3.aCarer$Carer, data=employ_datQ17.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ17.3.aDisability<-multidatClean(Q17.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aDisability$Disability<- factor(employ_datQ17.3.aDisability$Disability)
employ_datQ17.3.aDisability<-SucordinaldatClean(employ_datQ17.3.aDisability$Q17.3.a, employ_datQ17.3.aDisability)
conTable <- xtabs(~Q17.3.a + Disability, data = employ_datQ17.3.aDisability)
#ordinal(employ_datQ17.3.aDisability$CatOutcome, employ_datQ17.3.aDisability$Disability, employ_datQ17.3.aDisability)
prep <- analysisPrep(employ_datQ17.3.aDisability$CatOutcome, employ_datQ17.3.aDisability$Disability, employ_datQ17.3.aDisability)
analysis <- polr(employ_datQ17.3.aDisability$CatOutcome ~ employ_datQ17.3.aDisability$Disability, data=employ_datQ17.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ17.3.aEthnicity<-multidatClean(Q17.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aEthnicity$Ethnicity<- factor(employ_datQ17.3.aEthnicity$EthnicityCleaned)
employ_datQ17.3.aEthnicity<-SucordinaldatClean(employ_datQ17.3.aEthnicity$Q17.3.a, employ_datQ17.3.aEthnicity)
conTable <- xtabs(~Q17.3.a + EthnicityCleaned, data = employ_datQ17.3.aEthnicity)
conTable
#ordinal(employ_datQ17.3.aEthnicity$CatOutcome, employ_datQ17.3.aEthnicity$EthnicityCleaned, employ_datQ17.3.aEthnicity)
prep <- analysisPrep(employ_datQ17.3.aEthnicity$CatOutcome, employ_datQ17.3.aEthnicity$EthnicityCleaned, employ_datQ17.3.aEthnicity)
analysis <- polr(employ_datQ17.3.aEthnicity$CatOutcome ~ employ_datQ17.3.aEthnicity$EthnicityCleaned, data=employ_datQ17.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ17.3.aFirstGen<-multidatClean(Q17.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aFirstGen$FirstGen<-factor(employ_datQ17.3.aFirstGen$FirstGen)
employ_datQ17.3.aFirstGen<-SucordinaldatClean(employ_datQ17.3.aFirstGen$Q17.3.a, employ_datQ17.3.aFirstGen)
#ordinal(employ_datQ17.3.aFirstGen$CatOutcome, employ_datQ17.3.aFirstGen$FirstGen, employ_datQ17.3.aFirstGen)
prep <- analysisPrep(employ_datQ17.3.aFirstGen$CatOutcome, employ_datQ17.3.aFirstGen$FirstGen, employ_datQ17.3.aFirstGen)
analysis <- polr(employ_datQ17.3.aFirstGen$CatOutcome ~ employ_datQ17.3.aFirstGen$FirstGen, data=employ_datQ17.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ17.3.aGender<-multidatClean(Q17.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aGender$Gender<-factor(employ_datQ17.3.aGender$Gender)
employ_datQ17.3.aGender<-SucordinaldatClean(employ_datQ17.3.aGender$Q17.3.a, employ_datQ17.3.aGender)
#ordinal(employ_datQ17.3.aGender$CatOutcome, employ_datQ17.3.aGender$Gender, employ_datQ17.3.aGender)
prep <- analysisPrep(employ_datQ17.3.aGender$CatOutcome, employ_datQ17.3.aGender$Gender, employ_datQ17.3.aGender)
analysis <- polr(employ_datQ17.3.aGender$CatOutcome ~ employ_datQ17.3.aGender$Gender, data=employ_datQ17.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ17.3.aSexuality<-multidatClean(Q17.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.3.aSexuality$Sexuality<-factor(employ_datQ17.3.aSexuality$Sexuality)
employ_datQ17.3.aSexuality<-SucordinaldatClean(employ_datQ17.3.aSexuality$Q17.3.a, employ_datQ17.3.aSexuality)
#ordinal(employ_datQ17.3.aSexuality$CatOutcome, employ_datQ17.3.aSexuality$Sexuality, employ_datQ17.3.aSexuality)
prep <- analysisPrep(employ_datQ17.3.aSexuality$CatOutcome, employ_datQ17.3.aSexuality$Sexuality, employ_datQ17.3.aSexuality)
analysis <- polr(employ_datQ17.3.aSexuality$CatOutcome ~ employ_datQ17.3.aSexuality$Sexuality, data=employ_datQ17.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q17.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q17 - How successful is the University of Edinburgh as a whole in demonstrating each leadership characteristic?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q17.4. Creating development and career opportunities"
"Status"
employ_datQ17.4.a<-multidatClean(Q17.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.4.a + Academic, data = employ_datQ17.4.a)
detach(dat_long)
employ_datQ17.4.a<-SucordinaldatClean(employ_datQ17.4.a$Q17.4.a, employ_datQ17.4.a)
#ordinal(employ_datQ17.4.a$CatOutcome, employ_datQ17.4.a$Academic, employ_datQ17.4.a)
prep <- analysisPrep(employ_datQ17.4.a$CatOutcome, employ_datQ17.4.a$Academic, employ_datQ17.4.a)
analysis <- polr(employ_datQ17.4.a$CatOutcome ~ employ_datQ17.4.a$Academic, data=employ_datQ17.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ17.4.aCollege<-multidatClean(Q17.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q17.4.a + Q3, data = employ_datQ17.4.aCollege)
conTable
detach(dat_long)
employ_datQ17.4.aCollege$Q3[(employ_datQ17.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ17.4.aCollege$Q3<- factor(employ_datQ17.4.aCollege$Q3)
employ_datQ17.4.aCollege<-SucordinaldatClean(employ_datQ17.4.aCollege$Q17.4.a, employ_datQ17.4.aCollege)
#ordinal(employ_datQ17.4.aCollege$CatOutcome, employ_datQ17.4.aCollege$Q3, employ_datQ17.4.aCollege)
prep <- analysisPrep(employ_datQ17.4.aCollege$CatOutcome, employ_datQ17.4.aCollege$Q3, employ_datQ17.4.aCollege)
analysis <- polr(employ_datQ17.4.aCollege$CatOutcome ~ employ_datQ17.4.aCollege$Q3, data=employ_datQ17.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ17.4.aCarer<-multidatClean(Q17.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aCarer$Carer<- factor(employ_datQ17.4.aCarer$Carer)
employ_datQ17.4.aCarer<-SucordinaldatClean(employ_datQ17.4.aCarer$Q17.4.a, employ_datQ17.4.aCarer)
#ordinal(employ_datQ17.4.aCarer$CatOutcome, employ_datQ17.4.aCarer$Carer, employ_datQ17.4.aCarer)
prep <- analysisPrep(employ_datQ17.4.aCarer$CatOutcome, employ_datQ17.4.aCarer$Carer, employ_datQ17.4.aCarer)
analysis <- polr(employ_datQ17.4.aCarer$CatOutcome ~ employ_datQ17.4.aCarer$Carer, data=employ_datQ17.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ17.4.aDisability<-multidatClean(Q17.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aDisability$Disability<- factor(employ_datQ17.4.aDisability$Disability)
employ_datQ17.4.aDisability<-SucordinaldatClean(employ_datQ17.4.aDisability$Q17.4.a, employ_datQ17.4.aDisability)
conTable <- xtabs(~Q17.4.a + Disability, data = employ_datQ17.4.aDisability)
#ordinal(employ_datQ17.4.aDisability$CatOutcome, employ_datQ17.4.aDisability$Disability, employ_datQ17.4.aDisability)
prep <- analysisPrep(employ_datQ17.4.aDisability$CatOutcome, employ_datQ17.4.aDisability$Disability, employ_datQ17.4.aDisability)
analysis <- polr(employ_datQ17.4.aDisability$CatOutcome ~ employ_datQ17.4.aDisability$Disability, data=employ_datQ17.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ17.4.aEthnicity<-multidatClean(Q17.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aEthnicity$Ethnicity<- factor(employ_datQ17.4.aEthnicity$EthnicityCleaned)
employ_datQ17.4.aEthnicity<-SucordinaldatClean(employ_datQ17.4.aEthnicity$Q17.4.a, employ_datQ17.4.aEthnicity)
conTable <- xtabs(~Q17.4.a + EthnicityCleaned, data = employ_datQ17.4.aEthnicity)
conTable
#ordinal(employ_datQ17.4.aEthnicity$CatOutcome, employ_datQ17.4.aEthnicity$EthnicityCleaned, employ_datQ17.4.aEthnicity)
prep <- analysisPrep(employ_datQ17.4.aEthnicity$CatOutcome, employ_datQ17.4.aEthnicity$EthnicityCleaned, employ_datQ17.4.aEthnicity)
analysis <- polr(employ_datQ17.4.aEthnicity$CatOutcome ~ employ_datQ17.4.aEthnicity$EthnicityCleaned, data=employ_datQ17.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ17.4.aFirstGen<-multidatClean(Q17.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aFirstGen$FirstGen<-factor(employ_datQ17.4.aFirstGen$FirstGen)
employ_datQ17.4.aFirstGen<-SucordinaldatClean(employ_datQ17.4.aFirstGen$Q17.4.a, employ_datQ17.4.aFirstGen)
#ordinal(employ_datQ17.4.aFirstGen$CatOutcome, employ_datQ17.4.aFirstGen$FirstGen, employ_datQ17.4.aFirstGen)
prep <- analysisPrep(employ_datQ17.4.aFirstGen$CatOutcome, employ_datQ17.4.aFirstGen$FirstGen, employ_datQ17.4.aFirstGen)
analysis <- polr(employ_datQ17.4.aFirstGen$CatOutcome ~ employ_datQ17.4.aFirstGen$FirstGen, data=employ_datQ17.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ17.4.aGender<-multidatClean(Q17.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aGender$Gender<-factor(employ_datQ17.4.aGender$Gender)
employ_datQ17.4.aGender<-SucordinaldatClean(employ_datQ17.4.aGender$Q17.4.a, employ_datQ17.4.aGender)
#ordinal(employ_datQ17.4.aGender$CatOutcome, employ_datQ17.4.aGender$Gender, employ_datQ17.4.aGender)
prep <- analysisPrep(employ_datQ17.4.aGender$CatOutcome, employ_datQ17.4.aGender$Gender, employ_datQ17.4.aGender)
analysis <- polr(employ_datQ17.4.aGender$CatOutcome ~ employ_datQ17.4.aGender$Gender, data=employ_datQ17.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ17.4.aSexuality<-multidatClean(Q17.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ17.4.aSexuality$Sexuality<-factor(employ_datQ17.4.aSexuality$Sexuality)
employ_datQ17.4.aSexuality<-SucordinaldatClean(employ_datQ17.4.aSexuality$Q17.4.a, employ_datQ17.4.aSexuality)
#ordinal(employ_datQ17.4.aSexuality$CatOutcome, employ_datQ17.4.aSexuality$Sexuality, employ_datQ17.4.aSexuality)
prep <- analysisPrep(employ_datQ17.4.aSexuality$CatOutcome, employ_datQ17.4.aSexuality$Sexuality, employ_datQ17.4.aSexuality)
analysis <- polr(employ_datQ17.4.aSexuality$CatOutcome ~ employ_datQ17.4.aSexuality$Sexuality, data=employ_datQ17.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q17.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q17.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.1. My working environment promotes a good work-life balance"

"Status"
employ_datQ18.1.a<-multidatClean(Q18.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.1.a + Academic, data = employ_datQ18.1.a)
detach(dat_long)
employ_datQ18.1.a<-ordinaldatClean(employ_datQ18.1.a$Q18.1.a, employ_datQ18.1.a)
#ordinal(employ_datQ18.1.a$CatOutcome, employ_datQ18.1.a$Academic, employ_datQ18.1.a)
prep <- analysisPrep(employ_datQ18.1.a$CatOutcome, employ_datQ18.1.a$Academic, employ_datQ18.1.a)
analysis <- polr(employ_datQ18.1.a$CatOutcome ~ employ_datQ18.1.a$Academic, data=employ_datQ18.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.1.aCollege<-multidatClean(Q18.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.1.a + Q3, data = employ_datQ18.1.aCollege)
conTable
detach(dat_long)
employ_datQ18.1.aCollege$Q3[(employ_datQ18.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.1.aCollege$Q3<- factor(employ_datQ18.1.aCollege$Q3)
employ_datQ18.1.aCollege<-ordinaldatClean(employ_datQ18.1.aCollege$Q18.1.a, employ_datQ18.1.aCollege)
#ordinal(employ_datQ18.1.aCollege$CatOutcome, employ_datQ18.1.aCollege$Q3, employ_datQ18.1.aCollege)
prep <- analysisPrep(employ_datQ18.1.aCollege$CatOutcome, employ_datQ18.1.aCollege$Q3, employ_datQ18.1.aCollege)
analysis <- polr(employ_datQ18.1.aCollege$CatOutcome ~ employ_datQ18.1.aCollege$Q3, data=employ_datQ18.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.1.aCarer<-multidatClean(Q18.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aCarer$Carer<- factor(employ_datQ18.1.aCarer$Carer)
employ_datQ18.1.aCarer<-ordinaldatClean(employ_datQ18.1.aCarer$Q18.1.a, employ_datQ18.1.aCarer)
#ordinal(employ_datQ18.1.aCarer$CatOutcome, employ_datQ18.1.aCarer$Carer, employ_datQ18.1.aCarer)
prep <- analysisPrep(employ_datQ18.1.aCarer$CatOutcome, employ_datQ18.1.aCarer$Carer, employ_datQ18.1.aCarer)
analysis <- polr(employ_datQ18.1.aCarer$CatOutcome ~ employ_datQ18.1.aCarer$Carer, data=employ_datQ18.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.1.aDisability<-multidatClean(Q18.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aDisability$Disability<- factor(employ_datQ18.1.aDisability$Disability)
employ_datQ18.1.aDisability<-ordinaldatClean(employ_datQ18.1.aDisability$Q18.1.a, employ_datQ18.1.aDisability)
conTable <- xtabs(~Q18.1.a + Disability, data = employ_datQ18.1.aDisability)
#ordinal(employ_datQ18.1.aDisability$CatOutcome, employ_datQ18.1.aDisability$Disability, employ_datQ18.1.aDisability)
prep <- analysisPrep(employ_datQ18.1.aDisability$CatOutcome, employ_datQ18.1.aDisability$Disability, employ_datQ18.1.aDisability)
analysis <- polr(employ_datQ18.1.aDisability$CatOutcome ~ employ_datQ18.1.aDisability$Disability, data=employ_datQ18.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.1.aEthnicity<-multidatClean(Q18.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aEthnicity$Ethnicity<- factor(employ_datQ18.1.aEthnicity$EthnicityCleaned)
employ_datQ18.1.aEthnicity<-ordinaldatClean(employ_datQ18.1.aEthnicity$Q18.1.a, employ_datQ18.1.aEthnicity)
conTable <- xtabs(~Q18.1.a + EthnicityCleaned, data = employ_datQ18.1.aEthnicity)
conTable
#ordinal(employ_datQ18.1.aEthnicity$CatOutcome, employ_datQ18.1.aEthnicity$EthnicityCleaned, employ_datQ18.1.aEthnicity)
prep <- analysisPrep(employ_datQ18.1.aEthnicity$CatOutcome, employ_datQ18.1.aEthnicity$EthnicityCleaned, employ_datQ18.1.aEthnicity)
analysis <- polr(employ_datQ18.1.aEthnicity$CatOutcome ~ employ_datQ18.1.aEthnicity$EthnicityCleaned, data=employ_datQ18.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.1.aFirstGen<-multidatClean(Q18.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aFirstGen$FirstGen<-factor(employ_datQ18.1.aFirstGen$FirstGen)
employ_datQ18.1.aFirstGen<-ordinaldatClean(employ_datQ18.1.aFirstGen$Q18.1.a, employ_datQ18.1.aFirstGen)
#ordinal(employ_datQ18.1.aFirstGen$CatOutcome, employ_datQ18.1.aFirstGen$FirstGen, employ_datQ18.1.aFirstGen)
prep <- analysisPrep(employ_datQ18.1.aFirstGen$CatOutcome, employ_datQ18.1.aFirstGen$FirstGen, employ_datQ18.1.aFirstGen)
analysis <- polr(employ_datQ18.1.aFirstGen$CatOutcome ~ employ_datQ18.1.aFirstGen$FirstGen, data=employ_datQ18.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.1.aGender<-multidatClean(Q18.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aGender$Gender<-factor(employ_datQ18.1.aGender$Gender)
employ_datQ18.1.aGender<-ordinaldatClean(employ_datQ18.1.aGender$Q18.1.a, employ_datQ18.1.aGender)
#ordinal(employ_datQ18.1.aGender$CatOutcome, employ_datQ18.1.aGender$Gender, employ_datQ18.1.aGender)
prep <- analysisPrep(employ_datQ18.1.aGender$CatOutcome, employ_datQ18.1.aGender$Gender, employ_datQ18.1.aGender)
analysis <- polr(employ_datQ18.1.aGender$CatOutcome ~ employ_datQ18.1.aGender$Gender, data=employ_datQ18.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.1.aSexuality<-multidatClean(Q18.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1.aSexuality$Sexuality<-factor(employ_datQ18.1.aSexuality$Sexuality)
employ_datQ18.1.aSexuality<-ordinaldatClean(employ_datQ18.1.aSexuality$Q18.1.a, employ_datQ18.1.aSexuality)
#ordinal(employ_datQ18.1.aSexuality$CatOutcome, employ_datQ18.1.aSexuality$Sexuality, employ_datQ18.1.aSexuality)
prep <- analysisPrep(employ_datQ18.1.aSexuality$CatOutcome, employ_datQ18.1.aSexuality$Sexuality, employ_datQ18.1.aSexuality)
analysis <- polr(employ_datQ18.1.aSexuality$CatOutcome ~ employ_datQ18.1.aSexuality$Sexuality, data=employ_datQ18.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.2. My working environment promotes a collaborative culture"

"Status"
employ_datQ18.2.a<-multidatClean(Q18.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.2.a + Academic, data = employ_datQ18.2.a)
detach(dat_long)
employ_datQ18.2.a<-ordinaldatClean(employ_datQ18.2.a$Q18.2.a, employ_datQ18.2.a)
#ordinal(employ_datQ18.2.a$CatOutcome, employ_datQ18.2.a$Academic, employ_datQ18.2.a)
prep <- analysisPrep(employ_datQ18.2.a$CatOutcome, employ_datQ18.2.a$Academic, employ_datQ18.2.a)
analysis <- polr(employ_datQ18.2.a$CatOutcome ~ employ_datQ18.2.a$Academic, data=employ_datQ18.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.2.aCollege<-multidatClean(Q18.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.2.a + Q3, data = employ_datQ18.2.aCollege)
conTable
detach(dat_long)
employ_datQ18.2.aCollege$Q3[(employ_datQ18.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.2.aCollege$Q3<- factor(employ_datQ18.2.aCollege$Q3)
employ_datQ18.2.aCollege<-ordinaldatClean(employ_datQ18.2.aCollege$Q18.2.a, employ_datQ18.2.aCollege)
#ordinal(employ_datQ18.2.aCollege$CatOutcome, employ_datQ18.2.aCollege$Q3, employ_datQ18.2.aCollege)
prep <- analysisPrep(employ_datQ18.2.aCollege$CatOutcome, employ_datQ18.2.aCollege$Q3, employ_datQ18.2.aCollege)
analysis <- polr(employ_datQ18.2.aCollege$CatOutcome ~ employ_datQ18.2.aCollege$Q3, data=employ_datQ18.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.2.aCarer<-multidatClean(Q18.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aCarer$Carer<- factor(employ_datQ18.2.aCarer$Carer)
employ_datQ18.2.aCarer<-ordinaldatClean(employ_datQ18.2.aCarer$Q18.2.a, employ_datQ18.2.aCarer)
#ordinal(employ_datQ18.2.aCarer$CatOutcome, employ_datQ18.2.aCarer$Carer, employ_datQ18.2.aCarer)
prep <- analysisPrep(employ_datQ18.2.aCarer$CatOutcome, employ_datQ18.2.aCarer$Carer, employ_datQ18.2.aCarer)
analysis <- polr(employ_datQ18.2.aCarer$CatOutcome ~ employ_datQ18.2.aCarer$Carer, data=employ_datQ18.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.2.aDisability<-multidatClean(Q18.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aDisability$Disability<- factor(employ_datQ18.2.aDisability$Disability)
employ_datQ18.2.aDisability<-ordinaldatClean(employ_datQ18.2.aDisability$Q18.2.a, employ_datQ18.2.aDisability)
conTable <- xtabs(~Q18.2.a + Disability, data = employ_datQ18.2.aDisability)
#ordinal(employ_datQ18.2.aDisability$CatOutcome, employ_datQ18.2.aDisability$Disability, employ_datQ18.2.aDisability)
prep <- analysisPrep(employ_datQ18.2.aDisability$CatOutcome, employ_datQ18.2.aDisability$Disability, employ_datQ18.2.aDisability)
analysis <- polr(employ_datQ18.2.aDisability$CatOutcome ~ employ_datQ18.2.aDisability$Disability, data=employ_datQ18.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.2.aEthnicity<-multidatClean(Q18.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aEthnicity$Ethnicity<- factor(employ_datQ18.2.aEthnicity$EthnicityCleaned)
employ_datQ18.2.aEthnicity<-ordinaldatClean(employ_datQ18.2.aEthnicity$Q18.2.a, employ_datQ18.2.aEthnicity)
conTable <- xtabs(~Q18.2.a + EthnicityCleaned, data = employ_datQ18.2.aEthnicity)
conTable
#ordinal(employ_datQ18.2.aEthnicity$CatOutcome, employ_datQ18.2.aEthnicity$EthnicityCleaned, employ_datQ18.2.aEthnicity)
prep <- analysisPrep(employ_datQ18.2.aEthnicity$CatOutcome, employ_datQ18.2.aEthnicity$EthnicityCleaned, employ_datQ18.2.aEthnicity)
analysis <- polr(employ_datQ18.2.aEthnicity$CatOutcome ~ employ_datQ18.2.aEthnicity$EthnicityCleaned, data=employ_datQ18.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.2.aFirstGen<-multidatClean(Q18.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aFirstGen$FirstGen<-factor(employ_datQ18.2.aFirstGen$FirstGen)
employ_datQ18.2.aFirstGen<-ordinaldatClean(employ_datQ18.2.aFirstGen$Q18.2.a, employ_datQ18.2.aFirstGen)
#ordinal(employ_datQ18.2.aFirstGen$CatOutcome, employ_datQ18.2.aFirstGen$FirstGen, employ_datQ18.2.aFirstGen)
prep <- analysisPrep(employ_datQ18.2.aFirstGen$CatOutcome, employ_datQ18.2.aFirstGen$FirstGen, employ_datQ18.2.aFirstGen)
analysis <- polr(employ_datQ18.2.aFirstGen$CatOutcome ~ employ_datQ18.2.aFirstGen$FirstGen, data=employ_datQ18.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.2.aGender<-multidatClean(Q18.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aGender$Gender<-factor(employ_datQ18.2.aGender$Gender)
employ_datQ18.2.aGender<-ordinaldatClean(employ_datQ18.2.aGender$Q18.2.a, employ_datQ18.2.aGender)
#ordinal(employ_datQ18.2.aGender$CatOutcome, employ_datQ18.2.aGender$Gender, employ_datQ18.2.aGender)
prep <- analysisPrep(employ_datQ18.2.aGender$CatOutcome, employ_datQ18.2.aGender$Gender, employ_datQ18.2.aGender)
analysis <- polr(employ_datQ18.2.aGender$CatOutcome ~ employ_datQ18.2.aGender$Gender, data=employ_datQ18.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.2.aSexuality<-multidatClean(Q18.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2.aSexuality$Sexuality<-factor(employ_datQ18.2.aSexuality$Sexuality)
employ_datQ18.2.aSexuality<-ordinaldatClean(employ_datQ18.2.aSexuality$Q18.2.a, employ_datQ18.2.aSexuality)
#ordinal(employ_datQ18.2.aSexuality$CatOutcome, employ_datQ18.2.aSexuality$Sexuality, employ_datQ18.2.aSexuality)
prep <- analysisPrep(employ_datQ18.2.aSexuality$CatOutcome, employ_datQ18.2.aSexuality$Sexuality, employ_datQ18.2.aSexuality)
analysis <- polr(employ_datQ18.2.aSexuality$CatOutcome ~ employ_datQ18.2.aSexuality$Sexuality, data=employ_datQ18.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.3. Creativity is welcomed within my working environment in all its forms"

"Status"
employ_datQ18.3.a<-multidatClean(Q18.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.3.a + Academic, data = employ_datQ18.3.a)
detach(dat_long)
employ_datQ18.3.a<-ordinaldatClean(employ_datQ18.3.a$Q18.3.a, employ_datQ18.3.a)
#ordinal(employ_datQ18.3.a$CatOutcome, employ_datQ18.3.a$Academic, employ_datQ18.3.a)
prep <- analysisPrep(employ_datQ18.3.a$CatOutcome, employ_datQ18.3.a$Academic, employ_datQ18.3.a)
analysis <- polr(employ_datQ18.3.a$CatOutcome ~ employ_datQ18.3.a$Academic, data=employ_datQ18.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.3.aCollege<-multidatClean(Q18.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.3.a + Q3, data = employ_datQ18.3.aCollege)
conTable
detach(dat_long)
employ_datQ18.3.aCollege$Q3[(employ_datQ18.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.3.aCollege$Q3<- factor(employ_datQ18.3.aCollege$Q3)
employ_datQ18.3.aCollege<-ordinaldatClean(employ_datQ18.3.aCollege$Q18.3.a, employ_datQ18.3.aCollege)
#ordinal(employ_datQ18.3.aCollege$CatOutcome, employ_datQ18.3.aCollege$Q3, employ_datQ18.3.aCollege)
prep <- analysisPrep(employ_datQ18.3.aCollege$CatOutcome, employ_datQ18.3.aCollege$Q3, employ_datQ18.3.aCollege)
analysis <- polr(employ_datQ18.3.aCollege$CatOutcome ~ employ_datQ18.3.aCollege$Q3, data=employ_datQ18.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.3.aCarer<-multidatClean(Q18.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aCarer$Carer<- factor(employ_datQ18.3.aCarer$Carer)
employ_datQ18.3.aCarer<-ordinaldatClean(employ_datQ18.3.aCarer$Q18.3.a, employ_datQ18.3.aCarer)
#ordinal(employ_datQ18.3.aCarer$CatOutcome, employ_datQ18.3.aCarer$Carer, employ_datQ18.3.aCarer)
prep <- analysisPrep(employ_datQ18.3.aCarer$CatOutcome, employ_datQ18.3.aCarer$Carer, employ_datQ18.3.aCarer)
analysis <- polr(employ_datQ18.3.aCarer$CatOutcome ~ employ_datQ18.3.aCarer$Carer, data=employ_datQ18.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.3.aDisability<-multidatClean(Q18.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aDisability$Disability<- factor(employ_datQ18.3.aDisability$Disability)
employ_datQ18.3.aDisability<-ordinaldatClean(employ_datQ18.3.aDisability$Q18.3.a, employ_datQ18.3.aDisability)
conTable <- xtabs(~Q18.3.a + Disability, data = employ_datQ18.3.aDisability)
#ordinal(employ_datQ18.3.aDisability$CatOutcome, employ_datQ18.3.aDisability$Disability, employ_datQ18.3.aDisability)
prep <- analysisPrep(employ_datQ18.3.aDisability$CatOutcome, employ_datQ18.3.aDisability$Disability, employ_datQ18.3.aDisability)
analysis <- polr(employ_datQ18.3.aDisability$CatOutcome ~ employ_datQ18.3.aDisability$Disability, data=employ_datQ18.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.3.aEthnicity<-multidatClean(Q18.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aEthnicity$Ethnicity<- factor(employ_datQ18.3.aEthnicity$EthnicityCleaned)
employ_datQ18.3.aEthnicity<-ordinaldatClean(employ_datQ18.3.aEthnicity$Q18.3.a, employ_datQ18.3.aEthnicity)
conTable <- xtabs(~Q18.3.a + EthnicityCleaned, data = employ_datQ18.3.aEthnicity)
conTable
#ordinal(employ_datQ18.3.aEthnicity$CatOutcome, employ_datQ18.3.aEthnicity$EthnicityCleaned, employ_datQ18.3.aEthnicity)
prep <- analysisPrep(employ_datQ18.3.aEthnicity$CatOutcome, employ_datQ18.3.aEthnicity$EthnicityCleaned, employ_datQ18.3.aEthnicity)
analysis <- polr(employ_datQ18.3.aEthnicity$CatOutcome ~ employ_datQ18.3.aEthnicity$EthnicityCleaned, data=employ_datQ18.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.3.aFirstGen<-multidatClean(Q18.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aFirstGen$FirstGen<-factor(employ_datQ18.3.aFirstGen$FirstGen)
employ_datQ18.3.aFirstGen<-ordinaldatClean(employ_datQ18.3.aFirstGen$Q18.3.a, employ_datQ18.3.aFirstGen)
#ordinal(employ_datQ18.3.aFirstGen$CatOutcome, employ_datQ18.3.aFirstGen$FirstGen, employ_datQ18.3.aFirstGen)
prep <- analysisPrep(employ_datQ18.3.aFirstGen$CatOutcome, employ_datQ18.3.aFirstGen$FirstGen, employ_datQ18.3.aFirstGen)
analysis <- polr(employ_datQ18.3.aFirstGen$CatOutcome ~ employ_datQ18.3.aFirstGen$FirstGen, data=employ_datQ18.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.3.aGender<-multidatClean(Q18.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aGender$Gender<-factor(employ_datQ18.3.aGender$Gender)
employ_datQ18.3.aGender<-ordinaldatClean(employ_datQ18.3.aGender$Q18.3.a, employ_datQ18.3.aGender)
#ordinal(employ_datQ18.3.aGender$CatOutcome, employ_datQ18.3.aGender$Gender, employ_datQ18.3.aGender)
prep <- analysisPrep(employ_datQ18.3.aGender$CatOutcome, employ_datQ18.3.aGender$Gender, employ_datQ18.3.aGender)
analysis <- polr(employ_datQ18.3.aGender$CatOutcome ~ employ_datQ18.3.aGender$Gender, data=employ_datQ18.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.3.aSexuality<-multidatClean(Q18.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3.aSexuality$Sexuality<-factor(employ_datQ18.3.aSexuality$Sexuality)
employ_datQ18.3.aSexuality<-ordinaldatClean(employ_datQ18.3.aSexuality$Q18.3.a, employ_datQ18.3.aSexuality)
#ordinal(employ_datQ18.3.aSexuality$CatOutcome, employ_datQ18.3.aSexuality$Sexuality, employ_datQ18.3.aSexuality)
prep <- analysisPrep(employ_datQ18.3.aSexuality$CatOutcome, employ_datQ18.3.aSexuality$Sexuality, employ_datQ18.3.aSexuality)
analysis <- polr(employ_datQ18.3.aSexuality$CatOutcome ~ employ_datQ18.3.aSexuality$Sexuality, data=employ_datQ18.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.4. Healthy competition is encouraged within my working environment"

"Status"
employ_datQ18.4.a<-multidatClean(Q18.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.4.a + Academic, data = employ_datQ18.4.a)
detach(dat_long)
employ_datQ18.4.a<-ordinaldatClean(employ_datQ18.4.a$Q18.4.a, employ_datQ18.4.a)
#ordinal(employ_datQ18.4.a$CatOutcome, employ_datQ18.4.a$Academic, employ_datQ18.4.a)
prep <- analysisPrep(employ_datQ18.4.a$CatOutcome, employ_datQ18.4.a$Academic, employ_datQ18.4.a)
analysis <- polr(employ_datQ18.4.a$CatOutcome ~ employ_datQ18.4.a$Academic, data=employ_datQ18.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.4.aCollege<-multidatClean(Q18.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.4.a + Q3, data = employ_datQ18.4.aCollege)
conTable
detach(dat_long)
employ_datQ18.4.aCollege$Q3[(employ_datQ18.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.4.aCollege$Q3<- factor(employ_datQ18.4.aCollege$Q3)
employ_datQ18.4.aCollege<-ordinaldatClean(employ_datQ18.4.aCollege$Q18.4.a, employ_datQ18.4.aCollege)
#ordinal(employ_datQ18.4.aCollege$CatOutcome, employ_datQ18.4.aCollege$Q3, employ_datQ18.4.aCollege)
prep <- analysisPrep(employ_datQ18.4.aCollege$CatOutcome, employ_datQ18.4.aCollege$Q3, employ_datQ18.4.aCollege)
analysis <- polr(employ_datQ18.4.aCollege$CatOutcome ~ employ_datQ18.4.aCollege$Q3, data=employ_datQ18.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.4.aCarer<-multidatClean(Q18.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aCarer$Carer<- factor(employ_datQ18.4.aCarer$Carer)
employ_datQ18.4.aCarer<-ordinaldatClean(employ_datQ18.4.aCarer$Q18.4.a, employ_datQ18.4.aCarer)
#ordinal(employ_datQ18.4.aCarer$CatOutcome, employ_datQ18.4.aCarer$Carer, employ_datQ18.4.aCarer)
prep <- analysisPrep(employ_datQ18.4.aCarer$CatOutcome, employ_datQ18.4.aCarer$Carer, employ_datQ18.4.aCarer)
analysis <- polr(employ_datQ18.4.aCarer$CatOutcome ~ employ_datQ18.4.aCarer$Carer, data=employ_datQ18.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.4.aDisability<-multidatClean(Q18.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aDisability$Disability<- factor(employ_datQ18.4.aDisability$Disability)
employ_datQ18.4.aDisability<-ordinaldatClean(employ_datQ18.4.aDisability$Q18.4.a, employ_datQ18.4.aDisability)
conTable <- xtabs(~Q18.4.a + Disability, data = employ_datQ18.4.aDisability)
#ordinal(employ_datQ18.4.aDisability$CatOutcome, employ_datQ18.4.aDisability$Disability, employ_datQ18.4.aDisability)
prep <- analysisPrep(employ_datQ18.4.aDisability$CatOutcome, employ_datQ18.4.aDisability$Disability, employ_datQ18.4.aDisability)
analysis <- polr(employ_datQ18.4.aDisability$CatOutcome ~ employ_datQ18.4.aDisability$Disability, data=employ_datQ18.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.4.aEthnicity<-multidatClean(Q18.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aEthnicity$Ethnicity<- factor(employ_datQ18.4.aEthnicity$EthnicityCleaned)
employ_datQ18.4.aEthnicity<-ordinaldatClean(employ_datQ18.4.aEthnicity$Q18.4.a, employ_datQ18.4.aEthnicity)
conTable <- xtabs(~Q18.4.a + EthnicityCleaned, data = employ_datQ18.4.aEthnicity)
conTable
#ordinal(employ_datQ18.4.aEthnicity$CatOutcome, employ_datQ18.4.aEthnicity$EthnicityCleaned, employ_datQ18.4.aEthnicity)
prep <- analysisPrep(employ_datQ18.4.aEthnicity$CatOutcome, employ_datQ18.4.aEthnicity$EthnicityCleaned, employ_datQ18.4.aEthnicity)
analysis <- polr(employ_datQ18.4.aEthnicity$CatOutcome ~ employ_datQ18.4.aEthnicity$EthnicityCleaned, data=employ_datQ18.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.4.aFirstGen<-multidatClean(Q18.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aFirstGen$FirstGen<-factor(employ_datQ18.4.aFirstGen$FirstGen)
employ_datQ18.4.aFirstGen<-ordinaldatClean(employ_datQ18.4.aFirstGen$Q18.4.a, employ_datQ18.4.aFirstGen)
#ordinal(employ_datQ18.4.aFirstGen$CatOutcome, employ_datQ18.4.aFirstGen$FirstGen, employ_datQ18.4.aFirstGen)
prep <- analysisPrep(employ_datQ18.4.aFirstGen$CatOutcome, employ_datQ18.4.aFirstGen$FirstGen, employ_datQ18.4.aFirstGen)
analysis <- polr(employ_datQ18.4.aFirstGen$CatOutcome ~ employ_datQ18.4.aFirstGen$FirstGen, data=employ_datQ18.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.4.aGender<-multidatClean(Q18.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aGender$Gender<-factor(employ_datQ18.4.aGender$Gender)
employ_datQ18.4.aGender<-ordinaldatClean(employ_datQ18.4.aGender$Q18.4.a, employ_datQ18.4.aGender)
#ordinal(employ_datQ18.4.aGender$CatOutcome, employ_datQ18.4.aGender$Gender, employ_datQ18.4.aGender)
prep <- analysisPrep(employ_datQ18.4.aGender$CatOutcome, employ_datQ18.4.aGender$Gender, employ_datQ18.4.aGender)
analysis <- polr(employ_datQ18.4.aGender$CatOutcome ~ employ_datQ18.4.aGender$Gender, data=employ_datQ18.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.4.aSexuality<-multidatClean(Q18.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4.aSexuality$Sexuality<-factor(employ_datQ18.4.aSexuality$Sexuality)
employ_datQ18.4.aSexuality<-ordinaldatClean(employ_datQ18.4.aSexuality$Q18.4.a, employ_datQ18.4.aSexuality)
#ordinal(employ_datQ18.4.aSexuality$CatOutcome, employ_datQ18.4.aSexuality$Sexuality, employ_datQ18.4.aSexuality)
prep <- analysisPrep(employ_datQ18.4.aSexuality$CatOutcome, employ_datQ18.4.aSexuality$Sexuality, employ_datQ18.4.aSexuality)
analysis <- polr(employ_datQ18.4.aSexuality$CatOutcome ~ employ_datQ18.4.aSexuality$Sexuality, data=employ_datQ18.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.5. Unhealthy competition is present within my working environment"

"Status"
employ_datQ18.5.a<-multidatClean(Q18.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.5.a + Academic, data = employ_datQ18.5.a)
detach(dat_long)
employ_datQ18.5.a<-ordinaldatCleanNegative(employ_datQ18.5.a$Q18.5.a, employ_datQ18.5.a)
#ordinal(employ_datQ18.5.a$CatOutcome, employ_datQ18.5.a$Academic, employ_datQ18.5.a)
prep <- analysisPrep(employ_datQ18.5.a$CatOutcome, employ_datQ18.5.a$Academic, employ_datQ18.5.a)
analysis <- polr(employ_datQ18.5.a$CatOutcome ~ employ_datQ18.5.a$Academic, data=employ_datQ18.5.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.5.aCollege<-multidatClean(Q18.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.5.a + Q3, data = employ_datQ18.5.aCollege)
conTable
detach(dat_long)
employ_datQ18.5.aCollege$Q3[(employ_datQ18.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.5.aCollege$Q3<- factor(employ_datQ18.5.aCollege$Q3)
employ_datQ18.5.aCollege<-ordinaldatCleanNegative(employ_datQ18.5.aCollege$Q18.5.a, employ_datQ18.5.aCollege)
#ordinal(employ_datQ18.5.aCollege$CatOutcome, employ_datQ18.5.aCollege$Q3, employ_datQ18.5.aCollege)
prep <- analysisPrep(employ_datQ18.5.aCollege$CatOutcome, employ_datQ18.5.aCollege$Q3, employ_datQ18.5.aCollege)
analysis <- polr(employ_datQ18.5.aCollege$CatOutcome ~ employ_datQ18.5.aCollege$Q3, data=employ_datQ18.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.5.aCarer<-multidatClean(Q18.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aCarer$Carer<- factor(employ_datQ18.5.aCarer$Carer)
employ_datQ18.5.aCarer<-ordinaldatCleanNegative(employ_datQ18.5.aCarer$Q18.5.a, employ_datQ18.5.aCarer)
#ordinal(employ_datQ18.5.aCarer$CatOutcome, employ_datQ18.5.aCarer$Carer, employ_datQ18.5.aCarer)
prep <- analysisPrep(employ_datQ18.5.aCarer$CatOutcome, employ_datQ18.5.aCarer$Carer, employ_datQ18.5.aCarer)
analysis <- polr(employ_datQ18.5.aCarer$CatOutcome ~ employ_datQ18.5.aCarer$Carer, data=employ_datQ18.5.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.5.aDisability<-multidatClean(Q18.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aDisability$Disability<- factor(employ_datQ18.5.aDisability$Disability)
employ_datQ18.5.aDisability<-ordinaldatCleanNegative(employ_datQ18.5.aDisability$Q18.5.a, employ_datQ18.5.aDisability)
conTable <- xtabs(~Q18.5.a + Disability, data = employ_datQ18.5.aDisability)
#ordinal(employ_datQ18.5.aDisability$CatOutcome, employ_datQ18.5.aDisability$Disability, employ_datQ18.5.aDisability)
prep <- analysisPrep(employ_datQ18.5.aDisability$CatOutcome, employ_datQ18.5.aDisability$Disability, employ_datQ18.5.aDisability)
analysis <- polr(employ_datQ18.5.aDisability$CatOutcome ~ employ_datQ18.5.aDisability$Disability, data=employ_datQ18.5.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.5.aEthnicity<-multidatClean(Q18.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aEthnicity$Ethnicity<- factor(employ_datQ18.5.aEthnicity$EthnicityCleaned)
employ_datQ18.5.aEthnicity<-ordinaldatCleanNegative(employ_datQ18.5.aEthnicity$Q18.5.a, employ_datQ18.5.aEthnicity)
conTable <- xtabs(~Q18.5.a + EthnicityCleaned, data = employ_datQ18.5.aEthnicity)
conTable
#ordinal(employ_datQ18.5.aEthnicity$CatOutcome, employ_datQ18.5.aEthnicity$EthnicityCleaned, employ_datQ18.5.aEthnicity)
prep <- analysisPrep(employ_datQ18.5.aEthnicity$CatOutcome, employ_datQ18.5.aEthnicity$EthnicityCleaned, employ_datQ18.5.aEthnicity)
analysis <- polr(employ_datQ18.5.aEthnicity$CatOutcome ~ employ_datQ18.5.aEthnicity$EthnicityCleaned, data=employ_datQ18.5.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.5.aFirstGen<-multidatClean(Q18.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aFirstGen$FirstGen<-factor(employ_datQ18.5.aFirstGen$FirstGen)
employ_datQ18.5.aFirstGen<-ordinaldatCleanNegative(employ_datQ18.5.aFirstGen$Q18.5.a, employ_datQ18.5.aFirstGen)
#ordinal(employ_datQ18.5.aFirstGen$CatOutcome, employ_datQ18.5.aFirstGen$FirstGen, employ_datQ18.5.aFirstGen)
prep <- analysisPrep(employ_datQ18.5.aFirstGen$CatOutcome, employ_datQ18.5.aFirstGen$FirstGen, employ_datQ18.5.aFirstGen)
analysis <- polr(employ_datQ18.5.aFirstGen$CatOutcome ~ employ_datQ18.5.aFirstGen$FirstGen, data=employ_datQ18.5.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.5.aGender<-multidatClean(Q18.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aGender$Gender<-factor(employ_datQ18.5.aGender$Gender)
employ_datQ18.5.aGender<-ordinaldatCleanNegative(employ_datQ18.5.aGender$Q18.5.a, employ_datQ18.5.aGender)
#ordinal(employ_datQ18.5.aGender$CatOutcome, employ_datQ18.5.aGender$Gender, employ_datQ18.5.aGender)
prep <- analysisPrep(employ_datQ18.5.aGender$CatOutcome, employ_datQ18.5.aGender$Gender, employ_datQ18.5.aGender)
analysis <- polr(employ_datQ18.5.aGender$CatOutcome ~ employ_datQ18.5.aGender$Gender, data=employ_datQ18.5.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.5.aSexuality<-multidatClean(Q18.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5.aSexuality$Sexuality<-factor(employ_datQ18.5.aSexuality$Sexuality)
employ_datQ18.5.aSexuality<-ordinaldatCleanNegative(employ_datQ18.5.aSexuality$Q18.5.a, employ_datQ18.5.aSexuality)
#ordinal(employ_datQ18.5.aSexuality$CatOutcome, employ_datQ18.5.aSexuality$Sexuality, employ_datQ18.5.aSexuality)
prep <- analysisPrep(employ_datQ18.5.aSexuality$CatOutcome, employ_datQ18.5.aSexuality$Sexuality, employ_datQ18.5.aSexuality)
analysis <- polr(employ_datQ18.5.aSexuality$CatOutcome ~ employ_datQ18.5.aSexuality$Sexuality, data=employ_datQ18.5.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.6. My institution/workplace values speed of results over quality"

"Status"
employ_datQ18.6.a<-multidatClean(Q18.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.6.a + Academic, data = employ_datQ18.6.a)
detach(dat_long)
employ_datQ18.6.a<-ordinaldatCleanNegative(employ_datQ18.6.a$Q18.6.a, employ_datQ18.6.a)
#ordinal(employ_datQ18.6.a$CatOutcome, employ_datQ18.6.a$Academic, employ_datQ18.6.a)
prep <- analysisPrep(employ_datQ18.6.a$CatOutcome, employ_datQ18.6.a$Academic, employ_datQ18.6.a)
analysis <- polr(employ_datQ18.6.a$CatOutcome ~ employ_datQ18.6.a$Academic, data=employ_datQ18.6.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.6.aCollege<-multidatClean(Q18.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.6.a + Q3, data = employ_datQ18.6.aCollege)
conTable
detach(dat_long)
employ_datQ18.6.aCollege$Q3[(employ_datQ18.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.6.aCollege$Q3<- factor(employ_datQ18.6.aCollege$Q3)
employ_datQ18.6.aCollege<-ordinaldatCleanNegative(employ_datQ18.6.aCollege$Q18.6.a, employ_datQ18.6.aCollege)
#ordinal(employ_datQ18.6.aCollege$CatOutcome, employ_datQ18.6.aCollege$Q3, employ_datQ18.6.aCollege)
prep <- analysisPrep(employ_datQ18.6.aCollege$CatOutcome, employ_datQ18.6.aCollege$Q3, employ_datQ18.6.aCollege)
analysis <- polr(employ_datQ18.6.aCollege$CatOutcome ~ employ_datQ18.6.aCollege$Q3, data=employ_datQ18.6.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.6.aCarer<-multidatClean(Q18.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aCarer$Carer<- factor(employ_datQ18.6.aCarer$Carer)
employ_datQ18.6.aCarer<-ordinaldatCleanNegative(employ_datQ18.6.aCarer$Q18.6.a, employ_datQ18.6.aCarer)
#ordinal(employ_datQ18.6.aCarer$CatOutcome, employ_datQ18.6.aCarer$Carer, employ_datQ18.6.aCarer)
prep <- analysisPrep(employ_datQ18.6.aCarer$CatOutcome, employ_datQ18.6.aCarer$Carer, employ_datQ18.6.aCarer)
analysis <- polr(employ_datQ18.6.aCarer$CatOutcome ~ employ_datQ18.6.aCarer$Carer, data=employ_datQ18.6.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.6.aDisability<-multidatClean(Q18.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aDisability$Disability<- factor(employ_datQ18.6.aDisability$Disability)
employ_datQ18.6.aDisability<-ordinaldatCleanNegative(employ_datQ18.6.aDisability$Q18.6.a, employ_datQ18.6.aDisability)
conTable <- xtabs(~Q18.6.a + Disability, data = employ_datQ18.6.aDisability)
#ordinal(employ_datQ18.6.aDisability$CatOutcome, employ_datQ18.6.aDisability$Disability, employ_datQ18.6.aDisability)
prep <- analysisPrep(employ_datQ18.6.aDisability$CatOutcome, employ_datQ18.6.aDisability$Disability, employ_datQ18.6.aDisability)
analysis <- polr(employ_datQ18.6.aDisability$CatOutcome ~ employ_datQ18.6.aDisability$Disability, data=employ_datQ18.6.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.6.aEthnicity<-multidatClean(Q18.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aEthnicity$Ethnicity<- factor(employ_datQ18.6.aEthnicity$EthnicityCleaned)
employ_datQ18.6.aEthnicity<-ordinaldatCleanNegative(employ_datQ18.6.aEthnicity$Q18.6.a, employ_datQ18.6.aEthnicity)
conTable <- xtabs(~Q18.6.a + EthnicityCleaned, data = employ_datQ18.6.aEthnicity)
conTable
#ordinal(employ_datQ18.6.aEthnicity$CatOutcome, employ_datQ18.6.aEthnicity$EthnicityCleaned, employ_datQ18.6.aEthnicity)
prep <- analysisPrep(employ_datQ18.6.aEthnicity$CatOutcome, employ_datQ18.6.aEthnicity$EthnicityCleaned, employ_datQ18.6.aEthnicity)
analysis <- polr(employ_datQ18.6.aEthnicity$CatOutcome ~ employ_datQ18.6.aEthnicity$EthnicityCleaned, data=employ_datQ18.6.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.6.aFirstGen<-multidatClean(Q18.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aFirstGen$FirstGen<-factor(employ_datQ18.6.aFirstGen$FirstGen)
employ_datQ18.6.aFirstGen<-ordinaldatCleanNegative(employ_datQ18.6.aFirstGen$Q18.6.a, employ_datQ18.6.aFirstGen)
#ordinal(employ_datQ18.6.aFirstGen$CatOutcome, employ_datQ18.6.aFirstGen$FirstGen, employ_datQ18.6.aFirstGen)
prep <- analysisPrep(employ_datQ18.6.aFirstGen$CatOutcome, employ_datQ18.6.aFirstGen$FirstGen, employ_datQ18.6.aFirstGen)
analysis <- polr(employ_datQ18.6.aFirstGen$CatOutcome ~ employ_datQ18.6.aFirstGen$FirstGen, data=employ_datQ18.6.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.6.aGender<-multidatClean(Q18.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aGender$Gender<-factor(employ_datQ18.6.aGender$Gender)
employ_datQ18.6.aGender<-ordinaldatCleanNegative(employ_datQ18.6.aGender$Q18.6.a, employ_datQ18.6.aGender)
#ordinal(employ_datQ18.6.aGender$CatOutcome, employ_datQ18.6.aGender$Gender, employ_datQ18.6.aGender)
prep <- analysisPrep(employ_datQ18.6.aGender$CatOutcome, employ_datQ18.6.aGender$Gender, employ_datQ18.6.aGender)
analysis <- polr(employ_datQ18.6.aGender$CatOutcome ~ employ_datQ18.6.aGender$Gender, data=employ_datQ18.6.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.6.aSexuality<-multidatClean(Q18.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6.aSexuality$Sexuality<-factor(employ_datQ18.6.aSexuality$Sexuality)
employ_datQ18.6.aSexuality<-ordinaldatCleanNegative(employ_datQ18.6.aSexuality$Q18.6.a, employ_datQ18.6.aSexuality)
#ordinal(employ_datQ18.6.aSexuality$CatOutcome, employ_datQ18.6.aSexuality$Sexuality, employ_datQ18.6.aSexuality)
prep <- analysisPrep(employ_datQ18.6.aSexuality$CatOutcome, employ_datQ18.6.aSexuality$Sexuality, employ_datQ18.6.aSexuality)
analysis <- polr(employ_datQ18.6.aSexuality$CatOutcome ~ employ_datQ18.6.aSexuality$Sexuality, data=employ_datQ18.6.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.7. My institution/workplace could do more to ensure research practices do not cut corners"

"Status"
employ_datQ18.7.a<-multidatClean(Q18.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.7.a + Academic, data = employ_datQ18.7.a)
detach(dat_long)
employ_datQ18.7.a<-ordinaldatCleanNegative(employ_datQ18.7.a$Q18.7.a, employ_datQ18.7.a)
#ordinal(employ_datQ18.7.a$CatOutcome, employ_datQ18.7.a$Academic, employ_datQ18.7.a)
prep <- analysisPrep(employ_datQ18.7.a$CatOutcome, employ_datQ18.7.a$Academic, employ_datQ18.7.a)
analysis <- polr(employ_datQ18.7.a$CatOutcome ~ employ_datQ18.7.a$Academic, data=employ_datQ18.7.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.7.aCollege<-multidatClean(Q18.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.7.a + Q3, data = employ_datQ18.7.aCollege)
conTable
detach(dat_long)
employ_datQ18.7.aCollege$Q3[(employ_datQ18.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.7.aCollege$Q3<- factor(employ_datQ18.7.aCollege$Q3)
employ_datQ18.7.aCollege<-ordinaldatCleanNegative(employ_datQ18.7.aCollege$Q18.7.a, employ_datQ18.7.aCollege)
#ordinal(employ_datQ18.7.aCollege$CatOutcome, employ_datQ18.7.aCollege$Q3, employ_datQ18.7.aCollege)
prep <- analysisPrep(employ_datQ18.7.aCollege$CatOutcome, employ_datQ18.7.aCollege$Q3, employ_datQ18.7.aCollege)
analysis <- polr(employ_datQ18.7.aCollege$CatOutcome ~ employ_datQ18.7.aCollege$Q3, data=employ_datQ18.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.7.aCarer<-multidatClean(Q18.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aCarer$Carer<- factor(employ_datQ18.7.aCarer$Carer)
employ_datQ18.7.aCarer<-ordinaldatCleanNegative(employ_datQ18.7.aCarer$Q18.7.a, employ_datQ18.7.aCarer)
#ordinal(employ_datQ18.7.aCarer$CatOutcome, employ_datQ18.7.aCarer$Carer, employ_datQ18.7.aCarer)
prep <- analysisPrep(employ_datQ18.7.aCarer$CatOutcome, employ_datQ18.7.aCarer$Carer, employ_datQ18.7.aCarer)
analysis <- polr(employ_datQ18.7.aCarer$CatOutcome ~ employ_datQ18.7.aCarer$Carer, data=employ_datQ18.7.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.7.aDisability<-multidatClean(Q18.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aDisability$Disability<- factor(employ_datQ18.7.aDisability$Disability)
employ_datQ18.7.aDisability<-ordinaldatCleanNegative(employ_datQ18.7.aDisability$Q18.7.a, employ_datQ18.7.aDisability)
conTable <- xtabs(~Q18.7.a + Disability, data = employ_datQ18.7.aDisability)
#ordinal(employ_datQ18.7.aDisability$CatOutcome, employ_datQ18.7.aDisability$Disability, employ_datQ18.7.aDisability)
prep <- analysisPrep(employ_datQ18.7.aDisability$CatOutcome, employ_datQ18.7.aDisability$Disability, employ_datQ18.7.aDisability)
analysis <- polr(employ_datQ18.7.aDisability$CatOutcome ~ employ_datQ18.7.aDisability$Disability, data=employ_datQ18.7.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.7.aEthnicity<-multidatClean(Q18.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aEthnicity$Ethnicity<- factor(employ_datQ18.7.aEthnicity$EthnicityCleaned)
employ_datQ18.7.aEthnicity<-ordinaldatCleanNegative(employ_datQ18.7.aEthnicity$Q18.7.a, employ_datQ18.7.aEthnicity)
conTable <- xtabs(~Q18.7.a + EthnicityCleaned, data = employ_datQ18.7.aEthnicity)
conTable
#ordinal(employ_datQ18.7.aEthnicity$CatOutcome, employ_datQ18.7.aEthnicity$EthnicityCleaned, employ_datQ18.7.aEthnicity)
prep <- analysisPrep(employ_datQ18.7.aEthnicity$CatOutcome, employ_datQ18.7.aEthnicity$EthnicityCleaned, employ_datQ18.7.aEthnicity)
analysis <- polr(employ_datQ18.7.aEthnicity$CatOutcome ~ employ_datQ18.7.aEthnicity$EthnicityCleaned, data=employ_datQ18.7.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.7.aFirstGen<-multidatClean(Q18.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aFirstGen$FirstGen<-factor(employ_datQ18.7.aFirstGen$FirstGen)
employ_datQ18.7.aFirstGen<-ordinaldatCleanNegative(employ_datQ18.7.aFirstGen$Q18.7.a, employ_datQ18.7.aFirstGen)
#ordinal(employ_datQ18.7.aFirstGen$CatOutcome, employ_datQ18.7.aFirstGen$FirstGen, employ_datQ18.7.aFirstGen)
prep <- analysisPrep(employ_datQ18.7.aFirstGen$CatOutcome, employ_datQ18.7.aFirstGen$FirstGen, employ_datQ18.7.aFirstGen)
analysis <- polr(employ_datQ18.7.aFirstGen$CatOutcome ~ employ_datQ18.7.aFirstGen$FirstGen, data=employ_datQ18.7.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.7.aGender<-multidatClean(Q18.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aGender$Gender<-factor(employ_datQ18.7.aGender$Gender)
employ_datQ18.7.aGender<-ordinaldatCleanNegative(employ_datQ18.7.aGender$Q18.7.a, employ_datQ18.7.aGender)
#ordinal(employ_datQ18.7.aGender$CatOutcome, employ_datQ18.7.aGender$Gender, employ_datQ18.7.aGender)
prep <- analysisPrep(employ_datQ18.7.aGender$CatOutcome, employ_datQ18.7.aGender$Gender, employ_datQ18.7.aGender)
analysis <- polr(employ_datQ18.7.aGender$CatOutcome ~ employ_datQ18.7.aGender$Gender, data=employ_datQ18.7.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.7.aSexuality<-multidatClean(Q18.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7.aSexuality$Sexuality<-factor(employ_datQ18.7.aSexuality$Sexuality)
employ_datQ18.7.aSexuality<-ordinaldatCleanNegative(employ_datQ18.7.aSexuality$Q18.7.a, employ_datQ18.7.aSexuality)
#ordinal(employ_datQ18.7.aSexuality$CatOutcome, employ_datQ18.7.aSexuality$Sexuality, employ_datQ18.7.aSexuality)
prep <- analysisPrep(employ_datQ18.7.aSexuality$CatOutcome, employ_datQ18.7.aSexuality$Sexuality, employ_datQ18.7.aSexuality)
analysis <- polr(employ_datQ18.7.aSexuality$CatOutcome ~ employ_datQ18.7.aSexuality$Sexuality, data=employ_datQ18.7.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.8. Rigour of results is considered an important research outcome by my institution/workplace"

"Status"
employ_datQ18.8.a<-multidatClean(Q18.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.8.a + Academic, data = employ_datQ18.8.a)
detach(dat_long)
employ_datQ18.8.a<-ordinaldatClean(employ_datQ18.8.a$Q18.8.a, employ_datQ18.8.a)
#ordinal(employ_datQ18.8.a$CatOutcome, employ_datQ18.8.a$Academic, employ_datQ18.8.a)
prep <- analysisPrep(employ_datQ18.8.a$CatOutcome, employ_datQ18.8.a$Academic, employ_datQ18.8.a)
analysis <- polr(employ_datQ18.8.a$CatOutcome ~ employ_datQ18.8.a$Academic, data=employ_datQ18.8.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.8.aCollege<-multidatClean(Q18.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.8.a + Q3, data = employ_datQ18.8.aCollege)
conTable
detach(dat_long)
employ_datQ18.8.aCollege$Q3[(employ_datQ18.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.8.aCollege$Q3<- factor(employ_datQ18.8.aCollege$Q3)
employ_datQ18.8.aCollege<-ordinaldatClean(employ_datQ18.8.aCollege$Q18.8.a, employ_datQ18.8.aCollege)
#ordinal(employ_datQ18.8.aCollege$CatOutcome, employ_datQ18.8.aCollege$Q3, employ_datQ18.8.aCollege)
prep <- analysisPrep(employ_datQ18.8.aCollege$CatOutcome, employ_datQ18.8.aCollege$Q3, employ_datQ18.8.aCollege)
analysis <- polr(employ_datQ18.8.aCollege$CatOutcome ~ employ_datQ18.8.aCollege$Q3, data=employ_datQ18.8.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.8.aCarer<-multidatClean(Q18.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aCarer$Carer<- factor(employ_datQ18.8.aCarer$Carer)
employ_datQ18.8.aCarer<-ordinaldatClean(employ_datQ18.8.aCarer$Q18.8.a, employ_datQ18.8.aCarer)
#ordinal(employ_datQ18.8.aCarer$CatOutcome, employ_datQ18.8.aCarer$Carer, employ_datQ18.8.aCarer)
prep <- analysisPrep(employ_datQ18.8.aCarer$CatOutcome, employ_datQ18.8.aCarer$Carer, employ_datQ18.8.aCarer)
analysis <- polr(employ_datQ18.8.aCarer$CatOutcome ~ employ_datQ18.8.aCarer$Carer, data=employ_datQ18.8.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.8.aDisability<-multidatClean(Q18.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aDisability$Disability<- factor(employ_datQ18.8.aDisability$Disability)
employ_datQ18.8.aDisability<-ordinaldatClean(employ_datQ18.8.aDisability$Q18.8.a, employ_datQ18.8.aDisability)
conTable <- xtabs(~Q18.8.a + Disability, data = employ_datQ18.8.aDisability)
#ordinal(employ_datQ18.8.aDisability$CatOutcome, employ_datQ18.8.aDisability$Disability, employ_datQ18.8.aDisability)
prep <- analysisPrep(employ_datQ18.8.aDisability$CatOutcome, employ_datQ18.8.aDisability$Disability, employ_datQ18.8.aDisability)
analysis <- polr(employ_datQ18.8.aDisability$CatOutcome ~ employ_datQ18.8.aDisability$Disability, data=employ_datQ18.8.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.8.aEthnicity<-multidatClean(Q18.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aEthnicity$Ethnicity<- factor(employ_datQ18.8.aEthnicity$EthnicityCleaned)
employ_datQ18.8.aEthnicity<-ordinaldatClean(employ_datQ18.8.aEthnicity$Q18.8.a, employ_datQ18.8.aEthnicity)
conTable <- xtabs(~Q18.8.a + EthnicityCleaned, data = employ_datQ18.8.aEthnicity)
conTable
#ordinal(employ_datQ18.8.aEthnicity$CatOutcome, employ_datQ18.8.aEthnicity$EthnicityCleaned, employ_datQ18.8.aEthnicity)
prep <- analysisPrep(employ_datQ18.8.aEthnicity$CatOutcome, employ_datQ18.8.aEthnicity$EthnicityCleaned, employ_datQ18.8.aEthnicity)
analysis <- polr(employ_datQ18.8.aEthnicity$CatOutcome ~ employ_datQ18.8.aEthnicity$EthnicityCleaned, data=employ_datQ18.8.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.8.aFirstGen<-multidatClean(Q18.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aFirstGen$FirstGen<-factor(employ_datQ18.8.aFirstGen$FirstGen)
employ_datQ18.8.aFirstGen<-ordinaldatClean(employ_datQ18.8.aFirstGen$Q18.8.a, employ_datQ18.8.aFirstGen)
#ordinal(employ_datQ18.8.aFirstGen$CatOutcome, employ_datQ18.8.aFirstGen$FirstGen, employ_datQ18.8.aFirstGen)
prep <- analysisPrep(employ_datQ18.8.aFirstGen$CatOutcome, employ_datQ18.8.aFirstGen$FirstGen, employ_datQ18.8.aFirstGen)
analysis <- polr(employ_datQ18.8.aFirstGen$CatOutcome ~ employ_datQ18.8.aFirstGen$FirstGen, data=employ_datQ18.8.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.8.aGender<-multidatClean(Q18.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aGender$Gender<-factor(employ_datQ18.8.aGender$Gender)
employ_datQ18.8.aGender<-ordinaldatClean(employ_datQ18.8.aGender$Q18.8.a, employ_datQ18.8.aGender)
#ordinal(employ_datQ18.8.aGender$CatOutcome, employ_datQ18.8.aGender$Gender, employ_datQ18.8.aGender)
prep <- analysisPrep(employ_datQ18.8.aGender$CatOutcome, employ_datQ18.8.aGender$Gender, employ_datQ18.8.aGender)
analysis <- polr(employ_datQ18.8.aGender$CatOutcome ~ employ_datQ18.8.aGender$Gender, data=employ_datQ18.8.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.8.aSexuality<-multidatClean(Q18.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8.aSexuality$Sexuality<-factor(employ_datQ18.8.aSexuality$Sexuality)
employ_datQ18.8.aSexuality<-ordinaldatClean(employ_datQ18.8.aSexuality$Q18.8.a, employ_datQ18.8.aSexuality)
#ordinal(employ_datQ18.8.aSexuality$CatOutcome, employ_datQ18.8.aSexuality$Sexuality, employ_datQ18.8.aSexuality)
prep <- analysisPrep(employ_datQ18.8.aSexuality$CatOutcome, employ_datQ18.8.aSexuality$Sexuality, employ_datQ18.8.aSexuality)
analysis <- polr(employ_datQ18.8.aSexuality$CatOutcome ~ employ_datQ18.8.aSexuality$Sexuality, data=employ_datQ18.8.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.8.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.9. My institution/workplace places more value on meeting metrics, than it does on research quality"

"Status"
employ_datQ18.9.a<-multidatClean(Q18.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.9.a + Academic, data = employ_datQ18.9.a)
detach(dat_long)
employ_datQ18.9.a<-ordinaldatCleanNegative(employ_datQ18.9.a$Q18.9.a, employ_datQ18.9.a)
#ordinal(employ_datQ18.9.a$CatOutcome, employ_datQ18.9.a$Academic, employ_datQ18.9.a)
prep <- analysisPrep(employ_datQ18.9.a$CatOutcome, employ_datQ18.9.a$Academic, employ_datQ18.9.a)
analysis <- polr(employ_datQ18.9.a$CatOutcome ~ employ_datQ18.9.a$Academic, data=employ_datQ18.9.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.9.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.9.aCollege<-multidatClean(Q18.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.9.a + Q3, data = employ_datQ18.9.aCollege)
conTable
detach(dat_long)
employ_datQ18.9.aCollege$Q3[(employ_datQ18.9.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.9.aCollege$Q3<- factor(employ_datQ18.9.aCollege$Q3)
employ_datQ18.9.aCollege<-ordinaldatCleanNegative(employ_datQ18.9.aCollege$Q18.9.a, employ_datQ18.9.aCollege)
#ordinal(employ_datQ18.9.aCollege$CatOutcome, employ_datQ18.9.aCollege$Q3, employ_datQ18.9.aCollege)
prep <- analysisPrep(employ_datQ18.9.aCollege$CatOutcome, employ_datQ18.9.aCollege$Q3, employ_datQ18.9.aCollege)
analysis <- polr(employ_datQ18.9.aCollege$CatOutcome ~ employ_datQ18.9.aCollege$Q3, data=employ_datQ18.9.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.9.aCarer<-multidatClean(Q18.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aCarer$Carer<- factor(employ_datQ18.9.aCarer$Carer)
employ_datQ18.9.aCarer<-ordinaldatCleanNegative(employ_datQ18.9.aCarer$Q18.9.a, employ_datQ18.9.aCarer)
#ordinal(employ_datQ18.9.aCarer$CatOutcome, employ_datQ18.9.aCarer$Carer, employ_datQ18.9.aCarer)
prep <- analysisPrep(employ_datQ18.9.aCarer$CatOutcome, employ_datQ18.9.aCarer$Carer, employ_datQ18.9.aCarer)
analysis <- polr(employ_datQ18.9.aCarer$CatOutcome ~ employ_datQ18.9.aCarer$Carer, data=employ_datQ18.9.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.9.aDisability<-multidatClean(Q18.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aDisability$Disability<- factor(employ_datQ18.9.aDisability$Disability)
employ_datQ18.9.aDisability<-ordinaldatCleanNegative(employ_datQ18.9.aDisability$Q18.9.a, employ_datQ18.9.aDisability)
conTable <- xtabs(~Q18.9.a + Disability, data = employ_datQ18.9.aDisability)
#ordinal(employ_datQ18.9.aDisability$CatOutcome, employ_datQ18.9.aDisability$Disability, employ_datQ18.9.aDisability)
prep <- analysisPrep(employ_datQ18.9.aDisability$CatOutcome, employ_datQ18.9.aDisability$Disability, employ_datQ18.9.aDisability)
analysis <- polr(employ_datQ18.9.aDisability$CatOutcome ~ employ_datQ18.9.aDisability$Disability, data=employ_datQ18.9.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.9.aEthnicity<-multidatClean(Q18.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aEthnicity$Ethnicity<- factor(employ_datQ18.9.aEthnicity$EthnicityCleaned)
employ_datQ18.9.aEthnicity<-ordinaldatCleanNegative(employ_datQ18.9.aEthnicity$Q18.9.a, employ_datQ18.9.aEthnicity)
conTable <- xtabs(~Q18.9.a + EthnicityCleaned, data = employ_datQ18.9.aEthnicity)
conTable
#ordinal(employ_datQ18.9.aEthnicity$CatOutcome, employ_datQ18.9.aEthnicity$EthnicityCleaned, employ_datQ18.9.aEthnicity)
prep <- analysisPrep(employ_datQ18.9.aEthnicity$CatOutcome, employ_datQ18.9.aEthnicity$EthnicityCleaned, employ_datQ18.9.aEthnicity)
analysis <- polr(employ_datQ18.9.aEthnicity$CatOutcome ~ employ_datQ18.9.aEthnicity$EthnicityCleaned, data=employ_datQ18.9.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.9.aFirstGen<-multidatClean(Q18.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aFirstGen$FirstGen<-factor(employ_datQ18.9.aFirstGen$FirstGen)
employ_datQ18.9.aFirstGen<-ordinaldatCleanNegative(employ_datQ18.9.aFirstGen$Q18.9.a, employ_datQ18.9.aFirstGen)
#ordinal(employ_datQ18.9.aFirstGen$CatOutcome, employ_datQ18.9.aFirstGen$FirstGen, employ_datQ18.9.aFirstGen)
prep <- analysisPrep(employ_datQ18.9.aFirstGen$CatOutcome, employ_datQ18.9.aFirstGen$FirstGen, employ_datQ18.9.aFirstGen)
analysis <- polr(employ_datQ18.9.aFirstGen$CatOutcome ~ employ_datQ18.9.aFirstGen$FirstGen, data=employ_datQ18.9.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.9.aGender<-multidatClean(Q18.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aGender$Gender<-factor(employ_datQ18.9.aGender$Gender)
employ_datQ18.9.aGender<-ordinaldatCleanNegative(employ_datQ18.9.aGender$Q18.9.a, employ_datQ18.9.aGender)
#ordinal(employ_datQ18.9.aGender$CatOutcome, employ_datQ18.9.aGender$Gender, employ_datQ18.9.aGender)
prep <- analysisPrep(employ_datQ18.9.aGender$CatOutcome, employ_datQ18.9.aGender$Gender, employ_datQ18.9.aGender)
analysis <- polr(employ_datQ18.9.aGender$CatOutcome ~ employ_datQ18.9.aGender$Gender, data=employ_datQ18.9.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.9.aSexuality<-multidatClean(Q18.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9.aSexuality$Sexuality<-factor(employ_datQ18.9.aSexuality$Sexuality)
employ_datQ18.9.aSexuality<-ordinaldatCleanNegative(employ_datQ18.9.aSexuality$Q18.9.a, employ_datQ18.9.aSexuality)
#ordinal(employ_datQ18.9.aSexuality$CatOutcome, employ_datQ18.9.aSexuality$Sexuality, employ_datQ18.9.aSexuality)
prep <- analysisPrep(employ_datQ18.9.aSexuality$CatOutcome, employ_datQ18.9.aSexuality$Sexuality, employ_datQ18.9.aSexuality)
analysis <- polr(employ_datQ18.9.aSexuality$CatOutcome ~ employ_datQ18.9.aSexuality$Sexuality, data=employ_datQ18.9.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.9.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.9.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.10. I am confident that my institution/workplace would listen and take action if I raised a concern"

"Status"
employ_datQ18.10.a<-multidatClean(Q18.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.10.a + Academic, data = employ_datQ18.10.a)
detach(dat_long)
employ_datQ18.10.a<-ordinaldatClean(employ_datQ18.10.a$Q18.10.a, employ_datQ18.10.a)
#ordinal(employ_datQ18.10.a$CatOutcome, employ_datQ18.10.a$Academic, employ_datQ18.10.a)
prep <- analysisPrep(employ_datQ18.10.a$CatOutcome, employ_datQ18.10.a$Academic, employ_datQ18.10.a)
analysis <- polr(employ_datQ18.10.a$CatOutcome ~ employ_datQ18.10.a$Academic, data=employ_datQ18.10.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.10.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.10.aCollege<-multidatClean(Q18.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.10.a + Q3, data = employ_datQ18.10.aCollege)
conTable
detach(dat_long)
employ_datQ18.10.aCollege$Q3[(employ_datQ18.10.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.10.aCollege$Q3<- factor(employ_datQ18.10.aCollege$Q3)
employ_datQ18.10.aCollege<-ordinaldatClean(employ_datQ18.10.aCollege$Q18.10.a, employ_datQ18.10.aCollege)
#ordinal(employ_datQ18.10.aCollege$CatOutcome, employ_datQ18.10.aCollege$Q3, employ_datQ18.10.aCollege)
prep <- analysisPrep(employ_datQ18.10.aCollege$CatOutcome, employ_datQ18.10.aCollege$Q3, employ_datQ18.10.aCollege)
analysis <- polr(employ_datQ18.10.aCollege$CatOutcome ~ employ_datQ18.10.aCollege$Q3, data=employ_datQ18.10.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.10.aCarer<-multidatClean(Q18.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aCarer$Carer<- factor(employ_datQ18.10.aCarer$Carer)
employ_datQ18.10.aCarer<-ordinaldatClean(employ_datQ18.10.aCarer$Q18.10.a, employ_datQ18.10.aCarer)
#ordinal(employ_datQ18.10.aCarer$CatOutcome, employ_datQ18.10.aCarer$Carer, employ_datQ18.10.aCarer)
prep <- analysisPrep(employ_datQ18.10.aCarer$CatOutcome, employ_datQ18.10.aCarer$Carer, employ_datQ18.10.aCarer)
analysis <- polr(employ_datQ18.10.aCarer$CatOutcome ~ employ_datQ18.10.aCarer$Carer, data=employ_datQ18.10.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.10.aDisability<-multidatClean(Q18.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aDisability$Disability<- factor(employ_datQ18.10.aDisability$Disability)
employ_datQ18.10.aDisability<-ordinaldatClean(employ_datQ18.10.aDisability$Q18.10.a, employ_datQ18.10.aDisability)
conTable <- xtabs(~Q18.10.a + Disability, data = employ_datQ18.10.aDisability)
#ordinal(employ_datQ18.10.aDisability$CatOutcome, employ_datQ18.10.aDisability$Disability, employ_datQ18.10.aDisability)
prep <- analysisPrep(employ_datQ18.10.aDisability$CatOutcome, employ_datQ18.10.aDisability$Disability, employ_datQ18.10.aDisability)
analysis <- polr(employ_datQ18.10.aDisability$CatOutcome ~ employ_datQ18.10.aDisability$Disability, data=employ_datQ18.10.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.10.aEthnicity<-multidatClean(Q18.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aEthnicity$Ethnicity<- factor(employ_datQ18.10.aEthnicity$EthnicityCleaned)
employ_datQ18.10.aEthnicity<-ordinaldatClean(employ_datQ18.10.aEthnicity$Q18.10.a, employ_datQ18.10.aEthnicity)
conTable <- xtabs(~Q18.10.a + EthnicityCleaned, data = employ_datQ18.10.aEthnicity)
conTable
#ordinal(employ_datQ18.10.aEthnicity$CatOutcome, employ_datQ18.10.aEthnicity$EthnicityCleaned, employ_datQ18.10.aEthnicity)
prep <- analysisPrep(employ_datQ18.10.aEthnicity$CatOutcome, employ_datQ18.10.aEthnicity$EthnicityCleaned, employ_datQ18.10.aEthnicity)
analysis <- polr(employ_datQ18.10.aEthnicity$CatOutcome ~ employ_datQ18.10.aEthnicity$EthnicityCleaned, data=employ_datQ18.10.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.10.aFirstGen<-multidatClean(Q18.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aFirstGen$FirstGen<-factor(employ_datQ18.10.aFirstGen$FirstGen)
employ_datQ18.10.aFirstGen<-ordinaldatClean(employ_datQ18.10.aFirstGen$Q18.10.a, employ_datQ18.10.aFirstGen)
#ordinal(employ_datQ18.10.aFirstGen$CatOutcome, employ_datQ18.10.aFirstGen$FirstGen, employ_datQ18.10.aFirstGen)
prep <- analysisPrep(employ_datQ18.10.aFirstGen$CatOutcome, employ_datQ18.10.aFirstGen$FirstGen, employ_datQ18.10.aFirstGen)
analysis <- polr(employ_datQ18.10.aFirstGen$CatOutcome ~ employ_datQ18.10.aFirstGen$FirstGen, data=employ_datQ18.10.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.10.aGender<-multidatClean(Q18.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aGender$Gender<-factor(employ_datQ18.10.aGender$Gender)
employ_datQ18.10.aGender<-ordinaldatClean(employ_datQ18.10.aGender$Q18.10.a, employ_datQ18.10.aGender)
#ordinal(employ_datQ18.10.aGender$CatOutcome, employ_datQ18.10.aGender$Gender, employ_datQ18.10.aGender)
prep <- analysisPrep(employ_datQ18.10.aGender$CatOutcome, employ_datQ18.10.aGender$Gender, employ_datQ18.10.aGender)
analysis <- polr(employ_datQ18.10.aGender$CatOutcome ~ employ_datQ18.10.aGender$Gender, data=employ_datQ18.10.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.10.aSexuality<-multidatClean(Q18.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10.aSexuality$Sexuality<-factor(employ_datQ18.10.aSexuality$Sexuality)
employ_datQ18.10.aSexuality<-ordinaldatClean(employ_datQ18.10.aSexuality$Q18.10.a, employ_datQ18.10.aSexuality)
#ordinal(employ_datQ18.10.aSexuality$CatOutcome, employ_datQ18.10.aSexuality$Sexuality, employ_datQ18.10.aSexuality)
prep <- analysisPrep(employ_datQ18.10.aSexuality$CatOutcome, employ_datQ18.10.aSexuality$Sexuality, employ_datQ18.10.aSexuality)
analysis <- polr(employ_datQ18.10.aSexuality$CatOutcome ~ employ_datQ18.10.aSexuality$Sexuality, data=employ_datQ18.10.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.10.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.10.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.11. The culture around research in my working environment supports my ability to do good quality research"

"Status"
employ_datQ18.11.a<-multidatClean(Q18.11.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.11.a + Academic, data = employ_datQ18.11.a)
detach(dat_long)
employ_datQ18.11.a<-ordinaldatClean(employ_datQ18.11.a$Q18.11.a, employ_datQ18.11.a)
#ordinal(employ_datQ18.11.a$CatOutcome, employ_datQ18.11.a$Academic, employ_datQ18.11.a)
prep <- analysisPrep(employ_datQ18.11.a$CatOutcome, employ_datQ18.11.a$Academic, employ_datQ18.11.a)
analysis <- polr(employ_datQ18.11.a$CatOutcome ~ employ_datQ18.11.a$Academic, data=employ_datQ18.11.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.11.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.11.aCollege<-multidatClean(Q18.11.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.11.a + Q3, data = employ_datQ18.11.aCollege)
conTable
detach(dat_long)
employ_datQ18.11.aCollege$Q3[(employ_datQ18.11.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.11.aCollege$Q3<- factor(employ_datQ18.11.aCollege$Q3)
employ_datQ18.11.aCollege<-ordinaldatClean(employ_datQ18.11.aCollege$Q18.11.a, employ_datQ18.11.aCollege)
#ordinal(employ_datQ18.11.aCollege$CatOutcome, employ_datQ18.11.aCollege$Q3, employ_datQ18.11.aCollege)
prep <- analysisPrep(employ_datQ18.11.aCollege$CatOutcome, employ_datQ18.11.aCollege$Q3, employ_datQ18.11.aCollege)
analysis <- polr(employ_datQ18.11.aCollege$CatOutcome ~ employ_datQ18.11.aCollege$Q3, data=employ_datQ18.11.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.11.aCarer<-multidatClean(Q18.11.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aCarer$Carer<- factor(employ_datQ18.11.aCarer$Carer)
employ_datQ18.11.aCarer<-ordinaldatClean(employ_datQ18.11.aCarer$Q18.11.a, employ_datQ18.11.aCarer)
#ordinal(employ_datQ18.11.aCarer$CatOutcome, employ_datQ18.11.aCarer$Carer, employ_datQ18.11.aCarer)
prep <- analysisPrep(employ_datQ18.11.aCarer$CatOutcome, employ_datQ18.11.aCarer$Carer, employ_datQ18.11.aCarer)
analysis <- polr(employ_datQ18.11.aCarer$CatOutcome ~ employ_datQ18.11.aCarer$Carer, data=employ_datQ18.11.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.11.aDisability<-multidatClean(Q18.11.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aDisability$Disability<- factor(employ_datQ18.11.aDisability$Disability)
employ_datQ18.11.aDisability<-ordinaldatClean(employ_datQ18.11.aDisability$Q18.11.a, employ_datQ18.11.aDisability)
conTable <- xtabs(~Q18.11.a + Disability, data = employ_datQ18.11.aDisability)
#ordinal(employ_datQ18.11.aDisability$CatOutcome, employ_datQ18.11.aDisability$Disability, employ_datQ18.11.aDisability)
prep <- analysisPrep(employ_datQ18.11.aDisability$CatOutcome, employ_datQ18.11.aDisability$Disability, employ_datQ18.11.aDisability)
analysis <- polr(employ_datQ18.11.aDisability$CatOutcome ~ employ_datQ18.11.aDisability$Disability, data=employ_datQ18.11.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.11.aEthnicity<-multidatClean(Q18.11.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aEthnicity$Ethnicity<- factor(employ_datQ18.11.aEthnicity$EthnicityCleaned)
employ_datQ18.11.aEthnicity<-ordinaldatClean(employ_datQ18.11.aEthnicity$Q18.11.a, employ_datQ18.11.aEthnicity)
conTable <- xtabs(~Q18.11.a + EthnicityCleaned, data = employ_datQ18.11.aEthnicity)
conTable
#ordinal(employ_datQ18.11.aEthnicity$CatOutcome, employ_datQ18.11.aEthnicity$EthnicityCleaned, employ_datQ18.11.aEthnicity)
prep <- analysisPrep(employ_datQ18.11.aEthnicity$CatOutcome, employ_datQ18.11.aEthnicity$EthnicityCleaned, employ_datQ18.11.aEthnicity)
analysis <- polr(employ_datQ18.11.aEthnicity$CatOutcome ~ employ_datQ18.11.aEthnicity$EthnicityCleaned, data=employ_datQ18.11.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.11.aFirstGen<-multidatClean(Q18.11.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aFirstGen$FirstGen<-factor(employ_datQ18.11.aFirstGen$FirstGen)
employ_datQ18.11.aFirstGen<-ordinaldatClean(employ_datQ18.11.aFirstGen$Q18.11.a, employ_datQ18.11.aFirstGen)
#ordinal(employ_datQ18.11.aFirstGen$CatOutcome, employ_datQ18.11.aFirstGen$FirstGen, employ_datQ18.11.aFirstGen)
prep <- analysisPrep(employ_datQ18.11.aFirstGen$CatOutcome, employ_datQ18.11.aFirstGen$FirstGen, employ_datQ18.11.aFirstGen)
analysis <- polr(employ_datQ18.11.aFirstGen$CatOutcome ~ employ_datQ18.11.aFirstGen$FirstGen, data=employ_datQ18.11.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.11.aGender<-multidatClean(Q18.11.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aGender$Gender<-factor(employ_datQ18.11.aGender$Gender)
employ_datQ18.11.aGender<-ordinaldatClean(employ_datQ18.11.aGender$Q18.11.a, employ_datQ18.11.aGender)
#ordinal(employ_datQ18.11.aGender$CatOutcome, employ_datQ18.11.aGender$Gender, employ_datQ18.11.aGender)
prep <- analysisPrep(employ_datQ18.11.aGender$CatOutcome, employ_datQ18.11.aGender$Gender, employ_datQ18.11.aGender)
analysis <- polr(employ_datQ18.11.aGender$CatOutcome ~ employ_datQ18.11.aGender$Gender, data=employ_datQ18.11.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.11.aSexuality<-multidatClean(Q18.11.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11.aSexuality$Sexuality<-factor(employ_datQ18.11.aSexuality$Sexuality)
employ_datQ18.11.aSexuality<-ordinaldatClean(employ_datQ18.11.aSexuality$Q18.11.a, employ_datQ18.11.aSexuality)
#ordinal(employ_datQ18.11.aSexuality$CatOutcome, employ_datQ18.11.aSexuality$Sexuality, employ_datQ18.11.aSexuality)
prep <- analysisPrep(employ_datQ18.11.aSexuality$CatOutcome, employ_datQ18.11.aSexuality$Sexuality, employ_datQ18.11.aSexuality)
analysis <- polr(employ_datQ18.11.aSexuality$CatOutcome ~ employ_datQ18.11.aSexuality$Sexuality, data=employ_datQ18.11.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.11.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.11.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.12. My institution/workplace's expectations of me to undertake a number of roles leaves me little time for research"

"Status"
employ_datQ18.12.a<-multidatClean(Q18.12.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.12.a + Academic, data = employ_datQ18.12.a)
detach(dat_long)
employ_datQ18.12.a<-ordinaldatCleanNegative(employ_datQ18.12.a$Q18.12.a, employ_datQ18.12.a)
#ordinal(employ_datQ18.12.a$CatOutcome, employ_datQ18.12.a$Academic, employ_datQ18.12.a)
prep <- analysisPrep(employ_datQ18.12.a$CatOutcome, employ_datQ18.12.a$Academic, employ_datQ18.12.a)
analysis <- polr(employ_datQ18.12.a$CatOutcome ~ employ_datQ18.12.a$Academic, data=employ_datQ18.12.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.12.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.12.aCollege<-multidatClean(Q18.12.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.12.a + Q3, data = employ_datQ18.12.aCollege)
conTable
detach(dat_long)
employ_datQ18.12.aCollege$Q3[(employ_datQ18.12.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.12.aCollege$Q3<- factor(employ_datQ18.12.aCollege$Q3)
employ_datQ18.12.aCollege<-ordinaldatCleanNegative(employ_datQ18.12.aCollege$Q18.12.a, employ_datQ18.12.aCollege)
#ordinal(employ_datQ18.12.aCollege$CatOutcome, employ_datQ18.12.aCollege$Q3, employ_datQ18.12.aCollege)
prep <- analysisPrep(employ_datQ18.12.aCollege$CatOutcome, employ_datQ18.12.aCollege$Q3, employ_datQ18.12.aCollege)
analysis <- polr(employ_datQ18.12.aCollege$CatOutcome ~ employ_datQ18.12.aCollege$Q3, data=employ_datQ18.12.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.12.aCarer<-multidatClean(Q18.12.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aCarer$Carer<- factor(employ_datQ18.12.aCarer$Carer)
employ_datQ18.12.aCarer<-ordinaldatCleanNegative(employ_datQ18.12.aCarer$Q18.12.a, employ_datQ18.12.aCarer)
#ordinal(employ_datQ18.12.aCarer$CatOutcome, employ_datQ18.12.aCarer$Carer, employ_datQ18.12.aCarer)
prep <- analysisPrep(employ_datQ18.12.aCarer$CatOutcome, employ_datQ18.12.aCarer$Carer, employ_datQ18.12.aCarer)
analysis <- polr(employ_datQ18.12.aCarer$CatOutcome ~ employ_datQ18.12.aCarer$Carer, data=employ_datQ18.12.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.12.aDisability<-multidatClean(Q18.12.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aDisability$Disability<- factor(employ_datQ18.12.aDisability$Disability)
employ_datQ18.12.aDisability<-ordinaldatCleanNegative(employ_datQ18.12.aDisability$Q18.12.a, employ_datQ18.12.aDisability)
conTable <- xtabs(~Q18.12.a + Disability, data = employ_datQ18.12.aDisability)
#ordinal(employ_datQ18.12.aDisability$CatOutcome, employ_datQ18.12.aDisability$Disability, employ_datQ18.12.aDisability)
prep <- analysisPrep(employ_datQ18.12.aDisability$CatOutcome, employ_datQ18.12.aDisability$Disability, employ_datQ18.12.aDisability)
analysis <- polr(employ_datQ18.12.aDisability$CatOutcome ~ employ_datQ18.12.aDisability$Disability, data=employ_datQ18.12.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.12.aEthnicity<-multidatClean(Q18.12.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aEthnicity$Ethnicity<- factor(employ_datQ18.12.aEthnicity$EthnicityCleaned)
employ_datQ18.12.aEthnicity<-ordinaldatCleanNegative(employ_datQ18.12.aEthnicity$Q18.12.a, employ_datQ18.12.aEthnicity)
conTable <- xtabs(~Q18.12.a + EthnicityCleaned, data = employ_datQ18.12.aEthnicity)
conTable
#ordinal(employ_datQ18.12.aEthnicity$CatOutcome, employ_datQ18.12.aEthnicity$EthnicityCleaned, employ_datQ18.12.aEthnicity)
prep <- analysisPrep(employ_datQ18.12.aEthnicity$CatOutcome, employ_datQ18.12.aEthnicity$EthnicityCleaned, employ_datQ18.12.aEthnicity)
analysis <- polr(employ_datQ18.12.aEthnicity$CatOutcome ~ employ_datQ18.12.aEthnicity$EthnicityCleaned, data=employ_datQ18.12.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.12.aFirstGen<-multidatClean(Q18.12.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aFirstGen$FirstGen<-factor(employ_datQ18.12.aFirstGen$FirstGen)
employ_datQ18.12.aFirstGen<-ordinaldatCleanNegative(employ_datQ18.12.aFirstGen$Q18.12.a, employ_datQ18.12.aFirstGen)
#ordinal(employ_datQ18.12.aFirstGen$CatOutcome, employ_datQ18.12.aFirstGen$FirstGen, employ_datQ18.12.aFirstGen)
prep <- analysisPrep(employ_datQ18.12.aFirstGen$CatOutcome, employ_datQ18.12.aFirstGen$FirstGen, employ_datQ18.12.aFirstGen)
analysis <- polr(employ_datQ18.12.aFirstGen$CatOutcome ~ employ_datQ18.12.aFirstGen$FirstGen, data=employ_datQ18.12.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.12.aGender<-multidatClean(Q18.12.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aGender$Gender<-factor(employ_datQ18.12.aGender$Gender)
employ_datQ18.12.aGender<-ordinaldatCleanNegative(employ_datQ18.12.aGender$Q18.12.a, employ_datQ18.12.aGender)
#ordinal(employ_datQ18.12.aGender$CatOutcome, employ_datQ18.12.aGender$Gender, employ_datQ18.12.aGender)
prep <- analysisPrep(employ_datQ18.12.aGender$CatOutcome, employ_datQ18.12.aGender$Gender, employ_datQ18.12.aGender)
analysis <- polr(employ_datQ18.12.aGender$CatOutcome ~ employ_datQ18.12.aGender$Gender, data=employ_datQ18.12.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.12.aSexuality<-multidatClean(Q18.12.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12.aSexuality$Sexuality<-factor(employ_datQ18.12.aSexuality$Sexuality)
employ_datQ18.12.aSexuality<-ordinaldatCleanNegative(employ_datQ18.12.aSexuality$Q18.12.a, employ_datQ18.12.aSexuality)
#ordinal(employ_datQ18.12.aSexuality$CatOutcome, employ_datQ18.12.aSexuality$Sexuality, employ_datQ18.12.aSexuality)
prep <- analysisPrep(employ_datQ18.12.aSexuality$CatOutcome, employ_datQ18.12.aSexuality$Sexuality, employ_datQ18.12.aSexuality)
analysis <- polr(employ_datQ18.12.aSexuality$CatOutcome ~ employ_datQ18.12.aSexuality$Sexuality, data=employ_datQ18.12.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.12.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.12.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q18 - How far do you agree or disagree with the following statements relating to your current working environment?"
"Q18.13. My institution/workplace provides me with support to navigate the grant application process"

"Status"
employ_datQ18.13.a<-multidatClean(Q18.13.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.13.a + Academic, data = employ_datQ18.13.a)
detach(dat_long)
employ_datQ18.13.a<-ordinaldatClean(employ_datQ18.13.a$Q18.13.a, employ_datQ18.13.a)
#ordinal(employ_datQ18.13.a$CatOutcome, employ_datQ18.13.a$Academic, employ_datQ18.13.a)
prep <- analysisPrep(employ_datQ18.13.a$CatOutcome, employ_datQ18.13.a$Academic, employ_datQ18.13.a)
analysis <- polr(employ_datQ18.13.a$CatOutcome ~ employ_datQ18.13.a$Academic, data=employ_datQ18.13.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.13.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ18.13.aCollege<-multidatClean(Q18.13.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.13.a + Q3, data = employ_datQ18.13.aCollege)
conTable
detach(dat_long)
employ_datQ18.13.aCollege$Q3[(employ_datQ18.13.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.13.aCollege$Q3<- factor(employ_datQ18.13.aCollege$Q3)
employ_datQ18.13.aCollege<-ordinaldatClean(employ_datQ18.13.aCollege$Q18.13.a, employ_datQ18.13.aCollege)
#ordinal(employ_datQ18.13.aCollege$CatOutcome, employ_datQ18.13.aCollege$Q3, employ_datQ18.13.aCollege)
prep <- analysisPrep(employ_datQ18.13.aCollege$CatOutcome, employ_datQ18.13.aCollege$Q3, employ_datQ18.13.aCollege)
analysis <- polr(employ_datQ18.13.aCollege$CatOutcome ~ employ_datQ18.13.aCollege$Q3, data=employ_datQ18.13.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ18.13.aCarer<-multidatClean(Q18.13.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aCarer$Carer<- factor(employ_datQ18.13.aCarer$Carer)
employ_datQ18.13.aCarer<-ordinaldatClean(employ_datQ18.13.aCarer$Q18.13.a, employ_datQ18.13.aCarer)
#ordinal(employ_datQ18.13.aCarer$CatOutcome, employ_datQ18.13.aCarer$Carer, employ_datQ18.13.aCarer)
prep <- analysisPrep(employ_datQ18.13.aCarer$CatOutcome, employ_datQ18.13.aCarer$Carer, employ_datQ18.13.aCarer)
analysis <- polr(employ_datQ18.13.aCarer$CatOutcome ~ employ_datQ18.13.aCarer$Carer, data=employ_datQ18.13.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ18.13.aDisability<-multidatClean(Q18.13.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aDisability$Disability<- factor(employ_datQ18.13.aDisability$Disability)
employ_datQ18.13.aDisability<-ordinaldatClean(employ_datQ18.13.aDisability$Q18.13.a, employ_datQ18.13.aDisability)
conTable <- xtabs(~Q18.13.a + Disability, data = employ_datQ18.13.aDisability)
#ordinal(employ_datQ18.13.aDisability$CatOutcome, employ_datQ18.13.aDisability$Disability, employ_datQ18.13.aDisability)
prep <- analysisPrep(employ_datQ18.13.aDisability$CatOutcome, employ_datQ18.13.aDisability$Disability, employ_datQ18.13.aDisability)
analysis <- polr(employ_datQ18.13.aDisability$CatOutcome ~ employ_datQ18.13.aDisability$Disability, data=employ_datQ18.13.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ18.13.aEthnicity<-multidatClean(Q18.13.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aEthnicity$Ethnicity<- factor(employ_datQ18.13.aEthnicity$EthnicityCleaned)
employ_datQ18.13.aEthnicity<-ordinaldatClean(employ_datQ18.13.aEthnicity$Q18.13.a, employ_datQ18.13.aEthnicity)
conTable <- xtabs(~Q18.13.a + EthnicityCleaned, data = employ_datQ18.13.aEthnicity)
conTable
#ordinal(employ_datQ18.13.aEthnicity$CatOutcome, employ_datQ18.13.aEthnicity$EthnicityCleaned, employ_datQ18.13.aEthnicity)
prep <- analysisPrep(employ_datQ18.13.aEthnicity$CatOutcome, employ_datQ18.13.aEthnicity$EthnicityCleaned, employ_datQ18.13.aEthnicity)
analysis <- polr(employ_datQ18.13.aEthnicity$CatOutcome ~ employ_datQ18.13.aEthnicity$EthnicityCleaned, data=employ_datQ18.13.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ18.13.aFirstGen<-multidatClean(Q18.13.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aFirstGen$FirstGen<-factor(employ_datQ18.13.aFirstGen$FirstGen)
employ_datQ18.13.aFirstGen<-ordinaldatClean(employ_datQ18.13.aFirstGen$Q18.13.a, employ_datQ18.13.aFirstGen)
#ordinal(employ_datQ18.13.aFirstGen$CatOutcome, employ_datQ18.13.aFirstGen$FirstGen, employ_datQ18.13.aFirstGen)
prep <- analysisPrep(employ_datQ18.13.aFirstGen$CatOutcome, employ_datQ18.13.aFirstGen$FirstGen, employ_datQ18.13.aFirstGen)
analysis <- polr(employ_datQ18.13.aFirstGen$CatOutcome ~ employ_datQ18.13.aFirstGen$FirstGen, data=employ_datQ18.13.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ18.13.aGender<-multidatClean(Q18.13.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aGender$Gender<-factor(employ_datQ18.13.aGender$Gender)
employ_datQ18.13.aGender<-ordinaldatClean(employ_datQ18.13.aGender$Q18.13.a, employ_datQ18.13.aGender)
#ordinal(employ_datQ18.13.aGender$CatOutcome, employ_datQ18.13.aGender$Gender, employ_datQ18.13.aGender)
prep <- analysisPrep(employ_datQ18.13.aGender$CatOutcome, employ_datQ18.13.aGender$Gender, employ_datQ18.13.aGender)
analysis <- polr(employ_datQ18.13.aGender$CatOutcome ~ employ_datQ18.13.aGender$Gender, data=employ_datQ18.13.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ18.13.aSexuality<-multidatClean(Q18.13.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13.aSexuality$Sexuality<-factor(employ_datQ18.13.aSexuality$Sexuality)
employ_datQ18.13.aSexuality<-ordinaldatClean(employ_datQ18.13.aSexuality$Q18.13.a, employ_datQ18.13.aSexuality)
#ordinal(employ_datQ18.13.aSexuality$CatOutcome, employ_datQ18.13.aSexuality$Sexuality, employ_datQ18.13.aSexuality)
prep <- analysisPrep(employ_datQ18.13.aSexuality$CatOutcome, employ_datQ18.13.aSexuality$Sexuality, employ_datQ18.13.aSexuality)
analysis <- polr(employ_datQ18.13.aSexuality$CatOutcome ~ employ_datQ18.13.aSexuality$Sexuality, data=employ_datQ18.13.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q18.13.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q18.13.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)












