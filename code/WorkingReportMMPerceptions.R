source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239,93:95, 97:108, 6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)

### Q26 - Using the scale below, how would you rate current research culture in terms of its impact on...?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q26.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ26.1.a<-multidatClean(Q26.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.1.a + Academic, data = employ_datQ26.1.a)
detach(dat_long)
employ_datQ26.1.a<-PositordinaldatClean(employ_datQ26.1.a$Q26.1.a, employ_datQ26.1.a)
#ordinal(employ_datQ26.1.a$CatOutcome, employ_datQ26.1.a$Academic, employ_datQ26.1.a)
prep <- analysisPrep(employ_datQ26.1.a$CatOutcome, employ_datQ26.1.a$Academic, employ_datQ26.1.a)
analysis <- polr(employ_datQ26.1.a$CatOutcome ~ employ_datQ26.1.a$Academic, data=employ_datQ26.1.a, Hess=TRUE)
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
Question <- "Q26.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ26.1.aCollege<-multidatClean(Q26.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.1.a + Q3, data = employ_datQ26.1.aCollege)
conTable
detach(dat_long)
employ_datQ26.1.aCollege$Q3[(employ_datQ26.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ26.1.aCollege$Q3<- factor(employ_datQ26.1.aCollege$Q3)
employ_datQ26.1.aCollege<-PositordinaldatClean(employ_datQ26.1.aCollege$Q26.1.a, employ_datQ26.1.aCollege)
#ordinal(employ_datQ26.1.aCollege$CatOutcome, employ_datQ26.1.aCollege$Q3, employ_datQ26.1.aCollege)
prep <- analysisPrep(employ_datQ26.1.aCollege$CatOutcome, employ_datQ26.1.aCollege$Q3, employ_datQ26.1.aCollege)
analysis <- polr(employ_datQ26.1.aCollege$CatOutcome ~ employ_datQ26.1.aCollege$Q3, data=employ_datQ26.1.aCollege, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ26.1.aCarer<-multidatClean(Q26.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aCarer$Carer<- factor(employ_datQ26.1.aCarer$Carer)
employ_datQ26.1.aCarer<-PositordinaldatClean(employ_datQ26.1.aCarer$Q26.1.a, employ_datQ26.1.aCarer)
#ordinal(employ_datQ26.1.aCarer$CatOutcome, employ_datQ26.1.aCarer$Carer, employ_datQ26.1.aCarer)
prep <- analysisPrep(employ_datQ26.1.aCarer$CatOutcome, employ_datQ26.1.aCarer$Carer, employ_datQ26.1.aCarer)
analysis <- polr(employ_datQ26.1.aCarer$CatOutcome ~ employ_datQ26.1.aCarer$Carer, data=employ_datQ26.1.aCarer, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ26.1.aDisability<-multidatClean(Q26.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aDisability$Disability<- factor(employ_datQ26.1.aDisability$Disability)
employ_datQ26.1.aDisability<-PositordinaldatClean(employ_datQ26.1.aDisability$Q26.1.a, employ_datQ26.1.aDisability)
conTable <- xtabs(~Q26.1.a + Disability, data = employ_datQ26.1.aDisability)
#ordinal(employ_datQ26.1.aDisability$CatOutcome, employ_datQ26.1.aDisability$Disability, employ_datQ26.1.aDisability)
prep <- analysisPrep(employ_datQ26.1.aDisability$CatOutcome, employ_datQ26.1.aDisability$Disability, employ_datQ26.1.aDisability)
analysis <- polr(employ_datQ26.1.aDisability$CatOutcome ~ employ_datQ26.1.aDisability$Disability, data=employ_datQ26.1.aDisability, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ26.1.aEthnicity<-multidatClean(Q26.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aEthnicity$Ethnicity<- factor(employ_datQ26.1.aEthnicity$EthnicityCleaned)
employ_datQ26.1.aEthnicity<-PositordinaldatClean(employ_datQ26.1.aEthnicity$Q26.1.a, employ_datQ26.1.aEthnicity)
conTable <- xtabs(~Q26.1.a + EthnicityCleaned, data = employ_datQ26.1.aEthnicity)
conTable
#ordinal(employ_datQ26.1.aEthnicity$CatOutcome, employ_datQ26.1.aEthnicity$EthnicityCleaned, employ_datQ26.1.aEthnicity)
prep <- analysisPrep(employ_datQ26.1.aEthnicity$CatOutcome, employ_datQ26.1.aEthnicity$EthnicityCleaned, employ_datQ26.1.aEthnicity)
analysis <- polr(employ_datQ26.1.aEthnicity$CatOutcome ~ employ_datQ26.1.aEthnicity$EthnicityCleaned, data=employ_datQ26.1.aEthnicity, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ26.1.aFirstGen<-multidatClean(Q26.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aFirstGen$FirstGen<-factor(employ_datQ26.1.aFirstGen$FirstGen)
employ_datQ26.1.aFirstGen<-PositordinaldatClean(employ_datQ26.1.aFirstGen$Q26.1.a, employ_datQ26.1.aFirstGen)
#ordinal(employ_datQ26.1.aFirstGen$CatOutcome, employ_datQ26.1.aFirstGen$FirstGen, employ_datQ26.1.aFirstGen)
prep <- analysisPrep(employ_datQ26.1.aFirstGen$CatOutcome, employ_datQ26.1.aFirstGen$FirstGen, employ_datQ26.1.aFirstGen)
analysis <- polr(employ_datQ26.1.aFirstGen$CatOutcome ~ employ_datQ26.1.aFirstGen$FirstGen, data=employ_datQ26.1.aFirstGen, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ26.1.aGender<-multidatClean(Q26.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aGender$Gender<-factor(employ_datQ26.1.aGender$Gender)
employ_datQ26.1.aGender<-PositordinaldatClean(employ_datQ26.1.aGender$Q26.1.a, employ_datQ26.1.aGender)
#ordinal(employ_datQ26.1.aGender$CatOutcome, employ_datQ26.1.aGender$Gender, employ_datQ26.1.aGender)
prep <- analysisPrep(employ_datQ26.1.aGender$CatOutcome, employ_datQ26.1.aGender$Gender, employ_datQ26.1.aGender)
analysis <- polr(employ_datQ26.1.aGender$CatOutcome ~ employ_datQ26.1.aGender$Gender, data=employ_datQ26.1.aGender, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ26.1.aSexuality<-multidatClean(Q26.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.1.aSexuality$Sexuality<-factor(employ_datQ26.1.aSexuality$Sexuality)
employ_datQ26.1.aSexuality<-PositordinaldatClean(employ_datQ26.1.aSexuality$Q26.1.a, employ_datQ26.1.aSexuality)
#ordinal(employ_datQ26.1.aSexuality$CatOutcome, employ_datQ26.1.aSexuality$Sexuality, employ_datQ26.1.aSexuality)
prep <- analysisPrep(employ_datQ26.1.aSexuality$CatOutcome, employ_datQ26.1.aSexuality$Sexuality, employ_datQ26.1.aSexuality)
analysis <- polr(employ_datQ26.1.aSexuality$CatOutcome ~ employ_datQ26.1.aSexuality$Sexuality, data=employ_datQ26.1.aSexuality, Hess=TRUE)
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
Question <-  "Q26.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q26.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q26 - Using the scale below, how would you rate current research culture in terms of its impact on...?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Individuals"
"Q26.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ26.2.a<-multidatClean(Q26.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.2.a + Academic, data = employ_datQ26.2.a)
detach(dat_long)
employ_datQ26.2.a<-PositordinaldatClean(employ_datQ26.2.a$Q26.2.a, employ_datQ26.2.a)
#ordinal(employ_datQ26.2.a$CatOutcome, employ_datQ26.2.a$Academic, employ_datQ26.2.a)
prep <- analysisPrep(employ_datQ26.2.a$CatOutcome, employ_datQ26.2.a$Academic, employ_datQ26.2.a)
analysis <- polr(employ_datQ26.2.a$CatOutcome ~ employ_datQ26.2.a$Academic, data=employ_datQ26.2.a, Hess=TRUE)
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
Question <- "Q26.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ26.2.aCollege<-multidatClean(Q26.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.2.a + Q3, data = employ_datQ26.2.aCollege)
conTable
detach(dat_long)
employ_datQ26.2.aCollege$Q3[(employ_datQ26.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ26.2.aCollege$Q3<- factor(employ_datQ26.2.aCollege$Q3)
employ_datQ26.2.aCollege<-PositordinaldatClean(employ_datQ26.2.aCollege$Q26.2.a, employ_datQ26.2.aCollege)
#ordinal(employ_datQ26.2.aCollege$CatOutcome, employ_datQ26.2.aCollege$Q3, employ_datQ26.2.aCollege)
prep <- analysisPrep(employ_datQ26.2.aCollege$CatOutcome, employ_datQ26.2.aCollege$Q3, employ_datQ26.2.aCollege)
analysis <- polr(employ_datQ26.2.aCollege$CatOutcome ~ employ_datQ26.2.aCollege$Q3, data=employ_datQ26.2.aCollege, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ26.2.aCarer<-multidatClean(Q26.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aCarer$Carer<- factor(employ_datQ26.2.aCarer$Carer)
employ_datQ26.2.aCarer<-PositordinaldatClean(employ_datQ26.2.aCarer$Q26.2.a, employ_datQ26.2.aCarer)
#ordinal(employ_datQ26.2.aCarer$CatOutcome, employ_datQ26.2.aCarer$Carer, employ_datQ26.2.aCarer)
prep <- analysisPrep(employ_datQ26.2.aCarer$CatOutcome, employ_datQ26.2.aCarer$Carer, employ_datQ26.2.aCarer)
analysis <- polr(employ_datQ26.2.aCarer$CatOutcome ~ employ_datQ26.2.aCarer$Carer, data=employ_datQ26.2.aCarer, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ26.2.aDisability<-multidatClean(Q26.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aDisability$Disability<- factor(employ_datQ26.2.aDisability$Disability)
employ_datQ26.2.aDisability<-PositordinaldatClean(employ_datQ26.2.aDisability$Q26.2.a, employ_datQ26.2.aDisability)
conTable <- xtabs(~Q26.2.a + Disability, data = employ_datQ26.2.aDisability)
#ordinal(employ_datQ26.2.aDisability$CatOutcome, employ_datQ26.2.aDisability$Disability, employ_datQ26.2.aDisability)
prep <- analysisPrep(employ_datQ26.2.aDisability$CatOutcome, employ_datQ26.2.aDisability$Disability, employ_datQ26.2.aDisability)
analysis <- polr(employ_datQ26.2.aDisability$CatOutcome ~ employ_datQ26.2.aDisability$Disability, data=employ_datQ26.2.aDisability, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ26.2.aEthnicity<-multidatClean(Q26.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aEthnicity$Ethnicity<- factor(employ_datQ26.2.aEthnicity$EthnicityCleaned)
employ_datQ26.2.aEthnicity<-PositordinaldatClean(employ_datQ26.2.aEthnicity$Q26.2.a, employ_datQ26.2.aEthnicity)
conTable <- xtabs(~Q26.2.a + EthnicityCleaned, data = employ_datQ26.2.aEthnicity)
conTable
#ordinal(employ_datQ26.2.aEthnicity$CatOutcome, employ_datQ26.2.aEthnicity$EthnicityCleaned, employ_datQ26.2.aEthnicity)
prep <- analysisPrep(employ_datQ26.2.aEthnicity$CatOutcome, employ_datQ26.2.aEthnicity$EthnicityCleaned, employ_datQ26.2.aEthnicity)
analysis <- polr(employ_datQ26.2.aEthnicity$CatOutcome ~ employ_datQ26.2.aEthnicity$EthnicityCleaned, data=employ_datQ26.2.aEthnicity, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ26.2.aFirstGen<-multidatClean(Q26.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aFirstGen$FirstGen<-factor(employ_datQ26.2.aFirstGen$FirstGen)
employ_datQ26.2.aFirstGen<-PositordinaldatClean(employ_datQ26.2.aFirstGen$Q26.2.a, employ_datQ26.2.aFirstGen)
#ordinal(employ_datQ26.2.aFirstGen$CatOutcome, employ_datQ26.2.aFirstGen$FirstGen, employ_datQ26.2.aFirstGen)
prep <- analysisPrep(employ_datQ26.2.aFirstGen$CatOutcome, employ_datQ26.2.aFirstGen$FirstGen, employ_datQ26.2.aFirstGen)
analysis <- polr(employ_datQ26.2.aFirstGen$CatOutcome ~ employ_datQ26.2.aFirstGen$FirstGen, data=employ_datQ26.2.aFirstGen, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ26.2.aGender<-multidatClean(Q26.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aGender$Gender<-factor(employ_datQ26.2.aGender$Gender)
employ_datQ26.2.aGender<-PositordinaldatClean(employ_datQ26.2.aGender$Q26.2.a, employ_datQ26.2.aGender)
#ordinal(employ_datQ26.2.aGender$CatOutcome, employ_datQ26.2.aGender$Gender, employ_datQ26.2.aGender)
prep <- analysisPrep(employ_datQ26.2.aGender$CatOutcome, employ_datQ26.2.aGender$Gender, employ_datQ26.2.aGender)
analysis <- polr(employ_datQ26.2.aGender$CatOutcome ~ employ_datQ26.2.aGender$Gender, data=employ_datQ26.2.aGender, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ26.2.aSexuality<-multidatClean(Q26.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.2.aSexuality$Sexuality<-factor(employ_datQ26.2.aSexuality$Sexuality)
employ_datQ26.2.aSexuality<-PositordinaldatClean(employ_datQ26.2.aSexuality$Q26.2.a, employ_datQ26.2.aSexuality)
#ordinal(employ_datQ26.2.aSexuality$CatOutcome, employ_datQ26.2.aSexuality$Sexuality, employ_datQ26.2.aSexuality)
prep <- analysisPrep(employ_datQ26.2.aSexuality$CatOutcome, employ_datQ26.2.aSexuality$Sexuality, employ_datQ26.2.aSexuality)
analysis <- polr(employ_datQ26.2.aSexuality$CatOutcome ~ employ_datQ26.2.aSexuality$Sexuality, data=employ_datQ26.2.aSexuality, Hess=TRUE)
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
Question <-  "Q26.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q26.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q26 - Using the scale below, how would you rate current research culture in terms of its impact on...?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Quality of Research"
"Q26.3.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ26.3.a<-multidatClean(Q26.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.3.a + Academic, data = employ_datQ26.3.a)
detach(dat_long)
employ_datQ26.3.a<-PositordinaldatClean(employ_datQ26.3.a$Q26.3.a, employ_datQ26.3.a)
#ordinal(employ_datQ26.3.a$CatOutcome, employ_datQ26.3.a$Academic, employ_datQ26.3.a)
prep <- analysisPrep(employ_datQ26.3.a$CatOutcome, employ_datQ26.3.a$Academic, employ_datQ26.3.a)
analysis <- polr(employ_datQ26.3.a$CatOutcome ~ employ_datQ26.3.a$Academic, data=employ_datQ26.3.a, Hess=TRUE)
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
Question <- "Q26.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ26.3.aCollege<-multidatClean(Q26.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q26.3.a + Q3, data = employ_datQ26.3.aCollege)
conTable
detach(dat_long)
employ_datQ26.3.aCollege$Q3[(employ_datQ26.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ26.3.aCollege$Q3<- factor(employ_datQ26.3.aCollege$Q3)
employ_datQ26.3.aCollege<-PositordinaldatClean(employ_datQ26.3.aCollege$Q26.3.a, employ_datQ26.3.aCollege)
#ordinal(employ_datQ26.3.aCollege$CatOutcome, employ_datQ26.3.aCollege$Q3, employ_datQ26.3.aCollege)
prep <- analysisPrep(employ_datQ26.3.aCollege$CatOutcome, employ_datQ26.3.aCollege$Q3, employ_datQ26.3.aCollege)
analysis <- polr(employ_datQ26.3.aCollege$CatOutcome ~ employ_datQ26.3.aCollege$Q3, data=employ_datQ26.3.aCollege, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ26.3.aCarer<-multidatClean(Q26.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aCarer$Carer<- factor(employ_datQ26.3.aCarer$Carer)
employ_datQ26.3.aCarer<-PositordinaldatClean(employ_datQ26.3.aCarer$Q26.3.a, employ_datQ26.3.aCarer)
#ordinal(employ_datQ26.3.aCarer$CatOutcome, employ_datQ26.3.aCarer$Carer, employ_datQ26.3.aCarer)
prep <- analysisPrep(employ_datQ26.3.aCarer$CatOutcome, employ_datQ26.3.aCarer$Carer, employ_datQ26.3.aCarer)
analysis <- polr(employ_datQ26.3.aCarer$CatOutcome ~ employ_datQ26.3.aCarer$Carer, data=employ_datQ26.3.aCarer, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ26.3.aDisability<-multidatClean(Q26.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aDisability$Disability<- factor(employ_datQ26.3.aDisability$Disability)
employ_datQ26.3.aDisability<-PositordinaldatClean(employ_datQ26.3.aDisability$Q26.3.a, employ_datQ26.3.aDisability)
conTable <- xtabs(~Q26.3.a + Disability, data = employ_datQ26.3.aDisability)
#ordinal(employ_datQ26.3.aDisability$CatOutcome, employ_datQ26.3.aDisability$Disability, employ_datQ26.3.aDisability)
prep <- analysisPrep(employ_datQ26.3.aDisability$CatOutcome, employ_datQ26.3.aDisability$Disability, employ_datQ26.3.aDisability)
analysis <- polr(employ_datQ26.3.aDisability$CatOutcome ~ employ_datQ26.3.aDisability$Disability, data=employ_datQ26.3.aDisability, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ26.3.aEthnicity<-multidatClean(Q26.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aEthnicity$Ethnicity<- factor(employ_datQ26.3.aEthnicity$EthnicityCleaned)
employ_datQ26.3.aEthnicity<-PositordinaldatClean(employ_datQ26.3.aEthnicity$Q26.3.a, employ_datQ26.3.aEthnicity)
conTable <- xtabs(~Q26.3.a + EthnicityCleaned, data = employ_datQ26.3.aEthnicity)
conTable
#ordinal(employ_datQ26.3.aEthnicity$CatOutcome, employ_datQ26.3.aEthnicity$EthnicityCleaned, employ_datQ26.3.aEthnicity)
prep <- analysisPrep(employ_datQ26.3.aEthnicity$CatOutcome, employ_datQ26.3.aEthnicity$EthnicityCleaned, employ_datQ26.3.aEthnicity)
analysis <- polr(employ_datQ26.3.aEthnicity$CatOutcome ~ employ_datQ26.3.aEthnicity$EthnicityCleaned, data=employ_datQ26.3.aEthnicity, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ26.3.aFirstGen<-multidatClean(Q26.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aFirstGen$FirstGen<-factor(employ_datQ26.3.aFirstGen$FirstGen)
employ_datQ26.3.aFirstGen<-PositordinaldatClean(employ_datQ26.3.aFirstGen$Q26.3.a, employ_datQ26.3.aFirstGen)
#ordinal(employ_datQ26.3.aFirstGen$CatOutcome, employ_datQ26.3.aFirstGen$FirstGen, employ_datQ26.3.aFirstGen)
prep <- analysisPrep(employ_datQ26.3.aFirstGen$CatOutcome, employ_datQ26.3.aFirstGen$FirstGen, employ_datQ26.3.aFirstGen)
analysis <- polr(employ_datQ26.3.aFirstGen$CatOutcome ~ employ_datQ26.3.aFirstGen$FirstGen, data=employ_datQ26.3.aFirstGen, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ26.3.aGender<-multidatClean(Q26.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aGender$Gender<-factor(employ_datQ26.3.aGender$Gender)
employ_datQ26.3.aGender<-PositordinaldatClean(employ_datQ26.3.aGender$Q26.3.a, employ_datQ26.3.aGender)
#ordinal(employ_datQ26.3.aGender$CatOutcome, employ_datQ26.3.aGender$Gender, employ_datQ26.3.aGender)
prep <- analysisPrep(employ_datQ26.3.aGender$CatOutcome, employ_datQ26.3.aGender$Gender, employ_datQ26.3.aGender)
analysis <- polr(employ_datQ26.3.aGender$CatOutcome ~ employ_datQ26.3.aGender$Gender, data=employ_datQ26.3.aGender, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ26.3.aSexuality<-multidatClean(Q26.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ26.3.aSexuality$Sexuality<-factor(employ_datQ26.3.aSexuality$Sexuality)
employ_datQ26.3.aSexuality<-PositordinaldatClean(employ_datQ26.3.aSexuality$Q26.3.a, employ_datQ26.3.aSexuality)
#ordinal(employ_datQ26.3.aSexuality$CatOutcome, employ_datQ26.3.aSexuality$Sexuality, employ_datQ26.3.aSexuality)
prep <- analysisPrep(employ_datQ26.3.aSexuality$CatOutcome, employ_datQ26.3.aSexuality$Sexuality, employ_datQ26.3.aSexuality)
analysis <- polr(employ_datQ26.3.aSexuality$CatOutcome ~ employ_datQ26.3.aSexuality$Sexuality, data=employ_datQ26.3.aSexuality, Hess=TRUE)
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
Question <-  "Q26.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)
Q26.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"Current research culture promotes high- quality research"
"Q27.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.1.a<-multidatClean(Q27.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.1.a + Academic, data = employ_datQ27.1.a)
detach(dat_long)
employ_datQ27.1.a<-ordinaldatClean(employ_datQ27.1.a$Q27.1.a, employ_datQ27.1.a)
#ordinal(employ_datQ27.1.a$CatOutcome, employ_datQ27.1.a$Academic, employ_datQ27.1.a)
prep <- analysisPrep(employ_datQ27.1.a$CatOutcome, employ_datQ27.1.a$Academic, employ_datQ27.1.a)
analysis <- polr(employ_datQ27.1.a$CatOutcome ~ employ_datQ27.1.a$Academic, data=employ_datQ27.1.a, Hess=TRUE)
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
Question <- "Q27.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.1.aCollege<-multidatClean(Q27.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.1.a + Q3, data = employ_datQ27.1.aCollege)
conTable
detach(dat_long)
employ_datQ27.1.aCollege$Q3[(employ_datQ27.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.1.aCollege$Q3<- factor(employ_datQ27.1.aCollege$Q3)
employ_datQ27.1.aCollege<-ordinaldatClean(employ_datQ27.1.aCollege$Q27.1.a, employ_datQ27.1.aCollege)
#ordinal(employ_datQ27.1.aCollege$CatOutcome, employ_datQ27.1.aCollege$Q3, employ_datQ27.1.aCollege)
prep <- analysisPrep(employ_datQ27.1.aCollege$CatOutcome, employ_datQ27.1.aCollege$Q3, employ_datQ27.1.aCollege)
analysis <- polr(employ_datQ27.1.aCollege$CatOutcome ~ employ_datQ27.1.aCollege$Q3, data=employ_datQ27.1.aCollege, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.1.aCarer<-multidatClean(Q27.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aCarer$Carer<- factor(employ_datQ27.1.aCarer$Carer)
employ_datQ27.1.aCarer<-ordinaldatClean(employ_datQ27.1.aCarer$Q27.1.a, employ_datQ27.1.aCarer)
#ordinal(employ_datQ27.1.aCarer$CatOutcome, employ_datQ27.1.aCarer$Carer, employ_datQ27.1.aCarer)
prep <- analysisPrep(employ_datQ27.1.aCarer$CatOutcome, employ_datQ27.1.aCarer$Carer, employ_datQ27.1.aCarer)
analysis <- polr(employ_datQ27.1.aCarer$CatOutcome ~ employ_datQ27.1.aCarer$Carer, data=employ_datQ27.1.aCarer, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.1.aDisability<-multidatClean(Q27.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aDisability$Disability<- factor(employ_datQ27.1.aDisability$Disability)
employ_datQ27.1.aDisability<-ordinaldatClean(employ_datQ27.1.aDisability$Q27.1.a, employ_datQ27.1.aDisability)
conTable <- xtabs(~Q27.1.a + Disability, data = employ_datQ27.1.aDisability)
#ordinal(employ_datQ27.1.aDisability$CatOutcome, employ_datQ27.1.aDisability$Disability, employ_datQ27.1.aDisability)
prep <- analysisPrep(employ_datQ27.1.aDisability$CatOutcome, employ_datQ27.1.aDisability$Disability, employ_datQ27.1.aDisability)
analysis <- polr(employ_datQ27.1.aDisability$CatOutcome ~ employ_datQ27.1.aDisability$Disability, data=employ_datQ27.1.aDisability, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.1.aEthnicity<-multidatClean(Q27.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aEthnicity$Ethnicity<- factor(employ_datQ27.1.aEthnicity$EthnicityCleaned)
employ_datQ27.1.aEthnicity<-ordinaldatClean(employ_datQ27.1.aEthnicity$Q27.1.a, employ_datQ27.1.aEthnicity)
conTable <- xtabs(~Q27.1.a + EthnicityCleaned, data = employ_datQ27.1.aEthnicity)
conTable
#ordinal(employ_datQ27.1.aEthnicity$CatOutcome, employ_datQ27.1.aEthnicity$EthnicityCleaned, employ_datQ27.1.aEthnicity)
prep <- analysisPrep(employ_datQ27.1.aEthnicity$CatOutcome, employ_datQ27.1.aEthnicity$EthnicityCleaned, employ_datQ27.1.aEthnicity)
analysis <- polr(employ_datQ27.1.aEthnicity$CatOutcome ~ employ_datQ27.1.aEthnicity$EthnicityCleaned, data=employ_datQ27.1.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.1.aFirstGen<-multidatClean(Q27.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aFirstGen$FirstGen<-factor(employ_datQ27.1.aFirstGen$FirstGen)
employ_datQ27.1.aFirstGen<-ordinaldatClean(employ_datQ27.1.aFirstGen$Q27.1.a, employ_datQ27.1.aFirstGen)
#ordinal(employ_datQ27.1.aFirstGen$CatOutcome, employ_datQ27.1.aFirstGen$FirstGen, employ_datQ27.1.aFirstGen)
prep <- analysisPrep(employ_datQ27.1.aFirstGen$CatOutcome, employ_datQ27.1.aFirstGen$FirstGen, employ_datQ27.1.aFirstGen)
analysis <- polr(employ_datQ27.1.aFirstGen$CatOutcome ~ employ_datQ27.1.aFirstGen$FirstGen, data=employ_datQ27.1.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.1.aGender<-multidatClean(Q27.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aGender$Gender<-factor(employ_datQ27.1.aGender$Gender)
employ_datQ27.1.aGender<-ordinaldatClean(employ_datQ27.1.aGender$Q27.1.a, employ_datQ27.1.aGender)
#ordinal(employ_datQ27.1.aGender$CatOutcome, employ_datQ27.1.aGender$Gender, employ_datQ27.1.aGender)
prep <- analysisPrep(employ_datQ27.1.aGender$CatOutcome, employ_datQ27.1.aGender$Gender, employ_datQ27.1.aGender)
analysis <- polr(employ_datQ27.1.aGender$CatOutcome ~ employ_datQ27.1.aGender$Gender, data=employ_datQ27.1.aGender, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.1.aSexuality<-multidatClean(Q27.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.1.aSexuality$Sexuality<-factor(employ_datQ27.1.aSexuality$Sexuality)
employ_datQ27.1.aSexuality<-ordinaldatClean(employ_datQ27.1.aSexuality$Q27.1.a, employ_datQ27.1.aSexuality)
#ordinal(employ_datQ27.1.aSexuality$CatOutcome, employ_datQ27.1.aSexuality$Sexuality, employ_datQ27.1.aSexuality)
prep <- analysisPrep(employ_datQ27.1.aSexuality$CatOutcome, employ_datQ27.1.aSexuality$Sexuality, employ_datQ27.1.aSexuality)
analysis <- polr(employ_datQ27.1.aSexuality$CatOutcome ~ employ_datQ27.1.aSexuality$Sexuality, data=employ_datQ27.1.aSexuality, Hess=TRUE)
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
Question <-  "Q27.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)
Q27.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"High levels of competition have created unkind and aggressive research conditions"
"Status"
employ_datQ27.2.a<-multidatClean(Q27.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.2.a + Academic, data = employ_datQ27.2.a)
detach(dat_long)
employ_datQ27.2.a<-ordinaldatCleanNegative(employ_datQ27.2.a$Q27.2.a, employ_datQ27.2.a)
#ordinal(employ_datQ27.2.a$CatOutcome, employ_datQ27.2.a$Academic, employ_datQ27.2.a)
prep <- analysisPrep(employ_datQ27.2.a$CatOutcome, employ_datQ27.2.a$Academic, employ_datQ27.2.a)
analysis <- polr(employ_datQ27.2.a$CatOutcome ~ employ_datQ27.2.a$Academic, data=employ_datQ27.2.a, Hess=TRUE)
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
Question <- "Q27.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.2.aCollege<-multidatClean(Q27.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.2.a + Q3, data = employ_datQ27.2.aCollege)
conTable
detach(dat_long)
employ_datQ27.2.aCollege$Q3[(employ_datQ27.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.2.aCollege$Q3<- factor(employ_datQ27.2.aCollege$Q3)
employ_datQ27.2.aCollege<-ordinaldatCleanNegative(employ_datQ27.2.aCollege$Q27.2.a, employ_datQ27.2.aCollege)
#ordinal(employ_datQ27.2.aCollege$CatOutcome, employ_datQ27.2.aCollege$Q3, employ_datQ27.2.aCollege)
prep <- analysisPrep(employ_datQ27.2.aCollege$CatOutcome, employ_datQ27.2.aCollege$Q3, employ_datQ27.2.aCollege)
analysis <- polr(employ_datQ27.2.aCollege$CatOutcome ~ employ_datQ27.2.aCollege$Q3, data=employ_datQ27.2.aCollege, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.2.aCarer<-multidatClean(Q27.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aCarer$Carer<- factor(employ_datQ27.2.aCarer$Carer)
employ_datQ27.2.aCarer<-ordinaldatCleanNegative(employ_datQ27.2.aCarer$Q27.2.a, employ_datQ27.2.aCarer)
#ordinal(employ_datQ27.2.aCarer$CatOutcome, employ_datQ27.2.aCarer$Carer, employ_datQ27.2.aCarer)
prep <- analysisPrep(employ_datQ27.2.aCarer$CatOutcome, employ_datQ27.2.aCarer$Carer, employ_datQ27.2.aCarer)
analysis <- polr(employ_datQ27.2.aCarer$CatOutcome ~ employ_datQ27.2.aCarer$Carer, data=employ_datQ27.2.aCarer, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.2.aDisability<-multidatClean(Q27.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aDisability$Disability<- factor(employ_datQ27.2.aDisability$Disability)
employ_datQ27.2.aDisability<-ordinaldatCleanNegative(employ_datQ27.2.aDisability$Q27.2.a, employ_datQ27.2.aDisability)
conTable <- xtabs(~Q27.2.a + Disability, data = employ_datQ27.2.aDisability)
#ordinal(employ_datQ27.2.aDisability$CatOutcome, employ_datQ27.2.aDisability$Disability, employ_datQ27.2.aDisability)
prep <- analysisPrep(employ_datQ27.2.aDisability$CatOutcome, employ_datQ27.2.aDisability$Disability, employ_datQ27.2.aDisability)
analysis <- polr(employ_datQ27.2.aDisability$CatOutcome ~ employ_datQ27.2.aDisability$Disability, data=employ_datQ27.2.aDisability, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.2.aEthnicity<-multidatClean(Q27.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aEthnicity$Ethnicity<- factor(employ_datQ27.2.aEthnicity$EthnicityCleaned)
employ_datQ27.2.aEthnicity<-ordinaldatCleanNegative(employ_datQ27.2.aEthnicity$Q27.2.a, employ_datQ27.2.aEthnicity)
conTable <- xtabs(~Q27.2.a + EthnicityCleaned, data = employ_datQ27.2.aEthnicity)
conTable
#ordinal(employ_datQ27.2.aEthnicity$CatOutcome, employ_datQ27.2.aEthnicity$EthnicityCleaned, employ_datQ27.2.aEthnicity)
prep <- analysisPrep(employ_datQ27.2.aEthnicity$CatOutcome, employ_datQ27.2.aEthnicity$EthnicityCleaned, employ_datQ27.2.aEthnicity)
analysis <- polr(employ_datQ27.2.aEthnicity$CatOutcome ~ employ_datQ27.2.aEthnicity$EthnicityCleaned, data=employ_datQ27.2.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.2.aFirstGen<-multidatClean(Q27.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aFirstGen$FirstGen<-factor(employ_datQ27.2.aFirstGen$FirstGen)
employ_datQ27.2.aFirstGen<-ordinaldatCleanNegative(employ_datQ27.2.aFirstGen$Q27.2.a, employ_datQ27.2.aFirstGen)
#ordinal(employ_datQ27.2.aFirstGen$CatOutcome, employ_datQ27.2.aFirstGen$FirstGen, employ_datQ27.2.aFirstGen)
prep <- analysisPrep(employ_datQ27.2.aFirstGen$CatOutcome, employ_datQ27.2.aFirstGen$FirstGen, employ_datQ27.2.aFirstGen)
analysis <- polr(employ_datQ27.2.aFirstGen$CatOutcome ~ employ_datQ27.2.aFirstGen$FirstGen, data=employ_datQ27.2.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.2.aGender<-multidatClean(Q27.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aGender$Gender<-factor(employ_datQ27.2.aGender$Gender)
employ_datQ27.2.aGender<-ordinaldatCleanNegative(employ_datQ27.2.aGender$Q27.2.a, employ_datQ27.2.aGender)
#ordinal(employ_datQ27.2.aGender$CatOutcome, employ_datQ27.2.aGender$Gender, employ_datQ27.2.aGender)
prep <- analysisPrep(employ_datQ27.2.aGender$CatOutcome, employ_datQ27.2.aGender$Gender, employ_datQ27.2.aGender)
analysis <- polr(employ_datQ27.2.aGender$CatOutcome ~ employ_datQ27.2.aGender$Gender, data=employ_datQ27.2.aGender, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.2.aSexuality<-multidatClean(Q27.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.2.aSexuality$Sexuality<-factor(employ_datQ27.2.aSexuality$Sexuality)
employ_datQ27.2.aSexuality<-ordinaldatCleanNegative(employ_datQ27.2.aSexuality$Q27.2.a, employ_datQ27.2.aSexuality)
#ordinal(employ_datQ27.2.aSexuality$CatOutcome, employ_datQ27.2.aSexuality$Sexuality, employ_datQ27.2.aSexuality)
prep <- analysisPrep(employ_datQ27.2.aSexuality$CatOutcome, employ_datQ27.2.aSexuality$Sexuality, employ_datQ27.2.aSexuality)
analysis <- polr(employ_datQ27.2.aSexuality$CatOutcome ~ employ_datQ27.2.aSexuality$Sexuality, data=employ_datQ27.2.aSexuality, Hess=TRUE)
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
Question <-  "Q27.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"High standards and integrity are valued within the research community"
"Q27.3.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.3.a<-multidatClean(Q27.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.3.a + Academic, data = employ_datQ27.3.a)
detach(dat_long)
employ_datQ27.3.a<-ordinaldatClean(employ_datQ27.3.a$Q27.3.a, employ_datQ27.3.a)
#ordinal(employ_datQ27.3.a$CatOutcome, employ_datQ27.3.a$Academic, employ_datQ27.3.a)
prep <- analysisPrep(employ_datQ27.3.a$CatOutcome, employ_datQ27.3.a$Academic, employ_datQ27.3.a)
analysis <- polr(employ_datQ27.3.a$CatOutcome ~ employ_datQ27.3.a$Academic, data=employ_datQ27.3.a, Hess=TRUE)
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
Question <- "Q27.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.3.aCollege<-multidatClean(Q27.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.3.a + Q3, data = employ_datQ27.3.aCollege)
conTable
detach(dat_long)
employ_datQ27.3.aCollege$Q3[(employ_datQ27.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.3.aCollege$Q3<- factor(employ_datQ27.3.aCollege$Q3)
employ_datQ27.3.aCollege<-ordinaldatClean(employ_datQ27.3.aCollege$Q27.3.a, employ_datQ27.3.aCollege)
#ordinal(employ_datQ27.3.aCollege$CatOutcome, employ_datQ27.3.aCollege$Q3, employ_datQ27.3.aCollege)
prep <- analysisPrep(employ_datQ27.3.aCollege$CatOutcome, employ_datQ27.3.aCollege$Q3, employ_datQ27.3.aCollege)
analysis <- polr(employ_datQ27.3.aCollege$CatOutcome ~ employ_datQ27.3.aCollege$Q3, data=employ_datQ27.3.aCollege, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.3.aCarer<-multidatClean(Q27.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aCarer$Carer<- factor(employ_datQ27.3.aCarer$Carer)
employ_datQ27.3.aCarer<-ordinaldatClean(employ_datQ27.3.aCarer$Q27.3.a, employ_datQ27.3.aCarer)
#ordinal(employ_datQ27.3.aCarer$CatOutcome, employ_datQ27.3.aCarer$Carer, employ_datQ27.3.aCarer)
prep <- analysisPrep(employ_datQ27.3.aCarer$CatOutcome, employ_datQ27.3.aCarer$Carer, employ_datQ27.3.aCarer)
analysis <- polr(employ_datQ27.3.aCarer$CatOutcome ~ employ_datQ27.3.aCarer$Carer, data=employ_datQ27.3.aCarer, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.3.aDisability<-multidatClean(Q27.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aDisability$Disability<- factor(employ_datQ27.3.aDisability$Disability)
employ_datQ27.3.aDisability<-ordinaldatClean(employ_datQ27.3.aDisability$Q27.3.a, employ_datQ27.3.aDisability)
conTable <- xtabs(~Q27.3.a + Disability, data = employ_datQ27.3.aDisability)
#ordinal(employ_datQ27.3.aDisability$CatOutcome, employ_datQ27.3.aDisability$Disability, employ_datQ27.3.aDisability)
prep <- analysisPrep(employ_datQ27.3.aDisability$CatOutcome, employ_datQ27.3.aDisability$Disability, employ_datQ27.3.aDisability)
analysis <- polr(employ_datQ27.3.aDisability$CatOutcome ~ employ_datQ27.3.aDisability$Disability, data=employ_datQ27.3.aDisability, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.3.aEthnicity<-multidatClean(Q27.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aEthnicity$Ethnicity<- factor(employ_datQ27.3.aEthnicity$EthnicityCleaned)
employ_datQ27.3.aEthnicity<-ordinaldatClean(employ_datQ27.3.aEthnicity$Q27.3.a, employ_datQ27.3.aEthnicity)
conTable <- xtabs(~Q27.3.a + EthnicityCleaned, data = employ_datQ27.3.aEthnicity)
conTable
#ordinal(employ_datQ27.3.aEthnicity$CatOutcome, employ_datQ27.3.aEthnicity$EthnicityCleaned, employ_datQ27.3.aEthnicity)
prep <- analysisPrep(employ_datQ27.3.aEthnicity$CatOutcome, employ_datQ27.3.aEthnicity$EthnicityCleaned, employ_datQ27.3.aEthnicity)
analysis <- polr(employ_datQ27.3.aEthnicity$CatOutcome ~ employ_datQ27.3.aEthnicity$EthnicityCleaned, data=employ_datQ27.3.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.3.aFirstGen<-multidatClean(Q27.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aFirstGen$FirstGen<-factor(employ_datQ27.3.aFirstGen$FirstGen)
employ_datQ27.3.aFirstGen<-ordinaldatClean(employ_datQ27.3.aFirstGen$Q27.3.a, employ_datQ27.3.aFirstGen)
#ordinal(employ_datQ27.3.aFirstGen$CatOutcome, employ_datQ27.3.aFirstGen$FirstGen, employ_datQ27.3.aFirstGen)
prep <- analysisPrep(employ_datQ27.3.aFirstGen$CatOutcome, employ_datQ27.3.aFirstGen$FirstGen, employ_datQ27.3.aFirstGen)
analysis <- polr(employ_datQ27.3.aFirstGen$CatOutcome ~ employ_datQ27.3.aFirstGen$FirstGen, data=employ_datQ27.3.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.3.aGender<-multidatClean(Q27.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aGender$Gender<-factor(employ_datQ27.3.aGender$Gender)
employ_datQ27.3.aGender<-ordinaldatClean(employ_datQ27.3.aGender$Q27.3.a, employ_datQ27.3.aGender)
#ordinal(employ_datQ27.3.aGender$CatOutcome, employ_datQ27.3.aGender$Gender, employ_datQ27.3.aGender)
prep <- analysisPrep(employ_datQ27.3.aGender$CatOutcome, employ_datQ27.3.aGender$Gender, employ_datQ27.3.aGender)
analysis <- polr(employ_datQ27.3.aGender$CatOutcome ~ employ_datQ27.3.aGender$Gender, data=employ_datQ27.3.aGender, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.3.aSexuality<-multidatClean(Q27.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.3.aSexuality$Sexuality<-factor(employ_datQ27.3.aSexuality$Sexuality)
employ_datQ27.3.aSexuality<-ordinaldatClean(employ_datQ27.3.aSexuality$Q27.3.a, employ_datQ27.3.aSexuality)
#ordinal(employ_datQ27.3.aSexuality$CatOutcome, employ_datQ27.3.aSexuality$Sexuality, employ_datQ27.3.aSexuality)
prep <- analysisPrep(employ_datQ27.3.aSexuality$CatOutcome, employ_datQ27.3.aSexuality$Sexuality, employ_datQ27.3.aSexuality)
analysis <- polr(employ_datQ27.3.aSexuality$CatOutcome ~ employ_datQ27.3.aSexuality$Sexuality, data=employ_datQ27.3.aSexuality, Hess=TRUE)
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
Question <-  "Q27.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q26 - How far do you agree or disagree with the following statements relating to research culture?"
"Research culture promotes quantity over quality"
"Q27.4.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.4.a<-multidatClean(Q27.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.4.a + Academic, data = employ_datQ27.4.a)
detach(dat_long)
employ_datQ27.4.a<-ordinaldatCleanNegative(employ_datQ27.4.a$Q27.4.a, employ_datQ27.4.a)
#ordinal(employ_datQ27.4.a$CatOutcome, employ_datQ27.4.a$Academic, employ_datQ27.4.a)
prep <- analysisPrep(employ_datQ27.4.a$CatOutcome, employ_datQ27.4.a$Academic, employ_datQ27.4.a)
analysis <- polr(employ_datQ27.4.a$CatOutcome ~ employ_datQ27.4.a$Academic, data=employ_datQ27.4.a, Hess=TRUE)
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
Question <- "Q27.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.4.aCollege<-multidatClean(Q27.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.4.a + Q3, data = employ_datQ27.4.aCollege)
conTable
detach(dat_long)
employ_datQ27.4.aCollege$Q3[(employ_datQ27.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.4.aCollege$Q3<- factor(employ_datQ27.4.aCollege$Q3)
employ_datQ27.4.aCollege<-ordinaldatCleanNegative(employ_datQ27.4.aCollege$Q27.4.a, employ_datQ27.4.aCollege)
#ordinal(employ_datQ27.4.aCollege$CatOutcome, employ_datQ27.4.aCollege$Q3, employ_datQ27.4.aCollege)
prep <- analysisPrep(employ_datQ27.4.aCollege$CatOutcome, employ_datQ27.4.aCollege$Q3, employ_datQ27.4.aCollege)
analysis <- polr(employ_datQ27.4.aCollege$CatOutcome ~ employ_datQ27.4.aCollege$Q3, data=employ_datQ27.4.aCollege, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.4.aCarer<-multidatClean(Q27.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aCarer$Carer<- factor(employ_datQ27.4.aCarer$Carer)
employ_datQ27.4.aCarer<-ordinaldatCleanNegative(employ_datQ27.4.aCarer$Q27.4.a, employ_datQ27.4.aCarer)
#ordinal(employ_datQ27.4.aCarer$CatOutcome, employ_datQ27.4.aCarer$Carer, employ_datQ27.4.aCarer)
prep <- analysisPrep(employ_datQ27.4.aCarer$CatOutcome, employ_datQ27.4.aCarer$Carer, employ_datQ27.4.aCarer)
analysis <- polr(employ_datQ27.4.aCarer$CatOutcome ~ employ_datQ27.4.aCarer$Carer, data=employ_datQ27.4.aCarer, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.4.aDisability<-multidatClean(Q27.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aDisability$Disability<- factor(employ_datQ27.4.aDisability$Disability)
employ_datQ27.4.aDisability<-ordinaldatCleanNegative(employ_datQ27.4.aDisability$Q27.4.a, employ_datQ27.4.aDisability)
conTable <- xtabs(~Q27.4.a + Disability, data = employ_datQ27.4.aDisability)
#ordinal(employ_datQ27.4.aDisability$CatOutcome, employ_datQ27.4.aDisability$Disability, employ_datQ27.4.aDisability)
prep <- analysisPrep(employ_datQ27.4.aDisability$CatOutcome, employ_datQ27.4.aDisability$Disability, employ_datQ27.4.aDisability)
analysis <- polr(employ_datQ27.4.aDisability$CatOutcome ~ employ_datQ27.4.aDisability$Disability, data=employ_datQ27.4.aDisability, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.4.aEthnicity<-multidatClean(Q27.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aEthnicity$Ethnicity<- factor(employ_datQ27.4.aEthnicity$EthnicityCleaned)
employ_datQ27.4.aEthnicity<-ordinaldatCleanNegative(employ_datQ27.4.aEthnicity$Q27.4.a, employ_datQ27.4.aEthnicity)
conTable <- xtabs(~Q27.4.a + EthnicityCleaned, data = employ_datQ27.4.aEthnicity)
conTable
#ordinal(employ_datQ27.4.aEthnicity$CatOutcome, employ_datQ27.4.aEthnicity$EthnicityCleaned, employ_datQ27.4.aEthnicity)
prep <- analysisPrep(employ_datQ27.4.aEthnicity$CatOutcome, employ_datQ27.4.aEthnicity$EthnicityCleaned, employ_datQ27.4.aEthnicity)
analysis <- polr(employ_datQ27.4.aEthnicity$CatOutcome ~ employ_datQ27.4.aEthnicity$EthnicityCleaned, data=employ_datQ27.4.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.4.aFirstGen<-multidatClean(Q27.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aFirstGen$FirstGen<-factor(employ_datQ27.4.aFirstGen$FirstGen)
employ_datQ27.4.aFirstGen<-ordinaldatCleanNegative(employ_datQ27.4.aFirstGen$Q27.4.a, employ_datQ27.4.aFirstGen)
#ordinal(employ_datQ27.4.aFirstGen$CatOutcome, employ_datQ27.4.aFirstGen$FirstGen, employ_datQ27.4.aFirstGen)
prep <- analysisPrep(employ_datQ27.4.aFirstGen$CatOutcome, employ_datQ27.4.aFirstGen$FirstGen, employ_datQ27.4.aFirstGen)
analysis <- polr(employ_datQ27.4.aFirstGen$CatOutcome ~ employ_datQ27.4.aFirstGen$FirstGen, data=employ_datQ27.4.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.4.aGender<-multidatClean(Q27.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aGender$Gender<-factor(employ_datQ27.4.aGender$Gender)
employ_datQ27.4.aGender<-ordinaldatCleanNegative(employ_datQ27.4.aGender$Q27.4.a, employ_datQ27.4.aGender)
#ordinal(employ_datQ27.4.aGender$CatOutcome, employ_datQ27.4.aGender$Gender, employ_datQ27.4.aGender)
prep <- analysisPrep(employ_datQ27.4.aGender$CatOutcome, employ_datQ27.4.aGender$Gender, employ_datQ27.4.aGender)
analysis <- polr(employ_datQ27.4.aGender$CatOutcome ~ employ_datQ27.4.aGender$Gender, data=employ_datQ27.4.aGender, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.4.aSexuality<-multidatClean(Q27.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.4.aSexuality$Sexuality<-factor(employ_datQ27.4.aSexuality$Sexuality)
employ_datQ27.4.aSexuality<-ordinaldatCleanNegative(employ_datQ27.4.aSexuality$Q27.4.a, employ_datQ27.4.aSexuality)
#ordinal(employ_datQ27.4.aSexuality$CatOutcome, employ_datQ27.4.aSexuality$Sexuality, employ_datQ27.4.aSexuality)
prep <- analysisPrep(employ_datQ27.4.aSexuality$CatOutcome, employ_datQ27.4.aSexuality$Sexuality, employ_datQ27.4.aSexuality)
analysis <- polr(employ_datQ27.4.aSexuality$CatOutcome ~ employ_datQ27.4.aSexuality$Sexuality, data=employ_datQ27.4.aSexuality, Hess=TRUE)
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
Question <-  "Q27.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q26 - How far do you agree or disagree with the following statements relating to research culture?"
"Creativity is stifled due to research being driven by an impact agenda / emphasis on impact"
"Q27.5.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.5.a<-multidatClean(Q27.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.5.a + Academic, data = employ_datQ27.5.a)
detach(dat_long)
employ_datQ27.5.a<-ordinaldatCleanNegative(employ_datQ27.5.a$Q27.5.a, employ_datQ27.5.a)
#ordinal(employ_datQ27.5.a$CatOutcome, employ_datQ27.5.a$Academic, employ_datQ27.5.a)
prep <- analysisPrep(employ_datQ27.5.a$CatOutcome, employ_datQ27.5.a$Academic, employ_datQ27.5.a)
analysis <- polr(employ_datQ27.5.a$CatOutcome ~ employ_datQ27.5.a$Academic, data=employ_datQ27.5.a, Hess=TRUE)
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
Question <- "Q27.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.5.aCollege<-multidatClean(Q27.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.5.a + Q3, data = employ_datQ27.5.aCollege)
conTable
detach(dat_long)
employ_datQ27.5.aCollege$Q3[(employ_datQ27.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.5.aCollege$Q3<- factor(employ_datQ27.5.aCollege$Q3)
employ_datQ27.5.aCollege<-ordinaldatCleanNegative(employ_datQ27.5.aCollege$Q27.5.a, employ_datQ27.5.aCollege)
#ordinal(employ_datQ27.5.aCollege$CatOutcome, employ_datQ27.5.aCollege$Q3, employ_datQ27.5.aCollege)
prep <- analysisPrep(employ_datQ27.5.aCollege$CatOutcome, employ_datQ27.5.aCollege$Q3, employ_datQ27.5.aCollege)
analysis <- polr(employ_datQ27.5.aCollege$CatOutcome ~ employ_datQ27.5.aCollege$Q3, data=employ_datQ27.5.aCollege, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.5.aCarer<-multidatClean(Q27.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aCarer$Carer<- factor(employ_datQ27.5.aCarer$Carer)
employ_datQ27.5.aCarer<-ordinaldatCleanNegative(employ_datQ27.5.aCarer$Q27.5.a, employ_datQ27.5.aCarer)
#ordinal(employ_datQ27.5.aCarer$CatOutcome, employ_datQ27.5.aCarer$Carer, employ_datQ27.5.aCarer)
prep <- analysisPrep(employ_datQ27.5.aCarer$CatOutcome, employ_datQ27.5.aCarer$Carer, employ_datQ27.5.aCarer)
analysis <- polr(employ_datQ27.5.aCarer$CatOutcome ~ employ_datQ27.5.aCarer$Carer, data=employ_datQ27.5.aCarer, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.5.aDisability<-multidatClean(Q27.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aDisability$Disability<- factor(employ_datQ27.5.aDisability$Disability)
employ_datQ27.5.aDisability<-ordinaldatCleanNegative(employ_datQ27.5.aDisability$Q27.5.a, employ_datQ27.5.aDisability)
conTable <- xtabs(~Q27.5.a + Disability, data = employ_datQ27.5.aDisability)
#ordinal(employ_datQ27.5.aDisability$CatOutcome, employ_datQ27.5.aDisability$Disability, employ_datQ27.5.aDisability)
prep <- analysisPrep(employ_datQ27.5.aDisability$CatOutcome, employ_datQ27.5.aDisability$Disability, employ_datQ27.5.aDisability)
analysis <- polr(employ_datQ27.5.aDisability$CatOutcome ~ employ_datQ27.5.aDisability$Disability, data=employ_datQ27.5.aDisability, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.5.aEthnicity<-multidatClean(Q27.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aEthnicity$Ethnicity<- factor(employ_datQ27.5.aEthnicity$EthnicityCleaned)
employ_datQ27.5.aEthnicity<-ordinaldatCleanNegative(employ_datQ27.5.aEthnicity$Q27.5.a, employ_datQ27.5.aEthnicity)
conTable <- xtabs(~Q27.5.a + EthnicityCleaned, data = employ_datQ27.5.aEthnicity)
conTable
#ordinal(employ_datQ27.5.aEthnicity$CatOutcome, employ_datQ27.5.aEthnicity$EthnicityCleaned, employ_datQ27.5.aEthnicity)
prep <- analysisPrep(employ_datQ27.5.aEthnicity$CatOutcome, employ_datQ27.5.aEthnicity$EthnicityCleaned, employ_datQ27.5.aEthnicity)
analysis <- polr(employ_datQ27.5.aEthnicity$CatOutcome ~ employ_datQ27.5.aEthnicity$EthnicityCleaned, data=employ_datQ27.5.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.5.aFirstGen<-multidatClean(Q27.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aFirstGen$FirstGen<-factor(employ_datQ27.5.aFirstGen$FirstGen)
employ_datQ27.5.aFirstGen<-ordinaldatCleanNegative(employ_datQ27.5.aFirstGen$Q27.5.a, employ_datQ27.5.aFirstGen)
#ordinal(employ_datQ27.5.aFirstGen$CatOutcome, employ_datQ27.5.aFirstGen$FirstGen, employ_datQ27.5.aFirstGen)
prep <- analysisPrep(employ_datQ27.5.aFirstGen$CatOutcome, employ_datQ27.5.aFirstGen$FirstGen, employ_datQ27.5.aFirstGen)
analysis <- polr(employ_datQ27.5.aFirstGen$CatOutcome ~ employ_datQ27.5.aFirstGen$FirstGen, data=employ_datQ27.5.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.5.aGender<-multidatClean(Q27.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aGender$Gender<-factor(employ_datQ27.5.aGender$Gender)
employ_datQ27.5.aGender<-ordinaldatCleanNegative(employ_datQ27.5.aGender$Q27.5.a, employ_datQ27.5.aGender)
#ordinal(employ_datQ27.5.aGender$CatOutcome, employ_datQ27.5.aGender$Gender, employ_datQ27.5.aGender)
prep <- analysisPrep(employ_datQ27.5.aGender$CatOutcome, employ_datQ27.5.aGender$Gender, employ_datQ27.5.aGender)
analysis <- polr(employ_datQ27.5.aGender$CatOutcome ~ employ_datQ27.5.aGender$Gender, data=employ_datQ27.5.aGender, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.5.aSexuality<-multidatClean(Q27.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.5.aSexuality$Sexuality<-factor(employ_datQ27.5.aSexuality$Sexuality)
employ_datQ27.5.aSexuality<-ordinaldatCleanNegative(employ_datQ27.5.aSexuality$Q27.5.a, employ_datQ27.5.aSexuality)
#ordinal(employ_datQ27.5.aSexuality$CatOutcome, employ_datQ27.5.aSexuality$Sexuality, employ_datQ27.5.aSexuality)
prep <- analysisPrep(employ_datQ27.5.aSexuality$CatOutcome, employ_datQ27.5.aSexuality$Sexuality, employ_datQ27.5.aSexuality)
analysis <- polr(employ_datQ27.5.aSexuality$CatOutcome ~ employ_datQ27.5.aSexuality$Sexuality, data=employ_datQ27.5.aSexuality, Hess=TRUE)
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
Question <-  "Q27.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"The current culture supports research productivity"
"Status"
"Q27.6.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.6.a<-multidatClean(Q27.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.6.a + Academic, data = employ_datQ27.6.a)
detach(dat_long)
employ_datQ27.6.a<-ordinaldatClean(employ_datQ27.6.a$Q27.6.a, employ_datQ27.6.a)
#ordinal(employ_datQ27.6.a$CatOutcome, employ_datQ27.6.a$Academic, employ_datQ27.6.a)
prep <- analysisPrep(employ_datQ27.6.a$CatOutcome, employ_datQ27.6.a$Academic, employ_datQ27.6.a)
analysis <- polr(employ_datQ27.6.a$CatOutcome ~ employ_datQ27.6.a$Academic, data=employ_datQ27.6.a, Hess=TRUE)
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
Question <- "Q27.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.6.aCollege<-multidatClean(Q27.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.6.a + Q3, data = employ_datQ27.6.aCollege)
conTable
detach(dat_long)
employ_datQ27.6.aCollege$Q3[(employ_datQ27.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.6.aCollege$Q3<- factor(employ_datQ27.6.aCollege$Q3)
employ_datQ27.6.aCollege<-ordinaldatClean(employ_datQ27.6.aCollege$Q27.6.a, employ_datQ27.6.aCollege)
#ordinal(employ_datQ27.6.aCollege$CatOutcome, employ_datQ27.6.aCollege$Q3, employ_datQ27.6.aCollege)
prep <- analysisPrep(employ_datQ27.6.aCollege$CatOutcome, employ_datQ27.6.aCollege$Q3, employ_datQ27.6.aCollege)
analysis <- polr(employ_datQ27.6.aCollege$CatOutcome ~ employ_datQ27.6.aCollege$Q3, data=employ_datQ27.6.aCollege, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.6.aCarer<-multidatClean(Q27.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aCarer$Carer<- factor(employ_datQ27.6.aCarer$Carer)
employ_datQ27.6.aCarer<-ordinaldatClean(employ_datQ27.6.aCarer$Q27.6.a, employ_datQ27.6.aCarer)
#ordinal(employ_datQ27.6.aCarer$CatOutcome, employ_datQ27.6.aCarer$Carer, employ_datQ27.6.aCarer)
prep <- analysisPrep(employ_datQ27.6.aCarer$CatOutcome, employ_datQ27.6.aCarer$Carer, employ_datQ27.6.aCarer)
analysis <- polr(employ_datQ27.6.aCarer$CatOutcome ~ employ_datQ27.6.aCarer$Carer, data=employ_datQ27.6.aCarer, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.6.aDisability<-multidatClean(Q27.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aDisability$Disability<- factor(employ_datQ27.6.aDisability$Disability)
employ_datQ27.6.aDisability<-ordinaldatClean(employ_datQ27.6.aDisability$Q27.6.a, employ_datQ27.6.aDisability)
conTable <- xtabs(~Q27.6.a + Disability, data = employ_datQ27.6.aDisability)
#ordinal(employ_datQ27.6.aDisability$CatOutcome, employ_datQ27.6.aDisability$Disability, employ_datQ27.6.aDisability)
prep <- analysisPrep(employ_datQ27.6.aDisability$CatOutcome, employ_datQ27.6.aDisability$Disability, employ_datQ27.6.aDisability)
analysis <- polr(employ_datQ27.6.aDisability$CatOutcome ~ employ_datQ27.6.aDisability$Disability, data=employ_datQ27.6.aDisability, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.6.aEthnicity<-multidatClean(Q27.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aEthnicity$Ethnicity<- factor(employ_datQ27.6.aEthnicity$EthnicityCleaned)
employ_datQ27.6.aEthnicity<-ordinaldatClean(employ_datQ27.6.aEthnicity$Q27.6.a, employ_datQ27.6.aEthnicity)
conTable <- xtabs(~Q27.6.a + EthnicityCleaned, data = employ_datQ27.6.aEthnicity)
conTable
#ordinal(employ_datQ27.6.aEthnicity$CatOutcome, employ_datQ27.6.aEthnicity$EthnicityCleaned, employ_datQ27.6.aEthnicity)
prep <- analysisPrep(employ_datQ27.6.aEthnicity$CatOutcome, employ_datQ27.6.aEthnicity$EthnicityCleaned, employ_datQ27.6.aEthnicity)
analysis <- polr(employ_datQ27.6.aEthnicity$CatOutcome ~ employ_datQ27.6.aEthnicity$EthnicityCleaned, data=employ_datQ27.6.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.6.aFirstGen<-multidatClean(Q27.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aFirstGen$FirstGen<-factor(employ_datQ27.6.aFirstGen$FirstGen)
employ_datQ27.6.aFirstGen<-ordinaldatClean(employ_datQ27.6.aFirstGen$Q27.6.a, employ_datQ27.6.aFirstGen)
#ordinal(employ_datQ27.6.aFirstGen$CatOutcome, employ_datQ27.6.aFirstGen$FirstGen, employ_datQ27.6.aFirstGen)
prep <- analysisPrep(employ_datQ27.6.aFirstGen$CatOutcome, employ_datQ27.6.aFirstGen$FirstGen, employ_datQ27.6.aFirstGen)
analysis <- polr(employ_datQ27.6.aFirstGen$CatOutcome ~ employ_datQ27.6.aFirstGen$FirstGen, data=employ_datQ27.6.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.6.aGender<-multidatClean(Q27.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aGender$Gender<-factor(employ_datQ27.6.aGender$Gender)
employ_datQ27.6.aGender<-ordinaldatClean(employ_datQ27.6.aGender$Q27.6.a, employ_datQ27.6.aGender)
#ordinal(employ_datQ27.6.aGender$CatOutcome, employ_datQ27.6.aGender$Gender, employ_datQ27.6.aGender)
prep <- analysisPrep(employ_datQ27.6.aGender$CatOutcome, employ_datQ27.6.aGender$Gender, employ_datQ27.6.aGender)
analysis <- polr(employ_datQ27.6.aGender$CatOutcome ~ employ_datQ27.6.aGender$Gender, data=employ_datQ27.6.aGender, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.6.aSexuality<-multidatClean(Q27.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.6.aSexuality$Sexuality<-factor(employ_datQ27.6.aSexuality$Sexuality)
employ_datQ27.6.aSexuality<-ordinaldatClean(employ_datQ27.6.aSexuality$Q27.6.a, employ_datQ27.6.aSexuality)
#ordinal(employ_datQ27.6.aSexuality$CatOutcome, employ_datQ27.6.aSexuality$Sexuality, employ_datQ27.6.aSexuality)
prep <- analysisPrep(employ_datQ27.6.aSexuality$CatOutcome, employ_datQ27.6.aSexuality$Sexuality, employ_datQ27.6.aSexuality)
analysis <- polr(employ_datQ27.6.aSexuality$CatOutcome ~ employ_datQ27.6.aSexuality$Sexuality, data=employ_datQ27.6.aSexuality, Hess=TRUE)
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
Question <-  "Q27.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"Current research culture is healthy"
"Q27.7.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.7.a<-multidatClean(Q27.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.7.a + Academic, data = employ_datQ27.7.a)
detach(dat_long)
employ_datQ27.7.a<-ordinaldatClean(employ_datQ27.7.a$Q27.7.a, employ_datQ27.7.a)
#ordinal(employ_datQ27.7.a$CatOutcome, employ_datQ27.7.a$Academic, employ_datQ27.7.a)
prep <- analysisPrep(employ_datQ27.7.a$CatOutcome, employ_datQ27.7.a$Academic, employ_datQ27.7.a)
analysis <- polr(employ_datQ27.7.a$CatOutcome ~ employ_datQ27.7.a$Academic, data=employ_datQ27.7.a, Hess=TRUE)
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
Question <- "Q27.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.7.aCollege<-multidatClean(Q27.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.7.a + Q3, data = employ_datQ27.7.aCollege)
conTable
detach(dat_long)
employ_datQ27.7.aCollege$Q3[(employ_datQ27.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.7.aCollege$Q3<- factor(employ_datQ27.7.aCollege$Q3)
employ_datQ27.7.aCollege<-ordinaldatClean(employ_datQ27.7.aCollege$Q27.7.a, employ_datQ27.7.aCollege)
#ordinal(employ_datQ27.7.aCollege$CatOutcome, employ_datQ27.7.aCollege$Q3, employ_datQ27.7.aCollege)
prep <- analysisPrep(employ_datQ27.7.aCollege$CatOutcome, employ_datQ27.7.aCollege$Q3, employ_datQ27.7.aCollege)
analysis <- polr(employ_datQ27.7.aCollege$CatOutcome ~ employ_datQ27.7.aCollege$Q3, data=employ_datQ27.7.aCollege, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.7.aCarer<-multidatClean(Q27.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aCarer$Carer<- factor(employ_datQ27.7.aCarer$Carer)
employ_datQ27.7.aCarer<-ordinaldatClean(employ_datQ27.7.aCarer$Q27.7.a, employ_datQ27.7.aCarer)
#ordinal(employ_datQ27.7.aCarer$CatOutcome, employ_datQ27.7.aCarer$Carer, employ_datQ27.7.aCarer)
prep <- analysisPrep(employ_datQ27.7.aCarer$CatOutcome, employ_datQ27.7.aCarer$Carer, employ_datQ27.7.aCarer)
analysis <- polr(employ_datQ27.7.aCarer$CatOutcome ~ employ_datQ27.7.aCarer$Carer, data=employ_datQ27.7.aCarer, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.7.aDisability<-multidatClean(Q27.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aDisability$Disability<- factor(employ_datQ27.7.aDisability$Disability)
employ_datQ27.7.aDisability<-ordinaldatClean(employ_datQ27.7.aDisability$Q27.7.a, employ_datQ27.7.aDisability)
conTable <- xtabs(~Q27.7.a + Disability, data = employ_datQ27.7.aDisability)
#ordinal(employ_datQ27.7.aDisability$CatOutcome, employ_datQ27.7.aDisability$Disability, employ_datQ27.7.aDisability)
prep <- analysisPrep(employ_datQ27.7.aDisability$CatOutcome, employ_datQ27.7.aDisability$Disability, employ_datQ27.7.aDisability)
analysis <- polr(employ_datQ27.7.aDisability$CatOutcome ~ employ_datQ27.7.aDisability$Disability, data=employ_datQ27.7.aDisability, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.7.aEthnicity<-multidatClean(Q27.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aEthnicity$Ethnicity<- factor(employ_datQ27.7.aEthnicity$EthnicityCleaned)
employ_datQ27.7.aEthnicity<-ordinaldatClean(employ_datQ27.7.aEthnicity$Q27.7.a, employ_datQ27.7.aEthnicity)
conTable <- xtabs(~Q27.7.a + EthnicityCleaned, data = employ_datQ27.7.aEthnicity)
conTable
#ordinal(employ_datQ27.7.aEthnicity$CatOutcome, employ_datQ27.7.aEthnicity$EthnicityCleaned, employ_datQ27.7.aEthnicity)
prep <- analysisPrep(employ_datQ27.7.aEthnicity$CatOutcome, employ_datQ27.7.aEthnicity$EthnicityCleaned, employ_datQ27.7.aEthnicity)
analysis <- polr(employ_datQ27.7.aEthnicity$CatOutcome ~ employ_datQ27.7.aEthnicity$EthnicityCleaned, data=employ_datQ27.7.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.7.aFirstGen<-multidatClean(Q27.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aFirstGen$FirstGen<-factor(employ_datQ27.7.aFirstGen$FirstGen)
employ_datQ27.7.aFirstGen<-ordinaldatClean(employ_datQ27.7.aFirstGen$Q27.7.a, employ_datQ27.7.aFirstGen)
#ordinal(employ_datQ27.7.aFirstGen$CatOutcome, employ_datQ27.7.aFirstGen$FirstGen, employ_datQ27.7.aFirstGen)
prep <- analysisPrep(employ_datQ27.7.aFirstGen$CatOutcome, employ_datQ27.7.aFirstGen$FirstGen, employ_datQ27.7.aFirstGen)
analysis <- polr(employ_datQ27.7.aFirstGen$CatOutcome ~ employ_datQ27.7.aFirstGen$FirstGen, data=employ_datQ27.7.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.7.aGender<-multidatClean(Q27.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aGender$Gender<-factor(employ_datQ27.7.aGender$Gender)
employ_datQ27.7.aGender<-ordinaldatClean(employ_datQ27.7.aGender$Q27.7.a, employ_datQ27.7.aGender)
#ordinal(employ_datQ27.7.aGender$CatOutcome, employ_datQ27.7.aGender$Gender, employ_datQ27.7.aGender)
prep <- analysisPrep(employ_datQ27.7.aGender$CatOutcome, employ_datQ27.7.aGender$Gender, employ_datQ27.7.aGender)
analysis <- polr(employ_datQ27.7.aGender$CatOutcome ~ employ_datQ27.7.aGender$Gender, data=employ_datQ27.7.aGender, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.7.aSexuality<-multidatClean(Q27.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.7.aSexuality$Sexuality<-factor(employ_datQ27.7.aSexuality$Sexuality)
employ_datQ27.7.aSexuality<-ordinaldatClean(employ_datQ27.7.aSexuality$Q27.7.a, employ_datQ27.7.aSexuality)
#ordinal(employ_datQ27.7.aSexuality$CatOutcome, employ_datQ27.7.aSexuality$Sexuality, employ_datQ27.7.aSexuality)
prep <- analysisPrep(employ_datQ27.7.aSexuality$CatOutcome, employ_datQ27.7.aSexuality$Sexuality, employ_datQ27.7.aSexuality)
analysis <- polr(employ_datQ27.7.aSexuality$CatOutcome ~ employ_datQ27.7.aSexuality$Sexuality, data=employ_datQ27.7.aSexuality, Hess=TRUE)
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
Question <-  "Q27.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"Current research culture is unsustainable long-term"
"Q27.8.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.8.a<-multidatClean(Q27.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.8.a + Academic, data = employ_datQ27.8.a)
detach(dat_long)
employ_datQ27.8.a<-ordinaldatCleanNegative(employ_datQ27.8.a$Q27.8.a, employ_datQ27.8.a)
#ordinal(employ_datQ27.8.a$CatOutcome, employ_datQ27.8.a$Academic, employ_datQ27.8.a)
prep <- analysisPrep(employ_datQ27.8.a$CatOutcome, employ_datQ27.8.a$Academic, employ_datQ27.8.a)
analysis <- polr(employ_datQ27.8.a$CatOutcome ~ employ_datQ27.8.a$Academic, data=employ_datQ27.8.a, Hess=TRUE)
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
Question <- "Q27.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.8.aCollege<-multidatClean(Q27.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.8.a + Q3, data = employ_datQ27.8.aCollege)
conTable
detach(dat_long)
employ_datQ27.8.aCollege$Q3[(employ_datQ27.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.8.aCollege$Q3<- factor(employ_datQ27.8.aCollege$Q3)
employ_datQ27.8.aCollege<-ordinaldatCleanNegative(employ_datQ27.8.aCollege$Q27.8.a, employ_datQ27.8.aCollege)
#ordinal(employ_datQ27.8.aCollege$CatOutcome, employ_datQ27.8.aCollege$Q3, employ_datQ27.8.aCollege)
prep <- analysisPrep(employ_datQ27.8.aCollege$CatOutcome, employ_datQ27.8.aCollege$Q3, employ_datQ27.8.aCollege)
analysis <- polr(employ_datQ27.8.aCollege$CatOutcome ~ employ_datQ27.8.aCollege$Q3, data=employ_datQ27.8.aCollege, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.8.aCarer<-multidatClean(Q27.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aCarer$Carer<- factor(employ_datQ27.8.aCarer$Carer)
employ_datQ27.8.aCarer<-ordinaldatCleanNegative(employ_datQ27.8.aCarer$Q27.8.a, employ_datQ27.8.aCarer)
#ordinal(employ_datQ27.8.aCarer$CatOutcome, employ_datQ27.8.aCarer$Carer, employ_datQ27.8.aCarer)
prep <- analysisPrep(employ_datQ27.8.aCarer$CatOutcome, employ_datQ27.8.aCarer$Carer, employ_datQ27.8.aCarer)
analysis <- polr(employ_datQ27.8.aCarer$CatOutcome ~ employ_datQ27.8.aCarer$Carer, data=employ_datQ27.8.aCarer, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.8.aDisability<-multidatClean(Q27.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aDisability$Disability<- factor(employ_datQ27.8.aDisability$Disability)
employ_datQ27.8.aDisability<-ordinaldatCleanNegative(employ_datQ27.8.aDisability$Q27.8.a, employ_datQ27.8.aDisability)
conTable <- xtabs(~Q27.8.a + Disability, data = employ_datQ27.8.aDisability)
#ordinal(employ_datQ27.8.aDisability$CatOutcome, employ_datQ27.8.aDisability$Disability, employ_datQ27.8.aDisability)
prep <- analysisPrep(employ_datQ27.8.aDisability$CatOutcome, employ_datQ27.8.aDisability$Disability, employ_datQ27.8.aDisability)
analysis <- polr(employ_datQ27.8.aDisability$CatOutcome ~ employ_datQ27.8.aDisability$Disability, data=employ_datQ27.8.aDisability, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.8.aEthnicity<-multidatClean(Q27.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aEthnicity$Ethnicity<- factor(employ_datQ27.8.aEthnicity$EthnicityCleaned)
employ_datQ27.8.aEthnicity<-ordinaldatCleanNegative(employ_datQ27.8.aEthnicity$Q27.8.a, employ_datQ27.8.aEthnicity)
conTable <- xtabs(~Q27.8.a + EthnicityCleaned, data = employ_datQ27.8.aEthnicity)
conTable
#ordinal(employ_datQ27.8.aEthnicity$CatOutcome, employ_datQ27.8.aEthnicity$EthnicityCleaned, employ_datQ27.8.aEthnicity)
prep <- analysisPrep(employ_datQ27.8.aEthnicity$CatOutcome, employ_datQ27.8.aEthnicity$EthnicityCleaned, employ_datQ27.8.aEthnicity)
analysis <- polr(employ_datQ27.8.aEthnicity$CatOutcome ~ employ_datQ27.8.aEthnicity$EthnicityCleaned, data=employ_datQ27.8.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.8.aFirstGen<-multidatClean(Q27.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aFirstGen$FirstGen<-factor(employ_datQ27.8.aFirstGen$FirstGen)
employ_datQ27.8.aFirstGen<-ordinaldatCleanNegative(employ_datQ27.8.aFirstGen$Q27.8.a, employ_datQ27.8.aFirstGen)
#ordinal(employ_datQ27.8.aFirstGen$CatOutcome, employ_datQ27.8.aFirstGen$FirstGen, employ_datQ27.8.aFirstGen)
prep <- analysisPrep(employ_datQ27.8.aFirstGen$CatOutcome, employ_datQ27.8.aFirstGen$FirstGen, employ_datQ27.8.aFirstGen)
analysis <- polr(employ_datQ27.8.aFirstGen$CatOutcome ~ employ_datQ27.8.aFirstGen$FirstGen, data=employ_datQ27.8.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.8.aGender<-multidatClean(Q27.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aGender$Gender<-factor(employ_datQ27.8.aGender$Gender)
employ_datQ27.8.aGender<-ordinaldatCleanNegative(employ_datQ27.8.aGender$Q27.8.a, employ_datQ27.8.aGender)
#ordinal(employ_datQ27.8.aGender$CatOutcome, employ_datQ27.8.aGender$Gender, employ_datQ27.8.aGender)
prep <- analysisPrep(employ_datQ27.8.aGender$CatOutcome, employ_datQ27.8.aGender$Gender, employ_datQ27.8.aGender)
analysis <- polr(employ_datQ27.8.aGender$CatOutcome ~ employ_datQ27.8.aGender$Gender, data=employ_datQ27.8.aGender, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.8.aSexuality<-multidatClean(Q27.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.8.aSexuality$Sexuality<-factor(employ_datQ27.8.aSexuality$Sexuality)
employ_datQ27.8.aSexuality<-ordinaldatCleanNegative(employ_datQ27.8.aSexuality$Q27.8.a, employ_datQ27.8.aSexuality)
#ordinal(employ_datQ27.8.aSexuality$CatOutcome, employ_datQ27.8.aSexuality$Sexuality, employ_datQ27.8.aSexuality)
prep <- analysisPrep(employ_datQ27.8.aSexuality$CatOutcome, employ_datQ27.8.aSexuality$Sexuality, employ_datQ27.8.aSexuality)
analysis <- polr(employ_datQ27.8.aSexuality$CatOutcome ~ employ_datQ27.8.aSexuality$Sexuality, data=employ_datQ27.8.aSexuality, Hess=TRUE)
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
Question <-  "Q27.8.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"Initiatives to increase diversity and inclusion in research have gone far enough"
"Q27.9.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.9.a<-multidatClean(Q27.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.9.a + Academic, data = employ_datQ27.9.a)
detach(dat_long)
employ_datQ27.9.a<-ordinaldatClean(employ_datQ27.9.a$Q27.9.a, employ_datQ27.9.a)
#ordinal(employ_datQ27.9.a$CatOutcome, employ_datQ27.9.a$Academic, employ_datQ27.9.a)
prep <- analysisPrep(employ_datQ27.9.a$CatOutcome, employ_datQ27.9.a$Academic, employ_datQ27.9.a)
analysis <- polr(employ_datQ27.9.a$CatOutcome ~ employ_datQ27.9.a$Academic, data=employ_datQ27.9.a, Hess=TRUE)
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
Question <- "Q27.9.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.9.aCollege<-multidatClean(Q27.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.9.a + Q3, data = employ_datQ27.9.aCollege)
conTable
detach(dat_long)
employ_datQ27.9.aCollege$Q3[(employ_datQ27.9.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.9.aCollege$Q3<- factor(employ_datQ27.9.aCollege$Q3)
employ_datQ27.9.aCollege<-ordinaldatClean(employ_datQ27.9.aCollege$Q27.9.a, employ_datQ27.9.aCollege)
#ordinal(employ_datQ27.9.aCollege$CatOutcome, employ_datQ27.9.aCollege$Q3, employ_datQ27.9.aCollege)
prep <- analysisPrep(employ_datQ27.9.aCollege$CatOutcome, employ_datQ27.9.aCollege$Q3, employ_datQ27.9.aCollege)
analysis <- polr(employ_datQ27.9.aCollege$CatOutcome ~ employ_datQ27.9.aCollege$Q3, data=employ_datQ27.9.aCollege, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.9.aCarer<-multidatClean(Q27.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aCarer$Carer<- factor(employ_datQ27.9.aCarer$Carer)
employ_datQ27.9.aCarer<-ordinaldatClean(employ_datQ27.9.aCarer$Q27.9.a, employ_datQ27.9.aCarer)
#ordinal(employ_datQ27.9.aCarer$CatOutcome, employ_datQ27.9.aCarer$Carer, employ_datQ27.9.aCarer)
prep <- analysisPrep(employ_datQ27.9.aCarer$CatOutcome, employ_datQ27.9.aCarer$Carer, employ_datQ27.9.aCarer)
analysis <- polr(employ_datQ27.9.aCarer$CatOutcome ~ employ_datQ27.9.aCarer$Carer, data=employ_datQ27.9.aCarer, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.9.aDisability<-multidatClean(Q27.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aDisability$Disability<- factor(employ_datQ27.9.aDisability$Disability)
employ_datQ27.9.aDisability<-ordinaldatClean(employ_datQ27.9.aDisability$Q27.9.a, employ_datQ27.9.aDisability)
conTable <- xtabs(~Q27.9.a + Disability, data = employ_datQ27.9.aDisability)
#ordinal(employ_datQ27.9.aDisability$CatOutcome, employ_datQ27.9.aDisability$Disability, employ_datQ27.9.aDisability)
prep <- analysisPrep(employ_datQ27.9.aDisability$CatOutcome, employ_datQ27.9.aDisability$Disability, employ_datQ27.9.aDisability)
analysis <- polr(employ_datQ27.9.aDisability$CatOutcome ~ employ_datQ27.9.aDisability$Disability, data=employ_datQ27.9.aDisability, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.9.aEthnicity<-multidatClean(Q27.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aEthnicity$Ethnicity<- factor(employ_datQ27.9.aEthnicity$EthnicityCleaned)
employ_datQ27.9.aEthnicity<-ordinaldatClean(employ_datQ27.9.aEthnicity$Q27.9.a, employ_datQ27.9.aEthnicity)
conTable <- xtabs(~Q27.9.a + EthnicityCleaned, data = employ_datQ27.9.aEthnicity)
conTable
#ordinal(employ_datQ27.9.aEthnicity$CatOutcome, employ_datQ27.9.aEthnicity$EthnicityCleaned, employ_datQ27.9.aEthnicity)
prep <- analysisPrep(employ_datQ27.9.aEthnicity$CatOutcome, employ_datQ27.9.aEthnicity$EthnicityCleaned, employ_datQ27.9.aEthnicity)
analysis <- polr(employ_datQ27.9.aEthnicity$CatOutcome ~ employ_datQ27.9.aEthnicity$EthnicityCleaned, data=employ_datQ27.9.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.9.aFirstGen<-multidatClean(Q27.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aFirstGen$FirstGen<-factor(employ_datQ27.9.aFirstGen$FirstGen)
employ_datQ27.9.aFirstGen<-ordinaldatClean(employ_datQ27.9.aFirstGen$Q27.9.a, employ_datQ27.9.aFirstGen)
#ordinal(employ_datQ27.9.aFirstGen$CatOutcome, employ_datQ27.9.aFirstGen$FirstGen, employ_datQ27.9.aFirstGen)
prep <- analysisPrep(employ_datQ27.9.aFirstGen$CatOutcome, employ_datQ27.9.aFirstGen$FirstGen, employ_datQ27.9.aFirstGen)
analysis <- polr(employ_datQ27.9.aFirstGen$CatOutcome ~ employ_datQ27.9.aFirstGen$FirstGen, data=employ_datQ27.9.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.9.aGender<-multidatClean(Q27.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aGender$Gender<-factor(employ_datQ27.9.aGender$Gender)
employ_datQ27.9.aGender<-ordinaldatClean(employ_datQ27.9.aGender$Q27.9.a, employ_datQ27.9.aGender)
#ordinal(employ_datQ27.9.aGender$CatOutcome, employ_datQ27.9.aGender$Gender, employ_datQ27.9.aGender)
prep <- analysisPrep(employ_datQ27.9.aGender$CatOutcome, employ_datQ27.9.aGender$Gender, employ_datQ27.9.aGender)
analysis <- polr(employ_datQ27.9.aGender$CatOutcome ~ employ_datQ27.9.aGender$Gender, data=employ_datQ27.9.aGender, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.9.aSexuality<-multidatClean(Q27.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.9.aSexuality$Sexuality<-factor(employ_datQ27.9.aSexuality$Sexuality)
employ_datQ27.9.aSexuality<-ordinaldatClean(employ_datQ27.9.aSexuality$Q27.9.a, employ_datQ27.9.aSexuality)
#ordinal(employ_datQ27.9.aSexuality$CatOutcome, employ_datQ27.9.aSexuality$Sexuality, employ_datQ27.9.aSexuality)
prep <- analysisPrep(employ_datQ27.9.aSexuality$CatOutcome, employ_datQ27.9.aSexuality$Sexuality, employ_datQ27.9.aSexuality)
analysis <- polr(employ_datQ27.9.aSexuality$CatOutcome ~ employ_datQ27.9.aSexuality$Sexuality, data=employ_datQ27.9.aSexuality, Hess=TRUE)
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
Question <-  "Q27.9.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.9.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"I think current metrics have had a positive impact on research culture"
"Q27.10.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.10.a<-multidatClean(Q27.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.10.a + Academic, data = employ_datQ27.10.a)
detach(dat_long)
employ_datQ27.10.a<-ordinaldatClean(employ_datQ27.10.a$Q27.10.a, employ_datQ27.10.a)
#ordinal(employ_datQ27.10.a$CatOutcome, employ_datQ27.10.a$Academic, employ_datQ27.10.a)
prep <- analysisPrep(employ_datQ27.10.a$CatOutcome, employ_datQ27.10.a$Academic, employ_datQ27.10.a)
analysis <- polr(employ_datQ27.10.a$CatOutcome ~ employ_datQ27.10.a$Academic, data=employ_datQ27.10.a, Hess=TRUE)
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
Question <- "Q27.10.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.10.aCollege<-multidatClean(Q27.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.10.a + Q3, data = employ_datQ27.10.aCollege)
conTable
detach(dat_long)
employ_datQ27.10.aCollege$Q3[(employ_datQ27.10.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.10.aCollege$Q3<- factor(employ_datQ27.10.aCollege$Q3)
employ_datQ27.10.aCollege<-ordinaldatClean(employ_datQ27.10.aCollege$Q27.10.a, employ_datQ27.10.aCollege)
#ordinal(employ_datQ27.10.aCollege$CatOutcome, employ_datQ27.10.aCollege$Q3, employ_datQ27.10.aCollege)
prep <- analysisPrep(employ_datQ27.10.aCollege$CatOutcome, employ_datQ27.10.aCollege$Q3, employ_datQ27.10.aCollege)
analysis <- polr(employ_datQ27.10.aCollege$CatOutcome ~ employ_datQ27.10.aCollege$Q3, data=employ_datQ27.10.aCollege, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.10.aCarer<-multidatClean(Q27.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aCarer$Carer<- factor(employ_datQ27.10.aCarer$Carer)
employ_datQ27.10.aCarer<-ordinaldatClean(employ_datQ27.10.aCarer$Q27.10.a, employ_datQ27.10.aCarer)
#ordinal(employ_datQ27.10.aCarer$CatOutcome, employ_datQ27.10.aCarer$Carer, employ_datQ27.10.aCarer)
prep <- analysisPrep(employ_datQ27.10.aCarer$CatOutcome, employ_datQ27.10.aCarer$Carer, employ_datQ27.10.aCarer)
analysis <- polr(employ_datQ27.10.aCarer$CatOutcome ~ employ_datQ27.10.aCarer$Carer, data=employ_datQ27.10.aCarer, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.10.aDisability<-multidatClean(Q27.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aDisability$Disability<- factor(employ_datQ27.10.aDisability$Disability)
employ_datQ27.10.aDisability<-ordinaldatClean(employ_datQ27.10.aDisability$Q27.10.a, employ_datQ27.10.aDisability)
conTable <- xtabs(~Q27.10.a + Disability, data = employ_datQ27.10.aDisability)
#ordinal(employ_datQ27.10.aDisability$CatOutcome, employ_datQ27.10.aDisability$Disability, employ_datQ27.10.aDisability)
prep <- analysisPrep(employ_datQ27.10.aDisability$CatOutcome, employ_datQ27.10.aDisability$Disability, employ_datQ27.10.aDisability)
analysis <- polr(employ_datQ27.10.aDisability$CatOutcome ~ employ_datQ27.10.aDisability$Disability, data=employ_datQ27.10.aDisability, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.10.aEthnicity<-multidatClean(Q27.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aEthnicity$Ethnicity<- factor(employ_datQ27.10.aEthnicity$EthnicityCleaned)
employ_datQ27.10.aEthnicity<-ordinaldatClean(employ_datQ27.10.aEthnicity$Q27.10.a, employ_datQ27.10.aEthnicity)
conTable <- xtabs(~Q27.10.a + EthnicityCleaned, data = employ_datQ27.10.aEthnicity)
conTable
#ordinal(employ_datQ27.10.aEthnicity$CatOutcome, employ_datQ27.10.aEthnicity$EthnicityCleaned, employ_datQ27.10.aEthnicity)
prep <- analysisPrep(employ_datQ27.10.aEthnicity$CatOutcome, employ_datQ27.10.aEthnicity$EthnicityCleaned, employ_datQ27.10.aEthnicity)
analysis <- polr(employ_datQ27.10.aEthnicity$CatOutcome ~ employ_datQ27.10.aEthnicity$EthnicityCleaned, data=employ_datQ27.10.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.10.aFirstGen<-multidatClean(Q27.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aFirstGen$FirstGen<-factor(employ_datQ27.10.aFirstGen$FirstGen)
employ_datQ27.10.aFirstGen<-ordinaldatClean(employ_datQ27.10.aFirstGen$Q27.10.a, employ_datQ27.10.aFirstGen)
#ordinal(employ_datQ27.10.aFirstGen$CatOutcome, employ_datQ27.10.aFirstGen$FirstGen, employ_datQ27.10.aFirstGen)
prep <- analysisPrep(employ_datQ27.10.aFirstGen$CatOutcome, employ_datQ27.10.aFirstGen$FirstGen, employ_datQ27.10.aFirstGen)
analysis <- polr(employ_datQ27.10.aFirstGen$CatOutcome ~ employ_datQ27.10.aFirstGen$FirstGen, data=employ_datQ27.10.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.10.aGender<-multidatClean(Q27.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aGender$Gender<-factor(employ_datQ27.10.aGender$Gender)
employ_datQ27.10.aGender<-ordinaldatClean(employ_datQ27.10.aGender$Q27.10.a, employ_datQ27.10.aGender)
#ordinal(employ_datQ27.10.aGender$CatOutcome, employ_datQ27.10.aGender$Gender, employ_datQ27.10.aGender)
prep <- analysisPrep(employ_datQ27.10.aGender$CatOutcome, employ_datQ27.10.aGender$Gender, employ_datQ27.10.aGender)
analysis <- polr(employ_datQ27.10.aGender$CatOutcome ~ employ_datQ27.10.aGender$Gender, data=employ_datQ27.10.aGender, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.10.aSexuality<-multidatClean(Q27.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.10.aSexuality$Sexuality<-factor(employ_datQ27.10.aSexuality$Sexuality)
employ_datQ27.10.aSexuality<-ordinaldatClean(employ_datQ27.10.aSexuality$Q27.10.a, employ_datQ27.10.aSexuality)
#ordinal(employ_datQ27.10.aSexuality$CatOutcome, employ_datQ27.10.aSexuality$Sexuality, employ_datQ27.10.aSexuality)
prep <- analysisPrep(employ_datQ27.10.aSexuality$CatOutcome, employ_datQ27.10.aSexuality$Sexuality, employ_datQ27.10.aSexuality)
analysis <- polr(employ_datQ27.10.aSexuality$CatOutcome ~ employ_datQ27.10.aSexuality$Sexuality, data=employ_datQ27.10.aSexuality, Hess=TRUE)
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
Question <-  "Q27.10.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.10.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q27 - How far do you agree or disagree with the following statements relating to research culture?"
"Grant funding is sufficiently flexible to support career breaks, or health and disability related leave"
"Q27.11.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ27.11.a<-multidatClean(Q27.11.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.11.a + Academic, data = employ_datQ27.11.a)
detach(dat_long)
employ_datQ27.11.a<-ordinaldatClean(employ_datQ27.11.a$Q27.11.a, employ_datQ27.11.a)
#ordinal(employ_datQ27.11.a$CatOutcome, employ_datQ27.11.a$Academic, employ_datQ27.11.a)
prep <- analysisPrep(employ_datQ27.11.a$CatOutcome, employ_datQ27.11.a$Academic, employ_datQ27.11.a)
analysis <- polr(employ_datQ27.11.a$CatOutcome ~ employ_datQ27.11.a$Academic, data=employ_datQ27.11.a, Hess=TRUE)
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
Question <- "Q27.11.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ27.11.aCollege<-multidatClean(Q27.11.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q27.11.a + Q3, data = employ_datQ27.11.aCollege)
conTable
detach(dat_long)
employ_datQ27.11.aCollege$Q3[(employ_datQ27.11.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ27.11.aCollege$Q3<- factor(employ_datQ27.11.aCollege$Q3)
employ_datQ27.11.aCollege<-ordinaldatClean(employ_datQ27.11.aCollege$Q27.11.a, employ_datQ27.11.aCollege)
#ordinal(employ_datQ27.11.aCollege$CatOutcome, employ_datQ27.11.aCollege$Q3, employ_datQ27.11.aCollege)
prep <- analysisPrep(employ_datQ27.11.aCollege$CatOutcome, employ_datQ27.11.aCollege$Q3, employ_datQ27.11.aCollege)
analysis <- polr(employ_datQ27.11.aCollege$CatOutcome ~ employ_datQ27.11.aCollege$Q3, data=employ_datQ27.11.aCollege, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ27.11.aCarer<-multidatClean(Q27.11.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aCarer$Carer<- factor(employ_datQ27.11.aCarer$Carer)
employ_datQ27.11.aCarer<-ordinaldatClean(employ_datQ27.11.aCarer$Q27.11.a, employ_datQ27.11.aCarer)
#ordinal(employ_datQ27.11.aCarer$CatOutcome, employ_datQ27.11.aCarer$Carer, employ_datQ27.11.aCarer)
prep <- analysisPrep(employ_datQ27.11.aCarer$CatOutcome, employ_datQ27.11.aCarer$Carer, employ_datQ27.11.aCarer)
analysis <- polr(employ_datQ27.11.aCarer$CatOutcome ~ employ_datQ27.11.aCarer$Carer, data=employ_datQ27.11.aCarer, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ27.11.aDisability<-multidatClean(Q27.11.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aDisability$Disability<- factor(employ_datQ27.11.aDisability$Disability)
employ_datQ27.11.aDisability<-ordinaldatClean(employ_datQ27.11.aDisability$Q27.11.a, employ_datQ27.11.aDisability)
conTable <- xtabs(~Q27.11.a + Disability, data = employ_datQ27.11.aDisability)
#ordinal(employ_datQ27.11.aDisability$CatOutcome, employ_datQ27.11.aDisability$Disability, employ_datQ27.11.aDisability)
prep <- analysisPrep(employ_datQ27.11.aDisability$CatOutcome, employ_datQ27.11.aDisability$Disability, employ_datQ27.11.aDisability)
analysis <- polr(employ_datQ27.11.aDisability$CatOutcome ~ employ_datQ27.11.aDisability$Disability, data=employ_datQ27.11.aDisability, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ27.11.aEthnicity<-multidatClean(Q27.11.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aEthnicity$Ethnicity<- factor(employ_datQ27.11.aEthnicity$EthnicityCleaned)
employ_datQ27.11.aEthnicity<-ordinaldatClean(employ_datQ27.11.aEthnicity$Q27.11.a, employ_datQ27.11.aEthnicity)
conTable <- xtabs(~Q27.11.a + EthnicityCleaned, data = employ_datQ27.11.aEthnicity)
conTable
#ordinal(employ_datQ27.11.aEthnicity$CatOutcome, employ_datQ27.11.aEthnicity$EthnicityCleaned, employ_datQ27.11.aEthnicity)
prep <- analysisPrep(employ_datQ27.11.aEthnicity$CatOutcome, employ_datQ27.11.aEthnicity$EthnicityCleaned, employ_datQ27.11.aEthnicity)
analysis <- polr(employ_datQ27.11.aEthnicity$CatOutcome ~ employ_datQ27.11.aEthnicity$EthnicityCleaned, data=employ_datQ27.11.aEthnicity, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ27.11.aFirstGen<-multidatClean(Q27.11.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aFirstGen$FirstGen<-factor(employ_datQ27.11.aFirstGen$FirstGen)
employ_datQ27.11.aFirstGen<-ordinaldatClean(employ_datQ27.11.aFirstGen$Q27.11.a, employ_datQ27.11.aFirstGen)
#ordinal(employ_datQ27.11.aFirstGen$CatOutcome, employ_datQ27.11.aFirstGen$FirstGen, employ_datQ27.11.aFirstGen)
prep <- analysisPrep(employ_datQ27.11.aFirstGen$CatOutcome, employ_datQ27.11.aFirstGen$FirstGen, employ_datQ27.11.aFirstGen)
analysis <- polr(employ_datQ27.11.aFirstGen$CatOutcome ~ employ_datQ27.11.aFirstGen$FirstGen, data=employ_datQ27.11.aFirstGen, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ27.11.aGender<-multidatClean(Q27.11.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aGender$Gender<-factor(employ_datQ27.11.aGender$Gender)
employ_datQ27.11.aGender<-ordinaldatClean(employ_datQ27.11.aGender$Q27.11.a, employ_datQ27.11.aGender)
#ordinal(employ_datQ27.11.aGender$CatOutcome, employ_datQ27.11.aGender$Gender, employ_datQ27.11.aGender)
prep <- analysisPrep(employ_datQ27.11.aGender$CatOutcome, employ_datQ27.11.aGender$Gender, employ_datQ27.11.aGender)
analysis <- polr(employ_datQ27.11.aGender$CatOutcome ~ employ_datQ27.11.aGender$Gender, data=employ_datQ27.11.aGender, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ27.11.aSexuality<-multidatClean(Q27.11.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ27.11.aSexuality$Sexuality<-factor(employ_datQ27.11.aSexuality$Sexuality)
employ_datQ27.11.aSexuality<-ordinaldatClean(employ_datQ27.11.aSexuality$Q27.11.a, employ_datQ27.11.aSexuality)
#ordinal(employ_datQ27.11.aSexuality$CatOutcome, employ_datQ27.11.aSexuality$Sexuality, employ_datQ27.11.aSexuality)
prep <- analysisPrep(employ_datQ27.11.aSexuality$CatOutcome, employ_datQ27.11.aSexuality$Sexuality, employ_datQ27.11.aSexuality)
analysis <- polr(employ_datQ27.11.aSexuality$CatOutcome ~ employ_datQ27.11.aSexuality$Sexuality, data=employ_datQ27.11.aSexuality, Hess=TRUE)
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
Question <-  "Q27.11.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q27.11.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q28 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ28<-multidatClean(Q28, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q28 + Academic + UniqueResponseNumber, data = employ_datQ28)
detach(dat_long)
employ_datQ28<-PerformordinaldatClean(employ_datQ28$Q28, employ_datQ28)
ordinal(employ_datQ28$CatOutcome, employ_datQ28$Academic, employ_datQ28)
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ28College<-multidatClean(Q28, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q28 + Q3, data = employ_datQ28College)
conTable
detach(dat_long)
employ_datQ28College$Q3[(employ_datQ28College$Q3 == "Research Professional Staff")]="Other"
employ_datQ28College$Q3<- factor(employ_datQ28College$Q3)
employ_datQ28College<-PerformordinaldatClean(employ_datQ28College$Q28, employ_datQ28College)
ordinal(employ_datQ28College$CatOutcome, employ_datQ28College$Q3, employ_datQ28College)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ28Carer<-multidatClean(Q28, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Carer$Carer<- factor(employ_datQ28Carer$Carer)
employ_datQ28Carer<-PerformordinaldatClean(employ_datQ28Carer$Q28, employ_datQ28Carer)
ordinal(employ_datQ28Carer$CatOutcome, employ_datQ28Carer$Carer, employ_datQ28Carer)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ28Disability<-multidatClean(Q28, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Disability$Disability<- factor(employ_datQ28Disability$Disability)
employ_datQ28Disability<-PerformordinaldatClean(employ_datQ28Disability$Q28, employ_datQ28Disability)
conTable <- xtabs(~Q28 + Disability, data = employ_datQ28Disability)
conTable
ordinal(employ_datQ28Disability$CatOutcome, employ_datQ28Disability$Disability, employ_datQ28Disability)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ28Ethnicity<-multidatClean(Q28, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Ethnicity$Ethnicity<- factor(employ_datQ28Ethnicity$EthnicityCleaned)
employ_datQ28Ethnicity<-PerformordinaldatClean(employ_datQ28Ethnicity$Q28, employ_datQ28Ethnicity)
conTable <- xtabs(~Q28 + EthnicityCleaned, data = employ_datQ28Ethnicity)
conTable
ordinal(employ_datQ28Ethnicity$CatOutcome, employ_datQ28Ethnicity$EthnicityCleaned, employ_datQ28Ethnicity)
detach(data) 

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ28FirstGen<-multidatClean(Q28, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28FirstGen$FirstGen<-factor(employ_datQ28FirstGen$FirstGen)
employ_datQ28FirstGen<-PerformordinaldatClean(employ_datQ28FirstGen$Q28, employ_datQ28FirstGen)
ordinal(employ_datQ28FirstGen$CatOutcome, employ_datQ28FirstGen$FirstGen, employ_datQ28FirstGen)
detach(data) 

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ28Gender<-multidatClean(Q28, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Gender$Gender<-factor(employ_datQ28Gender$Gender)
employ_datQ28Gender<-PerformordinaldatClean(employ_datQ28Gender$Q28, employ_datQ28Gender)
ordinal(employ_datQ28Gender$CatOutcome, employ_datQ28Gender$Gender, employ_datQ28Gender)
detach(data) 

#### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ28Sexuality<-multidatClean(Q28, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Sexuality$Sexuality<-factor(employ_datQ28Sexuality$Sexuality)
employ_datQ28Sexuality<-PerformordinaldatClean(employ_datQ28Sexuality$Q28, employ_datQ28Sexuality)
ordinal(employ_datQ28Sexuality$CatOutcome, employ_datQ28Sexuality$Sexuality, employ_datQ28Sexuality)
detach(data) 

"Q28 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ28<-multidatClean(Q28, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q28 + Academic + UniqueResponseNumber, data = employ_datQ28)
detach(dat_long)
employ_datQ28<-PerformordinaldatClean(employ_datQ28$Q28, employ_datQ28)
#ordinal(employ_datQ28$CatOutcome, employ_datQ28$Academic, employ_datQ28)
#prep <- analysisPrep(employ_datQ28$CatOutcome, employ_datQ28$Academic, employ_datQ28)
analysis <- polr(employ_datQ28$CatOutcome ~ employ_datQ28$Academic, data=employ_datQ28, Hess=TRUE)
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
Question <- "Q28"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ28College<-multidatClean(Q28, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q28 + Q3, data = employ_datQ28College)
conTable
detach(dat_long)
employ_datQ28College$Q3[(employ_datQ28College$Q3 == "Research Professional Staff")]="Other"
employ_datQ28College$Q3<- factor(employ_datQ28College$Q3)
employ_datQ28College<-PerformordinaldatClean(employ_datQ28College$Q28, employ_datQ28College)
#ordinal(employ_datQ28College$CatOutcome, employ_datQ28College$Q3, employ_datQ28College)
prep <- analysisPrep(employ_datQ28College$CatOutcome, employ_datQ28College$Q3, employ_datQ28College)
analysis <- polr(employ_datQ28College$CatOutcome ~ employ_datQ28College$Q3, data=employ_datQ28College, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ28Carer<-multidatClean(Q28, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Carer$Carer<- factor(employ_datQ28Carer$Carer)
employ_datQ28Carer<-PerformordinaldatClean(employ_datQ28Carer$Q28, employ_datQ28Carer)
#ordinal(employ_datQ28Carer$CatOutcome, employ_datQ28Carer$Carer, employ_datQ28Carer)
prep <- analysisPrep(employ_datQ28Carer$CatOutcome, employ_datQ28Carer$Carer, employ_datQ28Carer)
analysis <- polr(employ_datQ28Carer$CatOutcome ~ employ_datQ28Carer$Carer, data=employ_datQ28Carer, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ28Disability<-multidatClean(Q28, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Disability$Disability<- factor(employ_datQ28Disability$Disability)
employ_datQ28Disability<-PerformordinaldatClean(employ_datQ28Disability$Q28, employ_datQ28Disability)
conTable <- xtabs(~Q28 + Disability, data = employ_datQ28Disability)
#ordinal(employ_datQ28Disability$CatOutcome, employ_datQ28Disability$Disability, employ_datQ28Disability)
prep <- analysisPrep(employ_datQ28Disability$CatOutcome, employ_datQ28Disability$Disability, employ_datQ28Disability)
analysis <- polr(employ_datQ28Disability$CatOutcome ~ employ_datQ28Disability$Disability, data=employ_datQ28Disability, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ28Ethnicity<-multidatClean(Q28, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Ethnicity$Ethnicity<- factor(employ_datQ28Ethnicity$EthnicityCleaned)
employ_datQ28Ethnicity<-PerformordinaldatClean(employ_datQ28Ethnicity$Q28, employ_datQ28Ethnicity)
conTable <- xtabs(~Q28 + EthnicityCleaned, data = employ_datQ28Ethnicity)
conTable
#ordinal(employ_datQ28Ethnicity$CatOutcome, employ_datQ28Ethnicity$EthnicityCleaned, employ_datQ28Ethnicity)
prep <- analysisPrep(employ_datQ28Ethnicity$CatOutcome, employ_datQ28Ethnicity$EthnicityCleaned, employ_datQ28Ethnicity)
analysis <- polr(employ_datQ28Ethnicity$CatOutcome ~ employ_datQ28Ethnicity$EthnicityCleaned, data=employ_datQ28Ethnicity, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ28FirstGen<-multidatClean(Q28, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28FirstGen$FirstGen<-factor(employ_datQ28FirstGen$FirstGen)
employ_datQ28FirstGen<-PerformordinaldatClean(employ_datQ28FirstGen$Q28, employ_datQ28FirstGen)
#ordinal(employ_datQ28FirstGen$CatOutcome, employ_datQ28FirstGen$FirstGen, employ_datQ28FirstGen)
prep <- analysisPrep(employ_datQ28FirstGen$CatOutcome, employ_datQ28FirstGen$FirstGen, employ_datQ28FirstGen)
analysis <- polr(employ_datQ28FirstGen$CatOutcome ~ employ_datQ28FirstGen$FirstGen, data=employ_datQ28FirstGen, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ28Gender<-multidatClean(Q28, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Gender$Gender<-factor(employ_datQ28Gender$Gender)
employ_datQ28Gender<-PerformordinaldatClean(employ_datQ28Gender$Q28, employ_datQ28Gender)
#ordinal(employ_datQ28Gender$CatOutcome, employ_datQ28Gender$Gender, employ_datQ28Gender)
prep <- analysisPrep(employ_datQ28Gender$CatOutcome, employ_datQ28Gender$Gender, employ_datQ28Gender)
analysis <- polr(employ_datQ28Gender$CatOutcome ~ employ_datQ28Gender$Gender, data=employ_datQ28Gender, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ28Sexuality<-multidatClean(Q28, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ28Sexuality$Sexuality<-factor(employ_datQ28Sexuality$Sexuality)
employ_datQ28Sexuality<-PerformordinaldatClean(employ_datQ28Sexuality$Q28, employ_datQ28Sexuality)
#ordinal(employ_datQ28Sexuality$CatOutcome, employ_datQ28Sexuality$Sexuality, employ_datQ28Sexuality)
prep <- analysisPrep(employ_datQ28Sexuality$CatOutcome, employ_datQ28Sexuality$Sexuality, employ_datQ28Sexuality)
analysis <- polr(employ_datQ28Sexuality$CatOutcome ~ employ_datQ28Sexuality$Sexuality, data=employ_datQ28Sexuality, Hess=TRUE)
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
Question <-  "Q28"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q28_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)