source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239, 60, 62:68, 70:82, 6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.1.a - The work I do is fairly and adequately recognised"
"Status"
employ_datQ20.1.a<-multidatClean(Q20.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.1.a + Academic, data = employ_datQ20.1.a)
detach(dat_long)
employ_datQ20.1.a<-ordinaldatClean(employ_datQ20.1.a$Q20.1.a, employ_datQ20.1.a)
#ordinal(employ_datQ20.1.a$CatOutcome, employ_datQ20.1.a$Academic, employ_datQ20.1.a)
prep <- analysisPrep(employ_datQ20.1.a$CatOutcome, employ_datQ20.1.a$Academic, employ_datQ20.1.a)
analysis <- polr(employ_datQ20.1.a$CatOutcome ~ employ_datQ20.1.a$Academic, data=employ_datQ20.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.1.aCollege<-multidatClean(Q20.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.1.a + Q3, data = employ_datQ20.1.aCollege)
conTable
detach(dat_long)
employ_datQ20.1.aCollege$Q3[(employ_datQ20.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.1.aCollege$Q3<- factor(employ_datQ20.1.aCollege$Q3)
employ_datQ20.1.aCollege<-ordinaldatClean(employ_datQ20.1.aCollege$Q20.1.a, employ_datQ20.1.aCollege)
#ordinal(employ_datQ20.1.aCollege$CatOutcome, employ_datQ20.1.aCollege$Q3, employ_datQ20.1.aCollege)
prep <- analysisPrep(employ_datQ20.1.aCollege$CatOutcome, employ_datQ20.1.aCollege$Q3, employ_datQ20.1.aCollege)
analysis <- polr(employ_datQ20.1.aCollege$CatOutcome ~ employ_datQ20.1.aCollege$Q3, data=employ_datQ20.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.1.aCarer<-multidatClean(Q20.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aCarer$Carer<- factor(employ_datQ20.1.aCarer$Carer)
employ_datQ20.1.aCarer<-ordinaldatClean(employ_datQ20.1.aCarer$Q20.1.a, employ_datQ20.1.aCarer)
#ordinal(employ_datQ20.1.aCarer$CatOutcome, employ_datQ20.1.aCarer$Carer, employ_datQ20.1.aCarer)
prep <- analysisPrep(employ_datQ20.1.aCarer$CatOutcome, employ_datQ20.1.aCarer$Carer, employ_datQ20.1.aCarer)
analysis <- polr(employ_datQ20.1.aCarer$CatOutcome ~ employ_datQ20.1.aCarer$Carer, data=employ_datQ20.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.1.aDisability<-multidatClean(Q20.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aDisability$Disability<- factor(employ_datQ20.1.aDisability$Disability)
employ_datQ20.1.aDisability<-ordinaldatClean(employ_datQ20.1.aDisability$Q20.1.a, employ_datQ20.1.aDisability)
conTable <- xtabs(~Q20.1.a + Disability, data = employ_datQ20.1.aDisability)
#ordinal(employ_datQ20.1.aDisability$CatOutcome, employ_datQ20.1.aDisability$Disability, employ_datQ20.1.aDisability)
prep <- analysisPrep(employ_datQ20.1.aDisability$CatOutcome, employ_datQ20.1.aDisability$Disability, employ_datQ20.1.aDisability)
analysis <- polr(employ_datQ20.1.aDisability$CatOutcome ~ employ_datQ20.1.aDisability$Disability, data=employ_datQ20.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.1.aEthnicity<-multidatClean(Q20.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aEthnicity$Ethnicity<- factor(employ_datQ20.1.aEthnicity$EthnicityCleaned)
employ_datQ20.1.aEthnicity<-ordinaldatClean(employ_datQ20.1.aEthnicity$Q20.1.a, employ_datQ20.1.aEthnicity)
conTable <- xtabs(~Q20.1.a + EthnicityCleaned, data = employ_datQ20.1.aEthnicity)
conTable
#ordinal(employ_datQ20.1.aEthnicity$CatOutcome, employ_datQ20.1.aEthnicity$EthnicityCleaned, employ_datQ20.1.aEthnicity)
prep <- analysisPrep(employ_datQ20.1.aEthnicity$CatOutcome, employ_datQ20.1.aEthnicity$EthnicityCleaned, employ_datQ20.1.aEthnicity)
analysis <- polr(employ_datQ20.1.aEthnicity$CatOutcome ~ employ_datQ20.1.aEthnicity$EthnicityCleaned, data=employ_datQ20.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.1.aFirstGen<-multidatClean(Q20.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aFirstGen$FirstGen<-factor(employ_datQ20.1.aFirstGen$FirstGen)
employ_datQ20.1.aFirstGen<-ordinaldatClean(employ_datQ20.1.aFirstGen$Q20.1.a, employ_datQ20.1.aFirstGen)
#ordinal(employ_datQ20.1.aFirstGen$CatOutcome, employ_datQ20.1.aFirstGen$FirstGen, employ_datQ20.1.aFirstGen)
prep <- analysisPrep(employ_datQ20.1.aFirstGen$CatOutcome, employ_datQ20.1.aFirstGen$FirstGen, employ_datQ20.1.aFirstGen)
analysis <- polr(employ_datQ20.1.aFirstGen$CatOutcome ~ employ_datQ20.1.aFirstGen$FirstGen, data=employ_datQ20.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.1.aGender<-multidatClean(Q20.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aGender$Gender<-factor(employ_datQ20.1.aGender$Gender)
employ_datQ20.1.aGender<-ordinaldatClean(employ_datQ20.1.aGender$Q20.1.a, employ_datQ20.1.aGender)
#ordinal(employ_datQ20.1.aGender$CatOutcome, employ_datQ20.1.aGender$Gender, employ_datQ20.1.aGender)
prep <- analysisPrep(employ_datQ20.1.aGender$CatOutcome, employ_datQ20.1.aGender$Gender, employ_datQ20.1.aGender)
analysis <- polr(employ_datQ20.1.aGender$CatOutcome ~ employ_datQ20.1.aGender$Gender, data=employ_datQ20.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.1.aSexuality<-multidatClean(Q20.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.1.aSexuality$Sexuality<-factor(employ_datQ20.1.aSexuality$Sexuality)
employ_datQ20.1.aSexuality<-ordinaldatClean(employ_datQ20.1.aSexuality$Q20.1.a, employ_datQ20.1.aSexuality)
#ordinal(employ_datQ20.1.aSexuality$CatOutcome, employ_datQ20.1.aSexuality$Sexuality, employ_datQ20.1.aSexuality)
prep <- analysisPrep(employ_datQ20.1.aSexuality$CatOutcome, employ_datQ20.1.aSexuality$Sexuality, employ_datQ20.1.aSexuality)
analysis <- polr(employ_datQ20.1.aSexuality$CatOutcome ~ employ_datQ20.1.aSexuality$Sexuality, data=employ_datQ20.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.2. I have experienced issues with others taking credit for my work"
"Status"
employ_datQ20.2.a<-multidatClean(Q20.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.2.a + Academic, data = employ_datQ20.2.a)
detach(dat_long)
employ_datQ20.2.a<-ordinaldatCleanNegative(employ_datQ20.2.a$Q20.2.a, employ_datQ20.2.a)
#ordinal(employ_datQ20.2.a$CatOutcome, employ_datQ20.2.a$Academic, employ_datQ20.2.a)
prep <- analysisPrep(employ_datQ20.2.a$CatOutcome, employ_datQ20.2.a$Academic, employ_datQ20.2.a)
analysis <- polr(employ_datQ20.2.a$CatOutcome ~ employ_datQ20.2.a$Academic, data=employ_datQ20.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.2.aCollege<-multidatClean(Q20.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.2.a + Q3, data = employ_datQ20.2.aCollege)
conTable
detach(dat_long)
employ_datQ20.2.aCollege$Q3[(employ_datQ20.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.2.aCollege$Q3<- factor(employ_datQ20.2.aCollege$Q3)
employ_datQ20.2.aCollege<-ordinaldatCleanNegative(employ_datQ20.2.aCollege$Q20.2.a, employ_datQ20.2.aCollege)
#ordinal(employ_datQ20.2.aCollege$CatOutcome, employ_datQ20.2.aCollege$Q3, employ_datQ20.2.aCollege)
prep <- analysisPrep(employ_datQ20.2.aCollege$CatOutcome, employ_datQ20.2.aCollege$Q3, employ_datQ20.2.aCollege)
analysis <- polr(employ_datQ20.2.aCollege$CatOutcome ~ employ_datQ20.2.aCollege$Q3, data=employ_datQ20.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.2.aCarer<-multidatClean(Q20.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aCarer$Carer<- factor(employ_datQ20.2.aCarer$Carer)
employ_datQ20.2.aCarer<-ordinaldatCleanNegative(employ_datQ20.2.aCarer$Q20.2.a, employ_datQ20.2.aCarer)
#ordinal(employ_datQ20.2.aCarer$CatOutcome, employ_datQ20.2.aCarer$Carer, employ_datQ20.2.aCarer)
prep <- analysisPrep(employ_datQ20.2.aCarer$CatOutcome, employ_datQ20.2.aCarer$Carer, employ_datQ20.2.aCarer)
analysis <- polr(employ_datQ20.2.aCarer$CatOutcome ~ employ_datQ20.2.aCarer$Carer, data=employ_datQ20.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.2.aDisability<-multidatClean(Q20.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aDisability$Disability<- factor(employ_datQ20.2.aDisability$Disability)
employ_datQ20.2.aDisability<-ordinaldatCleanNegative(employ_datQ20.2.aDisability$Q20.2.a, employ_datQ20.2.aDisability)
conTable <- xtabs(~Q20.2.a + Disability, data = employ_datQ20.2.aDisability)
#ordinal(employ_datQ20.2.aDisability$CatOutcome, employ_datQ20.2.aDisability$Disability, employ_datQ20.2.aDisability)
prep <- analysisPrep(employ_datQ20.2.aDisability$CatOutcome, employ_datQ20.2.aDisability$Disability, employ_datQ20.2.aDisability)
analysis <- polr(employ_datQ20.2.aDisability$CatOutcome ~ employ_datQ20.2.aDisability$Disability, data=employ_datQ20.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.2.aEthnicity<-multidatClean(Q20.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aEthnicity$Ethnicity<- factor(employ_datQ20.2.aEthnicity$EthnicityCleaned)
employ_datQ20.2.aEthnicity<-ordinaldatCleanNegative(employ_datQ20.2.aEthnicity$Q20.2.a, employ_datQ20.2.aEthnicity)
conTable <- xtabs(~Q20.2.a + EthnicityCleaned, data = employ_datQ20.2.aEthnicity)
conTable
#ordinal(employ_datQ20.2.aEthnicity$CatOutcome, employ_datQ20.2.aEthnicity$EthnicityCleaned, employ_datQ20.2.aEthnicity)
prep <- analysisPrep(employ_datQ20.2.aEthnicity$CatOutcome, employ_datQ20.2.aEthnicity$EthnicityCleaned, employ_datQ20.2.aEthnicity)
analysis <- polr(employ_datQ20.2.aEthnicity$CatOutcome ~ employ_datQ20.2.aEthnicity$EthnicityCleaned, data=employ_datQ20.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.2.aFirstGen<-multidatClean(Q20.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aFirstGen$FirstGen<-factor(employ_datQ20.2.aFirstGen$FirstGen)
employ_datQ20.2.aFirstGen<-ordinaldatCleanNegative(employ_datQ20.2.aFirstGen$Q20.2.a, employ_datQ20.2.aFirstGen)
#ordinal(employ_datQ20.2.aFirstGen$CatOutcome, employ_datQ20.2.aFirstGen$FirstGen, employ_datQ20.2.aFirstGen)
prep <- analysisPrep(employ_datQ20.2.aFirstGen$CatOutcome, employ_datQ20.2.aFirstGen$FirstGen, employ_datQ20.2.aFirstGen)
analysis <- polr(employ_datQ20.2.aFirstGen$CatOutcome ~ employ_datQ20.2.aFirstGen$FirstGen, data=employ_datQ20.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.2.aGender<-multidatClean(Q20.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aGender$Gender<-factor(employ_datQ20.2.aGender$Gender)
employ_datQ20.2.aGender<-ordinaldatCleanNegative(employ_datQ20.2.aGender$Q20.2.a, employ_datQ20.2.aGender)
#ordinal(employ_datQ20.2.aGender$CatOutcome, employ_datQ20.2.aGender$Gender, employ_datQ20.2.aGender)
prep <- analysisPrep(employ_datQ20.2.aGender$CatOutcome, employ_datQ20.2.aGender$Gender, employ_datQ20.2.aGender)
analysis <- polr(employ_datQ20.2.aGender$CatOutcome ~ employ_datQ20.2.aGender$Gender, data=employ_datQ20.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.2.aSexuality<-multidatClean(Q20.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.2.aSexuality$Sexuality<-factor(employ_datQ20.2.aSexuality$Sexuality)
employ_datQ20.2.aSexuality<-ordinaldatCleanNegative(employ_datQ20.2.aSexuality$Q20.2.a, employ_datQ20.2.aSexuality)
#ordinal(employ_datQ20.2.aSexuality$CatOutcome, employ_datQ20.2.aSexuality$Sexuality, employ_datQ20.2.aSexuality)
prep <- analysisPrep(employ_datQ20.2.aSexuality$CatOutcome, employ_datQ20.2.aSexuality$Sexuality, employ_datQ20.2.aSexuality)
analysis <- polr(employ_datQ20.2.aSexuality$CatOutcome ~ employ_datQ20.2.aSexuality$Sexuality, data=employ_datQ20.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.3. I would feel comfortable reporting instances of compromised research standards and misconduct without fear of personal consequences"
"Status"
employ_datQ20.3.a<-multidatClean(Q20.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.3.a + Academic, data = employ_datQ20.3.a)
detach(dat_long)
employ_datQ20.3.a<-ordinaldatClean(employ_datQ20.3.a$Q20.3.a, employ_datQ20.3.a)
#ordinal(employ_datQ20.3.a$CatOutcome, employ_datQ20.3.a$Academic, employ_datQ20.3.a)
prep <- analysisPrep(employ_datQ20.3.a$CatOutcome, employ_datQ20.3.a$Academic, employ_datQ20.3.a)
analysis <- polr(employ_datQ20.3.a$CatOutcome ~ employ_datQ20.3.a$Academic, data=employ_datQ20.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.3.aCollege<-multidatClean(Q20.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.3.a + Q3, data = employ_datQ20.3.aCollege)
conTable
detach(dat_long)
employ_datQ20.3.aCollege$Q3[(employ_datQ20.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.3.aCollege$Q3<- factor(employ_datQ20.3.aCollege$Q3)
employ_datQ20.3.aCollege<-ordinaldatClean(employ_datQ20.3.aCollege$Q20.3.a, employ_datQ20.3.aCollege)
#ordinal(employ_datQ20.3.aCollege$CatOutcome, employ_datQ20.3.aCollege$Q3, employ_datQ20.3.aCollege)
prep <- analysisPrep(employ_datQ20.3.aCollege$CatOutcome, employ_datQ20.3.aCollege$Q3, employ_datQ20.3.aCollege)
analysis <- polr(employ_datQ20.3.aCollege$CatOutcome ~ employ_datQ20.3.aCollege$Q3, data=employ_datQ20.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.3.aCarer<-multidatClean(Q20.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aCarer$Carer<- factor(employ_datQ20.3.aCarer$Carer)
employ_datQ20.3.aCarer<-ordinaldatClean(employ_datQ20.3.aCarer$Q20.3.a, employ_datQ20.3.aCarer)
#ordinal(employ_datQ20.3.aCarer$CatOutcome, employ_datQ20.3.aCarer$Carer, employ_datQ20.3.aCarer)
prep <- analysisPrep(employ_datQ20.3.aCarer$CatOutcome, employ_datQ20.3.aCarer$Carer, employ_datQ20.3.aCarer)
analysis <- polr(employ_datQ20.3.aCarer$CatOutcome ~ employ_datQ20.3.aCarer$Carer, data=employ_datQ20.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.3.aDisability<-multidatClean(Q20.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aDisability$Disability<- factor(employ_datQ20.3.aDisability$Disability)
employ_datQ20.3.aDisability<-ordinaldatClean(employ_datQ20.3.aDisability$Q20.3.a, employ_datQ20.3.aDisability)
conTable <- xtabs(~Q20.3.a + Disability, data = employ_datQ20.3.aDisability)
#ordinal(employ_datQ20.3.aDisability$CatOutcome, employ_datQ20.3.aDisability$Disability, employ_datQ20.3.aDisability)
prep <- analysisPrep(employ_datQ20.3.aDisability$CatOutcome, employ_datQ20.3.aDisability$Disability, employ_datQ20.3.aDisability)
analysis <- polr(employ_datQ20.3.aDisability$CatOutcome ~ employ_datQ20.3.aDisability$Disability, data=employ_datQ20.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.3.aEthnicity<-multidatClean(Q20.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aEthnicity$Ethnicity<- factor(employ_datQ20.3.aEthnicity$EthnicityCleaned)
employ_datQ20.3.aEthnicity<-ordinaldatClean(employ_datQ20.3.aEthnicity$Q20.3.a, employ_datQ20.3.aEthnicity)
conTable <- xtabs(~Q20.3.a + EthnicityCleaned, data = employ_datQ20.3.aEthnicity)
conTable
#ordinal(employ_datQ20.3.aEthnicity$CatOutcome, employ_datQ20.3.aEthnicity$EthnicityCleaned, employ_datQ20.3.aEthnicity)
prep <- analysisPrep(employ_datQ20.3.aEthnicity$CatOutcome, employ_datQ20.3.aEthnicity$EthnicityCleaned, employ_datQ20.3.aEthnicity)
analysis <- polr(employ_datQ20.3.aEthnicity$CatOutcome ~ employ_datQ20.3.aEthnicity$EthnicityCleaned, data=employ_datQ20.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.3.aFirstGen<-multidatClean(Q20.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aFirstGen$FirstGen<-factor(employ_datQ20.3.aFirstGen$FirstGen)
employ_datQ20.3.aFirstGen<-ordinaldatClean(employ_datQ20.3.aFirstGen$Q20.3.a, employ_datQ20.3.aFirstGen)
#ordinal(employ_datQ20.3.aFirstGen$CatOutcome, employ_datQ20.3.aFirstGen$FirstGen, employ_datQ20.3.aFirstGen)
prep <- analysisPrep(employ_datQ20.3.aFirstGen$CatOutcome, employ_datQ20.3.aFirstGen$FirstGen, employ_datQ20.3.aFirstGen)
analysis <- polr(employ_datQ20.3.aFirstGen$CatOutcome ~ employ_datQ20.3.aFirstGen$FirstGen, data=employ_datQ20.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.3.aGender<-multidatClean(Q20.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aGender$Gender<-factor(employ_datQ20.3.aGender$Gender)
employ_datQ20.3.aGender<-ordinaldatClean(employ_datQ20.3.aGender$Q20.3.a, employ_datQ20.3.aGender)
#ordinal(employ_datQ20.3.aGender$CatOutcome, employ_datQ20.3.aGender$Gender, employ_datQ20.3.aGender)
prep <- analysisPrep(employ_datQ20.3.aGender$CatOutcome, employ_datQ20.3.aGender$Gender, employ_datQ20.3.aGender)
analysis <- polr(employ_datQ20.3.aGender$CatOutcome ~ employ_datQ20.3.aGender$Gender, data=employ_datQ20.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.3.aSexuality<-multidatClean(Q20.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.3.aSexuality$Sexuality<-factor(employ_datQ20.3.aSexuality$Sexuality)
employ_datQ20.3.aSexuality<-ordinaldatClean(employ_datQ20.3.aSexuality$Q20.3.a, employ_datQ20.3.aSexuality)
#ordinal(employ_datQ20.3.aSexuality$CatOutcome, employ_datQ20.3.aSexuality$Sexuality, employ_datQ20.3.aSexuality)
prep <- analysisPrep(employ_datQ20.3.aSexuality$CatOutcome, employ_datQ20.3.aSexuality$Sexuality, employ_datQ20.3.aSexuality)
analysis <- polr(employ_datQ20.3.aSexuality$CatOutcome ~ employ_datQ20.3.aSexuality$Sexuality, data=employ_datQ20.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.4. I know how to report instances of research misconducts"
"Status"
employ_datQ20.4.a<-multidatClean(Q20.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.4.a + Academic, data = employ_datQ20.4.a)
detach(dat_long)
employ_datQ20.4.a<-ordinaldatClean(employ_datQ20.4.a$Q20.4.a, employ_datQ20.4.a)
#ordinal(employ_datQ20.4.a$CatOutcome, employ_datQ20.4.a$Academic, employ_datQ20.4.a)
prep <- analysisPrep(employ_datQ20.4.a$CatOutcome, employ_datQ20.4.a$Academic, employ_datQ20.4.a)
analysis <- polr(employ_datQ20.4.a$CatOutcome ~ employ_datQ20.4.a$Academic, data=employ_datQ20.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.4.aCollege<-multidatClean(Q20.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.4.a + Q3, data = employ_datQ20.4.aCollege)
conTable
detach(dat_long)
employ_datQ20.4.aCollege$Q3[(employ_datQ20.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.4.aCollege$Q3<- factor(employ_datQ20.4.aCollege$Q3)
employ_datQ20.4.aCollege<-ordinaldatClean(employ_datQ20.4.aCollege$Q20.4.a, employ_datQ20.4.aCollege)
#ordinal(employ_datQ20.4.aCollege$CatOutcome, employ_datQ20.4.aCollege$Q3, employ_datQ20.4.aCollege)
prep <- analysisPrep(employ_datQ20.4.aCollege$CatOutcome, employ_datQ20.4.aCollege$Q3, employ_datQ20.4.aCollege)
analysis <- polr(employ_datQ20.4.aCollege$CatOutcome ~ employ_datQ20.4.aCollege$Q3, data=employ_datQ20.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.4.aCarer<-multidatClean(Q20.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aCarer$Carer<- factor(employ_datQ20.4.aCarer$Carer)
employ_datQ20.4.aCarer<-ordinaldatClean(employ_datQ20.4.aCarer$Q20.4.a, employ_datQ20.4.aCarer)
#ordinal(employ_datQ20.4.aCarer$CatOutcome, employ_datQ20.4.aCarer$Carer, employ_datQ20.4.aCarer)
prep <- analysisPrep(employ_datQ20.4.aCarer$CatOutcome, employ_datQ20.4.aCarer$Carer, employ_datQ20.4.aCarer)
analysis <- polr(employ_datQ20.4.aCarer$CatOutcome ~ employ_datQ20.4.aCarer$Carer, data=employ_datQ20.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.4.aDisability<-multidatClean(Q20.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aDisability$Disability<- factor(employ_datQ20.4.aDisability$Disability)
employ_datQ20.4.aDisability<-ordinaldatClean(employ_datQ20.4.aDisability$Q20.4.a, employ_datQ20.4.aDisability)
conTable <- xtabs(~Q20.4.a + Disability, data = employ_datQ20.4.aDisability)
#ordinal(employ_datQ20.4.aDisability$CatOutcome, employ_datQ20.4.aDisability$Disability, employ_datQ20.4.aDisability)
prep <- analysisPrep(employ_datQ20.4.aDisability$CatOutcome, employ_datQ20.4.aDisability$Disability, employ_datQ20.4.aDisability)
analysis <- polr(employ_datQ20.4.aDisability$CatOutcome ~ employ_datQ20.4.aDisability$Disability, data=employ_datQ20.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.4.aEthnicity<-multidatClean(Q20.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aEthnicity$Ethnicity<- factor(employ_datQ20.4.aEthnicity$EthnicityCleaned)
employ_datQ20.4.aEthnicity<-ordinaldatClean(employ_datQ20.4.aEthnicity$Q20.4.a, employ_datQ20.4.aEthnicity)
conTable <- xtabs(~Q20.4.a + EthnicityCleaned, data = employ_datQ20.4.aEthnicity)
conTable
#ordinal(employ_datQ20.4.aEthnicity$CatOutcome, employ_datQ20.4.aEthnicity$EthnicityCleaned, employ_datQ20.4.aEthnicity)
prep <- analysisPrep(employ_datQ20.4.aEthnicity$CatOutcome, employ_datQ20.4.aEthnicity$EthnicityCleaned, employ_datQ20.4.aEthnicity)
analysis <- polr(employ_datQ20.4.aEthnicity$CatOutcome ~ employ_datQ20.4.aEthnicity$EthnicityCleaned, data=employ_datQ20.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.4.aFirstGen<-multidatClean(Q20.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aFirstGen$FirstGen<-factor(employ_datQ20.4.aFirstGen$FirstGen)
employ_datQ20.4.aFirstGen<-ordinaldatClean(employ_datQ20.4.aFirstGen$Q20.4.a, employ_datQ20.4.aFirstGen)
#ordinal(employ_datQ20.4.aFirstGen$CatOutcome, employ_datQ20.4.aFirstGen$FirstGen, employ_datQ20.4.aFirstGen)
prep <- analysisPrep(employ_datQ20.4.aFirstGen$CatOutcome, employ_datQ20.4.aFirstGen$FirstGen, employ_datQ20.4.aFirstGen)
analysis <- polr(employ_datQ20.4.aFirstGen$CatOutcome ~ employ_datQ20.4.aFirstGen$FirstGen, data=employ_datQ20.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.4.aGender<-multidatClean(Q20.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aGender$Gender<-factor(employ_datQ20.4.aGender$Gender)
employ_datQ20.4.aGender<-ordinaldatClean(employ_datQ20.4.aGender$Q20.4.a, employ_datQ20.4.aGender)
#ordinal(employ_datQ20.4.aGender$CatOutcome, employ_datQ20.4.aGender$Gender, employ_datQ20.4.aGender)
prep <- analysisPrep(employ_datQ20.4.aGender$CatOutcome, employ_datQ20.4.aGender$Gender, employ_datQ20.4.aGender)
analysis <- polr(employ_datQ20.4.aGender$CatOutcome ~ employ_datQ20.4.aGender$Gender, data=employ_datQ20.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.4.aSexuality<-multidatClean(Q20.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.4.aSexuality$Sexuality<-factor(employ_datQ20.4.aSexuality$Sexuality)
employ_datQ20.4.aSexuality<-ordinaldatClean(employ_datQ20.4.aSexuality$Q20.4.a, employ_datQ20.4.aSexuality)
#ordinal(employ_datQ20.4.aSexuality$CatOutcome, employ_datQ20.4.aSexuality$Sexuality, employ_datQ20.4.aSexuality)
prep <- analysisPrep(employ_datQ20.4.aSexuality$CatOutcome, employ_datQ20.4.aSexuality$Sexuality, employ_datQ20.4.aSexuality)
analysis <- polr(employ_datQ20.4.aSexuality$CatOutcome ~ employ_datQ20.4.aSexuality$Sexuality, data=employ_datQ20.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.5. I have a clear understanding of what the University considers compromised research to be"
"Status"
employ_datQ20.5.a<-multidatClean(Q20.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.5.a + Academic, data = employ_datQ20.5.a)
detach(dat_long)
employ_datQ20.5.a<-ordinaldatClean(employ_datQ20.5.a$Q20.5.a, employ_datQ20.5.a)
#ordinal(employ_datQ20.5.a$CatOutcome, employ_datQ20.5.a$Academic, employ_datQ20.5.a)
prep <- analysisPrep(employ_datQ20.5.a$CatOutcome, employ_datQ20.5.a$Academic, employ_datQ20.5.a)
analysis <- polr(employ_datQ20.5.a$CatOutcome ~ employ_datQ20.5.a$Academic, data=employ_datQ20.5.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.5.aCollege<-multidatClean(Q20.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.5.a + Q3, data = employ_datQ20.5.aCollege)
conTable
detach(dat_long)
employ_datQ20.5.aCollege$Q3[(employ_datQ20.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.5.aCollege$Q3<- factor(employ_datQ20.5.aCollege$Q3)
employ_datQ20.5.aCollege<-ordinaldatClean(employ_datQ20.5.aCollege$Q20.5.a, employ_datQ20.5.aCollege)
#ordinal(employ_datQ20.5.aCollege$CatOutcome, employ_datQ20.5.aCollege$Q3, employ_datQ20.5.aCollege)
prep <- analysisPrep(employ_datQ20.5.aCollege$CatOutcome, employ_datQ20.5.aCollege$Q3, employ_datQ20.5.aCollege)
analysis <- polr(employ_datQ20.5.aCollege$CatOutcome ~ employ_datQ20.5.aCollege$Q3, data=employ_datQ20.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.5.aCarer<-multidatClean(Q20.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aCarer$Carer<- factor(employ_datQ20.5.aCarer$Carer)
employ_datQ20.5.aCarer<-ordinaldatClean(employ_datQ20.5.aCarer$Q20.5.a, employ_datQ20.5.aCarer)
#ordinal(employ_datQ20.5.aCarer$CatOutcome, employ_datQ20.5.aCarer$Carer, employ_datQ20.5.aCarer)
prep <- analysisPrep(employ_datQ20.5.aCarer$CatOutcome, employ_datQ20.5.aCarer$Carer, employ_datQ20.5.aCarer)
analysis <- polr(employ_datQ20.5.aCarer$CatOutcome ~ employ_datQ20.5.aCarer$Carer, data=employ_datQ20.5.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.5.aDisability<-multidatClean(Q20.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aDisability$Disability<- factor(employ_datQ20.5.aDisability$Disability)
employ_datQ20.5.aDisability<-ordinaldatClean(employ_datQ20.5.aDisability$Q20.5.a, employ_datQ20.5.aDisability)
conTable <- xtabs(~Q20.5.a + Disability, data = employ_datQ20.5.aDisability)
#ordinal(employ_datQ20.5.aDisability$CatOutcome, employ_datQ20.5.aDisability$Disability, employ_datQ20.5.aDisability)
prep <- analysisPrep(employ_datQ20.5.aDisability$CatOutcome, employ_datQ20.5.aDisability$Disability, employ_datQ20.5.aDisability)
analysis <- polr(employ_datQ20.5.aDisability$CatOutcome ~ employ_datQ20.5.aDisability$Disability, data=employ_datQ20.5.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.5.aEthnicity<-multidatClean(Q20.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aEthnicity$Ethnicity<- factor(employ_datQ20.5.aEthnicity$EthnicityCleaned)
employ_datQ20.5.aEthnicity<-ordinaldatClean(employ_datQ20.5.aEthnicity$Q20.5.a, employ_datQ20.5.aEthnicity)
conTable <- xtabs(~Q20.5.a + EthnicityCleaned, data = employ_datQ20.5.aEthnicity)
conTable
#ordinal(employ_datQ20.5.aEthnicity$CatOutcome, employ_datQ20.5.aEthnicity$EthnicityCleaned, employ_datQ20.5.aEthnicity)
prep <- analysisPrep(employ_datQ20.5.aEthnicity$CatOutcome, employ_datQ20.5.aEthnicity$EthnicityCleaned, employ_datQ20.5.aEthnicity)
analysis <- polr(employ_datQ20.5.aEthnicity$CatOutcome ~ employ_datQ20.5.aEthnicity$EthnicityCleaned, data=employ_datQ20.5.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.5.aFirstGen<-multidatClean(Q20.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aFirstGen$FirstGen<-factor(employ_datQ20.5.aFirstGen$FirstGen)
employ_datQ20.5.aFirstGen<-ordinaldatClean(employ_datQ20.5.aFirstGen$Q20.5.a, employ_datQ20.5.aFirstGen)
#ordinal(employ_datQ20.5.aFirstGen$CatOutcome, employ_datQ20.5.aFirstGen$FirstGen, employ_datQ20.5.aFirstGen)
prep <- analysisPrep(employ_datQ20.5.aFirstGen$CatOutcome, employ_datQ20.5.aFirstGen$FirstGen, employ_datQ20.5.aFirstGen)
analysis <- polr(employ_datQ20.5.aFirstGen$CatOutcome ~ employ_datQ20.5.aFirstGen$FirstGen, data=employ_datQ20.5.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.5.aGender<-multidatClean(Q20.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aGender$Gender<-factor(employ_datQ20.5.aGender$Gender)
employ_datQ20.5.aGender<-ordinaldatClean(employ_datQ20.5.aGender$Q20.5.a, employ_datQ20.5.aGender)
#ordinal(employ_datQ20.5.aGender$CatOutcome, employ_datQ20.5.aGender$Gender, employ_datQ20.5.aGender)
prep <- analysisPrep(employ_datQ20.5.aGender$CatOutcome, employ_datQ20.5.aGender$Gender, employ_datQ20.5.aGender)
analysis <- polr(employ_datQ20.5.aGender$CatOutcome ~ employ_datQ20.5.aGender$Gender, data=employ_datQ20.5.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.5.aSexuality<-multidatClean(Q20.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.5.aSexuality$Sexuality<-factor(employ_datQ20.5.aSexuality$Sexuality)
employ_datQ20.5.aSexuality<-ordinaldatClean(employ_datQ20.5.aSexuality$Q20.5.a, employ_datQ20.5.aSexuality)
#ordinal(employ_datQ20.5.aSexuality$CatOutcome, employ_datQ20.5.aSexuality$Sexuality, employ_datQ20.5.aSexuality)
prep <- analysisPrep(employ_datQ20.5.aSexuality$CatOutcome, employ_datQ20.5.aSexuality$Sexuality, employ_datQ20.5.aSexuality)
analysis <- polr(employ_datQ20.5.aSexuality$CatOutcome ~ employ_datQ20.5.aSexuality$Sexuality, data=employ_datQ20.5.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.6. I am able to effectively balance the competing roles required as part of my employment"
"Status"
employ_datQ20.6.a<-multidatClean(Q20.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.6.a + Academic, data = employ_datQ20.6.a)
detach(dat_long)
employ_datQ20.6.a<-ordinaldatClean(employ_datQ20.6.a$Q20.6.a, employ_datQ20.6.a)
#ordinal(employ_datQ20.6.a$CatOutcome, employ_datQ20.6.a$Academic, employ_datQ20.6.a)
prep <- analysisPrep(employ_datQ20.6.a$CatOutcome, employ_datQ20.6.a$Academic, employ_datQ20.6.a)
analysis <- polr(employ_datQ20.6.a$CatOutcome ~ employ_datQ20.6.a$Academic, data=employ_datQ20.6.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.6.aCollege<-multidatClean(Q20.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.6.a + Q3, data = employ_datQ20.6.aCollege)
conTable
detach(dat_long)
employ_datQ20.6.aCollege$Q3[(employ_datQ20.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.6.aCollege$Q3<- factor(employ_datQ20.6.aCollege$Q3)
employ_datQ20.6.aCollege<-ordinaldatClean(employ_datQ20.6.aCollege$Q20.6.a, employ_datQ20.6.aCollege)
#ordinal(employ_datQ20.6.aCollege$CatOutcome, employ_datQ20.6.aCollege$Q3, employ_datQ20.6.aCollege)
prep <- analysisPrep(employ_datQ20.6.aCollege$CatOutcome, employ_datQ20.6.aCollege$Q3, employ_datQ20.6.aCollege)
analysis <- polr(employ_datQ20.6.aCollege$CatOutcome ~ employ_datQ20.6.aCollege$Q3, data=employ_datQ20.6.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.6.aCarer<-multidatClean(Q20.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aCarer$Carer<- factor(employ_datQ20.6.aCarer$Carer)
employ_datQ20.6.aCarer<-ordinaldatClean(employ_datQ20.6.aCarer$Q20.6.a, employ_datQ20.6.aCarer)
#ordinal(employ_datQ20.6.aCarer$CatOutcome, employ_datQ20.6.aCarer$Carer, employ_datQ20.6.aCarer)
prep <- analysisPrep(employ_datQ20.6.aCarer$CatOutcome, employ_datQ20.6.aCarer$Carer, employ_datQ20.6.aCarer)
analysis <- polr(employ_datQ20.6.aCarer$CatOutcome ~ employ_datQ20.6.aCarer$Carer, data=employ_datQ20.6.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.6.aDisability<-multidatClean(Q20.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aDisability$Disability<- factor(employ_datQ20.6.aDisability$Disability)
employ_datQ20.6.aDisability<-ordinaldatClean(employ_datQ20.6.aDisability$Q20.6.a, employ_datQ20.6.aDisability)
conTable <- xtabs(~Q20.6.a + Disability, data = employ_datQ20.6.aDisability)
#ordinal(employ_datQ20.6.aDisability$CatOutcome, employ_datQ20.6.aDisability$Disability, employ_datQ20.6.aDisability)
prep <- analysisPrep(employ_datQ20.6.aDisability$CatOutcome, employ_datQ20.6.aDisability$Disability, employ_datQ20.6.aDisability)
analysis <- polr(employ_datQ20.6.aDisability$CatOutcome ~ employ_datQ20.6.aDisability$Disability, data=employ_datQ20.6.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.6.aEthnicity<-multidatClean(Q20.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aEthnicity$Ethnicity<- factor(employ_datQ20.6.aEthnicity$EthnicityCleaned)
employ_datQ20.6.aEthnicity<-ordinaldatClean(employ_datQ20.6.aEthnicity$Q20.6.a, employ_datQ20.6.aEthnicity)
conTable <- xtabs(~Q20.6.a + EthnicityCleaned, data = employ_datQ20.6.aEthnicity)
conTable
#ordinal(employ_datQ20.6.aEthnicity$CatOutcome, employ_datQ20.6.aEthnicity$EthnicityCleaned, employ_datQ20.6.aEthnicity)
prep <- analysisPrep(employ_datQ20.6.aEthnicity$CatOutcome, employ_datQ20.6.aEthnicity$EthnicityCleaned, employ_datQ20.6.aEthnicity)
analysis <- polr(employ_datQ20.6.aEthnicity$CatOutcome ~ employ_datQ20.6.aEthnicity$EthnicityCleaned, data=employ_datQ20.6.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.6.aFirstGen<-multidatClean(Q20.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aFirstGen$FirstGen<-factor(employ_datQ20.6.aFirstGen$FirstGen)
employ_datQ20.6.aFirstGen<-ordinaldatClean(employ_datQ20.6.aFirstGen$Q20.6.a, employ_datQ20.6.aFirstGen)
#ordinal(employ_datQ20.6.aFirstGen$CatOutcome, employ_datQ20.6.aFirstGen$FirstGen, employ_datQ20.6.aFirstGen)
prep <- analysisPrep(employ_datQ20.6.aFirstGen$CatOutcome, employ_datQ20.6.aFirstGen$FirstGen, employ_datQ20.6.aFirstGen)
analysis <- polr(employ_datQ20.6.aFirstGen$CatOutcome ~ employ_datQ20.6.aFirstGen$FirstGen, data=employ_datQ20.6.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.6.aGender<-multidatClean(Q20.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aGender$Gender<-factor(employ_datQ20.6.aGender$Gender)
employ_datQ20.6.aGender<-ordinaldatClean(employ_datQ20.6.aGender$Q20.6.a, employ_datQ20.6.aGender)
#ordinal(employ_datQ20.6.aGender$CatOutcome, employ_datQ20.6.aGender$Gender, employ_datQ20.6.aGender)
prep <- analysisPrep(employ_datQ20.6.aGender$CatOutcome, employ_datQ20.6.aGender$Gender, employ_datQ20.6.aGender)
analysis <- polr(employ_datQ20.6.aGender$CatOutcome ~ employ_datQ20.6.aGender$Gender, data=employ_datQ20.6.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.6.aSexuality<-multidatClean(Q20.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.6.aSexuality$Sexuality<-factor(employ_datQ20.6.aSexuality$Sexuality)
employ_datQ20.6.aSexuality<-ordinaldatClean(employ_datQ20.6.aSexuality$Q20.6.a, employ_datQ20.6.aSexuality)
#ordinal(employ_datQ20.6.aSexuality$CatOutcome, employ_datQ20.6.aSexuality$Sexuality, employ_datQ20.6.aSexuality)
prep <- analysisPrep(employ_datQ20.6.aSexuality$CatOutcome, employ_datQ20.6.aSexuality$Sexuality, employ_datQ20.6.aSexuality)
analysis <- polr(employ_datQ20.6.aSexuality$CatOutcome ~ employ_datQ20.6.aSexuality$Sexuality, data=employ_datQ20.6.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q20.7. I feel pressured to meet Key Performance Indicators / metrics, e.g. REF, grant funding"
"Status"
employ_datQ20.7.a<-multidatClean(Q20.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.7.a + Academic, data = employ_datQ20.7.a)
detach(dat_long)
employ_datQ20.7.a<-ordinaldatCleanNegative(employ_datQ20.7.a$Q20.7.a, employ_datQ20.7.a)
#ordinal(employ_datQ20.7.a$CatOutcome, employ_datQ20.7.a$Academic, employ_datQ20.7.a)
prep <- analysisPrep(employ_datQ20.7.a$CatOutcome, employ_datQ20.7.a$Academic, employ_datQ20.7.a)
analysis <- polr(employ_datQ20.7.a$CatOutcome ~ employ_datQ20.7.a$Academic, data=employ_datQ20.7.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ20.7.aCollege<-multidatClean(Q20.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q20.7.a + Q3, data = employ_datQ20.7.aCollege)
conTable
detach(dat_long)
employ_datQ20.7.aCollege$Q3[(employ_datQ20.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ20.7.aCollege$Q3<- factor(employ_datQ20.7.aCollege$Q3)
employ_datQ20.7.aCollege<-ordinaldatCleanNegative(employ_datQ20.7.aCollege$Q20.7.a, employ_datQ20.7.aCollege)
#ordinal(employ_datQ20.7.aCollege$CatOutcome, employ_datQ20.7.aCollege$Q3, employ_datQ20.7.aCollege)
prep <- analysisPrep(employ_datQ20.7.aCollege$CatOutcome, employ_datQ20.7.aCollege$Q3, employ_datQ20.7.aCollege)
analysis <- polr(employ_datQ20.7.aCollege$CatOutcome ~ employ_datQ20.7.aCollege$Q3, data=employ_datQ20.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ20.7.aCarer<-multidatClean(Q20.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aCarer$Carer<- factor(employ_datQ20.7.aCarer$Carer)
employ_datQ20.7.aCarer<-ordinaldatCleanNegative(employ_datQ20.7.aCarer$Q20.7.a, employ_datQ20.7.aCarer)
#ordinal(employ_datQ20.7.aCarer$CatOutcome, employ_datQ20.7.aCarer$Carer, employ_datQ20.7.aCarer)
prep <- analysisPrep(employ_datQ20.7.aCarer$CatOutcome, employ_datQ20.7.aCarer$Carer, employ_datQ20.7.aCarer)
analysis <- polr(employ_datQ20.7.aCarer$CatOutcome ~ employ_datQ20.7.aCarer$Carer, data=employ_datQ20.7.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ20.7.aDisability<-multidatClean(Q20.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aDisability$Disability<- factor(employ_datQ20.7.aDisability$Disability)
employ_datQ20.7.aDisability<-ordinaldatCleanNegative(employ_datQ20.7.aDisability$Q20.7.a, employ_datQ20.7.aDisability)
conTable <- xtabs(~Q20.7.a + Disability, data = employ_datQ20.7.aDisability)
#ordinal(employ_datQ20.7.aDisability$CatOutcome, employ_datQ20.7.aDisability$Disability, employ_datQ20.7.aDisability)
prep <- analysisPrep(employ_datQ20.7.aDisability$CatOutcome, employ_datQ20.7.aDisability$Disability, employ_datQ20.7.aDisability)
analysis <- polr(employ_datQ20.7.aDisability$CatOutcome ~ employ_datQ20.7.aDisability$Disability, data=employ_datQ20.7.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ20.7.aEthnicity<-multidatClean(Q20.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aEthnicity$Ethnicity<- factor(employ_datQ20.7.aEthnicity$EthnicityCleaned)
employ_datQ20.7.aEthnicity<-ordinaldatCleanNegative(employ_datQ20.7.aEthnicity$Q20.7.a, employ_datQ20.7.aEthnicity)
conTable <- xtabs(~Q20.7.a + EthnicityCleaned, data = employ_datQ20.7.aEthnicity)
conTable
#ordinal(employ_datQ20.7.aEthnicity$CatOutcome, employ_datQ20.7.aEthnicity$EthnicityCleaned, employ_datQ20.7.aEthnicity)
prep <- analysisPrep(employ_datQ20.7.aEthnicity$CatOutcome, employ_datQ20.7.aEthnicity$EthnicityCleaned, employ_datQ20.7.aEthnicity)
analysis <- polr(employ_datQ20.7.aEthnicity$CatOutcome ~ employ_datQ20.7.aEthnicity$EthnicityCleaned, data=employ_datQ20.7.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ20.7.aFirstGen<-multidatClean(Q20.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aFirstGen$FirstGen<-factor(employ_datQ20.7.aFirstGen$FirstGen)
employ_datQ20.7.aFirstGen<-ordinaldatCleanNegative(employ_datQ20.7.aFirstGen$Q20.7.a, employ_datQ20.7.aFirstGen)
#ordinal(employ_datQ20.7.aFirstGen$CatOutcome, employ_datQ20.7.aFirstGen$FirstGen, employ_datQ20.7.aFirstGen)
prep <- analysisPrep(employ_datQ20.7.aFirstGen$CatOutcome, employ_datQ20.7.aFirstGen$FirstGen, employ_datQ20.7.aFirstGen)
analysis <- polr(employ_datQ20.7.aFirstGen$CatOutcome ~ employ_datQ20.7.aFirstGen$FirstGen, data=employ_datQ20.7.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ20.7.aGender<-multidatClean(Q20.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aGender$Gender<-factor(employ_datQ20.7.aGender$Gender)
employ_datQ20.7.aGender<-ordinaldatCleanNegative(employ_datQ20.7.aGender$Q20.7.a, employ_datQ20.7.aGender)
#ordinal(employ_datQ20.7.aGender$CatOutcome, employ_datQ20.7.aGender$Gender, employ_datQ20.7.aGender)
prep <- analysisPrep(employ_datQ20.7.aGender$CatOutcome, employ_datQ20.7.aGender$Gender, employ_datQ20.7.aGender)
analysis <- polr(employ_datQ20.7.aGender$CatOutcome ~ employ_datQ20.7.aGender$Gender, data=employ_datQ20.7.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ20.7.aSexuality<-multidatClean(Q20.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ20.7.aSexuality$Sexuality<-factor(employ_datQ20.7.aSexuality$Sexuality)
employ_datQ20.7.aSexuality<-ordinaldatCleanNegative(employ_datQ20.7.aSexuality$Q20.7.a, employ_datQ20.7.aSexuality)
#ordinal(employ_datQ20.7.aSexuality$CatOutcome, employ_datQ20.7.aSexuality$Sexuality, employ_datQ20.7.aSexuality)
prep <- analysisPrep(employ_datQ20.7.aSexuality$CatOutcome, employ_datQ20.7.aSexuality$Sexuality, employ_datQ20.7.aSexuality)
analysis <- polr(employ_datQ20.7.aSexuality$CatOutcome ~ employ_datQ20.7.aSexuality$Sexuality, data=employ_datQ20.7.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q20.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q20.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q21.1. I would recommend a research career in my sector"
"Status"
employ_datQ21.1.a<-multidatClean(Q21.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.1.a + Academic, data = employ_datQ21.1.a)
detach(dat_long)
employ_datQ21.1.a<-ordinaldatClean(employ_datQ21.1.a$Q21.1.a, employ_datQ21.1.a)
#ordinal(employ_datQ21.1.a$CatOutcome, employ_datQ21.1.a$Academic, employ_datQ21.1.a)
prep <- analysisPrep(employ_datQ21.1.a$CatOutcome, employ_datQ21.1.a$Academic, employ_datQ21.1.a)
analysis <- polr(employ_datQ21.1.a$CatOutcome ~ employ_datQ21.1.a$Academic, data=employ_datQ21.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.1.aCollege<-multidatClean(Q21.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.1.a + Q3, data = employ_datQ21.1.aCollege)
conTable
detach(dat_long)
employ_datQ21.1.aCollege$Q3[(employ_datQ21.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.1.aCollege$Q3<- factor(employ_datQ21.1.aCollege$Q3)
employ_datQ21.1.aCollege<-ordinaldatClean(employ_datQ21.1.aCollege$Q21.1.a, employ_datQ21.1.aCollege)
#ordinal(employ_datQ21.1.aCollege$CatOutcome, employ_datQ21.1.aCollege$Q3, employ_datQ21.1.aCollege)
prep <- analysisPrep(employ_datQ21.1.aCollege$CatOutcome, employ_datQ21.1.aCollege$Q3, employ_datQ21.1.aCollege)
analysis <- polr(employ_datQ21.1.aCollege$CatOutcome ~ employ_datQ21.1.aCollege$Q3, data=employ_datQ21.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.1.aCarer<-multidatClean(Q21.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aCarer$Carer<- factor(employ_datQ21.1.aCarer$Carer)
employ_datQ21.1.aCarer<-ordinaldatClean(employ_datQ21.1.aCarer$Q21.1.a, employ_datQ21.1.aCarer)
#ordinal(employ_datQ21.1.aCarer$CatOutcome, employ_datQ21.1.aCarer$Carer, employ_datQ21.1.aCarer)
prep <- analysisPrep(employ_datQ21.1.aCarer$CatOutcome, employ_datQ21.1.aCarer$Carer, employ_datQ21.1.aCarer)
analysis <- polr(employ_datQ21.1.aCarer$CatOutcome ~ employ_datQ21.1.aCarer$Carer, data=employ_datQ21.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.1.aDisability<-multidatClean(Q21.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aDisability$Disability<- factor(employ_datQ21.1.aDisability$Disability)
employ_datQ21.1.aDisability<-ordinaldatClean(employ_datQ21.1.aDisability$Q21.1.a, employ_datQ21.1.aDisability)
conTable <- xtabs(~Q21.1.a + Disability, data = employ_datQ21.1.aDisability)
#ordinal(employ_datQ21.1.aDisability$CatOutcome, employ_datQ21.1.aDisability$Disability, employ_datQ21.1.aDisability)
prep <- analysisPrep(employ_datQ21.1.aDisability$CatOutcome, employ_datQ21.1.aDisability$Disability, employ_datQ21.1.aDisability)
analysis <- polr(employ_datQ21.1.aDisability$CatOutcome ~ employ_datQ21.1.aDisability$Disability, data=employ_datQ21.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.1.aEthnicity<-multidatClean(Q21.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aEthnicity$Ethnicity<- factor(employ_datQ21.1.aEthnicity$EthnicityCleaned)
employ_datQ21.1.aEthnicity<-ordinaldatClean(employ_datQ21.1.aEthnicity$Q21.1.a, employ_datQ21.1.aEthnicity)
conTable <- xtabs(~Q21.1.a + EthnicityCleaned, data = employ_datQ21.1.aEthnicity)
conTable
#ordinal(employ_datQ21.1.aEthnicity$CatOutcome, employ_datQ21.1.aEthnicity$EthnicityCleaned, employ_datQ21.1.aEthnicity)
prep <- analysisPrep(employ_datQ21.1.aEthnicity$CatOutcome, employ_datQ21.1.aEthnicity$EthnicityCleaned, employ_datQ21.1.aEthnicity)
analysis <- polr(employ_datQ21.1.aEthnicity$CatOutcome ~ employ_datQ21.1.aEthnicity$EthnicityCleaned, data=employ_datQ21.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.1.aFirstGen<-multidatClean(Q21.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aFirstGen$FirstGen<-factor(employ_datQ21.1.aFirstGen$FirstGen)
employ_datQ21.1.aFirstGen<-ordinaldatClean(employ_datQ21.1.aFirstGen$Q21.1.a, employ_datQ21.1.aFirstGen)
#ordinal(employ_datQ21.1.aFirstGen$CatOutcome, employ_datQ21.1.aFirstGen$FirstGen, employ_datQ21.1.aFirstGen)
prep <- analysisPrep(employ_datQ21.1.aFirstGen$CatOutcome, employ_datQ21.1.aFirstGen$FirstGen, employ_datQ21.1.aFirstGen)
analysis <- polr(employ_datQ21.1.aFirstGen$CatOutcome ~ employ_datQ21.1.aFirstGen$FirstGen, data=employ_datQ21.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.1.aGender<-multidatClean(Q21.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aGender$Gender<-factor(employ_datQ21.1.aGender$Gender)
employ_datQ21.1.aGender<-ordinaldatClean(employ_datQ21.1.aGender$Q21.1.a, employ_datQ21.1.aGender)
#ordinal(employ_datQ21.1.aGender$CatOutcome, employ_datQ21.1.aGender$Gender, employ_datQ21.1.aGender)
prep <- analysisPrep(employ_datQ21.1.aGender$CatOutcome, employ_datQ21.1.aGender$Gender, employ_datQ21.1.aGender)
analysis <- polr(employ_datQ21.1.aGender$CatOutcome ~ employ_datQ21.1.aGender$Gender, data=employ_datQ21.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.1.aSexuality<-multidatClean(Q21.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.1.aSexuality$Sexuality<-factor(employ_datQ21.1.aSexuality$Sexuality)
employ_datQ21.1.aSexuality<-ordinaldatClean(employ_datQ21.1.aSexuality$Q21.1.a, employ_datQ21.1.aSexuality)
#ordinal(employ_datQ21.1.aSexuality$CatOutcome, employ_datQ21.1.aSexuality$Sexuality, employ_datQ21.1.aSexuality)
prep <- analysisPrep(employ_datQ21.1.aSexuality$CatOutcome, employ_datQ21.1.aSexuality$Sexuality, employ_datQ21.1.aSexuality)
analysis <- polr(employ_datQ21.1.aSexuality$CatOutcome ~ employ_datQ21.1.aSexuality$Sexuality, data=employ_datQ21.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q21.2. I would recommend my lab/department to other researchersr"
"Status"
employ_datQ21.2.a<-multidatClean(Q21.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.2.a + Academic, data = employ_datQ21.2.a)
detach(dat_long)
employ_datQ21.2.a<-ordinaldatClean(employ_datQ21.2.a$Q21.2.a, employ_datQ21.2.a)
#ordinal(employ_datQ21.2.a$CatOutcome, employ_datQ21.2.a$Academic, employ_datQ21.2.a)
prep <- analysisPrep(employ_datQ21.2.a$CatOutcome, employ_datQ21.2.a$Academic, employ_datQ21.2.a)
analysis <- polr(employ_datQ21.2.a$CatOutcome ~ employ_datQ21.2.a$Academic, data=employ_datQ21.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.2.aCollege<-multidatClean(Q21.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.2.a + Q3, data = employ_datQ21.2.aCollege)
conTable
detach(dat_long)
employ_datQ21.2.aCollege$Q3[(employ_datQ21.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.2.aCollege$Q3<- factor(employ_datQ21.2.aCollege$Q3)
employ_datQ21.2.aCollege<-ordinaldatClean(employ_datQ21.2.aCollege$Q21.2.a, employ_datQ21.2.aCollege)
#ordinal(employ_datQ21.2.aCollege$CatOutcome, employ_datQ21.2.aCollege$Q3, employ_datQ21.2.aCollege)
prep <- analysisPrep(employ_datQ21.2.aCollege$CatOutcome, employ_datQ21.2.aCollege$Q3, employ_datQ21.2.aCollege)
analysis <- polr(employ_datQ21.2.aCollege$CatOutcome ~ employ_datQ21.2.aCollege$Q3, data=employ_datQ21.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.2.aCarer<-multidatClean(Q21.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aCarer$Carer<- factor(employ_datQ21.2.aCarer$Carer)
employ_datQ21.2.aCarer<-ordinaldatClean(employ_datQ21.2.aCarer$Q21.2.a, employ_datQ21.2.aCarer)
#ordinal(employ_datQ21.2.aCarer$CatOutcome, employ_datQ21.2.aCarer$Carer, employ_datQ21.2.aCarer)
prep <- analysisPrep(employ_datQ21.2.aCarer$CatOutcome, employ_datQ21.2.aCarer$Carer, employ_datQ21.2.aCarer)
analysis <- polr(employ_datQ21.2.aCarer$CatOutcome ~ employ_datQ21.2.aCarer$Carer, data=employ_datQ21.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.2.aDisability<-multidatClean(Q21.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aDisability$Disability<- factor(employ_datQ21.2.aDisability$Disability)
employ_datQ21.2.aDisability<-ordinaldatClean(employ_datQ21.2.aDisability$Q21.2.a, employ_datQ21.2.aDisability)
conTable <- xtabs(~Q21.2.a + Disability, data = employ_datQ21.2.aDisability)
#ordinal(employ_datQ21.2.aDisability$CatOutcome, employ_datQ21.2.aDisability$Disability, employ_datQ21.2.aDisability)
prep <- analysisPrep(employ_datQ21.2.aDisability$CatOutcome, employ_datQ21.2.aDisability$Disability, employ_datQ21.2.aDisability)
analysis <- polr(employ_datQ21.2.aDisability$CatOutcome ~ employ_datQ21.2.aDisability$Disability, data=employ_datQ21.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.2.aEthnicity<-multidatClean(Q21.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aEthnicity$Ethnicity<- factor(employ_datQ21.2.aEthnicity$EthnicityCleaned)
employ_datQ21.2.aEthnicity<-ordinaldatClean(employ_datQ21.2.aEthnicity$Q21.2.a, employ_datQ21.2.aEthnicity)
conTable <- xtabs(~Q21.2.a + EthnicityCleaned, data = employ_datQ21.2.aEthnicity)
conTable
#ordinal(employ_datQ21.2.aEthnicity$CatOutcome, employ_datQ21.2.aEthnicity$EthnicityCleaned, employ_datQ21.2.aEthnicity)
prep <- analysisPrep(employ_datQ21.2.aEthnicity$CatOutcome, employ_datQ21.2.aEthnicity$EthnicityCleaned, employ_datQ21.2.aEthnicity)
analysis <- polr(employ_datQ21.2.aEthnicity$CatOutcome ~ employ_datQ21.2.aEthnicity$EthnicityCleaned, data=employ_datQ21.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.2.aFirstGen<-multidatClean(Q21.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aFirstGen$FirstGen<-factor(employ_datQ21.2.aFirstGen$FirstGen)
employ_datQ21.2.aFirstGen<-ordinaldatClean(employ_datQ21.2.aFirstGen$Q21.2.a, employ_datQ21.2.aFirstGen)
#ordinal(employ_datQ21.2.aFirstGen$CatOutcome, employ_datQ21.2.aFirstGen$FirstGen, employ_datQ21.2.aFirstGen)
prep <- analysisPrep(employ_datQ21.2.aFirstGen$CatOutcome, employ_datQ21.2.aFirstGen$FirstGen, employ_datQ21.2.aFirstGen)
analysis <- polr(employ_datQ21.2.aFirstGen$CatOutcome ~ employ_datQ21.2.aFirstGen$FirstGen, data=employ_datQ21.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.2.aGender<-multidatClean(Q21.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aGender$Gender<-factor(employ_datQ21.2.aGender$Gender)
employ_datQ21.2.aGender<-ordinaldatClean(employ_datQ21.2.aGender$Q21.2.a, employ_datQ21.2.aGender)
#ordinal(employ_datQ21.2.aGender$CatOutcome, employ_datQ21.2.aGender$Gender, employ_datQ21.2.aGender)
prep <- analysisPrep(employ_datQ21.2.aGender$CatOutcome, employ_datQ21.2.aGender$Gender, employ_datQ21.2.aGender)
analysis <- polr(employ_datQ21.2.aGender$CatOutcome ~ employ_datQ21.2.aGender$Gender, data=employ_datQ21.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.2.aSexuality<-multidatClean(Q21.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.2.aSexuality$Sexuality<-factor(employ_datQ21.2.aSexuality$Sexuality)
employ_datQ21.2.aSexuality<-ordinaldatClean(employ_datQ21.2.aSexuality$Q21.2.a, employ_datQ21.2.aSexuality)
#ordinal(employ_datQ21.2.aSexuality$CatOutcome, employ_datQ21.2.aSexuality$Sexuality, employ_datQ21.2.aSexuality)
prep <- analysisPrep(employ_datQ21.2.aSexuality$CatOutcome, employ_datQ21.2.aSexuality$Sexuality, employ_datQ21.2.aSexuality)
analysis <- polr(employ_datQ21.2.aSexuality$CatOutcome ~ employ_datQ21.2.aSexuality$Sexuality, data=employ_datQ21.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q21.3. I am proud to work within the research community"
"Status"
employ_datQ21.3.a<-multidatClean(Q21.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.3.a + Academic, data = employ_datQ21.3.a)
detach(dat_long)
employ_datQ21.3.a<-ordinaldatClean(employ_datQ21.3.a$Q21.3.a, employ_datQ21.3.a)
#ordinal(employ_datQ21.3.a$CatOutcome, employ_datQ21.3.a$Academic, employ_datQ21.3.a)
prep <- analysisPrep(employ_datQ21.3.a$CatOutcome, employ_datQ21.3.a$Academic, employ_datQ21.3.a)
analysis <- polr(employ_datQ21.3.a$CatOutcome ~ employ_datQ21.3.a$Academic, data=employ_datQ21.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.3.aCollege<-multidatClean(Q21.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.3.a + Q3, data = employ_datQ21.3.aCollege)
conTable
detach(dat_long)
employ_datQ21.3.aCollege$Q3[(employ_datQ21.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.3.aCollege$Q3<- factor(employ_datQ21.3.aCollege$Q3)
employ_datQ21.3.aCollege<-ordinaldatClean(employ_datQ21.3.aCollege$Q21.3.a, employ_datQ21.3.aCollege)
#ordinal(employ_datQ21.3.aCollege$CatOutcome, employ_datQ21.3.aCollege$Q3, employ_datQ21.3.aCollege)
prep <- analysisPrep(employ_datQ21.3.aCollege$CatOutcome, employ_datQ21.3.aCollege$Q3, employ_datQ21.3.aCollege)
analysis <- polr(employ_datQ21.3.aCollege$CatOutcome ~ employ_datQ21.3.aCollege$Q3, data=employ_datQ21.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.3.aCarer<-multidatClean(Q21.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aCarer$Carer<- factor(employ_datQ21.3.aCarer$Carer)
employ_datQ21.3.aCarer<-ordinaldatClean(employ_datQ21.3.aCarer$Q21.3.a, employ_datQ21.3.aCarer)
#ordinal(employ_datQ21.3.aCarer$CatOutcome, employ_datQ21.3.aCarer$Carer, employ_datQ21.3.aCarer)
prep <- analysisPrep(employ_datQ21.3.aCarer$CatOutcome, employ_datQ21.3.aCarer$Carer, employ_datQ21.3.aCarer)
analysis <- polr(employ_datQ21.3.aCarer$CatOutcome ~ employ_datQ21.3.aCarer$Carer, data=employ_datQ21.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.3.aDisability<-multidatClean(Q21.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aDisability$Disability<- factor(employ_datQ21.3.aDisability$Disability)
employ_datQ21.3.aDisability<-ordinaldatClean(employ_datQ21.3.aDisability$Q21.3.a, employ_datQ21.3.aDisability)
conTable <- xtabs(~Q21.3.a + Disability, data = employ_datQ21.3.aDisability)
#ordinal(employ_datQ21.3.aDisability$CatOutcome, employ_datQ21.3.aDisability$Disability, employ_datQ21.3.aDisability)
prep <- analysisPrep(employ_datQ21.3.aDisability$CatOutcome, employ_datQ21.3.aDisability$Disability, employ_datQ21.3.aDisability)
analysis <- polr(employ_datQ21.3.aDisability$CatOutcome ~ employ_datQ21.3.aDisability$Disability, data=employ_datQ21.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.3.aEthnicity<-multidatClean(Q21.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aEthnicity$Ethnicity<- factor(employ_datQ21.3.aEthnicity$EthnicityCleaned)
employ_datQ21.3.aEthnicity<-ordinaldatClean(employ_datQ21.3.aEthnicity$Q21.3.a, employ_datQ21.3.aEthnicity)
conTable <- xtabs(~Q21.3.a + EthnicityCleaned, data = employ_datQ21.3.aEthnicity)
conTable
#ordinal(employ_datQ21.3.aEthnicity$CatOutcome, employ_datQ21.3.aEthnicity$EthnicityCleaned, employ_datQ21.3.aEthnicity)
prep <- analysisPrep(employ_datQ21.3.aEthnicity$CatOutcome, employ_datQ21.3.aEthnicity$EthnicityCleaned, employ_datQ21.3.aEthnicity)
analysis <- polr(employ_datQ21.3.aEthnicity$CatOutcome ~ employ_datQ21.3.aEthnicity$EthnicityCleaned, data=employ_datQ21.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.3.aFirstGen<-multidatClean(Q21.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aFirstGen$FirstGen<-factor(employ_datQ21.3.aFirstGen$FirstGen)
employ_datQ21.3.aFirstGen<-ordinaldatClean(employ_datQ21.3.aFirstGen$Q21.3.a, employ_datQ21.3.aFirstGen)
#ordinal(employ_datQ21.3.aFirstGen$CatOutcome, employ_datQ21.3.aFirstGen$FirstGen, employ_datQ21.3.aFirstGen)
prep <- analysisPrep(employ_datQ21.3.aFirstGen$CatOutcome, employ_datQ21.3.aFirstGen$FirstGen, employ_datQ21.3.aFirstGen)
analysis <- polr(employ_datQ21.3.aFirstGen$CatOutcome ~ employ_datQ21.3.aFirstGen$FirstGen, data=employ_datQ21.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.3.aGender<-multidatClean(Q21.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aGender$Gender<-factor(employ_datQ21.3.aGender$Gender)
employ_datQ21.3.aGender<-ordinaldatClean(employ_datQ21.3.aGender$Q21.3.a, employ_datQ21.3.aGender)
#ordinal(employ_datQ21.3.aGender$CatOutcome, employ_datQ21.3.aGender$Gender, employ_datQ21.3.aGender)
prep <- analysisPrep(employ_datQ21.3.aGender$CatOutcome, employ_datQ21.3.aGender$Gender, employ_datQ21.3.aGender)
analysis <- polr(employ_datQ21.3.aGender$CatOutcome ~ employ_datQ21.3.aGender$Gender, data=employ_datQ21.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.3.aSexuality<-multidatClean(Q21.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.3.aSexuality$Sexuality<-factor(employ_datQ21.3.aSexuality$Sexuality)
employ_datQ21.3.aSexuality<-ordinaldatClean(employ_datQ21.3.aSexuality$Q21.3.a, employ_datQ21.3.aSexuality)
#ordinal(employ_datQ21.3.aSexuality$CatOutcome, employ_datQ21.3.aSexuality$Sexuality, employ_datQ21.3.aSexuality)
prep <- analysisPrep(employ_datQ21.3.aSexuality$CatOutcome, employ_datQ21.3.aSexuality$Sexuality, employ_datQ21.3.aSexuality)
analysis <- polr(employ_datQ21.3.aSexuality$CatOutcome ~ employ_datQ21.3.aSexuality$Sexuality, data=employ_datQ21.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q20 - How far do you agree or disagree with the following statements relating to your career over the last 1-5 years?
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q21.4. I am aware of alternative career options outside of research that could utilise my skills"
"Status"
employ_datQ21.4.a<-multidatClean(Q21.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.4.a + Academic, data = employ_datQ21.4.a)
detach(dat_long)
employ_datQ21.4.a<-ordinaldatClean(employ_datQ21.4.a$Q21.4.a, employ_datQ21.4.a)
#ordinal(employ_datQ21.4.a$CatOutcome, employ_datQ21.4.a$Academic, employ_datQ21.4.a)
prep <- analysisPrep(employ_datQ21.4.a$CatOutcome, employ_datQ21.4.a$Academic, employ_datQ21.4.a)
analysis <- polr(employ_datQ21.4.a$CatOutcome ~ employ_datQ21.4.a$Academic, data=employ_datQ21.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.4.aCollege<-multidatClean(Q21.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.4.a + Q3, data = employ_datQ21.4.aCollege)
conTable
detach(dat_long)
employ_datQ21.4.aCollege$Q3[(employ_datQ21.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.4.aCollege$Q3<- factor(employ_datQ21.4.aCollege$Q3)
employ_datQ21.4.aCollege<-ordinaldatClean(employ_datQ21.4.aCollege$Q21.4.a, employ_datQ21.4.aCollege)
#ordinal(employ_datQ21.4.aCollege$CatOutcome, employ_datQ21.4.aCollege$Q3, employ_datQ21.4.aCollege)
prep <- analysisPrep(employ_datQ21.4.aCollege$CatOutcome, employ_datQ21.4.aCollege$Q3, employ_datQ21.4.aCollege)
analysis <- polr(employ_datQ21.4.aCollege$CatOutcome ~ employ_datQ21.4.aCollege$Q3, data=employ_datQ21.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.4.aCarer<-multidatClean(Q21.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aCarer$Carer<- factor(employ_datQ21.4.aCarer$Carer)
employ_datQ21.4.aCarer<-ordinaldatClean(employ_datQ21.4.aCarer$Q21.4.a, employ_datQ21.4.aCarer)
#ordinal(employ_datQ21.4.aCarer$CatOutcome, employ_datQ21.4.aCarer$Carer, employ_datQ21.4.aCarer)
prep <- analysisPrep(employ_datQ21.4.aCarer$CatOutcome, employ_datQ21.4.aCarer$Carer, employ_datQ21.4.aCarer)
analysis <- polr(employ_datQ21.4.aCarer$CatOutcome ~ employ_datQ21.4.aCarer$Carer, data=employ_datQ21.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.4.aDisability<-multidatClean(Q21.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aDisability$Disability<- factor(employ_datQ21.4.aDisability$Disability)
employ_datQ21.4.aDisability<-ordinaldatClean(employ_datQ21.4.aDisability$Q21.4.a, employ_datQ21.4.aDisability)
conTable <- xtabs(~Q21.4.a + Disability, data = employ_datQ21.4.aDisability)
#ordinal(employ_datQ21.4.aDisability$CatOutcome, employ_datQ21.4.aDisability$Disability, employ_datQ21.4.aDisability)
prep <- analysisPrep(employ_datQ21.4.aDisability$CatOutcome, employ_datQ21.4.aDisability$Disability, employ_datQ21.4.aDisability)
analysis <- polr(employ_datQ21.4.aDisability$CatOutcome ~ employ_datQ21.4.aDisability$Disability, data=employ_datQ21.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.4.aEthnicity<-multidatClean(Q21.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aEthnicity$Ethnicity<- factor(employ_datQ21.4.aEthnicity$EthnicityCleaned)
employ_datQ21.4.aEthnicity<-ordinaldatClean(employ_datQ21.4.aEthnicity$Q21.4.a, employ_datQ21.4.aEthnicity)
conTable <- xtabs(~Q21.4.a + EthnicityCleaned, data = employ_datQ21.4.aEthnicity)
conTable
#ordinal(employ_datQ21.4.aEthnicity$CatOutcome, employ_datQ21.4.aEthnicity$EthnicityCleaned, employ_datQ21.4.aEthnicity)
prep <- analysisPrep(employ_datQ21.4.aEthnicity$CatOutcome, employ_datQ21.4.aEthnicity$EthnicityCleaned, employ_datQ21.4.aEthnicity)
analysis <- polr(employ_datQ21.4.aEthnicity$CatOutcome ~ employ_datQ21.4.aEthnicity$EthnicityCleaned, data=employ_datQ21.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.4.aFirstGen<-multidatClean(Q21.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aFirstGen$FirstGen<-factor(employ_datQ21.4.aFirstGen$FirstGen)
employ_datQ21.4.aFirstGen<-ordinaldatClean(employ_datQ21.4.aFirstGen$Q21.4.a, employ_datQ21.4.aFirstGen)
#ordinal(employ_datQ21.4.aFirstGen$CatOutcome, employ_datQ21.4.aFirstGen$FirstGen, employ_datQ21.4.aFirstGen)
prep <- analysisPrep(employ_datQ21.4.aFirstGen$CatOutcome, employ_datQ21.4.aFirstGen$FirstGen, employ_datQ21.4.aFirstGen)
analysis <- polr(employ_datQ21.4.aFirstGen$CatOutcome ~ employ_datQ21.4.aFirstGen$FirstGen, data=employ_datQ21.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.4.aGender<-multidatClean(Q21.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aGender$Gender<-factor(employ_datQ21.4.aGender$Gender)
employ_datQ21.4.aGender<-ordinaldatClean(employ_datQ21.4.aGender$Q21.4.a, employ_datQ21.4.aGender)
#ordinal(employ_datQ21.4.aGender$CatOutcome, employ_datQ21.4.aGender$Gender, employ_datQ21.4.aGender)
prep <- analysisPrep(employ_datQ21.4.aGender$CatOutcome, employ_datQ21.4.aGender$Gender, employ_datQ21.4.aGender)
analysis <- polr(employ_datQ21.4.aGender$CatOutcome ~ employ_datQ21.4.aGender$Gender, data=employ_datQ21.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.4.aSexuality<-multidatClean(Q21.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.4.aSexuality$Sexuality<-factor(employ_datQ21.4.aSexuality$Sexuality)
employ_datQ21.4.aSexuality<-ordinaldatClean(employ_datQ21.4.aSexuality$Q21.4.a, employ_datQ21.4.aSexuality)
#ordinal(employ_datQ21.4.aSexuality$CatOutcome, employ_datQ21.4.aSexuality$Sexuality, employ_datQ21.4.aSexuality)
prep <- analysisPrep(employ_datQ21.4.aSexuality$CatOutcome, employ_datQ21.4.aSexuality$Sexuality, employ_datQ21.4.aSexuality)
analysis <- polr(employ_datQ21.4.aSexuality$CatOutcome ~ employ_datQ21.4.aSexuality$Sexuality, data=employ_datQ21.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.5.a<-multidatClean(Q21.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.5.a + Academic, data = employ_datQ21.5.a)
detach(dat_long)
employ_datQ21.5.a<-ordinaldatClean(employ_datQ21.5.a$Q21.5.a, employ_datQ21.5.a)
#ordinal(employ_datQ21.5.a$CatOutcome, employ_datQ21.5.a$Academic, employ_datQ21.5.a)
prep <- analysisPrep(employ_datQ21.5.a$CatOutcome, employ_datQ21.5.a$Academic, employ_datQ21.5.a)
analysis <- polr(employ_datQ21.5.a$CatOutcome ~ employ_datQ21.5.a$Academic, data=employ_datQ21.5.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.5.aCollege<-multidatClean(Q21.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.5.a + Q3, data = employ_datQ21.5.aCollege)
conTable
detach(dat_long)
employ_datQ21.5.aCollege$Q3[(employ_datQ21.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.5.aCollege$Q3<- factor(employ_datQ21.5.aCollege$Q3)
employ_datQ21.5.aCollege<-ordinaldatClean(employ_datQ21.5.aCollege$Q21.5.a, employ_datQ21.5.aCollege)
#ordinal(employ_datQ21.5.aCollege$CatOutcome, employ_datQ21.5.aCollege$Q3, employ_datQ21.5.aCollege)
prep <- analysisPrep(employ_datQ21.5.aCollege$CatOutcome, employ_datQ21.5.aCollege$Q3, employ_datQ21.5.aCollege)
analysis <- polr(employ_datQ21.5.aCollege$CatOutcome ~ employ_datQ21.5.aCollege$Q3, data=employ_datQ21.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.5.aCarer<-multidatClean(Q21.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aCarer$Carer<- factor(employ_datQ21.5.aCarer$Carer)
employ_datQ21.5.aCarer<-ordinaldatClean(employ_datQ21.5.aCarer$Q21.5.a, employ_datQ21.5.aCarer)
#ordinal(employ_datQ21.5.aCarer$CatOutcome, employ_datQ21.5.aCarer$Carer, employ_datQ21.5.aCarer)
prep <- analysisPrep(employ_datQ21.5.aCarer$CatOutcome, employ_datQ21.5.aCarer$Carer, employ_datQ21.5.aCarer)
analysis <- polr(employ_datQ21.5.aCarer$CatOutcome ~ employ_datQ21.5.aCarer$Carer, data=employ_datQ21.5.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.5.aDisability<-multidatClean(Q21.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aDisability$Disability<- factor(employ_datQ21.5.aDisability$Disability)
employ_datQ21.5.aDisability<-ordinaldatClean(employ_datQ21.5.aDisability$Q21.5.a, employ_datQ21.5.aDisability)
conTable <- xtabs(~Q21.5.a + Disability, data = employ_datQ21.5.aDisability)
#ordinal(employ_datQ21.5.aDisability$CatOutcome, employ_datQ21.5.aDisability$Disability, employ_datQ21.5.aDisability)
prep <- analysisPrep(employ_datQ21.5.aDisability$CatOutcome, employ_datQ21.5.aDisability$Disability, employ_datQ21.5.aDisability)
analysis <- polr(employ_datQ21.5.aDisability$CatOutcome ~ employ_datQ21.5.aDisability$Disability, data=employ_datQ21.5.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.5.aEthnicity<-multidatClean(Q21.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aEthnicity$Ethnicity<- factor(employ_datQ21.5.aEthnicity$EthnicityCleaned)
employ_datQ21.5.aEthnicity<-ordinaldatClean(employ_datQ21.5.aEthnicity$Q21.5.a, employ_datQ21.5.aEthnicity)
conTable <- xtabs(~Q21.5.a + EthnicityCleaned, data = employ_datQ21.5.aEthnicity)
conTable
#ordinal(employ_datQ21.5.aEthnicity$CatOutcome, employ_datQ21.5.aEthnicity$EthnicityCleaned, employ_datQ21.5.aEthnicity)
prep <- analysisPrep(employ_datQ21.5.aEthnicity$CatOutcome, employ_datQ21.5.aEthnicity$EthnicityCleaned, employ_datQ21.5.aEthnicity)
analysis <- polr(employ_datQ21.5.aEthnicity$CatOutcome ~ employ_datQ21.5.aEthnicity$EthnicityCleaned, data=employ_datQ21.5.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.5.aFirstGen<-multidatClean(Q21.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aFirstGen$FirstGen<-factor(employ_datQ21.5.aFirstGen$FirstGen)
employ_datQ21.5.aFirstGen<-ordinaldatClean(employ_datQ21.5.aFirstGen$Q21.5.a, employ_datQ21.5.aFirstGen)
#ordinal(employ_datQ21.5.aFirstGen$CatOutcome, employ_datQ21.5.aFirstGen$FirstGen, employ_datQ21.5.aFirstGen)
prep <- analysisPrep(employ_datQ21.5.aFirstGen$CatOutcome, employ_datQ21.5.aFirstGen$FirstGen, employ_datQ21.5.aFirstGen)
analysis <- polr(employ_datQ21.5.aFirstGen$CatOutcome ~ employ_datQ21.5.aFirstGen$FirstGen, data=employ_datQ21.5.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.5.aGender<-multidatClean(Q21.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aGender$Gender<-factor(employ_datQ21.5.aGender$Gender)
employ_datQ21.5.aGender<-ordinaldatClean(employ_datQ21.5.aGender$Q21.5.a, employ_datQ21.5.aGender)
#ordinal(employ_datQ21.5.aGender$CatOutcome, employ_datQ21.5.aGender$Gender, employ_datQ21.5.aGender)
prep <- analysisPrep(employ_datQ21.5.aGender$CatOutcome, employ_datQ21.5.aGender$Gender, employ_datQ21.5.aGender)
analysis <- polr(employ_datQ21.5.aGender$CatOutcome ~ employ_datQ21.5.aGender$Gender, data=employ_datQ21.5.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.5.aSexuality<-multidatClean(Q21.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.5.aSexuality$Sexuality<-factor(employ_datQ21.5.aSexuality$Sexuality)
employ_datQ21.5.aSexuality<-ordinaldatClean(employ_datQ21.5.aSexuality$Q21.5.a, employ_datQ21.5.aSexuality)
#ordinal(employ_datQ21.5.aSexuality$CatOutcome, employ_datQ21.5.aSexuality$Sexuality, employ_datQ21.5.aSexuality)
prep <- analysisPrep(employ_datQ21.5.aSexuality$CatOutcome, employ_datQ21.5.aSexuality$Sexuality, employ_datQ21.5.aSexuality)
analysis <- polr(employ_datQ21.5.aSexuality$CatOutcome ~ employ_datQ21.5.aSexuality$Sexuality, data=employ_datQ21.5.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.6.a<-multidatClean(Q21.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.6.a + Academic, data = employ_datQ21.6.a)
detach(dat_long)
employ_datQ21.6.a<-ordinaldatClean(employ_datQ21.6.a$Q21.6.a, employ_datQ21.6.a)
#ordinal(employ_datQ21.6.a$CatOutcome, employ_datQ21.6.a$Academic, employ_datQ21.6.a)
prep <- analysisPrep(employ_datQ21.6.a$CatOutcome, employ_datQ21.6.a$Academic, employ_datQ21.6.a)
analysis <- polr(employ_datQ21.6.a$CatOutcome ~ employ_datQ21.6.a$Academic, data=employ_datQ21.6.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.6.aCollege<-multidatClean(Q21.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.6.a + Q3, data = employ_datQ21.6.aCollege)
conTable
detach(dat_long)
employ_datQ21.6.aCollege$Q3[(employ_datQ21.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.6.aCollege$Q3<- factor(employ_datQ21.6.aCollege$Q3)
employ_datQ21.6.aCollege<-ordinaldatClean(employ_datQ21.6.aCollege$Q21.6.a, employ_datQ21.6.aCollege)
#ordinal(employ_datQ21.6.aCollege$CatOutcome, employ_datQ21.6.aCollege$Q3, employ_datQ21.6.aCollege)
prep <- analysisPrep(employ_datQ21.6.aCollege$CatOutcome, employ_datQ21.6.aCollege$Q3, employ_datQ21.6.aCollege)
analysis <- polr(employ_datQ21.6.aCollege$CatOutcome ~ employ_datQ21.6.aCollege$Q3, data=employ_datQ21.6.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.6.aCarer<-multidatClean(Q21.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aCarer$Carer<- factor(employ_datQ21.6.aCarer$Carer)
employ_datQ21.6.aCarer<-ordinaldatClean(employ_datQ21.6.aCarer$Q21.6.a, employ_datQ21.6.aCarer)
#ordinal(employ_datQ21.6.aCarer$CatOutcome, employ_datQ21.6.aCarer$Carer, employ_datQ21.6.aCarer)
prep <- analysisPrep(employ_datQ21.6.aCarer$CatOutcome, employ_datQ21.6.aCarer$Carer, employ_datQ21.6.aCarer)
analysis <- polr(employ_datQ21.6.aCarer$CatOutcome ~ employ_datQ21.6.aCarer$Carer, data=employ_datQ21.6.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.6.aDisability<-multidatClean(Q21.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aDisability$Disability<- factor(employ_datQ21.6.aDisability$Disability)
employ_datQ21.6.aDisability<-ordinaldatClean(employ_datQ21.6.aDisability$Q21.6.a, employ_datQ21.6.aDisability)
conTable <- xtabs(~Q21.6.a + Disability, data = employ_datQ21.6.aDisability)
#ordinal(employ_datQ21.6.aDisability$CatOutcome, employ_datQ21.6.aDisability$Disability, employ_datQ21.6.aDisability)
prep <- analysisPrep(employ_datQ21.6.aDisability$CatOutcome, employ_datQ21.6.aDisability$Disability, employ_datQ21.6.aDisability)
analysis <- polr(employ_datQ21.6.aDisability$CatOutcome ~ employ_datQ21.6.aDisability$Disability, data=employ_datQ21.6.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.6.aEthnicity<-multidatClean(Q21.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aEthnicity$Ethnicity<- factor(employ_datQ21.6.aEthnicity$EthnicityCleaned)
employ_datQ21.6.aEthnicity<-ordinaldatClean(employ_datQ21.6.aEthnicity$Q21.6.a, employ_datQ21.6.aEthnicity)
conTable <- xtabs(~Q21.6.a + EthnicityCleaned, data = employ_datQ21.6.aEthnicity)
conTable
#ordinal(employ_datQ21.6.aEthnicity$CatOutcome, employ_datQ21.6.aEthnicity$EthnicityCleaned, employ_datQ21.6.aEthnicity)
prep <- analysisPrep(employ_datQ21.6.aEthnicity$CatOutcome, employ_datQ21.6.aEthnicity$EthnicityCleaned, employ_datQ21.6.aEthnicity)
analysis <- polr(employ_datQ21.6.aEthnicity$CatOutcome ~ employ_datQ21.6.aEthnicity$EthnicityCleaned, data=employ_datQ21.6.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.6.aFirstGen<-multidatClean(Q21.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aFirstGen$FirstGen<-factor(employ_datQ21.6.aFirstGen$FirstGen)
employ_datQ21.6.aFirstGen<-ordinaldatClean(employ_datQ21.6.aFirstGen$Q21.6.a, employ_datQ21.6.aFirstGen)
#ordinal(employ_datQ21.6.aFirstGen$CatOutcome, employ_datQ21.6.aFirstGen$FirstGen, employ_datQ21.6.aFirstGen)
prep <- analysisPrep(employ_datQ21.6.aFirstGen$CatOutcome, employ_datQ21.6.aFirstGen$FirstGen, employ_datQ21.6.aFirstGen)
analysis <- polr(employ_datQ21.6.aFirstGen$CatOutcome ~ employ_datQ21.6.aFirstGen$FirstGen, data=employ_datQ21.6.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.6.aGender<-multidatClean(Q21.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aGender$Gender<-factor(employ_datQ21.6.aGender$Gender)
employ_datQ21.6.aGender<-ordinaldatClean(employ_datQ21.6.aGender$Q21.6.a, employ_datQ21.6.aGender)
#ordinal(employ_datQ21.6.aGender$CatOutcome, employ_datQ21.6.aGender$Gender, employ_datQ21.6.aGender)
prep <- analysisPrep(employ_datQ21.6.aGender$CatOutcome, employ_datQ21.6.aGender$Gender, employ_datQ21.6.aGender)
analysis <- polr(employ_datQ21.6.aGender$CatOutcome ~ employ_datQ21.6.aGender$Gender, data=employ_datQ21.6.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.6.aSexuality<-multidatClean(Q21.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.6.aSexuality$Sexuality<-factor(employ_datQ21.6.aSexuality$Sexuality)
employ_datQ21.6.aSexuality<-ordinaldatClean(employ_datQ21.6.aSexuality$Q21.6.a, employ_datQ21.6.aSexuality)
#ordinal(employ_datQ21.6.aSexuality$CatOutcome, employ_datQ21.6.aSexuality$Sexuality, employ_datQ21.6.aSexuality)
prep <- analysisPrep(employ_datQ21.6.aSexuality$CatOutcome, employ_datQ21.6.aSexuality$Sexuality, employ_datQ21.6.aSexuality)
analysis <- polr(employ_datQ21.6.aSexuality$CatOutcome ~ employ_datQ21.6.aSexuality$Sexuality, data=employ_datQ21.6.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.7.a<-multidatClean(Q21.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.7.a + Academic, data = employ_datQ21.7.a)
detach(dat_long)
employ_datQ21.7.a<-ordinaldatClean(employ_datQ21.7.a$Q21.7.a, employ_datQ21.7.a)
#ordinal(employ_datQ21.7.a$CatOutcome, employ_datQ21.7.a$Academic, employ_datQ21.7.a)
prep <- analysisPrep(employ_datQ21.7.a$CatOutcome, employ_datQ21.7.a$Academic, employ_datQ21.7.a)
analysis <- polr(employ_datQ21.7.a$CatOutcome ~ employ_datQ21.7.a$Academic, data=employ_datQ21.7.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.7.aCollege<-multidatClean(Q21.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.7.a + Q3, data = employ_datQ21.7.aCollege)
conTable
detach(dat_long)
employ_datQ21.7.aCollege$Q3[(employ_datQ21.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.7.aCollege$Q3<- factor(employ_datQ21.7.aCollege$Q3)
employ_datQ21.7.aCollege<-ordinaldatClean(employ_datQ21.7.aCollege$Q21.7.a, employ_datQ21.7.aCollege)
#ordinal(employ_datQ21.7.aCollege$CatOutcome, employ_datQ21.7.aCollege$Q3, employ_datQ21.7.aCollege)
prep <- analysisPrep(employ_datQ21.7.aCollege$CatOutcome, employ_datQ21.7.aCollege$Q3, employ_datQ21.7.aCollege)
analysis <- polr(employ_datQ21.7.aCollege$CatOutcome ~ employ_datQ21.7.aCollege$Q3, data=employ_datQ21.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.7.aCarer<-multidatClean(Q21.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aCarer$Carer<- factor(employ_datQ21.7.aCarer$Carer)
employ_datQ21.7.aCarer<-ordinaldatClean(employ_datQ21.7.aCarer$Q21.7.a, employ_datQ21.7.aCarer)
#ordinal(employ_datQ21.7.aCarer$CatOutcome, employ_datQ21.7.aCarer$Carer, employ_datQ21.7.aCarer)
prep <- analysisPrep(employ_datQ21.7.aCarer$CatOutcome, employ_datQ21.7.aCarer$Carer, employ_datQ21.7.aCarer)
analysis <- polr(employ_datQ21.7.aCarer$CatOutcome ~ employ_datQ21.7.aCarer$Carer, data=employ_datQ21.7.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.7.aDisability<-multidatClean(Q21.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aDisability$Disability<- factor(employ_datQ21.7.aDisability$Disability)
employ_datQ21.7.aDisability<-ordinaldatClean(employ_datQ21.7.aDisability$Q21.7.a, employ_datQ21.7.aDisability)
conTable <- xtabs(~Q21.7.a + Disability, data = employ_datQ21.7.aDisability)
#ordinal(employ_datQ21.7.aDisability$CatOutcome, employ_datQ21.7.aDisability$Disability, employ_datQ21.7.aDisability)
prep <- analysisPrep(employ_datQ21.7.aDisability$CatOutcome, employ_datQ21.7.aDisability$Disability, employ_datQ21.7.aDisability)
analysis <- polr(employ_datQ21.7.aDisability$CatOutcome ~ employ_datQ21.7.aDisability$Disability, data=employ_datQ21.7.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.7.aEthnicity<-multidatClean(Q21.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aEthnicity$Ethnicity<- factor(employ_datQ21.7.aEthnicity$EthnicityCleaned)
employ_datQ21.7.aEthnicity<-ordinaldatClean(employ_datQ21.7.aEthnicity$Q21.7.a, employ_datQ21.7.aEthnicity)
conTable <- xtabs(~Q21.7.a + EthnicityCleaned, data = employ_datQ21.7.aEthnicity)
conTable
#ordinal(employ_datQ21.7.aEthnicity$CatOutcome, employ_datQ21.7.aEthnicity$EthnicityCleaned, employ_datQ21.7.aEthnicity)
prep <- analysisPrep(employ_datQ21.7.aEthnicity$CatOutcome, employ_datQ21.7.aEthnicity$EthnicityCleaned, employ_datQ21.7.aEthnicity)
analysis <- polr(employ_datQ21.7.aEthnicity$CatOutcome ~ employ_datQ21.7.aEthnicity$EthnicityCleaned, data=employ_datQ21.7.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.7.aFirstGen<-multidatClean(Q21.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aFirstGen$FirstGen<-factor(employ_datQ21.7.aFirstGen$FirstGen)
employ_datQ21.7.aFirstGen<-ordinaldatClean(employ_datQ21.7.aFirstGen$Q21.7.a, employ_datQ21.7.aFirstGen)
#ordinal(employ_datQ21.7.aFirstGen$CatOutcome, employ_datQ21.7.aFirstGen$FirstGen, employ_datQ21.7.aFirstGen)
prep <- analysisPrep(employ_datQ21.7.aFirstGen$CatOutcome, employ_datQ21.7.aFirstGen$FirstGen, employ_datQ21.7.aFirstGen)
analysis <- polr(employ_datQ21.7.aFirstGen$CatOutcome ~ employ_datQ21.7.aFirstGen$FirstGen, data=employ_datQ21.7.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.7.aGender<-multidatClean(Q21.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aGender$Gender<-factor(employ_datQ21.7.aGender$Gender)
employ_datQ21.7.aGender<-ordinaldatClean(employ_datQ21.7.aGender$Q21.7.a, employ_datQ21.7.aGender)
#ordinal(employ_datQ21.7.aGender$CatOutcome, employ_datQ21.7.aGender$Gender, employ_datQ21.7.aGender)
prep <- analysisPrep(employ_datQ21.7.aGender$CatOutcome, employ_datQ21.7.aGender$Gender, employ_datQ21.7.aGender)
analysis <- polr(employ_datQ21.7.aGender$CatOutcome ~ employ_datQ21.7.aGender$Gender, data=employ_datQ21.7.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.7.aSexuality<-multidatClean(Q21.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.7.aSexuality$Sexuality<-factor(employ_datQ21.7.aSexuality$Sexuality)
employ_datQ21.7.aSexuality<-ordinaldatClean(employ_datQ21.7.aSexuality$Q21.7.a, employ_datQ21.7.aSexuality)
#ordinal(employ_datQ21.7.aSexuality$CatOutcome, employ_datQ21.7.aSexuality$Sexuality, employ_datQ21.7.aSexuality)
prep <- analysisPrep(employ_datQ21.7.aSexuality$CatOutcome, employ_datQ21.7.aSexuality$Sexuality, employ_datQ21.7.aSexuality)
analysis <- polr(employ_datQ21.7.aSexuality$CatOutcome ~ employ_datQ21.7.aSexuality$Sexuality, data=employ_datQ21.7.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.8.a<-multidatClean(Q21.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.8.a + Academic, data = employ_datQ21.8.a)
detach(dat_long)
employ_datQ21.8.a<-ordinaldatClean(employ_datQ21.8.a$Q21.8.a, employ_datQ21.8.a)
#ordinal(employ_datQ21.8.a$CatOutcome, employ_datQ21.8.a$Academic, employ_datQ21.8.a)
prep <- analysisPrep(employ_datQ21.8.a$CatOutcome, employ_datQ21.8.a$Academic, employ_datQ21.8.a)
analysis <- polr(employ_datQ21.8.a$CatOutcome ~ employ_datQ21.8.a$Academic, data=employ_datQ21.8.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.8.aCollege<-multidatClean(Q21.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.8.a + Q3, data = employ_datQ21.8.aCollege)
conTable
detach(dat_long)
employ_datQ21.8.aCollege$Q3[(employ_datQ21.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.8.aCollege$Q3<- factor(employ_datQ21.8.aCollege$Q3)
employ_datQ21.8.aCollege<-ordinaldatClean(employ_datQ21.8.aCollege$Q21.8.a, employ_datQ21.8.aCollege)
#ordinal(employ_datQ21.8.aCollege$CatOutcome, employ_datQ21.8.aCollege$Q3, employ_datQ21.8.aCollege)
prep <- analysisPrep(employ_datQ21.8.aCollege$CatOutcome, employ_datQ21.8.aCollege$Q3, employ_datQ21.8.aCollege)
analysis <- polr(employ_datQ21.8.aCollege$CatOutcome ~ employ_datQ21.8.aCollege$Q3, data=employ_datQ21.8.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.8.aCarer<-multidatClean(Q21.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aCarer$Carer<- factor(employ_datQ21.8.aCarer$Carer)
employ_datQ21.8.aCarer<-ordinaldatClean(employ_datQ21.8.aCarer$Q21.8.a, employ_datQ21.8.aCarer)
#ordinal(employ_datQ21.8.aCarer$CatOutcome, employ_datQ21.8.aCarer$Carer, employ_datQ21.8.aCarer)
prep <- analysisPrep(employ_datQ21.8.aCarer$CatOutcome, employ_datQ21.8.aCarer$Carer, employ_datQ21.8.aCarer)
analysis <- polr(employ_datQ21.8.aCarer$CatOutcome ~ employ_datQ21.8.aCarer$Carer, data=employ_datQ21.8.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.8.aDisability<-multidatClean(Q21.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aDisability$Disability<- factor(employ_datQ21.8.aDisability$Disability)
employ_datQ21.8.aDisability<-ordinaldatClean(employ_datQ21.8.aDisability$Q21.8.a, employ_datQ21.8.aDisability)
conTable <- xtabs(~Q21.8.a + Disability, data = employ_datQ21.8.aDisability)
#ordinal(employ_datQ21.8.aDisability$CatOutcome, employ_datQ21.8.aDisability$Disability, employ_datQ21.8.aDisability)
prep <- analysisPrep(employ_datQ21.8.aDisability$CatOutcome, employ_datQ21.8.aDisability$Disability, employ_datQ21.8.aDisability)
analysis <- polr(employ_datQ21.8.aDisability$CatOutcome ~ employ_datQ21.8.aDisability$Disability, data=employ_datQ21.8.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.8.aEthnicity<-multidatClean(Q21.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aEthnicity$Ethnicity<- factor(employ_datQ21.8.aEthnicity$EthnicityCleaned)
employ_datQ21.8.aEthnicity<-ordinaldatClean(employ_datQ21.8.aEthnicity$Q21.8.a, employ_datQ21.8.aEthnicity)
conTable <- xtabs(~Q21.8.a + EthnicityCleaned, data = employ_datQ21.8.aEthnicity)
conTable
#ordinal(employ_datQ21.8.aEthnicity$CatOutcome, employ_datQ21.8.aEthnicity$EthnicityCleaned, employ_datQ21.8.aEthnicity)
prep <- analysisPrep(employ_datQ21.8.aEthnicity$CatOutcome, employ_datQ21.8.aEthnicity$EthnicityCleaned, employ_datQ21.8.aEthnicity)
analysis <- polr(employ_datQ21.8.aEthnicity$CatOutcome ~ employ_datQ21.8.aEthnicity$EthnicityCleaned, data=employ_datQ21.8.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.8.aFirstGen<-multidatClean(Q21.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aFirstGen$FirstGen<-factor(employ_datQ21.8.aFirstGen$FirstGen)
employ_datQ21.8.aFirstGen<-ordinaldatClean(employ_datQ21.8.aFirstGen$Q21.8.a, employ_datQ21.8.aFirstGen)
#ordinal(employ_datQ21.8.aFirstGen$CatOutcome, employ_datQ21.8.aFirstGen$FirstGen, employ_datQ21.8.aFirstGen)
prep <- analysisPrep(employ_datQ21.8.aFirstGen$CatOutcome, employ_datQ21.8.aFirstGen$FirstGen, employ_datQ21.8.aFirstGen)
analysis <- polr(employ_datQ21.8.aFirstGen$CatOutcome ~ employ_datQ21.8.aFirstGen$FirstGen, data=employ_datQ21.8.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.8.aGender<-multidatClean(Q21.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aGender$Gender<-factor(employ_datQ21.8.aGender$Gender)
employ_datQ21.8.aGender<-ordinaldatClean(employ_datQ21.8.aGender$Q21.8.a, employ_datQ21.8.aGender)
#ordinal(employ_datQ21.8.aGender$CatOutcome, employ_datQ21.8.aGender$Gender, employ_datQ21.8.aGender)
prep <- analysisPrep(employ_datQ21.8.aGender$CatOutcome, employ_datQ21.8.aGender$Gender, employ_datQ21.8.aGender)
analysis <- polr(employ_datQ21.8.aGender$CatOutcome ~ employ_datQ21.8.aGender$Gender, data=employ_datQ21.8.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.8.aSexuality<-multidatClean(Q21.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.8.aSexuality$Sexuality<-factor(employ_datQ21.8.aSexuality$Sexuality)
employ_datQ21.8.aSexuality<-ordinaldatClean(employ_datQ21.8.aSexuality$Q21.8.a, employ_datQ21.8.aSexuality)
#ordinal(employ_datQ21.8.aSexuality$CatOutcome, employ_datQ21.8.aSexuality$Sexuality, employ_datQ21.8.aSexuality)
prep <- analysisPrep(employ_datQ21.8.aSexuality$CatOutcome, employ_datQ21.8.aSexuality$Sexuality, employ_datQ21.8.aSexuality)
analysis <- polr(employ_datQ21.8.aSexuality$CatOutcome ~ employ_datQ21.8.aSexuality$Sexuality, data=employ_datQ21.8.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.8.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.9.a<-multidatClean(Q21.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.9.a + Academic, data = employ_datQ21.9.a)
detach(dat_long)
employ_datQ21.9.a<-ordinaldatClean(employ_datQ21.9.a$Q21.9.a, employ_datQ21.9.a)
#ordinal(employ_datQ21.9.a$CatOutcome, employ_datQ21.9.a$Academic, employ_datQ21.9.a)
prep <- analysisPrep(employ_datQ21.9.a$CatOutcome, employ_datQ21.9.a$Academic, employ_datQ21.9.a)
analysis <- polr(employ_datQ21.9.a$CatOutcome ~ employ_datQ21.9.a$Academic, data=employ_datQ21.9.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.9.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.9.aCollege<-multidatClean(Q21.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.9.a + Q3, data = employ_datQ21.9.aCollege)
conTable
detach(dat_long)
employ_datQ21.9.aCollege$Q3[(employ_datQ21.9.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.9.aCollege$Q3<- factor(employ_datQ21.9.aCollege$Q3)
employ_datQ21.9.aCollege<-ordinaldatClean(employ_datQ21.9.aCollege$Q21.9.a, employ_datQ21.9.aCollege)
#ordinal(employ_datQ21.9.aCollege$CatOutcome, employ_datQ21.9.aCollege$Q3, employ_datQ21.9.aCollege)
prep <- analysisPrep(employ_datQ21.9.aCollege$CatOutcome, employ_datQ21.9.aCollege$Q3, employ_datQ21.9.aCollege)
analysis <- polr(employ_datQ21.9.aCollege$CatOutcome ~ employ_datQ21.9.aCollege$Q3, data=employ_datQ21.9.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.9.aCarer<-multidatClean(Q21.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aCarer$Carer<- factor(employ_datQ21.9.aCarer$Carer)
employ_datQ21.9.aCarer<-ordinaldatClean(employ_datQ21.9.aCarer$Q21.9.a, employ_datQ21.9.aCarer)
#ordinal(employ_datQ21.9.aCarer$CatOutcome, employ_datQ21.9.aCarer$Carer, employ_datQ21.9.aCarer)
prep <- analysisPrep(employ_datQ21.9.aCarer$CatOutcome, employ_datQ21.9.aCarer$Carer, employ_datQ21.9.aCarer)
analysis <- polr(employ_datQ21.9.aCarer$CatOutcome ~ employ_datQ21.9.aCarer$Carer, data=employ_datQ21.9.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.9.aDisability<-multidatClean(Q21.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aDisability$Disability<- factor(employ_datQ21.9.aDisability$Disability)
employ_datQ21.9.aDisability<-ordinaldatClean(employ_datQ21.9.aDisability$Q21.9.a, employ_datQ21.9.aDisability)
conTable <- xtabs(~Q21.9.a + Disability, data = employ_datQ21.9.aDisability)
#ordinal(employ_datQ21.9.aDisability$CatOutcome, employ_datQ21.9.aDisability$Disability, employ_datQ21.9.aDisability)
prep <- analysisPrep(employ_datQ21.9.aDisability$CatOutcome, employ_datQ21.9.aDisability$Disability, employ_datQ21.9.aDisability)
analysis <- polr(employ_datQ21.9.aDisability$CatOutcome ~ employ_datQ21.9.aDisability$Disability, data=employ_datQ21.9.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.9.aEthnicity<-multidatClean(Q21.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aEthnicity$Ethnicity<- factor(employ_datQ21.9.aEthnicity$EthnicityCleaned)
employ_datQ21.9.aEthnicity<-ordinaldatClean(employ_datQ21.9.aEthnicity$Q21.9.a, employ_datQ21.9.aEthnicity)
conTable <- xtabs(~Q21.9.a + EthnicityCleaned, data = employ_datQ21.9.aEthnicity)
conTable
#ordinal(employ_datQ21.9.aEthnicity$CatOutcome, employ_datQ21.9.aEthnicity$EthnicityCleaned, employ_datQ21.9.aEthnicity)
prep <- analysisPrep(employ_datQ21.9.aEthnicity$CatOutcome, employ_datQ21.9.aEthnicity$EthnicityCleaned, employ_datQ21.9.aEthnicity)
analysis <- polr(employ_datQ21.9.aEthnicity$CatOutcome ~ employ_datQ21.9.aEthnicity$EthnicityCleaned, data=employ_datQ21.9.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.9.aFirstGen<-multidatClean(Q21.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aFirstGen$FirstGen<-factor(employ_datQ21.9.aFirstGen$FirstGen)
employ_datQ21.9.aFirstGen<-ordinaldatClean(employ_datQ21.9.aFirstGen$Q21.9.a, employ_datQ21.9.aFirstGen)
#ordinal(employ_datQ21.9.aFirstGen$CatOutcome, employ_datQ21.9.aFirstGen$FirstGen, employ_datQ21.9.aFirstGen)
prep <- analysisPrep(employ_datQ21.9.aFirstGen$CatOutcome, employ_datQ21.9.aFirstGen$FirstGen, employ_datQ21.9.aFirstGen)
analysis <- polr(employ_datQ21.9.aFirstGen$CatOutcome ~ employ_datQ21.9.aFirstGen$FirstGen, data=employ_datQ21.9.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.9.aGender<-multidatClean(Q21.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aGender$Gender<-factor(employ_datQ21.9.aGender$Gender)
employ_datQ21.9.aGender<-ordinaldatClean(employ_datQ21.9.aGender$Q21.9.a, employ_datQ21.9.aGender)
#ordinal(employ_datQ21.9.aGender$CatOutcome, employ_datQ21.9.aGender$Gender, employ_datQ21.9.aGender)
prep <- analysisPrep(employ_datQ21.9.aGender$CatOutcome, employ_datQ21.9.aGender$Gender, employ_datQ21.9.aGender)
analysis <- polr(employ_datQ21.9.aGender$CatOutcome ~ employ_datQ21.9.aGender$Gender, data=employ_datQ21.9.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.9.aSexuality<-multidatClean(Q21.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.9.aSexuality$Sexuality<-factor(employ_datQ21.9.aSexuality$Sexuality)
employ_datQ21.9.aSexuality<-ordinaldatClean(employ_datQ21.9.aSexuality$Q21.9.a, employ_datQ21.9.aSexuality)
#ordinal(employ_datQ21.9.aSexuality$CatOutcome, employ_datQ21.9.aSexuality$Sexuality, employ_datQ21.9.aSexuality)
prep <- analysisPrep(employ_datQ21.9.aSexuality$CatOutcome, employ_datQ21.9.aSexuality$Sexuality, employ_datQ21.9.aSexuality)
analysis <- polr(employ_datQ21.9.aSexuality$CatOutcome ~ employ_datQ21.9.aSexuality$Sexuality, data=employ_datQ21.9.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.9.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.9.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.10.a<-multidatClean(Q21.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.10.a + Academic, data = employ_datQ21.10.a)
detach(dat_long)
employ_datQ21.10.a<-ordinaldatClean(employ_datQ21.10.a$Q21.10.a, employ_datQ21.10.a)
#ordinal(employ_datQ21.10.a$CatOutcome, employ_datQ21.10.a$Academic, employ_datQ21.10.a)
prep <- analysisPrep(employ_datQ21.10.a$CatOutcome, employ_datQ21.10.a$Academic, employ_datQ21.10.a)
analysis <- polr(employ_datQ21.10.a$CatOutcome ~ employ_datQ21.10.a$Academic, data=employ_datQ21.10.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.10.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.10.aCollege<-multidatClean(Q21.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.10.a + Q3, data = employ_datQ21.10.aCollege)
conTable
detach(dat_long)
employ_datQ21.10.aCollege$Q3[(employ_datQ21.10.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.10.aCollege$Q3<- factor(employ_datQ21.10.aCollege$Q3)
employ_datQ21.10.aCollege<-ordinaldatClean(employ_datQ21.10.aCollege$Q21.10.a, employ_datQ21.10.aCollege)
#ordinal(employ_datQ21.10.aCollege$CatOutcome, employ_datQ21.10.aCollege$Q3, employ_datQ21.10.aCollege)
prep <- analysisPrep(employ_datQ21.10.aCollege$CatOutcome, employ_datQ21.10.aCollege$Q3, employ_datQ21.10.aCollege)
analysis <- polr(employ_datQ21.10.aCollege$CatOutcome ~ employ_datQ21.10.aCollege$Q3, data=employ_datQ21.10.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.10.aCarer<-multidatClean(Q21.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aCarer$Carer<- factor(employ_datQ21.10.aCarer$Carer)
employ_datQ21.10.aCarer<-ordinaldatClean(employ_datQ21.10.aCarer$Q21.10.a, employ_datQ21.10.aCarer)
#ordinal(employ_datQ21.10.aCarer$CatOutcome, employ_datQ21.10.aCarer$Carer, employ_datQ21.10.aCarer)
prep <- analysisPrep(employ_datQ21.10.aCarer$CatOutcome, employ_datQ21.10.aCarer$Carer, employ_datQ21.10.aCarer)
analysis <- polr(employ_datQ21.10.aCarer$CatOutcome ~ employ_datQ21.10.aCarer$Carer, data=employ_datQ21.10.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.10.aDisability<-multidatClean(Q21.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aDisability$Disability<- factor(employ_datQ21.10.aDisability$Disability)
employ_datQ21.10.aDisability<-ordinaldatClean(employ_datQ21.10.aDisability$Q21.10.a, employ_datQ21.10.aDisability)
conTable <- xtabs(~Q21.10.a + Disability, data = employ_datQ21.10.aDisability)
#ordinal(employ_datQ21.10.aDisability$CatOutcome, employ_datQ21.10.aDisability$Disability, employ_datQ21.10.aDisability)
prep <- analysisPrep(employ_datQ21.10.aDisability$CatOutcome, employ_datQ21.10.aDisability$Disability, employ_datQ21.10.aDisability)
analysis <- polr(employ_datQ21.10.aDisability$CatOutcome ~ employ_datQ21.10.aDisability$Disability, data=employ_datQ21.10.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.10.aEthnicity<-multidatClean(Q21.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aEthnicity$Ethnicity<- factor(employ_datQ21.10.aEthnicity$EthnicityCleaned)
employ_datQ21.10.aEthnicity<-ordinaldatClean(employ_datQ21.10.aEthnicity$Q21.10.a, employ_datQ21.10.aEthnicity)
conTable <- xtabs(~Q21.10.a + EthnicityCleaned, data = employ_datQ21.10.aEthnicity)
conTable
#ordinal(employ_datQ21.10.aEthnicity$CatOutcome, employ_datQ21.10.aEthnicity$EthnicityCleaned, employ_datQ21.10.aEthnicity)
prep <- analysisPrep(employ_datQ21.10.aEthnicity$CatOutcome, employ_datQ21.10.aEthnicity$EthnicityCleaned, employ_datQ21.10.aEthnicity)
analysis <- polr(employ_datQ21.10.aEthnicity$CatOutcome ~ employ_datQ21.10.aEthnicity$EthnicityCleaned, data=employ_datQ21.10.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.10.aFirstGen<-multidatClean(Q21.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aFirstGen$FirstGen<-factor(employ_datQ21.10.aFirstGen$FirstGen)
employ_datQ21.10.aFirstGen<-ordinaldatClean(employ_datQ21.10.aFirstGen$Q21.10.a, employ_datQ21.10.aFirstGen)
#ordinal(employ_datQ21.10.aFirstGen$CatOutcome, employ_datQ21.10.aFirstGen$FirstGen, employ_datQ21.10.aFirstGen)
prep <- analysisPrep(employ_datQ21.10.aFirstGen$CatOutcome, employ_datQ21.10.aFirstGen$FirstGen, employ_datQ21.10.aFirstGen)
analysis <- polr(employ_datQ21.10.aFirstGen$CatOutcome ~ employ_datQ21.10.aFirstGen$FirstGen, data=employ_datQ21.10.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.10.aGender<-multidatClean(Q21.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aGender$Gender<-factor(employ_datQ21.10.aGender$Gender)
employ_datQ21.10.aGender<-ordinaldatClean(employ_datQ21.10.aGender$Q21.10.a, employ_datQ21.10.aGender)
#ordinal(employ_datQ21.10.aGender$CatOutcome, employ_datQ21.10.aGender$Gender, employ_datQ21.10.aGender)
prep <- analysisPrep(employ_datQ21.10.aGender$CatOutcome, employ_datQ21.10.aGender$Gender, employ_datQ21.10.aGender)
analysis <- polr(employ_datQ21.10.aGender$CatOutcome ~ employ_datQ21.10.aGender$Gender, data=employ_datQ21.10.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.10.aSexuality<-multidatClean(Q21.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.10.aSexuality$Sexuality<-factor(employ_datQ21.10.aSexuality$Sexuality)
employ_datQ21.10.aSexuality<-ordinaldatClean(employ_datQ21.10.aSexuality$Q21.10.a, employ_datQ21.10.aSexuality)
#ordinal(employ_datQ21.10.aSexuality$CatOutcome, employ_datQ21.10.aSexuality$Sexuality, employ_datQ21.10.aSexuality)
prep <- analysisPrep(employ_datQ21.10.aSexuality$CatOutcome, employ_datQ21.10.aSexuality$Sexuality, employ_datQ21.10.aSexuality)
analysis <- polr(employ_datQ21.10.aSexuality$CatOutcome ~ employ_datQ21.10.aSexuality$Sexuality, data=employ_datQ21.10.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.10.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.10.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ21.11.a<-multidatClean(Q21.11.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.11.a + Academic, data = employ_datQ21.11.a)
detach(dat_long)
employ_datQ21.11.a<-ordinaldatClean(employ_datQ21.11.a$Q21.11.a, employ_datQ21.11.a)
#ordinal(employ_datQ21.11.a$CatOutcome, employ_datQ21.11.a$Academic, employ_datQ21.11.a)
prep <- analysisPrep(employ_datQ21.11.a$CatOutcome, employ_datQ21.11.a$Academic, employ_datQ21.11.a)
analysis <- polr(employ_datQ21.11.a$CatOutcome ~ employ_datQ21.11.a$Academic, data=employ_datQ21.11.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.11.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ21.11.aCollege<-multidatClean(Q21.11.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q21.11.a + Q3, data = employ_datQ21.11.aCollege)
conTable
detach(dat_long)
employ_datQ21.11.aCollege$Q3[(employ_datQ21.11.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ21.11.aCollege$Q3<- factor(employ_datQ21.11.aCollege$Q3)
employ_datQ21.11.aCollege<-ordinaldatClean(employ_datQ21.11.aCollege$Q21.11.a, employ_datQ21.11.aCollege)
#ordinal(employ_datQ21.11.aCollege$CatOutcome, employ_datQ21.11.aCollege$Q3, employ_datQ21.11.aCollege)
prep <- analysisPrep(employ_datQ21.11.aCollege$CatOutcome, employ_datQ21.11.aCollege$Q3, employ_datQ21.11.aCollege)
analysis <- polr(employ_datQ21.11.aCollege$CatOutcome ~ employ_datQ21.11.aCollege$Q3, data=employ_datQ21.11.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ21.11.aCarer<-multidatClean(Q21.11.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aCarer$Carer<- factor(employ_datQ21.11.aCarer$Carer)
employ_datQ21.11.aCarer<-ordinaldatClean(employ_datQ21.11.aCarer$Q21.11.a, employ_datQ21.11.aCarer)
#ordinal(employ_datQ21.11.aCarer$CatOutcome, employ_datQ21.11.aCarer$Carer, employ_datQ21.11.aCarer)
prep <- analysisPrep(employ_datQ21.11.aCarer$CatOutcome, employ_datQ21.11.aCarer$Carer, employ_datQ21.11.aCarer)
analysis <- polr(employ_datQ21.11.aCarer$CatOutcome ~ employ_datQ21.11.aCarer$Carer, data=employ_datQ21.11.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ21.11.aDisability<-multidatClean(Q21.11.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aDisability$Disability<- factor(employ_datQ21.11.aDisability$Disability)
employ_datQ21.11.aDisability<-ordinaldatClean(employ_datQ21.11.aDisability$Q21.11.a, employ_datQ21.11.aDisability)
conTable <- xtabs(~Q21.11.a + Disability, data = employ_datQ21.11.aDisability)
#ordinal(employ_datQ21.11.aDisability$CatOutcome, employ_datQ21.11.aDisability$Disability, employ_datQ21.11.aDisability)
prep <- analysisPrep(employ_datQ21.11.aDisability$CatOutcome, employ_datQ21.11.aDisability$Disability, employ_datQ21.11.aDisability)
analysis <- polr(employ_datQ21.11.aDisability$CatOutcome ~ employ_datQ21.11.aDisability$Disability, data=employ_datQ21.11.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ21.11.aEthnicity<-multidatClean(Q21.11.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aEthnicity$Ethnicity<- factor(employ_datQ21.11.aEthnicity$EthnicityCleaned)
employ_datQ21.11.aEthnicity<-ordinaldatClean(employ_datQ21.11.aEthnicity$Q21.11.a, employ_datQ21.11.aEthnicity)
conTable <- xtabs(~Q21.11.a + EthnicityCleaned, data = employ_datQ21.11.aEthnicity)
conTable
#ordinal(employ_datQ21.11.aEthnicity$CatOutcome, employ_datQ21.11.aEthnicity$EthnicityCleaned, employ_datQ21.11.aEthnicity)
prep <- analysisPrep(employ_datQ21.11.aEthnicity$CatOutcome, employ_datQ21.11.aEthnicity$EthnicityCleaned, employ_datQ21.11.aEthnicity)
analysis <- polr(employ_datQ21.11.aEthnicity$CatOutcome ~ employ_datQ21.11.aEthnicity$EthnicityCleaned, data=employ_datQ21.11.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ21.11.aFirstGen<-multidatClean(Q21.11.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aFirstGen$FirstGen<-factor(employ_datQ21.11.aFirstGen$FirstGen)
employ_datQ21.11.aFirstGen<-ordinaldatClean(employ_datQ21.11.aFirstGen$Q21.11.a, employ_datQ21.11.aFirstGen)
#ordinal(employ_datQ21.11.aFirstGen$CatOutcome, employ_datQ21.11.aFirstGen$FirstGen, employ_datQ21.11.aFirstGen)
prep <- analysisPrep(employ_datQ21.11.aFirstGen$CatOutcome, employ_datQ21.11.aFirstGen$FirstGen, employ_datQ21.11.aFirstGen)
analysis <- polr(employ_datQ21.11.aFirstGen$CatOutcome ~ employ_datQ21.11.aFirstGen$FirstGen, data=employ_datQ21.11.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ21.11.aGender<-multidatClean(Q21.11.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aGender$Gender<-factor(employ_datQ21.11.aGender$Gender)
employ_datQ21.11.aGender<-ordinaldatClean(employ_datQ21.11.aGender$Q21.11.a, employ_datQ21.11.aGender)
#ordinal(employ_datQ21.11.aGender$CatOutcome, employ_datQ21.11.aGender$Gender, employ_datQ21.11.aGender)
prep <- analysisPrep(employ_datQ21.11.aGender$CatOutcome, employ_datQ21.11.aGender$Gender, employ_datQ21.11.aGender)
analysis <- polr(employ_datQ21.11.aGender$CatOutcome ~ employ_datQ21.11.aGender$Gender, data=employ_datQ21.11.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ21.11.aSexuality<-multidatClean(Q21.11.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ21.11.aSexuality$Sexuality<-factor(employ_datQ21.11.aSexuality$Sexuality)
employ_datQ21.11.aSexuality<-ordinaldatClean(employ_datQ21.11.aSexuality$Q21.11.a, employ_datQ21.11.aSexuality)
#ordinal(employ_datQ21.11.aSexuality$CatOutcome, employ_datQ21.11.aSexuality$Sexuality, employ_datQ21.11.aSexuality)
prep <- analysisPrep(employ_datQ21.11.aSexuality$CatOutcome, employ_datQ21.11.aSexuality$Sexuality, employ_datQ21.11.aSexuality)
analysis <- polr(employ_datQ21.11.aSexuality$CatOutcome ~ employ_datQ21.11.aSexuality$Sexuality, data=employ_datQ21.11.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q21.11.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q21.11.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

employ_datQ22<-multidatClean(Q22, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q22 + Academic, data = employ_datQ22)
detach(dat_long)
employ_datQ22<-SatisfacordinaldatClean(employ_datQ22$Q22, employ_datQ22)
#ordinal(employ_datQ22$CatOutcome, employ_datQ22$Academic, employ_datQ22)
prep <- analysisPrep(employ_datQ22$CatOutcome, employ_datQ22$Academic, employ_datQ22)
analysis <- polr(employ_datQ22$CatOutcome ~ employ_datQ22$Academic, data=employ_datQ22, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q22"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ22College<-multidatClean(Q22, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q22 + Q3, data = employ_datQ22College)
conTable
detach(dat_long)
employ_datQ22College$Q3[(employ_datQ22College$Q3 == "Research Professional Staff")]="Other"
employ_datQ22College$Q3<- factor(employ_datQ22College$Q3)
employ_datQ22College<-SatisfacordinaldatClean(employ_datQ22College$Q22, employ_datQ22College)
#ordinal(employ_datQ22College$CatOutcome, employ_datQ22College$Q3, employ_datQ22College)
prep <- analysisPrep(employ_datQ22College$CatOutcome, employ_datQ22College$Q3, employ_datQ22College)
analysis <- polr(employ_datQ22College$CatOutcome ~ employ_datQ22College$Q3, data=employ_datQ22College, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ22Carer<-multidatClean(Q22, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22Carer$Carer<- factor(employ_datQ22Carer$Carer)
employ_datQ22Carer<-SatisfacordinaldatClean(employ_datQ22Carer$Q22, employ_datQ22Carer)
#ordinal(employ_datQ22Carer$CatOutcome, employ_datQ22Carer$Carer, employ_datQ22Carer)
prep <- analysisPrep(employ_datQ22Carer$CatOutcome, employ_datQ22Carer$Carer, employ_datQ22Carer)
analysis <- polr(employ_datQ22Carer$CatOutcome ~ employ_datQ22Carer$Carer, data=employ_datQ22Carer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ22Disability<-multidatClean(Q22, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22Disability$Disability<- factor(employ_datQ22Disability$Disability)
employ_datQ22Disability<-SatisfacordinaldatClean(employ_datQ22Disability$Q22, employ_datQ22Disability)
conTable <- xtabs(~Q22 + Disability, data = employ_datQ22Disability)
#ordinal(employ_datQ22Disability$CatOutcome, employ_datQ22Disability$Disability, employ_datQ22Disability)
prep <- analysisPrep(employ_datQ22Disability$CatOutcome, employ_datQ22Disability$Disability, employ_datQ22Disability)
analysis <- polr(employ_datQ22Disability$CatOutcome ~ employ_datQ22Disability$Disability, data=employ_datQ22Disability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ22Ethnicity<-multidatClean(Q22, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22Ethnicity$Ethnicity<- factor(employ_datQ22Ethnicity$EthnicityCleaned)
employ_datQ22Ethnicity<-SatisfacordinaldatClean(employ_datQ22Ethnicity$Q22, employ_datQ22Ethnicity)
conTable <- xtabs(~Q22 + EthnicityCleaned, data = employ_datQ22Ethnicity)
conTable
#ordinal(employ_datQ22Ethnicity$CatOutcome, employ_datQ22Ethnicity$EthnicityCleaned, employ_datQ22Ethnicity)
prep <- analysisPrep(employ_datQ22Ethnicity$CatOutcome, employ_datQ22Ethnicity$EthnicityCleaned, employ_datQ22Ethnicity)
analysis <- polr(employ_datQ22Ethnicity$CatOutcome ~ employ_datQ22Ethnicity$EthnicityCleaned, data=employ_datQ22Ethnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ22FirstGen<-multidatClean(Q22, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22FirstGen$FirstGen<-factor(employ_datQ22FirstGen$FirstGen)
employ_datQ22FirstGen<-SatisfacordinaldatClean(employ_datQ22FirstGen$Q22, employ_datQ22FirstGen)
#ordinal(employ_datQ22FirstGen$CatOutcome, employ_datQ22FirstGen$FirstGen, employ_datQ22FirstGen)
prep <- analysisPrep(employ_datQ22FirstGen$CatOutcome, employ_datQ22FirstGen$FirstGen, employ_datQ22FirstGen)
analysis <- polr(employ_datQ22FirstGen$CatOutcome ~ employ_datQ22FirstGen$FirstGen, data=employ_datQ22FirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ22Gender<-multidatClean(Q22, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22Gender$Gender<-factor(employ_datQ22Gender$Gender)
employ_datQ22Gender<-SatisfacordinaldatClean(employ_datQ22Gender$Q22, employ_datQ22Gender)
#ordinal(employ_datQ22Gender$CatOutcome, employ_datQ22Gender$Gender, employ_datQ22Gender)
prep <- analysisPrep(employ_datQ22Gender$CatOutcome, employ_datQ22Gender$Gender, employ_datQ22Gender)
analysis <- polr(employ_datQ22Gender$CatOutcome ~ employ_datQ22Gender$Gender, data=employ_datQ22Gender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ22Sexuality<-multidatClean(Q22, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ22Sexuality$Sexuality<-factor(employ_datQ22Sexuality$Sexuality)
employ_datQ22Sexuality<-SatisfacordinaldatClean(employ_datQ22Sexuality$Q22, employ_datQ22Sexuality)
#ordinal(employ_datQ22Sexuality$CatOutcome, employ_datQ22Sexuality$Sexuality, employ_datQ22Sexuality)
prep <- analysisPrep(employ_datQ22Sexuality$CatOutcome, employ_datQ22Sexuality$Sexuality, employ_datQ22Sexuality)
analysis <- polr(employ_datQ22Sexuality$CatOutcome ~ employ_datQ22Sexuality$Sexuality, data=employ_datQ22Sexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q22"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q22_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

