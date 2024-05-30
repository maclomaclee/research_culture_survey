source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239,184:222,6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)

### Q47 - To what extent has your research activity been affected by the COVID pandemic? Not at all to stopped
"Q47.1 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ47.1<-multidatClean(Q47.1, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q47.1 + Academic, data = employ_datQ47.1)
detach(dat_long)
employ_datQ47.1<-ordinaldatClean47(employ_datQ47.1$Q47.1, employ_datQ47.1)
#ordinal(employ_datQ47.1$CatOutcome, employ_datQ47.1$Academic, employ_datQ47.1)
prep <- analysisPrep(employ_datQ47.1$CatOutcome, employ_datQ47.1$Academic, employ_datQ47.1)
analysis <- polr(employ_datQ47.1$CatOutcome ~ employ_datQ47.1$Academic, data=employ_datQ47.1, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q47.1"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ47.1College<-multidatClean(Q47.1, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q47.1 + Q3, data = employ_datQ47.1College)
conTable
detach(dat_long)
employ_datQ47.1College$Q3[(employ_datQ47.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ47.1College$Q3<- factor(employ_datQ47.1College$Q3)
employ_datQ47.1College<-ordinaldatClean47(employ_datQ47.1College$Q47.1, employ_datQ47.1College)
#ordinal(employ_datQ47.1College$CatOutcome, employ_datQ47.1College$Q3, employ_datQ47.1College)
prep <- analysisPrep(employ_datQ47.1College$CatOutcome, employ_datQ47.1College$Q3, employ_datQ47.1College)
analysis <- polr(employ_datQ47.1College$CatOutcome ~ employ_datQ47.1College$Q3, data=employ_datQ47.1College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ47.1Carer<-multidatClean(Q47.1, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1Carer$Carer<- factor(employ_datQ47.1Carer$Carer)
employ_datQ47.1Carer<-ordinaldatClean47(employ_datQ47.1Carer$Q47.1, employ_datQ47.1Carer)
#ordinal(employ_datQ47.1Carer$CatOutcome, employ_datQ47.1Carer$Carer, employ_datQ47.1Carer)
prep <- analysisPrep(employ_datQ47.1Carer$CatOutcome, employ_datQ47.1Carer$Carer, employ_datQ47.1Carer)
analysis <- polr(employ_datQ47.1Carer$CatOutcome ~ employ_datQ47.1Carer$Carer, data=employ_datQ47.1Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ47.1Disability<-multidatClean(Q47.1, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1Disability$Disability<- factor(employ_datQ47.1Disability$Disability)
employ_datQ47.1Disability<-ordinaldatClean47(employ_datQ47.1Disability$Q47.1, employ_datQ47.1Disability)
conTable <- xtabs(~Q47.1 + Disability, data = employ_datQ47.1Disability)
#ordinal(employ_datQ47.1Disability$CatOutcome, employ_datQ47.1Disability$Disability, employ_datQ47.1Disability)
prep <- analysisPrep(employ_datQ47.1Disability$CatOutcome, employ_datQ47.1Disability$Disability, employ_datQ47.1Disability)
analysis <- polr(employ_datQ47.1Disability$CatOutcome ~ employ_datQ47.1Disability$Disability, data=employ_datQ47.1Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ47.1Ethnicity<-multidatClean(Q47.1, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1Ethnicity$Ethnicity<- factor(employ_datQ47.1Ethnicity$EthnicityCleaned)
employ_datQ47.1Ethnicity<-ordinaldatClean47(employ_datQ47.1Ethnicity$Q47.1, employ_datQ47.1Ethnicity)
conTable <- xtabs(~Q47.1 + EthnicityCleaned, data = employ_datQ47.1Ethnicity)
conTable
#ordinal(employ_datQ47.1Ethnicity$CatOutcome, employ_datQ47.1Ethnicity$EthnicityCleaned, employ_datQ47.1Ethnicity)
prep <- analysisPrep(employ_datQ47.1Ethnicity$CatOutcome, employ_datQ47.1Ethnicity$EthnicityCleaned, employ_datQ47.1Ethnicity)
analysis <- polr(employ_datQ47.1Ethnicity$CatOutcome ~ employ_datQ47.1Ethnicity$EthnicityCleaned, data=employ_datQ47.1Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ47.1FirstGen<-multidatClean(Q47.1, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1FirstGen$FirstGen<-factor(employ_datQ47.1FirstGen$FirstGen)
employ_datQ47.1FirstGen<-ordinaldatClean47(employ_datQ47.1FirstGen$Q47.1, employ_datQ47.1FirstGen)
#ordinal(employ_datQ47.1FirstGen$CatOutcome, employ_datQ47.1FirstGen$FirstGen, employ_datQ47.1FirstGen)
prep <- analysisPrep(employ_datQ47.1FirstGen$CatOutcome, employ_datQ47.1FirstGen$FirstGen, employ_datQ47.1FirstGen)
analysis <- polr(employ_datQ47.1FirstGen$CatOutcome ~ employ_datQ47.1FirstGen$FirstGen, data=employ_datQ47.1FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ47.1Gender<-multidatClean(Q47.1, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1Gender$Gender<-factor(employ_datQ47.1Gender$Gender)
employ_datQ47.1Gender<-ordinaldatClean47(employ_datQ47.1Gender$Q47.1, employ_datQ47.1Gender)
#ordinal(employ_datQ47.1Gender$CatOutcome, employ_datQ47.1Gender$Gender, employ_datQ47.1Gender)
prep <- analysisPrep(employ_datQ47.1Gender$CatOutcome, employ_datQ47.1Gender$Gender, employ_datQ47.1Gender)
analysis <- polr(employ_datQ47.1Gender$CatOutcome ~ employ_datQ47.1Gender$Gender, data=employ_datQ47.1Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ47.1Sexuality<-multidatClean(Q47.1, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ47.1Sexuality$Sexuality<-factor(employ_datQ47.1Sexuality$Sexuality)
employ_datQ47.1Sexuality<-ordinaldatClean47(employ_datQ47.1Sexuality$Q47.1, employ_datQ47.1Sexuality)
#ordinal(employ_datQ47.1Sexuality$CatOutcome, employ_datQ47.1Sexuality$Sexuality, employ_datQ47.1Sexuality)
prep <- analysisPrep(employ_datQ47.1Sexuality$CatOutcome, employ_datQ47.1Sexuality$Sexuality, employ_datQ47.1Sexuality)
analysis <- polr(employ_datQ47.1Sexuality$CatOutcome ~ employ_datQ47.1Sexuality$Sexuality, data=employ_datQ47.1Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q47.1"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q47.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q48.1.a - Have academic colleagues been helpful? 
"Q48.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ48.1<-multidatClean(Q48.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q48.1.a + Academic, data = employ_datQ48.1)
detach(dat_long)
employ_datQ48.1<-ordinaldatClean48(employ_datQ48.1$Q48.1.a, employ_datQ48.1)
#ordinal(employ_datQ48.1$CatOutcome, employ_datQ48.1$Academic, employ_datQ48.1)
prep <- analysisPrep(employ_datQ48.1$CatOutcome, employ_datQ48.1$Academic, employ_datQ48.1)
analysis <- polr(employ_datQ48.1$CatOutcome ~ employ_datQ48.1$Academic, data=employ_datQ48.1, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q48.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ48.1College<-multidatClean(Q48.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q48.1.a + Q3, data = employ_datQ48.1College)
conTable
detach(dat_long)
employ_datQ48.1College$Q3[(employ_datQ48.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ48.1College$Q3<- factor(employ_datQ48.1College$Q3)
employ_datQ48.1College<-ordinaldatClean48(employ_datQ48.1College$Q48.1.a, employ_datQ48.1College)
#ordinal(employ_datQ48.1College$CatOutcome, employ_datQ48.1College$Q3, employ_datQ48.1College)
prep <- analysisPrep(employ_datQ48.1College$CatOutcome, employ_datQ48.1College$Q3, employ_datQ48.1College)
analysis <- polr(employ_datQ48.1College$CatOutcome ~ employ_datQ48.1College$Q3, data=employ_datQ48.1College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ48.1Carer<-multidatClean(Q48.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1Carer$Carer<- factor(employ_datQ48.1Carer$Carer)
employ_datQ48.1Carer<-ordinaldatClean48(employ_datQ48.1Carer$Q48.1.a, employ_datQ48.1Carer)
#ordinal(employ_datQ48.1Carer$CatOutcome, employ_datQ48.1Carer$Carer, employ_datQ48.1Carer)
prep <- analysisPrep(employ_datQ48.1Carer$CatOutcome, employ_datQ48.1Carer$Carer, employ_datQ48.1Carer)
analysis <- polr(employ_datQ48.1Carer$CatOutcome ~ employ_datQ48.1Carer$Carer, data=employ_datQ48.1Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ48.1Disability<-multidatClean(Q48.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1Disability$Disability<- factor(employ_datQ48.1Disability$Disability)
employ_datQ48.1Disability<-ordinaldatClean48(employ_datQ48.1Disability$Q48.1.a, employ_datQ48.1Disability)
conTable <- xtabs(~Q48.1.a + Disability, data = employ_datQ48.1Disability)
#ordinal(employ_datQ48.1Disability$CatOutcome, employ_datQ48.1Disability$Disability, employ_datQ48.1Disability)
prep <- analysisPrep(employ_datQ48.1Disability$CatOutcome, employ_datQ48.1Disability$Disability, employ_datQ48.1Disability)
analysis <- polr(employ_datQ48.1Disability$CatOutcome ~ employ_datQ48.1Disability$Disability, data=employ_datQ48.1Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ48.1Ethnicity<-multidatClean(Q48.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1Ethnicity$Ethnicity<- factor(employ_datQ48.1Ethnicity$EthnicityCleaned)
employ_datQ48.1Ethnicity<-ordinaldatClean48(employ_datQ48.1Ethnicity$Q48.1.a, employ_datQ48.1Ethnicity)
conTable <- xtabs(~Q48.1.a + EthnicityCleaned, data = employ_datQ48.1Ethnicity)
conTable
#ordinal(employ_datQ48.1Ethnicity$CatOutcome, employ_datQ48.1Ethnicity$EthnicityCleaned, employ_datQ48.1Ethnicity)
prep <- analysisPrep(employ_datQ48.1Ethnicity$CatOutcome, employ_datQ48.1Ethnicity$EthnicityCleaned, employ_datQ48.1Ethnicity)
analysis <- polr(employ_datQ48.1Ethnicity$CatOutcome ~ employ_datQ48.1Ethnicity$EthnicityCleaned, data=employ_datQ48.1Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ48.1FirstGen<-multidatClean(Q48.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1FirstGen$FirstGen<-factor(employ_datQ48.1FirstGen$FirstGen)
employ_datQ48.1FirstGen<-ordinaldatClean48(employ_datQ48.1FirstGen$Q48.1.a, employ_datQ48.1FirstGen)
#ordinal(employ_datQ48.1FirstGen$CatOutcome, employ_datQ48.1FirstGen$FirstGen, employ_datQ48.1FirstGen)
prep <- analysisPrep(employ_datQ48.1FirstGen$CatOutcome, employ_datQ48.1FirstGen$FirstGen, employ_datQ48.1FirstGen)
analysis <- polr(employ_datQ48.1FirstGen$CatOutcome ~ employ_datQ48.1FirstGen$FirstGen, data=employ_datQ48.1FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ48.1Gender<-multidatClean(Q48.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1Gender$Gender<-factor(employ_datQ48.1Gender$Gender)
employ_datQ48.1Gender<-ordinaldatClean48(employ_datQ48.1Gender$Q48.1.a, employ_datQ48.1Gender)
#ordinal(employ_datQ48.1Gender$CatOutcome, employ_datQ48.1Gender$Gender, employ_datQ48.1Gender)
prep <- analysisPrep(employ_datQ48.1Gender$CatOutcome, employ_datQ48.1Gender$Gender, employ_datQ48.1Gender)
analysis <- polr(employ_datQ48.1Gender$CatOutcome ~ employ_datQ48.1Gender$Gender, data=employ_datQ48.1Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ48.1Sexuality<-multidatClean(Q48.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.1Sexuality$Sexuality<-factor(employ_datQ48.1Sexuality$Sexuality)
employ_datQ48.1Sexuality<-ordinaldatClean48(employ_datQ48.1Sexuality$Q48.1.a, employ_datQ48.1Sexuality)
#ordinal(employ_datQ48.1Sexuality$CatOutcome, employ_datQ48.1Sexuality$Sexuality, employ_datQ48.1Sexuality)
prep <- analysisPrep(employ_datQ48.1Sexuality$CatOutcome, employ_datQ48.1Sexuality$Sexuality, employ_datQ48.1Sexuality)
analysis <- polr(employ_datQ48.1Sexuality$CatOutcome ~ employ_datQ48.1Sexuality$Sexuality, data=employ_datQ48.1Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q48.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q48.2.a - Have non-academic colleagues been helpful? 
"Q48.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ48.2<-multidatClean(Q48.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q48.2.a + Academic, data = employ_datQ48.2)
detach(dat_long)
employ_datQ48.2<-ordinaldatClean48(employ_datQ48.2$Q48.2.a, employ_datQ48.2)
#ordinal(employ_datQ48.2$CatOutcome, employ_datQ48.2$Academic, employ_datQ48.2)
prep <- analysisPrep(employ_datQ48.2$CatOutcome, employ_datQ48.2$Academic, employ_datQ48.2)
analysis <- polr(employ_datQ48.2$CatOutcome ~ employ_datQ48.2$Academic, data=employ_datQ48.2, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q48.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ48.2College<-multidatClean(Q48.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q48.2.a + Q3, data = employ_datQ48.2College)
conTable
detach(dat_long)
employ_datQ48.2College$Q3[(employ_datQ48.2College$Q3 == "Research Professional Staff")]="Other"
employ_datQ48.2College$Q3<- factor(employ_datQ48.2College$Q3)
employ_datQ48.2College<-ordinaldatClean48(employ_datQ48.2College$Q48.2.a, employ_datQ48.2College)
#ordinal(employ_datQ48.2College$CatOutcome, employ_datQ48.2College$Q3, employ_datQ48.2College)
prep <- analysisPrep(employ_datQ48.2College$CatOutcome, employ_datQ48.2College$Q3, employ_datQ48.2College)
analysis <- polr(employ_datQ48.2College$CatOutcome ~ employ_datQ48.2College$Q3, data=employ_datQ48.2College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ48.2Carer<-multidatClean(Q48.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2Carer$Carer<- factor(employ_datQ48.2Carer$Carer)
employ_datQ48.2Carer<-ordinaldatClean48(employ_datQ48.2Carer$Q48.2.a, employ_datQ48.2Carer)
#ordinal(employ_datQ48.2Carer$CatOutcome, employ_datQ48.2Carer$Carer, employ_datQ48.2Carer)
prep <- analysisPrep(employ_datQ48.2Carer$CatOutcome, employ_datQ48.2Carer$Carer, employ_datQ48.2Carer)
analysis <- polr(employ_datQ48.2Carer$CatOutcome ~ employ_datQ48.2Carer$Carer, data=employ_datQ48.2Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ48.2Disability<-multidatClean(Q48.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2Disability$Disability<- factor(employ_datQ48.2Disability$Disability)
employ_datQ48.2Disability<-ordinaldatClean48(employ_datQ48.2Disability$Q48.2.a, employ_datQ48.2Disability)
conTable <- xtabs(~Q48.2.a + Disability, data = employ_datQ48.2Disability)
#ordinal(employ_datQ48.2Disability$CatOutcome, employ_datQ48.2Disability$Disability, employ_datQ48.2Disability)
prep <- analysisPrep(employ_datQ48.2Disability$CatOutcome, employ_datQ48.2Disability$Disability, employ_datQ48.2Disability)
analysis <- polr(employ_datQ48.2Disability$CatOutcome ~ employ_datQ48.2Disability$Disability, data=employ_datQ48.2Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ48.2Ethnicity<-multidatClean(Q48.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2Ethnicity$Ethnicity<- factor(employ_datQ48.2Ethnicity$EthnicityCleaned)
employ_datQ48.2Ethnicity<-ordinaldatClean48(employ_datQ48.2Ethnicity$Q48.2.a, employ_datQ48.2Ethnicity)
conTable <- xtabs(~Q48.2.a + EthnicityCleaned, data = employ_datQ48.2Ethnicity)
conTable
#ordinal(employ_datQ48.2Ethnicity$CatOutcome, employ_datQ48.2Ethnicity$EthnicityCleaned, employ_datQ48.2Ethnicity)
prep <- analysisPrep(employ_datQ48.2Ethnicity$CatOutcome, employ_datQ48.2Ethnicity$EthnicityCleaned, employ_datQ48.2Ethnicity)
analysis <- polr(employ_datQ48.2Ethnicity$CatOutcome ~ employ_datQ48.2Ethnicity$EthnicityCleaned, data=employ_datQ48.2Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ48.2FirstGen<-multidatClean(Q48.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2FirstGen$FirstGen<-factor(employ_datQ48.2FirstGen$FirstGen)
employ_datQ48.2FirstGen<-ordinaldatClean48(employ_datQ48.2FirstGen$Q48.2.a, employ_datQ48.2FirstGen)
#ordinal(employ_datQ48.2FirstGen$CatOutcome, employ_datQ48.2FirstGen$FirstGen, employ_datQ48.2FirstGen)
prep <- analysisPrep(employ_datQ48.2FirstGen$CatOutcome, employ_datQ48.2FirstGen$FirstGen, employ_datQ48.2FirstGen)
analysis <- polr(employ_datQ48.2FirstGen$CatOutcome ~ employ_datQ48.2FirstGen$FirstGen, data=employ_datQ48.2FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ48.2Gender<-multidatClean(Q48.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2Gender$Gender<-factor(employ_datQ48.2Gender$Gender)
employ_datQ48.2Gender<-ordinaldatClean48(employ_datQ48.2Gender$Q48.2.a, employ_datQ48.2Gender)
#ordinal(employ_datQ48.2Gender$CatOutcome, employ_datQ48.2Gender$Gender, employ_datQ48.2Gender)
prep <- analysisPrep(employ_datQ48.2Gender$CatOutcome, employ_datQ48.2Gender$Gender, employ_datQ48.2Gender)
analysis <- polr(employ_datQ48.2Gender$CatOutcome ~ employ_datQ48.2Gender$Gender, data=employ_datQ48.2Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ48.2Sexuality<-multidatClean(Q48.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ48.2Sexuality$Sexuality<-factor(employ_datQ48.2Sexuality$Sexuality)
employ_datQ48.2Sexuality<-ordinaldatClean48(employ_datQ48.2Sexuality$Q48.2.a, employ_datQ48.2Sexuality)
#ordinal(employ_datQ48.2Sexuality$CatOutcome, employ_datQ48.2Sexuality$Sexuality, employ_datQ48.2Sexuality)
prep <- analysisPrep(employ_datQ48.2Sexuality$CatOutcome, employ_datQ48.2Sexuality$Sexuality, employ_datQ48.2Sexuality)
analysis <- polr(employ_datQ48.2Sexuality$CatOutcome ~ employ_datQ48.2Sexuality$Sexuality, data=employ_datQ48.2Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q48.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q48.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q49 - What proportion of your research activity have you been able to continue during the "lockdown"?
"Q49 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ49<-multidatClean(Q49, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q49 + Academic, data = employ_datQ49)
detach(dat_long)
employ_datQ49<-ordinaldatClean49(employ_datQ49$Q49, employ_datQ49)
#ordinal(employ_datQ49$CatOutcome, employ_datQ49$Academic, employ_datQ49)
prep <- analysisPrep(employ_datQ49$CatOutcome, employ_datQ49$Academic, employ_datQ49)
analysis <- polr(employ_datQ49$CatOutcome ~ employ_datQ49$Academic, data=employ_datQ49, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q49"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ49College<-multidatClean(Q49, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q49 + Q3, data = employ_datQ49College)
conTable
detach(dat_long)
employ_datQ49College$Q3[(employ_datQ49College$Q3 == "Research Professional Staff")]="Other"
employ_datQ49College$Q3<- factor(employ_datQ49College$Q3)
employ_datQ49College<-ordinaldatClean49(employ_datQ49College$Q49, employ_datQ49College)
#ordinal(employ_datQ49College$CatOutcome, employ_datQ49College$Q3, employ_datQ49College)
prep <- analysisPrep(employ_datQ49College$CatOutcome, employ_datQ49College$Q3, employ_datQ49College)
analysis <- polr(employ_datQ49College$CatOutcome ~ employ_datQ49College$Q3, data=employ_datQ49College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ49Carer<-multidatClean(Q49, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49Carer$Carer<- factor(employ_datQ49Carer$Carer)
employ_datQ49Carer<-ordinaldatClean49(employ_datQ49Carer$Q49, employ_datQ49Carer)
#ordinal(employ_datQ49Carer$CatOutcome, employ_datQ49Carer$Carer, employ_datQ49Carer)
prep <- analysisPrep(employ_datQ49Carer$CatOutcome, employ_datQ49Carer$Carer, employ_datQ49Carer)
analysis <- polr(employ_datQ49Carer$CatOutcome ~ employ_datQ49Carer$Carer, data=employ_datQ49Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ49Disability<-multidatClean(Q49, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49Disability$Disability<- factor(employ_datQ49Disability$Disability)
employ_datQ49Disability<-ordinaldatClean49(employ_datQ49Disability$Q49, employ_datQ49Disability)
conTable <- xtabs(~Q49 + Disability, data = employ_datQ49Disability)
#ordinal(employ_datQ49Disability$CatOutcome, employ_datQ49Disability$Disability, employ_datQ49Disability)
prep <- analysisPrep(employ_datQ49Disability$CatOutcome, employ_datQ49Disability$Disability, employ_datQ49Disability)
analysis <- polr(employ_datQ49Disability$CatOutcome ~ employ_datQ49Disability$Disability, data=employ_datQ49Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ49Ethnicity<-multidatClean(Q49, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49Ethnicity$Ethnicity<- factor(employ_datQ49Ethnicity$EthnicityCleaned)
employ_datQ49Ethnicity<-ordinaldatClean49(employ_datQ49Ethnicity$Q49, employ_datQ49Ethnicity)
conTable <- xtabs(~Q49 + EthnicityCleaned, data = employ_datQ49Ethnicity)
conTable
#ordinal(employ_datQ49Ethnicity$CatOutcome, employ_datQ49Ethnicity$EthnicityCleaned, employ_datQ49Ethnicity)
prep <- analysisPrep(employ_datQ49Ethnicity$CatOutcome, employ_datQ49Ethnicity$EthnicityCleaned, employ_datQ49Ethnicity)
analysis <- polr(employ_datQ49Ethnicity$CatOutcome ~ employ_datQ49Ethnicity$EthnicityCleaned, data=employ_datQ49Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ49FirstGen<-multidatClean(Q49, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49FirstGen$FirstGen<-factor(employ_datQ49FirstGen$FirstGen)
employ_datQ49FirstGen<-ordinaldatClean49(employ_datQ49FirstGen$Q49, employ_datQ49FirstGen)
#ordinal(employ_datQ49FirstGen$CatOutcome, employ_datQ49FirstGen$FirstGen, employ_datQ49FirstGen)
prep <- analysisPrep(employ_datQ49FirstGen$CatOutcome, employ_datQ49FirstGen$FirstGen, employ_datQ49FirstGen)
analysis <- polr(employ_datQ49FirstGen$CatOutcome ~ employ_datQ49FirstGen$FirstGen, data=employ_datQ49FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ49Gender<-multidatClean(Q49, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49Gender$Gender<-factor(employ_datQ49Gender$Gender)
employ_datQ49Gender<-ordinaldatClean49(employ_datQ49Gender$Q49, employ_datQ49Gender)
#ordinal(employ_datQ49Gender$CatOutcome, employ_datQ49Gender$Gender, employ_datQ49Gender)
prep <- analysisPrep(employ_datQ49Gender$CatOutcome, employ_datQ49Gender$Gender, employ_datQ49Gender)
analysis <- polr(employ_datQ49Gender$CatOutcome ~ employ_datQ49Gender$Gender, data=employ_datQ49Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ49Sexuality<-multidatClean(Q49, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ49Sexuality$Sexuality<-factor(employ_datQ49Sexuality$Sexuality)
employ_datQ49Sexuality<-ordinaldatClean49(employ_datQ49Sexuality$Q49, employ_datQ49Sexuality)
#ordinal(employ_datQ49Sexuality$CatOutcome, employ_datQ49Sexuality$Sexuality, employ_datQ49Sexuality)
prep <- analysisPrep(employ_datQ49Sexuality$CatOutcome, employ_datQ49Sexuality$Sexuality, employ_datQ49Sexuality)
analysis <- polr(employ_datQ49Sexuality$CatOutcome ~ employ_datQ49Sexuality$Sexuality, data=employ_datQ49Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q49"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q49_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.1 - A limitedproportion of my work is suitable for  home working
"Q50.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.1<-multidatClean(Q50.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.1.a + Academic, data = employ_datQ50.1)
detach(dat_long)
employ_datQ50.1<-ordinaldatClean(employ_datQ50.1$Q50.1.a, employ_datQ50.1)
#ordinal(employ_datQ50.1$CatOutcome, employ_datQ50.1$Academic, employ_datQ50.1)
prep <- analysisPrep(employ_datQ50.1$CatOutcome, employ_datQ50.1$Academic, employ_datQ50.1)
analysis <- polr(employ_datQ50.1$CatOutcome ~ employ_datQ50.1$Academic, data=employ_datQ50.1, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.1College<-multidatClean(Q50.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.1.a + Q3, data = employ_datQ50.1College)
conTable
detach(dat_long)
employ_datQ50.1College$Q3[(employ_datQ50.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.1College$Q3<- factor(employ_datQ50.1College$Q3)
employ_datQ50.1College<-ordinaldatClean(employ_datQ50.1College$Q50.1.a, employ_datQ50.1College)
#ordinal(employ_datQ50.1College$CatOutcome, employ_datQ50.1College$Q3, employ_datQ50.1College)
prep <- analysisPrep(employ_datQ50.1College$CatOutcome, employ_datQ50.1College$Q3, employ_datQ50.1College)
analysis <- polr(employ_datQ50.1College$CatOutcome ~ employ_datQ50.1College$Q3, data=employ_datQ50.1College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.1Carer<-multidatClean(Q50.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1Carer$Carer<- factor(employ_datQ50.1Carer$Carer)
employ_datQ50.1Carer<-ordinaldatClean(employ_datQ50.1Carer$Q50.1.a, employ_datQ50.1Carer)
#ordinal(employ_datQ50.1Carer$CatOutcome, employ_datQ50.1Carer$Carer, employ_datQ50.1Carer)
prep <- analysisPrep(employ_datQ50.1Carer$CatOutcome, employ_datQ50.1Carer$Carer, employ_datQ50.1Carer)
analysis <- polr(employ_datQ50.1Carer$CatOutcome ~ employ_datQ50.1Carer$Carer, data=employ_datQ50.1Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.1Disability<-multidatClean(Q50.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1Disability$Disability<- factor(employ_datQ50.1Disability$Disability)
employ_datQ50.1Disability<-ordinaldatClean(employ_datQ50.1Disability$Q50.1.a, employ_datQ50.1Disability)
conTable <- xtabs(~Q50.1.a + Disability, data = employ_datQ50.1Disability)
#ordinal(employ_datQ50.1Disability$CatOutcome, employ_datQ50.1Disability$Disability, employ_datQ50.1Disability)
prep <- analysisPrep(employ_datQ50.1Disability$CatOutcome, employ_datQ50.1Disability$Disability, employ_datQ50.1Disability)
analysis <- polr(employ_datQ50.1Disability$CatOutcome ~ employ_datQ50.1Disability$Disability, data=employ_datQ50.1Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.1Ethnicity<-multidatClean(Q50.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1Ethnicity$Ethnicity<- factor(employ_datQ50.1Ethnicity$EthnicityCleaned)
employ_datQ50.1Ethnicity<-ordinaldatClean(employ_datQ50.1Ethnicity$Q50.1.a, employ_datQ50.1Ethnicity)
conTable <- xtabs(~Q50.1.a + EthnicityCleaned, data = employ_datQ50.1Ethnicity)
conTable
#ordinal(employ_datQ50.1Ethnicity$CatOutcome, employ_datQ50.1Ethnicity$EthnicityCleaned, employ_datQ50.1Ethnicity)
prep <- analysisPrep(employ_datQ50.1Ethnicity$CatOutcome, employ_datQ50.1Ethnicity$EthnicityCleaned, employ_datQ50.1Ethnicity)
analysis <- polr(employ_datQ50.1Ethnicity$CatOutcome ~ employ_datQ50.1Ethnicity$EthnicityCleaned, data=employ_datQ50.1Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.1FirstGen<-multidatClean(Q50.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1FirstGen$FirstGen<-factor(employ_datQ50.1FirstGen$FirstGen)
employ_datQ50.1FirstGen<-ordinaldatClean(employ_datQ50.1FirstGen$Q50.1.a, employ_datQ50.1FirstGen)
#ordinal(employ_datQ50.1FirstGen$CatOutcome, employ_datQ50.1FirstGen$FirstGen, employ_datQ50.1FirstGen)
prep <- analysisPrep(employ_datQ50.1FirstGen$CatOutcome, employ_datQ50.1FirstGen$FirstGen, employ_datQ50.1FirstGen)
analysis <- polr(employ_datQ50.1FirstGen$CatOutcome ~ employ_datQ50.1FirstGen$FirstGen, data=employ_datQ50.1FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.1Gender<-multidatClean(Q50.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1Gender$Gender<-factor(employ_datQ50.1Gender$Gender)
employ_datQ50.1Gender<-ordinaldatClean(employ_datQ50.1Gender$Q50.1.a, employ_datQ50.1Gender)
#ordinal(employ_datQ50.1Gender$CatOutcome, employ_datQ50.1Gender$Gender, employ_datQ50.1Gender)
prep <- analysisPrep(employ_datQ50.1Gender$CatOutcome, employ_datQ50.1Gender$Gender, employ_datQ50.1Gender)
analysis <- polr(employ_datQ50.1Gender$CatOutcome ~ employ_datQ50.1Gender$Gender, data=employ_datQ50.1Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.1Sexuality<-multidatClean(Q50.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.1Sexuality$Sexuality<-factor(employ_datQ50.1Sexuality$Sexuality)
employ_datQ50.1Sexuality<-ordinaldatClean(employ_datQ50.1Sexuality$Q50.1.a, employ_datQ50.1Sexuality)
#ordinal(employ_datQ50.1Sexuality$CatOutcome, employ_datQ50.1Sexuality$Sexuality, employ_datQ50.1Sexuality)
prep <- analysisPrep(employ_datQ50.1Sexuality$CatOutcome, employ_datQ50.1Sexuality$Sexuality, employ_datQ50.1Sexuality)
analysis <- polr(employ_datQ50.1Sexuality$CatOutcome ~ employ_datQ50.1Sexuality$Sexuality, data=employ_datQ50.1Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.2 - I do not have an appropriate physical workspace at home
"Q50.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.2<-multidatClean(Q50.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.2.a + Academic, data = employ_datQ50.2)
detach(dat_long)
employ_datQ50.2<-ordinaldatClean(employ_datQ50.2$Q50.2.a, employ_datQ50.2)
#ordinal(employ_datQ50.2$CatOutcome, employ_datQ50.2$Academic, employ_datQ50.2)
prep <- analysisPrep(employ_datQ50.2$CatOutcome, employ_datQ50.2$Academic, employ_datQ50.2)
analysis <- polr(employ_datQ50.2$CatOutcome ~ employ_datQ50.2$Academic, data=employ_datQ50.2, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.2College<-multidatClean(Q50.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.2.a + Q3, data = employ_datQ50.2College)
conTable
detach(dat_long)
employ_datQ50.2College$Q3[(employ_datQ50.2College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.2College$Q3<- factor(employ_datQ50.2College$Q3)
employ_datQ50.2College<-ordinaldatClean(employ_datQ50.2College$Q50.2.a, employ_datQ50.2College)
#ordinal(employ_datQ50.2College$CatOutcome, employ_datQ50.2College$Q3, employ_datQ50.2College)
prep <- analysisPrep(employ_datQ50.2College$CatOutcome, employ_datQ50.2College$Q3, employ_datQ50.2College)
analysis <- polr(employ_datQ50.2College$CatOutcome ~ employ_datQ50.2College$Q3, data=employ_datQ50.2College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.2Carer<-multidatClean(Q50.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2Carer$Carer<- factor(employ_datQ50.2Carer$Carer)
employ_datQ50.2Carer<-ordinaldatClean(employ_datQ50.2Carer$Q50.2.a, employ_datQ50.2Carer)
#ordinal(employ_datQ50.2Carer$CatOutcome, employ_datQ50.2Carer$Carer, employ_datQ50.2Carer)
prep <- analysisPrep(employ_datQ50.2Carer$CatOutcome, employ_datQ50.2Carer$Carer, employ_datQ50.2Carer)
analysis <- polr(employ_datQ50.2Carer$CatOutcome ~ employ_datQ50.2Carer$Carer, data=employ_datQ50.2Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.2Disability<-multidatClean(Q50.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2Disability$Disability<- factor(employ_datQ50.2Disability$Disability)
employ_datQ50.2Disability<-ordinaldatClean(employ_datQ50.2Disability$Q50.2.a, employ_datQ50.2Disability)
conTable <- xtabs(~Q50.2.a + Disability, data = employ_datQ50.2Disability)
#ordinal(employ_datQ50.2Disability$CatOutcome, employ_datQ50.2Disability$Disability, employ_datQ50.2Disability)
prep <- analysisPrep(employ_datQ50.2Disability$CatOutcome, employ_datQ50.2Disability$Disability, employ_datQ50.2Disability)
analysis <- polr(employ_datQ50.2Disability$CatOutcome ~ employ_datQ50.2Disability$Disability, data=employ_datQ50.2Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.2Ethnicity<-multidatClean(Q50.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2Ethnicity$Ethnicity<- factor(employ_datQ50.2Ethnicity$EthnicityCleaned)
employ_datQ50.2Ethnicity<-ordinaldatClean(employ_datQ50.2Ethnicity$Q50.2.a, employ_datQ50.2Ethnicity)
conTable <- xtabs(~Q50.2.a + EthnicityCleaned, data = employ_datQ50.2Ethnicity)
conTable
#ordinal(employ_datQ50.2Ethnicity$CatOutcome, employ_datQ50.2Ethnicity$EthnicityCleaned, employ_datQ50.2Ethnicity)
prep <- analysisPrep(employ_datQ50.2Ethnicity$CatOutcome, employ_datQ50.2Ethnicity$EthnicityCleaned, employ_datQ50.2Ethnicity)
analysis <- polr(employ_datQ50.2Ethnicity$CatOutcome ~ employ_datQ50.2Ethnicity$EthnicityCleaned, data=employ_datQ50.2Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.2FirstGen<-multidatClean(Q50.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2FirstGen$FirstGen<-factor(employ_datQ50.2FirstGen$FirstGen)
employ_datQ50.2FirstGen<-ordinaldatClean(employ_datQ50.2FirstGen$Q50.2.a, employ_datQ50.2FirstGen)
#ordinal(employ_datQ50.2FirstGen$CatOutcome, employ_datQ50.2FirstGen$FirstGen, employ_datQ50.2FirstGen)
prep <- analysisPrep(employ_datQ50.2FirstGen$CatOutcome, employ_datQ50.2FirstGen$FirstGen, employ_datQ50.2FirstGen)
analysis <- polr(employ_datQ50.2FirstGen$CatOutcome ~ employ_datQ50.2FirstGen$FirstGen, data=employ_datQ50.2FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.2Gender<-multidatClean(Q50.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2Gender$Gender<-factor(employ_datQ50.2Gender$Gender)
employ_datQ50.2Gender<-ordinaldatClean(employ_datQ50.2Gender$Q50.2.a, employ_datQ50.2Gender)
#ordinal(employ_datQ50.2Gender$CatOutcome, employ_datQ50.2Gender$Gender, employ_datQ50.2Gender)
prep <- analysisPrep(employ_datQ50.2Gender$CatOutcome, employ_datQ50.2Gender$Gender, employ_datQ50.2Gender)
analysis <- polr(employ_datQ50.2Gender$CatOutcome ~ employ_datQ50.2Gender$Gender, data=employ_datQ50.2Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.2Sexuality<-multidatClean(Q50.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.2Sexuality$Sexuality<-factor(employ_datQ50.2Sexuality$Sexuality)
employ_datQ50.2Sexuality<-ordinaldatClean(employ_datQ50.2Sexuality$Q50.2.a, employ_datQ50.2Sexuality)
#ordinal(employ_datQ50.2Sexuality$CatOutcome, employ_datQ50.2Sexuality$Sexuality, employ_datQ50.2Sexuality)
prep <- analysisPrep(employ_datQ50.2Sexuality$CatOutcome, employ_datQ50.2Sexuality$Sexuality, employ_datQ50.2Sexuality)
analysis <- polr(employ_datQ50.2Sexuality$CatOutcome ~ employ_datQ50.2Sexuality$Sexuality, data=employ_datQ50.2Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.3 - My home environment is not conducive to work
"Q50.3.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.3<-multidatClean(Q50.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.3.a + Academic, data = employ_datQ50.3)
detach(dat_long)
employ_datQ50.3<-ordinaldatClean(employ_datQ50.3$Q50.3.a, employ_datQ50.3)
#ordinal(employ_datQ50.3$CatOutcome, employ_datQ50.3$Academic, employ_datQ50.3)
prep <- analysisPrep(employ_datQ50.3$CatOutcome, employ_datQ50.3$Academic, employ_datQ50.3)
analysis <- polr(employ_datQ50.3$CatOutcome ~ employ_datQ50.3$Academic, data=employ_datQ50.3, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.3College<-multidatClean(Q50.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.3.a + Q3, data = employ_datQ50.3College)
conTable
detach(dat_long)
employ_datQ50.3College$Q3[(employ_datQ50.3College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.3College$Q3<- factor(employ_datQ50.3College$Q3)
employ_datQ50.3College<-ordinaldatClean(employ_datQ50.3College$Q50.3.a, employ_datQ50.3College)
#ordinal(employ_datQ50.3College$CatOutcome, employ_datQ50.3College$Q3, employ_datQ50.3College)
prep <- analysisPrep(employ_datQ50.3College$CatOutcome, employ_datQ50.3College$Q3, employ_datQ50.3College)
analysis <- polr(employ_datQ50.3College$CatOutcome ~ employ_datQ50.3College$Q3, data=employ_datQ50.3College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.3Carer<-multidatClean(Q50.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3Carer$Carer<- factor(employ_datQ50.3Carer$Carer)
employ_datQ50.3Carer<-ordinaldatClean(employ_datQ50.3Carer$Q50.3.a, employ_datQ50.3Carer)
#ordinal(employ_datQ50.3Carer$CatOutcome, employ_datQ50.3Carer$Carer, employ_datQ50.3Carer)
prep <- analysisPrep(employ_datQ50.3Carer$CatOutcome, employ_datQ50.3Carer$Carer, employ_datQ50.3Carer)
analysis <- polr(employ_datQ50.3Carer$CatOutcome ~ employ_datQ50.3Carer$Carer, data=employ_datQ50.3Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.3Disability<-multidatClean(Q50.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3Disability$Disability<- factor(employ_datQ50.3Disability$Disability)
employ_datQ50.3Disability<-ordinaldatClean(employ_datQ50.3Disability$Q50.3.a, employ_datQ50.3Disability)
conTable <- xtabs(~Q50.3.a + Disability, data = employ_datQ50.3Disability)
#ordinal(employ_datQ50.3Disability$CatOutcome, employ_datQ50.3Disability$Disability, employ_datQ50.3Disability)
prep <- analysisPrep(employ_datQ50.3Disability$CatOutcome, employ_datQ50.3Disability$Disability, employ_datQ50.3Disability)
analysis <- polr(employ_datQ50.3Disability$CatOutcome ~ employ_datQ50.3Disability$Disability, data=employ_datQ50.3Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.3Ethnicity<-multidatClean(Q50.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3Ethnicity$Ethnicity<- factor(employ_datQ50.3Ethnicity$EthnicityCleaned)
employ_datQ50.3Ethnicity<-ordinaldatClean(employ_datQ50.3Ethnicity$Q50.3.a, employ_datQ50.3Ethnicity)
conTable <- xtabs(~Q50.3.a + EthnicityCleaned, data = employ_datQ50.3Ethnicity)
conTable
#ordinal(employ_datQ50.3Ethnicity$CatOutcome, employ_datQ50.3Ethnicity$EthnicityCleaned, employ_datQ50.3Ethnicity)
prep <- analysisPrep(employ_datQ50.3Ethnicity$CatOutcome, employ_datQ50.3Ethnicity$EthnicityCleaned, employ_datQ50.3Ethnicity)
analysis <- polr(employ_datQ50.3Ethnicity$CatOutcome ~ employ_datQ50.3Ethnicity$EthnicityCleaned, data=employ_datQ50.3Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.3FirstGen<-multidatClean(Q50.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3FirstGen$FirstGen<-factor(employ_datQ50.3FirstGen$FirstGen)
employ_datQ50.3FirstGen<-ordinaldatClean(employ_datQ50.3FirstGen$Q50.3.a, employ_datQ50.3FirstGen)
#ordinal(employ_datQ50.3FirstGen$CatOutcome, employ_datQ50.3FirstGen$FirstGen, employ_datQ50.3FirstGen)
prep <- analysisPrep(employ_datQ50.3FirstGen$CatOutcome, employ_datQ50.3FirstGen$FirstGen, employ_datQ50.3FirstGen)
analysis <- polr(employ_datQ50.3FirstGen$CatOutcome ~ employ_datQ50.3FirstGen$FirstGen, data=employ_datQ50.3FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.3Gender<-multidatClean(Q50.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3Gender$Gender<-factor(employ_datQ50.3Gender$Gender)
employ_datQ50.3Gender<-ordinaldatClean(employ_datQ50.3Gender$Q50.3.a, employ_datQ50.3Gender)
#ordinal(employ_datQ50.3Gender$CatOutcome, employ_datQ50.3Gender$Gender, employ_datQ50.3Gender)
prep <- analysisPrep(employ_datQ50.3Gender$CatOutcome, employ_datQ50.3Gender$Gender, employ_datQ50.3Gender)
analysis <- polr(employ_datQ50.3Gender$CatOutcome ~ employ_datQ50.3Gender$Gender, data=employ_datQ50.3Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.3Sexuality<-multidatClean(Q50.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.3Sexuality$Sexuality<-factor(employ_datQ50.3Sexuality$Sexuality)
employ_datQ50.3Sexuality<-ordinaldatClean(employ_datQ50.3Sexuality$Q50.3.a, employ_datQ50.3Sexuality)
#ordinal(employ_datQ50.3Sexuality$CatOutcome, employ_datQ50.3Sexuality$Sexuality, employ_datQ50.3Sexuality)
prep <- analysisPrep(employ_datQ50.3Sexuality$CatOutcome, employ_datQ50.3Sexuality$Sexuality, employ_datQ50.3Sexuality)
analysis <- polr(employ_datQ50.3Sexuality$CatOutcome ~ employ_datQ50.3Sexuality$Sexuality, data=employ_datQ50.3Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

"Q50.4.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.4<-multidatClean(Q50.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.4.a + Academic, data = employ_datQ50.4)
detach(dat_long)
employ_datQ50.4<-ordinaldatClean(employ_datQ50.4$Q50.4.a, employ_datQ50.4)
#ordinal(employ_datQ50.4$CatOutcome, employ_datQ50.4$Academic, employ_datQ50.4)
prep <- analysisPrep(employ_datQ50.4$CatOutcome, employ_datQ50.4$Academic, employ_datQ50.4)
analysis <- polr(employ_datQ50.4$CatOutcome ~ employ_datQ50.4$Academic, data=employ_datQ50.4, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.4College<-multidatClean(Q50.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.4.a + Q3, data = employ_datQ50.4College)
conTable
detach(dat_long)
employ_datQ50.4College$Q3[(employ_datQ50.4College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.4College$Q3<- factor(employ_datQ50.4College$Q3)
employ_datQ50.4College<-ordinaldatClean(employ_datQ50.4College$Q50.4.a, employ_datQ50.4College)
#ordinal(employ_datQ50.4College$CatOutcome, employ_datQ50.4College$Q3, employ_datQ50.4College)
prep <- analysisPrep(employ_datQ50.4College$CatOutcome, employ_datQ50.4College$Q3, employ_datQ50.4College)
analysis <- polr(employ_datQ50.4College$CatOutcome ~ employ_datQ50.4College$Q3, data=employ_datQ50.4College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.4Carer<-multidatClean(Q50.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4Carer$Carer<- factor(employ_datQ50.4Carer$Carer)
employ_datQ50.4Carer<-ordinaldatClean(employ_datQ50.4Carer$Q50.4.a, employ_datQ50.4Carer)
#ordinal(employ_datQ50.4Carer$CatOutcome, employ_datQ50.4Carer$Carer, employ_datQ50.4Carer)
prep <- analysisPrep(employ_datQ50.4Carer$CatOutcome, employ_datQ50.4Carer$Carer, employ_datQ50.4Carer)
analysis <- polr(employ_datQ50.4Carer$CatOutcome ~ employ_datQ50.4Carer$Carer, data=employ_datQ50.4Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.4Disability<-multidatClean(Q50.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4Disability$Disability<- factor(employ_datQ50.4Disability$Disability)
employ_datQ50.4Disability<-ordinaldatClean(employ_datQ50.4Disability$Q50.4.a, employ_datQ50.4Disability)
conTable <- xtabs(~Q50.4.a + Disability, data = employ_datQ50.4Disability)
#ordinal(employ_datQ50.4Disability$CatOutcome, employ_datQ50.4Disability$Disability, employ_datQ50.4Disability)
prep <- analysisPrep(employ_datQ50.4Disability$CatOutcome, employ_datQ50.4Disability$Disability, employ_datQ50.4Disability)
analysis <- polr(employ_datQ50.4Disability$CatOutcome ~ employ_datQ50.4Disability$Disability, data=employ_datQ50.4Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.4Ethnicity<-multidatClean(Q50.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4Ethnicity$Ethnicity<- factor(employ_datQ50.4Ethnicity$EthnicityCleaned)
employ_datQ50.4Ethnicity<-ordinaldatClean(employ_datQ50.4Ethnicity$Q50.4.a, employ_datQ50.4Ethnicity)
conTable <- xtabs(~Q50.4.a + EthnicityCleaned, data = employ_datQ50.4Ethnicity)
conTable
#ordinal(employ_datQ50.4Ethnicity$CatOutcome, employ_datQ50.4Ethnicity$EthnicityCleaned, employ_datQ50.4Ethnicity)
prep <- analysisPrep(employ_datQ50.4Ethnicity$CatOutcome, employ_datQ50.4Ethnicity$EthnicityCleaned, employ_datQ50.4Ethnicity)
analysis <- polr(employ_datQ50.4Ethnicity$CatOutcome ~ employ_datQ50.4Ethnicity$EthnicityCleaned, data=employ_datQ50.4Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.4FirstGen<-multidatClean(Q50.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4FirstGen$FirstGen<-factor(employ_datQ50.4FirstGen$FirstGen)
employ_datQ50.4FirstGen<-ordinaldatClean(employ_datQ50.4FirstGen$Q50.4.a, employ_datQ50.4FirstGen)
#ordinal(employ_datQ50.4FirstGen$CatOutcome, employ_datQ50.4FirstGen$FirstGen, employ_datQ50.4FirstGen)
prep <- analysisPrep(employ_datQ50.4FirstGen$CatOutcome, employ_datQ50.4FirstGen$FirstGen, employ_datQ50.4FirstGen)
analysis <- polr(employ_datQ50.4FirstGen$CatOutcome ~ employ_datQ50.4FirstGen$FirstGen, data=employ_datQ50.4FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.4Gender<-multidatClean(Q50.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4Gender$Gender<-factor(employ_datQ50.4Gender$Gender)
employ_datQ50.4Gender<-ordinaldatClean(employ_datQ50.4Gender$Q50.4.a, employ_datQ50.4Gender)
#ordinal(employ_datQ50.4Gender$CatOutcome, employ_datQ50.4Gender$Gender, employ_datQ50.4Gender)
prep <- analysisPrep(employ_datQ50.4Gender$CatOutcome, employ_datQ50.4Gender$Gender, employ_datQ50.4Gender)
analysis <- polr(employ_datQ50.4Gender$CatOutcome ~ employ_datQ50.4Gender$Gender, data=employ_datQ50.4Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.4Sexuality<-multidatClean(Q50.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.4Sexuality$Sexuality<-factor(employ_datQ50.4Sexuality$Sexuality)
employ_datQ50.4Sexuality<-ordinaldatClean(employ_datQ50.4Sexuality$Q50.4.a, employ_datQ50.4Sexuality)
#ordinal(employ_datQ50.4Sexuality$CatOutcome, employ_datQ50.4Sexuality$Sexuality, employ_datQ50.4Sexuality)
prep <- analysisPrep(employ_datQ50.4Sexuality$CatOutcome, employ_datQ50.4Sexuality$Sexuality, employ_datQ50.4Sexuality)
analysis <- polr(employ_datQ50.4Sexuality$CatOutcome ~ employ_datQ50.4Sexuality$Sexuality, data=employ_datQ50.4Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.5 - I have caring responsibilities
"Q50.5.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.5<-multidatClean(Q50.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.5.a + Academic, data = employ_datQ50.5)
detach(dat_long)
employ_datQ50.5<-ordinaldatClean(employ_datQ50.5$Q50.5.a, employ_datQ50.5)
#ordinal(employ_datQ50.5$CatOutcome, employ_datQ50.5$Academic, employ_datQ50.5)
prep <- analysisPrep(employ_datQ50.5$CatOutcome, employ_datQ50.5$Academic, employ_datQ50.5)
analysis <- polr(employ_datQ50.5$CatOutcome ~ employ_datQ50.5$Academic, data=employ_datQ50.5, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.5College<-multidatClean(Q50.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.5.a + Q3, data = employ_datQ50.5College)
conTable
detach(dat_long)
employ_datQ50.5College$Q3[(employ_datQ50.5College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.5College$Q3<- factor(employ_datQ50.5College$Q3)
employ_datQ50.5College<-ordinaldatClean(employ_datQ50.5College$Q50.5.a, employ_datQ50.5College)
#ordinal(employ_datQ50.5College$CatOutcome, employ_datQ50.5College$Q3, employ_datQ50.5College)
prep <- analysisPrep(employ_datQ50.5College$CatOutcome, employ_datQ50.5College$Q3, employ_datQ50.5College)
analysis <- polr(employ_datQ50.5College$CatOutcome ~ employ_datQ50.5College$Q3, data=employ_datQ50.5College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.5Carer<-multidatClean(Q50.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5Carer$Carer<- factor(employ_datQ50.5Carer$Carer)
employ_datQ50.5Carer<-ordinaldatClean(employ_datQ50.5Carer$Q50.5.a, employ_datQ50.5Carer)
#ordinal(employ_datQ50.5Carer$CatOutcome, employ_datQ50.5Carer$Carer, employ_datQ50.5Carer)
prep <- analysisPrep(employ_datQ50.5Carer$CatOutcome, employ_datQ50.5Carer$Carer, employ_datQ50.5Carer)
analysis <- polr(employ_datQ50.5Carer$CatOutcome ~ employ_datQ50.5Carer$Carer, data=employ_datQ50.5Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.5Disability<-multidatClean(Q50.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5Disability$Disability<- factor(employ_datQ50.5Disability$Disability)
employ_datQ50.5Disability<-ordinaldatClean(employ_datQ50.5Disability$Q50.5.a, employ_datQ50.5Disability)
conTable <- xtabs(~Q50.5.a + Disability, data = employ_datQ50.5Disability)
#ordinal(employ_datQ50.5Disability$CatOutcome, employ_datQ50.5Disability$Disability, employ_datQ50.5Disability)
prep <- analysisPrep(employ_datQ50.5Disability$CatOutcome, employ_datQ50.5Disability$Disability, employ_datQ50.5Disability)
analysis <- polr(employ_datQ50.5Disability$CatOutcome ~ employ_datQ50.5Disability$Disability, data=employ_datQ50.5Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.5Ethnicity<-multidatClean(Q50.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5Ethnicity$Ethnicity<- factor(employ_datQ50.5Ethnicity$EthnicityCleaned)
employ_datQ50.5Ethnicity<-ordinaldatClean(employ_datQ50.5Ethnicity$Q50.5.a, employ_datQ50.5Ethnicity)
conTable <- xtabs(~Q50.5.a + EthnicityCleaned, data = employ_datQ50.5Ethnicity)
conTable
#ordinal(employ_datQ50.5Ethnicity$CatOutcome, employ_datQ50.5Ethnicity$EthnicityCleaned, employ_datQ50.5Ethnicity)
prep <- analysisPrep(employ_datQ50.5Ethnicity$CatOutcome, employ_datQ50.5Ethnicity$EthnicityCleaned, employ_datQ50.5Ethnicity)
analysis <- polr(employ_datQ50.5Ethnicity$CatOutcome ~ employ_datQ50.5Ethnicity$EthnicityCleaned, data=employ_datQ50.5Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.5FirstGen<-multidatClean(Q50.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5FirstGen$FirstGen<-factor(employ_datQ50.5FirstGen$FirstGen)
employ_datQ50.5FirstGen<-ordinaldatClean(employ_datQ50.5FirstGen$Q50.5.a, employ_datQ50.5FirstGen)
#ordinal(employ_datQ50.5FirstGen$CatOutcome, employ_datQ50.5FirstGen$FirstGen, employ_datQ50.5FirstGen)
prep <- analysisPrep(employ_datQ50.5FirstGen$CatOutcome, employ_datQ50.5FirstGen$FirstGen, employ_datQ50.5FirstGen)
analysis <- polr(employ_datQ50.5FirstGen$CatOutcome ~ employ_datQ50.5FirstGen$FirstGen, data=employ_datQ50.5FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.5Gender<-multidatClean(Q50.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5Gender$Gender<-factor(employ_datQ50.5Gender$Gender)
employ_datQ50.5Gender<-ordinaldatClean(employ_datQ50.5Gender$Q50.5.a, employ_datQ50.5Gender)
#ordinal(employ_datQ50.5Gender$CatOutcome, employ_datQ50.5Gender$Gender, employ_datQ50.5Gender)
prep <- analysisPrep(employ_datQ50.5Gender$CatOutcome, employ_datQ50.5Gender$Gender, employ_datQ50.5Gender)
analysis <- polr(employ_datQ50.5Gender$CatOutcome ~ employ_datQ50.5Gender$Gender, data=employ_datQ50.5Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.5Sexuality<-multidatClean(Q50.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.5Sexuality$Sexuality<-factor(employ_datQ50.5Sexuality$Sexuality)
employ_datQ50.5Sexuality<-ordinaldatClean(employ_datQ50.5Sexuality$Q50.5.a, employ_datQ50.5Sexuality)
#ordinal(employ_datQ50.5Sexuality$CatOutcome, employ_datQ50.5Sexuality$Sexuality, employ_datQ50.5Sexuality)
prep <- analysisPrep(employ_datQ50.5Sexuality$CatOutcome, employ_datQ50.5Sexuality$Sexuality, employ_datQ50.5Sexuality)
analysis <- polr(employ_datQ50.5Sexuality$CatOutcome ~ employ_datQ50.5Sexuality$Sexuality, data=employ_datQ50.5Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.6 - My work is limited by my home computing facilities
"Q50.6.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.6<-multidatClean(Q50.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.6.a + Academic, data = employ_datQ50.6)
detach(dat_long)
employ_datQ50.6<-ordinaldatClean(employ_datQ50.6$Q50.6.a, employ_datQ50.6)
#ordinal(employ_datQ50.6$CatOutcome, employ_datQ50.6$Academic, employ_datQ50.6)
prep <- analysisPrep(employ_datQ50.6$CatOutcome, employ_datQ50.6$Academic, employ_datQ50.6)
analysis <- polr(employ_datQ50.6$CatOutcome ~ employ_datQ50.6$Academic, data=employ_datQ50.6, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.6College<-multidatClean(Q50.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.6.a + Q3, data = employ_datQ50.6College)
conTable
detach(dat_long)
employ_datQ50.6College$Q3[(employ_datQ50.6College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.6College$Q3<- factor(employ_datQ50.6College$Q3)
employ_datQ50.6College<-ordinaldatClean(employ_datQ50.6College$Q50.6.a, employ_datQ50.6College)
#ordinal(employ_datQ50.6College$CatOutcome, employ_datQ50.6College$Q3, employ_datQ50.6College)
prep <- analysisPrep(employ_datQ50.6College$CatOutcome, employ_datQ50.6College$Q3, employ_datQ50.6College)
analysis <- polr(employ_datQ50.6College$CatOutcome ~ employ_datQ50.6College$Q3, data=employ_datQ50.6College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.6Carer<-multidatClean(Q50.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6Carer$Carer<- factor(employ_datQ50.6Carer$Carer)
employ_datQ50.6Carer<-ordinaldatClean(employ_datQ50.6Carer$Q50.6.a, employ_datQ50.6Carer)
#ordinal(employ_datQ50.6Carer$CatOutcome, employ_datQ50.6Carer$Carer, employ_datQ50.6Carer)
prep <- analysisPrep(employ_datQ50.6Carer$CatOutcome, employ_datQ50.6Carer$Carer, employ_datQ50.6Carer)
analysis <- polr(employ_datQ50.6Carer$CatOutcome ~ employ_datQ50.6Carer$Carer, data=employ_datQ50.6Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.6Disability<-multidatClean(Q50.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6Disability$Disability<- factor(employ_datQ50.6Disability$Disability)
employ_datQ50.6Disability<-ordinaldatClean(employ_datQ50.6Disability$Q50.6.a, employ_datQ50.6Disability)
conTable <- xtabs(~Q50.6.a + Disability, data = employ_datQ50.6Disability)
#ordinal(employ_datQ50.6Disability$CatOutcome, employ_datQ50.6Disability$Disability, employ_datQ50.6Disability)
prep <- analysisPrep(employ_datQ50.6Disability$CatOutcome, employ_datQ50.6Disability$Disability, employ_datQ50.6Disability)
analysis <- polr(employ_datQ50.6Disability$CatOutcome ~ employ_datQ50.6Disability$Disability, data=employ_datQ50.6Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.6Ethnicity<-multidatClean(Q50.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6Ethnicity$Ethnicity<- factor(employ_datQ50.6Ethnicity$EthnicityCleaned)
employ_datQ50.6Ethnicity<-ordinaldatClean(employ_datQ50.6Ethnicity$Q50.6.a, employ_datQ50.6Ethnicity)
conTable <- xtabs(~Q50.6.a + EthnicityCleaned, data = employ_datQ50.6Ethnicity)
conTable
#ordinal(employ_datQ50.6Ethnicity$CatOutcome, employ_datQ50.6Ethnicity$EthnicityCleaned, employ_datQ50.6Ethnicity)
prep <- analysisPrep(employ_datQ50.6Ethnicity$CatOutcome, employ_datQ50.6Ethnicity$EthnicityCleaned, employ_datQ50.6Ethnicity)
analysis <- polr(employ_datQ50.6Ethnicity$CatOutcome ~ employ_datQ50.6Ethnicity$EthnicityCleaned, data=employ_datQ50.6Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.6FirstGen<-multidatClean(Q50.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6FirstGen$FirstGen<-factor(employ_datQ50.6FirstGen$FirstGen)
employ_datQ50.6FirstGen<-ordinaldatClean(employ_datQ50.6FirstGen$Q50.6.a, employ_datQ50.6FirstGen)
#ordinal(employ_datQ50.6FirstGen$CatOutcome, employ_datQ50.6FirstGen$FirstGen, employ_datQ50.6FirstGen)
prep <- analysisPrep(employ_datQ50.6FirstGen$CatOutcome, employ_datQ50.6FirstGen$FirstGen, employ_datQ50.6FirstGen)
analysis <- polr(employ_datQ50.6FirstGen$CatOutcome ~ employ_datQ50.6FirstGen$FirstGen, data=employ_datQ50.6FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.6Gender<-multidatClean(Q50.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6Gender$Gender<-factor(employ_datQ50.6Gender$Gender)
employ_datQ50.6Gender<-ordinaldatClean(employ_datQ50.6Gender$Q50.6.a, employ_datQ50.6Gender)
#ordinal(employ_datQ50.6Gender$CatOutcome, employ_datQ50.6Gender$Gender, employ_datQ50.6Gender)
prep <- analysisPrep(employ_datQ50.6Gender$CatOutcome, employ_datQ50.6Gender$Gender, employ_datQ50.6Gender)
analysis <- polr(employ_datQ50.6Gender$CatOutcome ~ employ_datQ50.6Gender$Gender, data=employ_datQ50.6Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.6Sexuality<-multidatClean(Q50.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.6Sexuality$Sexuality<-factor(employ_datQ50.6Sexuality$Sexuality)
employ_datQ50.6Sexuality<-ordinaldatClean(employ_datQ50.6Sexuality$Q50.6.a, employ_datQ50.6Sexuality)
#ordinal(employ_datQ50.6Sexuality$CatOutcome, employ_datQ50.6Sexuality$Sexuality, employ_datQ50.6Sexuality)
prep <- analysisPrep(employ_datQ50.6Sexuality$CatOutcome, employ_datQ50.6Sexuality$Sexuality, employ_datQ50.6Sexuality)
analysis <- polr(employ_datQ50.6Sexuality$CatOutcome ~ employ_datQ50.6Sexuality$Sexuality, data=employ_datQ50.6Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q50 - what barriers have you faced
#Q50.7 - My work is limited by the quality of my internet access
"Q50.7.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ50.7<-multidatClean(Q50.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.7.a + Academic, data = employ_datQ50.7)
detach(dat_long)
employ_datQ50.7<-ordinaldatClean(employ_datQ50.7$Q50.7.a, employ_datQ50.7)
#ordinal(employ_datQ50.7$CatOutcome, employ_datQ50.7$Academic, employ_datQ50.7)
prep <- analysisPrep(employ_datQ50.7$CatOutcome, employ_datQ50.7$Academic, employ_datQ50.7)
analysis <- polr(employ_datQ50.7$CatOutcome ~ employ_datQ50.7$Academic, data=employ_datQ50.7, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q50.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ50.7College<-multidatClean(Q50.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q50.7.a + Q3, data = employ_datQ50.7College)
conTable
detach(dat_long)
employ_datQ50.7College$Q3[(employ_datQ50.7College$Q3 == "Research Professional Staff")]="Other"
employ_datQ50.7College$Q3<- factor(employ_datQ50.7College$Q3)
employ_datQ50.7College<-ordinaldatClean(employ_datQ50.7College$Q50.7.a, employ_datQ50.7College)
#ordinal(employ_datQ50.7College$CatOutcome, employ_datQ50.7College$Q3, employ_datQ50.7College)
prep <- analysisPrep(employ_datQ50.7College$CatOutcome, employ_datQ50.7College$Q3, employ_datQ50.7College)
analysis <- polr(employ_datQ50.7College$CatOutcome ~ employ_datQ50.7College$Q3, data=employ_datQ50.7College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ50.7Carer<-multidatClean(Q50.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7Carer$Carer<- factor(employ_datQ50.7Carer$Carer)
employ_datQ50.7Carer<-ordinaldatClean(employ_datQ50.7Carer$Q50.7.a, employ_datQ50.7Carer)
#ordinal(employ_datQ50.7Carer$CatOutcome, employ_datQ50.7Carer$Carer, employ_datQ50.7Carer)
prep <- analysisPrep(employ_datQ50.7Carer$CatOutcome, employ_datQ50.7Carer$Carer, employ_datQ50.7Carer)
analysis <- polr(employ_datQ50.7Carer$CatOutcome ~ employ_datQ50.7Carer$Carer, data=employ_datQ50.7Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ50.7Disability<-multidatClean(Q50.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7Disability$Disability<- factor(employ_datQ50.7Disability$Disability)
employ_datQ50.7Disability<-ordinaldatClean(employ_datQ50.7Disability$Q50.7.a, employ_datQ50.7Disability)
conTable <- xtabs(~Q50.7.a + Disability, data = employ_datQ50.7Disability)
#ordinal(employ_datQ50.7Disability$CatOutcome, employ_datQ50.7Disability$Disability, employ_datQ50.7Disability)
prep <- analysisPrep(employ_datQ50.7Disability$CatOutcome, employ_datQ50.7Disability$Disability, employ_datQ50.7Disability)
analysis <- polr(employ_datQ50.7Disability$CatOutcome ~ employ_datQ50.7Disability$Disability, data=employ_datQ50.7Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ50.7Ethnicity<-multidatClean(Q50.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7Ethnicity$Ethnicity<- factor(employ_datQ50.7Ethnicity$EthnicityCleaned)
employ_datQ50.7Ethnicity<-ordinaldatClean(employ_datQ50.7Ethnicity$Q50.7.a, employ_datQ50.7Ethnicity)
conTable <- xtabs(~Q50.7.a + EthnicityCleaned, data = employ_datQ50.7Ethnicity)
conTable
#ordinal(employ_datQ50.7Ethnicity$CatOutcome, employ_datQ50.7Ethnicity$EthnicityCleaned, employ_datQ50.7Ethnicity)
prep <- analysisPrep(employ_datQ50.7Ethnicity$CatOutcome, employ_datQ50.7Ethnicity$EthnicityCleaned, employ_datQ50.7Ethnicity)
analysis <- polr(employ_datQ50.7Ethnicity$CatOutcome ~ employ_datQ50.7Ethnicity$EthnicityCleaned, data=employ_datQ50.7Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ50.7FirstGen<-multidatClean(Q50.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7FirstGen$FirstGen<-factor(employ_datQ50.7FirstGen$FirstGen)
employ_datQ50.7FirstGen<-ordinaldatClean(employ_datQ50.7FirstGen$Q50.7.a, employ_datQ50.7FirstGen)
#ordinal(employ_datQ50.7FirstGen$CatOutcome, employ_datQ50.7FirstGen$FirstGen, employ_datQ50.7FirstGen)
prep <- analysisPrep(employ_datQ50.7FirstGen$CatOutcome, employ_datQ50.7FirstGen$FirstGen, employ_datQ50.7FirstGen)
analysis <- polr(employ_datQ50.7FirstGen$CatOutcome ~ employ_datQ50.7FirstGen$FirstGen, data=employ_datQ50.7FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ50.7Gender<-multidatClean(Q50.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7Gender$Gender<-factor(employ_datQ50.7Gender$Gender)
employ_datQ50.7Gender<-ordinaldatClean(employ_datQ50.7Gender$Q50.7.a, employ_datQ50.7Gender)
#ordinal(employ_datQ50.7Gender$CatOutcome, employ_datQ50.7Gender$Gender, employ_datQ50.7Gender)
prep <- analysisPrep(employ_datQ50.7Gender$CatOutcome, employ_datQ50.7Gender$Gender, employ_datQ50.7Gender)
analysis <- polr(employ_datQ50.7Gender$CatOutcome ~ employ_datQ50.7Gender$Gender, data=employ_datQ50.7Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ50.7Sexuality<-multidatClean(Q50.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ50.7Sexuality$Sexuality<-factor(employ_datQ50.7Sexuality$Sexuality)
employ_datQ50.7Sexuality<-ordinaldatClean(employ_datQ50.7Sexuality$Q50.7.a, employ_datQ50.7Sexuality)
#ordinal(employ_datQ50.7Sexuality$CatOutcome, employ_datQ50.7Sexuality$Sexuality, employ_datQ50.7Sexuality)
prep <- analysisPrep(employ_datQ50.7Sexuality$CatOutcome, employ_datQ50.7Sexuality$Sexuality, employ_datQ50.7Sexuality)
analysis <- polr(employ_datQ50.7Sexuality$CatOutcome ~ employ_datQ50.7Sexuality$Sexuality, data=employ_datQ50.7Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q50.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q50.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q51.1 (using orddatclean47) How have you felt in yourself - anxious
"Q51.1 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ51.1<-multidatClean(Q51.1, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.1 + Academic, data = employ_datQ51.1)
detach(dat_long)
employ_datQ51.1<-ordinaldatClean47(employ_datQ51.1$Q51.1, employ_datQ51.1)
#ordinal(employ_datQ51.1$CatOutcome, employ_datQ51.1$Academic, employ_datQ51.1)
prep <- analysisPrep(employ_datQ51.1$CatOutcome, employ_datQ51.1$Academic, employ_datQ51.1)
analysis <- polr(employ_datQ51.1$CatOutcome ~ employ_datQ51.1$Academic, data=employ_datQ51.1, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q51.1"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ51.1College<-multidatClean(Q51.1, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.1 + Q3, data = employ_datQ51.1College)
conTable
detach(dat_long)
employ_datQ51.1College$Q3[(employ_datQ51.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ51.1College$Q3<- factor(employ_datQ51.1College$Q3)
employ_datQ51.1College<-ordinaldatClean47(employ_datQ51.1College$Q51.1, employ_datQ51.1College)
#ordinal(employ_datQ51.1College$CatOutcome, employ_datQ51.1College$Q3, employ_datQ51.1College)
prep <- analysisPrep(employ_datQ51.1College$CatOutcome, employ_datQ51.1College$Q3, employ_datQ51.1College)
analysis <- polr(employ_datQ51.1College$CatOutcome ~ employ_datQ51.1College$Q3, data=employ_datQ51.1College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ51.1Carer<-multidatClean(Q51.1, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1Carer$Carer<- factor(employ_datQ51.1Carer$Carer)
employ_datQ51.1Carer<-ordinaldatClean47(employ_datQ51.1Carer$Q51.1, employ_datQ51.1Carer)
#ordinal(employ_datQ51.1Carer$CatOutcome, employ_datQ51.1Carer$Carer, employ_datQ51.1Carer)
prep <- analysisPrep(employ_datQ51.1Carer$CatOutcome, employ_datQ51.1Carer$Carer, employ_datQ51.1Carer)
analysis <- polr(employ_datQ51.1Carer$CatOutcome ~ employ_datQ51.1Carer$Carer, data=employ_datQ51.1Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ51.1Disability<-multidatClean(Q51.1, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1Disability$Disability<- factor(employ_datQ51.1Disability$Disability)
employ_datQ51.1Disability<-ordinaldatClean47(employ_datQ51.1Disability$Q51.1, employ_datQ51.1Disability)
conTable <- xtabs(~Q51.1 + Disability, data = employ_datQ51.1Disability)
#ordinal(employ_datQ51.1Disability$CatOutcome, employ_datQ51.1Disability$Disability, employ_datQ51.1Disability)
prep <- analysisPrep(employ_datQ51.1Disability$CatOutcome, employ_datQ51.1Disability$Disability, employ_datQ51.1Disability)
analysis <- polr(employ_datQ51.1Disability$CatOutcome ~ employ_datQ51.1Disability$Disability, data=employ_datQ51.1Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ51.1Ethnicity<-multidatClean(Q51.1, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1Ethnicity$Ethnicity<- factor(employ_datQ51.1Ethnicity$EthnicityCleaned)
employ_datQ51.1Ethnicity<-ordinaldatClean47(employ_datQ51.1Ethnicity$Q51.1, employ_datQ51.1Ethnicity)
conTable <- xtabs(~Q51.1 + EthnicityCleaned, data = employ_datQ51.1Ethnicity)
conTable
#ordinal(employ_datQ51.1Ethnicity$CatOutcome, employ_datQ51.1Ethnicity$EthnicityCleaned, employ_datQ51.1Ethnicity)
prep <- analysisPrep(employ_datQ51.1Ethnicity$CatOutcome, employ_datQ51.1Ethnicity$EthnicityCleaned, employ_datQ51.1Ethnicity)
analysis <- polr(employ_datQ51.1Ethnicity$CatOutcome ~ employ_datQ51.1Ethnicity$EthnicityCleaned, data=employ_datQ51.1Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ51.1FirstGen<-multidatClean(Q51.1, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1FirstGen$FirstGen<-factor(employ_datQ51.1FirstGen$FirstGen)
employ_datQ51.1FirstGen<-ordinaldatClean47(employ_datQ51.1FirstGen$Q51.1, employ_datQ51.1FirstGen)
#ordinal(employ_datQ51.1FirstGen$CatOutcome, employ_datQ51.1FirstGen$FirstGen, employ_datQ51.1FirstGen)
prep <- analysisPrep(employ_datQ51.1FirstGen$CatOutcome, employ_datQ51.1FirstGen$FirstGen, employ_datQ51.1FirstGen)
analysis <- polr(employ_datQ51.1FirstGen$CatOutcome ~ employ_datQ51.1FirstGen$FirstGen, data=employ_datQ51.1FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ51.1Gender<-multidatClean(Q51.1, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1Gender$Gender<-factor(employ_datQ51.1Gender$Gender)
employ_datQ51.1Gender<-ordinaldatClean47(employ_datQ51.1Gender$Q51.1, employ_datQ51.1Gender)
#ordinal(employ_datQ51.1Gender$CatOutcome, employ_datQ51.1Gender$Gender, employ_datQ51.1Gender)
prep <- analysisPrep(employ_datQ51.1Gender$CatOutcome, employ_datQ51.1Gender$Gender, employ_datQ51.1Gender)
analysis <- polr(employ_datQ51.1Gender$CatOutcome ~ employ_datQ51.1Gender$Gender, data=employ_datQ51.1Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ51.1Sexuality<-multidatClean(Q51.1, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.1Sexuality$Sexuality<-factor(employ_datQ51.1Sexuality$Sexuality)
employ_datQ51.1Sexuality<-ordinaldatClean47(employ_datQ51.1Sexuality$Q51.1, employ_datQ51.1Sexuality)
#ordinal(employ_datQ51.1Sexuality$CatOutcome, employ_datQ51.1Sexuality$Sexuality, employ_datQ51.1Sexuality)
prep <- analysisPrep(employ_datQ51.1Sexuality$CatOutcome, employ_datQ51.1Sexuality$Sexuality, employ_datQ51.1Sexuality)
analysis <- polr(employ_datQ51.1Sexuality$CatOutcome ~ employ_datQ51.1Sexuality$Sexuality, data=employ_datQ51.1Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.1"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q51.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q51.2 (using orddatclean47) How have you felt in yourself - stressed
"Q51.2 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ51.2<-multidatClean(Q51.2, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.2 + Academic, data = employ_datQ51.2)
detach(dat_long)
employ_datQ51.2<-ordinaldatClean47(employ_datQ51.2$Q51.2, employ_datQ51.2)
#ordinal(employ_datQ51.2$CatOutcome, employ_datQ51.2$Academic, employ_datQ51.2)
prep <- analysisPrep(employ_datQ51.2$CatOutcome, employ_datQ51.2$Academic, employ_datQ51.2)
analysis <- polr(employ_datQ51.2$CatOutcome ~ employ_datQ51.2$Academic, data=employ_datQ51.2, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q51.2"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ51.2College<-multidatClean(Q51.2, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.2 + Q3, data = employ_datQ51.2College)
conTable
detach(dat_long)
employ_datQ51.2College$Q3[(employ_datQ51.2College$Q3 == "Research Professional Staff")]="Other"
employ_datQ51.2College$Q3<- factor(employ_datQ51.2College$Q3)
employ_datQ51.2College<-ordinaldatClean47(employ_datQ51.2College$Q51.2, employ_datQ51.2College)
#ordinal(employ_datQ51.2College$CatOutcome, employ_datQ51.2College$Q3, employ_datQ51.2College)
prep <- analysisPrep(employ_datQ51.2College$CatOutcome, employ_datQ51.2College$Q3, employ_datQ51.2College)
analysis <- polr(employ_datQ51.2College$CatOutcome ~ employ_datQ51.2College$Q3, data=employ_datQ51.2College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ51.2Carer<-multidatClean(Q51.2, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2Carer$Carer<- factor(employ_datQ51.2Carer$Carer)
employ_datQ51.2Carer<-ordinaldatClean47(employ_datQ51.2Carer$Q51.2, employ_datQ51.2Carer)
#ordinal(employ_datQ51.2Carer$CatOutcome, employ_datQ51.2Carer$Carer, employ_datQ51.2Carer)
prep <- analysisPrep(employ_datQ51.2Carer$CatOutcome, employ_datQ51.2Carer$Carer, employ_datQ51.2Carer)
analysis <- polr(employ_datQ51.2Carer$CatOutcome ~ employ_datQ51.2Carer$Carer, data=employ_datQ51.2Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ51.2Disability<-multidatClean(Q51.2, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2Disability$Disability<- factor(employ_datQ51.2Disability$Disability)
employ_datQ51.2Disability<-ordinaldatClean47(employ_datQ51.2Disability$Q51.2, employ_datQ51.2Disability)
conTable <- xtabs(~Q51.2 + Disability, data = employ_datQ51.2Disability)
#ordinal(employ_datQ51.2Disability$CatOutcome, employ_datQ51.2Disability$Disability, employ_datQ51.2Disability)
prep <- analysisPrep(employ_datQ51.2Disability$CatOutcome, employ_datQ51.2Disability$Disability, employ_datQ51.2Disability)
analysis <- polr(employ_datQ51.2Disability$CatOutcome ~ employ_datQ51.2Disability$Disability, data=employ_datQ51.2Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ51.2Ethnicity<-multidatClean(Q51.2, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2Ethnicity$Ethnicity<- factor(employ_datQ51.2Ethnicity$EthnicityCleaned)
employ_datQ51.2Ethnicity<-ordinaldatClean47(employ_datQ51.2Ethnicity$Q51.2, employ_datQ51.2Ethnicity)
conTable <- xtabs(~Q51.2 + EthnicityCleaned, data = employ_datQ51.2Ethnicity)
conTable
#ordinal(employ_datQ51.2Ethnicity$CatOutcome, employ_datQ51.2Ethnicity$EthnicityCleaned, employ_datQ51.2Ethnicity)
prep <- analysisPrep(employ_datQ51.2Ethnicity$CatOutcome, employ_datQ51.2Ethnicity$EthnicityCleaned, employ_datQ51.2Ethnicity)
analysis <- polr(employ_datQ51.2Ethnicity$CatOutcome ~ employ_datQ51.2Ethnicity$EthnicityCleaned, data=employ_datQ51.2Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ51.2FirstGen<-multidatClean(Q51.2, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2FirstGen$FirstGen<-factor(employ_datQ51.2FirstGen$FirstGen)
employ_datQ51.2FirstGen<-ordinaldatClean47(employ_datQ51.2FirstGen$Q51.2, employ_datQ51.2FirstGen)
#ordinal(employ_datQ51.2FirstGen$CatOutcome, employ_datQ51.2FirstGen$FirstGen, employ_datQ51.2FirstGen)
prep <- analysisPrep(employ_datQ51.2FirstGen$CatOutcome, employ_datQ51.2FirstGen$FirstGen, employ_datQ51.2FirstGen)
analysis <- polr(employ_datQ51.2FirstGen$CatOutcome ~ employ_datQ51.2FirstGen$FirstGen, data=employ_datQ51.2FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ51.2Gender<-multidatClean(Q51.2, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2Gender$Gender<-factor(employ_datQ51.2Gender$Gender)
employ_datQ51.2Gender<-ordinaldatClean47(employ_datQ51.2Gender$Q51.2, employ_datQ51.2Gender)
#ordinal(employ_datQ51.2Gender$CatOutcome, employ_datQ51.2Gender$Gender, employ_datQ51.2Gender)
prep <- analysisPrep(employ_datQ51.2Gender$CatOutcome, employ_datQ51.2Gender$Gender, employ_datQ51.2Gender)
analysis <- polr(employ_datQ51.2Gender$CatOutcome ~ employ_datQ51.2Gender$Gender, data=employ_datQ51.2Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ51.2Sexuality<-multidatClean(Q51.2, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.2Sexuality$Sexuality<-factor(employ_datQ51.2Sexuality$Sexuality)
employ_datQ51.2Sexuality<-ordinaldatClean47(employ_datQ51.2Sexuality$Q51.2, employ_datQ51.2Sexuality)
#ordinal(employ_datQ51.2Sexuality$CatOutcome, employ_datQ51.2Sexuality$Sexuality, employ_datQ51.2Sexuality)
prep <- analysisPrep(employ_datQ51.2Sexuality$CatOutcome, employ_datQ51.2Sexuality$Sexuality, employ_datQ51.2Sexuality)
analysis <- polr(employ_datQ51.2Sexuality$CatOutcome ~ employ_datQ51.2Sexuality$Sexuality, data=employ_datQ51.2Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.2"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q51.2_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q51.3 (using orddatclean47) How have you felt in yourself - depressed
#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q51.3 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ51.3<-multidatClean(Q51.3, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.3 + Academic, data = employ_datQ51.3)
detach(dat_long)
employ_datQ51.3<-ordinaldatClean47(employ_datQ51.3$Q51.3, employ_datQ51.3)
#ordinal(employ_datQ51.3$CatOutcome, employ_datQ51.3$Academic, employ_datQ51.3)
prep <- analysisPrep(employ_datQ51.3$CatOutcome, employ_datQ51.3$Academic, employ_datQ51.3)
analysis <- polr(employ_datQ51.3$CatOutcome ~ employ_datQ51.3$Academic, data=employ_datQ51.3, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q51.3"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ51.3College<-multidatClean(Q51.3, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q51.3 + Q3, data = employ_datQ51.3College)
conTable
detach(dat_long)
employ_datQ51.3College$Q3[(employ_datQ51.3College$Q3 == "Research Professional Staff")]="Other"
employ_datQ51.3College$Q3<- factor(employ_datQ51.3College$Q3)
employ_datQ51.3College<-ordinaldatClean47(employ_datQ51.3College$Q51.3, employ_datQ51.3College)
#ordinal(employ_datQ51.3College$CatOutcome, employ_datQ51.3College$Q3, employ_datQ51.3College)
prep <- analysisPrep(employ_datQ51.3College$CatOutcome, employ_datQ51.3College$Q3, employ_datQ51.3College)
analysis <- polr(employ_datQ51.3College$CatOutcome ~ employ_datQ51.3College$Q3, data=employ_datQ51.3College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ51.3Carer<-multidatClean(Q51.3, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3Carer$Carer<- factor(employ_datQ51.3Carer$Carer)
employ_datQ51.3Carer<-ordinaldatClean47(employ_datQ51.3Carer$Q51.3, employ_datQ51.3Carer)
#ordinal(employ_datQ51.3Carer$CatOutcome, employ_datQ51.3Carer$Carer, employ_datQ51.3Carer)
prep <- analysisPrep(employ_datQ51.3Carer$CatOutcome, employ_datQ51.3Carer$Carer, employ_datQ51.3Carer)
analysis <- polr(employ_datQ51.3Carer$CatOutcome ~ employ_datQ51.3Carer$Carer, data=employ_datQ51.3Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ51.3Disability<-multidatClean(Q51.3, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3Disability$Disability<- factor(employ_datQ51.3Disability$Disability)
employ_datQ51.3Disability<-ordinaldatClean47(employ_datQ51.3Disability$Q51.3, employ_datQ51.3Disability)
conTable <- xtabs(~Q51.3 + Disability, data = employ_datQ51.3Disability)
#ordinal(employ_datQ51.3Disability$CatOutcome, employ_datQ51.3Disability$Disability, employ_datQ51.3Disability)
prep <- analysisPrep(employ_datQ51.3Disability$CatOutcome, employ_datQ51.3Disability$Disability, employ_datQ51.3Disability)
analysis <- polr(employ_datQ51.3Disability$CatOutcome ~ employ_datQ51.3Disability$Disability, data=employ_datQ51.3Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ51.3Ethnicity<-multidatClean(Q51.3, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3Ethnicity$Ethnicity<- factor(employ_datQ51.3Ethnicity$EthnicityCleaned)
employ_datQ51.3Ethnicity<-ordinaldatClean47(employ_datQ51.3Ethnicity$Q51.3, employ_datQ51.3Ethnicity)
conTable <- xtabs(~Q51.3 + EthnicityCleaned, data = employ_datQ51.3Ethnicity)
conTable
#ordinal(employ_datQ51.3Ethnicity$CatOutcome, employ_datQ51.3Ethnicity$EthnicityCleaned, employ_datQ51.3Ethnicity)
prep <- analysisPrep(employ_datQ51.3Ethnicity$CatOutcome, employ_datQ51.3Ethnicity$EthnicityCleaned, employ_datQ51.3Ethnicity)
analysis <- polr(employ_datQ51.3Ethnicity$CatOutcome ~ employ_datQ51.3Ethnicity$EthnicityCleaned, data=employ_datQ51.3Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ51.3FirstGen<-multidatClean(Q51.3, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3FirstGen$FirstGen<-factor(employ_datQ51.3FirstGen$FirstGen)
employ_datQ51.3FirstGen<-ordinaldatClean47(employ_datQ51.3FirstGen$Q51.3, employ_datQ51.3FirstGen)
#ordinal(employ_datQ51.3FirstGen$CatOutcome, employ_datQ51.3FirstGen$FirstGen, employ_datQ51.3FirstGen)
prep <- analysisPrep(employ_datQ51.3FirstGen$CatOutcome, employ_datQ51.3FirstGen$FirstGen, employ_datQ51.3FirstGen)
analysis <- polr(employ_datQ51.3FirstGen$CatOutcome ~ employ_datQ51.3FirstGen$FirstGen, data=employ_datQ51.3FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ51.3Gender<-multidatClean(Q51.3, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3Gender$Gender<-factor(employ_datQ51.3Gender$Gender)
employ_datQ51.3Gender<-ordinaldatClean47(employ_datQ51.3Gender$Q51.3, employ_datQ51.3Gender)
#ordinal(employ_datQ51.3Gender$CatOutcome, employ_datQ51.3Gender$Gender, employ_datQ51.3Gender)
prep <- analysisPrep(employ_datQ51.3Gender$CatOutcome, employ_datQ51.3Gender$Gender, employ_datQ51.3Gender)
analysis <- polr(employ_datQ51.3Gender$CatOutcome ~ employ_datQ51.3Gender$Gender, data=employ_datQ51.3Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ51.3Sexuality<-multidatClean(Q51.3, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ51.3Sexuality$Sexuality<-factor(employ_datQ51.3Sexuality$Sexuality)
employ_datQ51.3Sexuality<-ordinaldatClean47(employ_datQ51.3Sexuality$Q51.3, employ_datQ51.3Sexuality)
#ordinal(employ_datQ51.3Sexuality$CatOutcome, employ_datQ51.3Sexuality$Sexuality, employ_datQ51.3Sexuality)
prep <- analysisPrep(employ_datQ51.3Sexuality$CatOutcome, employ_datQ51.3Sexuality$Sexuality, employ_datQ51.3Sexuality)
analysis <- polr(employ_datQ51.3Sexuality$CatOutcome ~ employ_datQ51.3Sexuality$Sexuality, data=employ_datQ51.3Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q51.3"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q51.3_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)


### Q52.1.a (using orddatclean52) Are there aspects of working from home which you would like to be able to continue? Use of collaborrative software
"Q52.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ52.1.<-multidatClean(Q52.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q52.1.a + Academic, data = employ_datQ52.1.)
detach(dat_long)
employ_datQ52.1.<-ordinaldatClean52(employ_datQ52.1.$Q52.1.a, employ_datQ52.1.)
#ordinal(employ_datQ52.1.$CatOutcome, employ_datQ52.1.$Academic, employ_datQ52.1.)
prep <- analysisPrep(employ_datQ52.1.$CatOutcome, employ_datQ52.1.$Academic, employ_datQ52.1.)
analysis <- polr(employ_datQ52.1.$CatOutcome ~ employ_datQ52.1.$Academic, data=employ_datQ52.1., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q52.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ52.1.College<-multidatClean(Q52.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q52.1.a + Q3, data = employ_datQ52.1.College)
conTable
detach(dat_long)
employ_datQ52.1.College$Q3[(employ_datQ52.1.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ52.1.College$Q3<- factor(employ_datQ52.1.College$Q3)
employ_datQ52.1.College<-ordinaldatClean52(employ_datQ52.1.College$Q52.1.a, employ_datQ52.1.College)
#ordinal(employ_datQ52.1.College$CatOutcome, employ_datQ52.1.College$Q3, employ_datQ52.1.College)
prep <- analysisPrep(employ_datQ52.1.College$CatOutcome, employ_datQ52.1.College$Q3, employ_datQ52.1.College)
analysis <- polr(employ_datQ52.1.College$CatOutcome ~ employ_datQ52.1.College$Q3, data=employ_datQ52.1.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ52.1.Carer<-multidatClean(Q52.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.Carer$Carer<- factor(employ_datQ52.1.Carer$Carer)
employ_datQ52.1.Carer<-ordinaldatClean52(employ_datQ52.1.Carer$Q52.1.a, employ_datQ52.1.Carer)
#ordinal(employ_datQ52.1.Carer$CatOutcome, employ_datQ52.1.Carer$Carer, employ_datQ52.1.Carer)
prep <- analysisPrep(employ_datQ52.1.Carer$CatOutcome, employ_datQ52.1.Carer$Carer, employ_datQ52.1.Carer)
analysis <- polr(employ_datQ52.1.Carer$CatOutcome ~ employ_datQ52.1.Carer$Carer, data=employ_datQ52.1.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ52.1.Disability<-multidatClean(Q52.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.Disability$Disability<- factor(employ_datQ52.1.Disability$Disability)
employ_datQ52.1.Disability<-ordinaldatClean52(employ_datQ52.1.Disability$Q52.1.a, employ_datQ52.1.Disability)
conTable <- xtabs(~Q52.1.a + Disability, data = employ_datQ52.1.Disability)
#ordinal(employ_datQ52.1.Disability$CatOutcome, employ_datQ52.1.Disability$Disability, employ_datQ52.1.Disability)
prep <- analysisPrep(employ_datQ52.1.Disability$CatOutcome, employ_datQ52.1.Disability$Disability, employ_datQ52.1.Disability)
analysis <- polr(employ_datQ52.1.Disability$CatOutcome ~ employ_datQ52.1.Disability$Disability, data=employ_datQ52.1.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ52.1.Ethnicity<-multidatClean(Q52.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.Ethnicity$Ethnicity<- factor(employ_datQ52.1.Ethnicity$EthnicityCleaned)
employ_datQ52.1.Ethnicity<-ordinaldatClean52(employ_datQ52.1.Ethnicity$Q52.1.a, employ_datQ52.1.Ethnicity)
conTable <- xtabs(~Q52.1.a + EthnicityCleaned, data = employ_datQ52.1.Ethnicity)
conTable
#ordinal(employ_datQ52.1.Ethnicity$CatOutcome, employ_datQ52.1.Ethnicity$EthnicityCleaned, employ_datQ52.1.Ethnicity)
prep <- analysisPrep(employ_datQ52.1.Ethnicity$CatOutcome, employ_datQ52.1.Ethnicity$EthnicityCleaned, employ_datQ52.1.Ethnicity)
analysis <- polr(employ_datQ52.1.Ethnicity$CatOutcome ~ employ_datQ52.1.Ethnicity$EthnicityCleaned, data=employ_datQ52.1.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ52.1.FirstGen<-multidatClean(Q52.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.FirstGen$FirstGen<-factor(employ_datQ52.1.FirstGen$FirstGen)
employ_datQ52.1.FirstGen<-ordinaldatClean52(employ_datQ52.1.FirstGen$Q52.1.a, employ_datQ52.1.FirstGen)
#ordinal(employ_datQ52.1.FirstGen$CatOutcome, employ_datQ52.1.FirstGen$FirstGen, employ_datQ52.1.FirstGen)
prep <- analysisPrep(employ_datQ52.1.FirstGen$CatOutcome, employ_datQ52.1.FirstGen$FirstGen, employ_datQ52.1.FirstGen)
analysis <- polr(employ_datQ52.1.FirstGen$CatOutcome ~ employ_datQ52.1.FirstGen$FirstGen, data=employ_datQ52.1.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ52.1.Gender<-multidatClean(Q52.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.Gender$Gender<-factor(employ_datQ52.1.Gender$Gender)
employ_datQ52.1.Gender<-ordinaldatClean52(employ_datQ52.1.Gender$Q52.1.a, employ_datQ52.1.Gender)
#ordinal(employ_datQ52.1.Gender$CatOutcome, employ_datQ52.1.Gender$Gender, employ_datQ52.1.Gender)
prep <- analysisPrep(employ_datQ52.1.Gender$CatOutcome, employ_datQ52.1.Gender$Gender, employ_datQ52.1.Gender)
analysis <- polr(employ_datQ52.1.Gender$CatOutcome ~ employ_datQ52.1.Gender$Gender, data=employ_datQ52.1.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ52.1.Sexuality<-multidatClean(Q52.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.1.Sexuality$Sexuality<-factor(employ_datQ52.1.Sexuality$Sexuality)
employ_datQ52.1.Sexuality<-ordinaldatClean52(employ_datQ52.1.Sexuality$Q52.1.a, employ_datQ52.1.Sexuality)
#ordinal(employ_datQ52.1.Sexuality$CatOutcome, employ_datQ52.1.Sexuality$Sexuality, employ_datQ52.1.Sexuality)
prep <- analysisPrep(employ_datQ52.1.Sexuality$CatOutcome, employ_datQ52.1.Sexuality$Sexuality, employ_datQ52.1.Sexuality)
analysis <- polr(employ_datQ52.1.Sexuality$CatOutcome ~ employ_datQ52.1.Sexuality$Sexuality, data=employ_datQ52.1.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q52.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q52.2.a (using orddatclean52) Are there aspects of working from home which you would like to be able to continue? Use of virtual meetings
"Q52.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ52.2.<-multidatClean(Q52.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q52.2.a + Academic, data = employ_datQ52.2.)
detach(dat_long)
employ_datQ52.2.<-ordinaldatClean52(employ_datQ52.2.$Q52.2.a, employ_datQ52.2.)
#ordinal(employ_datQ52.2.$CatOutcome, employ_datQ52.2.$Academic, employ_datQ52.2.)
prep <- analysisPrep(employ_datQ52.2.$CatOutcome, employ_datQ52.2.$Academic, employ_datQ52.2.)
analysis <- polr(employ_datQ52.2.$CatOutcome ~ employ_datQ52.2.$Academic, data=employ_datQ52.2., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q52.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ52.2.College<-multidatClean(Q52.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q52.2.a + Q3, data = employ_datQ52.2.College)
conTable
detach(dat_long)
employ_datQ52.2.College$Q3[(employ_datQ52.2.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ52.2.College$Q3<- factor(employ_datQ52.2.College$Q3)
employ_datQ52.2.College<-ordinaldatClean52(employ_datQ52.2.College$Q52.2.a, employ_datQ52.2.College)
#ordinal(employ_datQ52.2.College$CatOutcome, employ_datQ52.2.College$Q3, employ_datQ52.2.College)
prep <- analysisPrep(employ_datQ52.2.College$CatOutcome, employ_datQ52.2.College$Q3, employ_datQ52.2.College)
analysis <- polr(employ_datQ52.2.College$CatOutcome ~ employ_datQ52.2.College$Q3, data=employ_datQ52.2.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ52.2.Carer<-multidatClean(Q52.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.Carer$Carer<- factor(employ_datQ52.2.Carer$Carer)
employ_datQ52.2.Carer<-ordinaldatClean52(employ_datQ52.2.Carer$Q52.2.a, employ_datQ52.2.Carer)
#ordinal(employ_datQ52.2.Carer$CatOutcome, employ_datQ52.2.Carer$Carer, employ_datQ52.2.Carer)
prep <- analysisPrep(employ_datQ52.2.Carer$CatOutcome, employ_datQ52.2.Carer$Carer, employ_datQ52.2.Carer)
analysis <- polr(employ_datQ52.2.Carer$CatOutcome ~ employ_datQ52.2.Carer$Carer, data=employ_datQ52.2.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ52.2.Disability<-multidatClean(Q52.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.Disability$Disability<- factor(employ_datQ52.2.Disability$Disability)
employ_datQ52.2.Disability<-ordinaldatClean52(employ_datQ52.2.Disability$Q52.2.a, employ_datQ52.2.Disability)
conTable <- xtabs(~Q52.2.a + Disability, data = employ_datQ52.2.Disability)
#ordinal(employ_datQ52.2.Disability$CatOutcome, employ_datQ52.2.Disability$Disability, employ_datQ52.2.Disability)
prep <- analysisPrep(employ_datQ52.2.Disability$CatOutcome, employ_datQ52.2.Disability$Disability, employ_datQ52.2.Disability)
analysis <- polr(employ_datQ52.2.Disability$CatOutcome ~ employ_datQ52.2.Disability$Disability, data=employ_datQ52.2.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ52.2.Ethnicity<-multidatClean(Q52.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.Ethnicity$Ethnicity<- factor(employ_datQ52.2.Ethnicity$EthnicityCleaned)
employ_datQ52.2.Ethnicity<-ordinaldatClean52(employ_datQ52.2.Ethnicity$Q52.2.a, employ_datQ52.2.Ethnicity)
conTable <- xtabs(~Q52.2.a + EthnicityCleaned, data = employ_datQ52.2.Ethnicity)
conTable
#ordinal(employ_datQ52.2.Ethnicity$CatOutcome, employ_datQ52.2.Ethnicity$EthnicityCleaned, employ_datQ52.2.Ethnicity)
prep <- analysisPrep(employ_datQ52.2.Ethnicity$CatOutcome, employ_datQ52.2.Ethnicity$EthnicityCleaned, employ_datQ52.2.Ethnicity)
analysis <- polr(employ_datQ52.2.Ethnicity$CatOutcome ~ employ_datQ52.2.Ethnicity$EthnicityCleaned, data=employ_datQ52.2.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ52.2.FirstGen<-multidatClean(Q52.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.FirstGen$FirstGen<-factor(employ_datQ52.2.FirstGen$FirstGen)
employ_datQ52.2.FirstGen<-ordinaldatClean52(employ_datQ52.2.FirstGen$Q52.2.a, employ_datQ52.2.FirstGen)
#ordinal(employ_datQ52.2.FirstGen$CatOutcome, employ_datQ52.2.FirstGen$FirstGen, employ_datQ52.2.FirstGen)
prep <- analysisPrep(employ_datQ52.2.FirstGen$CatOutcome, employ_datQ52.2.FirstGen$FirstGen, employ_datQ52.2.FirstGen)
analysis <- polr(employ_datQ52.2.FirstGen$CatOutcome ~ employ_datQ52.2.FirstGen$FirstGen, data=employ_datQ52.2.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ52.2.Gender<-multidatClean(Q52.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.Gender$Gender<-factor(employ_datQ52.2.Gender$Gender)
employ_datQ52.2.Gender<-ordinaldatClean52(employ_datQ52.2.Gender$Q52.2.a, employ_datQ52.2.Gender)
#ordinal(employ_datQ52.2.Gender$CatOutcome, employ_datQ52.2.Gender$Gender, employ_datQ52.2.Gender)
prep <- analysisPrep(employ_datQ52.2.Gender$CatOutcome, employ_datQ52.2.Gender$Gender, employ_datQ52.2.Gender)
analysis <- polr(employ_datQ52.2.Gender$CatOutcome ~ employ_datQ52.2.Gender$Gender, data=employ_datQ52.2.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ52.2.Sexuality<-multidatClean(Q52.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ52.2.Sexuality$Sexuality<-factor(employ_datQ52.2.Sexuality$Sexuality)
employ_datQ52.2.Sexuality<-ordinaldatClean52(employ_datQ52.2.Sexuality$Q52.2.a, employ_datQ52.2.Sexuality)
#ordinal(employ_datQ52.2.Sexuality$CatOutcome, employ_datQ52.2.Sexuality$Sexuality, employ_datQ52.2.Sexuality)
prep <- analysisPrep(employ_datQ52.2.Sexuality$CatOutcome, employ_datQ52.2.Sexuality$Sexuality, employ_datQ52.2.Sexuality)
analysis <- polr(employ_datQ52.2.Sexuality$CatOutcome ~ employ_datQ52.2.Sexuality$Sexuality, data=employ_datQ52.2.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q52.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q52.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q53 (using orddatclean53) If working from home was an option in the future, would you see yourself doing this
"Q53 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ53.<-multidatClean(Q53, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q53 + Academic, data = employ_datQ53.)
detach(dat_long)
employ_datQ53.<-ordinaldatClean53(employ_datQ53.$Q53, employ_datQ53.)
#ordinal(employ_datQ53.$CatOutcome, employ_datQ53.$Academic, employ_datQ53.)
prep <- analysisPrep(employ_datQ53.$CatOutcome, employ_datQ53.$Academic, employ_datQ53.)
analysis <- polr(employ_datQ53.$CatOutcome ~ employ_datQ53.$Academic, data=employ_datQ53., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q53"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ53.College<-multidatClean(Q53, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q53 + Q3, data = employ_datQ53.College)
conTable
detach(dat_long)
employ_datQ53.College$Q3[(employ_datQ53.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ53.College$Q3<- factor(employ_datQ53.College$Q3)
employ_datQ53.College<-ordinaldatClean53(employ_datQ53.College$Q53, employ_datQ53.College)
#ordinal(employ_datQ53.College$CatOutcome, employ_datQ53.College$Q3, employ_datQ53.College)
prep <- analysisPrep(employ_datQ53.College$CatOutcome, employ_datQ53.College$Q3, employ_datQ53.College)
analysis <- polr(employ_datQ53.College$CatOutcome ~ employ_datQ53.College$Q3, data=employ_datQ53.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ53.Carer<-multidatClean(Q53, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.Carer$Carer<- factor(employ_datQ53.Carer$Carer)
employ_datQ53.Carer<-ordinaldatClean53(employ_datQ53.Carer$Q53, employ_datQ53.Carer)
#ordinal(employ_datQ53.Carer$CatOutcome, employ_datQ53.Carer$Carer, employ_datQ53.Carer)
prep <- analysisPrep(employ_datQ53.Carer$CatOutcome, employ_datQ53.Carer$Carer, employ_datQ53.Carer)
analysis <- polr(employ_datQ53.Carer$CatOutcome ~ employ_datQ53.Carer$Carer, data=employ_datQ53.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ53.Disability<-multidatClean(Q53, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.Disability$Disability<- factor(employ_datQ53.Disability$Disability)
employ_datQ53.Disability<-ordinaldatClean53(employ_datQ53.Disability$Q53, employ_datQ53.Disability)
conTable <- xtabs(~Q53 + Disability, data = employ_datQ53.Disability)
#ordinal(employ_datQ53.Disability$CatOutcome, employ_datQ53.Disability$Disability, employ_datQ53.Disability)
prep <- analysisPrep(employ_datQ53.Disability$CatOutcome, employ_datQ53.Disability$Disability, employ_datQ53.Disability)
analysis <- polr(employ_datQ53.Disability$CatOutcome ~ employ_datQ53.Disability$Disability, data=employ_datQ53.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ53.Ethnicity<-multidatClean(Q53, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.Ethnicity$Ethnicity<- factor(employ_datQ53.Ethnicity$EthnicityCleaned)
employ_datQ53.Ethnicity<-ordinaldatClean53(employ_datQ53.Ethnicity$Q53, employ_datQ53.Ethnicity)
conTable <- xtabs(~Q53 + EthnicityCleaned, data = employ_datQ53.Ethnicity)
conTable
#ordinal(employ_datQ53.Ethnicity$CatOutcome, employ_datQ53.Ethnicity$EthnicityCleaned, employ_datQ53.Ethnicity)
prep <- analysisPrep(employ_datQ53.Ethnicity$CatOutcome, employ_datQ53.Ethnicity$EthnicityCleaned, employ_datQ53.Ethnicity)
analysis <- polr(employ_datQ53.Ethnicity$CatOutcome ~ employ_datQ53.Ethnicity$EthnicityCleaned, data=employ_datQ53.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ53.FirstGen<-multidatClean(Q53, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.FirstGen$FirstGen<-factor(employ_datQ53.FirstGen$FirstGen)
employ_datQ53.FirstGen<-ordinaldatClean53(employ_datQ53.FirstGen$Q53, employ_datQ53.FirstGen)
#ordinal(employ_datQ53.FirstGen$CatOutcome, employ_datQ53.FirstGen$FirstGen, employ_datQ53.FirstGen)
prep <- analysisPrep(employ_datQ53.FirstGen$CatOutcome, employ_datQ53.FirstGen$FirstGen, employ_datQ53.FirstGen)
analysis <- polr(employ_datQ53.FirstGen$CatOutcome ~ employ_datQ53.FirstGen$FirstGen, data=employ_datQ53.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ53.Gender<-multidatClean(Q53, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.Gender$Gender<-factor(employ_datQ53.Gender$Gender)
employ_datQ53.Gender<-ordinaldatClean53(employ_datQ53.Gender$Q53, employ_datQ53.Gender)
#ordinal(employ_datQ53.Gender$CatOutcome, employ_datQ53.Gender$Gender, employ_datQ53.Gender)
prep <- analysisPrep(employ_datQ53.Gender$CatOutcome, employ_datQ53.Gender$Gender, employ_datQ53.Gender)
analysis <- polr(employ_datQ53.Gender$CatOutcome ~ employ_datQ53.Gender$Gender, data=employ_datQ53.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ53.Sexuality<-multidatClean(Q53, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ53.Sexuality$Sexuality<-factor(employ_datQ53.Sexuality$Sexuality)
employ_datQ53.Sexuality<-ordinaldatClean53(employ_datQ53.Sexuality$Q53, employ_datQ53.Sexuality)
#ordinal(employ_datQ53.Sexuality$CatOutcome, employ_datQ53.Sexuality$Sexuality, employ_datQ53.Sexuality)
prep <- analysisPrep(employ_datQ53.Sexuality$CatOutcome, employ_datQ53.Sexuality$Sexuality, employ_datQ53.Sexuality)
analysis <- polr(employ_datQ53.Sexuality$CatOutcome ~ employ_datQ53.Sexuality$Sexuality, data=employ_datQ53.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q53"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q53_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.1 (using orddatclean54.1) Where shift patterns are introduced to reduce numbers in work at any time, giving priority to those with caring responsibilities in allocation of shifts
"Q54.1 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.1.<-multidatClean(Q54.1, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.1 + Academic, data = employ_datQ54.1.)
detach(dat_long)
employ_datQ54.1.<-ordinaldatClean54(employ_datQ54.1.$Q54.1, employ_datQ54.1.)
#ordinal(employ_datQ54.1.$CatOutcome, employ_datQ54.1.$Academic, employ_datQ54.1.)
prep <- analysisPrep(employ_datQ54.1.$CatOutcome, employ_datQ54.1.$Academic, employ_datQ54.1.)
analysis <- polr(employ_datQ54.1.$CatOutcome ~ employ_datQ54.1.$Academic, data=employ_datQ54.1., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.1"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.1.College<-multidatClean(Q54.1, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.1 + Q3, data = employ_datQ54.1.College)
conTable
detach(dat_long)
employ_datQ54.1.College$Q3[(employ_datQ54.1.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.1.College$Q3<- factor(employ_datQ54.1.College$Q3)
employ_datQ54.1.College<-ordinaldatClean54(employ_datQ54.1.College$Q54.1, employ_datQ54.1.College)
#ordinal(employ_datQ54.1.College$CatOutcome, employ_datQ54.1.College$Q3, employ_datQ54.1.College)
prep <- analysisPrep(employ_datQ54.1.College$CatOutcome, employ_datQ54.1.College$Q3, employ_datQ54.1.College)
analysis <- polr(employ_datQ54.1.College$CatOutcome ~ employ_datQ54.1.College$Q3, data=employ_datQ54.1.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.1.Carer<-multidatClean(Q54.1, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.Carer$Carer<- factor(employ_datQ54.1.Carer$Carer)
employ_datQ54.1.Carer<-ordinaldatClean54(employ_datQ54.1.Carer$Q54.1, employ_datQ54.1.Carer)
#ordinal(employ_datQ54.1.Carer$CatOutcome, employ_datQ54.1.Carer$Carer, employ_datQ54.1.Carer)
prep <- analysisPrep(employ_datQ54.1.Carer$CatOutcome, employ_datQ54.1.Carer$Carer, employ_datQ54.1.Carer)
analysis <- polr(employ_datQ54.1.Carer$CatOutcome ~ employ_datQ54.1.Carer$Carer, data=employ_datQ54.1.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.1.Disability<-multidatClean(Q54.1, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.Disability$Disability<- factor(employ_datQ54.1.Disability$Disability)
employ_datQ54.1.Disability<-ordinaldatClean54(employ_datQ54.1.Disability$Q54.1, employ_datQ54.1.Disability)
conTable <- xtabs(~Q54.1 + Disability, data = employ_datQ54.1.Disability)
#ordinal(employ_datQ54.1.Disability$CatOutcome, employ_datQ54.1.Disability$Disability, employ_datQ54.1.Disability)
prep <- analysisPrep(employ_datQ54.1.Disability$CatOutcome, employ_datQ54.1.Disability$Disability, employ_datQ54.1.Disability)
analysis <- polr(employ_datQ54.1.Disability$CatOutcome ~ employ_datQ54.1.Disability$Disability, data=employ_datQ54.1.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.1.Ethnicity<-multidatClean(Q54.1, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.Ethnicity$Ethnicity<- factor(employ_datQ54.1.Ethnicity$EthnicityCleaned)
employ_datQ54.1.Ethnicity<-ordinaldatClean54(employ_datQ54.1.Ethnicity$Q54.1, employ_datQ54.1.Ethnicity)
conTable <- xtabs(~Q54.1 + EthnicityCleaned, data = employ_datQ54.1.Ethnicity)
conTable
#ordinal(employ_datQ54.1.Ethnicity$CatOutcome, employ_datQ54.1.Ethnicity$EthnicityCleaned, employ_datQ54.1.Ethnicity)
prep <- analysisPrep(employ_datQ54.1.Ethnicity$CatOutcome, employ_datQ54.1.Ethnicity$EthnicityCleaned, employ_datQ54.1.Ethnicity)
analysis <- polr(employ_datQ54.1.Ethnicity$CatOutcome ~ employ_datQ54.1.Ethnicity$EthnicityCleaned, data=employ_datQ54.1.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.1.FirstGen<-multidatClean(Q54.1, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.FirstGen$FirstGen<-factor(employ_datQ54.1.FirstGen$FirstGen)
employ_datQ54.1.FirstGen<-ordinaldatClean54(employ_datQ54.1.FirstGen$Q54.1, employ_datQ54.1.FirstGen)
#ordinal(employ_datQ54.1.FirstGen$CatOutcome, employ_datQ54.1.FirstGen$FirstGen, employ_datQ54.1.FirstGen)
prep <- analysisPrep(employ_datQ54.1.FirstGen$CatOutcome, employ_datQ54.1.FirstGen$FirstGen, employ_datQ54.1.FirstGen)
analysis <- polr(employ_datQ54.1.FirstGen$CatOutcome ~ employ_datQ54.1.FirstGen$FirstGen, data=employ_datQ54.1.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.1.Gender<-multidatClean(Q54.1, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.Gender$Gender<-factor(employ_datQ54.1.Gender$Gender)
employ_datQ54.1.Gender<-ordinaldatClean54(employ_datQ54.1.Gender$Q54.1, employ_datQ54.1.Gender)
#ordinal(employ_datQ54.1.Gender$CatOutcome, employ_datQ54.1.Gender$Gender, employ_datQ54.1.Gender)
prep <- analysisPrep(employ_datQ54.1.Gender$CatOutcome, employ_datQ54.1.Gender$Gender, employ_datQ54.1.Gender)
analysis <- polr(employ_datQ54.1.Gender$CatOutcome ~ employ_datQ54.1.Gender$Gender, data=employ_datQ54.1.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.1.Sexuality<-multidatClean(Q54.1, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.1.Sexuality$Sexuality<-factor(employ_datQ54.1.Sexuality$Sexuality)
employ_datQ54.1.Sexuality<-ordinaldatClean54(employ_datQ54.1.Sexuality$Q54.1, employ_datQ54.1.Sexuality)
#ordinal(employ_datQ54.1.Sexuality$CatOutcome, employ_datQ54.1.Sexuality$Sexuality, employ_datQ54.1.Sexuality)
prep <- analysisPrep(employ_datQ54.1.Sexuality$CatOutcome, employ_datQ54.1.Sexuality$Sexuality, employ_datQ54.1.Sexuality)
analysis <- polr(employ_datQ54.1.Sexuality$CatOutcome ~ employ_datQ54.1.Sexuality$Sexuality, data=employ_datQ54.1.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.1"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.2 (using orddatclean54) Availability of key research resources (animal facilities, shared equipment, library facilities)?#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
"Q54.2 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.2.<-multidatClean(Q54.2, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.2 + Academic, data = employ_datQ54.2.)
detach(dat_long)
employ_datQ54.2.<-ordinaldatClean54(employ_datQ54.2.$Q54.2, employ_datQ54.2.)
#ordinal(employ_datQ54.2.$CatOutcome, employ_datQ54.2.$Academic, employ_datQ54.2.)
prep <- analysisPrep(employ_datQ54.2.$CatOutcome, employ_datQ54.2.$Academic, employ_datQ54.2.)
analysis <- polr(employ_datQ54.2.$CatOutcome ~ employ_datQ54.2.$Academic, data=employ_datQ54.2., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.2"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.2.College<-multidatClean(Q54.2, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.2 + Q3, data = employ_datQ54.2.College)
conTable
detach(dat_long)
employ_datQ54.2.College$Q3[(employ_datQ54.2.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.2.College$Q3<- factor(employ_datQ54.2.College$Q3)
employ_datQ54.2.College<-ordinaldatClean54(employ_datQ54.2.College$Q54.2, employ_datQ54.2.College)
#ordinal(employ_datQ54.2.College$CatOutcome, employ_datQ54.2.College$Q3, employ_datQ54.2.College)
prep <- analysisPrep(employ_datQ54.2.College$CatOutcome, employ_datQ54.2.College$Q3, employ_datQ54.2.College)
analysis <- polr(employ_datQ54.2.College$CatOutcome ~ employ_datQ54.2.College$Q3, data=employ_datQ54.2.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.2.Carer<-multidatClean(Q54.2, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.Carer$Carer<- factor(employ_datQ54.2.Carer$Carer)
employ_datQ54.2.Carer<-ordinaldatClean54(employ_datQ54.2.Carer$Q54.2, employ_datQ54.2.Carer)
#ordinal(employ_datQ54.2.Carer$CatOutcome, employ_datQ54.2.Carer$Carer, employ_datQ54.2.Carer)
prep <- analysisPrep(employ_datQ54.2.Carer$CatOutcome, employ_datQ54.2.Carer$Carer, employ_datQ54.2.Carer)
analysis <- polr(employ_datQ54.2.Carer$CatOutcome ~ employ_datQ54.2.Carer$Carer, data=employ_datQ54.2.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.2.Disability<-multidatClean(Q54.2, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.Disability$Disability<- factor(employ_datQ54.2.Disability$Disability)
employ_datQ54.2.Disability<-ordinaldatClean54(employ_datQ54.2.Disability$Q54.2, employ_datQ54.2.Disability)
conTable <- xtabs(~Q54.2 + Disability, data = employ_datQ54.2.Disability)
#ordinal(employ_datQ54.2.Disability$CatOutcome, employ_datQ54.2.Disability$Disability, employ_datQ54.2.Disability)
prep <- analysisPrep(employ_datQ54.2.Disability$CatOutcome, employ_datQ54.2.Disability$Disability, employ_datQ54.2.Disability)
analysis <- polr(employ_datQ54.2.Disability$CatOutcome ~ employ_datQ54.2.Disability$Disability, data=employ_datQ54.2.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.2.Ethnicity<-multidatClean(Q54.2, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.Ethnicity$Ethnicity<- factor(employ_datQ54.2.Ethnicity$EthnicityCleaned)
employ_datQ54.2.Ethnicity<-ordinaldatClean54(employ_datQ54.2.Ethnicity$Q54.2, employ_datQ54.2.Ethnicity)
conTable <- xtabs(~Q54.2 + EthnicityCleaned, data = employ_datQ54.2.Ethnicity)
conTable
#ordinal(employ_datQ54.2.Ethnicity$CatOutcome, employ_datQ54.2.Ethnicity$EthnicityCleaned, employ_datQ54.2.Ethnicity)
prep <- analysisPrep(employ_datQ54.2.Ethnicity$CatOutcome, employ_datQ54.2.Ethnicity$EthnicityCleaned, employ_datQ54.2.Ethnicity)
analysis <- polr(employ_datQ54.2.Ethnicity$CatOutcome ~ employ_datQ54.2.Ethnicity$EthnicityCleaned, data=employ_datQ54.2.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.2.FirstGen<-multidatClean(Q54.2, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.FirstGen$FirstGen<-factor(employ_datQ54.2.FirstGen$FirstGen)
employ_datQ54.2.FirstGen<-ordinaldatClean54(employ_datQ54.2.FirstGen$Q54.2, employ_datQ54.2.FirstGen)
#ordinal(employ_datQ54.2.FirstGen$CatOutcome, employ_datQ54.2.FirstGen$FirstGen, employ_datQ54.2.FirstGen)
prep <- analysisPrep(employ_datQ54.2.FirstGen$CatOutcome, employ_datQ54.2.FirstGen$FirstGen, employ_datQ54.2.FirstGen)
analysis <- polr(employ_datQ54.2.FirstGen$CatOutcome ~ employ_datQ54.2.FirstGen$FirstGen, data=employ_datQ54.2.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.2.Gender<-multidatClean(Q54.2, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.Gender$Gender<-factor(employ_datQ54.2.Gender$Gender)
employ_datQ54.2.Gender<-ordinaldatClean54(employ_datQ54.2.Gender$Q54.2, employ_datQ54.2.Gender)
#ordinal(employ_datQ54.2.Gender$CatOutcome, employ_datQ54.2.Gender$Gender, employ_datQ54.2.Gender)
prep <- analysisPrep(employ_datQ54.2.Gender$CatOutcome, employ_datQ54.2.Gender$Gender, employ_datQ54.2.Gender)
analysis <- polr(employ_datQ54.2.Gender$CatOutcome ~ employ_datQ54.2.Gender$Gender, data=employ_datQ54.2.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.2.Sexuality<-multidatClean(Q54.2, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.2.Sexuality$Sexuality<-factor(employ_datQ54.2.Sexuality$Sexuality)
employ_datQ54.2.Sexuality<-ordinaldatClean54(employ_datQ54.2.Sexuality$Q54.2, employ_datQ54.2.Sexuality)
#ordinal(employ_datQ54.2.Sexuality$CatOutcome, employ_datQ54.2.Sexuality$Sexuality, employ_datQ54.2.Sexuality)
prep <- analysisPrep(employ_datQ54.2.Sexuality$CatOutcome, employ_datQ54.2.Sexuality$Sexuality, employ_datQ54.2.Sexuality)
analysis <- polr(employ_datQ54.2.Sexuality$CatOutcome ~ employ_datQ54.2.Sexuality$Sexuality, data=employ_datQ54.2.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.2"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.2_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.3 (using orddatclean54) The return of researchers for whom access to facilities or buildings is especially crucial for career development?
"Q54.3 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.3.<-multidatClean(Q54.3, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.3 + Academic, data = employ_datQ54.3.)
detach(dat_long)
employ_datQ54.3.<-ordinaldatClean54(employ_datQ54.3.$Q54.3, employ_datQ54.3.)
#ordinal(employ_datQ54.3.$CatOutcome, employ_datQ54.3.$Academic, employ_datQ54.3.)
prep <- analysisPrep(employ_datQ54.3.$CatOutcome, employ_datQ54.3.$Academic, employ_datQ54.3.)
analysis <- polr(employ_datQ54.3.$CatOutcome ~ employ_datQ54.3.$Academic, data=employ_datQ54.3., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.3"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.3.College<-multidatClean(Q54.3, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.3 + Q3, data = employ_datQ54.3.College)
conTable
detach(dat_long)
employ_datQ54.3.College$Q3[(employ_datQ54.3.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.3.College$Q3<- factor(employ_datQ54.3.College$Q3)
employ_datQ54.3.College<-ordinaldatClean54(employ_datQ54.3.College$Q54.3, employ_datQ54.3.College)
#ordinal(employ_datQ54.3.College$CatOutcome, employ_datQ54.3.College$Q3, employ_datQ54.3.College)
prep <- analysisPrep(employ_datQ54.3.College$CatOutcome, employ_datQ54.3.College$Q3, employ_datQ54.3.College)
analysis <- polr(employ_datQ54.3.College$CatOutcome ~ employ_datQ54.3.College$Q3, data=employ_datQ54.3.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.3.Carer<-multidatClean(Q54.3, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.Carer$Carer<- factor(employ_datQ54.3.Carer$Carer)
employ_datQ54.3.Carer<-ordinaldatClean54(employ_datQ54.3.Carer$Q54.3, employ_datQ54.3.Carer)
#ordinal(employ_datQ54.3.Carer$CatOutcome, employ_datQ54.3.Carer$Carer, employ_datQ54.3.Carer)
prep <- analysisPrep(employ_datQ54.3.Carer$CatOutcome, employ_datQ54.3.Carer$Carer, employ_datQ54.3.Carer)
analysis <- polr(employ_datQ54.3.Carer$CatOutcome ~ employ_datQ54.3.Carer$Carer, data=employ_datQ54.3.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.3.Disability<-multidatClean(Q54.3, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.Disability$Disability<- factor(employ_datQ54.3.Disability$Disability)
employ_datQ54.3.Disability<-ordinaldatClean54(employ_datQ54.3.Disability$Q54.3, employ_datQ54.3.Disability)
conTable <- xtabs(~Q54.3 + Disability, data = employ_datQ54.3.Disability)
#ordinal(employ_datQ54.3.Disability$CatOutcome, employ_datQ54.3.Disability$Disability, employ_datQ54.3.Disability)
prep <- analysisPrep(employ_datQ54.3.Disability$CatOutcome, employ_datQ54.3.Disability$Disability, employ_datQ54.3.Disability)
analysis <- polr(employ_datQ54.3.Disability$CatOutcome ~ employ_datQ54.3.Disability$Disability, data=employ_datQ54.3.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.3.Ethnicity<-multidatClean(Q54.3, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.Ethnicity$Ethnicity<- factor(employ_datQ54.3.Ethnicity$EthnicityCleaned)
employ_datQ54.3.Ethnicity<-ordinaldatClean54(employ_datQ54.3.Ethnicity$Q54.3, employ_datQ54.3.Ethnicity)
conTable <- xtabs(~Q54.3 + EthnicityCleaned, data = employ_datQ54.3.Ethnicity)
conTable
#ordinal(employ_datQ54.3.Ethnicity$CatOutcome, employ_datQ54.3.Ethnicity$EthnicityCleaned, employ_datQ54.3.Ethnicity)
prep <- analysisPrep(employ_datQ54.3.Ethnicity$CatOutcome, employ_datQ54.3.Ethnicity$EthnicityCleaned, employ_datQ54.3.Ethnicity)
analysis <- polr(employ_datQ54.3.Ethnicity$CatOutcome ~ employ_datQ54.3.Ethnicity$EthnicityCleaned, data=employ_datQ54.3.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.3.FirstGen<-multidatClean(Q54.3, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.FirstGen$FirstGen<-factor(employ_datQ54.3.FirstGen$FirstGen)
employ_datQ54.3.FirstGen<-ordinaldatClean54(employ_datQ54.3.FirstGen$Q54.3, employ_datQ54.3.FirstGen)
#ordinal(employ_datQ54.3.FirstGen$CatOutcome, employ_datQ54.3.FirstGen$FirstGen, employ_datQ54.3.FirstGen)
prep <- analysisPrep(employ_datQ54.3.FirstGen$CatOutcome, employ_datQ54.3.FirstGen$FirstGen, employ_datQ54.3.FirstGen)
analysis <- polr(employ_datQ54.3.FirstGen$CatOutcome ~ employ_datQ54.3.FirstGen$FirstGen, data=employ_datQ54.3.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.3.Gender<-multidatClean(Q54.3, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.Gender$Gender<-factor(employ_datQ54.3.Gender$Gender)
employ_datQ54.3.Gender<-ordinaldatClean54(employ_datQ54.3.Gender$Q54.3, employ_datQ54.3.Gender)
#ordinal(employ_datQ54.3.Gender$CatOutcome, employ_datQ54.3.Gender$Gender, employ_datQ54.3.Gender)
prep <- analysisPrep(employ_datQ54.3.Gender$CatOutcome, employ_datQ54.3.Gender$Gender, employ_datQ54.3.Gender)
analysis <- polr(employ_datQ54.3.Gender$CatOutcome ~ employ_datQ54.3.Gender$Gender, data=employ_datQ54.3.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.3.Sexuality<-multidatClean(Q54.3, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.3.Sexuality$Sexuality<-factor(employ_datQ54.3.Sexuality$Sexuality)
employ_datQ54.3.Sexuality<-ordinaldatClean54(employ_datQ54.3.Sexuality$Q54.3, employ_datQ54.3.Sexuality)
#ordinal(employ_datQ54.3.Sexuality$CatOutcome, employ_datQ54.3.Sexuality$Sexuality, employ_datQ54.3.Sexuality)
prep <- analysisPrep(employ_datQ54.3.Sexuality$CatOutcome, employ_datQ54.3.Sexuality$Sexuality, employ_datQ54.3.Sexuality)
analysis <- polr(employ_datQ54.3.Sexuality$CatOutcome ~ employ_datQ54.3.Sexuality$Sexuality, data=employ_datQ54.3.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.3"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.3_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.4 (using orddatclean54) Ensuring a line of communication for researchers with concerns or complaints?
"Q54.4 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.4.<-multidatClean(Q54.4, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.4 + Academic, data = employ_datQ54.4.)
detach(dat_long)
employ_datQ54.4.<-ordinaldatClean54(employ_datQ54.4.$Q54.4, employ_datQ54.4.)
#ordinal(employ_datQ54.4.$CatOutcome, employ_datQ54.4.$Academic, employ_datQ54.4.)
prep <- analysisPrep(employ_datQ54.4.$CatOutcome, employ_datQ54.4.$Academic, employ_datQ54.4.)
analysis <- polr(employ_datQ54.4.$CatOutcome ~ employ_datQ54.4.$Academic, data=employ_datQ54.4., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.4"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.4.College<-multidatClean(Q54.4, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.4 + Q3, data = employ_datQ54.4.College)
conTable
detach(dat_long)
employ_datQ54.4.College$Q3[(employ_datQ54.4.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.4.College$Q3<- factor(employ_datQ54.4.College$Q3)
employ_datQ54.4.College<-ordinaldatClean54(employ_datQ54.4.College$Q54.4, employ_datQ54.4.College)
#ordinal(employ_datQ54.4.College$CatOutcome, employ_datQ54.4.College$Q3, employ_datQ54.4.College)
prep <- analysisPrep(employ_datQ54.4.College$CatOutcome, employ_datQ54.4.College$Q3, employ_datQ54.4.College)
analysis <- polr(employ_datQ54.4.College$CatOutcome ~ employ_datQ54.4.College$Q3, data=employ_datQ54.4.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.4.Carer<-multidatClean(Q54.4, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.Carer$Carer<- factor(employ_datQ54.4.Carer$Carer)
employ_datQ54.4.Carer<-ordinaldatClean54(employ_datQ54.4.Carer$Q54.4, employ_datQ54.4.Carer)
#ordinal(employ_datQ54.4.Carer$CatOutcome, employ_datQ54.4.Carer$Carer, employ_datQ54.4.Carer)
prep <- analysisPrep(employ_datQ54.4.Carer$CatOutcome, employ_datQ54.4.Carer$Carer, employ_datQ54.4.Carer)
analysis <- polr(employ_datQ54.4.Carer$CatOutcome ~ employ_datQ54.4.Carer$Carer, data=employ_datQ54.4.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.4.Disability<-multidatClean(Q54.4, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.Disability$Disability<- factor(employ_datQ54.4.Disability$Disability)
employ_datQ54.4.Disability<-ordinaldatClean54(employ_datQ54.4.Disability$Q54.4, employ_datQ54.4.Disability)
conTable <- xtabs(~Q54.4 + Disability, data = employ_datQ54.4.Disability)
#ordinal(employ_datQ54.4.Disability$CatOutcome, employ_datQ54.4.Disability$Disability, employ_datQ54.4.Disability)
prep <- analysisPrep(employ_datQ54.4.Disability$CatOutcome, employ_datQ54.4.Disability$Disability, employ_datQ54.4.Disability)
analysis <- polr(employ_datQ54.4.Disability$CatOutcome ~ employ_datQ54.4.Disability$Disability, data=employ_datQ54.4.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.4.Ethnicity<-multidatClean(Q54.4, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.Ethnicity$Ethnicity<- factor(employ_datQ54.4.Ethnicity$EthnicityCleaned)
employ_datQ54.4.Ethnicity<-ordinaldatClean54(employ_datQ54.4.Ethnicity$Q54.4, employ_datQ54.4.Ethnicity)
conTable <- xtabs(~Q54.4 + EthnicityCleaned, data = employ_datQ54.4.Ethnicity)
conTable
#ordinal(employ_datQ54.4.Ethnicity$CatOutcome, employ_datQ54.4.Ethnicity$EthnicityCleaned, employ_datQ54.4.Ethnicity)
prep <- analysisPrep(employ_datQ54.4.Ethnicity$CatOutcome, employ_datQ54.4.Ethnicity$EthnicityCleaned, employ_datQ54.4.Ethnicity)
analysis <- polr(employ_datQ54.4.Ethnicity$CatOutcome ~ employ_datQ54.4.Ethnicity$EthnicityCleaned, data=employ_datQ54.4.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.4.FirstGen<-multidatClean(Q54.4, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.FirstGen$FirstGen<-factor(employ_datQ54.4.FirstGen$FirstGen)
employ_datQ54.4.FirstGen<-ordinaldatClean54(employ_datQ54.4.FirstGen$Q54.4, employ_datQ54.4.FirstGen)
#ordinal(employ_datQ54.4.FirstGen$CatOutcome, employ_datQ54.4.FirstGen$FirstGen, employ_datQ54.4.FirstGen)
prep <- analysisPrep(employ_datQ54.4.FirstGen$CatOutcome, employ_datQ54.4.FirstGen$FirstGen, employ_datQ54.4.FirstGen)
analysis <- polr(employ_datQ54.4.FirstGen$CatOutcome ~ employ_datQ54.4.FirstGen$FirstGen, data=employ_datQ54.4.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.4.Gender<-multidatClean(Q54.4, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.Gender$Gender<-factor(employ_datQ54.4.Gender$Gender)
employ_datQ54.4.Gender<-ordinaldatClean54(employ_datQ54.4.Gender$Q54.4, employ_datQ54.4.Gender)
#ordinal(employ_datQ54.4.Gender$CatOutcome, employ_datQ54.4.Gender$Gender, employ_datQ54.4.Gender)
prep <- analysisPrep(employ_datQ54.4.Gender$CatOutcome, employ_datQ54.4.Gender$Gender, employ_datQ54.4.Gender)
analysis <- polr(employ_datQ54.4.Gender$CatOutcome ~ employ_datQ54.4.Gender$Gender, data=employ_datQ54.4.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.4.Sexuality<-multidatClean(Q54.4, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.4.Sexuality$Sexuality<-factor(employ_datQ54.4.Sexuality$Sexuality)
employ_datQ54.4.Sexuality<-ordinaldatClean54(employ_datQ54.4.Sexuality$Q54.4, employ_datQ54.4.Sexuality)
#ordinal(employ_datQ54.4.Sexuality$CatOutcome, employ_datQ54.4.Sexuality$Sexuality, employ_datQ54.4.Sexuality)
prep <- analysisPrep(employ_datQ54.4.Sexuality$CatOutcome, employ_datQ54.4.Sexuality$Sexuality, employ_datQ54.4.Sexuality)
analysis <- polr(employ_datQ54.4.Sexuality$CatOutcome ~ employ_datQ54.4.Sexuality$Sexuality, data=employ_datQ54.4.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.4"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.4_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.5 (using orddatclean54) Additional support/ equipment to support working from home?
"Q54.5 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.5.<-multidatClean(Q54.5, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.5 + Academic, data = employ_datQ54.5.)
detach(dat_long)
employ_datQ54.5.<-ordinaldatClean54(employ_datQ54.5.$Q54.5, employ_datQ54.5.)
#ordinal(employ_datQ54.5.$CatOutcome, employ_datQ54.5.$Academic, employ_datQ54.5.)
prep <- analysisPrep(employ_datQ54.5.$CatOutcome, employ_datQ54.5.$Academic, employ_datQ54.5.)
analysis <- polr(employ_datQ54.5.$CatOutcome ~ employ_datQ54.5.$Academic, data=employ_datQ54.5., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.5"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.5.College<-multidatClean(Q54.5, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.5 + Q3, data = employ_datQ54.5.College)
conTable
detach(dat_long)
employ_datQ54.5.College$Q3[(employ_datQ54.5.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.5.College$Q3<- factor(employ_datQ54.5.College$Q3)
employ_datQ54.5.College<-ordinaldatClean54(employ_datQ54.5.College$Q54.5, employ_datQ54.5.College)
#ordinal(employ_datQ54.5.College$CatOutcome, employ_datQ54.5.College$Q3, employ_datQ54.5.College)
prep <- analysisPrep(employ_datQ54.5.College$CatOutcome, employ_datQ54.5.College$Q3, employ_datQ54.5.College)
analysis <- polr(employ_datQ54.5.College$CatOutcome ~ employ_datQ54.5.College$Q3, data=employ_datQ54.5.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.5.Carer<-multidatClean(Q54.5, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.Carer$Carer<- factor(employ_datQ54.5.Carer$Carer)
employ_datQ54.5.Carer<-ordinaldatClean54(employ_datQ54.5.Carer$Q54.5, employ_datQ54.5.Carer)
#ordinal(employ_datQ54.5.Carer$CatOutcome, employ_datQ54.5.Carer$Carer, employ_datQ54.5.Carer)
prep <- analysisPrep(employ_datQ54.5.Carer$CatOutcome, employ_datQ54.5.Carer$Carer, employ_datQ54.5.Carer)
analysis <- polr(employ_datQ54.5.Carer$CatOutcome ~ employ_datQ54.5.Carer$Carer, data=employ_datQ54.5.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.5.Disability<-multidatClean(Q54.5, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.Disability$Disability<- factor(employ_datQ54.5.Disability$Disability)
employ_datQ54.5.Disability<-ordinaldatClean54(employ_datQ54.5.Disability$Q54.5, employ_datQ54.5.Disability)
conTable <- xtabs(~Q54.5 + Disability, data = employ_datQ54.5.Disability)
#ordinal(employ_datQ54.5.Disability$CatOutcome, employ_datQ54.5.Disability$Disability, employ_datQ54.5.Disability)
prep <- analysisPrep(employ_datQ54.5.Disability$CatOutcome, employ_datQ54.5.Disability$Disability, employ_datQ54.5.Disability)
analysis <- polr(employ_datQ54.5.Disability$CatOutcome ~ employ_datQ54.5.Disability$Disability, data=employ_datQ54.5.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.5.Ethnicity<-multidatClean(Q54.5, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.Ethnicity$Ethnicity<- factor(employ_datQ54.5.Ethnicity$EthnicityCleaned)
employ_datQ54.5.Ethnicity<-ordinaldatClean54(employ_datQ54.5.Ethnicity$Q54.5, employ_datQ54.5.Ethnicity)
conTable <- xtabs(~Q54.5 + EthnicityCleaned, data = employ_datQ54.5.Ethnicity)
conTable
#ordinal(employ_datQ54.5.Ethnicity$CatOutcome, employ_datQ54.5.Ethnicity$EthnicityCleaned, employ_datQ54.5.Ethnicity)
prep <- analysisPrep(employ_datQ54.5.Ethnicity$CatOutcome, employ_datQ54.5.Ethnicity$EthnicityCleaned, employ_datQ54.5.Ethnicity)
analysis <- polr(employ_datQ54.5.Ethnicity$CatOutcome ~ employ_datQ54.5.Ethnicity$EthnicityCleaned, data=employ_datQ54.5.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.5.FirstGen<-multidatClean(Q54.5, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.FirstGen$FirstGen<-factor(employ_datQ54.5.FirstGen$FirstGen)
employ_datQ54.5.FirstGen<-ordinaldatClean54(employ_datQ54.5.FirstGen$Q54.5, employ_datQ54.5.FirstGen)
#ordinal(employ_datQ54.5.FirstGen$CatOutcome, employ_datQ54.5.FirstGen$FirstGen, employ_datQ54.5.FirstGen)
prep <- analysisPrep(employ_datQ54.5.FirstGen$CatOutcome, employ_datQ54.5.FirstGen$FirstGen, employ_datQ54.5.FirstGen)
analysis <- polr(employ_datQ54.5.FirstGen$CatOutcome ~ employ_datQ54.5.FirstGen$FirstGen, data=employ_datQ54.5.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.5.Gender<-multidatClean(Q54.5, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.Gender$Gender<-factor(employ_datQ54.5.Gender$Gender)
employ_datQ54.5.Gender<-ordinaldatClean54(employ_datQ54.5.Gender$Q54.5, employ_datQ54.5.Gender)
#ordinal(employ_datQ54.5.Gender$CatOutcome, employ_datQ54.5.Gender$Gender, employ_datQ54.5.Gender)
prep <- analysisPrep(employ_datQ54.5.Gender$CatOutcome, employ_datQ54.5.Gender$Gender, employ_datQ54.5.Gender)
analysis <- polr(employ_datQ54.5.Gender$CatOutcome ~ employ_datQ54.5.Gender$Gender, data=employ_datQ54.5.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.5.Sexuality<-multidatClean(Q54.5, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.5.Sexuality$Sexuality<-factor(employ_datQ54.5.Sexuality$Sexuality)
employ_datQ54.5.Sexuality<-ordinaldatClean54(employ_datQ54.5.Sexuality$Q54.5, employ_datQ54.5.Sexuality)
#ordinal(employ_datQ54.5.Sexuality$CatOutcome, employ_datQ54.5.Sexuality$Sexuality, employ_datQ54.5.Sexuality)
prep <- analysisPrep(employ_datQ54.5.Sexuality$CatOutcome, employ_datQ54.5.Sexuality$Sexuality, employ_datQ54.5.Sexuality)
analysis <- polr(employ_datQ54.5.Sexuality$CatOutcome ~ employ_datQ54.5.Sexuality$Sexuality, data=employ_datQ54.5.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.5"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.5_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q54.6 (using orddatclean54) Opportunity for short visits to collect materials to support working from home?
"Q54.6 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ54.6.<-multidatClean(Q54.6, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.6 + Academic, data = employ_datQ54.6.)
detach(dat_long)
employ_datQ54.6.<-ordinaldatClean54(employ_datQ54.6.$Q54.6, employ_datQ54.6.)
#ordinal(employ_datQ54.6.$CatOutcome, employ_datQ54.6.$Academic, employ_datQ54.6.)
prep <- analysisPrep(employ_datQ54.6.$CatOutcome, employ_datQ54.6.$Academic, employ_datQ54.6.)
analysis <- polr(employ_datQ54.6.$CatOutcome ~ employ_datQ54.6.$Academic, data=employ_datQ54.6., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q54.6"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ54.6.College<-multidatClean(Q54.6, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q54.6 + Q3, data = employ_datQ54.6.College)
conTable
detach(dat_long)
employ_datQ54.6.College$Q3[(employ_datQ54.6.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ54.6.College$Q3<- factor(employ_datQ54.6.College$Q3)
employ_datQ54.6.College<-ordinaldatClean54(employ_datQ54.6.College$Q54.6, employ_datQ54.6.College)
#ordinal(employ_datQ54.6.College$CatOutcome, employ_datQ54.6.College$Q3, employ_datQ54.6.College)
prep <- analysisPrep(employ_datQ54.6.College$CatOutcome, employ_datQ54.6.College$Q3, employ_datQ54.6.College)
analysis <- polr(employ_datQ54.6.College$CatOutcome ~ employ_datQ54.6.College$Q3, data=employ_datQ54.6.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ54.6.Carer<-multidatClean(Q54.6, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.Carer$Carer<- factor(employ_datQ54.6.Carer$Carer)
employ_datQ54.6.Carer<-ordinaldatClean54(employ_datQ54.6.Carer$Q54.6, employ_datQ54.6.Carer)
#ordinal(employ_datQ54.6.Carer$CatOutcome, employ_datQ54.6.Carer$Carer, employ_datQ54.6.Carer)
prep <- analysisPrep(employ_datQ54.6.Carer$CatOutcome, employ_datQ54.6.Carer$Carer, employ_datQ54.6.Carer)
analysis <- polr(employ_datQ54.6.Carer$CatOutcome ~ employ_datQ54.6.Carer$Carer, data=employ_datQ54.6.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ54.6.Disability<-multidatClean(Q54.6, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.Disability$Disability<- factor(employ_datQ54.6.Disability$Disability)
employ_datQ54.6.Disability<-ordinaldatClean54(employ_datQ54.6.Disability$Q54.6, employ_datQ54.6.Disability)
conTable <- xtabs(~Q54.6 + Disability, data = employ_datQ54.6.Disability)
#ordinal(employ_datQ54.6.Disability$CatOutcome, employ_datQ54.6.Disability$Disability, employ_datQ54.6.Disability)
prep <- analysisPrep(employ_datQ54.6.Disability$CatOutcome, employ_datQ54.6.Disability$Disability, employ_datQ54.6.Disability)
analysis <- polr(employ_datQ54.6.Disability$CatOutcome ~ employ_datQ54.6.Disability$Disability, data=employ_datQ54.6.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ54.6.Ethnicity<-multidatClean(Q54.6, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.Ethnicity$Ethnicity<- factor(employ_datQ54.6.Ethnicity$EthnicityCleaned)
employ_datQ54.6.Ethnicity<-ordinaldatClean54(employ_datQ54.6.Ethnicity$Q54.6, employ_datQ54.6.Ethnicity)
conTable <- xtabs(~Q54.6 + EthnicityCleaned, data = employ_datQ54.6.Ethnicity)
conTable
#ordinal(employ_datQ54.6.Ethnicity$CatOutcome, employ_datQ54.6.Ethnicity$EthnicityCleaned, employ_datQ54.6.Ethnicity)
prep <- analysisPrep(employ_datQ54.6.Ethnicity$CatOutcome, employ_datQ54.6.Ethnicity$EthnicityCleaned, employ_datQ54.6.Ethnicity)
analysis <- polr(employ_datQ54.6.Ethnicity$CatOutcome ~ employ_datQ54.6.Ethnicity$EthnicityCleaned, data=employ_datQ54.6.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ54.6.FirstGen<-multidatClean(Q54.6, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.FirstGen$FirstGen<-factor(employ_datQ54.6.FirstGen$FirstGen)
employ_datQ54.6.FirstGen<-ordinaldatClean54(employ_datQ54.6.FirstGen$Q54.6, employ_datQ54.6.FirstGen)
#ordinal(employ_datQ54.6.FirstGen$CatOutcome, employ_datQ54.6.FirstGen$FirstGen, employ_datQ54.6.FirstGen)
prep <- analysisPrep(employ_datQ54.6.FirstGen$CatOutcome, employ_datQ54.6.FirstGen$FirstGen, employ_datQ54.6.FirstGen)
analysis <- polr(employ_datQ54.6.FirstGen$CatOutcome ~ employ_datQ54.6.FirstGen$FirstGen, data=employ_datQ54.6.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ54.6.Gender<-multidatClean(Q54.6, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.Gender$Gender<-factor(employ_datQ54.6.Gender$Gender)
employ_datQ54.6.Gender<-ordinaldatClean54(employ_datQ54.6.Gender$Q54.6, employ_datQ54.6.Gender)
#ordinal(employ_datQ54.6.Gender$CatOutcome, employ_datQ54.6.Gender$Gender, employ_datQ54.6.Gender)
prep <- analysisPrep(employ_datQ54.6.Gender$CatOutcome, employ_datQ54.6.Gender$Gender, employ_datQ54.6.Gender)
analysis <- polr(employ_datQ54.6.Gender$CatOutcome ~ employ_datQ54.6.Gender$Gender, data=employ_datQ54.6.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ54.6.Sexuality<-multidatClean(Q54.6, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ54.6.Sexuality$Sexuality<-factor(employ_datQ54.6.Sexuality$Sexuality)
employ_datQ54.6.Sexuality<-ordinaldatClean54(employ_datQ54.6.Sexuality$Q54.6, employ_datQ54.6.Sexuality)
#ordinal(employ_datQ54.6.Sexuality$CatOutcome, employ_datQ54.6.Sexuality$Sexuality, employ_datQ54.6.Sexuality)
prep <- analysisPrep(employ_datQ54.6.Sexuality$CatOutcome, employ_datQ54.6.Sexuality$Sexuality, employ_datQ54.6.Sexuality)
analysis <- polr(employ_datQ54.6.Sexuality$CatOutcome ~ employ_datQ54.6.Sexuality$Sexuality, data=employ_datQ54.6.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q54.6"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q54.6_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q55 (using orddatclean55) For laboratory based researchers: Would you prefer that reduced lab capacity is addressed by ...
"Q55 - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ55.<-multidatClean(Q55, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q55 + Academic, data = employ_datQ55.)
detach(dat_long)
employ_datQ55.<-ordinaldatClean55(employ_datQ55.$Q55, employ_datQ55.)
#ordinal(employ_datQ55.$CatOutcome, employ_datQ55.$Academic, employ_datQ55.)
prep <- analysisPrep(employ_datQ55.$CatOutcome, employ_datQ55.$Academic, employ_datQ55.)
analysis <- polr(employ_datQ55.$CatOutcome ~ employ_datQ55.$Academic, data=employ_datQ55., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q55"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ55.College<-multidatClean(Q55, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q55 + Q3, data = employ_datQ55.College)
conTable
detach(dat_long)
employ_datQ55.College$Q3[(employ_datQ55.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ55.College$Q3<- factor(employ_datQ55.College$Q3)
employ_datQ55.College<-ordinaldatClean55(employ_datQ55.College$Q55, employ_datQ55.College)
#ordinal(employ_datQ55.College$CatOutcome, employ_datQ55.College$Q3, employ_datQ55.College)
prep <- analysisPrep(employ_datQ55.College$CatOutcome, employ_datQ55.College$Q3, employ_datQ55.College)
analysis <- polr(employ_datQ55.College$CatOutcome ~ employ_datQ55.College$Q3, data=employ_datQ55.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ55.Carer<-multidatClean(Q55, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.Carer$Carer<- factor(employ_datQ55.Carer$Carer)
employ_datQ55.Carer<-ordinaldatClean55(employ_datQ55.Carer$Q55, employ_datQ55.Carer)
#ordinal(employ_datQ55.Carer$CatOutcome, employ_datQ55.Carer$Carer, employ_datQ55.Carer)
prep <- analysisPrep(employ_datQ55.Carer$CatOutcome, employ_datQ55.Carer$Carer, employ_datQ55.Carer)
analysis <- polr(employ_datQ55.Carer$CatOutcome ~ employ_datQ55.Carer$Carer, data=employ_datQ55.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ55.Disability<-multidatClean(Q55, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.Disability$Disability<- factor(employ_datQ55.Disability$Disability)
employ_datQ55.Disability<-ordinaldatClean55(employ_datQ55.Disability$Q55, employ_datQ55.Disability)
conTable <- xtabs(~Q55 + Disability, data = employ_datQ55.Disability)
#ordinal(employ_datQ55.Disability$CatOutcome, employ_datQ55.Disability$Disability, employ_datQ55.Disability)
prep <- analysisPrep(employ_datQ55.Disability$CatOutcome, employ_datQ55.Disability$Disability, employ_datQ55.Disability)
analysis <- polr(employ_datQ55.Disability$CatOutcome ~ employ_datQ55.Disability$Disability, data=employ_datQ55.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ55.Ethnicity<-multidatClean(Q55, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.Ethnicity$Ethnicity<- factor(employ_datQ55.Ethnicity$EthnicityCleaned)
employ_datQ55.Ethnicity<-ordinaldatClean55(employ_datQ55.Ethnicity$Q55, employ_datQ55.Ethnicity)
conTable <- xtabs(~Q55 + EthnicityCleaned, data = employ_datQ55.Ethnicity)
conTable
#ordinal(employ_datQ55.Ethnicity$CatOutcome, employ_datQ55.Ethnicity$EthnicityCleaned, employ_datQ55.Ethnicity)
prep <- analysisPrep(employ_datQ55.Ethnicity$CatOutcome, employ_datQ55.Ethnicity$EthnicityCleaned, employ_datQ55.Ethnicity)
analysis <- polr(employ_datQ55.Ethnicity$CatOutcome ~ employ_datQ55.Ethnicity$EthnicityCleaned, data=employ_datQ55.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ55.FirstGen<-multidatClean(Q55, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.FirstGen$FirstGen<-factor(employ_datQ55.FirstGen$FirstGen)
employ_datQ55.FirstGen<-ordinaldatClean55(employ_datQ55.FirstGen$Q55, employ_datQ55.FirstGen)
#ordinal(employ_datQ55.FirstGen$CatOutcome, employ_datQ55.FirstGen$FirstGen, employ_datQ55.FirstGen)
prep <- analysisPrep(employ_datQ55.FirstGen$CatOutcome, employ_datQ55.FirstGen$FirstGen, employ_datQ55.FirstGen)
analysis <- polr(employ_datQ55.FirstGen$CatOutcome ~ employ_datQ55.FirstGen$FirstGen, data=employ_datQ55.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ55.Gender<-multidatClean(Q55, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.Gender$Gender<-factor(employ_datQ55.Gender$Gender)
employ_datQ55.Gender<-ordinaldatClean55(employ_datQ55.Gender$Q55, employ_datQ55.Gender)
#ordinal(employ_datQ55.Gender$CatOutcome, employ_datQ55.Gender$Gender, employ_datQ55.Gender)
prep <- analysisPrep(employ_datQ55.Gender$CatOutcome, employ_datQ55.Gender$Gender, employ_datQ55.Gender)
analysis <- polr(employ_datQ55.Gender$CatOutcome ~ employ_datQ55.Gender$Gender, data=employ_datQ55.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ55.Sexuality<-multidatClean(Q55, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ55.Sexuality$Sexuality<-factor(employ_datQ55.Sexuality$Sexuality)
employ_datQ55.Sexuality<-ordinaldatClean55(employ_datQ55.Sexuality$Q55, employ_datQ55.Sexuality)
#ordinal(employ_datQ55.Sexuality$CatOutcome, employ_datQ55.Sexuality$Sexuality, employ_datQ55.Sexuality)
prep <- analysisPrep(employ_datQ55.Sexuality$CatOutcome, employ_datQ55.Sexuality$Sexuality, employ_datQ55.Sexuality)
analysis <- polr(employ_datQ55.Sexuality$CatOutcome ~ employ_datQ55.Sexuality$Sexuality, data=employ_datQ55.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q55"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q55_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatcleanNegative) My career prospects have been damaged by COVID
"Q56.1.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.1.<-multidatClean(Q56.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.1.a + Academic, data = employ_datQ56.1.)
detach(dat_long)
employ_datQ56.1.<-ordinaldatCleanNegative(employ_datQ56.1.$Q56.1.a, employ_datQ56.1.)
#ordinal(employ_datQ56.1.$CatOutcome, employ_datQ56.1.$Academic, employ_datQ56.1.)
prep <- analysisPrep(employ_datQ56.1.$CatOutcome, employ_datQ56.1.$Academic, employ_datQ56.1.)
analysis <- polr(employ_datQ56.1.$CatOutcome ~ employ_datQ56.1.$Academic, data=employ_datQ56.1., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.1.College<-multidatClean(Q56.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.1.a + Q3, data = employ_datQ56.1.College)
conTable
detach(dat_long)
employ_datQ56.1.College$Q3[(employ_datQ56.1.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.1.College$Q3<- factor(employ_datQ56.1.College$Q3)
employ_datQ56.1.College<-ordinaldatCleanNegative(employ_datQ56.1.College$Q56.1.a, employ_datQ56.1.College)
#ordinal(employ_datQ56.1.College$CatOutcome, employ_datQ56.1.College$Q3, employ_datQ56.1.College)
prep <- analysisPrep(employ_datQ56.1.College$CatOutcome, employ_datQ56.1.College$Q3, employ_datQ56.1.College)
analysis <- polr(employ_datQ56.1.College$CatOutcome ~ employ_datQ56.1.College$Q3, data=employ_datQ56.1.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.1.Carer<-multidatClean(Q56.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.Carer$Carer<- factor(employ_datQ56.1.Carer$Carer)
employ_datQ56.1.Carer<-ordinaldatCleanNegative(employ_datQ56.1.Carer$Q56.1.a, employ_datQ56.1.Carer)
#ordinal(employ_datQ56.1.Carer$CatOutcome, employ_datQ56.1.Carer$Carer, employ_datQ56.1.Carer)
prep <- analysisPrep(employ_datQ56.1.Carer$CatOutcome, employ_datQ56.1.Carer$Carer, employ_datQ56.1.Carer)
analysis <- polr(employ_datQ56.1.Carer$CatOutcome ~ employ_datQ56.1.Carer$Carer, data=employ_datQ56.1.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.1.Disability<-multidatClean(Q56.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.Disability$Disability<- factor(employ_datQ56.1.Disability$Disability)
employ_datQ56.1.Disability<-ordinaldatCleanNegative(employ_datQ56.1.Disability$Q56.1.a, employ_datQ56.1.Disability)
conTable <- xtabs(~Q56.1.a + Disability, data = employ_datQ56.1.Disability)
#ordinal(employ_datQ56.1.Disability$CatOutcome, employ_datQ56.1.Disability$Disability, employ_datQ56.1.Disability)
prep <- analysisPrep(employ_datQ56.1.Disability$CatOutcome, employ_datQ56.1.Disability$Disability, employ_datQ56.1.Disability)
analysis <- polr(employ_datQ56.1.Disability$CatOutcome ~ employ_datQ56.1.Disability$Disability, data=employ_datQ56.1.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.1.Ethnicity<-multidatClean(Q56.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.Ethnicity$Ethnicity<- factor(employ_datQ56.1.Ethnicity$EthnicityCleaned)
employ_datQ56.1.Ethnicity<-ordinaldatCleanNegative(employ_datQ56.1.Ethnicity$Q56.1.a, employ_datQ56.1.Ethnicity)
conTable <- xtabs(~Q56.1.a + EthnicityCleaned, data = employ_datQ56.1.Ethnicity)
conTable
#ordinal(employ_datQ56.1.Ethnicity$CatOutcome, employ_datQ56.1.Ethnicity$EthnicityCleaned, employ_datQ56.1.Ethnicity)
prep <- analysisPrep(employ_datQ56.1.Ethnicity$CatOutcome, employ_datQ56.1.Ethnicity$EthnicityCleaned, employ_datQ56.1.Ethnicity)
analysis <- polr(employ_datQ56.1.Ethnicity$CatOutcome ~ employ_datQ56.1.Ethnicity$EthnicityCleaned, data=employ_datQ56.1.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.1.FirstGen<-multidatClean(Q56.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.FirstGen$FirstGen<-factor(employ_datQ56.1.FirstGen$FirstGen)
employ_datQ56.1.FirstGen<-ordinaldatCleanNegative(employ_datQ56.1.FirstGen$Q56.1.a, employ_datQ56.1.FirstGen)
#ordinal(employ_datQ56.1.FirstGen$CatOutcome, employ_datQ56.1.FirstGen$FirstGen, employ_datQ56.1.FirstGen)
prep <- analysisPrep(employ_datQ56.1.FirstGen$CatOutcome, employ_datQ56.1.FirstGen$FirstGen, employ_datQ56.1.FirstGen)
analysis <- polr(employ_datQ56.1.FirstGen$CatOutcome ~ employ_datQ56.1.FirstGen$FirstGen, data=employ_datQ56.1.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.1.Gender<-multidatClean(Q56.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.Gender$Gender<-factor(employ_datQ56.1.Gender$Gender)
employ_datQ56.1.Gender<-ordinaldatCleanNegative(employ_datQ56.1.Gender$Q56.1.a, employ_datQ56.1.Gender)
#ordinal(employ_datQ56.1.Gender$CatOutcome, employ_datQ56.1.Gender$Gender, employ_datQ56.1.Gender)
prep <- analysisPrep(employ_datQ56.1.Gender$CatOutcome, employ_datQ56.1.Gender$Gender, employ_datQ56.1.Gender)
analysis <- polr(employ_datQ56.1.Gender$CatOutcome ~ employ_datQ56.1.Gender$Gender, data=employ_datQ56.1.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.1.Sexuality<-multidatClean(Q56.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.1.Sexuality$Sexuality<-factor(employ_datQ56.1.Sexuality$Sexuality)
employ_datQ56.1.Sexuality<-ordinaldatCleanNegative(employ_datQ56.1.Sexuality$Q56.1.a, employ_datQ56.1.Sexuality)
#ordinal(employ_datQ56.1.Sexuality$CatOutcome, employ_datQ56.1.Sexuality$Sexuality, employ_datQ56.1.Sexuality)
prep <- analysisPrep(employ_datQ56.1.Sexuality$CatOutcome, employ_datQ56.1.Sexuality$Sexuality, employ_datQ56.1.Sexuality)
analysis <- polr(employ_datQ56.1.Sexuality$CatOutcome ~ employ_datQ56.1.Sexuality$Sexuality, data=employ_datQ56.1.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatcleanNegative) I am worried that I will not be able to complete work on existing grants on time
"Q56.2.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.2.<-multidatClean(Q56.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.2.a + Academic, data = employ_datQ56.2.)
detach(dat_long)
employ_datQ56.2.<-ordinaldatCleanNegative(employ_datQ56.2.$Q56.2.a, employ_datQ56.2.)
#ordinal(employ_datQ56.2.$CatOutcome, employ_datQ56.2.$Academic, employ_datQ56.2.)
prep <- analysisPrep(employ_datQ56.2.$CatOutcome, employ_datQ56.2.$Academic, employ_datQ56.2.)
analysis <- polr(employ_datQ56.2.$CatOutcome ~ employ_datQ56.2.$Academic, data=employ_datQ56.2., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.2.College<-multidatClean(Q56.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.2.a + Q3, data = employ_datQ56.2.College)
conTable
detach(dat_long)
employ_datQ56.2.College$Q3[(employ_datQ56.2.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.2.College$Q3<- factor(employ_datQ56.2.College$Q3)
employ_datQ56.2.College<-ordinaldatCleanNegative(employ_datQ56.2.College$Q56.2.a, employ_datQ56.2.College)
#ordinal(employ_datQ56.2.College$CatOutcome, employ_datQ56.2.College$Q3, employ_datQ56.2.College)
prep <- analysisPrep(employ_datQ56.2.College$CatOutcome, employ_datQ56.2.College$Q3, employ_datQ56.2.College)
analysis <- polr(employ_datQ56.2.College$CatOutcome ~ employ_datQ56.2.College$Q3, data=employ_datQ56.2.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.2.Carer<-multidatClean(Q56.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.Carer$Carer<- factor(employ_datQ56.2.Carer$Carer)
employ_datQ56.2.Carer<-ordinaldatCleanNegative(employ_datQ56.2.Carer$Q56.2.a, employ_datQ56.2.Carer)
#ordinal(employ_datQ56.2.Carer$CatOutcome, employ_datQ56.2.Carer$Carer, employ_datQ56.2.Carer)
prep <- analysisPrep(employ_datQ56.2.Carer$CatOutcome, employ_datQ56.2.Carer$Carer, employ_datQ56.2.Carer)
analysis <- polr(employ_datQ56.2.Carer$CatOutcome ~ employ_datQ56.2.Carer$Carer, data=employ_datQ56.2.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.2.Disability<-multidatClean(Q56.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.Disability$Disability<- factor(employ_datQ56.2.Disability$Disability)
employ_datQ56.2.Disability<-ordinaldatCleanNegative(employ_datQ56.2.Disability$Q56.2.a, employ_datQ56.2.Disability)
conTable <- xtabs(~Q56.2.a + Disability, data = employ_datQ56.2.Disability)
#ordinal(employ_datQ56.2.Disability$CatOutcome, employ_datQ56.2.Disability$Disability, employ_datQ56.2.Disability)
prep <- analysisPrep(employ_datQ56.2.Disability$CatOutcome, employ_datQ56.2.Disability$Disability, employ_datQ56.2.Disability)
analysis <- polr(employ_datQ56.2.Disability$CatOutcome ~ employ_datQ56.2.Disability$Disability, data=employ_datQ56.2.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.2.Ethnicity<-multidatClean(Q56.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.Ethnicity$Ethnicity<- factor(employ_datQ56.2.Ethnicity$EthnicityCleaned)
employ_datQ56.2.Ethnicity<-ordinaldatCleanNegative(employ_datQ56.2.Ethnicity$Q56.2.a, employ_datQ56.2.Ethnicity)
conTable <- xtabs(~Q56.2.a + EthnicityCleaned, data = employ_datQ56.2.Ethnicity)
conTable
#ordinal(employ_datQ56.2.Ethnicity$CatOutcome, employ_datQ56.2.Ethnicity$EthnicityCleaned, employ_datQ56.2.Ethnicity)
prep <- analysisPrep(employ_datQ56.2.Ethnicity$CatOutcome, employ_datQ56.2.Ethnicity$EthnicityCleaned, employ_datQ56.2.Ethnicity)
analysis <- polr(employ_datQ56.2.Ethnicity$CatOutcome ~ employ_datQ56.2.Ethnicity$EthnicityCleaned, data=employ_datQ56.2.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.2.FirstGen<-multidatClean(Q56.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.FirstGen$FirstGen<-factor(employ_datQ56.2.FirstGen$FirstGen)
employ_datQ56.2.FirstGen<-ordinaldatCleanNegative(employ_datQ56.2.FirstGen$Q56.2.a, employ_datQ56.2.FirstGen)
#ordinal(employ_datQ56.2.FirstGen$CatOutcome, employ_datQ56.2.FirstGen$FirstGen, employ_datQ56.2.FirstGen)
prep <- analysisPrep(employ_datQ56.2.FirstGen$CatOutcome, employ_datQ56.2.FirstGen$FirstGen, employ_datQ56.2.FirstGen)
analysis <- polr(employ_datQ56.2.FirstGen$CatOutcome ~ employ_datQ56.2.FirstGen$FirstGen, data=employ_datQ56.2.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.2.Gender<-multidatClean(Q56.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.Gender$Gender<-factor(employ_datQ56.2.Gender$Gender)
employ_datQ56.2.Gender<-ordinaldatCleanNegative(employ_datQ56.2.Gender$Q56.2.a, employ_datQ56.2.Gender)
#ordinal(employ_datQ56.2.Gender$CatOutcome, employ_datQ56.2.Gender$Gender, employ_datQ56.2.Gender)
prep <- analysisPrep(employ_datQ56.2.Gender$CatOutcome, employ_datQ56.2.Gender$Gender, employ_datQ56.2.Gender)
analysis <- polr(employ_datQ56.2.Gender$CatOutcome ~ employ_datQ56.2.Gender$Gender, data=employ_datQ56.2.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.2.Sexuality<-multidatClean(Q56.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.2.Sexuality$Sexuality<-factor(employ_datQ56.2.Sexuality$Sexuality)
employ_datQ56.2.Sexuality<-ordinaldatCleanNegative(employ_datQ56.2.Sexuality$Q56.2.a, employ_datQ56.2.Sexuality)
#ordinal(employ_datQ56.2.Sexuality$CatOutcome, employ_datQ56.2.Sexuality$Sexuality, employ_datQ56.2.Sexuality)
prep <- analysisPrep(employ_datQ56.2.Sexuality$CatOutcome, employ_datQ56.2.Sexuality$Sexuality, employ_datQ56.2.Sexuality)
analysis <- polr(employ_datQ56.2.Sexuality$CatOutcome ~ employ_datQ56.2.Sexuality$Sexuality, data=employ_datQ56.2.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatcleanNegative) I am worried that I will not be able to generate pilot data for forthcoming grant applications
"Q56.3.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.3.<-multidatClean(Q56.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.3.a + Academic, data = employ_datQ56.3.)
detach(dat_long)
employ_datQ56.3.<-ordinaldatCleanNegative(employ_datQ56.3.$Q56.3.a, employ_datQ56.3.)
#ordinal(employ_datQ56.3.$CatOutcome, employ_datQ56.3.$Academic, employ_datQ56.3.)
prep <- analysisPrep(employ_datQ56.3.$CatOutcome, employ_datQ56.3.$Academic, employ_datQ56.3.)
analysis <- polr(employ_datQ56.3.$CatOutcome ~ employ_datQ56.3.$Academic, data=employ_datQ56.3., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.3.College<-multidatClean(Q56.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.3.a + Q3, data = employ_datQ56.3.College)
conTable
detach(dat_long)
employ_datQ56.3.College$Q3[(employ_datQ56.3.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.3.College$Q3<- factor(employ_datQ56.3.College$Q3)
employ_datQ56.3.College<-ordinaldatCleanNegative(employ_datQ56.3.College$Q56.3.a, employ_datQ56.3.College)
#ordinal(employ_datQ56.3.College$CatOutcome, employ_datQ56.3.College$Q3, employ_datQ56.3.College)
prep <- analysisPrep(employ_datQ56.3.College$CatOutcome, employ_datQ56.3.College$Q3, employ_datQ56.3.College)
analysis <- polr(employ_datQ56.3.College$CatOutcome ~ employ_datQ56.3.College$Q3, data=employ_datQ56.3.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.3.Carer<-multidatClean(Q56.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.Carer$Carer<- factor(employ_datQ56.3.Carer$Carer)
employ_datQ56.3.Carer<-ordinaldatCleanNegative(employ_datQ56.3.Carer$Q56.3.a, employ_datQ56.3.Carer)
#ordinal(employ_datQ56.3.Carer$CatOutcome, employ_datQ56.3.Carer$Carer, employ_datQ56.3.Carer)
prep <- analysisPrep(employ_datQ56.3.Carer$CatOutcome, employ_datQ56.3.Carer$Carer, employ_datQ56.3.Carer)
analysis <- polr(employ_datQ56.3.Carer$CatOutcome ~ employ_datQ56.3.Carer$Carer, data=employ_datQ56.3.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.3.Disability<-multidatClean(Q56.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.Disability$Disability<- factor(employ_datQ56.3.Disability$Disability)
employ_datQ56.3.Disability<-ordinaldatCleanNegative(employ_datQ56.3.Disability$Q56.3.a, employ_datQ56.3.Disability)
conTable <- xtabs(~Q56.3.a + Disability, data = employ_datQ56.3.Disability)
#ordinal(employ_datQ56.3.Disability$CatOutcome, employ_datQ56.3.Disability$Disability, employ_datQ56.3.Disability)
prep <- analysisPrep(employ_datQ56.3.Disability$CatOutcome, employ_datQ56.3.Disability$Disability, employ_datQ56.3.Disability)
analysis <- polr(employ_datQ56.3.Disability$CatOutcome ~ employ_datQ56.3.Disability$Disability, data=employ_datQ56.3.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.3.Ethnicity<-multidatClean(Q56.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.Ethnicity$Ethnicity<- factor(employ_datQ56.3.Ethnicity$EthnicityCleaned)
employ_datQ56.3.Ethnicity<-ordinaldatCleanNegative(employ_datQ56.3.Ethnicity$Q56.3.a, employ_datQ56.3.Ethnicity)
conTable <- xtabs(~Q56.3.a + EthnicityCleaned, data = employ_datQ56.3.Ethnicity)
conTable
#ordinal(employ_datQ56.3.Ethnicity$CatOutcome, employ_datQ56.3.Ethnicity$EthnicityCleaned, employ_datQ56.3.Ethnicity)
prep <- analysisPrep(employ_datQ56.3.Ethnicity$CatOutcome, employ_datQ56.3.Ethnicity$EthnicityCleaned, employ_datQ56.3.Ethnicity)
analysis <- polr(employ_datQ56.3.Ethnicity$CatOutcome ~ employ_datQ56.3.Ethnicity$EthnicityCleaned, data=employ_datQ56.3.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.3.FirstGen<-multidatClean(Q56.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.FirstGen$FirstGen<-factor(employ_datQ56.3.FirstGen$FirstGen)
employ_datQ56.3.FirstGen<-ordinaldatCleanNegative(employ_datQ56.3.FirstGen$Q56.3.a, employ_datQ56.3.FirstGen)
#ordinal(employ_datQ56.3.FirstGen$CatOutcome, employ_datQ56.3.FirstGen$FirstGen, employ_datQ56.3.FirstGen)
prep <- analysisPrep(employ_datQ56.3.FirstGen$CatOutcome, employ_datQ56.3.FirstGen$FirstGen, employ_datQ56.3.FirstGen)
analysis <- polr(employ_datQ56.3.FirstGen$CatOutcome ~ employ_datQ56.3.FirstGen$FirstGen, data=employ_datQ56.3.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.3.Gender<-multidatClean(Q56.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.Gender$Gender<-factor(employ_datQ56.3.Gender$Gender)
employ_datQ56.3.Gender<-ordinaldatCleanNegative(employ_datQ56.3.Gender$Q56.3.a, employ_datQ56.3.Gender)
#ordinal(employ_datQ56.3.Gender$CatOutcome, employ_datQ56.3.Gender$Gender, employ_datQ56.3.Gender)
prep <- analysisPrep(employ_datQ56.3.Gender$CatOutcome, employ_datQ56.3.Gender$Gender, employ_datQ56.3.Gender)
analysis <- polr(employ_datQ56.3.Gender$CatOutcome ~ employ_datQ56.3.Gender$Gender, data=employ_datQ56.3.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.3.Sexuality<-multidatClean(Q56.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.3.Sexuality$Sexuality<-factor(employ_datQ56.3.Sexuality$Sexuality)
employ_datQ56.3.Sexuality<-ordinaldatCleanNegative(employ_datQ56.3.Sexuality$Q56.3.a, employ_datQ56.3.Sexuality)
#ordinal(employ_datQ56.3.Sexuality$CatOutcome, employ_datQ56.3.Sexuality$Sexuality, employ_datQ56.3.Sexuality)
prep <- analysisPrep(employ_datQ56.3.Sexuality$CatOutcome, employ_datQ56.3.Sexuality$Sexuality, employ_datQ56.3.Sexuality)
analysis <- polr(employ_datQ56.3.Sexuality$CatOutcome ~ employ_datQ56.3.Sexuality$Sexuality, data=employ_datQ56.3.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatclean) The University has been sensitive in its dealings with researchers
"Q56.4.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.4.<-multidatClean(Q56.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.4.a + Academic, data = employ_datQ56.4.)
detach(dat_long)
employ_datQ56.4.<-ordinaldatClean(employ_datQ56.4.$Q56.4.a, employ_datQ56.4.)
#ordinal(employ_datQ56.4.$CatOutcome, employ_datQ56.4.$Academic, employ_datQ56.4.)
prep <- analysisPrep(employ_datQ56.4.$CatOutcome, employ_datQ56.4.$Academic, employ_datQ56.4.)
analysis <- polr(employ_datQ56.4.$CatOutcome ~ employ_datQ56.4.$Academic, data=employ_datQ56.4., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.4.College<-multidatClean(Q56.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.4.a + Q3, data = employ_datQ56.4.College)
conTable
detach(dat_long)
employ_datQ56.4.College$Q3[(employ_datQ56.4.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.4.College$Q3<- factor(employ_datQ56.4.College$Q3)
employ_datQ56.4.College<-ordinaldatClean(employ_datQ56.4.College$Q56.4.a, employ_datQ56.4.College)
#ordinal(employ_datQ56.4.College$CatOutcome, employ_datQ56.4.College$Q3, employ_datQ56.4.College)
prep <- analysisPrep(employ_datQ56.4.College$CatOutcome, employ_datQ56.4.College$Q3, employ_datQ56.4.College)
analysis <- polr(employ_datQ56.4.College$CatOutcome ~ employ_datQ56.4.College$Q3, data=employ_datQ56.4.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.4.Carer<-multidatClean(Q56.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.Carer$Carer<- factor(employ_datQ56.4.Carer$Carer)
employ_datQ56.4.Carer<-ordinaldatClean(employ_datQ56.4.Carer$Q56.4.a, employ_datQ56.4.Carer)
#ordinal(employ_datQ56.4.Carer$CatOutcome, employ_datQ56.4.Carer$Carer, employ_datQ56.4.Carer)
prep <- analysisPrep(employ_datQ56.4.Carer$CatOutcome, employ_datQ56.4.Carer$Carer, employ_datQ56.4.Carer)
analysis <- polr(employ_datQ56.4.Carer$CatOutcome ~ employ_datQ56.4.Carer$Carer, data=employ_datQ56.4.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.4.Disability<-multidatClean(Q56.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.Disability$Disability<- factor(employ_datQ56.4.Disability$Disability)
employ_datQ56.4.Disability<-ordinaldatClean(employ_datQ56.4.Disability$Q56.4.a, employ_datQ56.4.Disability)
conTable <- xtabs(~Q56.4.a + Disability, data = employ_datQ56.4.Disability)
#ordinal(employ_datQ56.4.Disability$CatOutcome, employ_datQ56.4.Disability$Disability, employ_datQ56.4.Disability)
prep <- analysisPrep(employ_datQ56.4.Disability$CatOutcome, employ_datQ56.4.Disability$Disability, employ_datQ56.4.Disability)
analysis <- polr(employ_datQ56.4.Disability$CatOutcome ~ employ_datQ56.4.Disability$Disability, data=employ_datQ56.4.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.4.Ethnicity<-multidatClean(Q56.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.Ethnicity$Ethnicity<- factor(employ_datQ56.4.Ethnicity$EthnicityCleaned)
employ_datQ56.4.Ethnicity<-ordinaldatClean(employ_datQ56.4.Ethnicity$Q56.4.a, employ_datQ56.4.Ethnicity)
conTable <- xtabs(~Q56.4.a + EthnicityCleaned, data = employ_datQ56.4.Ethnicity)
conTable
#ordinal(employ_datQ56.4.Ethnicity$CatOutcome, employ_datQ56.4.Ethnicity$EthnicityCleaned, employ_datQ56.4.Ethnicity)
prep <- analysisPrep(employ_datQ56.4.Ethnicity$CatOutcome, employ_datQ56.4.Ethnicity$EthnicityCleaned, employ_datQ56.4.Ethnicity)
analysis <- polr(employ_datQ56.4.Ethnicity$CatOutcome ~ employ_datQ56.4.Ethnicity$EthnicityCleaned, data=employ_datQ56.4.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.4.FirstGen<-multidatClean(Q56.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.FirstGen$FirstGen<-factor(employ_datQ56.4.FirstGen$FirstGen)
employ_datQ56.4.FirstGen<-ordinaldatClean(employ_datQ56.4.FirstGen$Q56.4.a, employ_datQ56.4.FirstGen)
#ordinal(employ_datQ56.4.FirstGen$CatOutcome, employ_datQ56.4.FirstGen$FirstGen, employ_datQ56.4.FirstGen)
prep <- analysisPrep(employ_datQ56.4.FirstGen$CatOutcome, employ_datQ56.4.FirstGen$FirstGen, employ_datQ56.4.FirstGen)
analysis <- polr(employ_datQ56.4.FirstGen$CatOutcome ~ employ_datQ56.4.FirstGen$FirstGen, data=employ_datQ56.4.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.4.Gender<-multidatClean(Q56.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.Gender$Gender<-factor(employ_datQ56.4.Gender$Gender)
employ_datQ56.4.Gender<-ordinaldatClean(employ_datQ56.4.Gender$Q56.4.a, employ_datQ56.4.Gender)
#ordinal(employ_datQ56.4.Gender$CatOutcome, employ_datQ56.4.Gender$Gender, employ_datQ56.4.Gender)
prep <- analysisPrep(employ_datQ56.4.Gender$CatOutcome, employ_datQ56.4.Gender$Gender, employ_datQ56.4.Gender)
analysis <- polr(employ_datQ56.4.Gender$CatOutcome ~ employ_datQ56.4.Gender$Gender, data=employ_datQ56.4.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.4.Sexuality<-multidatClean(Q56.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.4.Sexuality$Sexuality<-factor(employ_datQ56.4.Sexuality$Sexuality)
employ_datQ56.4.Sexuality<-ordinaldatClean(employ_datQ56.4.Sexuality$Q56.4.a, employ_datQ56.4.Sexuality)
#ordinal(employ_datQ56.4.Sexuality$CatOutcome, employ_datQ56.4.Sexuality$Sexuality, employ_datQ56.4.Sexuality)
prep <- analysisPrep(employ_datQ56.4.Sexuality$CatOutcome, employ_datQ56.4.Sexuality$Sexuality, employ_datQ56.4.Sexuality)
analysis <- polr(employ_datQ56.4.Sexuality$CatOutcome ~ employ_datQ56.4.Sexuality$Sexuality, data=employ_datQ56.4.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatclean) The University has made reasonable efforts to keep me informed
"Q56.5.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.5.<-multidatClean(Q56.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.5.a + Academic, data = employ_datQ56.5.)
detach(dat_long)
employ_datQ56.5.<-ordinaldatClean(employ_datQ56.5.$Q56.5.a, employ_datQ56.5.)
#ordinal(employ_datQ56.5.$CatOutcome, employ_datQ56.5.$Academic, employ_datQ56.5.)
prep <- analysisPrep(employ_datQ56.5.$CatOutcome, employ_datQ56.5.$Academic, employ_datQ56.5.)
analysis <- polr(employ_datQ56.5.$CatOutcome ~ employ_datQ56.5.$Academic, data=employ_datQ56.5., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.5.College<-multidatClean(Q56.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.5.a + Q3, data = employ_datQ56.5.College)
conTable
detach(dat_long)
employ_datQ56.5.College$Q3[(employ_datQ56.5.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.5.College$Q3<- factor(employ_datQ56.5.College$Q3)
employ_datQ56.5.College<-ordinaldatClean(employ_datQ56.5.College$Q56.5.a, employ_datQ56.5.College)
#ordinal(employ_datQ56.5.College$CatOutcome, employ_datQ56.5.College$Q3, employ_datQ56.5.College)
prep <- analysisPrep(employ_datQ56.5.College$CatOutcome, employ_datQ56.5.College$Q3, employ_datQ56.5.College)
analysis <- polr(employ_datQ56.5.College$CatOutcome ~ employ_datQ56.5.College$Q3, data=employ_datQ56.5.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.5.Carer<-multidatClean(Q56.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.Carer$Carer<- factor(employ_datQ56.5.Carer$Carer)
employ_datQ56.5.Carer<-ordinaldatClean(employ_datQ56.5.Carer$Q56.5.a, employ_datQ56.5.Carer)
#ordinal(employ_datQ56.5.Carer$CatOutcome, employ_datQ56.5.Carer$Carer, employ_datQ56.5.Carer)
prep <- analysisPrep(employ_datQ56.5.Carer$CatOutcome, employ_datQ56.5.Carer$Carer, employ_datQ56.5.Carer)
analysis <- polr(employ_datQ56.5.Carer$CatOutcome ~ employ_datQ56.5.Carer$Carer, data=employ_datQ56.5.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.5.Disability<-multidatClean(Q56.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.Disability$Disability<- factor(employ_datQ56.5.Disability$Disability)
employ_datQ56.5.Disability<-ordinaldatClean(employ_datQ56.5.Disability$Q56.5.a, employ_datQ56.5.Disability)
conTable <- xtabs(~Q56.5.a + Disability, data = employ_datQ56.5.Disability)
#ordinal(employ_datQ56.5.Disability$CatOutcome, employ_datQ56.5.Disability$Disability, employ_datQ56.5.Disability)
prep <- analysisPrep(employ_datQ56.5.Disability$CatOutcome, employ_datQ56.5.Disability$Disability, employ_datQ56.5.Disability)
analysis <- polr(employ_datQ56.5.Disability$CatOutcome ~ employ_datQ56.5.Disability$Disability, data=employ_datQ56.5.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.5.Ethnicity<-multidatClean(Q56.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.Ethnicity$Ethnicity<- factor(employ_datQ56.5.Ethnicity$EthnicityCleaned)
employ_datQ56.5.Ethnicity<-ordinaldatClean(employ_datQ56.5.Ethnicity$Q56.5.a, employ_datQ56.5.Ethnicity)
conTable <- xtabs(~Q56.5.a + EthnicityCleaned, data = employ_datQ56.5.Ethnicity)
conTable
#ordinal(employ_datQ56.5.Ethnicity$CatOutcome, employ_datQ56.5.Ethnicity$EthnicityCleaned, employ_datQ56.5.Ethnicity)
prep <- analysisPrep(employ_datQ56.5.Ethnicity$CatOutcome, employ_datQ56.5.Ethnicity$EthnicityCleaned, employ_datQ56.5.Ethnicity)
analysis <- polr(employ_datQ56.5.Ethnicity$CatOutcome ~ employ_datQ56.5.Ethnicity$EthnicityCleaned, data=employ_datQ56.5.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.5.FirstGen<-multidatClean(Q56.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.FirstGen$FirstGen<-factor(employ_datQ56.5.FirstGen$FirstGen)
employ_datQ56.5.FirstGen<-ordinaldatClean(employ_datQ56.5.FirstGen$Q56.5.a, employ_datQ56.5.FirstGen)
#ordinal(employ_datQ56.5.FirstGen$CatOutcome, employ_datQ56.5.FirstGen$FirstGen, employ_datQ56.5.FirstGen)
prep <- analysisPrep(employ_datQ56.5.FirstGen$CatOutcome, employ_datQ56.5.FirstGen$FirstGen, employ_datQ56.5.FirstGen)
analysis <- polr(employ_datQ56.5.FirstGen$CatOutcome ~ employ_datQ56.5.FirstGen$FirstGen, data=employ_datQ56.5.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.5.Gender<-multidatClean(Q56.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.Gender$Gender<-factor(employ_datQ56.5.Gender$Gender)
employ_datQ56.5.Gender<-ordinaldatClean(employ_datQ56.5.Gender$Q56.5.a, employ_datQ56.5.Gender)
#ordinal(employ_datQ56.5.Gender$CatOutcome, employ_datQ56.5.Gender$Gender, employ_datQ56.5.Gender)
prep <- analysisPrep(employ_datQ56.5.Gender$CatOutcome, employ_datQ56.5.Gender$Gender, employ_datQ56.5.Gender)
analysis <- polr(employ_datQ56.5.Gender$CatOutcome ~ employ_datQ56.5.Gender$Gender, data=employ_datQ56.5.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.5.Sexuality<-multidatClean(Q56.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.5.Sexuality$Sexuality<-factor(employ_datQ56.5.Sexuality$Sexuality)
employ_datQ56.5.Sexuality<-ordinaldatClean(employ_datQ56.5.Sexuality$Q56.5.a, employ_datQ56.5.Sexuality)
#ordinal(employ_datQ56.5.Sexuality$CatOutcome, employ_datQ56.5.Sexuality$Sexuality, employ_datQ56.5.Sexuality)
prep <- analysisPrep(employ_datQ56.5.Sexuality$CatOutcome, employ_datQ56.5.Sexuality$Sexuality, employ_datQ56.5.Sexuality)
analysis <- polr(employ_datQ56.5.Sexuality$CatOutcome ~ employ_datQ56.5.Sexuality$Sexuality, data=employ_datQ56.5.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatclean) I am optimistic about the future
"Q56.6.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.6.<-multidatClean(Q56.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.6.a + Academic, data = employ_datQ56.6.)
detach(dat_long)
employ_datQ56.6.<-ordinaldatClean(employ_datQ56.6.$Q56.6.a, employ_datQ56.6.)
#ordinal(employ_datQ56.6.$CatOutcome, employ_datQ56.6.$Academic, employ_datQ56.6.)
prep <- analysisPrep(employ_datQ56.6.$CatOutcome, employ_datQ56.6.$Academic, employ_datQ56.6.)
analysis <- polr(employ_datQ56.6.$CatOutcome ~ employ_datQ56.6.$Academic, data=employ_datQ56.6., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.6.College<-multidatClean(Q56.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.6.a + Q3, data = employ_datQ56.6.College)
conTable
detach(dat_long)
employ_datQ56.6.College$Q3[(employ_datQ56.6.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.6.College$Q3<- factor(employ_datQ56.6.College$Q3)
employ_datQ56.6.College<-ordinaldatClean(employ_datQ56.6.College$Q56.6.a, employ_datQ56.6.College)
#ordinal(employ_datQ56.6.College$CatOutcome, employ_datQ56.6.College$Q3, employ_datQ56.6.College)
prep <- analysisPrep(employ_datQ56.6.College$CatOutcome, employ_datQ56.6.College$Q3, employ_datQ56.6.College)
analysis <- polr(employ_datQ56.6.College$CatOutcome ~ employ_datQ56.6.College$Q3, data=employ_datQ56.6.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.6.Carer<-multidatClean(Q56.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.Carer$Carer<- factor(employ_datQ56.6.Carer$Carer)
employ_datQ56.6.Carer<-ordinaldatClean(employ_datQ56.6.Carer$Q56.6.a, employ_datQ56.6.Carer)
#ordinal(employ_datQ56.6.Carer$CatOutcome, employ_datQ56.6.Carer$Carer, employ_datQ56.6.Carer)
prep <- analysisPrep(employ_datQ56.6.Carer$CatOutcome, employ_datQ56.6.Carer$Carer, employ_datQ56.6.Carer)
analysis <- polr(employ_datQ56.6.Carer$CatOutcome ~ employ_datQ56.6.Carer$Carer, data=employ_datQ56.6.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.6.Disability<-multidatClean(Q56.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.Disability$Disability<- factor(employ_datQ56.6.Disability$Disability)
employ_datQ56.6.Disability<-ordinaldatClean(employ_datQ56.6.Disability$Q56.6.a, employ_datQ56.6.Disability)
conTable <- xtabs(~Q56.6.a + Disability, data = employ_datQ56.6.Disability)
#ordinal(employ_datQ56.6.Disability$CatOutcome, employ_datQ56.6.Disability$Disability, employ_datQ56.6.Disability)
prep <- analysisPrep(employ_datQ56.6.Disability$CatOutcome, employ_datQ56.6.Disability$Disability, employ_datQ56.6.Disability)
analysis <- polr(employ_datQ56.6.Disability$CatOutcome ~ employ_datQ56.6.Disability$Disability, data=employ_datQ56.6.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.6.Ethnicity<-multidatClean(Q56.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.Ethnicity$Ethnicity<- factor(employ_datQ56.6.Ethnicity$EthnicityCleaned)
employ_datQ56.6.Ethnicity<-ordinaldatClean(employ_datQ56.6.Ethnicity$Q56.6.a, employ_datQ56.6.Ethnicity)
conTable <- xtabs(~Q56.6.a + EthnicityCleaned, data = employ_datQ56.6.Ethnicity)
conTable
#ordinal(employ_datQ56.6.Ethnicity$CatOutcome, employ_datQ56.6.Ethnicity$EthnicityCleaned, employ_datQ56.6.Ethnicity)
prep <- analysisPrep(employ_datQ56.6.Ethnicity$CatOutcome, employ_datQ56.6.Ethnicity$EthnicityCleaned, employ_datQ56.6.Ethnicity)
analysis <- polr(employ_datQ56.6.Ethnicity$CatOutcome ~ employ_datQ56.6.Ethnicity$EthnicityCleaned, data=employ_datQ56.6.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.6.FirstGen<-multidatClean(Q56.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.FirstGen$FirstGen<-factor(employ_datQ56.6.FirstGen$FirstGen)
employ_datQ56.6.FirstGen<-ordinaldatClean(employ_datQ56.6.FirstGen$Q56.6.a, employ_datQ56.6.FirstGen)
#ordinal(employ_datQ56.6.FirstGen$CatOutcome, employ_datQ56.6.FirstGen$FirstGen, employ_datQ56.6.FirstGen)
prep <- analysisPrep(employ_datQ56.6.FirstGen$CatOutcome, employ_datQ56.6.FirstGen$FirstGen, employ_datQ56.6.FirstGen)
analysis <- polr(employ_datQ56.6.FirstGen$CatOutcome ~ employ_datQ56.6.FirstGen$FirstGen, data=employ_datQ56.6.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.6.Gender<-multidatClean(Q56.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.Gender$Gender<-factor(employ_datQ56.6.Gender$Gender)
employ_datQ56.6.Gender<-ordinaldatClean(employ_datQ56.6.Gender$Q56.6.a, employ_datQ56.6.Gender)
#ordinal(employ_datQ56.6.Gender$CatOutcome, employ_datQ56.6.Gender$Gender, employ_datQ56.6.Gender)
prep <- analysisPrep(employ_datQ56.6.Gender$CatOutcome, employ_datQ56.6.Gender$Gender, employ_datQ56.6.Gender)
analysis <- polr(employ_datQ56.6.Gender$CatOutcome ~ employ_datQ56.6.Gender$Gender, data=employ_datQ56.6.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.6.Sexuality<-multidatClean(Q56.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.6.Sexuality$Sexuality<-factor(employ_datQ56.6.Sexuality$Sexuality)
employ_datQ56.6.Sexuality<-ordinaldatClean(employ_datQ56.6.Sexuality$Q56.6.a, employ_datQ56.6.Sexuality)
#ordinal(employ_datQ56.6.Sexuality$CatOutcome, employ_datQ56.6.Sexuality$Sexuality, employ_datQ56.6.Sexuality)
prep <- analysisPrep(employ_datQ56.6.Sexuality$CatOutcome, employ_datQ56.6.Sexuality$Sexuality, employ_datQ56.6.Sexuality)
analysis <- polr(employ_datQ56.6.Sexuality$CatOutcome ~ employ_datQ56.6.Sexuality$Sexuality, data=employ_datQ56.6.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatclean) The University has provided appropriate information and support for researchers whose fixed term contracts are due to end shortly
"Q56.7.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.7.<-multidatClean(Q56.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.7.a + Academic, data = employ_datQ56.7.)
detach(dat_long)
employ_datQ56.7.<-ordinaldatClean(employ_datQ56.7.$Q56.7.a, employ_datQ56.7.)
#ordinal(employ_datQ56.7.$CatOutcome, employ_datQ56.7.$Academic, employ_datQ56.7.)
prep <- analysisPrep(employ_datQ56.7.$CatOutcome, employ_datQ56.7.$Academic, employ_datQ56.7.)
analysis <- polr(employ_datQ56.7.$CatOutcome ~ employ_datQ56.7.$Academic, data=employ_datQ56.7., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.7.College<-multidatClean(Q56.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.7.a + Q3, data = employ_datQ56.7.College)
conTable
detach(dat_long)
employ_datQ56.7.College$Q3[(employ_datQ56.7.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.7.College$Q3<- factor(employ_datQ56.7.College$Q3)
employ_datQ56.7.College<-ordinaldatClean(employ_datQ56.7.College$Q56.7.a, employ_datQ56.7.College)
#ordinal(employ_datQ56.7.College$CatOutcome, employ_datQ56.7.College$Q3, employ_datQ56.7.College)
prep <- analysisPrep(employ_datQ56.7.College$CatOutcome, employ_datQ56.7.College$Q3, employ_datQ56.7.College)
analysis <- polr(employ_datQ56.7.College$CatOutcome ~ employ_datQ56.7.College$Q3, data=employ_datQ56.7.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.7.Carer<-multidatClean(Q56.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.Carer$Carer<- factor(employ_datQ56.7.Carer$Carer)
employ_datQ56.7.Carer<-ordinaldatClean(employ_datQ56.7.Carer$Q56.7.a, employ_datQ56.7.Carer)
#ordinal(employ_datQ56.7.Carer$CatOutcome, employ_datQ56.7.Carer$Carer, employ_datQ56.7.Carer)
prep <- analysisPrep(employ_datQ56.7.Carer$CatOutcome, employ_datQ56.7.Carer$Carer, employ_datQ56.7.Carer)
analysis <- polr(employ_datQ56.7.Carer$CatOutcome ~ employ_datQ56.7.Carer$Carer, data=employ_datQ56.7.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.7.Disability<-multidatClean(Q56.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.Disability$Disability<- factor(employ_datQ56.7.Disability$Disability)
employ_datQ56.7.Disability<-ordinaldatClean(employ_datQ56.7.Disability$Q56.7.a, employ_datQ56.7.Disability)
conTable <- xtabs(~Q56.7.a + Disability, data = employ_datQ56.7.Disability)
#ordinal(employ_datQ56.7.Disability$CatOutcome, employ_datQ56.7.Disability$Disability, employ_datQ56.7.Disability)
prep <- analysisPrep(employ_datQ56.7.Disability$CatOutcome, employ_datQ56.7.Disability$Disability, employ_datQ56.7.Disability)
analysis <- polr(employ_datQ56.7.Disability$CatOutcome ~ employ_datQ56.7.Disability$Disability, data=employ_datQ56.7.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.7.Ethnicity<-multidatClean(Q56.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.Ethnicity$Ethnicity<- factor(employ_datQ56.7.Ethnicity$EthnicityCleaned)
employ_datQ56.7.Ethnicity<-ordinaldatClean(employ_datQ56.7.Ethnicity$Q56.7.a, employ_datQ56.7.Ethnicity)
conTable <- xtabs(~Q56.7.a + EthnicityCleaned, data = employ_datQ56.7.Ethnicity)
conTable
#ordinal(employ_datQ56.7.Ethnicity$CatOutcome, employ_datQ56.7.Ethnicity$EthnicityCleaned, employ_datQ56.7.Ethnicity)
prep <- analysisPrep(employ_datQ56.7.Ethnicity$CatOutcome, employ_datQ56.7.Ethnicity$EthnicityCleaned, employ_datQ56.7.Ethnicity)
analysis <- polr(employ_datQ56.7.Ethnicity$CatOutcome ~ employ_datQ56.7.Ethnicity$EthnicityCleaned, data=employ_datQ56.7.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.7.FirstGen<-multidatClean(Q56.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.FirstGen$FirstGen<-factor(employ_datQ56.7.FirstGen$FirstGen)
employ_datQ56.7.FirstGen<-ordinaldatClean(employ_datQ56.7.FirstGen$Q56.7.a, employ_datQ56.7.FirstGen)
#ordinal(employ_datQ56.7.FirstGen$CatOutcome, employ_datQ56.7.FirstGen$FirstGen, employ_datQ56.7.FirstGen)
prep <- analysisPrep(employ_datQ56.7.FirstGen$CatOutcome, employ_datQ56.7.FirstGen$FirstGen, employ_datQ56.7.FirstGen)
analysis <- polr(employ_datQ56.7.FirstGen$CatOutcome ~ employ_datQ56.7.FirstGen$FirstGen, data=employ_datQ56.7.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.7.Gender<-multidatClean(Q56.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.Gender$Gender<-factor(employ_datQ56.7.Gender$Gender)
employ_datQ56.7.Gender<-ordinaldatClean(employ_datQ56.7.Gender$Q56.7.a, employ_datQ56.7.Gender)
#ordinal(employ_datQ56.7.Gender$CatOutcome, employ_datQ56.7.Gender$Gender, employ_datQ56.7.Gender)
prep <- analysisPrep(employ_datQ56.7.Gender$CatOutcome, employ_datQ56.7.Gender$Gender, employ_datQ56.7.Gender)
analysis <- polr(employ_datQ56.7.Gender$CatOutcome ~ employ_datQ56.7.Gender$Gender, data=employ_datQ56.7.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.7.Sexuality<-multidatClean(Q56.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.7.Sexuality$Sexuality<-factor(employ_datQ56.7.Sexuality$Sexuality)
employ_datQ56.7.Sexuality<-ordinaldatClean(employ_datQ56.7.Sexuality$Q56.7.a, employ_datQ56.7.Sexuality)
#ordinal(employ_datQ56.7.Sexuality$CatOutcome, employ_datQ56.7.Sexuality$Sexuality, employ_datQ56.7.Sexuality)
prep <- analysisPrep(employ_datQ56.7.Sexuality$CatOutcome, employ_datQ56.7.Sexuality$Sexuality, employ_datQ56.7.Sexuality)
analysis <- polr(employ_datQ56.7.Sexuality$CatOutcome ~ employ_datQ56.7.Sexuality$Sexuality, data=employ_datQ56.7.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q56.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q56 (using orddatclean) The COVID pandemic has naturally caused some concerns. Have you felt
### Q56 (using orddatclean) I have been able to access help and advice during the COVID pandemic
"Q56.8.a - How do you think the University of Edinburgh compares to others in regards to encouraging good research culture?"
"Status"
employ_datQ56.8.<-multidatClean(Q56.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.8.a + Academic, data = employ_datQ56.8.)
detach(dat_long)
employ_datQ56.8.<-ordinaldatClean(employ_datQ56.8.$Q56.8.a, employ_datQ56.8.)
#ordinal(employ_datQ56.8.$CatOutcome, employ_datQ56.8.$Academic, employ_datQ56.8.)
prep <- analysisPrep(employ_datQ56.8.$CatOutcome, employ_datQ56.8.$Academic, employ_datQ56.8.)
analysis <- polr(employ_datQ56.8.$CatOutcome ~ employ_datQ56.8.$Academic, data=employ_datQ56.8., Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q56.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ56.8.College<-multidatClean(Q56.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q56.8.a + Q3, data = employ_datQ56.8.College)
conTable
detach(dat_long)
employ_datQ56.8.College$Q3[(employ_datQ56.8.College$Q3 == "Research Professional Staff")]="Other"
employ_datQ56.8.College$Q3<- factor(employ_datQ56.8.College$Q3)
employ_datQ56.8.College<-ordinaldatClean(employ_datQ56.8.College$Q56.8.a, employ_datQ56.8.College)
#ordinal(employ_datQ56.8.College$CatOutcome, employ_datQ56.8.College$Q3, employ_datQ56.8.College)
prep <- analysisPrep(employ_datQ56.8.College$CatOutcome, employ_datQ56.8.College$Q3, employ_datQ56.8.College)
analysis <- polr(employ_datQ56.8.College$CatOutcome ~ employ_datQ56.8.College$Q3, data=employ_datQ56.8.College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ56.8.Carer<-multidatClean(Q56.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.Carer$Carer<- factor(employ_datQ56.8.Carer$Carer)
employ_datQ56.8.Carer<-ordinaldatClean(employ_datQ56.8.Carer$Q56.8.a, employ_datQ56.8.Carer)
#ordinal(employ_datQ56.8.Carer$CatOutcome, employ_datQ56.8.Carer$Carer, employ_datQ56.8.Carer)
prep <- analysisPrep(employ_datQ56.8.Carer$CatOutcome, employ_datQ56.8.Carer$Carer, employ_datQ56.8.Carer)
analysis <- polr(employ_datQ56.8.Carer$CatOutcome ~ employ_datQ56.8.Carer$Carer, data=employ_datQ56.8.Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ56.8.Disability<-multidatClean(Q56.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.Disability$Disability<- factor(employ_datQ56.8.Disability$Disability)
employ_datQ56.8.Disability<-ordinaldatClean(employ_datQ56.8.Disability$Q56.8.a, employ_datQ56.8.Disability)
conTable <- xtabs(~Q56.8.a + Disability, data = employ_datQ56.8.Disability)
#ordinal(employ_datQ56.8.Disability$CatOutcome, employ_datQ56.8.Disability$Disability, employ_datQ56.8.Disability)
prep <- analysisPrep(employ_datQ56.8.Disability$CatOutcome, employ_datQ56.8.Disability$Disability, employ_datQ56.8.Disability)
analysis <- polr(employ_datQ56.8.Disability$CatOutcome ~ employ_datQ56.8.Disability$Disability, data=employ_datQ56.8.Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ56.8.Ethnicity<-multidatClean(Q56.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.Ethnicity$Ethnicity<- factor(employ_datQ56.8.Ethnicity$EthnicityCleaned)
employ_datQ56.8.Ethnicity<-ordinaldatClean(employ_datQ56.8.Ethnicity$Q56.8.a, employ_datQ56.8.Ethnicity)
conTable <- xtabs(~Q56.8.a + EthnicityCleaned, data = employ_datQ56.8.Ethnicity)
conTable
#ordinal(employ_datQ56.8.Ethnicity$CatOutcome, employ_datQ56.8.Ethnicity$EthnicityCleaned, employ_datQ56.8.Ethnicity)
prep <- analysisPrep(employ_datQ56.8.Ethnicity$CatOutcome, employ_datQ56.8.Ethnicity$EthnicityCleaned, employ_datQ56.8.Ethnicity)
analysis <- polr(employ_datQ56.8.Ethnicity$CatOutcome ~ employ_datQ56.8.Ethnicity$EthnicityCleaned, data=employ_datQ56.8.Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ56.8.FirstGen<-multidatClean(Q56.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.FirstGen$FirstGen<-factor(employ_datQ56.8.FirstGen$FirstGen)
employ_datQ56.8.FirstGen<-ordinaldatClean(employ_datQ56.8.FirstGen$Q56.8.a, employ_datQ56.8.FirstGen)
#ordinal(employ_datQ56.8.FirstGen$CatOutcome, employ_datQ56.8.FirstGen$FirstGen, employ_datQ56.8.FirstGen)
prep <- analysisPrep(employ_datQ56.8.FirstGen$CatOutcome, employ_datQ56.8.FirstGen$FirstGen, employ_datQ56.8.FirstGen)
analysis <- polr(employ_datQ56.8.FirstGen$CatOutcome ~ employ_datQ56.8.FirstGen$FirstGen, data=employ_datQ56.8.FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ56.8.Gender<-multidatClean(Q56.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.Gender$Gender<-factor(employ_datQ56.8.Gender$Gender)
employ_datQ56.8.Gender<-ordinaldatClean(employ_datQ56.8.Gender$Q56.8.a, employ_datQ56.8.Gender)
#ordinal(employ_datQ56.8.Gender$CatOutcome, employ_datQ56.8.Gender$Gender, employ_datQ56.8.Gender)
prep <- analysisPrep(employ_datQ56.8.Gender$CatOutcome, employ_datQ56.8.Gender$Gender, employ_datQ56.8.Gender)
analysis <- polr(employ_datQ56.8.Gender$CatOutcome ~ employ_datQ56.8.Gender$Gender, data=employ_datQ56.8.Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ56.8.Sexuality<-multidatClean(Q56.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ56.8.Sexuality$Sexuality<-factor(employ_datQ56.8.Sexuality$Sexuality)
employ_datQ56.8.Sexuality<-ordinaldatClean(employ_datQ56.8.Sexuality$Q56.8.a, employ_datQ56.8.Sexuality)
#ordinal(employ_datQ56.8.Sexuality$CatOutcome, employ_datQ56.8.Sexuality$Sexuality, employ_datQ56.8.Sexuality)
prep <- analysisPrep(employ_datQ56.8.Sexuality$CatOutcome, employ_datQ56.8.Sexuality$Sexuality, employ_datQ56.8.Sexuality)
analysis <- polr(employ_datQ56.8.Sexuality$CatOutcome ~ employ_datQ56.8.Sexuality$Sexuality, data=employ_datQ56.8.Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
confint.default(analysis) # CIs assuming normality
exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q56.8.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data) 
OR_Outcomes

Q56.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

