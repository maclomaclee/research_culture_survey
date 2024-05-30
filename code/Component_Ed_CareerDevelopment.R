source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239,82:91, 6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)
employ_dat$Academic <- as.character(employ_dat$Academic)
employ_dat$UniqueResponseNumber <- as.character(employ_dat$UniqueResponseNumber)

### Q25
"Status"
employ_datQ25.1.a<-multidatClean(Q25.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.1.a + Academic, data = employ_datQ25.1.a)
detach(dat_long)
employ_datQ25.1.a<-ordinaldatClean(employ_datQ25.1.a$Q25.1.a, employ_datQ25.1.a)
#ordinal(employ_datQ25.1.a$CatOutcome, employ_datQ25.1.a$Academic, employ_datQ25.1.a)
prep <- analysisPrep(employ_datQ25.1.a$CatOutcome, employ_datQ25.1.a$Academic, employ_datQ25.1.a)
analysis <- polr(employ_datQ25.1.a$CatOutcome ~ employ_datQ25.1.a$Academic, data=employ_datQ25.1.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ25.1.aCollege<-multidatClean(Q25.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.1.a + Q3, data = employ_datQ25.1.aCollege)
conTable
detach(dat_long)
employ_datQ25.1.aCollege$Q3[(employ_datQ25.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ25.1.aCollege$Q3<- factor(employ_datQ25.1.aCollege$Q3)
employ_datQ25.1.aCollege<-ordinaldatClean(employ_datQ25.1.aCollege$Q25.1.a, employ_datQ25.1.aCollege)
#ordinal(employ_datQ25.1.aCollege$CatOutcome, employ_datQ25.1.aCollege$Q3, employ_datQ25.1.aCollege)
prep <- analysisPrep(employ_datQ25.1.aCollege$CatOutcome, employ_datQ25.1.aCollege$Q3, employ_datQ25.1.aCollege)
analysis <- polr(employ_datQ25.1.aCollege$CatOutcome ~ employ_datQ25.1.aCollege$Q3, data=employ_datQ25.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ25.1.aCarer<-multidatClean(Q25.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aCarer$Carer<- factor(employ_datQ25.1.aCarer$Carer)
employ_datQ25.1.aCarer<-ordinaldatClean(employ_datQ25.1.aCarer$Q25.1.a, employ_datQ25.1.aCarer)
#ordinal(employ_datQ25.1.aCarer$CatOutcome, employ_datQ25.1.aCarer$Carer, employ_datQ25.1.aCarer)
prep <- analysisPrep(employ_datQ25.1.aCarer$CatOutcome, employ_datQ25.1.aCarer$Carer, employ_datQ25.1.aCarer)
analysis <- polr(employ_datQ25.1.aCarer$CatOutcome ~ employ_datQ25.1.aCarer$Carer, data=employ_datQ25.1.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ25.1.aDisability<-multidatClean(Q25.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aDisability$Disability<- factor(employ_datQ25.1.aDisability$Disability)
employ_datQ25.1.aDisability<-ordinaldatClean(employ_datQ25.1.aDisability$Q25.1.a, employ_datQ25.1.aDisability)
conTable <- xtabs(~Q25.1.a + Disability, data = employ_datQ25.1.aDisability)
#ordinal(employ_datQ25.1.aDisability$CatOutcome, employ_datQ25.1.aDisability$Disability, employ_datQ25.1.aDisability)
prep <- analysisPrep(employ_datQ25.1.aDisability$CatOutcome, employ_datQ25.1.aDisability$Disability, employ_datQ25.1.aDisability)
analysis <- polr(employ_datQ25.1.aDisability$CatOutcome ~ employ_datQ25.1.aDisability$Disability, data=employ_datQ25.1.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ25.1.aEthnicity<-multidatClean(Q25.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aEthnicity$Ethnicity<- factor(employ_datQ25.1.aEthnicity$EthnicityCleaned)
employ_datQ25.1.aEthnicity<-ordinaldatClean(employ_datQ25.1.aEthnicity$Q25.1.a, employ_datQ25.1.aEthnicity)
conTable <- xtabs(~Q25.1.a + EthnicityCleaned, data = employ_datQ25.1.aEthnicity)
conTable
#ordinal(employ_datQ25.1.aEthnicity$CatOutcome, employ_datQ25.1.aEthnicity$EthnicityCleaned, employ_datQ25.1.aEthnicity)
prep <- analysisPrep(employ_datQ25.1.aEthnicity$CatOutcome, employ_datQ25.1.aEthnicity$EthnicityCleaned, employ_datQ25.1.aEthnicity)
analysis <- polr(employ_datQ25.1.aEthnicity$CatOutcome ~ employ_datQ25.1.aEthnicity$EthnicityCleaned, data=employ_datQ25.1.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ25.1.aFirstGen<-multidatClean(Q25.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aFirstGen$FirstGen<-factor(employ_datQ25.1.aFirstGen$FirstGen)
employ_datQ25.1.aFirstGen<-ordinaldatClean(employ_datQ25.1.aFirstGen$Q25.1.a, employ_datQ25.1.aFirstGen)
#ordinal(employ_datQ25.1.aFirstGen$CatOutcome, employ_datQ25.1.aFirstGen$FirstGen, employ_datQ25.1.aFirstGen)
prep <- analysisPrep(employ_datQ25.1.aFirstGen$CatOutcome, employ_datQ25.1.aFirstGen$FirstGen, employ_datQ25.1.aFirstGen)
analysis <- polr(employ_datQ25.1.aFirstGen$CatOutcome ~ employ_datQ25.1.aFirstGen$FirstGen, data=employ_datQ25.1.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ25.1.aGender<-multidatClean(Q25.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aGender$Gender<-factor(employ_datQ25.1.aGender$Gender)
employ_datQ25.1.aGender<-ordinaldatClean(employ_datQ25.1.aGender$Q25.1.a, employ_datQ25.1.aGender)
#ordinal(employ_datQ25.1.aGender$CatOutcome, employ_datQ25.1.aGender$Gender, employ_datQ25.1.aGender)
prep <- analysisPrep(employ_datQ25.1.aGender$CatOutcome, employ_datQ25.1.aGender$Gender, employ_datQ25.1.aGender)
analysis <- polr(employ_datQ25.1.aGender$CatOutcome ~ employ_datQ25.1.aGender$Gender, data=employ_datQ25.1.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ25.1.aSexuality<-multidatClean(Q25.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.1.aSexuality$Sexuality<-factor(employ_datQ25.1.aSexuality$Sexuality)
employ_datQ25.1.aSexuality<-ordinaldatClean(employ_datQ25.1.aSexuality$Q25.1.a, employ_datQ25.1.aSexuality)
#ordinal(employ_datQ25.1.aSexuality$CatOutcome, employ_datQ25.1.aSexuality$Sexuality, employ_datQ25.1.aSexuality)
prep <- analysisPrep(employ_datQ25.1.aSexuality$CatOutcome, employ_datQ25.1.aSexuality$Sexuality, employ_datQ25.1.aSexuality)
analysis <- polr(employ_datQ25.1.aSexuality$CatOutcome ~ employ_datQ25.1.aSexuality$Sexuality, data=employ_datQ25.1.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q25.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q25
"Status"
employ_datQ25.2.a<-multidatClean(Q25.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.2.a + Academic, data = employ_datQ25.2.a)
detach(dat_long)
employ_datQ25.2.a<-ordinaldatClean(employ_datQ25.2.a$Q25.2.a, employ_datQ25.2.a)
#ordinal(employ_datQ25.2.a$CatOutcome, employ_datQ25.2.a$Academic, employ_datQ25.2.a)
prep <- analysisPrep(employ_datQ25.2.a$CatOutcome, employ_datQ25.2.a$Academic, employ_datQ25.2.a)
analysis <- polr(employ_datQ25.2.a$CatOutcome ~ employ_datQ25.2.a$Academic, data=employ_datQ25.2.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ25.2.aCollege<-multidatClean(Q25.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.2.a + Q3, data = employ_datQ25.2.aCollege)
conTable
detach(dat_long)
employ_datQ25.2.aCollege$Q3[(employ_datQ25.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ25.2.aCollege$Q3<- factor(employ_datQ25.2.aCollege$Q3)
employ_datQ25.2.aCollege<-ordinaldatClean(employ_datQ25.2.aCollege$Q25.2.a, employ_datQ25.2.aCollege)
#ordinal(employ_datQ25.2.aCollege$CatOutcome, employ_datQ25.2.aCollege$Q3, employ_datQ25.2.aCollege)
prep <- analysisPrep(employ_datQ25.2.aCollege$CatOutcome, employ_datQ25.2.aCollege$Q3, employ_datQ25.2.aCollege)
analysis <- polr(employ_datQ25.2.aCollege$CatOutcome ~ employ_datQ25.2.aCollege$Q3, data=employ_datQ25.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ25.2.aCarer<-multidatClean(Q25.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aCarer$Carer<- factor(employ_datQ25.2.aCarer$Carer)
employ_datQ25.2.aCarer<-ordinaldatClean(employ_datQ25.2.aCarer$Q25.2.a, employ_datQ25.2.aCarer)
#ordinal(employ_datQ25.2.aCarer$CatOutcome, employ_datQ25.2.aCarer$Carer, employ_datQ25.2.aCarer)
prep <- analysisPrep(employ_datQ25.2.aCarer$CatOutcome, employ_datQ25.2.aCarer$Carer, employ_datQ25.2.aCarer)
analysis <- polr(employ_datQ25.2.aCarer$CatOutcome ~ employ_datQ25.2.aCarer$Carer, data=employ_datQ25.2.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ25.2.aDisability<-multidatClean(Q25.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aDisability$Disability<- factor(employ_datQ25.2.aDisability$Disability)
employ_datQ25.2.aDisability<-ordinaldatClean(employ_datQ25.2.aDisability$Q25.2.a, employ_datQ25.2.aDisability)
conTable <- xtabs(~Q25.2.a + Disability, data = employ_datQ25.2.aDisability)
#ordinal(employ_datQ25.2.aDisability$CatOutcome, employ_datQ25.2.aDisability$Disability, employ_datQ25.2.aDisability)
prep <- analysisPrep(employ_datQ25.2.aDisability$CatOutcome, employ_datQ25.2.aDisability$Disability, employ_datQ25.2.aDisability)
analysis <- polr(employ_datQ25.2.aDisability$CatOutcome ~ employ_datQ25.2.aDisability$Disability, data=employ_datQ25.2.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ25.2.aEthnicity<-multidatClean(Q25.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aEthnicity$Ethnicity<- factor(employ_datQ25.2.aEthnicity$EthnicityCleaned)
employ_datQ25.2.aEthnicity<-ordinaldatClean(employ_datQ25.2.aEthnicity$Q25.2.a, employ_datQ25.2.aEthnicity)
conTable <- xtabs(~Q25.2.a + EthnicityCleaned, data = employ_datQ25.2.aEthnicity)
conTable
#ordinal(employ_datQ25.2.aEthnicity$CatOutcome, employ_datQ25.2.aEthnicity$EthnicityCleaned, employ_datQ25.2.aEthnicity)
prep <- analysisPrep(employ_datQ25.2.aEthnicity$CatOutcome, employ_datQ25.2.aEthnicity$EthnicityCleaned, employ_datQ25.2.aEthnicity)
analysis <- polr(employ_datQ25.2.aEthnicity$CatOutcome ~ employ_datQ25.2.aEthnicity$EthnicityCleaned, data=employ_datQ25.2.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ25.2.aFirstGen<-multidatClean(Q25.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aFirstGen$FirstGen<-factor(employ_datQ25.2.aFirstGen$FirstGen)
employ_datQ25.2.aFirstGen<-ordinaldatClean(employ_datQ25.2.aFirstGen$Q25.2.a, employ_datQ25.2.aFirstGen)
#ordinal(employ_datQ25.2.aFirstGen$CatOutcome, employ_datQ25.2.aFirstGen$FirstGen, employ_datQ25.2.aFirstGen)
prep <- analysisPrep(employ_datQ25.2.aFirstGen$CatOutcome, employ_datQ25.2.aFirstGen$FirstGen, employ_datQ25.2.aFirstGen)
analysis <- polr(employ_datQ25.2.aFirstGen$CatOutcome ~ employ_datQ25.2.aFirstGen$FirstGen, data=employ_datQ25.2.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ25.2.aGender<-multidatClean(Q25.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aGender$Gender<-factor(employ_datQ25.2.aGender$Gender)
employ_datQ25.2.aGender<-ordinaldatClean(employ_datQ25.2.aGender$Q25.2.a, employ_datQ25.2.aGender)
#ordinal(employ_datQ25.2.aGender$CatOutcome, employ_datQ25.2.aGender$Gender, employ_datQ25.2.aGender)
prep <- analysisPrep(employ_datQ25.2.aGender$CatOutcome, employ_datQ25.2.aGender$Gender, employ_datQ25.2.aGender)
analysis <- polr(employ_datQ25.2.aGender$CatOutcome ~ employ_datQ25.2.aGender$Gender, data=employ_datQ25.2.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ25.2.aSexuality<-multidatClean(Q25.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.2.aSexuality$Sexuality<-factor(employ_datQ25.2.aSexuality$Sexuality)
employ_datQ25.2.aSexuality<-ordinaldatClean(employ_datQ25.2.aSexuality$Q25.2.a, employ_datQ25.2.aSexuality)
#ordinal(employ_datQ25.2.aSexuality$CatOutcome, employ_datQ25.2.aSexuality$Sexuality, employ_datQ25.2.aSexuality)
prep <- analysisPrep(employ_datQ25.2.aSexuality$CatOutcome, employ_datQ25.2.aSexuality$Sexuality, employ_datQ25.2.aSexuality)
analysis <- polr(employ_datQ25.2.aSexuality$CatOutcome ~ employ_datQ25.2.aSexuality$Sexuality, data=employ_datQ25.2.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q25.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q25
"Status"
employ_datQ25.3.a<-multidatClean(Q25.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.3.a + Academic, data = employ_datQ25.3.a)
detach(dat_long)
employ_datQ25.3.a<-ordinaldatClean(employ_datQ25.3.a$Q25.3.a, employ_datQ25.3.a)
#ordinal(employ_datQ25.3.a$CatOutcome, employ_datQ25.3.a$Academic, employ_datQ25.3.a)
prep <- analysisPrep(employ_datQ25.3.a$CatOutcome, employ_datQ25.3.a$Academic, employ_datQ25.3.a)
analysis <- polr(employ_datQ25.3.a$CatOutcome ~ employ_datQ25.3.a$Academic, data=employ_datQ25.3.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ25.3.aCollege<-multidatClean(Q25.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.3.a + Q3, data = employ_datQ25.3.aCollege)
conTable
detach(dat_long)
employ_datQ25.3.aCollege$Q3[(employ_datQ25.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ25.3.aCollege$Q3<- factor(employ_datQ25.3.aCollege$Q3)
employ_datQ25.3.aCollege<-ordinaldatClean(employ_datQ25.3.aCollege$Q25.3.a, employ_datQ25.3.aCollege)
#ordinal(employ_datQ25.3.aCollege$CatOutcome, employ_datQ25.3.aCollege$Q3, employ_datQ25.3.aCollege)
prep <- analysisPrep(employ_datQ25.3.aCollege$CatOutcome, employ_datQ25.3.aCollege$Q3, employ_datQ25.3.aCollege)
analysis <- polr(employ_datQ25.3.aCollege$CatOutcome ~ employ_datQ25.3.aCollege$Q3, data=employ_datQ25.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ25.3.aCarer<-multidatClean(Q25.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aCarer$Carer<- factor(employ_datQ25.3.aCarer$Carer)
employ_datQ25.3.aCarer<-ordinaldatClean(employ_datQ25.3.aCarer$Q25.3.a, employ_datQ25.3.aCarer)
#ordinal(employ_datQ25.3.aCarer$CatOutcome, employ_datQ25.3.aCarer$Carer, employ_datQ25.3.aCarer)
prep <- analysisPrep(employ_datQ25.3.aCarer$CatOutcome, employ_datQ25.3.aCarer$Carer, employ_datQ25.3.aCarer)
analysis <- polr(employ_datQ25.3.aCarer$CatOutcome ~ employ_datQ25.3.aCarer$Carer, data=employ_datQ25.3.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ25.3.aDisability<-multidatClean(Q25.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aDisability$Disability<- factor(employ_datQ25.3.aDisability$Disability)
employ_datQ25.3.aDisability<-ordinaldatClean(employ_datQ25.3.aDisability$Q25.3.a, employ_datQ25.3.aDisability)
conTable <- xtabs(~Q25.3.a + Disability, data = employ_datQ25.3.aDisability)
#ordinal(employ_datQ25.3.aDisability$CatOutcome, employ_datQ25.3.aDisability$Disability, employ_datQ25.3.aDisability)
prep <- analysisPrep(employ_datQ25.3.aDisability$CatOutcome, employ_datQ25.3.aDisability$Disability, employ_datQ25.3.aDisability)
analysis <- polr(employ_datQ25.3.aDisability$CatOutcome ~ employ_datQ25.3.aDisability$Disability, data=employ_datQ25.3.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ25.3.aEthnicity<-multidatClean(Q25.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aEthnicity$Ethnicity<- factor(employ_datQ25.3.aEthnicity$EthnicityCleaned)
employ_datQ25.3.aEthnicity<-ordinaldatClean(employ_datQ25.3.aEthnicity$Q25.3.a, employ_datQ25.3.aEthnicity)
conTable <- xtabs(~Q25.3.a + EthnicityCleaned, data = employ_datQ25.3.aEthnicity)
conTable
#ordinal(employ_datQ25.3.aEthnicity$CatOutcome, employ_datQ25.3.aEthnicity$EthnicityCleaned, employ_datQ25.3.aEthnicity)
prep <- analysisPrep(employ_datQ25.3.aEthnicity$CatOutcome, employ_datQ25.3.aEthnicity$EthnicityCleaned, employ_datQ25.3.aEthnicity)
analysis <- polr(employ_datQ25.3.aEthnicity$CatOutcome ~ employ_datQ25.3.aEthnicity$EthnicityCleaned, data=employ_datQ25.3.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ25.3.aFirstGen<-multidatClean(Q25.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aFirstGen$FirstGen<-factor(employ_datQ25.3.aFirstGen$FirstGen)
employ_datQ25.3.aFirstGen<-ordinaldatClean(employ_datQ25.3.aFirstGen$Q25.3.a, employ_datQ25.3.aFirstGen)
#ordinal(employ_datQ25.3.aFirstGen$CatOutcome, employ_datQ25.3.aFirstGen$FirstGen, employ_datQ25.3.aFirstGen)
prep <- analysisPrep(employ_datQ25.3.aFirstGen$CatOutcome, employ_datQ25.3.aFirstGen$FirstGen, employ_datQ25.3.aFirstGen)
analysis <- polr(employ_datQ25.3.aFirstGen$CatOutcome ~ employ_datQ25.3.aFirstGen$FirstGen, data=employ_datQ25.3.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ25.3.aGender<-multidatClean(Q25.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aGender$Gender<-factor(employ_datQ25.3.aGender$Gender)
employ_datQ25.3.aGender<-ordinaldatClean(employ_datQ25.3.aGender$Q25.3.a, employ_datQ25.3.aGender)
#ordinal(employ_datQ25.3.aGender$CatOutcome, employ_datQ25.3.aGender$Gender, employ_datQ25.3.aGender)
prep <- analysisPrep(employ_datQ25.3.aGender$CatOutcome, employ_datQ25.3.aGender$Gender, employ_datQ25.3.aGender)
analysis <- polr(employ_datQ25.3.aGender$CatOutcome ~ employ_datQ25.3.aGender$Gender, data=employ_datQ25.3.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ25.3.aSexuality<-multidatClean(Q25.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.3.aSexuality$Sexuality<-factor(employ_datQ25.3.aSexuality$Sexuality)
employ_datQ25.3.aSexuality<-ordinaldatClean(employ_datQ25.3.aSexuality$Q25.3.a, employ_datQ25.3.aSexuality)
#ordinal(employ_datQ25.3.aSexuality$CatOutcome, employ_datQ25.3.aSexuality$Sexuality, employ_datQ25.3.aSexuality)
prep <- analysisPrep(employ_datQ25.3.aSexuality$CatOutcome, employ_datQ25.3.aSexuality$Sexuality, employ_datQ25.3.aSexuality)
analysis <- polr(employ_datQ25.3.aSexuality$CatOutcome ~ employ_datQ25.3.aSexuality$Sexuality, data=employ_datQ25.3.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q25.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q25
"Status"
employ_datQ25.4.a<-multidatClean(Q25.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.4.a + Academic, data = employ_datQ25.4.a)
detach(dat_long)
employ_datQ25.4.a<-ordinaldatClean(employ_datQ25.4.a$Q25.4.a, employ_datQ25.4.a)
#ordinal(employ_datQ25.4.a$CatOutcome, employ_datQ25.4.a$Academic, employ_datQ25.4.a)
prep <- analysisPrep(employ_datQ25.4.a$CatOutcome, employ_datQ25.4.a$Academic, employ_datQ25.4.a)
analysis <- polr(employ_datQ25.4.a$CatOutcome ~ employ_datQ25.4.a$Academic, data=employ_datQ25.4.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ25.4.aCollege<-multidatClean(Q25.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.4.a + Q3, data = employ_datQ25.4.aCollege)
conTable
detach(dat_long)
employ_datQ25.4.aCollege$Q3[(employ_datQ25.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ25.4.aCollege$Q3<- factor(employ_datQ25.4.aCollege$Q3)
employ_datQ25.4.aCollege<-ordinaldatClean(employ_datQ25.4.aCollege$Q25.4.a, employ_datQ25.4.aCollege)
#ordinal(employ_datQ25.4.aCollege$CatOutcome, employ_datQ25.4.aCollege$Q3, employ_datQ25.4.aCollege)
prep <- analysisPrep(employ_datQ25.4.aCollege$CatOutcome, employ_datQ25.4.aCollege$Q3, employ_datQ25.4.aCollege)
analysis <- polr(employ_datQ25.4.aCollege$CatOutcome ~ employ_datQ25.4.aCollege$Q3, data=employ_datQ25.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ25.4.aCarer<-multidatClean(Q25.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aCarer$Carer<- factor(employ_datQ25.4.aCarer$Carer)
employ_datQ25.4.aCarer<-ordinaldatClean(employ_datQ25.4.aCarer$Q25.4.a, employ_datQ25.4.aCarer)
#ordinal(employ_datQ25.4.aCarer$CatOutcome, employ_datQ25.4.aCarer$Carer, employ_datQ25.4.aCarer)
prep <- analysisPrep(employ_datQ25.4.aCarer$CatOutcome, employ_datQ25.4.aCarer$Carer, employ_datQ25.4.aCarer)
analysis <- polr(employ_datQ25.4.aCarer$CatOutcome ~ employ_datQ25.4.aCarer$Carer, data=employ_datQ25.4.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ25.4.aDisability<-multidatClean(Q25.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aDisability$Disability<- factor(employ_datQ25.4.aDisability$Disability)
employ_datQ25.4.aDisability<-ordinaldatClean(employ_datQ25.4.aDisability$Q25.4.a, employ_datQ25.4.aDisability)
conTable <- xtabs(~Q25.4.a + Disability, data = employ_datQ25.4.aDisability)
#ordinal(employ_datQ25.4.aDisability$CatOutcome, employ_datQ25.4.aDisability$Disability, employ_datQ25.4.aDisability)
prep <- analysisPrep(employ_datQ25.4.aDisability$CatOutcome, employ_datQ25.4.aDisability$Disability, employ_datQ25.4.aDisability)
analysis <- polr(employ_datQ25.4.aDisability$CatOutcome ~ employ_datQ25.4.aDisability$Disability, data=employ_datQ25.4.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ25.4.aEthnicity<-multidatClean(Q25.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aEthnicity$Ethnicity<- factor(employ_datQ25.4.aEthnicity$EthnicityCleaned)
employ_datQ25.4.aEthnicity<-ordinaldatClean(employ_datQ25.4.aEthnicity$Q25.4.a, employ_datQ25.4.aEthnicity)
conTable <- xtabs(~Q25.4.a + EthnicityCleaned, data = employ_datQ25.4.aEthnicity)
conTable
#ordinal(employ_datQ25.4.aEthnicity$CatOutcome, employ_datQ25.4.aEthnicity$EthnicityCleaned, employ_datQ25.4.aEthnicity)
prep <- analysisPrep(employ_datQ25.4.aEthnicity$CatOutcome, employ_datQ25.4.aEthnicity$EthnicityCleaned, employ_datQ25.4.aEthnicity)
analysis <- polr(employ_datQ25.4.aEthnicity$CatOutcome ~ employ_datQ25.4.aEthnicity$EthnicityCleaned, data=employ_datQ25.4.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ25.4.aFirstGen<-multidatClean(Q25.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aFirstGen$FirstGen<-factor(employ_datQ25.4.aFirstGen$FirstGen)
employ_datQ25.4.aFirstGen<-ordinaldatClean(employ_datQ25.4.aFirstGen$Q25.4.a, employ_datQ25.4.aFirstGen)
#ordinal(employ_datQ25.4.aFirstGen$CatOutcome, employ_datQ25.4.aFirstGen$FirstGen, employ_datQ25.4.aFirstGen)
prep <- analysisPrep(employ_datQ25.4.aFirstGen$CatOutcome, employ_datQ25.4.aFirstGen$FirstGen, employ_datQ25.4.aFirstGen)
analysis <- polr(employ_datQ25.4.aFirstGen$CatOutcome ~ employ_datQ25.4.aFirstGen$FirstGen, data=employ_datQ25.4.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ25.4.aGender<-multidatClean(Q25.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aGender$Gender<-factor(employ_datQ25.4.aGender$Gender)
employ_datQ25.4.aGender<-ordinaldatClean(employ_datQ25.4.aGender$Q25.4.a, employ_datQ25.4.aGender)
#ordinal(employ_datQ25.4.aGender$CatOutcome, employ_datQ25.4.aGender$Gender, employ_datQ25.4.aGender)
prep <- analysisPrep(employ_datQ25.4.aGender$CatOutcome, employ_datQ25.4.aGender$Gender, employ_datQ25.4.aGender)
analysis <- polr(employ_datQ25.4.aGender$CatOutcome ~ employ_datQ25.4.aGender$Gender, data=employ_datQ25.4.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ25.4.aSexuality<-multidatClean(Q25.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.4.aSexuality$Sexuality<-factor(employ_datQ25.4.aSexuality$Sexuality)
employ_datQ25.4.aSexuality<-ordinaldatClean(employ_datQ25.4.aSexuality$Q25.4.a, employ_datQ25.4.aSexuality)
#ordinal(employ_datQ25.4.aSexuality$CatOutcome, employ_datQ25.4.aSexuality$Sexuality, employ_datQ25.4.aSexuality)
prep <- analysisPrep(employ_datQ25.4.aSexuality$CatOutcome, employ_datQ25.4.aSexuality$Sexuality, employ_datQ25.4.aSexuality)
analysis <- polr(employ_datQ25.4.aSexuality$CatOutcome ~ employ_datQ25.4.aSexuality$Sexuality, data=employ_datQ25.4.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q25.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q25
"Status"
employ_datQ25.5.a<-multidatClean(Q25.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.5.a + Academic, data = employ_datQ25.5.a)
detach(dat_long)
employ_datQ25.5.a<-ordinaldatClean(employ_datQ25.5.a$Q25.5.a, employ_datQ25.5.a)
#ordinal(employ_datQ25.5.a$CatOutcome, employ_datQ25.5.a$Academic, employ_datQ25.5.a)
prep <- analysisPrep(employ_datQ25.5.a$CatOutcome, employ_datQ25.5.a$Academic, employ_datQ25.5.a)
analysis <- polr(employ_datQ25.5.a$CatOutcome ~ employ_datQ25.5.a$Academic, data=employ_datQ25.5.a, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ25.5.aCollege<-multidatClean(Q25.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q25.5.a + Q3, data = employ_datQ25.5.aCollege)
conTable
detach(dat_long)
employ_datQ25.5.aCollege$Q3[(employ_datQ25.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ25.5.aCollege$Q3<- factor(employ_datQ25.5.aCollege$Q3)
employ_datQ25.5.aCollege<-ordinaldatClean(employ_datQ25.5.aCollege$Q25.5.a, employ_datQ25.5.aCollege)
#ordinal(employ_datQ25.5.aCollege$CatOutcome, employ_datQ25.5.aCollege$Q3, employ_datQ25.5.aCollege)
prep <- analysisPrep(employ_datQ25.5.aCollege$CatOutcome, employ_datQ25.5.aCollege$Q3, employ_datQ25.5.aCollege)
analysis <- polr(employ_datQ25.5.aCollege$CatOutcome ~ employ_datQ25.5.aCollege$Q3, data=employ_datQ25.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ25.5.aCarer<-multidatClean(Q25.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aCarer$Carer<- factor(employ_datQ25.5.aCarer$Carer)
employ_datQ25.5.aCarer<-ordinaldatClean(employ_datQ25.5.aCarer$Q25.5.a, employ_datQ25.5.aCarer)
#ordinal(employ_datQ25.5.aCarer$CatOutcome, employ_datQ25.5.aCarer$Carer, employ_datQ25.5.aCarer)
prep <- analysisPrep(employ_datQ25.5.aCarer$CatOutcome, employ_datQ25.5.aCarer$Carer, employ_datQ25.5.aCarer)
analysis <- polr(employ_datQ25.5.aCarer$CatOutcome ~ employ_datQ25.5.aCarer$Carer, data=employ_datQ25.5.aCarer, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ25.5.aDisability<-multidatClean(Q25.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aDisability$Disability<- factor(employ_datQ25.5.aDisability$Disability)
employ_datQ25.5.aDisability<-ordinaldatClean(employ_datQ25.5.aDisability$Q25.5.a, employ_datQ25.5.aDisability)
conTable <- xtabs(~Q25.5.a + Disability, data = employ_datQ25.5.aDisability)
#ordinal(employ_datQ25.5.aDisability$CatOutcome, employ_datQ25.5.aDisability$Disability, employ_datQ25.5.aDisability)
prep <- analysisPrep(employ_datQ25.5.aDisability$CatOutcome, employ_datQ25.5.aDisability$Disability, employ_datQ25.5.aDisability)
analysis <- polr(employ_datQ25.5.aDisability$CatOutcome ~ employ_datQ25.5.aDisability$Disability, data=employ_datQ25.5.aDisability, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ25.5.aEthnicity<-multidatClean(Q25.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aEthnicity$Ethnicity<- factor(employ_datQ25.5.aEthnicity$EthnicityCleaned)
employ_datQ25.5.aEthnicity<-ordinaldatClean(employ_datQ25.5.aEthnicity$Q25.5.a, employ_datQ25.5.aEthnicity)
conTable <- xtabs(~Q25.5.a + EthnicityCleaned, data = employ_datQ25.5.aEthnicity)
conTable
#ordinal(employ_datQ25.5.aEthnicity$CatOutcome, employ_datQ25.5.aEthnicity$EthnicityCleaned, employ_datQ25.5.aEthnicity)
prep <- analysisPrep(employ_datQ25.5.aEthnicity$CatOutcome, employ_datQ25.5.aEthnicity$EthnicityCleaned, employ_datQ25.5.aEthnicity)
analysis <- polr(employ_datQ25.5.aEthnicity$CatOutcome ~ employ_datQ25.5.aEthnicity$EthnicityCleaned, data=employ_datQ25.5.aEthnicity, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ25.5.aFirstGen<-multidatClean(Q25.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aFirstGen$FirstGen<-factor(employ_datQ25.5.aFirstGen$FirstGen)
employ_datQ25.5.aFirstGen<-ordinaldatClean(employ_datQ25.5.aFirstGen$Q25.5.a, employ_datQ25.5.aFirstGen)
#ordinal(employ_datQ25.5.aFirstGen$CatOutcome, employ_datQ25.5.aFirstGen$FirstGen, employ_datQ25.5.aFirstGen)
prep <- analysisPrep(employ_datQ25.5.aFirstGen$CatOutcome, employ_datQ25.5.aFirstGen$FirstGen, employ_datQ25.5.aFirstGen)
analysis <- polr(employ_datQ25.5.aFirstGen$CatOutcome ~ employ_datQ25.5.aFirstGen$FirstGen, data=employ_datQ25.5.aFirstGen, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ25.5.aGender<-multidatClean(Q25.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aGender$Gender<-factor(employ_datQ25.5.aGender$Gender)
employ_datQ25.5.aGender<-ordinaldatClean(employ_datQ25.5.aGender$Q25.5.a, employ_datQ25.5.aGender)
#ordinal(employ_datQ25.5.aGender$CatOutcome, employ_datQ25.5.aGender$Gender, employ_datQ25.5.aGender)
prep <- analysisPrep(employ_datQ25.5.aGender$CatOutcome, employ_datQ25.5.aGender$Gender, employ_datQ25.5.aGender)
analysis <- polr(employ_datQ25.5.aGender$CatOutcome ~ employ_datQ25.5.aGender$Gender, data=employ_datQ25.5.aGender, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ25.5.aSexuality<-multidatClean(Q25.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ25.5.aSexuality$Sexuality<-factor(employ_datQ25.5.aSexuality$Sexuality)
employ_datQ25.5.aSexuality<-ordinaldatClean(employ_datQ25.5.aSexuality$Q25.5.a, employ_datQ25.5.aSexuality)
#ordinal(employ_datQ25.5.aSexuality$CatOutcome, employ_datQ25.5.aSexuality$Sexuality, employ_datQ25.5.aSexuality)
prep <- analysisPrep(employ_datQ25.5.aSexuality$CatOutcome, employ_datQ25.5.aSexuality$Sexuality, employ_datQ25.5.aSexuality)
analysis <- polr(employ_datQ25.5.aSexuality$CatOutcome ~ employ_datQ25.5.aSexuality$Sexuality, data=employ_datQ25.5.aSexuality, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q25.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q25.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)






