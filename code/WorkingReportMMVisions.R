source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239,164:183, 6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)
### Q42
"Status"
employ_datQ42.1<-multidatClean(Q42.1, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.1 + Academic, data = employ_datQ42.1)
detach(dat_long)
employ_datQ42.1<-PriorityordinaldatClean(employ_datQ42.1$Q42.1, employ_datQ42.1)
#ordinal(employ_datQ42.1$CatOutcome, employ_datQ42.1$Academic, employ_datQ42.1)
prep <- analysisPrep(employ_datQ42.1$CatOutcome, employ_datQ42.1$Academic, employ_datQ42.1)
analysis <- polr(employ_datQ42.1$CatOutcome ~ employ_datQ42.1$Academic, data=employ_datQ42.1, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q42.1"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ42.1College<-multidatClean(Q42.1, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.1 + Q3, data = employ_datQ42.1College)
conTable
detach(dat_long)
employ_datQ42.1College$Q3[(employ_datQ42.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ42.1College$Q3<- factor(employ_datQ42.1College$Q3)
employ_datQ42.1College<-PriorityordinaldatClean(employ_datQ42.1College$Q42.1, employ_datQ42.1College)
#ordinal(employ_datQ42.1College$CatOutcome, employ_datQ42.1College$Q3, employ_datQ42.1College)
prep <- analysisPrep(employ_datQ42.1College$CatOutcome, employ_datQ42.1College$Q3, employ_datQ42.1College)
analysis <- polr(employ_datQ42.1College$CatOutcome ~ employ_datQ42.1College$Q3, data=employ_datQ42.1College, Hess=TRUE)
##assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ42.1Carer<-multidatClean(Q42.1, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1Carer$Carer<- factor(employ_datQ42.1Carer$Carer)
employ_datQ42.1Carer<-PriorityordinaldatClean(employ_datQ42.1Carer$Q42.1, employ_datQ42.1Carer)
#ordinal(employ_datQ42.1Carer$CatOutcome, employ_datQ42.1Carer$Carer, employ_datQ42.1Carer)
prep <- analysisPrep(employ_datQ42.1Carer$CatOutcome, employ_datQ42.1Carer$Carer, employ_datQ42.1Carer)
analysis <- (polr(employ_datQ42.1Carer$CatOutcome ~ employ_datQ42.1Carer$Carer, data=employ_datQ42.1Carer, Hess=TRUE))
#assumption <- brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ42.1Disability<-multidatClean(Q42.1, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1Disability$Disability<- factor(employ_datQ42.1Disability$Disability)
employ_datQ42.1Disability<-PriorityordinaldatClean(employ_datQ42.1Disability$Q42.1, employ_datQ42.1Disability)
conTable <- xtabs(~Q42.1 + Disability, data = employ_datQ42.1Disability)
#ordinal(employ_datQ42.1Disability$CatOutcome, employ_datQ42.1Disability$Disability, employ_datQ42.1Disability)
prep <- analysisPrep(employ_datQ42.1Disability$CatOutcome, employ_datQ42.1Disability$Disability, employ_datQ42.1Disability)
analysis <- polr(employ_datQ42.1Disability$CatOutcome ~ employ_datQ42.1Disability$Disability, data=employ_datQ42.1Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ42.1Ethnicity<-multidatClean(Q42.1, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1Ethnicity$Ethnicity<- factor(employ_datQ42.1Ethnicity$EthnicityCleaned)
employ_datQ42.1Ethnicity<-PriorityordinaldatClean(employ_datQ42.1Ethnicity$Q42.1, employ_datQ42.1Ethnicity)
conTable <- xtabs(~Q42.1 + EthnicityCleaned, data = employ_datQ42.1Ethnicity)
conTable
#ordinal(employ_datQ42.1Ethnicity$CatOutcome, employ_datQ42.1Ethnicity$EthnicityCleaned, employ_datQ42.1Ethnicity)
prep <- analysisPrep(employ_datQ42.1Ethnicity$CatOutcome, employ_datQ42.1Ethnicity$EthnicityCleaned, employ_datQ42.1Ethnicity)
analysis <- polr(employ_datQ42.1Ethnicity$CatOutcome ~ employ_datQ42.1Ethnicity$EthnicityCleaned, data=employ_datQ42.1Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ42.1FirstGen<-multidatClean(Q42.1, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1FirstGen$FirstGen<-factor(employ_datQ42.1FirstGen$FirstGen)
employ_datQ42.1FirstGen<-PriorityordinaldatClean(employ_datQ42.1FirstGen$Q42.1, employ_datQ42.1FirstGen)
#ordinal(employ_datQ42.1FirstGen$CatOutcome, employ_datQ42.1FirstGen$FirstGen, employ_datQ42.1FirstGen)
prep <- analysisPrep(employ_datQ42.1FirstGen$CatOutcome, employ_datQ42.1FirstGen$FirstGen, employ_datQ42.1FirstGen)
analysis <- polr(employ_datQ42.1FirstGen$CatOutcome ~ employ_datQ42.1FirstGen$FirstGen, data=employ_datQ42.1FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ42.1Gender<-multidatClean(Q42.1, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1Gender$Gender<-factor(employ_datQ42.1Gender$Gender)
employ_datQ42.1Gender<-PriorityordinaldatClean(employ_datQ42.1Gender$Q42.1, employ_datQ42.1Gender)
#ordinal(employ_datQ42.1Gender$CatOutcome, employ_datQ42.1Gender$Gender, employ_datQ42.1Gender)
prep <- analysisPrep(employ_datQ42.1Gender$CatOutcome, employ_datQ42.1Gender$Gender, employ_datQ42.1Gender)
analysis <- polr(employ_datQ42.1Gender$CatOutcome ~ employ_datQ42.1Gender$Gender, data=employ_datQ42.1Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ42.1Sexuality<-multidatClean(Q42.1, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.1Sexuality$Sexuality<-factor(employ_datQ42.1Sexuality$Sexuality)
employ_datQ42.1Sexuality<-PriorityordinaldatClean(employ_datQ42.1Sexuality$Q42.1, employ_datQ42.1Sexuality)
#ordinal(employ_datQ42.1Sexuality$CatOutcome, employ_datQ42.1Sexuality$Sexuality, employ_datQ42.1Sexuality)
prep <- analysisPrep(employ_datQ42.1Sexuality$CatOutcome, employ_datQ42.1Sexuality$Sexuality, employ_datQ42.1Sexuality)
analysis <- polr(employ_datQ42.1Sexuality$CatOutcome ~ employ_datQ42.1Sexuality$Sexuality, data=employ_datQ42.1Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.1"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q42.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q42
"Status"
employ_datQ42.2<-multidatClean(Q42.2, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.2 + Academic, data = employ_datQ42.2)
detach(dat_long)
employ_datQ42.2<-PriorityordinaldatClean(employ_datQ42.2$Q42.2, employ_datQ42.2)
#ordinal(employ_datQ42.2$CatOutcome, employ_datQ42.2$Academic, employ_datQ42.2)
prep <- analysisPrep(employ_datQ42.2$CatOutcome, employ_datQ42.2$Academic, employ_datQ42.2)
analysis <- polr(employ_datQ42.2$CatOutcome ~ employ_datQ42.2$Academic, data=employ_datQ42.2, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q42.2"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ42.2College<-multidatClean(Q42.2, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.2 + Q3, data = employ_datQ42.2College)
conTable
detach(dat_long)
employ_datQ42.2College$Q3[(employ_datQ42.2College$Q3 == "Research Professional Staff")]="Other"
employ_datQ42.2College$Q3<- factor(employ_datQ42.2College$Q3)
employ_datQ42.2College<-PriorityordinaldatClean(employ_datQ42.2College$Q42.2, employ_datQ42.2College)
#ordinal(employ_datQ42.2College$CatOutcome, employ_datQ42.2College$Q3, employ_datQ42.2College)
prep <- analysisPrep(employ_datQ42.2College$CatOutcome, employ_datQ42.2College$Q3, employ_datQ42.2College)
analysis <- polr(employ_datQ42.2College$CatOutcome ~ employ_datQ42.2College$Q3, data=employ_datQ42.2College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ42.2Carer<-multidatClean(Q42.2, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2Carer$Carer<- factor(employ_datQ42.2Carer$Carer)
employ_datQ42.2Carer<-PriorityordinaldatClean(employ_datQ42.2Carer$Q42.2, employ_datQ42.2Carer)
#ordinal(employ_datQ42.2Carer$CatOutcome, employ_datQ42.2Carer$Carer, employ_datQ42.2Carer)
prep <- analysisPrep(employ_datQ42.2Carer$CatOutcome, employ_datQ42.2Carer$Carer, employ_datQ42.2Carer)
analysis <- polr(employ_datQ42.2Carer$CatOutcome ~ employ_datQ42.2Carer$Carer, data=employ_datQ42.2Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ42.2Disability<-multidatClean(Q42.2, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2Disability$Disability<- factor(employ_datQ42.2Disability$Disability)
employ_datQ42.2Disability<-PriorityordinaldatClean(employ_datQ42.2Disability$Q42.2, employ_datQ42.2Disability)
conTable <- xtabs(~Q42.2 + Disability, data = employ_datQ42.2Disability)
#ordinal(employ_datQ42.2Disability$CatOutcome, employ_datQ42.2Disability$Disability, employ_datQ42.2Disability)
prep <- analysisPrep(employ_datQ42.2Disability$CatOutcome, employ_datQ42.2Disability$Disability, employ_datQ42.2Disability)
analysis <- polr(employ_datQ42.2Disability$CatOutcome ~ employ_datQ42.2Disability$Disability, data=employ_datQ42.2Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ42.2Ethnicity<-multidatClean(Q42.2, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2Ethnicity$Ethnicity<- factor(employ_datQ42.2Ethnicity$EthnicityCleaned)
employ_datQ42.2Ethnicity<-PriorityordinaldatClean(employ_datQ42.2Ethnicity$Q42.2, employ_datQ42.2Ethnicity)
conTable <- xtabs(~Q42.2 + EthnicityCleaned, data = employ_datQ42.2Ethnicity)
conTable
#ordinal(employ_datQ42.2Ethnicity$CatOutcome, employ_datQ42.2Ethnicity$EthnicityCleaned, employ_datQ42.2Ethnicity)
prep <- analysisPrep(employ_datQ42.2Ethnicity$CatOutcome, employ_datQ42.2Ethnicity$EthnicityCleaned, employ_datQ42.2Ethnicity)
analysis <- polr(employ_datQ42.2Ethnicity$CatOutcome ~ employ_datQ42.2Ethnicity$EthnicityCleaned, data=employ_datQ42.2Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ42.2FirstGen<-multidatClean(Q42.2, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2FirstGen$FirstGen<-factor(employ_datQ42.2FirstGen$FirstGen)
employ_datQ42.2FirstGen<-PriorityordinaldatClean(employ_datQ42.2FirstGen$Q42.2, employ_datQ42.2FirstGen)
#ordinal(employ_datQ42.2FirstGen$CatOutcome, employ_datQ42.2FirstGen$FirstGen, employ_datQ42.2FirstGen)
prep <- analysisPrep(employ_datQ42.2FirstGen$CatOutcome, employ_datQ42.2FirstGen$FirstGen, employ_datQ42.2FirstGen)
analysis <- polr(employ_datQ42.2FirstGen$CatOutcome ~ employ_datQ42.2FirstGen$FirstGen, data=employ_datQ42.2FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ42.2Gender<-multidatClean(Q42.2, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2Gender$Gender<-factor(employ_datQ42.2Gender$Gender)
employ_datQ42.2Gender<-PriorityordinaldatClean(employ_datQ42.2Gender$Q42.2, employ_datQ42.2Gender)
#ordinal(employ_datQ42.2Gender$CatOutcome, employ_datQ42.2Gender$Gender, employ_datQ42.2Gender)
prep <- analysisPrep(employ_datQ42.2Gender$CatOutcome, employ_datQ42.2Gender$Gender, employ_datQ42.2Gender)
analysis <- polr(employ_datQ42.2Gender$CatOutcome ~ employ_datQ42.2Gender$Gender, data=employ_datQ42.2Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ42.2Sexuality<-multidatClean(Q42.2, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.2Sexuality$Sexuality<-factor(employ_datQ42.2Sexuality$Sexuality)
employ_datQ42.2Sexuality<-PriorityordinaldatClean(employ_datQ42.2Sexuality$Q42.2, employ_datQ42.2Sexuality)
#ordinal(employ_datQ42.2Sexuality$CatOutcome, employ_datQ42.2Sexuality$Sexuality, employ_datQ42.2Sexuality)
prep <- analysisPrep(employ_datQ42.2Sexuality$CatOutcome, employ_datQ42.2Sexuality$Sexuality, employ_datQ42.2Sexuality)
analysis <- polr(employ_datQ42.2Sexuality$CatOutcome ~ employ_datQ42.2Sexuality$Sexuality, data=employ_datQ42.2Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.2"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q42.2_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q42

"Status"
employ_datQ42.3<-multidatClean(Q42.3, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.3 + Academic, data = employ_datQ42.3)
detach(dat_long)
employ_datQ42.3<-PriorityordinaldatClean(employ_datQ42.3$Q42.3, employ_datQ42.3)
#ordinal(employ_datQ42.3$CatOutcome, employ_datQ42.3$Academic, employ_datQ42.3)
prep <- analysisPrep(employ_datQ42.3$CatOutcome, employ_datQ42.3$Academic, employ_datQ42.3)
analysis <- polr(employ_datQ42.3$CatOutcome ~ employ_datQ42.3$Academic, data=employ_datQ42.3, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q42.3"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ42.3College<-multidatClean(Q42.3, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q42.3 + Q3, data = employ_datQ42.3College)
conTable
detach(dat_long)
employ_datQ42.3College$Q3[(employ_datQ42.3College$Q3 == "Research Professional Staff")]="Other"
employ_datQ42.3College$Q3<- factor(employ_datQ42.3College$Q3)
employ_datQ42.3College<-PriorityordinaldatClean(employ_datQ42.3College$Q42.3, employ_datQ42.3College)
#ordinal(employ_datQ42.3College$CatOutcome, employ_datQ42.3College$Q3, employ_datQ42.3College)
prep <- analysisPrep(employ_datQ42.3College$CatOutcome, employ_datQ42.3College$Q3, employ_datQ42.3College)
analysis <- polr(employ_datQ42.3College$CatOutcome ~ employ_datQ42.3College$Q3, data=employ_datQ42.3College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ42.3Carer<-multidatClean(Q42.3, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3Carer$Carer<- factor(employ_datQ42.3Carer$Carer)
employ_datQ42.3Carer<-PriorityordinaldatClean(employ_datQ42.3Carer$Q42.3, employ_datQ42.3Carer)
#ordinal(employ_datQ42.3Carer$CatOutcome, employ_datQ42.3Carer$Carer, employ_datQ42.3Carer)
prep <- analysisPrep(employ_datQ42.3Carer$CatOutcome, employ_datQ42.3Carer$Carer, employ_datQ42.3Carer)
analysis <- polr(employ_datQ42.3Carer$CatOutcome ~ employ_datQ42.3Carer$Carer, data=employ_datQ42.3Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ42.3Disability<-multidatClean(Q42.3, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3Disability$Disability<- factor(employ_datQ42.3Disability$Disability)
employ_datQ42.3Disability<-PriorityordinaldatClean(employ_datQ42.3Disability$Q42.3, employ_datQ42.3Disability)
conTable <- xtabs(~Q42.3 + Disability, data = employ_datQ42.3Disability)
#ordinal(employ_datQ42.3Disability$CatOutcome, employ_datQ42.3Disability$Disability, employ_datQ42.3Disability)
prep <- analysisPrep(employ_datQ42.3Disability$CatOutcome, employ_datQ42.3Disability$Disability, employ_datQ42.3Disability)
analysis <- polr(employ_datQ42.3Disability$CatOutcome ~ employ_datQ42.3Disability$Disability, data=employ_datQ42.3Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ42.3Ethnicity<-multidatClean(Q42.3, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3Ethnicity$Ethnicity<- factor(employ_datQ42.3Ethnicity$EthnicityCleaned)
employ_datQ42.3Ethnicity<-PriorityordinaldatClean(employ_datQ42.3Ethnicity$Q42.3, employ_datQ42.3Ethnicity)
conTable <- xtabs(~Q42.3 + EthnicityCleaned, data = employ_datQ42.3Ethnicity)
conTable
#ordinal(employ_datQ42.3Ethnicity$CatOutcome, employ_datQ42.3Ethnicity$EthnicityCleaned, employ_datQ42.3Ethnicity)
prep <- analysisPrep(employ_datQ42.3Ethnicity$CatOutcome, employ_datQ42.3Ethnicity$EthnicityCleaned, employ_datQ42.3Ethnicity)
analysis <- polr(employ_datQ42.3Ethnicity$CatOutcome ~ employ_datQ42.3Ethnicity$EthnicityCleaned, data=employ_datQ42.3Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ42.3FirstGen<-multidatClean(Q42.3, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3FirstGen$FirstGen<-factor(employ_datQ42.3FirstGen$FirstGen)
employ_datQ42.3FirstGen<-PriorityordinaldatClean(employ_datQ42.3FirstGen$Q42.3, employ_datQ42.3FirstGen)
#ordinal(employ_datQ42.3FirstGen$CatOutcome, employ_datQ42.3FirstGen$FirstGen, employ_datQ42.3FirstGen)
prep <- analysisPrep(employ_datQ42.3FirstGen$CatOutcome, employ_datQ42.3FirstGen$FirstGen, employ_datQ42.3FirstGen)
analysis <- polr(employ_datQ42.3FirstGen$CatOutcome ~ employ_datQ42.3FirstGen$FirstGen, data=employ_datQ42.3FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ42.3Gender<-multidatClean(Q42.3, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3Gender$Gender<-factor(employ_datQ42.3Gender$Gender)
employ_datQ42.3Gender<-PriorityordinaldatClean(employ_datQ42.3Gender$Q42.3, employ_datQ42.3Gender)
#ordinal(employ_datQ42.3Gender$CatOutcome, employ_datQ42.3Gender$Gender, employ_datQ42.3Gender)
prep <- analysisPrep(employ_datQ42.3Gender$CatOutcome, employ_datQ42.3Gender$Gender, employ_datQ42.3Gender)
analysis <- polr(employ_datQ42.3Gender$CatOutcome ~ employ_datQ42.3Gender$Gender, data=employ_datQ42.3Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ42.3Sexuality<-multidatClean(Q42.3, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ42.3Sexuality$Sexuality<-factor(employ_datQ42.3Sexuality$Sexuality)
employ_datQ42.3Sexuality<-PriorityordinaldatClean(employ_datQ42.3Sexuality$Q42.3, employ_datQ42.3Sexuality)
#ordinal(employ_datQ42.3Sexuality$CatOutcome, employ_datQ42.3Sexuality$Sexuality, employ_datQ42.3Sexuality)
prep <- analysisPrep(employ_datQ42.3Sexuality$CatOutcome, employ_datQ42.3Sexuality$Sexuality, employ_datQ42.3Sexuality)
analysis <- polr(employ_datQ42.3Sexuality$CatOutcome ~ employ_datQ42.3Sexuality$Sexuality, data=employ_datQ42.3Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q42.3"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q42.3_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.1.a<-multidatClean(Q43.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.1.a + Academic, data = employ_datQ43.1.a)
detach(dat_long)
employ_datQ43.1.a<-ImpactordinaldatClean(employ_datQ43.1.a$Q43.1.a, employ_datQ43.1.a)
#ordinal(employ_datQ43.1.a$CatOutcome, employ_datQ43.1.a$Academic, employ_datQ43.1.a)
prep <- analysisPrep(employ_datQ43.1.a$CatOutcome, employ_datQ43.1.a$Academic, employ_datQ43.1.a)
analysis <- polr(employ_datQ43.1.a$CatOutcome ~ employ_datQ43.1.a$Academic, data=employ_datQ43.1.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.1.aCollege<-multidatClean(Q43.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.1.a + Q3, data = employ_datQ43.1.aCollege)
conTable
detach(dat_long)
employ_datQ43.1.aCollege$Q3[(employ_datQ43.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.1.aCollege$Q3<- factor(employ_datQ43.1.aCollege$Q3)
employ_datQ43.1.aCollege<-ImpactordinaldatClean(employ_datQ43.1.aCollege$Q43.1.a, employ_datQ43.1.aCollege)
#ordinal(employ_datQ43.1.aCollege$CatOutcome, employ_datQ43.1.aCollege$Q3, employ_datQ43.1.aCollege)
prep <- analysisPrep(employ_datQ43.1.aCollege$CatOutcome, employ_datQ43.1.aCollege$Q3, employ_datQ43.1.aCollege)
analysis <- polr(employ_datQ43.1.aCollege$CatOutcome ~ employ_datQ43.1.aCollege$Q3, data=employ_datQ43.1.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.1.aCarer<-multidatClean(Q43.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aCarer$Carer<- factor(employ_datQ43.1.aCarer$Carer)
employ_datQ43.1.aCarer<-ImpactordinaldatClean(employ_datQ43.1.aCarer$Q43.1.a, employ_datQ43.1.aCarer)
#ordinal(employ_datQ43.1.aCarer$CatOutcome, employ_datQ43.1.aCarer$Carer, employ_datQ43.1.aCarer)
prep <- analysisPrep(employ_datQ43.1.aCarer$CatOutcome, employ_datQ43.1.aCarer$Carer, employ_datQ43.1.aCarer)
analysis <- polr(employ_datQ43.1.aCarer$CatOutcome ~ employ_datQ43.1.aCarer$Carer, data=employ_datQ43.1.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.1.aDisability<-multidatClean(Q43.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aDisability$Disability<- factor(employ_datQ43.1.aDisability$Disability)
employ_datQ43.1.aDisability<-ImpactordinaldatClean(employ_datQ43.1.aDisability$Q43.1.a, employ_datQ43.1.aDisability)
conTable <- xtabs(~Q43.1.a + Disability, data = employ_datQ43.1.aDisability)
#ordinal(employ_datQ43.1.aDisability$CatOutcome, employ_datQ43.1.aDisability$Disability, employ_datQ43.1.aDisability)
prep <- analysisPrep(employ_datQ43.1.aDisability$CatOutcome, employ_datQ43.1.aDisability$Disability, employ_datQ43.1.aDisability)
analysis <- polr(employ_datQ43.1.aDisability$CatOutcome ~ employ_datQ43.1.aDisability$Disability, data=employ_datQ43.1.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.1.aEthnicity<-multidatClean(Q43.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aEthnicity$Ethnicity<- factor(employ_datQ43.1.aEthnicity$EthnicityCleaned)
employ_datQ43.1.aEthnicity<-ImpactordinaldatClean(employ_datQ43.1.aEthnicity$Q43.1.a, employ_datQ43.1.aEthnicity)
conTable <- xtabs(~Q43.1.a + EthnicityCleaned, data = employ_datQ43.1.aEthnicity)
conTable
#ordinal(employ_datQ43.1.aEthnicity$CatOutcome, employ_datQ43.1.aEthnicity$EthnicityCleaned, employ_datQ43.1.aEthnicity)
prep <- analysisPrep(employ_datQ43.1.aEthnicity$CatOutcome, employ_datQ43.1.aEthnicity$EthnicityCleaned, employ_datQ43.1.aEthnicity)
analysis <- polr(employ_datQ43.1.aEthnicity$CatOutcome ~ employ_datQ43.1.aEthnicity$EthnicityCleaned, data=employ_datQ43.1.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.1.aFirstGen<-multidatClean(Q43.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aFirstGen$FirstGen<-factor(employ_datQ43.1.aFirstGen$FirstGen)
employ_datQ43.1.aFirstGen<-ImpactordinaldatClean(employ_datQ43.1.aFirstGen$Q43.1.a, employ_datQ43.1.aFirstGen)
#ordinal(employ_datQ43.1.aFirstGen$CatOutcome, employ_datQ43.1.aFirstGen$FirstGen, employ_datQ43.1.aFirstGen)
prep <- analysisPrep(employ_datQ43.1.aFirstGen$CatOutcome, employ_datQ43.1.aFirstGen$FirstGen, employ_datQ43.1.aFirstGen)
analysis <- polr(employ_datQ43.1.aFirstGen$CatOutcome ~ employ_datQ43.1.aFirstGen$FirstGen, data=employ_datQ43.1.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.1.aGender<-multidatClean(Q43.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aGender$Gender<-factor(employ_datQ43.1.aGender$Gender)
employ_datQ43.1.aGender<-ImpactordinaldatClean(employ_datQ43.1.aGender$Q43.1.a, employ_datQ43.1.aGender)
#ordinal(employ_datQ43.1.aGender$CatOutcome, employ_datQ43.1.aGender$Gender, employ_datQ43.1.aGender)
prep <- analysisPrep(employ_datQ43.1.aGender$CatOutcome, employ_datQ43.1.aGender$Gender, employ_datQ43.1.aGender)
analysis <- polr(employ_datQ43.1.aGender$CatOutcome ~ employ_datQ43.1.aGender$Gender, data=employ_datQ43.1.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.1.aSexuality<-multidatClean(Q43.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.1.aSexuality$Sexuality<-factor(employ_datQ43.1.aSexuality$Sexuality)
employ_datQ43.1.aSexuality<-ImpactordinaldatClean(employ_datQ43.1.aSexuality$Q43.1.a, employ_datQ43.1.aSexuality)
#ordinal(employ_datQ43.1.aSexuality$CatOutcome, employ_datQ43.1.aSexuality$Sexuality, employ_datQ43.1.aSexuality)
prep <- analysisPrep(employ_datQ43.1.aSexuality$CatOutcome, employ_datQ43.1.aSexuality$Sexuality, employ_datQ43.1.aSexuality)
analysis <- polr(employ_datQ43.1.aSexuality$CatOutcome ~ employ_datQ43.1.aSexuality$Sexuality, data=employ_datQ43.1.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.2.a<-multidatClean(Q43.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.2.a + Academic, data = employ_datQ43.2.a)
detach(dat_long)
employ_datQ43.2.a<-ImpactordinaldatClean(employ_datQ43.2.a$Q43.2.a, employ_datQ43.2.a)
#ordinal(employ_datQ43.2.a$CatOutcome, employ_datQ43.2.a$Academic, employ_datQ43.2.a)
prep <- analysisPrep(employ_datQ43.2.a$CatOutcome, employ_datQ43.2.a$Academic, employ_datQ43.2.a)
analysis <- polr(employ_datQ43.2.a$CatOutcome ~ employ_datQ43.2.a$Academic, data=employ_datQ43.2.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.2.aCollege<-multidatClean(Q43.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.2.a + Q3, data = employ_datQ43.2.aCollege)
conTable
detach(dat_long)
employ_datQ43.2.aCollege$Q3[(employ_datQ43.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.2.aCollege$Q3<- factor(employ_datQ43.2.aCollege$Q3)
employ_datQ43.2.aCollege<-ImpactordinaldatClean(employ_datQ43.2.aCollege$Q43.2.a, employ_datQ43.2.aCollege)
#ordinal(employ_datQ43.2.aCollege$CatOutcome, employ_datQ43.2.aCollege$Q3, employ_datQ43.2.aCollege)
prep <- analysisPrep(employ_datQ43.2.aCollege$CatOutcome, employ_datQ43.2.aCollege$Q3, employ_datQ43.2.aCollege)
analysis <- polr(employ_datQ43.2.aCollege$CatOutcome ~ employ_datQ43.2.aCollege$Q3, data=employ_datQ43.2.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.2.aCarer<-multidatClean(Q43.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aCarer$Carer<- factor(employ_datQ43.2.aCarer$Carer)
employ_datQ43.2.aCarer<-ImpactordinaldatClean(employ_datQ43.2.aCarer$Q43.2.a, employ_datQ43.2.aCarer)
#ordinal(employ_datQ43.2.aCarer$CatOutcome, employ_datQ43.2.aCarer$Carer, employ_datQ43.2.aCarer)
prep <- analysisPrep(employ_datQ43.2.aCarer$CatOutcome, employ_datQ43.2.aCarer$Carer, employ_datQ43.2.aCarer)
analysis <- polr(employ_datQ43.2.aCarer$CatOutcome ~ employ_datQ43.2.aCarer$Carer, data=employ_datQ43.2.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.2.aDisability<-multidatClean(Q43.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aDisability$Disability<- factor(employ_datQ43.2.aDisability$Disability)
employ_datQ43.2.aDisability<-ImpactordinaldatClean(employ_datQ43.2.aDisability$Q43.2.a, employ_datQ43.2.aDisability)
conTable <- xtabs(~Q43.2.a + Disability, data = employ_datQ43.2.aDisability)
#ordinal(employ_datQ43.2.aDisability$CatOutcome, employ_datQ43.2.aDisability$Disability, employ_datQ43.2.aDisability)
prep <- analysisPrep(employ_datQ43.2.aDisability$CatOutcome, employ_datQ43.2.aDisability$Disability, employ_datQ43.2.aDisability)
analysis <- polr(employ_datQ43.2.aDisability$CatOutcome ~ employ_datQ43.2.aDisability$Disability, data=employ_datQ43.2.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.2.aEthnicity<-multidatClean(Q43.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aEthnicity$Ethnicity<- factor(employ_datQ43.2.aEthnicity$EthnicityCleaned)
employ_datQ43.2.aEthnicity<-ImpactordinaldatClean(employ_datQ43.2.aEthnicity$Q43.2.a, employ_datQ43.2.aEthnicity)
conTable <- xtabs(~Q43.2.a + EthnicityCleaned, data = employ_datQ43.2.aEthnicity)
conTable
#ordinal(employ_datQ43.2.aEthnicity$CatOutcome, employ_datQ43.2.aEthnicity$EthnicityCleaned, employ_datQ43.2.aEthnicity)
prep <- analysisPrep(employ_datQ43.2.aEthnicity$CatOutcome, employ_datQ43.2.aEthnicity$EthnicityCleaned, employ_datQ43.2.aEthnicity)
analysis <- polr(employ_datQ43.2.aEthnicity$CatOutcome ~ employ_datQ43.2.aEthnicity$EthnicityCleaned, data=employ_datQ43.2.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.2.aFirstGen<-multidatClean(Q43.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aFirstGen$FirstGen<-factor(employ_datQ43.2.aFirstGen$FirstGen)
employ_datQ43.2.aFirstGen<-ImpactordinaldatClean(employ_datQ43.2.aFirstGen$Q43.2.a, employ_datQ43.2.aFirstGen)
#ordinal(employ_datQ43.2.aFirstGen$CatOutcome, employ_datQ43.2.aFirstGen$FirstGen, employ_datQ43.2.aFirstGen)
prep <- analysisPrep(employ_datQ43.2.aFirstGen$CatOutcome, employ_datQ43.2.aFirstGen$FirstGen, employ_datQ43.2.aFirstGen)
analysis <- polr(employ_datQ43.2.aFirstGen$CatOutcome ~ employ_datQ43.2.aFirstGen$FirstGen, data=employ_datQ43.2.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.2.aGender<-multidatClean(Q43.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aGender$Gender<-factor(employ_datQ43.2.aGender$Gender)
employ_datQ43.2.aGender<-ImpactordinaldatClean(employ_datQ43.2.aGender$Q43.2.a, employ_datQ43.2.aGender)
#ordinal(employ_datQ43.2.aGender$CatOutcome, employ_datQ43.2.aGender$Gender, employ_datQ43.2.aGender)
prep <- analysisPrep(employ_datQ43.2.aGender$CatOutcome, employ_datQ43.2.aGender$Gender, employ_datQ43.2.aGender)
analysis <- polr(employ_datQ43.2.aGender$CatOutcome ~ employ_datQ43.2.aGender$Gender, data=employ_datQ43.2.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.2.aSexuality<-multidatClean(Q43.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.2.aSexuality$Sexuality<-factor(employ_datQ43.2.aSexuality$Sexuality)
employ_datQ43.2.aSexuality<-ImpactordinaldatClean(employ_datQ43.2.aSexuality$Q43.2.a, employ_datQ43.2.aSexuality)
#ordinal(employ_datQ43.2.aSexuality$CatOutcome, employ_datQ43.2.aSexuality$Sexuality, employ_datQ43.2.aSexuality)
prep <- analysisPrep(employ_datQ43.2.aSexuality$CatOutcome, employ_datQ43.2.aSexuality$Sexuality, employ_datQ43.2.aSexuality)
analysis <- polr(employ_datQ43.2.aSexuality$CatOutcome ~ employ_datQ43.2.aSexuality$Sexuality, data=employ_datQ43.2.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.3.a<-multidatClean(Q43.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.3.a + Academic, data = employ_datQ43.3.a)
detach(dat_long)
employ_datQ43.3.a<-ImpactordinaldatClean(employ_datQ43.3.a$Q43.3.a, employ_datQ43.3.a)
#ordinal(employ_datQ43.3.a$CatOutcome, employ_datQ43.3.a$Academic, employ_datQ43.3.a)
prep <- analysisPrep(employ_datQ43.3.a$CatOutcome, employ_datQ43.3.a$Academic, employ_datQ43.3.a)
analysis <- polr(employ_datQ43.3.a$CatOutcome ~ employ_datQ43.3.a$Academic, data=employ_datQ43.3.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.3.aCollege<-multidatClean(Q43.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.3.a + Q3, data = employ_datQ43.3.aCollege)
conTable
detach(dat_long)
employ_datQ43.3.aCollege$Q3[(employ_datQ43.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.3.aCollege$Q3<- factor(employ_datQ43.3.aCollege$Q3)
employ_datQ43.3.aCollege<-ImpactordinaldatClean(employ_datQ43.3.aCollege$Q43.3.a, employ_datQ43.3.aCollege)
#ordinal(employ_datQ43.3.aCollege$CatOutcome, employ_datQ43.3.aCollege$Q3, employ_datQ43.3.aCollege)
prep <- analysisPrep(employ_datQ43.3.aCollege$CatOutcome, employ_datQ43.3.aCollege$Q3, employ_datQ43.3.aCollege)
analysis <- polr(employ_datQ43.3.aCollege$CatOutcome ~ employ_datQ43.3.aCollege$Q3, data=employ_datQ43.3.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.3.aCarer<-multidatClean(Q43.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aCarer$Carer<- factor(employ_datQ43.3.aCarer$Carer)
employ_datQ43.3.aCarer<-ImpactordinaldatClean(employ_datQ43.3.aCarer$Q43.3.a, employ_datQ43.3.aCarer)
#ordinal(employ_datQ43.3.aCarer$CatOutcome, employ_datQ43.3.aCarer$Carer, employ_datQ43.3.aCarer)
prep <- analysisPrep(employ_datQ43.3.aCarer$CatOutcome, employ_datQ43.3.aCarer$Carer, employ_datQ43.3.aCarer)
analysis <- polr(employ_datQ43.3.aCarer$CatOutcome ~ employ_datQ43.3.aCarer$Carer, data=employ_datQ43.3.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.3.aDisability<-multidatClean(Q43.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aDisability$Disability<- factor(employ_datQ43.3.aDisability$Disability)
employ_datQ43.3.aDisability<-ImpactordinaldatClean(employ_datQ43.3.aDisability$Q43.3.a, employ_datQ43.3.aDisability)
conTable <- xtabs(~Q43.3.a + Disability, data = employ_datQ43.3.aDisability)
#ordinal(employ_datQ43.3.aDisability$CatOutcome, employ_datQ43.3.aDisability$Disability, employ_datQ43.3.aDisability)
prep <- analysisPrep(employ_datQ43.3.aDisability$CatOutcome, employ_datQ43.3.aDisability$Disability, employ_datQ43.3.aDisability)
analysis <- polr(employ_datQ43.3.aDisability$CatOutcome ~ employ_datQ43.3.aDisability$Disability, data=employ_datQ43.3.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.3.aEthnicity<-multidatClean(Q43.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aEthnicity$Ethnicity<- factor(employ_datQ43.3.aEthnicity$EthnicityCleaned)
employ_datQ43.3.aEthnicity<-ImpactordinaldatClean(employ_datQ43.3.aEthnicity$Q43.3.a, employ_datQ43.3.aEthnicity)
conTable <- xtabs(~Q43.3.a + EthnicityCleaned, data = employ_datQ43.3.aEthnicity)
conTable
#ordinal(employ_datQ43.3.aEthnicity$CatOutcome, employ_datQ43.3.aEthnicity$EthnicityCleaned, employ_datQ43.3.aEthnicity)
prep <- analysisPrep(employ_datQ43.3.aEthnicity$CatOutcome, employ_datQ43.3.aEthnicity$EthnicityCleaned, employ_datQ43.3.aEthnicity)
analysis <- polr(employ_datQ43.3.aEthnicity$CatOutcome ~ employ_datQ43.3.aEthnicity$EthnicityCleaned, data=employ_datQ43.3.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.3.aFirstGen<-multidatClean(Q43.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aFirstGen$FirstGen<-factor(employ_datQ43.3.aFirstGen$FirstGen)
employ_datQ43.3.aFirstGen<-ImpactordinaldatClean(employ_datQ43.3.aFirstGen$Q43.3.a, employ_datQ43.3.aFirstGen)
#ordinal(employ_datQ43.3.aFirstGen$CatOutcome, employ_datQ43.3.aFirstGen$FirstGen, employ_datQ43.3.aFirstGen)
prep <- analysisPrep(employ_datQ43.3.aFirstGen$CatOutcome, employ_datQ43.3.aFirstGen$FirstGen, employ_datQ43.3.aFirstGen)
analysis <- polr(employ_datQ43.3.aFirstGen$CatOutcome ~ employ_datQ43.3.aFirstGen$FirstGen, data=employ_datQ43.3.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.3.aGender<-multidatClean(Q43.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aGender$Gender<-factor(employ_datQ43.3.aGender$Gender)
employ_datQ43.3.aGender<-ImpactordinaldatClean(employ_datQ43.3.aGender$Q43.3.a, employ_datQ43.3.aGender)
#ordinal(employ_datQ43.3.aGender$CatOutcome, employ_datQ43.3.aGender$Gender, employ_datQ43.3.aGender)
prep <- analysisPrep(employ_datQ43.3.aGender$CatOutcome, employ_datQ43.3.aGender$Gender, employ_datQ43.3.aGender)
analysis <- polr(employ_datQ43.3.aGender$CatOutcome ~ employ_datQ43.3.aGender$Gender, data=employ_datQ43.3.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.3.aSexuality<-multidatClean(Q43.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.3.aSexuality$Sexuality<-factor(employ_datQ43.3.aSexuality$Sexuality)
employ_datQ43.3.aSexuality<-ImpactordinaldatClean(employ_datQ43.3.aSexuality$Q43.3.a, employ_datQ43.3.aSexuality)
#ordinal(employ_datQ43.3.aSexuality$CatOutcome, employ_datQ43.3.aSexuality$Sexuality, employ_datQ43.3.aSexuality)
prep <- analysisPrep(employ_datQ43.3.aSexuality$CatOutcome, employ_datQ43.3.aSexuality$Sexuality, employ_datQ43.3.aSexuality)
analysis <- polr(employ_datQ43.3.aSexuality$CatOutcome ~ employ_datQ43.3.aSexuality$Sexuality, data=employ_datQ43.3.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.4.a<-multidatClean(Q43.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.4.a + Academic, data = employ_datQ43.4.a)
detach(dat_long)
employ_datQ43.4.a<-ImpactordinaldatClean(employ_datQ43.4.a$Q43.4.a, employ_datQ43.4.a)
#ordinal(employ_datQ43.4.a$CatOutcome, employ_datQ43.4.a$Academic, employ_datQ43.4.a)
prep <- analysisPrep(employ_datQ43.4.a$CatOutcome, employ_datQ43.4.a$Academic, employ_datQ43.4.a)
analysis <- polr(employ_datQ43.4.a$CatOutcome ~ employ_datQ43.4.a$Academic, data=employ_datQ43.4.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.4.aCollege<-multidatClean(Q43.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.4.a + Q3, data = employ_datQ43.4.aCollege)
conTable
detach(dat_long)
employ_datQ43.4.aCollege$Q3[(employ_datQ43.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.4.aCollege$Q3<- factor(employ_datQ43.4.aCollege$Q3)
employ_datQ43.4.aCollege<-ImpactordinaldatClean(employ_datQ43.4.aCollege$Q43.4.a, employ_datQ43.4.aCollege)
#ordinal(employ_datQ43.4.aCollege$CatOutcome, employ_datQ43.4.aCollege$Q3, employ_datQ43.4.aCollege)
prep <- analysisPrep(employ_datQ43.4.aCollege$CatOutcome, employ_datQ43.4.aCollege$Q3, employ_datQ43.4.aCollege)
analysis <- polr(employ_datQ43.4.aCollege$CatOutcome ~ employ_datQ43.4.aCollege$Q3, data=employ_datQ43.4.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.4.aCarer<-multidatClean(Q43.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aCarer$Carer<- factor(employ_datQ43.4.aCarer$Carer)
employ_datQ43.4.aCarer<-ImpactordinaldatClean(employ_datQ43.4.aCarer$Q43.4.a, employ_datQ43.4.aCarer)
#ordinal(employ_datQ43.4.aCarer$CatOutcome, employ_datQ43.4.aCarer$Carer, employ_datQ43.4.aCarer)
prep <- analysisPrep(employ_datQ43.4.aCarer$CatOutcome, employ_datQ43.4.aCarer$Carer, employ_datQ43.4.aCarer)
analysis <- polr(employ_datQ43.4.aCarer$CatOutcome ~ employ_datQ43.4.aCarer$Carer, data=employ_datQ43.4.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.4.aDisability<-multidatClean(Q43.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aDisability$Disability<- factor(employ_datQ43.4.aDisability$Disability)
employ_datQ43.4.aDisability<-ImpactordinaldatClean(employ_datQ43.4.aDisability$Q43.4.a, employ_datQ43.4.aDisability)
conTable <- xtabs(~Q43.4.a + Disability, data = employ_datQ43.4.aDisability)
#ordinal(employ_datQ43.4.aDisability$CatOutcome, employ_datQ43.4.aDisability$Disability, employ_datQ43.4.aDisability)
prep <- analysisPrep(employ_datQ43.4.aDisability$CatOutcome, employ_datQ43.4.aDisability$Disability, employ_datQ43.4.aDisability)
analysis <- polr(employ_datQ43.4.aDisability$CatOutcome ~ employ_datQ43.4.aDisability$Disability, data=employ_datQ43.4.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.4.aEthnicity<-multidatClean(Q43.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aEthnicity$Ethnicity<- factor(employ_datQ43.4.aEthnicity$EthnicityCleaned)
employ_datQ43.4.aEthnicity<-ImpactordinaldatClean(employ_datQ43.4.aEthnicity$Q43.4.a, employ_datQ43.4.aEthnicity)
conTable <- xtabs(~Q43.4.a + EthnicityCleaned, data = employ_datQ43.4.aEthnicity)
conTable
#ordinal(employ_datQ43.4.aEthnicity$CatOutcome, employ_datQ43.4.aEthnicity$EthnicityCleaned, employ_datQ43.4.aEthnicity)
prep <- analysisPrep(employ_datQ43.4.aEthnicity$CatOutcome, employ_datQ43.4.aEthnicity$EthnicityCleaned, employ_datQ43.4.aEthnicity)
analysis <- polr(employ_datQ43.4.aEthnicity$CatOutcome ~ employ_datQ43.4.aEthnicity$EthnicityCleaned, data=employ_datQ43.4.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.4.aFirstGen<-multidatClean(Q43.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aFirstGen$FirstGen<-factor(employ_datQ43.4.aFirstGen$FirstGen)
employ_datQ43.4.aFirstGen<-ImpactordinaldatClean(employ_datQ43.4.aFirstGen$Q43.4.a, employ_datQ43.4.aFirstGen)
#ordinal(employ_datQ43.4.aFirstGen$CatOutcome, employ_datQ43.4.aFirstGen$FirstGen, employ_datQ43.4.aFirstGen)
prep <- analysisPrep(employ_datQ43.4.aFirstGen$CatOutcome, employ_datQ43.4.aFirstGen$FirstGen, employ_datQ43.4.aFirstGen)
analysis <- polr(employ_datQ43.4.aFirstGen$CatOutcome ~ employ_datQ43.4.aFirstGen$FirstGen, data=employ_datQ43.4.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.4.aGender<-multidatClean(Q43.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aGender$Gender<-factor(employ_datQ43.4.aGender$Gender)
employ_datQ43.4.aGender<-ImpactordinaldatClean(employ_datQ43.4.aGender$Q43.4.a, employ_datQ43.4.aGender)
#ordinal(employ_datQ43.4.aGender$CatOutcome, employ_datQ43.4.aGender$Gender, employ_datQ43.4.aGender)
prep <- analysisPrep(employ_datQ43.4.aGender$CatOutcome, employ_datQ43.4.aGender$Gender, employ_datQ43.4.aGender)
analysis <- polr(employ_datQ43.4.aGender$CatOutcome ~ employ_datQ43.4.aGender$Gender, data=employ_datQ43.4.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.4.aSexuality<-multidatClean(Q43.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.4.aSexuality$Sexuality<-factor(employ_datQ43.4.aSexuality$Sexuality)
employ_datQ43.4.aSexuality<-ImpactordinaldatClean(employ_datQ43.4.aSexuality$Q43.4.a, employ_datQ43.4.aSexuality)
#ordinal(employ_datQ43.4.aSexuality$CatOutcome, employ_datQ43.4.aSexuality$Sexuality, employ_datQ43.4.aSexuality)
prep <- analysisPrep(employ_datQ43.4.aSexuality$CatOutcome, employ_datQ43.4.aSexuality$Sexuality, employ_datQ43.4.aSexuality)
analysis <- polr(employ_datQ43.4.aSexuality$CatOutcome ~ employ_datQ43.4.aSexuality$Sexuality, data=employ_datQ43.4.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.5.a<-multidatClean(Q43.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.5.a + Academic, data = employ_datQ43.5.a)
detach(dat_long)
employ_datQ43.5.a<-ImpactordinaldatClean(employ_datQ43.5.a$Q43.5.a, employ_datQ43.5.a)
#ordinal(employ_datQ43.5.a$CatOutcome, employ_datQ43.5.a$Academic, employ_datQ43.5.a)
prep <- analysisPrep(employ_datQ43.5.a$CatOutcome, employ_datQ43.5.a$Academic, employ_datQ43.5.a)
analysis <- polr(employ_datQ43.5.a$CatOutcome ~ employ_datQ43.5.a$Academic, data=employ_datQ43.5.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.5.aCollege<-multidatClean(Q43.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.5.a + Q3, data = employ_datQ43.5.aCollege)
conTable
detach(dat_long)
employ_datQ43.5.aCollege$Q3[(employ_datQ43.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.5.aCollege$Q3<- factor(employ_datQ43.5.aCollege$Q3)
employ_datQ43.5.aCollege<-ImpactordinaldatClean(employ_datQ43.5.aCollege$Q43.5.a, employ_datQ43.5.aCollege)
#ordinal(employ_datQ43.5.aCollege$CatOutcome, employ_datQ43.5.aCollege$Q3, employ_datQ43.5.aCollege)
prep <- analysisPrep(employ_datQ43.5.aCollege$CatOutcome, employ_datQ43.5.aCollege$Q3, employ_datQ43.5.aCollege)
analysis <- polr(employ_datQ43.5.aCollege$CatOutcome ~ employ_datQ43.5.aCollege$Q3, data=employ_datQ43.5.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.5.aCarer<-multidatClean(Q43.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aCarer$Carer<- factor(employ_datQ43.5.aCarer$Carer)
employ_datQ43.5.aCarer<-ImpactordinaldatClean(employ_datQ43.5.aCarer$Q43.5.a, employ_datQ43.5.aCarer)
#ordinal(employ_datQ43.5.aCarer$CatOutcome, employ_datQ43.5.aCarer$Carer, employ_datQ43.5.aCarer)
prep <- analysisPrep(employ_datQ43.5.aCarer$CatOutcome, employ_datQ43.5.aCarer$Carer, employ_datQ43.5.aCarer)
analysis <- polr(employ_datQ43.5.aCarer$CatOutcome ~ employ_datQ43.5.aCarer$Carer, data=employ_datQ43.5.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.5.aDisability<-multidatClean(Q43.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aDisability$Disability<- factor(employ_datQ43.5.aDisability$Disability)
employ_datQ43.5.aDisability<-ImpactordinaldatClean(employ_datQ43.5.aDisability$Q43.5.a, employ_datQ43.5.aDisability)
conTable <- xtabs(~Q43.5.a + Disability, data = employ_datQ43.5.aDisability)
#ordinal(employ_datQ43.5.aDisability$CatOutcome, employ_datQ43.5.aDisability$Disability, employ_datQ43.5.aDisability)
prep <- analysisPrep(employ_datQ43.5.aDisability$CatOutcome, employ_datQ43.5.aDisability$Disability, employ_datQ43.5.aDisability)
analysis <- polr(employ_datQ43.5.aDisability$CatOutcome ~ employ_datQ43.5.aDisability$Disability, data=employ_datQ43.5.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.5.aEthnicity<-multidatClean(Q43.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aEthnicity$Ethnicity<- factor(employ_datQ43.5.aEthnicity$EthnicityCleaned)
employ_datQ43.5.aEthnicity<-ImpactordinaldatClean(employ_datQ43.5.aEthnicity$Q43.5.a, employ_datQ43.5.aEthnicity)
conTable <- xtabs(~Q43.5.a + EthnicityCleaned, data = employ_datQ43.5.aEthnicity)
conTable
#ordinal(employ_datQ43.5.aEthnicity$CatOutcome, employ_datQ43.5.aEthnicity$EthnicityCleaned, employ_datQ43.5.aEthnicity)
prep <- analysisPrep(employ_datQ43.5.aEthnicity$CatOutcome, employ_datQ43.5.aEthnicity$EthnicityCleaned, employ_datQ43.5.aEthnicity)
analysis <- polr(employ_datQ43.5.aEthnicity$CatOutcome ~ employ_datQ43.5.aEthnicity$EthnicityCleaned, data=employ_datQ43.5.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.5.aFirstGen<-multidatClean(Q43.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aFirstGen$FirstGen<-factor(employ_datQ43.5.aFirstGen$FirstGen)
employ_datQ43.5.aFirstGen<-ImpactordinaldatClean(employ_datQ43.5.aFirstGen$Q43.5.a, employ_datQ43.5.aFirstGen)
#ordinal(employ_datQ43.5.aFirstGen$CatOutcome, employ_datQ43.5.aFirstGen$FirstGen, employ_datQ43.5.aFirstGen)
prep <- analysisPrep(employ_datQ43.5.aFirstGen$CatOutcome, employ_datQ43.5.aFirstGen$FirstGen, employ_datQ43.5.aFirstGen)
analysis <- polr(employ_datQ43.5.aFirstGen$CatOutcome ~ employ_datQ43.5.aFirstGen$FirstGen, data=employ_datQ43.5.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.5.aGender<-multidatClean(Q43.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aGender$Gender<-factor(employ_datQ43.5.aGender$Gender)
employ_datQ43.5.aGender<-ImpactordinaldatClean(employ_datQ43.5.aGender$Q43.5.a, employ_datQ43.5.aGender)
#ordinal(employ_datQ43.5.aGender$CatOutcome, employ_datQ43.5.aGender$Gender, employ_datQ43.5.aGender)
prep <- analysisPrep(employ_datQ43.5.aGender$CatOutcome, employ_datQ43.5.aGender$Gender, employ_datQ43.5.aGender)
analysis <- polr(employ_datQ43.5.aGender$CatOutcome ~ employ_datQ43.5.aGender$Gender, data=employ_datQ43.5.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.5.aSexuality<-multidatClean(Q43.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.5.aSexuality$Sexuality<-factor(employ_datQ43.5.aSexuality$Sexuality)
employ_datQ43.5.aSexuality<-ImpactordinaldatClean(employ_datQ43.5.aSexuality$Q43.5.a, employ_datQ43.5.aSexuality)
#ordinal(employ_datQ43.5.aSexuality$CatOutcome, employ_datQ43.5.aSexuality$Sexuality, employ_datQ43.5.aSexuality)
prep <- analysisPrep(employ_datQ43.5.aSexuality$CatOutcome, employ_datQ43.5.aSexuality$Sexuality, employ_datQ43.5.aSexuality)
analysis <- polr(employ_datQ43.5.aSexuality$CatOutcome ~ employ_datQ43.5.aSexuality$Sexuality, data=employ_datQ43.5.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q43
"Status"
employ_datQ43.6.a<-multidatClean(Q43.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.6.a + Academic, data = employ_datQ43.6.a)
detach(dat_long)
employ_datQ43.6.a<-ImpactordinaldatClean(employ_datQ43.6.a$Q43.6.a, employ_datQ43.6.a)
#ordinal(employ_datQ43.6.a$CatOutcome, employ_datQ43.6.a$Academic, employ_datQ43.6.a)
prep <- analysisPrep(employ_datQ43.6.a$CatOutcome, employ_datQ43.6.a$Academic, employ_datQ43.6.a)
analysis <- polr(employ_datQ43.6.a$CatOutcome ~ employ_datQ43.6.a$Academic, data=employ_datQ43.6.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q43.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ43.6.aCollege<-multidatClean(Q43.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q43.6.a + Q3, data = employ_datQ43.6.aCollege)
conTable
detach(dat_long)
employ_datQ43.6.aCollege$Q3[(employ_datQ43.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ43.6.aCollege$Q3<- factor(employ_datQ43.6.aCollege$Q3)
employ_datQ43.6.aCollege<-ImpactordinaldatClean(employ_datQ43.6.aCollege$Q43.6.a, employ_datQ43.6.aCollege)
#ordinal(employ_datQ43.6.aCollege$CatOutcome, employ_datQ43.6.aCollege$Q3, employ_datQ43.6.aCollege)
prep <- analysisPrep(employ_datQ43.6.aCollege$CatOutcome, employ_datQ43.6.aCollege$Q3, employ_datQ43.6.aCollege)
analysis <- polr(employ_datQ43.6.aCollege$CatOutcome ~ employ_datQ43.6.aCollege$Q3, data=employ_datQ43.6.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ43.6.aCarer<-multidatClean(Q43.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aCarer$Carer<- factor(employ_datQ43.6.aCarer$Carer)
employ_datQ43.6.aCarer<-ImpactordinaldatClean(employ_datQ43.6.aCarer$Q43.6.a, employ_datQ43.6.aCarer)
#ordinal(employ_datQ43.6.aCarer$CatOutcome, employ_datQ43.6.aCarer$Carer, employ_datQ43.6.aCarer)
prep <- analysisPrep(employ_datQ43.6.aCarer$CatOutcome, employ_datQ43.6.aCarer$Carer, employ_datQ43.6.aCarer)
analysis <- polr(employ_datQ43.6.aCarer$CatOutcome ~ employ_datQ43.6.aCarer$Carer, data=employ_datQ43.6.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ43.6.aDisability<-multidatClean(Q43.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aDisability$Disability<- factor(employ_datQ43.6.aDisability$Disability)
employ_datQ43.6.aDisability<-ImpactordinaldatClean(employ_datQ43.6.aDisability$Q43.6.a, employ_datQ43.6.aDisability)
conTable <- xtabs(~Q43.6.a + Disability, data = employ_datQ43.6.aDisability)
#ordinal(employ_datQ43.6.aDisability$CatOutcome, employ_datQ43.6.aDisability$Disability, employ_datQ43.6.aDisability)
prep <- analysisPrep(employ_datQ43.6.aDisability$CatOutcome, employ_datQ43.6.aDisability$Disability, employ_datQ43.6.aDisability)
analysis <- polr(employ_datQ43.6.aDisability$CatOutcome ~ employ_datQ43.6.aDisability$Disability, data=employ_datQ43.6.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ43.6.aEthnicity<-multidatClean(Q43.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aEthnicity$Ethnicity<- factor(employ_datQ43.6.aEthnicity$EthnicityCleaned)
employ_datQ43.6.aEthnicity<-ImpactordinaldatClean(employ_datQ43.6.aEthnicity$Q43.6.a, employ_datQ43.6.aEthnicity)
conTable <- xtabs(~Q43.6.a + EthnicityCleaned, data = employ_datQ43.6.aEthnicity)
conTable
#ordinal(employ_datQ43.6.aEthnicity$CatOutcome, employ_datQ43.6.aEthnicity$EthnicityCleaned, employ_datQ43.6.aEthnicity)
prep <- analysisPrep(employ_datQ43.6.aEthnicity$CatOutcome, employ_datQ43.6.aEthnicity$EthnicityCleaned, employ_datQ43.6.aEthnicity)
analysis <- polr(employ_datQ43.6.aEthnicity$CatOutcome ~ employ_datQ43.6.aEthnicity$EthnicityCleaned, data=employ_datQ43.6.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ43.6.aFirstGen<-multidatClean(Q43.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aFirstGen$FirstGen<-factor(employ_datQ43.6.aFirstGen$FirstGen)
employ_datQ43.6.aFirstGen<-ImpactordinaldatClean(employ_datQ43.6.aFirstGen$Q43.6.a, employ_datQ43.6.aFirstGen)
#ordinal(employ_datQ43.6.aFirstGen$CatOutcome, employ_datQ43.6.aFirstGen$FirstGen, employ_datQ43.6.aFirstGen)
prep <- analysisPrep(employ_datQ43.6.aFirstGen$CatOutcome, employ_datQ43.6.aFirstGen$FirstGen, employ_datQ43.6.aFirstGen)
analysis <- polr(employ_datQ43.6.aFirstGen$CatOutcome ~ employ_datQ43.6.aFirstGen$FirstGen, data=employ_datQ43.6.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ43.6.aGender<-multidatClean(Q43.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aGender$Gender<-factor(employ_datQ43.6.aGender$Gender)
employ_datQ43.6.aGender<-ImpactordinaldatClean(employ_datQ43.6.aGender$Q43.6.a, employ_datQ43.6.aGender)
#ordinal(employ_datQ43.6.aGender$CatOutcome, employ_datQ43.6.aGender$Gender, employ_datQ43.6.aGender)
prep <- analysisPrep(employ_datQ43.6.aGender$CatOutcome, employ_datQ43.6.aGender$Gender, employ_datQ43.6.aGender)
analysis <- polr(employ_datQ43.6.aGender$CatOutcome ~ employ_datQ43.6.aGender$Gender, data=employ_datQ43.6.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ43.6.aSexuality<-multidatClean(Q43.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ43.6.aSexuality$Sexuality<-factor(employ_datQ43.6.aSexuality$Sexuality)
employ_datQ43.6.aSexuality<-ImpactordinaldatClean(employ_datQ43.6.aSexuality$Q43.6.a, employ_datQ43.6.aSexuality)
#ordinal(employ_datQ43.6.aSexuality$CatOutcome, employ_datQ43.6.aSexuality$Sexuality, employ_datQ43.6.aSexuality)
prep <- analysisPrep(employ_datQ43.6.aSexuality$CatOutcome, employ_datQ43.6.aSexuality$Sexuality, employ_datQ43.6.aSexuality)
analysis <- polr(employ_datQ43.6.aSexuality$CatOutcome ~ employ_datQ43.6.aSexuality$Sexuality, data=employ_datQ43.6.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q43.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q43.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.1.a<-multidatClean(Q44.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.1.a + Academic, data = employ_datQ44.1.a)
detach(dat_long)
employ_datQ44.1.a<-ResponsibilityordinaldatClean(employ_datQ44.1.a$Q44.1.a, employ_datQ44.1.a)
#ordinal(employ_datQ44.1.a$CatOutcome, employ_datQ44.1.a$Academic, employ_datQ44.1.a)
prep <- analysisPrep(employ_datQ44.1.a$CatOutcome, employ_datQ44.1.a$Academic, employ_datQ44.1.a)
analysis <- polr(employ_datQ44.1.a$CatOutcome ~ employ_datQ44.1.a$Academic, data=employ_datQ44.1.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.1.aCollege<-multidatClean(Q44.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.1.a + Q3, data = employ_datQ44.1.aCollege)
conTable
detach(dat_long)
employ_datQ44.1.aCollege$Q3[(employ_datQ44.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.1.aCollege$Q3<- factor(employ_datQ44.1.aCollege$Q3)
employ_datQ44.1.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.1.aCollege$Q44.1.a, employ_datQ44.1.aCollege)
#ordinal(employ_datQ44.1.aCollege$CatOutcome, employ_datQ44.1.aCollege$Q3, employ_datQ44.1.aCollege)
prep <- analysisPrep(employ_datQ44.1.aCollege$CatOutcome, employ_datQ44.1.aCollege$Q3, employ_datQ44.1.aCollege)
analysis <- polr(employ_datQ44.1.aCollege$CatOutcome ~ employ_datQ44.1.aCollege$Q3, data=employ_datQ44.1.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.1.aCarer<-multidatClean(Q44.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aCarer$Carer<- factor(employ_datQ44.1.aCarer$Carer)
employ_datQ44.1.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.1.aCarer$Q44.1.a, employ_datQ44.1.aCarer)
#ordinal(employ_datQ44.1.aCarer$CatOutcome, employ_datQ44.1.aCarer$Carer, employ_datQ44.1.aCarer)
prep <- analysisPrep(employ_datQ44.1.aCarer$CatOutcome, employ_datQ44.1.aCarer$Carer, employ_datQ44.1.aCarer)
analysis <- polr(employ_datQ44.1.aCarer$CatOutcome ~ employ_datQ44.1.aCarer$Carer, data=employ_datQ44.1.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.1.aDisability<-multidatClean(Q44.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aDisability$Disability<- factor(employ_datQ44.1.aDisability$Disability)
employ_datQ44.1.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.1.aDisability$Q44.1.a, employ_datQ44.1.aDisability)
conTable <- xtabs(~Q44.1.a + Disability, data = employ_datQ44.1.aDisability)
#ordinal(employ_datQ44.1.aDisability$CatOutcome, employ_datQ44.1.aDisability$Disability, employ_datQ44.1.aDisability)
prep <- analysisPrep(employ_datQ44.1.aDisability$CatOutcome, employ_datQ44.1.aDisability$Disability, employ_datQ44.1.aDisability)
analysis <- polr(employ_datQ44.1.aDisability$CatOutcome ~ employ_datQ44.1.aDisability$Disability, data=employ_datQ44.1.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.1.aEthnicity<-multidatClean(Q44.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aEthnicity$Ethnicity<- factor(employ_datQ44.1.aEthnicity$EthnicityCleaned)
employ_datQ44.1.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.1.aEthnicity$Q44.1.a, employ_datQ44.1.aEthnicity)
conTable <- xtabs(~Q44.1.a + EthnicityCleaned, data = employ_datQ44.1.aEthnicity)
conTable
#ordinal(employ_datQ44.1.aEthnicity$CatOutcome, employ_datQ44.1.aEthnicity$EthnicityCleaned, employ_datQ44.1.aEthnicity)
prep <- analysisPrep(employ_datQ44.1.aEthnicity$CatOutcome, employ_datQ44.1.aEthnicity$EthnicityCleaned, employ_datQ44.1.aEthnicity)
analysis <- polr(employ_datQ44.1.aEthnicity$CatOutcome ~ employ_datQ44.1.aEthnicity$EthnicityCleaned, data=employ_datQ44.1.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.1.aFirstGen<-multidatClean(Q44.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aFirstGen$FirstGen<-factor(employ_datQ44.1.aFirstGen$FirstGen)
employ_datQ44.1.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.1.aFirstGen$Q44.1.a, employ_datQ44.1.aFirstGen)
#ordinal(employ_datQ44.1.aFirstGen$CatOutcome, employ_datQ44.1.aFirstGen$FirstGen, employ_datQ44.1.aFirstGen)
prep <- analysisPrep(employ_datQ44.1.aFirstGen$CatOutcome, employ_datQ44.1.aFirstGen$FirstGen, employ_datQ44.1.aFirstGen)
analysis <- polr(employ_datQ44.1.aFirstGen$CatOutcome ~ employ_datQ44.1.aFirstGen$FirstGen, data=employ_datQ44.1.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.1.aGender<-multidatClean(Q44.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aGender$Gender<-factor(employ_datQ44.1.aGender$Gender)
employ_datQ44.1.aGender<-ResponsibilityordinaldatClean(employ_datQ44.1.aGender$Q44.1.a, employ_datQ44.1.aGender)
#ordinal(employ_datQ44.1.aGender$CatOutcome, employ_datQ44.1.aGender$Gender, employ_datQ44.1.aGender)
prep <- analysisPrep(employ_datQ44.1.aGender$CatOutcome, employ_datQ44.1.aGender$Gender, employ_datQ44.1.aGender)
analysis <- polr(employ_datQ44.1.aGender$CatOutcome ~ employ_datQ44.1.aGender$Gender, data=employ_datQ44.1.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.1.aSexuality<-multidatClean(Q44.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.1.aSexuality$Sexuality<-factor(employ_datQ44.1.aSexuality$Sexuality)
employ_datQ44.1.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.1.aSexuality$Q44.1.a, employ_datQ44.1.aSexuality)
#ordinal(employ_datQ44.1.aSexuality$CatOutcome, employ_datQ44.1.aSexuality$Sexuality, employ_datQ44.1.aSexuality)
prep <- analysisPrep(employ_datQ44.1.aSexuality$CatOutcome, employ_datQ44.1.aSexuality$Sexuality, employ_datQ44.1.aSexuality)
analysis <- polr(employ_datQ44.1.aSexuality$CatOutcome ~ employ_datQ44.1.aSexuality$Sexuality, data=employ_datQ44.1.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.1.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.2.a<-multidatClean(Q44.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.2.a + Academic, data = employ_datQ44.2.a)
detach(dat_long)
employ_datQ44.2.a<-ResponsibilityordinaldatClean(employ_datQ44.2.a$Q44.2.a, employ_datQ44.2.a)
#ordinal(employ_datQ44.2.a$CatOutcome, employ_datQ44.2.a$Academic, employ_datQ44.2.a)
prep <- analysisPrep(employ_datQ44.2.a$CatOutcome, employ_datQ44.2.a$Academic, employ_datQ44.2.a)
analysis <- polr(employ_datQ44.2.a$CatOutcome ~ employ_datQ44.2.a$Academic, data=employ_datQ44.2.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.2.aCollege<-multidatClean(Q44.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.2.a + Q3, data = employ_datQ44.2.aCollege)
conTable
detach(dat_long)
employ_datQ44.2.aCollege$Q3[(employ_datQ44.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.2.aCollege$Q3<- factor(employ_datQ44.2.aCollege$Q3)
employ_datQ44.2.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.2.aCollege$Q44.2.a, employ_datQ44.2.aCollege)
#ordinal(employ_datQ44.2.aCollege$CatOutcome, employ_datQ44.2.aCollege$Q3, employ_datQ44.2.aCollege)
prep <- analysisPrep(employ_datQ44.2.aCollege$CatOutcome, employ_datQ44.2.aCollege$Q3, employ_datQ44.2.aCollege)
analysis <- polr(employ_datQ44.2.aCollege$CatOutcome ~ employ_datQ44.2.aCollege$Q3, data=employ_datQ44.2.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.2.aCarer<-multidatClean(Q44.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aCarer$Carer<- factor(employ_datQ44.2.aCarer$Carer)
employ_datQ44.2.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.2.aCarer$Q44.2.a, employ_datQ44.2.aCarer)
#ordinal(employ_datQ44.2.aCarer$CatOutcome, employ_datQ44.2.aCarer$Carer, employ_datQ44.2.aCarer)
prep <- analysisPrep(employ_datQ44.2.aCarer$CatOutcome, employ_datQ44.2.aCarer$Carer, employ_datQ44.2.aCarer)
analysis <- polr(employ_datQ44.2.aCarer$CatOutcome ~ employ_datQ44.2.aCarer$Carer, data=employ_datQ44.2.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.2.aDisability<-multidatClean(Q44.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aDisability$Disability<- factor(employ_datQ44.2.aDisability$Disability)
employ_datQ44.2.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.2.aDisability$Q44.2.a, employ_datQ44.2.aDisability)
conTable <- xtabs(~Q44.2.a + Disability, data = employ_datQ44.2.aDisability)
#ordinal(employ_datQ44.2.aDisability$CatOutcome, employ_datQ44.2.aDisability$Disability, employ_datQ44.2.aDisability)
prep <- analysisPrep(employ_datQ44.2.aDisability$CatOutcome, employ_datQ44.2.aDisability$Disability, employ_datQ44.2.aDisability)
analysis <- polr(employ_datQ44.2.aDisability$CatOutcome ~ employ_datQ44.2.aDisability$Disability, data=employ_datQ44.2.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.2.aEthnicity<-multidatClean(Q44.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aEthnicity$Ethnicity<- factor(employ_datQ44.2.aEthnicity$EthnicityCleaned)
employ_datQ44.2.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.2.aEthnicity$Q44.2.a, employ_datQ44.2.aEthnicity)
conTable <- xtabs(~Q44.2.a + EthnicityCleaned, data = employ_datQ44.2.aEthnicity)
conTable
#ordinal(employ_datQ44.2.aEthnicity$CatOutcome, employ_datQ44.2.aEthnicity$EthnicityCleaned, employ_datQ44.2.aEthnicity)
prep <- analysisPrep(employ_datQ44.2.aEthnicity$CatOutcome, employ_datQ44.2.aEthnicity$EthnicityCleaned, employ_datQ44.2.aEthnicity)
analysis <- polr(employ_datQ44.2.aEthnicity$CatOutcome ~ employ_datQ44.2.aEthnicity$EthnicityCleaned, data=employ_datQ44.2.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.2.aFirstGen<-multidatClean(Q44.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aFirstGen$FirstGen<-factor(employ_datQ44.2.aFirstGen$FirstGen)
employ_datQ44.2.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.2.aFirstGen$Q44.2.a, employ_datQ44.2.aFirstGen)
#ordinal(employ_datQ44.2.aFirstGen$CatOutcome, employ_datQ44.2.aFirstGen$FirstGen, employ_datQ44.2.aFirstGen)
prep <- analysisPrep(employ_datQ44.2.aFirstGen$CatOutcome, employ_datQ44.2.aFirstGen$FirstGen, employ_datQ44.2.aFirstGen)
analysis <- polr(employ_datQ44.2.aFirstGen$CatOutcome ~ employ_datQ44.2.aFirstGen$FirstGen, data=employ_datQ44.2.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.2.aGender<-multidatClean(Q44.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aGender$Gender<-factor(employ_datQ44.2.aGender$Gender)
employ_datQ44.2.aGender<-ResponsibilityordinaldatClean(employ_datQ44.2.aGender$Q44.2.a, employ_datQ44.2.aGender)
#ordinal(employ_datQ44.2.aGender$CatOutcome, employ_datQ44.2.aGender$Gender, employ_datQ44.2.aGender)
prep <- analysisPrep(employ_datQ44.2.aGender$CatOutcome, employ_datQ44.2.aGender$Gender, employ_datQ44.2.aGender)
analysis <- polr(employ_datQ44.2.aGender$CatOutcome ~ employ_datQ44.2.aGender$Gender, data=employ_datQ44.2.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.2.aSexuality<-multidatClean(Q44.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.2.aSexuality$Sexuality<-factor(employ_datQ44.2.aSexuality$Sexuality)
employ_datQ44.2.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.2.aSexuality$Q44.2.a, employ_datQ44.2.aSexuality)
#ordinal(employ_datQ44.2.aSexuality$CatOutcome, employ_datQ44.2.aSexuality$Sexuality, employ_datQ44.2.aSexuality)
prep <- analysisPrep(employ_datQ44.2.aSexuality$CatOutcome, employ_datQ44.2.aSexuality$Sexuality, employ_datQ44.2.aSexuality)
analysis <- polr(employ_datQ44.2.aSexuality$CatOutcome ~ employ_datQ44.2.aSexuality$Sexuality, data=employ_datQ44.2.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.2.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.3.a<-multidatClean(Q44.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.3.a + Academic, data = employ_datQ44.3.a)
detach(dat_long)
employ_datQ44.3.a<-ResponsibilityordinaldatClean(employ_datQ44.3.a$Q44.3.a, employ_datQ44.3.a)
#ordinal(employ_datQ44.3.a$CatOutcome, employ_datQ44.3.a$Academic, employ_datQ44.3.a)
prep <- analysisPrep(employ_datQ44.3.a$CatOutcome, employ_datQ44.3.a$Academic, employ_datQ44.3.a)
analysis <- polr(employ_datQ44.3.a$CatOutcome ~ employ_datQ44.3.a$Academic, data=employ_datQ44.3.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.3.aCollege<-multidatClean(Q44.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.3.a + Q3, data = employ_datQ44.3.aCollege)
conTable
detach(dat_long)
employ_datQ44.3.aCollege$Q3[(employ_datQ44.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.3.aCollege$Q3<- factor(employ_datQ44.3.aCollege$Q3)
employ_datQ44.3.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.3.aCollege$Q44.3.a, employ_datQ44.3.aCollege)
#ordinal(employ_datQ44.3.aCollege$CatOutcome, employ_datQ44.3.aCollege$Q3, employ_datQ44.3.aCollege)
prep <- analysisPrep(employ_datQ44.3.aCollege$CatOutcome, employ_datQ44.3.aCollege$Q3, employ_datQ44.3.aCollege)
analysis <- polr(employ_datQ44.3.aCollege$CatOutcome ~ employ_datQ44.3.aCollege$Q3, data=employ_datQ44.3.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.3.aCarer<-multidatClean(Q44.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aCarer$Carer<- factor(employ_datQ44.3.aCarer$Carer)
employ_datQ44.3.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.3.aCarer$Q44.3.a, employ_datQ44.3.aCarer)
#ordinal(employ_datQ44.3.aCarer$CatOutcome, employ_datQ44.3.aCarer$Carer, employ_datQ44.3.aCarer)
prep <- analysisPrep(employ_datQ44.3.aCarer$CatOutcome, employ_datQ44.3.aCarer$Carer, employ_datQ44.3.aCarer)
analysis <- polr(employ_datQ44.3.aCarer$CatOutcome ~ employ_datQ44.3.aCarer$Carer, data=employ_datQ44.3.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.3.aDisability<-multidatClean(Q44.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aDisability$Disability<- factor(employ_datQ44.3.aDisability$Disability)
employ_datQ44.3.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.3.aDisability$Q44.3.a, employ_datQ44.3.aDisability)
conTable <- xtabs(~Q44.3.a + Disability, data = employ_datQ44.3.aDisability)
#ordinal(employ_datQ44.3.aDisability$CatOutcome, employ_datQ44.3.aDisability$Disability, employ_datQ44.3.aDisability)
prep <- analysisPrep(employ_datQ44.3.aDisability$CatOutcome, employ_datQ44.3.aDisability$Disability, employ_datQ44.3.aDisability)
analysis <- polr(employ_datQ44.3.aDisability$CatOutcome ~ employ_datQ44.3.aDisability$Disability, data=employ_datQ44.3.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.3.aEthnicity<-multidatClean(Q44.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aEthnicity$Ethnicity<- factor(employ_datQ44.3.aEthnicity$EthnicityCleaned)
employ_datQ44.3.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.3.aEthnicity$Q44.3.a, employ_datQ44.3.aEthnicity)
conTable <- xtabs(~Q44.3.a + EthnicityCleaned, data = employ_datQ44.3.aEthnicity)
conTable
#ordinal(employ_datQ44.3.aEthnicity$CatOutcome, employ_datQ44.3.aEthnicity$EthnicityCleaned, employ_datQ44.3.aEthnicity)
prep <- analysisPrep(employ_datQ44.3.aEthnicity$CatOutcome, employ_datQ44.3.aEthnicity$EthnicityCleaned, employ_datQ44.3.aEthnicity)
analysis <- polr(employ_datQ44.3.aEthnicity$CatOutcome ~ employ_datQ44.3.aEthnicity$EthnicityCleaned, data=employ_datQ44.3.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.3.aFirstGen<-multidatClean(Q44.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aFirstGen$FirstGen<-factor(employ_datQ44.3.aFirstGen$FirstGen)
employ_datQ44.3.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.3.aFirstGen$Q44.3.a, employ_datQ44.3.aFirstGen)
#ordinal(employ_datQ44.3.aFirstGen$CatOutcome, employ_datQ44.3.aFirstGen$FirstGen, employ_datQ44.3.aFirstGen)
prep <- analysisPrep(employ_datQ44.3.aFirstGen$CatOutcome, employ_datQ44.3.aFirstGen$FirstGen, employ_datQ44.3.aFirstGen)
analysis <- polr(employ_datQ44.3.aFirstGen$CatOutcome ~ employ_datQ44.3.aFirstGen$FirstGen, data=employ_datQ44.3.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.3.aGender<-multidatClean(Q44.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aGender$Gender<-factor(employ_datQ44.3.aGender$Gender)
employ_datQ44.3.aGender<-ResponsibilityordinaldatClean(employ_datQ44.3.aGender$Q44.3.a, employ_datQ44.3.aGender)
#ordinal(employ_datQ44.3.aGender$CatOutcome, employ_datQ44.3.aGender$Gender, employ_datQ44.3.aGender)
prep <- analysisPrep(employ_datQ44.3.aGender$CatOutcome, employ_datQ44.3.aGender$Gender, employ_datQ44.3.aGender)
analysis <- polr(employ_datQ44.3.aGender$CatOutcome ~ employ_datQ44.3.aGender$Gender, data=employ_datQ44.3.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.3.aSexuality<-multidatClean(Q44.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.3.aSexuality$Sexuality<-factor(employ_datQ44.3.aSexuality$Sexuality)
employ_datQ44.3.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.3.aSexuality$Q44.3.a, employ_datQ44.3.aSexuality)
#ordinal(employ_datQ44.3.aSexuality$CatOutcome, employ_datQ44.3.aSexuality$Sexuality, employ_datQ44.3.aSexuality)
prep <- analysisPrep(employ_datQ44.3.aSexuality$CatOutcome, employ_datQ44.3.aSexuality$Sexuality, employ_datQ44.3.aSexuality)
analysis <- polr(employ_datQ44.3.aSexuality$CatOutcome ~ employ_datQ44.3.aSexuality$Sexuality, data=employ_datQ44.3.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.3.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.4.a<-multidatClean(Q44.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.4.a + Academic, data = employ_datQ44.4.a)
detach(dat_long)
employ_datQ44.4.a<-ResponsibilityordinaldatClean(employ_datQ44.4.a$Q44.4.a, employ_datQ44.4.a)
#ordinal(employ_datQ44.4.a$CatOutcome, employ_datQ44.4.a$Academic, employ_datQ44.4.a)
prep <- analysisPrep(employ_datQ44.4.a$CatOutcome, employ_datQ44.4.a$Academic, employ_datQ44.4.a)
analysis <- polr(employ_datQ44.4.a$CatOutcome ~ employ_datQ44.4.a$Academic, data=employ_datQ44.4.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.4.aCollege<-multidatClean(Q44.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.4.a + Q3, data = employ_datQ44.4.aCollege)
conTable
detach(dat_long)
employ_datQ44.4.aCollege$Q3[(employ_datQ44.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.4.aCollege$Q3<- factor(employ_datQ44.4.aCollege$Q3)
employ_datQ44.4.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.4.aCollege$Q44.4.a, employ_datQ44.4.aCollege)
#ordinal(employ_datQ44.4.aCollege$CatOutcome, employ_datQ44.4.aCollege$Q3, employ_datQ44.4.aCollege)
prep <- analysisPrep(employ_datQ44.4.aCollege$CatOutcome, employ_datQ44.4.aCollege$Q3, employ_datQ44.4.aCollege)
analysis <- polr(employ_datQ44.4.aCollege$CatOutcome ~ employ_datQ44.4.aCollege$Q3, data=employ_datQ44.4.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.4.aCarer<-multidatClean(Q44.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aCarer$Carer<- factor(employ_datQ44.4.aCarer$Carer)
employ_datQ44.4.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.4.aCarer$Q44.4.a, employ_datQ44.4.aCarer)
#ordinal(employ_datQ44.4.aCarer$CatOutcome, employ_datQ44.4.aCarer$Carer, employ_datQ44.4.aCarer)
prep <- analysisPrep(employ_datQ44.4.aCarer$CatOutcome, employ_datQ44.4.aCarer$Carer, employ_datQ44.4.aCarer)
analysis <- polr(employ_datQ44.4.aCarer$CatOutcome ~ employ_datQ44.4.aCarer$Carer, data=employ_datQ44.4.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.4.aDisability<-multidatClean(Q44.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aDisability$Disability<- factor(employ_datQ44.4.aDisability$Disability)
employ_datQ44.4.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.4.aDisability$Q44.4.a, employ_datQ44.4.aDisability)
conTable <- xtabs(~Q44.4.a + Disability, data = employ_datQ44.4.aDisability)
#ordinal(employ_datQ44.4.aDisability$CatOutcome, employ_datQ44.4.aDisability$Disability, employ_datQ44.4.aDisability)
prep <- analysisPrep(employ_datQ44.4.aDisability$CatOutcome, employ_datQ44.4.aDisability$Disability, employ_datQ44.4.aDisability)
analysis <- polr(employ_datQ44.4.aDisability$CatOutcome ~ employ_datQ44.4.aDisability$Disability, data=employ_datQ44.4.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.4.aEthnicity<-multidatClean(Q44.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aEthnicity$Ethnicity<- factor(employ_datQ44.4.aEthnicity$EthnicityCleaned)
employ_datQ44.4.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.4.aEthnicity$Q44.4.a, employ_datQ44.4.aEthnicity)
conTable <- xtabs(~Q44.4.a + EthnicityCleaned, data = employ_datQ44.4.aEthnicity)
conTable
#ordinal(employ_datQ44.4.aEthnicity$CatOutcome, employ_datQ44.4.aEthnicity$EthnicityCleaned, employ_datQ44.4.aEthnicity)
prep <- analysisPrep(employ_datQ44.4.aEthnicity$CatOutcome, employ_datQ44.4.aEthnicity$EthnicityCleaned, employ_datQ44.4.aEthnicity)
analysis <- polr(employ_datQ44.4.aEthnicity$CatOutcome ~ employ_datQ44.4.aEthnicity$EthnicityCleaned, data=employ_datQ44.4.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.4.aFirstGen<-multidatClean(Q44.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aFirstGen$FirstGen<-factor(employ_datQ44.4.aFirstGen$FirstGen)
employ_datQ44.4.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.4.aFirstGen$Q44.4.a, employ_datQ44.4.aFirstGen)
#ordinal(employ_datQ44.4.aFirstGen$CatOutcome, employ_datQ44.4.aFirstGen$FirstGen, employ_datQ44.4.aFirstGen)
prep <- analysisPrep(employ_datQ44.4.aFirstGen$CatOutcome, employ_datQ44.4.aFirstGen$FirstGen, employ_datQ44.4.aFirstGen)
analysis <- polr(employ_datQ44.4.aFirstGen$CatOutcome ~ employ_datQ44.4.aFirstGen$FirstGen, data=employ_datQ44.4.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.4.aGender<-multidatClean(Q44.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aGender$Gender<-factor(employ_datQ44.4.aGender$Gender)
employ_datQ44.4.aGender<-ResponsibilityordinaldatClean(employ_datQ44.4.aGender$Q44.4.a, employ_datQ44.4.aGender)
#ordinal(employ_datQ44.4.aGender$CatOutcome, employ_datQ44.4.aGender$Gender, employ_datQ44.4.aGender)
prep <- analysisPrep(employ_datQ44.4.aGender$CatOutcome, employ_datQ44.4.aGender$Gender, employ_datQ44.4.aGender)
analysis <- polr(employ_datQ44.4.aGender$CatOutcome ~ employ_datQ44.4.aGender$Gender, data=employ_datQ44.4.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.4.aSexuality<-multidatClean(Q44.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.4.aSexuality$Sexuality<-factor(employ_datQ44.4.aSexuality$Sexuality)
employ_datQ44.4.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.4.aSexuality$Q44.4.a, employ_datQ44.4.aSexuality)
#ordinal(employ_datQ44.4.aSexuality$CatOutcome, employ_datQ44.4.aSexuality$Sexuality, employ_datQ44.4.aSexuality)
prep <- analysisPrep(employ_datQ44.4.aSexuality$CatOutcome, employ_datQ44.4.aSexuality$Sexuality, employ_datQ44.4.aSexuality)
analysis <- polr(employ_datQ44.4.aSexuality$CatOutcome ~ employ_datQ44.4.aSexuality$Sexuality, data=employ_datQ44.4.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.4.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.5.a<-multidatClean(Q44.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.5.a + Academic, data = employ_datQ44.5.a)
detach(dat_long)
employ_datQ44.5.a<-ResponsibilityordinaldatClean(employ_datQ44.5.a$Q44.5.a, employ_datQ44.5.a)
#ordinal(employ_datQ44.5.a$CatOutcome, employ_datQ44.5.a$Academic, employ_datQ44.5.a)
prep <- analysisPrep(employ_datQ44.5.a$CatOutcome, employ_datQ44.5.a$Academic, employ_datQ44.5.a)
analysis <- polr(employ_datQ44.5.a$CatOutcome ~ employ_datQ44.5.a$Academic, data=employ_datQ44.5.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.5.aCollege<-multidatClean(Q44.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.5.a + Q3, data = employ_datQ44.5.aCollege)
conTable
detach(dat_long)
employ_datQ44.5.aCollege$Q3[(employ_datQ44.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.5.aCollege$Q3<- factor(employ_datQ44.5.aCollege$Q3)
employ_datQ44.5.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.5.aCollege$Q44.5.a, employ_datQ44.5.aCollege)
#ordinal(employ_datQ44.5.aCollege$CatOutcome, employ_datQ44.5.aCollege$Q3, employ_datQ44.5.aCollege)
prep <- analysisPrep(employ_datQ44.5.aCollege$CatOutcome, employ_datQ44.5.aCollege$Q3, employ_datQ44.5.aCollege)
analysis <- polr(employ_datQ44.5.aCollege$CatOutcome ~ employ_datQ44.5.aCollege$Q3, data=employ_datQ44.5.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.5.aCarer<-multidatClean(Q44.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aCarer$Carer<- factor(employ_datQ44.5.aCarer$Carer)
employ_datQ44.5.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.5.aCarer$Q44.5.a, employ_datQ44.5.aCarer)
#ordinal(employ_datQ44.5.aCarer$CatOutcome, employ_datQ44.5.aCarer$Carer, employ_datQ44.5.aCarer)
prep <- analysisPrep(employ_datQ44.5.aCarer$CatOutcome, employ_datQ44.5.aCarer$Carer, employ_datQ44.5.aCarer)
analysis <- polr(employ_datQ44.5.aCarer$CatOutcome ~ employ_datQ44.5.aCarer$Carer, data=employ_datQ44.5.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.5.aDisability<-multidatClean(Q44.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aDisability$Disability<- factor(employ_datQ44.5.aDisability$Disability)
employ_datQ44.5.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.5.aDisability$Q44.5.a, employ_datQ44.5.aDisability)
conTable <- xtabs(~Q44.5.a + Disability, data = employ_datQ44.5.aDisability)
#ordinal(employ_datQ44.5.aDisability$CatOutcome, employ_datQ44.5.aDisability$Disability, employ_datQ44.5.aDisability)
prep <- analysisPrep(employ_datQ44.5.aDisability$CatOutcome, employ_datQ44.5.aDisability$Disability, employ_datQ44.5.aDisability)
analysis <- polr(employ_datQ44.5.aDisability$CatOutcome ~ employ_datQ44.5.aDisability$Disability, data=employ_datQ44.5.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.5.aEthnicity<-multidatClean(Q44.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aEthnicity$Ethnicity<- factor(employ_datQ44.5.aEthnicity$EthnicityCleaned)
employ_datQ44.5.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.5.aEthnicity$Q44.5.a, employ_datQ44.5.aEthnicity)
conTable <- xtabs(~Q44.5.a + EthnicityCleaned, data = employ_datQ44.5.aEthnicity)
conTable
#ordinal(employ_datQ44.5.aEthnicity$CatOutcome, employ_datQ44.5.aEthnicity$EthnicityCleaned, employ_datQ44.5.aEthnicity)
prep <- analysisPrep(employ_datQ44.5.aEthnicity$CatOutcome, employ_datQ44.5.aEthnicity$EthnicityCleaned, employ_datQ44.5.aEthnicity)
analysis <- polr(employ_datQ44.5.aEthnicity$CatOutcome ~ employ_datQ44.5.aEthnicity$EthnicityCleaned, data=employ_datQ44.5.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.5.aFirstGen<-multidatClean(Q44.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aFirstGen$FirstGen<-factor(employ_datQ44.5.aFirstGen$FirstGen)
employ_datQ44.5.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.5.aFirstGen$Q44.5.a, employ_datQ44.5.aFirstGen)
#ordinal(employ_datQ44.5.aFirstGen$CatOutcome, employ_datQ44.5.aFirstGen$FirstGen, employ_datQ44.5.aFirstGen)
prep <- analysisPrep(employ_datQ44.5.aFirstGen$CatOutcome, employ_datQ44.5.aFirstGen$FirstGen, employ_datQ44.5.aFirstGen)
analysis <- polr(employ_datQ44.5.aFirstGen$CatOutcome ~ employ_datQ44.5.aFirstGen$FirstGen, data=employ_datQ44.5.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.5.aGender<-multidatClean(Q44.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aGender$Gender<-factor(employ_datQ44.5.aGender$Gender)
employ_datQ44.5.aGender<-ResponsibilityordinaldatClean(employ_datQ44.5.aGender$Q44.5.a, employ_datQ44.5.aGender)
#ordinal(employ_datQ44.5.aGender$CatOutcome, employ_datQ44.5.aGender$Gender, employ_datQ44.5.aGender)
prep <- analysisPrep(employ_datQ44.5.aGender$CatOutcome, employ_datQ44.5.aGender$Gender, employ_datQ44.5.aGender)
analysis <- polr(employ_datQ44.5.aGender$CatOutcome ~ employ_datQ44.5.aGender$Gender, data=employ_datQ44.5.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.5.aSexuality<-multidatClean(Q44.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.5.aSexuality$Sexuality<-factor(employ_datQ44.5.aSexuality$Sexuality)
employ_datQ44.5.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.5.aSexuality$Q44.5.a, employ_datQ44.5.aSexuality)
#ordinal(employ_datQ44.5.aSexuality$CatOutcome, employ_datQ44.5.aSexuality$Sexuality, employ_datQ44.5.aSexuality)
prep <- analysisPrep(employ_datQ44.5.aSexuality$CatOutcome, employ_datQ44.5.aSexuality$Sexuality, employ_datQ44.5.aSexuality)
analysis <- polr(employ_datQ44.5.aSexuality$CatOutcome ~ employ_datQ44.5.aSexuality$Sexuality, data=employ_datQ44.5.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.5.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.6.a<-multidatClean(Q44.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.6.a + Academic, data = employ_datQ44.6.a)
detach(dat_long)
employ_datQ44.6.a<-ResponsibilityordinaldatClean(employ_datQ44.6.a$Q44.6.a, employ_datQ44.6.a)
#ordinal(employ_datQ44.6.a$CatOutcome, employ_datQ44.6.a$Academic, employ_datQ44.6.a)
prep <- analysisPrep(employ_datQ44.6.a$CatOutcome, employ_datQ44.6.a$Academic, employ_datQ44.6.a)
analysis <- polr(employ_datQ44.6.a$CatOutcome ~ employ_datQ44.6.a$Academic, data=employ_datQ44.6.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.6.aCollege<-multidatClean(Q44.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.6.a + Q3, data = employ_datQ44.6.aCollege)
conTable
detach(dat_long)
employ_datQ44.6.aCollege$Q3[(employ_datQ44.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.6.aCollege$Q3<- factor(employ_datQ44.6.aCollege$Q3)
employ_datQ44.6.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.6.aCollege$Q44.6.a, employ_datQ44.6.aCollege)
#ordinal(employ_datQ44.6.aCollege$CatOutcome, employ_datQ44.6.aCollege$Q3, employ_datQ44.6.aCollege)
prep <- analysisPrep(employ_datQ44.6.aCollege$CatOutcome, employ_datQ44.6.aCollege$Q3, employ_datQ44.6.aCollege)
analysis <- polr(employ_datQ44.6.aCollege$CatOutcome ~ employ_datQ44.6.aCollege$Q3, data=employ_datQ44.6.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.6.aCarer<-multidatClean(Q44.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aCarer$Carer<- factor(employ_datQ44.6.aCarer$Carer)
employ_datQ44.6.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.6.aCarer$Q44.6.a, employ_datQ44.6.aCarer)
#ordinal(employ_datQ44.6.aCarer$CatOutcome, employ_datQ44.6.aCarer$Carer, employ_datQ44.6.aCarer)
prep <- analysisPrep(employ_datQ44.6.aCarer$CatOutcome, employ_datQ44.6.aCarer$Carer, employ_datQ44.6.aCarer)
analysis <- polr(employ_datQ44.6.aCarer$CatOutcome ~ employ_datQ44.6.aCarer$Carer, data=employ_datQ44.6.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.6.aDisability<-multidatClean(Q44.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aDisability$Disability<- factor(employ_datQ44.6.aDisability$Disability)
employ_datQ44.6.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.6.aDisability$Q44.6.a, employ_datQ44.6.aDisability)
conTable <- xtabs(~Q44.6.a + Disability, data = employ_datQ44.6.aDisability)
#ordinal(employ_datQ44.6.aDisability$CatOutcome, employ_datQ44.6.aDisability$Disability, employ_datQ44.6.aDisability)
prep <- analysisPrep(employ_datQ44.6.aDisability$CatOutcome, employ_datQ44.6.aDisability$Disability, employ_datQ44.6.aDisability)
analysis <- polr(employ_datQ44.6.aDisability$CatOutcome ~ employ_datQ44.6.aDisability$Disability, data=employ_datQ44.6.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.6.aEthnicity<-multidatClean(Q44.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aEthnicity$Ethnicity<- factor(employ_datQ44.6.aEthnicity$EthnicityCleaned)
employ_datQ44.6.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.6.aEthnicity$Q44.6.a, employ_datQ44.6.aEthnicity)
conTable <- xtabs(~Q44.6.a + EthnicityCleaned, data = employ_datQ44.6.aEthnicity)
conTable
#ordinal(employ_datQ44.6.aEthnicity$CatOutcome, employ_datQ44.6.aEthnicity$EthnicityCleaned, employ_datQ44.6.aEthnicity)
prep <- analysisPrep(employ_datQ44.6.aEthnicity$CatOutcome, employ_datQ44.6.aEthnicity$EthnicityCleaned, employ_datQ44.6.aEthnicity)
analysis <- polr(employ_datQ44.6.aEthnicity$CatOutcome ~ employ_datQ44.6.aEthnicity$EthnicityCleaned, data=employ_datQ44.6.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.6.aFirstGen<-multidatClean(Q44.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aFirstGen$FirstGen<-factor(employ_datQ44.6.aFirstGen$FirstGen)
employ_datQ44.6.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.6.aFirstGen$Q44.6.a, employ_datQ44.6.aFirstGen)
#ordinal(employ_datQ44.6.aFirstGen$CatOutcome, employ_datQ44.6.aFirstGen$FirstGen, employ_datQ44.6.aFirstGen)
prep <- analysisPrep(employ_datQ44.6.aFirstGen$CatOutcome, employ_datQ44.6.aFirstGen$FirstGen, employ_datQ44.6.aFirstGen)
analysis <- polr(employ_datQ44.6.aFirstGen$CatOutcome ~ employ_datQ44.6.aFirstGen$FirstGen, data=employ_datQ44.6.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.6.aGender<-multidatClean(Q44.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aGender$Gender<-factor(employ_datQ44.6.aGender$Gender)
employ_datQ44.6.aGender<-ResponsibilityordinaldatClean(employ_datQ44.6.aGender$Q44.6.a, employ_datQ44.6.aGender)
#ordinal(employ_datQ44.6.aGender$CatOutcome, employ_datQ44.6.aGender$Gender, employ_datQ44.6.aGender)
prep <- analysisPrep(employ_datQ44.6.aGender$CatOutcome, employ_datQ44.6.aGender$Gender, employ_datQ44.6.aGender)
analysis <- polr(employ_datQ44.6.aGender$CatOutcome ~ employ_datQ44.6.aGender$Gender, data=employ_datQ44.6.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.6.aSexuality<-multidatClean(Q44.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.6.aSexuality$Sexuality<-factor(employ_datQ44.6.aSexuality$Sexuality)
employ_datQ44.6.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.6.aSexuality$Q44.6.a, employ_datQ44.6.aSexuality)
#ordinal(employ_datQ44.6.aSexuality$CatOutcome, employ_datQ44.6.aSexuality$Sexuality, employ_datQ44.6.aSexuality)
prep <- analysisPrep(employ_datQ44.6.aSexuality$CatOutcome, employ_datQ44.6.aSexuality$Sexuality, employ_datQ44.6.aSexuality)
analysis <- polr(employ_datQ44.6.aSexuality$CatOutcome ~ employ_datQ44.6.aSexuality$Sexuality, data=employ_datQ44.6.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.6.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q44
"Status"
employ_datQ44.7.a<-multidatClean(Q44.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.7.a + Academic, data = employ_datQ44.7.a)
detach(dat_long)
employ_datQ44.7.a<-ResponsibilityordinaldatClean(employ_datQ44.7.a$Q44.7.a, employ_datQ44.7.a)
#ordinal(employ_datQ44.7.a$CatOutcome, employ_datQ44.7.a$Academic, employ_datQ44.7.a)
prep <- analysisPrep(employ_datQ44.7.a$CatOutcome, employ_datQ44.7.a$Academic, employ_datQ44.7.a)
analysis <- polr(employ_datQ44.7.a$CatOutcome ~ employ_datQ44.7.a$Academic, data=employ_datQ44.7.a, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q44.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ44.7.aCollege<-multidatClean(Q44.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q44.7.a + Q3, data = employ_datQ44.7.aCollege)
conTable
detach(dat_long)
employ_datQ44.7.aCollege$Q3[(employ_datQ44.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ44.7.aCollege$Q3<- factor(employ_datQ44.7.aCollege$Q3)
employ_datQ44.7.aCollege<-ResponsibilityordinaldatClean(employ_datQ44.7.aCollege$Q44.7.a, employ_datQ44.7.aCollege)
#ordinal(employ_datQ44.7.aCollege$CatOutcome, employ_datQ44.7.aCollege$Q3, employ_datQ44.7.aCollege)
prep <- analysisPrep(employ_datQ44.7.aCollege$CatOutcome, employ_datQ44.7.aCollege$Q3, employ_datQ44.7.aCollege)
analysis <- polr(employ_datQ44.7.aCollege$CatOutcome ~ employ_datQ44.7.aCollege$Q3, data=employ_datQ44.7.aCollege, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ44.7.aCarer<-multidatClean(Q44.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aCarer$Carer<- factor(employ_datQ44.7.aCarer$Carer)
employ_datQ44.7.aCarer<-ResponsibilityordinaldatClean(employ_datQ44.7.aCarer$Q44.7.a, employ_datQ44.7.aCarer)
#ordinal(employ_datQ44.7.aCarer$CatOutcome, employ_datQ44.7.aCarer$Carer, employ_datQ44.7.aCarer)
prep <- analysisPrep(employ_datQ44.7.aCarer$CatOutcome, employ_datQ44.7.aCarer$Carer, employ_datQ44.7.aCarer)
analysis <- polr(employ_datQ44.7.aCarer$CatOutcome ~ employ_datQ44.7.aCarer$Carer, data=employ_datQ44.7.aCarer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ44.7.aDisability<-multidatClean(Q44.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aDisability$Disability<- factor(employ_datQ44.7.aDisability$Disability)
employ_datQ44.7.aDisability<-ResponsibilityordinaldatClean(employ_datQ44.7.aDisability$Q44.7.a, employ_datQ44.7.aDisability)
conTable <- xtabs(~Q44.7.a + Disability, data = employ_datQ44.7.aDisability)
#ordinal(employ_datQ44.7.aDisability$CatOutcome, employ_datQ44.7.aDisability$Disability, employ_datQ44.7.aDisability)
prep <- analysisPrep(employ_datQ44.7.aDisability$CatOutcome, employ_datQ44.7.aDisability$Disability, employ_datQ44.7.aDisability)
analysis <- polr(employ_datQ44.7.aDisability$CatOutcome ~ employ_datQ44.7.aDisability$Disability, data=employ_datQ44.7.aDisability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ44.7.aEthnicity<-multidatClean(Q44.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aEthnicity$Ethnicity<- factor(employ_datQ44.7.aEthnicity$EthnicityCleaned)
employ_datQ44.7.aEthnicity<-ResponsibilityordinaldatClean(employ_datQ44.7.aEthnicity$Q44.7.a, employ_datQ44.7.aEthnicity)
conTable <- xtabs(~Q44.7.a + EthnicityCleaned, data = employ_datQ44.7.aEthnicity)
conTable
#ordinal(employ_datQ44.7.aEthnicity$CatOutcome, employ_datQ44.7.aEthnicity$EthnicityCleaned, employ_datQ44.7.aEthnicity)
prep <- analysisPrep(employ_datQ44.7.aEthnicity$CatOutcome, employ_datQ44.7.aEthnicity$EthnicityCleaned, employ_datQ44.7.aEthnicity)
analysis <- polr(employ_datQ44.7.aEthnicity$CatOutcome ~ employ_datQ44.7.aEthnicity$EthnicityCleaned, data=employ_datQ44.7.aEthnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ44.7.aFirstGen<-multidatClean(Q44.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aFirstGen$FirstGen<-factor(employ_datQ44.7.aFirstGen$FirstGen)
employ_datQ44.7.aFirstGen<-ResponsibilityordinaldatClean(employ_datQ44.7.aFirstGen$Q44.7.a, employ_datQ44.7.aFirstGen)
#ordinal(employ_datQ44.7.aFirstGen$CatOutcome, employ_datQ44.7.aFirstGen$FirstGen, employ_datQ44.7.aFirstGen)
prep <- analysisPrep(employ_datQ44.7.aFirstGen$CatOutcome, employ_datQ44.7.aFirstGen$FirstGen, employ_datQ44.7.aFirstGen)
analysis <- polr(employ_datQ44.7.aFirstGen$CatOutcome ~ employ_datQ44.7.aFirstGen$FirstGen, data=employ_datQ44.7.aFirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ44.7.aGender<-multidatClean(Q44.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aGender$Gender<-factor(employ_datQ44.7.aGender$Gender)
employ_datQ44.7.aGender<-ResponsibilityordinaldatClean(employ_datQ44.7.aGender$Q44.7.a, employ_datQ44.7.aGender)
#ordinal(employ_datQ44.7.aGender$CatOutcome, employ_datQ44.7.aGender$Gender, employ_datQ44.7.aGender)
prep <- analysisPrep(employ_datQ44.7.aGender$CatOutcome, employ_datQ44.7.aGender$Gender, employ_datQ44.7.aGender)
analysis <- polr(employ_datQ44.7.aGender$CatOutcome ~ employ_datQ44.7.aGender$Gender, data=employ_datQ44.7.aGender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ44.7.aSexuality<-multidatClean(Q44.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ44.7.aSexuality$Sexuality<-factor(employ_datQ44.7.aSexuality$Sexuality)
employ_datQ44.7.aSexuality<-ResponsibilityordinaldatClean(employ_datQ44.7.aSexuality$Q44.7.a, employ_datQ44.7.aSexuality)
#ordinal(employ_datQ44.7.aSexuality$CatOutcome, employ_datQ44.7.aSexuality$Sexuality, employ_datQ44.7.aSexuality)
prep <- analysisPrep(employ_datQ44.7.aSexuality$CatOutcome, employ_datQ44.7.aSexuality$Sexuality, employ_datQ44.7.aSexuality)
analysis <- polr(employ_datQ44.7.aSexuality$CatOutcome ~ employ_datQ44.7.aSexuality$Sexuality, data=employ_datQ44.7.aSexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q44.7.a"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q44.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q45
"Status"
employ_datQ45<-multidatClean(Q45, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q45 + Academic, data = employ_datQ45)
detach(dat_long)
employ_datQ45<-NotsureordinaldatClean(employ_datQ45$Q45, employ_datQ45)
#ordinal(employ_datQ45$CatOutcome, employ_datQ45$Academic, employ_datQ45)
prep <- analysisPrep(employ_datQ45$CatOutcome, employ_datQ45$Academic, employ_datQ45)
analysis <- polr(employ_datQ45$CatOutcome ~ employ_datQ45$Academic, data=employ_datQ45, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q45"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
employ_datQ45College<-multidatClean(Q45, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q45 + Q3, data = employ_datQ45College)
conTable
detach(dat_long)
employ_datQ45College$Q3[(employ_datQ45College$Q3 == "Research Professional Staff")]="Other"
employ_datQ45College$Q3<- factor(employ_datQ45College$Q3)
employ_datQ45College<-NotsureordinaldatClean(employ_datQ45College$Q45, employ_datQ45College)
#ordinal(employ_datQ45College$CatOutcome, employ_datQ45College$Q3, employ_datQ45College)
prep <- analysisPrep(employ_datQ45College$CatOutcome, employ_datQ45College$Q3, employ_datQ45College)
analysis <- polr(employ_datQ45College$CatOutcome ~ employ_datQ45College$Q3, data=employ_datQ45College, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ45Carer<-multidatClean(Q45, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45Carer$Carer<- factor(employ_datQ45Carer$Carer)
employ_datQ45Carer<-NotsureordinaldatClean(employ_datQ45Carer$Q45, employ_datQ45Carer)
#ordinal(employ_datQ45Carer$CatOutcome, employ_datQ45Carer$Carer, employ_datQ45Carer)
prep <- analysisPrep(employ_datQ45Carer$CatOutcome, employ_datQ45Carer$Carer, employ_datQ45Carer)
analysis <- polr(employ_datQ45Carer$CatOutcome ~ employ_datQ45Carer$Carer, data=employ_datQ45Carer, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ45Disability<-multidatClean(Q45, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45Disability$Disability<- factor(employ_datQ45Disability$Disability)
employ_datQ45Disability<-NotsureordinaldatClean(employ_datQ45Disability$Q45, employ_datQ45Disability)
conTable <- xtabs(~Q45 + Disability, data = employ_datQ45Disability)
#ordinal(employ_datQ45Disability$CatOutcome, employ_datQ45Disability$Disability, employ_datQ45Disability)
prep <- analysisPrep(employ_datQ45Disability$CatOutcome, employ_datQ45Disability$Disability, employ_datQ45Disability)
analysis <- polr(employ_datQ45Disability$CatOutcome ~ employ_datQ45Disability$Disability, data=employ_datQ45Disability, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ45Ethnicity<-multidatClean(Q45, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45Ethnicity$Ethnicity<- factor(employ_datQ45Ethnicity$EthnicityCleaned)
employ_datQ45Ethnicity<-NotsureordinaldatClean(employ_datQ45Ethnicity$Q45, employ_datQ45Ethnicity)
conTable <- xtabs(~Q45 + EthnicityCleaned, data = employ_datQ45Ethnicity)
conTable
#ordinal(employ_datQ45Ethnicity$CatOutcome, employ_datQ45Ethnicity$EthnicityCleaned, employ_datQ45Ethnicity)
prep <- analysisPrep(employ_datQ45Ethnicity$CatOutcome, employ_datQ45Ethnicity$EthnicityCleaned, employ_datQ45Ethnicity)
analysis <- polr(employ_datQ45Ethnicity$CatOutcome ~ employ_datQ45Ethnicity$EthnicityCleaned, data=employ_datQ45Ethnicity, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ45FirstGen<-multidatClean(Q45, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45FirstGen$FirstGen<-factor(employ_datQ45FirstGen$FirstGen)
employ_datQ45FirstGen<-NotsureordinaldatClean(employ_datQ45FirstGen$Q45, employ_datQ45FirstGen)
#ordinal(employ_datQ45FirstGen$CatOutcome, employ_datQ45FirstGen$FirstGen, employ_datQ45FirstGen)
prep <- analysisPrep(employ_datQ45FirstGen$CatOutcome, employ_datQ45FirstGen$FirstGen, employ_datQ45FirstGen)
analysis <- polr(employ_datQ45FirstGen$CatOutcome ~ employ_datQ45FirstGen$FirstGen, data=employ_datQ45FirstGen, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ45Gender<-multidatClean(Q45, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45Gender$Gender<-factor(employ_datQ45Gender$Gender)
employ_datQ45Gender<-NotsureordinaldatClean(employ_datQ45Gender$Q45, employ_datQ45Gender)
#ordinal(employ_datQ45Gender$CatOutcome, employ_datQ45Gender$Gender, employ_datQ45Gender)
prep <- analysisPrep(employ_datQ45Gender$CatOutcome, employ_datQ45Gender$Gender, employ_datQ45Gender)
analysis <- polr(employ_datQ45Gender$CatOutcome ~ employ_datQ45Gender$Gender, data=employ_datQ45Gender, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ45Sexuality<-multidatClean(Q45, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ45Sexuality$Sexuality<-factor(employ_datQ45Sexuality$Sexuality)
employ_datQ45Sexuality<-NotsureordinaldatClean(employ_datQ45Sexuality$Q45, employ_datQ45Sexuality)
#ordinal(employ_datQ45Sexuality$CatOutcome, employ_datQ45Sexuality$Sexuality, employ_datQ45Sexuality)
prep <- analysisPrep(employ_datQ45Sexuality$CatOutcome, employ_datQ45Sexuality$Sexuality, employ_datQ45Sexuality)
analysis <- polr(employ_datQ45Sexuality$CatOutcome ~ employ_datQ45Sexuality$Sexuality, data=employ_datQ45Sexuality, Hess=TRUE)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q45"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q45_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)






