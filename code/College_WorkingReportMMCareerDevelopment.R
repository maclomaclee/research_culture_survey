source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3, 239,82:91, 6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)
employ_dat$Q23 <- as.character(employ_dat$Q23)
employ_dat$Academic <- as.character(employ_dat$Academic)
employ_dat$UniqueResponseNumber <- as.character(employ_dat$UniqueResponseNumber)### Q23

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
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)


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

employ_datQ28College<-multidatClean(Q28, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q28 + Q3, data = employ_datQ28College)
conTable
detach(dat_long)
employ_datQ28College$Q3[(employ_datQ28College$Q3 == "Research Professional Staff")]="Other"
employ_datQ28College$Q3<- factor(employ_datQ28College$Q3)
employ_datQ28College<-PerformordinaldatClean(employ_datQ28College$Q28, employ_datQ28College)
ordinal(employ_datQ28College$CatOutcome, employ_datQ28College$Q3, employ_datQ28College)
detach(data)

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

write.csv(OR_Outcomes, "Careerdeve_college_PhD.csv")



