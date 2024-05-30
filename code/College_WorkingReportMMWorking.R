source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3, 239,22:59, 6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)

employ_datQ13.1.aCollege<-multidatClean(Q13.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.1.a + Q3, data = employ_datQ13.1.aCollege)
conTable
detach(dat_long)
employ_datQ13.1.aCollege$Q3[(employ_datQ13.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.1.aCollege$Q3<- factor(employ_datQ13.1.aCollege$Q3)
employ_datQ13.1.aCollege<-ordinaldatClean(employ_datQ13.1.aCollege$Q13.1.a, employ_datQ13.1.aCollege)
#ordinal(employ_datQ13.1.aCollege$CatOutcome, employ_datQ13.1.aCollege$Q3, employ_datQ13.1.aCollege)
prep <- analysisPrep(employ_datQ13.1.aCollege$CatOutcome, employ_datQ13.1.aCollege$Q3, employ_datQ13.1.aCollege)
analysis <- polr(employ_datQ13.1.aCollege$CatOutcome ~ employ_datQ13.1.aCollege$Q3, data=employ_datQ13.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.1.a"
Dimension <- "College"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

employ_datQ13.2.aCollege<-multidatClean(Q13.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.2.a + Q3, data = employ_datQ13.2.aCollege)
conTable
detach(dat_long)
employ_datQ13.2.aCollege$Q3[(employ_datQ13.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.2.aCollege$Q3<- factor(employ_datQ13.2.aCollege$Q3)
employ_datQ13.2.aCollege<-ordinaldatClean(employ_datQ13.2.aCollege$Q13.2.a, employ_datQ13.2.aCollege)
#ordinal(employ_datQ13.2.aCollege$CatOutcome, employ_datQ13.2.aCollege$Q3, employ_datQ13.2.aCollege)
prep <- analysisPrep(employ_datQ13.2.aCollege$CatOutcome, employ_datQ13.2.aCollege$Q3, employ_datQ13.2.aCollege)
analysis <- polr(employ_datQ13.2.aCollege$CatOutcome ~ employ_datQ13.2.aCollege$Q3, data=employ_datQ13.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

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

employ_datQ13.5.aCollege<-multidatClean(Q13.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q13.5.a + Q3, data = employ_datQ13.5.aCollege)
conTable
detach(dat_long)
employ_datQ13.5.aCollege$Q3[(employ_datQ13.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ13.5.aCollege$Q3<- factor(employ_datQ13.5.aCollege$Q3)
employ_datQ13.5.aCollege<-ordinaldatClean(employ_datQ13.5.aCollege$Q13.5.a, employ_datQ13.5.aCollege)
#ordinal(employ_datQ13.5.aCollege$CatOutcome, employ_datQ13.5.aCollege$Q3, employ_datQ13.5.aCollege)
prep <- analysisPrep(employ_datQ13.5.aCollege$CatOutcome, employ_datQ13.5.aCollege$Q3, employ_datQ13.5.aCollege)
analysis <- polr(employ_datQ13.5.aCollege$CatOutcome ~ employ_datQ13.5.aCollege$Q3, data=employ_datQ13.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q13.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ14.1.aCollege<-multidatClean(Q14.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q14.1.a + Q3, data = employ_datQ14.1.aCollege)
conTable
detach(dat_long)
employ_datQ14.1.aCollege$Q3[(employ_datQ14.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ14.1.aCollege$Q3<- factor(employ_datQ14.1.aCollege$Q3)
employ_datQ14.1.aCollege<-ordinaldatClean(employ_datQ14.1.aCollege$Q14.1.a, employ_datQ14.1.aCollege)
#ordinal(employ_datQ14.1.aCollege$CatOutcome, employ_datQ14.1.aCollege$Q3, employ_datQ14.1.aCollege)
prep <- analysisPrep(employ_datQ14.1.aCollege$CatOutcome, employ_datQ14.1.aCollege$Q3, employ_datQ14.1.aCollege)
analysis <- polr(employ_datQ14.1.aCollege$CatOutcome ~ employ_datQ14.1.aCollege$Q3, data=employ_datQ14.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q14.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

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

employ_datQ15.2.aCollege<-multidatClean(Q15.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q15.2.a + Q3, data = employ_datQ15.2.aCollege)
conTable
detach(dat_long)
employ_datQ15.2.aCollege$Q3[(employ_datQ15.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ15.2.aCollege$Q3<- factor(employ_datQ15.2.aCollege$Q3)
employ_datQ15.2.aCollege<-ImpordinaldatClean(employ_datQ15.2.aCollege$Q15.2.a, employ_datQ15.2.aCollege)
#ordinal(employ_datQ15.2.aCollege$CatOutcome, employ_datQ15.2.aCollege$Q3, employ_datQ15.2.aCollege)
prep <- analysisPrep(employ_datQ15.2.aCollege$CatOutcome, employ_datQ15.2.aCollege$Q3, employ_datQ15.2.aCollege)
analysis <- polr(employ_datQ15.2.aCollege$CatOutcome ~ employ_datQ15.2.aCollege$Q3, data=employ_datQ15.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q15.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

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

write.csv(OR_Outcomes, "working_college_PhD.csv")
