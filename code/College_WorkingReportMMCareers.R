source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3, 239, 60, 62:68, 70:82, 6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)

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
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

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

write.csv(OR_Outcomes, "Careers_college_PhD.csv")
