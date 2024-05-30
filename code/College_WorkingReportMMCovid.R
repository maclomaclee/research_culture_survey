source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3,239,184:222,6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)

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
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

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

write.csv(OR_Outcomes, "Covid_college_PhD.csv")