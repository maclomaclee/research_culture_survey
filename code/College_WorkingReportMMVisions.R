source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3,239,164:183, 6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)

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
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

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

write.csv(OR_Outcomes, "Visions_college_PhD.csv")
