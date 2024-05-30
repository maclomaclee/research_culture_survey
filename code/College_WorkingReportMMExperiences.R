source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,3, 239,109:182, 6, 225,227,229,231:234,240:243)]
employ_dat <- subset(employ_dat, employ_dat$Q2 == "PhD Student" )
employ_dat$Q3 <- as.character(employ_dat$Q3)

employ_datQ29College<-multidatClean(Q29, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q29 + Q3, data = employ_datQ29College)
conTable
detach(dat_long)
employ_datQ29College$Q3[(employ_datQ29College$Q3 == "Research Professional Staff")]="Other"
employ_datQ29College$Q3<- factor(employ_datQ29College$Q3)
employ_datQ29College<-ordinaldatCleanbin(employ_datQ29College$Q29, employ_datQ29College)
#ordinal(employ_datQ29College$CatOutcome, employ_datQ29College$Q3, employ_datQ29College)
prep <- analysisPrep(employ_datQ29College$CatOutcome, employ_datQ29College$Q3, employ_datQ29College)
analysis <- glm(employ_datQ29College$CatOutcome ~ employ_datQ29College$Q3, family = binomial, data=employ_datQ29College)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
#ctable <- coef(summary(analysis))
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR[4,] <- OR[1,1]+OR[4,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "College"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

employ_datQ30College<-multidatClean(Q30, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q30 + Q3, data = employ_datQ30College)
conTable
detach(dat_long)
employ_datQ30College$Q3[(employ_datQ30College$Q3 == "Research Professional Staff")]="Other"
employ_datQ30College$Q3<- factor(employ_datQ30College$Q3)
employ_datQ30College<-ordinaldatCleanbin(employ_datQ30College$Q30, employ_datQ30College)
#ordinal(employ_datQ30College$CatOutcome, employ_datQ30College$Q3, employ_datQ30College)
prep <- analysisPrep(employ_datQ30College$CatOutcome, employ_datQ30College$Q3, employ_datQ30College)
analysis <- glm(employ_datQ30College$CatOutcome ~ employ_datQ30College$Q3, family = binomial, data=employ_datQ30College)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
#ctable <- coef(summary(analysis))
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR[4,] <- OR[1,1]+OR[4,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ31College<-multidatClean(Q31, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q31 + Q3, data = employ_datQ31College)
conTable
detach(dat_long)
employ_datQ31College$Q3[(employ_datQ31College$Q3 == "Research Professional Staff")]="Other"
employ_datQ31College$Q3<- factor(employ_datQ31College$Q3)
employ_datQ31College<-ordinaldatCleanbin(employ_datQ31College$Q31, employ_datQ31College)
#ordinal(employ_datQ31College$CatOutcome, employ_datQ31College$Q3, employ_datQ31College)
prep <- analysisPrep(employ_datQ31College$CatOutcome, employ_datQ31College$Q3, employ_datQ31College)
analysis <- glm(employ_datQ31College$CatOutcome ~ employ_datQ31College$Q3, family = binomial, data=employ_datQ31College)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
#ctable <- coef(summary(analysis))
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR[4,] <- OR[1,1]+OR[4,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ32College<-multidatClean(Q32, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q32 + Q3, data = employ_datQ32College)
conTable
detach(dat_long)
employ_datQ32College$Q3[(employ_datQ32College$Q3 == "Research Professional Staff")]="Other"
employ_datQ32College$Q3<- factor(employ_datQ32College$Q3)
employ_datQ32College<-ordinaldatCleanbin(employ_datQ32College$Q32, employ_datQ32College)
#ordinal(employ_datQ32College$CatOutcome, employ_datQ32College$Q3, employ_datQ32College)
prep <- analysisPrep(employ_datQ32College$CatOutcome, employ_datQ32College$Q3, employ_datQ32College)
analysis <- glm(employ_datQ32College$CatOutcome ~ employ_datQ32College$Q3, family = binomial, data=employ_datQ32College)
#assumption<-brant(analysis)
analysisSummary <- summary(analysis)
#ctable <- coef(summary(analysis))
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR[4,] <- OR[1,1]+OR[4,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ33College<-multidatClean(Q33, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q33 + Q3, data = employ_datQ33College)
conTable
detach(dat_long)
employ_datQ33College$Q3[(employ_datQ33College$Q3 == "Research Professional Staff")]="Other"
employ_datQ33College$Q3<- factor(employ_datQ33College$Q3)
employ_datQ33College<-UnsureordinaldatClean(employ_datQ33College$Q33, employ_datQ33College)
#ordinal(employ_datQ33College$CatOutcome, employ_datQ33College$Q3, employ_datQ33College)
prep <- analysisPrep(employ_datQ33College$CatOutcome, employ_datQ33College$Q3, employ_datQ33College)
analysis <- polr(employ_datQ33College$CatOutcome ~ employ_datQ33College$Q3, data=employ_datQ33College, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q33"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ34College<-multidatClean(Q34, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q34 + Q3, data = employ_datQ34College)
conTable
detach(dat_long)
employ_datQ34College$Q3[(employ_datQ34College$Q3 == "Research Professional Staff")]="Other"
employ_datQ34College$Q3<- factor(employ_datQ34College$Q3)
employ_datQ34College<-UnsureordinaldatClean(employ_datQ34College$Q34, employ_datQ34College)
#ordinal(employ_datQ34College$CatOutcome, employ_datQ34College$Q3, employ_datQ34College)
prep <- analysisPrep(employ_datQ34College$CatOutcome, employ_datQ34College$Q3, employ_datQ34College)
analysis <- polr(employ_datQ34College$CatOutcome ~ employ_datQ34College$Q3, data=employ_datQ34College, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q34"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ35College<-multidatClean(Q35, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q35 + Q3, data = employ_datQ35College)
conTable
detach(dat_long)
employ_datQ35College$Q3[(employ_datQ35College$Q3 == "Research Professional Staff")]="Other"
employ_datQ35College$Q3<- factor(employ_datQ35College$Q3)
employ_datQ35College<-UnsureordinaldatClean(employ_datQ35College$Q35, employ_datQ35College)
#ordinal(employ_datQ35College$CatOutcome, employ_datQ35College$Q3, employ_datQ35College)
prep <- analysisPrep(employ_datQ35College$CatOutcome, employ_datQ35College$Q3, employ_datQ35College)
analysis <- polr(employ_datQ35College$CatOutcome ~ employ_datQ35College$Q3, data=employ_datQ35College, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q35"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.1.aCollege<-multidatClean(Q37.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.1.a + Q3, data = employ_datQ37.1.aCollege)
conTable
detach(dat_long)
employ_datQ37.1.aCollege$Q3[(employ_datQ37.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.1.aCollege$Q3<- factor(employ_datQ37.1.aCollege$Q3)
employ_datQ37.1.aCollege<-ordinaldatClean(employ_datQ37.1.aCollege$Q37.1.a, employ_datQ37.1.aCollege)
#ordinal(employ_datQ37.1.aCollege$CatOutcome, employ_datQ37.1.aCollege$Q3, employ_datQ37.1.aCollege)
prep <- analysisPrep(employ_datQ37.1.aCollege$CatOutcome, employ_datQ37.1.aCollege$Q3, employ_datQ37.1.aCollege)
analysis <- polr(employ_datQ37.1.aCollege$CatOutcome ~ employ_datQ37.1.aCollege$Q3, data=employ_datQ37.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.2.aCollege<-multidatClean(Q37.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.2.a + Q3, data = employ_datQ37.2.aCollege)
conTable
detach(dat_long)
employ_datQ37.2.aCollege$Q3[(employ_datQ37.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.2.aCollege$Q3<- factor(employ_datQ37.2.aCollege$Q3)
employ_datQ37.2.aCollege<-ordinaldatClean(employ_datQ37.2.aCollege$Q37.2.a, employ_datQ37.2.aCollege)
#ordinal(employ_datQ37.2.aCollege$CatOutcome, employ_datQ37.2.aCollege$Q3, employ_datQ37.2.aCollege)
prep <- analysisPrep(employ_datQ37.2.aCollege$CatOutcome, employ_datQ37.2.aCollege$Q3, employ_datQ37.2.aCollege)
analysis <- polr(employ_datQ37.2.aCollege$CatOutcome ~ employ_datQ37.2.aCollege$Q3, data=employ_datQ37.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.3.aCollege<-multidatClean(Q37.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.3.a + Q3, data = employ_datQ37.3.aCollege)
conTable
detach(dat_long)
employ_datQ37.3.aCollege$Q3[(employ_datQ37.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.3.aCollege$Q3<- factor(employ_datQ37.3.aCollege$Q3)
employ_datQ37.3.aCollege<-ordinaldatClean(employ_datQ37.3.aCollege$Q37.3.a, employ_datQ37.3.aCollege)
#ordinal(employ_datQ37.3.aCollege$CatOutcome, employ_datQ37.3.aCollege$Q3, employ_datQ37.3.aCollege)
prep <- analysisPrep(employ_datQ37.3.aCollege$CatOutcome, employ_datQ37.3.aCollege$Q3, employ_datQ37.3.aCollege)
analysis <- polr(employ_datQ37.3.aCollege$CatOutcome ~ employ_datQ37.3.aCollege$Q3, data=employ_datQ37.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.4.aCollege<-multidatClean(Q37.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.4.a + Q3, data = employ_datQ37.4.aCollege)
conTable
detach(dat_long)
employ_datQ37.4.aCollege$Q3[(employ_datQ37.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.4.aCollege$Q3<- factor(employ_datQ37.4.aCollege$Q3)
employ_datQ37.4.aCollege<-ordinaldatClean(employ_datQ37.4.aCollege$Q37.4.a, employ_datQ37.4.aCollege)
#ordinal(employ_datQ37.4.aCollege$CatOutcome, employ_datQ37.4.aCollege$Q3, employ_datQ37.4.aCollege)
prep <- analysisPrep(employ_datQ37.4.aCollege$CatOutcome, employ_datQ37.4.aCollege$Q3, employ_datQ37.4.aCollege)
analysis <- polr(employ_datQ37.4.aCollege$CatOutcome ~ employ_datQ37.4.aCollege$Q3, data=employ_datQ37.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.5.aCollege<-multidatClean(Q37.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.5.a + Q3, data = employ_datQ37.5.aCollege)
conTable
detach(dat_long)
employ_datQ37.5.aCollege$Q3[(employ_datQ37.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.5.aCollege$Q3<- factor(employ_datQ37.5.aCollege$Q3)
employ_datQ37.5.aCollege<-ordinaldatCleanNegative(employ_datQ37.5.aCollege$Q37.5.a, employ_datQ37.5.aCollege)
#ordinal(employ_datQ37.5.aCollege$CatOutcome, employ_datQ37.5.aCollege$Q3, employ_datQ37.5.aCollege)
prep <- analysisPrep(employ_datQ37.5.aCollege$CatOutcome, employ_datQ37.5.aCollege$Q3, employ_datQ37.5.aCollege)
analysis <- polr(employ_datQ37.5.aCollege$CatOutcome ~ employ_datQ37.5.aCollege$Q3, data=employ_datQ37.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.6.aCollege<-multidatClean(Q37.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.6.a + Q3, data = employ_datQ37.6.aCollege)
conTable
detach(dat_long)
employ_datQ37.6.aCollege$Q3[(employ_datQ37.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.6.aCollege$Q3<- factor(employ_datQ37.6.aCollege$Q3)
employ_datQ37.6.aCollege<-ordinaldatCleanNegative(employ_datQ37.6.aCollege$Q37.6.a, employ_datQ37.6.aCollege)
#ordinal(employ_datQ37.6.aCollege$CatOutcome, employ_datQ37.6.aCollege$Q3, employ_datQ37.6.aCollege)
prep <- analysisPrep(employ_datQ37.6.aCollege$CatOutcome, employ_datQ37.6.aCollege$Q3, employ_datQ37.6.aCollege)
analysis <- polr(employ_datQ37.6.aCollege$CatOutcome ~ employ_datQ37.6.aCollege$Q3, data=employ_datQ37.6.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.7.aCollege<-multidatClean(Q37.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.7.a + Q3, data = employ_datQ37.7.aCollege)
conTable
detach(dat_long)
employ_datQ37.7.aCollege$Q3[(employ_datQ37.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.7.aCollege$Q3<- factor(employ_datQ37.7.aCollege$Q3)
employ_datQ37.7.aCollege<-ordinaldatCleanNegative(employ_datQ37.7.aCollege$Q37.7.a, employ_datQ37.7.aCollege)
#ordinal(employ_datQ37.7.aCollege$CatOutcome, employ_datQ37.7.aCollege$Q3, employ_datQ37.7.aCollege)
prep <- analysisPrep(employ_datQ37.7.aCollege$CatOutcome, employ_datQ37.7.aCollege$Q3, employ_datQ37.7.aCollege)
analysis <- polr(employ_datQ37.7.aCollege$CatOutcome ~ employ_datQ37.7.aCollege$Q3, data=employ_datQ37.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.8.aCollege<-multidatClean(Q37.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.8.a + Q3, data = employ_datQ37.8.aCollege)
conTable
detach(dat_long)
employ_datQ37.8.aCollege$Q3[(employ_datQ37.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.8.aCollege$Q3<- factor(employ_datQ37.8.aCollege$Q3)
employ_datQ37.8.aCollege<-ordinaldatClean(employ_datQ37.8.aCollege$Q37.8.a, employ_datQ37.8.aCollege)
#ordinal(employ_datQ37.8.aCollege$CatOutcome, employ_datQ37.8.aCollege$Q3, employ_datQ37.8.aCollege)
prep <- analysisPrep(employ_datQ37.8.aCollege$CatOutcome, employ_datQ37.8.aCollege$Q3, employ_datQ37.8.aCollege)
analysis <- polr(employ_datQ37.8.aCollege$CatOutcome ~ employ_datQ37.8.aCollege$Q3, data=employ_datQ37.8.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.9.aCollege<-multidatClean(Q37.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.9.a + Q3, data = employ_datQ37.9.aCollege)
conTable
detach(dat_long)
employ_datQ37.9.aCollege$Q3[(employ_datQ37.9.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.9.aCollege$Q3<- factor(employ_datQ37.9.aCollege$Q3)
employ_datQ37.9.aCollege<-ordinaldatCleanNegative(employ_datQ37.9.aCollege$Q37.9.a, employ_datQ37.9.aCollege)
#ordinal(employ_datQ37.9.aCollege$CatOutcome, employ_datQ37.9.aCollege$Q3, employ_datQ37.9.aCollege)
prep <- analysisPrep(employ_datQ37.9.aCollege$CatOutcome, employ_datQ37.9.aCollege$Q3, employ_datQ37.9.aCollege)
analysis <- polr(employ_datQ37.9.aCollege$CatOutcome ~ employ_datQ37.9.aCollege$Q3, data=employ_datQ37.9.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.9.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.10.aCollege<-multidatClean(Q37.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.10.a + Q3, data = employ_datQ37.10.aCollege)
conTable
detach(dat_long)
employ_datQ37.10.aCollege$Q3[(employ_datQ37.10.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.10.aCollege$Q3<- factor(employ_datQ37.10.aCollege$Q3)
employ_datQ37.10.aCollege<-ordinaldatClean(employ_datQ37.10.aCollege$Q37.10.a, employ_datQ37.10.aCollege)
#ordinal(employ_datQ37.10.aCollege$CatOutcome, employ_datQ37.10.aCollege$Q3, employ_datQ37.10.aCollege)
prep <- analysisPrep(employ_datQ37.10.aCollege$CatOutcome, employ_datQ37.10.aCollege$Q3, employ_datQ37.10.aCollege)
analysis <- polr(employ_datQ37.10.aCollege$CatOutcome ~ employ_datQ37.10.aCollege$Q3, data=employ_datQ37.10.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.10.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.11.aCollege<-multidatClean(Q37.11.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.11.a + Q3, data = employ_datQ37.11.aCollege)
conTable
detach(dat_long)
employ_datQ37.11.aCollege$Q3[(employ_datQ37.11.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.11.aCollege$Q3<- factor(employ_datQ37.11.aCollege$Q3)
employ_datQ37.11.aCollege<-ordinaldatClean(employ_datQ37.11.aCollege$Q37.11.a, employ_datQ37.11.aCollege)
#ordinal(employ_datQ37.11.aCollege$CatOutcome, employ_datQ37.11.aCollege$Q3, employ_datQ37.11.aCollege)
prep <- analysisPrep(employ_datQ37.11.aCollege$CatOutcome, employ_datQ37.11.aCollege$Q3, employ_datQ37.11.aCollege)
analysis <- polr(employ_datQ37.11.aCollege$CatOutcome ~ employ_datQ37.11.aCollege$Q3, data=employ_datQ37.11.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.11.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.12.aCollege<-multidatClean(Q37.12.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.12.a + Q3, data = employ_datQ37.12.aCollege)
conTable
detach(dat_long)
employ_datQ37.12.aCollege$Q3[(employ_datQ37.12.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.12.aCollege$Q3<- factor(employ_datQ37.12.aCollege$Q3)
employ_datQ37.12.aCollege<-ordinaldatClean(employ_datQ37.12.aCollege$Q37.12.a, employ_datQ37.12.aCollege)
#ordinal(employ_datQ37.12.aCollege$CatOutcome, employ_datQ37.12.aCollege$Q3, employ_datQ37.12.aCollege)
prep <- analysisPrep(employ_datQ37.12.aCollege$CatOutcome, employ_datQ37.12.aCollege$Q3, employ_datQ37.12.aCollege)
analysis <- polr(employ_datQ37.12.aCollege$CatOutcome ~ employ_datQ37.12.aCollege$Q3, data=employ_datQ37.12.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.12.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.13.aCollege<-multidatClean(Q37.13.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.13.a + Q3, data = employ_datQ37.13.aCollege)
conTable
detach(dat_long)
employ_datQ37.13.aCollege$Q3[(employ_datQ37.13.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.13.aCollege$Q3<- factor(employ_datQ37.13.aCollege$Q3)
employ_datQ37.13.aCollege<-ordinaldatClean(employ_datQ37.13.aCollege$Q37.13.a, employ_datQ37.13.aCollege)
#ordinal(employ_datQ37.13.aCollege$CatOutcome, employ_datQ37.13.aCollege$Q3, employ_datQ37.13.aCollege)
prep <- analysisPrep(employ_datQ37.13.aCollege$CatOutcome, employ_datQ37.13.aCollege$Q3, employ_datQ37.13.aCollege)
analysis <- polr(employ_datQ37.13.aCollege$CatOutcome ~ employ_datQ37.13.aCollege$Q3, data=employ_datQ37.13.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.13.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.14.aCollege<-multidatClean(Q37.14.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.14.a + Q3, data = employ_datQ37.14.aCollege)
conTable
detach(dat_long)
employ_datQ37.14.aCollege$Q3[(employ_datQ37.14.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.14.aCollege$Q3<- factor(employ_datQ37.14.aCollege$Q3)
employ_datQ37.14.aCollege<-ordinaldatClean(employ_datQ37.14.aCollege$Q37.14.a, employ_datQ37.14.aCollege)
#ordinal(employ_datQ37.14.aCollege$CatOutcome, employ_datQ37.14.aCollege$Q3, employ_datQ37.14.aCollege)
prep <- analysisPrep(employ_datQ37.14.aCollege$CatOutcome, employ_datQ37.14.aCollege$Q3, employ_datQ37.14.aCollege)
analysis <- polr(employ_datQ37.14.aCollege$CatOutcome ~ employ_datQ37.14.aCollege$Q3, data=employ_datQ37.14.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.14.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ37.15.aCollege<-multidatClean(Q37.15.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.15.a + Q3, data = employ_datQ37.15.aCollege)
conTable
detach(dat_long)
employ_datQ37.15.aCollege$Q3[(employ_datQ37.15.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ37.15.aCollege$Q3<- factor(employ_datQ37.15.aCollege$Q3)
employ_datQ37.15.aCollege<-ordinaldatCleanNegative(employ_datQ37.15.aCollege$Q37.15.a, employ_datQ37.15.aCollege)
#ordinal(employ_datQ37.15.aCollege$CatOutcome, employ_datQ37.15.aCollege$Q3, employ_datQ37.15.aCollege)
prep <- analysisPrep(employ_datQ37.15.aCollege$CatOutcome, employ_datQ37.15.aCollege$Q3, employ_datQ37.15.aCollege)
analysis <- polr(employ_datQ37.15.aCollege$CatOutcome ~ employ_datQ37.15.aCollege$Q3, data=employ_datQ37.15.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q37.15.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ38.1College<-multidatClean(Q38.1, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q38.1 + Q3, data = employ_datQ38.1College)
conTable
detach(dat_long)
employ_datQ38.1College$Q3[(employ_datQ38.1College$Q3 == "Research Professional Staff")]="Other"
employ_datQ38.1College$Q3<- factor(employ_datQ38.1College$Q3)
employ_datQ38.1College<-ordinaldatClean38(employ_datQ38.1College$Q38.1, employ_datQ38.1College)
#ordinal(employ_datQ38.1College$CatOutcome, employ_datQ38.1College$Q3, employ_datQ38.1College)
prep <- analysisPrep(employ_datQ38.1College$CatOutcome, employ_datQ38.1College$Q3, employ_datQ38.1College)
analysis <- polr(employ_datQ38.1College$CatOutcome ~ employ_datQ38.1College$Q3, data=employ_datQ38.1College, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q38.1"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ40.1.aCollege<-multidatClean(Q40.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.1.a + Q3, data = employ_datQ40.1.aCollege)
conTable
detach(dat_long)
employ_datQ40.1.aCollege$Q3[(employ_datQ40.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ40.1.aCollege$Q3<- factor(employ_datQ40.1.aCollege$Q3)
employ_datQ40.1.aCollege<-ordinaldatCleanNegative(employ_datQ40.1.aCollege$Q40.1.a, employ_datQ40.1.aCollege)
#ordinal(employ_datQ40.1.aCollege$CatOutcome, employ_datQ40.1.aCollege$Q3, employ_datQ40.1.aCollege)
prep <- analysisPrep(employ_datQ40.1.aCollege$CatOutcome, employ_datQ40.1.aCollege$Q3, employ_datQ40.1.aCollege)
analysis <- polr(employ_datQ40.1.aCollege$CatOutcome ~ employ_datQ40.1.aCollege$Q3, data=employ_datQ40.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q40.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ40.2.aCollege<-multidatClean(Q40.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.2.a + Q3, data = employ_datQ40.2.aCollege)
conTable
detach(dat_long)
employ_datQ40.2.aCollege$Q3[(employ_datQ40.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ40.2.aCollege$Q3<- factor(employ_datQ40.2.aCollege$Q3)
employ_datQ40.2.aCollege<-ordinaldatClean(employ_datQ40.2.aCollege$Q40.2.a, employ_datQ40.2.aCollege)
#ordinal(employ_datQ40.2.aCollege$CatOutcome, employ_datQ40.2.aCollege$Q3, employ_datQ40.2.aCollege)
prep <- analysisPrep(employ_datQ40.2.aCollege$CatOutcome, employ_datQ40.2.aCollege$Q3, employ_datQ40.2.aCollege)
analysis <- polr(employ_datQ40.2.aCollege$CatOutcome ~ employ_datQ40.2.aCollege$Q3, data=employ_datQ40.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q40.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ40.3.aCollege<-multidatClean(Q40.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.3.a + Q3, data = employ_datQ40.3.aCollege)
conTable
detach(dat_long)
employ_datQ40.3.aCollege$Q3[(employ_datQ40.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ40.3.aCollege$Q3<- factor(employ_datQ40.3.aCollege$Q3)
employ_datQ40.3.aCollege<-ordinaldatClean(employ_datQ40.3.aCollege$Q40.3.a, employ_datQ40.3.aCollege)
#ordinal(employ_datQ40.3.aCollege$CatOutcome, employ_datQ40.3.aCollege$Q3, employ_datQ40.3.aCollege)
prep <- analysisPrep(employ_datQ40.3.aCollege$CatOutcome, employ_datQ40.3.aCollege$Q3, employ_datQ40.3.aCollege)
analysis <- polr(employ_datQ40.3.aCollege$CatOutcome ~ employ_datQ40.3.aCollege$Q3, data=employ_datQ40.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q40.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ40.4.aCollege<-multidatClean(Q40.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.4.a + Q3, data = employ_datQ40.4.aCollege)
conTable
detach(dat_long)
employ_datQ40.4.aCollege$Q3[(employ_datQ40.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ40.4.aCollege$Q3<- factor(employ_datQ40.4.aCollege$Q3)
employ_datQ40.4.aCollege<-ordinaldatCleanNegative(employ_datQ40.4.aCollege$Q40.4.a, employ_datQ40.4.aCollege)
#ordinal(employ_datQ40.4.aCollege$CatOutcome, employ_datQ40.4.aCollege$Q3, employ_datQ40.4.aCollege)
prep <- analysisPrep(employ_datQ40.4.aCollege$CatOutcome, employ_datQ40.4.aCollege$Q3, employ_datQ40.4.aCollege)
analysis <- polr(employ_datQ40.4.aCollege$CatOutcome ~ employ_datQ40.4.aCollege$Q3, data=employ_datQ40.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q40.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ40.5.aCollege<-multidatClean(Q40.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.5.a + Q3, data = employ_datQ40.5.aCollege)
conTable
detach(dat_long)
employ_datQ40.5.aCollege$Q3[(employ_datQ40.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ40.5.aCollege$Q3<- factor(employ_datQ40.5.aCollege$Q3)
employ_datQ40.5.aCollege<-ordinaldatClean(employ_datQ40.5.aCollege$Q40.5.a, employ_datQ40.5.aCollege)
#ordinal(employ_datQ40.5.aCollege$CatOutcome, employ_datQ40.5.aCollege$Q3, employ_datQ40.5.aCollege)
prep <- analysisPrep(employ_datQ40.5.aCollege$CatOutcome, employ_datQ40.5.aCollege$Q3, employ_datQ40.5.aCollege)
analysis <- polr(employ_datQ40.5.aCollege$CatOutcome ~ employ_datQ40.5.aCollege$Q3, data=employ_datQ40.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q40.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.1.aCollege<-multidatClean(Q41.1.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.1.a + Q3, data = employ_datQ41.1.aCollege)
conTable
detach(dat_long)
employ_datQ41.1.aCollege$Q3[(employ_datQ41.1.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.1.aCollege$Q3<- factor(employ_datQ41.1.aCollege$Q3)
employ_datQ41.1.aCollege<-ordinaldatClean(employ_datQ41.1.aCollege$Q41.1.a, employ_datQ41.1.aCollege)
#ordinal(employ_datQ41.1.aCollege$CatOutcome, employ_datQ41.1.aCollege$Q3, employ_datQ41.1.aCollege)
prep <- analysisPrep(employ_datQ41.1.aCollege$CatOutcome, employ_datQ41.1.aCollege$Q3, employ_datQ41.1.aCollege)
analysis <- polr(employ_datQ41.1.aCollege$CatOutcome ~ employ_datQ41.1.aCollege$Q3, data=employ_datQ41.1.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.1.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.2.aCollege<-multidatClean(Q41.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.2.a + Q3, data = employ_datQ41.2.aCollege)
conTable
detach(dat_long)
employ_datQ41.2.aCollege$Q3[(employ_datQ41.2.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.2.aCollege$Q3<- factor(employ_datQ41.2.aCollege$Q3)
employ_datQ41.2.aCollege<-ordinaldatClean(employ_datQ41.2.aCollege$Q41.2.a, employ_datQ41.2.aCollege)
#ordinal(employ_datQ41.2.aCollege$CatOutcome, employ_datQ41.2.aCollege$Q3, employ_datQ41.2.aCollege)
prep <- analysisPrep(employ_datQ41.2.aCollege$CatOutcome, employ_datQ41.2.aCollege$Q3, employ_datQ41.2.aCollege)
analysis <- polr(employ_datQ41.2.aCollege$CatOutcome ~ employ_datQ41.2.aCollege$Q3, data=employ_datQ41.2.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.2.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.3.aCollege<-multidatClean(Q41.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.3.a + Q3, data = employ_datQ41.3.aCollege)
conTable
detach(dat_long)
employ_datQ41.3.aCollege$Q3[(employ_datQ41.3.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.3.aCollege$Q3<- factor(employ_datQ41.3.aCollege$Q3)
employ_datQ41.3.aCollege<-ordinaldatClean(employ_datQ41.3.aCollege$Q41.3.a, employ_datQ41.3.aCollege)
#ordinal(employ_datQ41.3.aCollege$CatOutcome, employ_datQ41.3.aCollege$Q3, employ_datQ41.3.aCollege)
prep <- analysisPrep(employ_datQ41.3.aCollege$CatOutcome, employ_datQ41.3.aCollege$Q3, employ_datQ41.3.aCollege)
analysis <- polr(employ_datQ41.3.aCollege$CatOutcome ~ employ_datQ41.3.aCollege$Q3, data=employ_datQ41.3.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.3.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.4.aCollege<-multidatClean(Q41.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.4.a + Q3, data = employ_datQ41.4.aCollege)
conTable
detach(dat_long)
employ_datQ41.4.aCollege$Q3[(employ_datQ41.4.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.4.aCollege$Q3<- factor(employ_datQ41.4.aCollege$Q3)
employ_datQ41.4.aCollege<-ordinaldatCleanNegative(employ_datQ41.4.aCollege$Q41.4.a, employ_datQ41.4.aCollege)
#ordinal(employ_datQ41.4.aCollege$CatOutcome, employ_datQ41.4.aCollege$Q3, employ_datQ41.4.aCollege)
prep <- analysisPrep(employ_datQ41.4.aCollege$CatOutcome, employ_datQ41.4.aCollege$Q3, employ_datQ41.4.aCollege)
analysis <- polr(employ_datQ41.4.aCollege$CatOutcome ~ employ_datQ41.4.aCollege$Q3, data=employ_datQ41.4.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.4.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.5.aCollege<-multidatClean(Q41.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.5.a + Q3, data = employ_datQ41.5.aCollege)
conTable
detach(dat_long)
employ_datQ41.5.aCollege$Q3[(employ_datQ41.5.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.5.aCollege$Q3<- factor(employ_datQ41.5.aCollege$Q3)
employ_datQ41.5.aCollege<-ordinaldatCleanNegative(employ_datQ41.5.aCollege$Q41.5.a, employ_datQ41.5.aCollege)
#ordinal(employ_datQ41.5.aCollege$CatOutcome, employ_datQ41.5.aCollege$Q3, employ_datQ41.5.aCollege)
prep <- analysisPrep(employ_datQ41.5.aCollege$CatOutcome, employ_datQ41.5.aCollege$Q3, employ_datQ41.5.aCollege)
analysis <- polr(employ_datQ41.5.aCollege$CatOutcome ~ employ_datQ41.5.aCollege$Q3, data=employ_datQ41.5.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.5.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.6.aCollege<-multidatClean(Q41.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.6.a + Q3, data = employ_datQ41.6.aCollege)
conTable
detach(dat_long)
employ_datQ41.6.aCollege$Q3[(employ_datQ41.6.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.6.aCollege$Q3<- factor(employ_datQ41.6.aCollege$Q3)
employ_datQ41.6.aCollege<-ordinaldatCleanNegative(employ_datQ41.6.aCollege$Q41.6.a, employ_datQ41.6.aCollege)
#ordinal(employ_datQ41.6.aCollege$CatOutcome, employ_datQ41.6.aCollege$Q3, employ_datQ41.6.aCollege)
prep <- analysisPrep(employ_datQ41.6.aCollege$CatOutcome, employ_datQ41.6.aCollege$Q3, employ_datQ41.6.aCollege)
analysis <- polr(employ_datQ41.6.aCollege$CatOutcome ~ employ_datQ41.6.aCollege$Q3, data=employ_datQ41.6.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.6.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.7.aCollege<-multidatClean(Q41.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.7.a + Q3, data = employ_datQ41.7.aCollege)
conTable
detach(dat_long)
employ_datQ41.7.aCollege$Q3[(employ_datQ41.7.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.7.aCollege$Q3<- factor(employ_datQ41.7.aCollege$Q3)
employ_datQ41.7.aCollege<-ordinaldatClean(employ_datQ41.7.aCollege$Q41.7.a, employ_datQ41.7.aCollege)
#ordinal(employ_datQ41.7.aCollege$CatOutcome, employ_datQ41.7.aCollege$Q3, employ_datQ41.7.aCollege)
prep <- analysisPrep(employ_datQ41.7.aCollege$CatOutcome, employ_datQ41.7.aCollege$Q3, employ_datQ41.7.aCollege)
analysis <- polr(employ_datQ41.7.aCollege$CatOutcome ~ employ_datQ41.7.aCollege$Q3, data=employ_datQ41.7.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.7.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.8.aCollege<-multidatClean(Q41.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.8.a + Q3, data = employ_datQ41.8.aCollege)
conTable
detach(dat_long)
employ_datQ41.8.aCollege$Q3[(employ_datQ41.8.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.8.aCollege$Q3<- factor(employ_datQ41.8.aCollege$Q3)
employ_datQ41.8.aCollege<-ordinaldatClean(employ_datQ41.8.aCollege$Q41.8.a, employ_datQ41.8.aCollege)
#ordinal(employ_datQ41.8.aCollege$CatOutcome, employ_datQ41.8.aCollege$Q3, employ_datQ41.8.aCollege)
prep <- analysisPrep(employ_datQ41.8.aCollege$CatOutcome, employ_datQ41.8.aCollege$Q3, employ_datQ41.8.aCollege)
analysis <- polr(employ_datQ41.8.aCollege$CatOutcome ~ employ_datQ41.8.aCollege$Q3, data=employ_datQ41.8.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.8.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.9.aCollege<-multidatClean(Q41.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.9.a + Q3, data = employ_datQ41.9.aCollege)
conTable
detach(dat_long)
employ_datQ41.9.aCollege$Q3[(employ_datQ41.9.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.9.aCollege$Q3<- factor(employ_datQ41.9.aCollege$Q3)
employ_datQ41.9.aCollege<-ordinaldatClean(employ_datQ41.9.aCollege$Q41.9.a, employ_datQ41.9.aCollege)
#ordinal(employ_datQ41.9.aCollege$CatOutcome, employ_datQ41.9.aCollege$Q3, employ_datQ41.9.aCollege)
prep <- analysisPrep(employ_datQ41.9.aCollege$CatOutcome, employ_datQ41.9.aCollege$Q3, employ_datQ41.9.aCollege)
analysis <- polr(employ_datQ41.9.aCollege$CatOutcome ~ employ_datQ41.9.aCollege$Q3, data=employ_datQ41.9.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.9.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

employ_datQ41.10.aCollege<-multidatClean(Q41.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.10.a + Q3, data = employ_datQ41.10.aCollege)
conTable
detach(dat_long)
employ_datQ41.10.aCollege$Q3[(employ_datQ41.10.aCollege$Q3 == "Research Professional Staff")]="Other"
employ_datQ41.10.aCollege$Q3<- factor(employ_datQ41.10.aCollege$Q3)
employ_datQ41.10.aCollege<-ordinaldatCleanNegative(employ_datQ41.10.aCollege$Q41.10.a, employ_datQ41.10.aCollege)
#ordinal(employ_datQ41.10.aCollege$CatOutcome, employ_datQ41.10.aCollege$Q3, employ_datQ41.10.aCollege)
prep <- analysisPrep(employ_datQ41.10.aCollege$CatOutcome, employ_datQ41.10.aCollege$Q3, employ_datQ41.10.aCollege)
analysis <- polr(employ_datQ41.10.aCollege$CatOutcome ~ employ_datQ41.10.aCollege$Q3, data=employ_datQ41.10.aCollege, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q41.10.a"
Dimension <- "College"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

write.csv(OR_Outcomes, "Experiences_college_PhD.csv")
