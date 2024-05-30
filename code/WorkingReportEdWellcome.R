source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1:243)]
EdWelcome_dat_Work<-EdWelcome_dat[c(1:150)]

EdWelcome_datQ13.1<-multidatCleanWelcome(Q13.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q13.1.a + EdinvsWelcome, data = EdWelcome_datQ13.1)
conTable
detach(dat_long)
EdWelcome_datQ13.1$Q13.1.a[(EdWelcome_datQ13.1$Q13.1.a == "8")]=NA
EdWelcome_datQ13.1<-ordinaldatWelcomeClean(EdWelcome_datQ13.1)
EdWelcome_datQ13.1$EdinvsWelcome<-factor(EdWelcome_datQ13.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ13.1$Q13.1.a, EdWelcome_datQ13.1$EdinvsWelcome, EdWelcome_datQ13.1)
analysis <- polr(EdWelcome_datQ13.1$Q13.1.a ~ EdWelcome_datQ13.1$EdinvsWelcome, EdWelcome_datQ13.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.1.a"
OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(OR_Outcomes) <-(c("Question", "OR", "l95", "u95"))
detach(data)

EdWelcome_datQ13.2<-multidatCleanWelcome(Q13.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q13.2.a + EdinvsWelcome, data = EdWelcome_datQ13.2)
conTable
detach(dat_long)
EdWelcome_datQ13.2$Q13.2.a[(EdWelcome_datQ13.2$Q13.2.a == "8")]=NA
EdWelcome_datQ13.2<-ordinaldatWelcomeClean(EdWelcome_datQ13.2)
EdWelcome_datQ13.2$EdinvsWelcome<-factor(EdWelcome_datQ13.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ13.2$Q13.2.a, EdWelcome_datQ13.2$EdinvsWelcome, EdWelcome_datQ13.2)
analysis <- polr(EdWelcome_datQ13.2$Q13.2.a ~ EdWelcome_datQ13.2$EdinvsWelcome, EdWelcome_datQ13.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ13.3<-multidatCleanWelcome(Q13.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q13.3.a + EdinvsWelcome, data = EdWelcome_datQ13.3)
conTable
detach(dat_long)
EdWelcome_datQ13.3$Q13.3.a[(EdWelcome_datQ13.3$Q13.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ13.3<-ordinaldatWelcomeClean(EdWelcome_datQ13.3)
EdWelcome_datQ13.3$EdinvsWelcome<-factor(EdWelcome_datQ13.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ13.3$Q13.3.a, EdWelcome_datQ13.3$EdinvsWelcome, EdWelcome_datQ13.3)
analysis <- polr(EdWelcome_datQ13.3$Q13.3.a ~ EdWelcome_datQ13.3$EdinvsWelcome, EdWelcome_datQ13.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ13.4<-multidatCleanWelcome(Q13.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q13.4.a + EdinvsWelcome, data = EdWelcome_datQ13.4)
conTable
detach(dat_long)
EdWelcome_datQ13.4$Q13.4.a[(EdWelcome_datQ13.4$Q13.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ13.4<-ordinaldatWelcomeClean(EdWelcome_datQ13.4, reverse=TRUE)
EdWelcome_datQ13.4$EdinvsWelcome<-factor(EdWelcome_datQ13.4$EdinvsWelcome)
EdWelcome_datQ13.4$Q13.4.a<-factor(EdWelcome_datQ13.4$Q13.4.a)
prep <- analysisPrep(EdWelcome_datQ13.4$Q13.4.a, EdWelcome_datQ13.4$EdinvsWelcome, EdWelcome_datQ13.4)
analysis <- polr(EdWelcome_datQ13.4$Q13.4.a ~ EdWelcome_datQ13.4$EdinvsWelcome, EdWelcome_datQ13.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ13.5<-multidatCleanWelcome(Q13.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q13.5.a + EdinvsWelcome, data = EdWelcome_datQ13.5)
conTable
detach(dat_long)
EdWelcome_datQ13.5$Q13.5.a[(EdWelcome_datQ13.5$Q13.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ13.5<-ordinaldatWelcomeClean(EdWelcome_datQ13.5, reverse=TRUE)
EdWelcome_datQ13.5$EdinvsWelcome<-factor(EdWelcome_datQ13.5$EdinvsWelcome)
EdWelcome_datQ13.5$Q13.5.a<-factor(EdWelcome_datQ13.5$Q13.5.a)
prep <- analysisPrep(EdWelcome_datQ13.5$Q13.5.a, EdWelcome_datQ13.5$EdinvsWelcome, EdWelcome_datQ13.5)
analysis <- polr(EdWelcome_datQ13.5$Q13.5.a ~ EdWelcome_datQ13.5$EdinvsWelcome, EdWelcome_datQ13.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q13.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```
EdWelcome_datQ14.1<-multidatCleanWelcome(Q14.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q14.1.a + EdinvsWelcome, data = EdWelcome_datQ14.1)
conTable
detach(dat_long)
EdWelcome_datQ14.1$Q14.1.a[(EdWelcome_datQ14.1$Q14.1.a == "8")]=NA
EdWelcome_datQ14.1<-ordinaldatWelcomeClean(EdWelcome_datQ14.1)
EdWelcome_datQ14.1$EdinvsWelcome<-factor(EdWelcome_datQ14.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ14.1$Q14.1.a, EdWelcome_datQ14.1$EdinvsWelcome, EdWelcome_datQ14.1)
analysis <- polr(EdWelcome_datQ14.1$Q14.1.a ~ EdWelcome_datQ14.1$EdinvsWelcome, EdWelcome_datQ14.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q14.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ14.2<-multidatCleanWelcome(Q14.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q14.2.a + EdinvsWelcome, data = EdWelcome_datQ14.2)
conTable
detach(dat_long)
EdWelcome_datQ14.2$Q14.2.a[(EdWelcome_datQ14.2$Q14.2.a == "8")]=NA
EdWelcome_datQ14.2<-ordinaldatWelcomeClean(EdWelcome_datQ14.2)
EdWelcome_datQ14.2$EdinvsWelcome<-factor(EdWelcome_datQ14.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ14.2$Q14.2.a, EdWelcome_datQ14.2$EdinvsWelcome, EdWelcome_datQ14.2)
analysis <- polr(EdWelcome_datQ14.2$Q14.2.a ~ EdWelcome_datQ14.2$EdinvsWelcome, EdWelcome_datQ14.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q14.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ14.3<-multidatCleanWelcome(Q14.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q14.3.a + EdinvsWelcome, data = EdWelcome_datQ14.3)
conTable
detach(dat_long)
EdWelcome_datQ14.3$Q14.3.a[(EdWelcome_datQ14.3$Q14.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ14.3<-ordinaldatWelcomeClean(EdWelcome_datQ14.3)
EdWelcome_datQ14.3$EdinvsWelcome<-factor(EdWelcome_datQ14.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ14.3$Q14.3.a, EdWelcome_datQ14.3$EdinvsWelcome, EdWelcome_datQ14.3)
analysis <- polr(EdWelcome_datQ14.3$Q14.3.a ~ EdWelcome_datQ14.3$EdinvsWelcome, EdWelcome_datQ14.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q14.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ15.1<-multidatCleanWelcome(Q15.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q15.1.a + EdinvsWelcome, data = EdWelcome_datQ15.1)
conTable
detach(dat_long)
EdWelcome_datQ15.1$Q15.1.a[(EdWelcome_datQ15.1$Q15.1.a == "8")]=NA
EdWelcome_datQ15.1<-ImpordinaldatWelcomeClean(EdWelcome_datQ15.1)
EdWelcome_datQ15.1$EdinvsWelcome<-factor(EdWelcome_datQ15.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ15.1$Q15.1.a, EdWelcome_datQ15.1$EdinvsWelcome, EdWelcome_datQ15.1)
analysis <- polr(EdWelcome_datQ15.1$Q15.1.a ~ EdWelcome_datQ15.1$EdinvsWelcome, EdWelcome_datQ15.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ15.2<-multidatCleanWelcome(Q15.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q15.2.a + EdinvsWelcome, data = EdWelcome_datQ15.2)
conTable
detach(dat_long)
EdWelcome_datQ15.2$Q15.2.a[(EdWelcome_datQ15.2$Q15.2.a == "8")]=NA
EdWelcome_datQ15.2<-ImpordinaldatWelcomeClean(EdWelcome_datQ15.2)
EdWelcome_datQ15.2$EdinvsWelcome<-factor(EdWelcome_datQ15.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ15.2$Q15.2.a, EdWelcome_datQ15.2$EdinvsWelcome, EdWelcome_datQ15.2)
analysis <- polr(EdWelcome_datQ15.2$Q15.2.a ~ EdWelcome_datQ15.2$EdinvsWelcome, EdWelcome_datQ15.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ15.3<-multidatCleanWelcome(Q15.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q15.3.a + EdinvsWelcome, data = EdWelcome_datQ15.3)
conTable
detach(dat_long)
EdWelcome_datQ15.3$Q15.3.a[(EdWelcome_datQ15.3$Q15.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ15.3<-ImpordinaldatWelcomeClean(EdWelcome_datQ15.3)
EdWelcome_datQ15.3$EdinvsWelcome<-factor(EdWelcome_datQ15.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ15.3$Q15.3.a, EdWelcome_datQ15.3$EdinvsWelcome, EdWelcome_datQ15.3)
analysis <- polr(EdWelcome_datQ15.3$Q15.3.a ~ EdWelcome_datQ15.3$EdinvsWelcome, EdWelcome_datQ15.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ15.4<-multidatCleanWelcome(Q15.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q15.4.a + EdinvsWelcome, data = EdWelcome_datQ15.4)
conTable
detach(dat_long)
EdWelcome_datQ15.4$Q15.4.a[(EdWelcome_datQ15.4$Q15.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ15.4<-ImpordinaldatWelcomeClean(EdWelcome_datQ15.4)
EdWelcome_datQ15.4$EdinvsWelcome<-factor(EdWelcome_datQ15.4$EdinvsWelcome)
EdWelcome_datQ15.4$Q15.4.a<-factor(EdWelcome_datQ15.4$Q15.4.a)
prep <- analysisPrep(EdWelcome_datQ15.4$Q15.4.a, EdWelcome_datQ15.4$EdinvsWelcome, EdWelcome_datQ15.4)
analysis <- polr(EdWelcome_datQ15.4$Q15.4.a ~ EdWelcome_datQ15.4$EdinvsWelcome, EdWelcome_datQ15.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q15.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ16.1<-multidatCleanWelcome(Q16.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q16.1.a + EdinvsWelcome, data = EdWelcome_datQ16.1)
conTable
detach(dat_long)
EdWelcome_datQ16.1$Q16.1.a[(EdWelcome_datQ16.1$Q16.1.a == "8")]=NA
EdWelcome_datQ16.1<-SuccessdatWelcomeClean(EdWelcome_datQ16.1)
EdWelcome_datQ16.1$EdinvsWelcome<-factor(EdWelcome_datQ16.1$EdinvsWelcome)
EdWelcome_datQ16.1$Q16.1.a<-factor(EdWelcome_datQ16.1$Q16.1.a)
prep <- analysisPrep(EdWelcome_datQ16.1$Q16.1.a, EdWelcome_datQ16.1$EdinvsWelcome, EdWelcome_datQ16.1)
analysis <- polr(EdWelcome_datQ16.1$Q16.1.a ~ EdWelcome_datQ16.1$EdinvsWelcome, EdWelcome_datQ16.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ16.2<-multidatCleanWelcome(Q16.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q16.2.a + EdinvsWelcome, data = EdWelcome_datQ16.2)
conTable
detach(dat_long)
EdWelcome_datQ16.2$Q16.2.a[(EdWelcome_datQ16.2$Q16.2.a == "8")]=NA
EdWelcome_datQ16.2<-SuccessdatWelcomeClean(EdWelcome_datQ16.2)
EdWelcome_datQ16.2$EdinvsWelcome<-factor(EdWelcome_datQ16.2$EdinvsWelcome)
EdWelcome_datQ16.2$Q16.2.a<-factor(EdWelcome_datQ16.2$Q16.2.a)
prep <- analysisPrep(EdWelcome_datQ16.2$Q16.2.a, EdWelcome_datQ16.2$EdinvsWelcome, EdWelcome_datQ16.2)
analysis <- polr(EdWelcome_datQ16.2$Q16.2.a ~ EdWelcome_datQ16.2$EdinvsWelcome, EdWelcome_datQ16.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ16.3<-multidatCleanWelcome(Q16.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q16.3.a + EdinvsWelcome, data = EdWelcome_datQ16.3)
conTable
detach(dat_long)
EdWelcome_datQ16.3$Q16.3.a[(EdWelcome_datQ16.3$Q16.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ16.3<-SuccessdatWelcomeClean(EdWelcome_datQ16.3)
EdWelcome_datQ16.3$EdinvsWelcome<-factor(EdWelcome_datQ16.3$EdinvsWelcome)
EdWelcome_datQ16.3$Q16.3.a<-factor(EdWelcome_datQ16.3$Q16.3.a)
prep <- analysisPrep(EdWelcome_datQ16.3$Q16.3.a, EdWelcome_datQ16.3$EdinvsWelcome, EdWelcome_datQ16.3)
analysis <- polr(EdWelcome_datQ16.3$Q16.3.a ~ EdWelcome_datQ16.3$EdinvsWelcome, EdWelcome_datQ16.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ16.4<-multidatCleanWelcome(Q16.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q16.4.a + EdinvsWelcome, data = EdWelcome_datQ16.4)
conTable
detach(dat_long)
EdWelcome_datQ16.4$Q16.4.a[(EdWelcome_datQ16.4$Q16.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ16.4<-SuccessdatWelcomeClean(EdWelcome_datQ16.4)
EdWelcome_datQ16.4$EdinvsWelcome<-factor(EdWelcome_datQ16.4$EdinvsWelcome)
EdWelcome_datQ16.4$Q16.4.a<-factor(EdWelcome_datQ16.4$Q16.4.a)
prep <- analysisPrep(EdWelcome_datQ16.4$Q16.4.a, EdWelcome_datQ16.4$EdinvsWelcome, EdWelcome_datQ16.4)
analysis <- polr(EdWelcome_datQ16.4$Q16.4.a ~ EdWelcome_datQ16.4$EdinvsWelcome, EdWelcome_datQ16.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q16.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ17.1<-multidatCleanWelcome(Q17.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q17.1.a + EdinvsWelcome, data = EdWelcome_datQ17.1)
conTable
detach(dat_long)
EdWelcome_datQ17.1$Q17.1.a[(EdWelcome_datQ17.1$Q17.1.a == "8")]=NA
EdWelcome_datQ17.1<-SuccessdatWelcomeClean(EdWelcome_datQ17.1)
EdWelcome_datQ17.1$EdinvsWelcome<-factor(EdWelcome_datQ17.1$EdinvsWelcome)
EdWelcome_datQ17.1$Q17.1.a<-factor(EdWelcome_datQ17.1$Q17.1.a)
prep <- analysisPrep(EdWelcome_datQ17.1$Q17.1.a, EdWelcome_datQ17.1$EdinvsWelcome, EdWelcome_datQ17.1)
analysis <- polr(EdWelcome_datQ17.1$Q17.1.a ~ EdWelcome_datQ17.1$EdinvsWelcome, EdWelcome_datQ17.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ17.2<-multidatCleanWelcome(Q17.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q17.2.a + EdinvsWelcome, data = EdWelcome_datQ17.2)
conTable
detach(dat_long)
EdWelcome_datQ17.2$Q17.2.a[(EdWelcome_datQ17.2$Q17.2.a == "8")]=NA
EdWelcome_datQ17.2<-SuccessdatWelcomeClean(EdWelcome_datQ17.2)
EdWelcome_datQ17.2$EdinvsWelcome<-factor(EdWelcome_datQ17.2$EdinvsWelcome)
EdWelcome_datQ17.2$Q17.2.a<-factor(EdWelcome_datQ17.2$Q17.2.a)
prep <- analysisPrep(EdWelcome_datQ17.2$Q17.2.a, EdWelcome_datQ17.2$EdinvsWelcome, EdWelcome_datQ17.2)
analysis <- polr(EdWelcome_datQ17.2$Q17.2.a ~ EdWelcome_datQ17.2$EdinvsWelcome, EdWelcome_datQ17.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ17.3<-multidatCleanWelcome(Q17.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q17.3.a + EdinvsWelcome, data = EdWelcome_datQ17.3)
conTable
detach(dat_long)
EdWelcome_datQ17.3$Q17.3.a[(EdWelcome_datQ17.3$Q17.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ17.3<-SuccessdatWelcomeClean(EdWelcome_datQ17.3)
EdWelcome_datQ17.3$EdinvsWelcome<-factor(EdWelcome_datQ17.3$EdinvsWelcome)
EdWelcome_datQ17.3$Q17.3.a<-factor(EdWelcome_datQ17.3$Q17.3.a)
prep <- analysisPrep(EdWelcome_datQ17.3$Q17.3.a, EdWelcome_datQ17.3$EdinvsWelcome, EdWelcome_datQ17.3)
analysis <- polr(EdWelcome_datQ17.3$Q17.3.a ~ EdWelcome_datQ17.3$EdinvsWelcome, EdWelcome_datQ17.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ17.4<-multidatCleanWelcome(Q17.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q17.4.a + EdinvsWelcome, data = EdWelcome_datQ17.4)
conTable
detach(dat_long)
EdWelcome_datQ17.4$Q17.4.a[(EdWelcome_datQ17.4$Q17.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ17.4<-SuccessdatWelcomeClean(EdWelcome_datQ17.4)
EdWelcome_datQ17.4$EdinvsWelcome<-factor(EdWelcome_datQ17.4$EdinvsWelcome)
EdWelcome_datQ17.4$Q17.4.a<-factor(EdWelcome_datQ17.4$Q17.4.a)
prep <- analysisPrep(EdWelcome_datQ17.4$Q17.4.a, EdWelcome_datQ17.4$EdinvsWelcome, EdWelcome_datQ17.4)
analysis <- polr(EdWelcome_datQ17.4$Q17.4.a ~ EdWelcome_datQ17.4$EdinvsWelcome, EdWelcome_datQ17.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q17.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)


EdWelcome_datQ18.1<-multidatCleanWelcome(Q18.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.1.a + EdinvsWelcome, data = EdWelcome_datQ18.1)
conTable
detach(dat_long)
EdWelcome_datQ18.1$Q18.1.a[(EdWelcome_datQ18.1$Q18.1.a == "8")]=NA
EdWelcome_datQ18.1<-ordinaldatWelcomeClean(EdWelcome_datQ18.1)
EdWelcome_datQ18.1$EdinvsWelcome<-factor(EdWelcome_datQ18.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ18.1$Q18.1.a, EdWelcome_datQ18.1$EdinvsWelcome, EdWelcome_datQ18.1)
analysis <- polr(EdWelcome_datQ18.1$Q18.1.a ~ EdWelcome_datQ18.1$EdinvsWelcome, EdWelcome_datQ18.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ18.2<-multidatCleanWelcome(Q18.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.2.a + EdinvsWelcome, data = EdWelcome_datQ18.2)
conTable
detach(dat_long)
EdWelcome_datQ18.2$Q18.2.a[(EdWelcome_datQ18.2$Q18.2.a == "8")]=NA
EdWelcome_datQ18.2<-ordinaldatWelcomeClean(EdWelcome_datQ18.2)
EdWelcome_datQ18.2$EdinvsWelcome<-factor(EdWelcome_datQ18.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ18.2$Q18.2.a, EdWelcome_datQ18.2$EdinvsWelcome, EdWelcome_datQ18.2)
analysis <- polr(EdWelcome_datQ18.2$Q18.2.a ~ EdWelcome_datQ18.2$EdinvsWelcome, EdWelcome_datQ18.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ18.3<-multidatCleanWelcome(Q18.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.3.a + EdinvsWelcome, data = EdWelcome_datQ18.3)
conTable
detach(dat_long)
EdWelcome_datQ18.3$Q18.3.a[(EdWelcome_datQ18.3$Q18.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.3<-ordinaldatWelcomeClean(EdWelcome_datQ18.3)
EdWelcome_datQ18.3$EdinvsWelcome<-factor(EdWelcome_datQ18.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ18.3$Q18.3.a, EdWelcome_datQ18.3$EdinvsWelcome, EdWelcome_datQ18.3)
analysis <- polr(EdWelcome_datQ18.3$Q18.3.a ~ EdWelcome_datQ18.3$EdinvsWelcome, EdWelcome_datQ18.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.4<-multidatCleanWelcome(Q18.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.4.a + EdinvsWelcome, data = EdWelcome_datQ18.4)
conTable
detach(dat_long)
EdWelcome_datQ18.4$Q18.4.a[(EdWelcome_datQ18.4$Q18.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.4<-ordinaldatWelcomeClean(EdWelcome_datQ18.4)
EdWelcome_datQ18.4$EdinvsWelcome<-factor(EdWelcome_datQ18.4$EdinvsWelcome)
EdWelcome_datQ18.4$Q18.4.a<-factor(EdWelcome_datQ18.4$Q18.4.a)
prep <- analysisPrep(EdWelcome_datQ18.4$Q18.4.a, EdWelcome_datQ18.4$EdinvsWelcome, EdWelcome_datQ18.4)
analysis <- polr(EdWelcome_datQ18.4$Q18.4.a ~ EdWelcome_datQ18.4$EdinvsWelcome, EdWelcome_datQ18.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.5<-multidatCleanWelcome(Q18.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.5.a + EdinvsWelcome, data = EdWelcome_datQ18.5)
conTable
detach(dat_long)
EdWelcome_datQ18.5$Q18.5.a[(EdWelcome_datQ18.5$Q18.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.5<- ordinaldatWelcomeClean (EdWelcome_datQ18.5, reverse=TRUE)
EdWelcome_datQ18.5$EdinvsWelcome<-factor(EdWelcome_datQ18.5$EdinvsWelcome)
EdWelcome_datQ18.5$Q18.5.a<-factor(EdWelcome_datQ18.5$Q18.5.a)
prep <- analysisPrep(EdWelcome_datQ18.5$Q18.5.a, EdWelcome_datQ18.5$EdinvsWelcome, EdWelcome_datQ18.5)
analysis <- polr(EdWelcome_datQ18.5$Q18.5.a ~ EdWelcome_datQ18.5$EdinvsWelcome, EdWelcome_datQ18.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.6<-multidatCleanWelcome(Q18.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.6.a + EdinvsWelcome, data = EdWelcome_datQ18.6)
conTable
detach(dat_long)
EdWelcome_datQ18.6$Q18.6.a[(EdWelcome_datQ18.6$Q18.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.6<- ordinaldatWelcomeClean (EdWelcome_datQ18.6, reverse=TRUE)
EdWelcome_datQ18.6$EdinvsWelcome<-factor(EdWelcome_datQ18.6$EdinvsWelcome)
EdWelcome_datQ18.6$Q18.6.a<-factor(EdWelcome_datQ18.6$Q18.6.a)
prep <- analysisPrep(EdWelcome_datQ18.6$Q18.6.a, EdWelcome_datQ18.6$EdinvsWelcome, EdWelcome_datQ18.6)
analysis <- polr(EdWelcome_datQ18.6$Q18.6.a ~ EdWelcome_datQ18.6$EdinvsWelcome, EdWelcome_datQ18.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.7<-multidatCleanWelcome(Q18.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.7.a + EdinvsWelcome, data = EdWelcome_datQ18.7)
conTable
detach(dat_long)
EdWelcome_datQ18.7$Q18.7.a[(EdWelcome_datQ18.7$Q18.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.7<- ordinaldatWelcomeClean (EdWelcome_datQ18.7, reverse=TRUE)
EdWelcome_datQ18.7$EdinvsWelcome<-factor(EdWelcome_datQ18.7$EdinvsWelcome)
EdWelcome_datQ18.7$Q18.7.a<-factor(EdWelcome_datQ18.7$Q18.7.a)
prep <- analysisPrep(EdWelcome_datQ18.7$Q18.7.a, EdWelcome_datQ18.7$EdinvsWelcome, EdWelcome_datQ18.7)
analysis <- polr(EdWelcome_datQ18.7$Q18.7.a ~ EdWelcome_datQ18.7$EdinvsWelcome, EdWelcome_datQ18.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.8<-multidatCleanWelcome(Q18.8.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.8.a + EdinvsWelcome, data = EdWelcome_datQ18.8)
conTable
detach(dat_long)
EdWelcome_datQ18.8$Q18.8.a[(EdWelcome_datQ18.8$Q18.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.8<- ordinaldatWelcomeClean (EdWelcome_datQ18.8, reverse=FALSE)
EdWelcome_datQ18.8$EdinvsWelcome<-factor(EdWelcome_datQ18.8$EdinvsWelcome)
EdWelcome_datQ18.8$Q18.8.a<-factor(EdWelcome_datQ18.8$Q18.8.a)
prep <- analysisPrep(EdWelcome_datQ18.8$Q18.8.a, EdWelcome_datQ18.8$EdinvsWelcome, EdWelcome_datQ18.8)
analysis <- polr(EdWelcome_datQ18.8$Q18.8.a ~ EdWelcome_datQ18.8$EdinvsWelcome, EdWelcome_datQ18.8, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.8.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.9<-multidatCleanWelcome(Q18.9.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.9.a + EdinvsWelcome, data = EdWelcome_datQ18.9)
conTable
detach(dat_long)
EdWelcome_datQ18.9$Q18.9.a[(EdWelcome_datQ18.9$Q18.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.9<- ordinaldatWelcomeClean(EdWelcome_datQ18.9, reverse=TRUE)
EdWelcome_datQ18.9$EdinvsWelcome<-factor(EdWelcome_datQ18.9$EdinvsWelcome)
EdWelcome_datQ18.9$Q18.9.a<-factor(EdWelcome_datQ18.9$Q18.9.a)
prep <- analysisPrep(EdWelcome_datQ18.9$Q18.9.a, EdWelcome_datQ18.9$EdinvsWelcome, EdWelcome_datQ18.9)
analysis <- polr(EdWelcome_datQ18.9$Q18.9.a ~ EdWelcome_datQ18.9$EdinvsWelcome, EdWelcome_datQ18.9, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.9.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ18.10<-multidatCleanWelcome(Q18.10.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.10.a + EdinvsWelcome, data = EdWelcome_datQ18.10)
conTable
detach(dat_long)
EdWelcome_datQ18.10$Q18.10.a[(EdWelcome_datQ18.10$Q18.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.10<- ordinaldatWelcomeClean (EdWelcome_datQ18.10, reverse=FALSE)
EdWelcome_datQ18.10$EdinvsWelcome<-factor(EdWelcome_datQ18.10$EdinvsWelcome)
EdWelcome_datQ18.10$Q18.10.a<-factor(EdWelcome_datQ18.10$Q18.10.a)
prep <- analysisPrep(EdWelcome_datQ18.10$Q18.10.a, EdWelcome_datQ18.10$EdinvsWelcome, EdWelcome_datQ18.10)
analysis <- polr(EdWelcome_datQ18.10$Q18.10.a ~ EdWelcome_datQ18.10$EdinvsWelcome, EdWelcome_datQ18.10, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.10.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.11<-multidatCleanWelcome(Q18.11.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.11.a + EdinvsWelcome, data = EdWelcome_datQ18.11)
conTable
detach(dat_long)
EdWelcome_datQ18.11$Q18.11.a[(EdWelcome_datQ18.11$Q18.11.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.11<- ordinaldatWelcomeClean (EdWelcome_datQ18.11, reverse=FALSE)
EdWelcome_datQ18.11$EdinvsWelcome<-factor(EdWelcome_datQ18.11$EdinvsWelcome)
EdWelcome_datQ18.11$Q18.11.a<-factor(EdWelcome_datQ18.11$Q18.11.a)
prep <- analysisPrep(EdWelcome_datQ18.11$Q18.11.a, EdWelcome_datQ18.11$EdinvsWelcome, EdWelcome_datQ18.11)
analysis <- polr(EdWelcome_datQ18.11$Q18.11.a ~ EdWelcome_datQ18.11$EdinvsWelcome, EdWelcome_datQ18.11, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.11.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.12<-multidatCleanWelcome(Q18.12.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.12.a + EdinvsWelcome, data = EdWelcome_datQ18.12)
conTable
detach(dat_long)
EdWelcome_datQ18.12$Q18.12.a[(EdWelcome_datQ18.12$Q18.12.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.12<- ordinaldatWelcomeClean (EdWelcome_datQ18.12, reverse=TRUE)
EdWelcome_datQ18.12$EdinvsWelcome<-factor(EdWelcome_datQ18.12$EdinvsWelcome)
EdWelcome_datQ18.12$Q18.12.a<-factor(EdWelcome_datQ18.12$Q18.12.a)
prep <- analysisPrep(EdWelcome_datQ18.12$Q18.12.a, EdWelcome_datQ18.12$EdinvsWelcome, EdWelcome_datQ18.12)
analysis <- polr(EdWelcome_datQ18.12$Q18.12.a ~ EdWelcome_datQ18.12$EdinvsWelcome, EdWelcome_datQ18.12, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.12.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ18.13<-multidatCleanWelcome(Q18.13.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.13.a + EdinvsWelcome, data = EdWelcome_datQ18.13)
conTable
detach(dat_long)
EdWelcome_datQ18.13$Q18.13.a[(EdWelcome_datQ18.13$Q18.13.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.13<- ordinaldatWelcomeClean (EdWelcome_datQ18.13, reverse=FALSE)
EdWelcome_datQ18.13$EdinvsWelcome<-factor(EdWelcome_datQ18.13$EdinvsWelcome)
EdWelcome_datQ18.13$Q18.13.a<-factor(EdWelcome_datQ18.13$Q18.13.a)
prep <- analysisPrep(EdWelcome_datQ18.13$Q18.13.a, EdWelcome_datQ18.13$EdinvsWelcome, EdWelcome_datQ18.13)
analysis <- polr(EdWelcome_datQ18.13$Q18.13.a ~ EdWelcome_datQ18.13$EdinvsWelcome, EdWelcome_datQ18.13, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q18.13.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ20.1<-multidatCleanWelcome(Q20.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.1.a + EdinvsWelcome, data = EdWelcome_datQ20.1)
conTable
detach(dat_long)
EdWelcome_datQ20.1$Q20.1.a[(EdWelcome_datQ20.1$Q20.1.a == "8")]=NA
EdWelcome_datQ20.1<-ordinaldatWelcomeClean(EdWelcome_datQ20.1)
EdWelcome_datQ20.1$EdinvsWelcome<-factor(EdWelcome_datQ20.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ20.1$Q20.1.a, EdWelcome_datQ20.1$EdinvsWelcome, EdWelcome_datQ20.1)
analysis <- polr(EdWelcome_datQ20.1$Q20.1.a ~ EdWelcome_datQ20.1$EdinvsWelcome, EdWelcome_datQ20.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ20.2<-multidatCleanWelcome(Q20.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.2.a + EdinvsWelcome, data = EdWelcome_datQ20.2)
conTable
detach(dat_long)
EdWelcome_datQ20.2$Q20.2.a[(EdWelcome_datQ20.2$Q20.2.a == "8")]=NA
EdWelcome_datQ20.2<-ordinaldatWelcomeClean(EdWelcome_datQ20.2, reverse=TRUE)
EdWelcome_datQ20.2$EdinvsWelcome<-factor(EdWelcome_datQ20.2$EdinvsWelcome)
EdWelcome_datQ20.2$Q20.2.a<-factor(EdWelcome_datQ20.2$Q20.2.a)
prep <- analysisPrep(EdWelcome_datQ20.2$Q20.2.a, EdWelcome_datQ20.2$EdinvsWelcome, EdWelcome_datQ20.2)
analysis <- polr(EdWelcome_datQ20.2$Q20.2.a ~ EdWelcome_datQ20.2$EdinvsWelcome, EdWelcome_datQ20.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ20.3<-multidatCleanWelcome(Q20.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.3.a + EdinvsWelcome, data = EdWelcome_datQ20.3)
conTable
detach(dat_long)
EdWelcome_datQ20.3$Q20.3.a[(EdWelcome_datQ20.3$Q20.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ20.3<-ordinaldatWelcomeClean(EdWelcome_datQ20.3)
EdWelcome_datQ20.3$EdinvsWelcome<-factor(EdWelcome_datQ20.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ20.3$Q20.3.a, EdWelcome_datQ20.3$EdinvsWelcome, EdWelcome_datQ20.3)
analysis <- polr(EdWelcome_datQ20.3$Q20.3.a ~ EdWelcome_datQ20.3$EdinvsWelcome, EdWelcome_datQ20.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ20.4<-multidatCleanWelcome(Q20.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.4.a + EdinvsWelcome, data = EdWelcome_datQ20.4)
conTable
detach(dat_long)
EdWelcome_datQ20.4$Q20.4.a[(EdWelcome_datQ20.4$Q20.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ20.4<-ordinaldatWelcomeClean(EdWelcome_datQ20.4)
EdWelcome_datQ20.4$EdinvsWelcome<-factor(EdWelcome_datQ20.4$EdinvsWelcome)
EdWelcome_datQ20.4$Q20.4.a<-factor(EdWelcome_datQ20.4$Q20.4.a)
prep <- analysisPrep(EdWelcome_datQ20.4$Q20.4.a, EdWelcome_datQ20.4$EdinvsWelcome, EdWelcome_datQ20.4)
analysis <- polr(EdWelcome_datQ20.4$Q20.4.a ~ EdWelcome_datQ20.4$EdinvsWelcome, EdWelcome_datQ20.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ20.5<-multidatCleanWelcome(Q20.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.5.a + EdinvsWelcome, data = EdWelcome_datQ20.5)
conTable
detach(dat_long)
EdWelcome_datQ20.5$Q20.5.a[(EdWelcome_datQ20.5$Q20.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ20.5<- ordinaldatWelcomeClean (EdWelcome_datQ20.5, reverse=TRUE)
EdWelcome_datQ20.5$EdinvsWelcome<-factor(EdWelcome_datQ20.5$EdinvsWelcome)
EdWelcome_datQ20.5$Q20.5.a<-factor(EdWelcome_datQ20.5$Q20.5.a)
prep <- analysisPrep(EdWelcome_datQ20.5$Q20.5.a, EdWelcome_datQ20.5$EdinvsWelcome, EdWelcome_datQ20.5)
analysis <- polr(EdWelcome_datQ20.5$Q20.5.a ~ EdWelcome_datQ20.5$EdinvsWelcome, EdWelcome_datQ20.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ20.6<-multidatCleanWelcome(Q20.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.6.a + EdinvsWelcome, data = EdWelcome_datQ20.6)
conTable
detach(dat_long)
EdWelcome_datQ20.6$Q20.6.a[(EdWelcome_datQ20.6$Q20.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ20.6<- ordinaldatWelcomeClean (EdWelcome_datQ20.6)
EdWelcome_datQ20.6$EdinvsWelcome<-factor(EdWelcome_datQ20.6$EdinvsWelcome)
EdWelcome_datQ20.6$Q20.6.a<-factor(EdWelcome_datQ20.6$Q20.6.a)
prep <- analysisPrep(EdWelcome_datQ20.6$Q20.6.a, EdWelcome_datQ20.6$EdinvsWelcome, EdWelcome_datQ20.6)
analysis <- polr(EdWelcome_datQ20.6$Q20.6.a ~ EdWelcome_datQ20.6$EdinvsWelcome, EdWelcome_datQ20.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ20.7<-multidatCleanWelcome(Q20.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q20.7.a + EdinvsWelcome, data = EdWelcome_datQ20.7)
conTable
detach(dat_long)
EdWelcome_datQ20.7$Q20.7.a[(EdWelcome_datQ20.7$Q20.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ20.7<- ordinaldatWelcomeClean (EdWelcome_datQ20.7, reverse=TRUE)
EdWelcome_datQ20.7$EdinvsWelcome<-factor(EdWelcome_datQ20.7$EdinvsWelcome)
EdWelcome_datQ20.7$Q20.7.a<-factor(EdWelcome_datQ20.7$Q20.7.a)
prep <- analysisPrep(EdWelcome_datQ20.7$Q20.7.a, EdWelcome_datQ20.7$EdinvsWelcome, EdWelcome_datQ20.7)
analysis <- polr(EdWelcome_datQ20.7$Q20.7.a ~ EdWelcome_datQ20.7$EdinvsWelcome, EdWelcome_datQ20.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q20.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.1<-multidatCleanWelcome(Q21.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.1.a + EdinvsWelcome, data = EdWelcome_datQ21.1)
conTable
detach(dat_long)
EdWelcome_datQ21.1$Q21.1.a[(EdWelcome_datQ21.1$Q21.1.a == "8")]=NA
EdWelcome_datQ21.1<-ordinaldatWelcomeClean(EdWelcome_datQ21.1)
EdWelcome_datQ21.1$EdinvsWelcome<-factor(EdWelcome_datQ21.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ21.1$Q21.1.a, EdWelcome_datQ21.1$EdinvsWelcome, EdWelcome_datQ21.1)
analysis <- polr(EdWelcome_datQ21.1$Q21.1.a ~ EdWelcome_datQ21.1$EdinvsWelcome, EdWelcome_datQ21.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ21.2<-multidatCleanWelcome(Q21.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.2.a + EdinvsWelcome, data = EdWelcome_datQ21.2)
conTable
detach(dat_long)
EdWelcome_datQ21.2$Q21.2.a[(EdWelcome_datQ21.2$Q21.2.a == "8")]=NA
EdWelcome_datQ21.2<-ordinaldatWelcomeClean(EdWelcome_datQ21.2)
EdWelcome_datQ21.2$EdinvsWelcome<-factor(EdWelcome_datQ21.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ21.2$Q21.2.a, EdWelcome_datQ21.2$EdinvsWelcome, EdWelcome_datQ21.2)
analysis <- polr(EdWelcome_datQ21.2$Q21.2.a ~ EdWelcome_datQ21.2$EdinvsWelcome, EdWelcome_datQ21.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ21.3<-multidatCleanWelcome(Q21.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.3.a + EdinvsWelcome, data = EdWelcome_datQ21.3)
conTable
detach(dat_long)
EdWelcome_datQ21.3$Q21.3.a[(EdWelcome_datQ21.3$Q21.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.3<-ordinaldatWelcomeClean(EdWelcome_datQ21.3)
EdWelcome_datQ21.3$EdinvsWelcome<-factor(EdWelcome_datQ21.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ21.3$Q21.3.a, EdWelcome_datQ21.3$EdinvsWelcome, EdWelcome_datQ21.3)
analysis <- polr(EdWelcome_datQ21.3$Q21.3.a ~ EdWelcome_datQ21.3$EdinvsWelcome, EdWelcome_datQ21.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.4<-multidatCleanWelcome(Q21.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.4.a + EdinvsWelcome, data = EdWelcome_datQ21.4)
conTable
detach(dat_long)
EdWelcome_datQ21.4$Q21.4.a[(EdWelcome_datQ21.4$Q21.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.4<-ordinaldatWelcomeClean(EdWelcome_datQ21.4)
EdWelcome_datQ21.4$EdinvsWelcome<-factor(EdWelcome_datQ21.4$EdinvsWelcome)
EdWelcome_datQ21.4$Q21.4.a<-factor(EdWelcome_datQ21.4$Q21.4.a)
prep <- analysisPrep(EdWelcome_datQ21.4$Q21.4.a, EdWelcome_datQ21.4$EdinvsWelcome, EdWelcome_datQ21.4)
analysis <- polr(EdWelcome_datQ21.4$Q21.4.a ~ EdWelcome_datQ21.4$EdinvsWelcome, EdWelcome_datQ21.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.5<-multidatCleanWelcome(Q21.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.5.a + EdinvsWelcome, data = EdWelcome_datQ21.5)
conTable
detach(dat_long)
EdWelcome_datQ21.5$Q21.5.a[(EdWelcome_datQ21.5$Q21.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.5<- ordinaldatWelcomeClean (EdWelcome_datQ21.5)
EdWelcome_datQ21.5$EdinvsWelcome<-factor(EdWelcome_datQ21.5$EdinvsWelcome)
EdWelcome_datQ21.5$Q21.5.a<-factor(EdWelcome_datQ21.5$Q21.5.a)
prep <- analysisPrep(EdWelcome_datQ21.5$Q21.5.a, EdWelcome_datQ21.5$EdinvsWelcome, EdWelcome_datQ21.5)
analysis <- polr(EdWelcome_datQ21.5$Q21.5.a ~ EdWelcome_datQ21.5$EdinvsWelcome, EdWelcome_datQ21.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.6<-multidatCleanWelcome(Q21.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.6.a + EdinvsWelcome, data = EdWelcome_datQ21.6)
conTable
detach(dat_long)
EdWelcome_datQ21.6$Q21.6.a[(EdWelcome_datQ21.6$Q21.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.6<- ordinaldatWelcomeClean (EdWelcome_datQ21.6)
EdWelcome_datQ21.6$EdinvsWelcome<-factor(EdWelcome_datQ21.6$EdinvsWelcome)
EdWelcome_datQ21.6$Q21.6.a<-factor(EdWelcome_datQ21.6$Q21.6.a)
prep <- analysisPrep(EdWelcome_datQ21.6$Q21.6.a, EdWelcome_datQ21.6$EdinvsWelcome, EdWelcome_datQ21.6)
analysis <- polr(EdWelcome_datQ21.6$Q21.6.a ~ EdWelcome_datQ21.6$EdinvsWelcome, EdWelcome_datQ21.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.7<-multidatCleanWelcome(Q21.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.7.a + EdinvsWelcome, data = EdWelcome_datQ21.7)
conTable
detach(dat_long)
EdWelcome_datQ21.7$Q21.7.a[(EdWelcome_datQ21.7$Q21.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.7<- ordinaldatWelcomeClean (EdWelcome_datQ21.7)
EdWelcome_datQ21.7$EdinvsWelcome<-factor(EdWelcome_datQ21.7$EdinvsWelcome)
EdWelcome_datQ21.7$Q21.7.a<-factor(EdWelcome_datQ21.7$Q21.7.a)
prep <- analysisPrep(EdWelcome_datQ21.7$Q21.7.a, EdWelcome_datQ21.7$EdinvsWelcome, EdWelcome_datQ21.7)
analysis <- polr(EdWelcome_datQ21.7$Q21.7.a ~ EdWelcome_datQ21.7$EdinvsWelcome, EdWelcome_datQ21.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.8<-multidatCleanWelcome(Q21.8.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.8.a + EdinvsWelcome, data = EdWelcome_datQ21.8)
conTable
detach(dat_long)
EdWelcome_datQ21.8$Q21.8.a[(EdWelcome_datQ21.8$Q21.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.8<- ordinaldatWelcomeClean (EdWelcome_datQ21.8, reverse=FALSE)
EdWelcome_datQ21.8$EdinvsWelcome<-factor(EdWelcome_datQ21.8$EdinvsWelcome)
EdWelcome_datQ21.8$Q21.8.a<-factor(EdWelcome_datQ21.8$Q21.8.a)
prep <- analysisPrep(EdWelcome_datQ21.8$Q21.8.a, EdWelcome_datQ21.8$EdinvsWelcome, EdWelcome_datQ21.8)
analysis <- polr(EdWelcome_datQ21.8$Q21.8.a ~ EdWelcome_datQ21.8$EdinvsWelcome, EdWelcome_datQ21.8, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.8.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.9<-multidatCleanWelcome(Q21.9.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.9.a + EdinvsWelcome, data = EdWelcome_datQ21.9)
conTable
detach(dat_long)
EdWelcome_datQ21.9$Q21.9.a[(EdWelcome_datQ21.9$Q21.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.9<- ordinaldatWelcomeClean(EdWelcome_datQ21.9)
EdWelcome_datQ21.9$EdinvsWelcome<-factor(EdWelcome_datQ21.9$EdinvsWelcome)
EdWelcome_datQ21.9$Q21.9.a<-factor(EdWelcome_datQ21.9$Q21.9.a)
prep <- analysisPrep(EdWelcome_datQ21.9$Q21.9.a, EdWelcome_datQ21.9$EdinvsWelcome, EdWelcome_datQ21.9)
analysis <- polr(EdWelcome_datQ21.9$Q21.9.a ~ EdWelcome_datQ21.9$EdinvsWelcome, EdWelcome_datQ21.9, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.9.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ21.10<-multidatCleanWelcome(Q21.10.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.10.a + EdinvsWelcome, data = EdWelcome_datQ21.10)
conTable
detach(dat_long)
EdWelcome_datQ21.10$Q21.10.a[(EdWelcome_datQ21.10$Q21.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.10<- ordinaldatWelcomeClean (EdWelcome_datQ21.10, reverse=FALSE)
EdWelcome_datQ21.10$EdinvsWelcome<-factor(EdWelcome_datQ21.10$EdinvsWelcome)
EdWelcome_datQ21.10$Q21.10.a<-factor(EdWelcome_datQ21.10$Q21.10.a)
prep <- analysisPrep(EdWelcome_datQ21.10$Q21.10.a, EdWelcome_datQ21.10$EdinvsWelcome, EdWelcome_datQ21.10)
analysis <- polr(EdWelcome_datQ21.10$Q21.10.a ~ EdWelcome_datQ21.10$EdinvsWelcome, EdWelcome_datQ21.10, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.10.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ21.11<-multidatCleanWelcome(Q21.11.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q21.11.a + EdinvsWelcome, data = EdWelcome_datQ21.11)
conTable
detach(dat_long)
EdWelcome_datQ21.11$Q21.11.a[(EdWelcome_datQ21.11$Q21.11.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ21.11<- ordinaldatWelcomeClean (EdWelcome_datQ21.11, reverse=FALSE)
EdWelcome_datQ21.11$EdinvsWelcome<-factor(EdWelcome_datQ21.11$EdinvsWelcome)
EdWelcome_datQ21.11$Q21.11.a<-factor(EdWelcome_datQ21.11$Q21.11.a)
prep <- analysisPrep(EdWelcome_datQ21.11$Q21.11.a, EdWelcome_datQ21.11$EdinvsWelcome, EdWelcome_datQ21.11)
analysis <- polr(EdWelcome_datQ21.11$Q21.11.a ~ EdWelcome_datQ21.11$EdinvsWelcome, EdWelcome_datQ21.11, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q21.11.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ22<-multidatCleanWelcome(Q22, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q22 + EdinvsWelcome, data = EdWelcome_datQ22)
conTable
detach(dat_long)
EdWelcome_datQ22$Q22[(EdWelcome_datQ22$Q22 == "8")]=NA
EdWelcome_datQ22<-SatisdatWelcomeClean(EdWelcome_datQ22)
EdWelcome_datQ22$EdinvsWelcome<-factor(EdWelcome_datQ22$EdinvsWelcome)
EdWelcome_datQ22$Q22<-factor(EdWelcome_datQ22$Q22)
prep <- analysisPrep(EdWelcome_datQ22$Q22, EdWelcome_datQ22$EdinvsWelcome, EdWelcome_datQ22)
analysis <- polr(EdWelcome_datQ22$Q22 ~ EdWelcome_datQ22$EdinvsWelcome, EdWelcome_datQ22, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q22"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ25.1<-multidatCleanWelcome(Q25.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q25.1.a + EdinvsWelcome, data = EdWelcome_datQ25.1)
conTable
detach(dat_long)
EdWelcome_datQ25.1$Q25.1.a[(EdWelcome_datQ25.1$Q25.1.a == "8")]=NA
EdWelcome_datQ25.1<-ordinaldatWelcomeClean(EdWelcome_datQ25.1)
EdWelcome_datQ25.1$EdinvsWelcome<-factor(EdWelcome_datQ25.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ25.1$Q25.1.a, EdWelcome_datQ25.1$EdinvsWelcome, EdWelcome_datQ25.1)
analysis <- polr(EdWelcome_datQ25.1$Q25.1.a ~ EdWelcome_datQ25.1$EdinvsWelcome, EdWelcome_datQ25.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ25.2<-multidatCleanWelcome(Q25.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q25.2.a + EdinvsWelcome, data = EdWelcome_datQ25.2)
conTable
detach(dat_long)
EdWelcome_datQ25.2$Q25.2.a[(EdWelcome_datQ25.2$Q25.2.a == "8")]=NA
EdWelcome_datQ25.2<-ordinaldatWelcomeClean(EdWelcome_datQ25.2)
EdWelcome_datQ25.2$EdinvsWelcome<-factor(EdWelcome_datQ25.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ25.2$Q25.2.a, EdWelcome_datQ25.2$EdinvsWelcome, EdWelcome_datQ25.2)
analysis <- polr(EdWelcome_datQ25.2$Q25.2.a ~ EdWelcome_datQ25.2$EdinvsWelcome, EdWelcome_datQ25.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ25.3<-multidatCleanWelcome(Q25.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q25.3.a + EdinvsWelcome, data = EdWelcome_datQ25.3)
conTable
detach(dat_long)
EdWelcome_datQ25.3$Q25.3.a[(EdWelcome_datQ25.3$Q25.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ25.3<-ordinaldatWelcomeClean(EdWelcome_datQ25.3)
EdWelcome_datQ25.3$EdinvsWelcome<-factor(EdWelcome_datQ25.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ25.3$Q25.3.a, EdWelcome_datQ25.3$EdinvsWelcome, EdWelcome_datQ25.3)
analysis <- polr(EdWelcome_datQ25.3$Q25.3.a ~ EdWelcome_datQ25.3$EdinvsWelcome, EdWelcome_datQ25.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ25.4<-multidatCleanWelcome(Q25.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q25.4.a + EdinvsWelcome, data = EdWelcome_datQ25.4)
conTable
detach(dat_long)
EdWelcome_datQ25.4$Q25.4.a[(EdWelcome_datQ25.4$Q25.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ25.4<-ordinaldatWelcomeClean(EdWelcome_datQ25.4)
EdWelcome_datQ25.4$EdinvsWelcome<-factor(EdWelcome_datQ25.4$EdinvsWelcome)
EdWelcome_datQ25.4$Q25.4.a<-factor(EdWelcome_datQ25.4$Q25.4.a)
prep <- analysisPrep(EdWelcome_datQ25.4$Q25.4.a, EdWelcome_datQ25.4$EdinvsWelcome, EdWelcome_datQ25.4)
analysis <- polr(EdWelcome_datQ25.4$Q25.4.a ~ EdWelcome_datQ25.4$EdinvsWelcome, EdWelcome_datQ25.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ25.5<-multidatCleanWelcome(Q25.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q25.5.a + EdinvsWelcome, data = EdWelcome_datQ25.5)
conTable
detach(dat_long)
EdWelcome_datQ25.5$Q25.5.a[(EdWelcome_datQ25.5$Q25.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ25.5<- ordinaldatWelcomeClean (EdWelcome_datQ25.5)
EdWelcome_datQ25.5$EdinvsWelcome<-factor(EdWelcome_datQ25.5$EdinvsWelcome)
EdWelcome_datQ25.5$Q25.5.a<-factor(EdWelcome_datQ25.5$Q25.5.a)
prep <- analysisPrep(EdWelcome_datQ25.5$Q25.5.a, EdWelcome_datQ25.5$EdinvsWelcome, EdWelcome_datQ25.5)
analysis <- polr(EdWelcome_datQ25.5$Q25.5.a ~ EdWelcome_datQ25.5$EdinvsWelcome, EdWelcome_datQ25.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q25.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.1<-multidatCleanWelcome(Q27.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.1.a + EdinvsWelcome, data = EdWelcome_datQ27.1)
conTable
detach(dat_long)
EdWelcome_datQ27.1$Q27.1.a[(EdWelcome_datQ27.1$Q27.1.a == "8")]=NA
EdWelcome_datQ27.1<-ordinaldatWelcomeClean(EdWelcome_datQ27.1)
EdWelcome_datQ27.1$EdinvsWelcome<-factor(EdWelcome_datQ27.1$EdinvsWelcome)
EdWelcome_datQ27.1$Q27.1.a<-factor(EdWelcome_datQ27.1$Q27.1.a)
prep <- analysisPrep(EdWelcome_datQ27.1$Q27.1.a, EdWelcome_datQ27.1$EdinvsWelcome, EdWelcome_datQ27.1)
analysis <- polr(EdWelcome_datQ27.1$Q27.1.a ~ EdWelcome_datQ27.1$EdinvsWelcome, EdWelcome_datQ27.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ27.2<-multidatCleanWelcome(Q27.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.2.a + EdinvsWelcome, data = EdWelcome_datQ27.2)
conTable
detach(dat_long)
EdWelcome_datQ27.2$Q27.2.a[(EdWelcome_datQ27.2$Q27.2.a == "8")]=NA
EdWelcome_datQ27.2<-ordinaldatWelcomeClean(EdWelcome_datQ27.2, reverse=TRUE)
EdWelcome_datQ27.2$EdinvsWelcome<-factor(EdWelcome_datQ27.2$EdinvsWelcome)
EdWelcome_datQ27.2$Q27.2.a<-factor(EdWelcome_datQ27.2$Q27.2.a)
prep <- analysisPrep(EdWelcome_datQ27.2$Q27.2.a, EdWelcome_datQ27.2$EdinvsWelcome, EdWelcome_datQ27.2)
analysis <- polr(EdWelcome_datQ27.2$Q27.2.a ~ EdWelcome_datQ27.2$EdinvsWelcome, EdWelcome_datQ27.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ27.3<-multidatCleanWelcome(Q27.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.3.a + EdinvsWelcome, data = EdWelcome_datQ27.3)
conTable
detach(dat_long)
EdWelcome_datQ27.3$Q27.3.a[(EdWelcome_datQ27.3$Q27.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.3<-ordinaldatWelcomeClean(EdWelcome_datQ27.3)
EdWelcome_datQ27.3$EdinvsWelcome<-factor(EdWelcome_datQ27.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ27.3$Q27.3.a, EdWelcome_datQ27.3$EdinvsWelcome, EdWelcome_datQ27.3)
analysis <- polr(EdWelcome_datQ27.3$Q27.3.a ~ EdWelcome_datQ27.3$EdinvsWelcome, EdWelcome_datQ27.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.4<-multidatCleanWelcome(Q27.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.4.a + EdinvsWelcome, data = EdWelcome_datQ27.4)
conTable
detach(dat_long)
EdWelcome_datQ27.4$Q27.4.a[(EdWelcome_datQ27.4$Q27.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.4<-ordinaldatWelcomeClean(EdWelcome_datQ27.4, reverse=TRUE)
EdWelcome_datQ27.4$EdinvsWelcome<-factor(EdWelcome_datQ27.4$EdinvsWelcome)
EdWelcome_datQ27.4$Q27.4.a<-factor(EdWelcome_datQ27.4$Q27.4.a)
prep <- analysisPrep(EdWelcome_datQ27.4$Q27.4.a, EdWelcome_datQ27.4$EdinvsWelcome, EdWelcome_datQ27.4)
analysis <- polr(EdWelcome_datQ27.4$Q27.4.a ~ EdWelcome_datQ27.4$EdinvsWelcome, EdWelcome_datQ27.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.5<-multidatCleanWelcome(Q27.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.5.a + EdinvsWelcome, data = EdWelcome_datQ27.5)
conTable
detach(dat_long)
EdWelcome_datQ27.5$Q27.5.a[(EdWelcome_datQ27.5$Q27.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.5<- ordinaldatWelcomeClean (EdWelcome_datQ27.5, reverse=TRUE)
EdWelcome_datQ27.5$EdinvsWelcome<-factor(EdWelcome_datQ27.5$EdinvsWelcome)
EdWelcome_datQ27.5$Q27.5.a<-factor(EdWelcome_datQ27.5$Q27.5.a)
prep <- analysisPrep(EdWelcome_datQ27.5$Q27.5.a, EdWelcome_datQ27.5$EdinvsWelcome, EdWelcome_datQ27.5)
analysis <- polr(EdWelcome_datQ27.5$Q27.5.a ~ EdWelcome_datQ27.5$EdinvsWelcome, EdWelcome_datQ27.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.6<-multidatCleanWelcome(Q27.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.6.a + EdinvsWelcome, data = EdWelcome_datQ27.6)
conTable
detach(dat_long)
EdWelcome_datQ27.6$Q27.6.a[(EdWelcome_datQ27.6$Q27.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.6<- ordinaldatWelcomeClean (EdWelcome_datQ27.6)
EdWelcome_datQ27.6$EdinvsWelcome<-factor(EdWelcome_datQ27.6$EdinvsWelcome)
EdWelcome_datQ27.6$Q27.6.a<-factor(EdWelcome_datQ27.6$Q27.6.a)
prep <- analysisPrep(EdWelcome_datQ27.6$Q27.6.a, EdWelcome_datQ27.6$EdinvsWelcome, EdWelcome_datQ27.6)
analysis <- polr(EdWelcome_datQ27.6$Q27.6.a ~ EdWelcome_datQ27.6$EdinvsWelcome, EdWelcome_datQ27.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.7<-multidatCleanWelcome(Q27.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.7.a + EdinvsWelcome, data = EdWelcome_datQ27.7)
conTable
detach(dat_long)
EdWelcome_datQ27.7$Q27.7.a[(EdWelcome_datQ27.7$Q27.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.7<- ordinaldatWelcomeClean (EdWelcome_datQ27.7)
EdWelcome_datQ27.7$EdinvsWelcome<-factor(EdWelcome_datQ27.7$EdinvsWelcome)
EdWelcome_datQ27.7$Q27.7.a<-factor(EdWelcome_datQ27.7$Q27.7.a)
prep <- analysisPrep(EdWelcome_datQ27.7$Q27.7.a, EdWelcome_datQ27.7$EdinvsWelcome, EdWelcome_datQ27.7)
analysis <- polr(EdWelcome_datQ27.7$Q27.7.a ~ EdWelcome_datQ27.7$EdinvsWelcome, EdWelcome_datQ27.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.8<-multidatCleanWelcome(Q27.8.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.8.a + EdinvsWelcome, data = EdWelcome_datQ27.8)
conTable
detach(dat_long)
EdWelcome_datQ27.8$Q27.8.a[(EdWelcome_datQ27.8$Q27.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.8<- ordinaldatWelcomeClean (EdWelcome_datQ27.8, reverse=TRUE)
EdWelcome_datQ27.8$EdinvsWelcome<-factor(EdWelcome_datQ27.8$EdinvsWelcome)
EdWelcome_datQ27.8$Q27.8.a<-factor(EdWelcome_datQ27.8$Q27.8.a)
prep <- analysisPrep(EdWelcome_datQ27.8$Q27.8.a, EdWelcome_datQ27.8$EdinvsWelcome, EdWelcome_datQ27.8)
analysis <- polr(EdWelcome_datQ27.8$Q27.8.a ~ EdWelcome_datQ27.8$EdinvsWelcome, EdWelcome_datQ27.8, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.8.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.9<-multidatCleanWelcome(Q27.9.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.9.a + EdinvsWelcome, data = EdWelcome_datQ27.9)
conTable
detach(dat_long)
EdWelcome_datQ27.9$Q27.9.a[(EdWelcome_datQ27.9$Q27.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.9<- ordinaldatWelcomeClean(EdWelcome_datQ27.9)
EdWelcome_datQ27.9$EdinvsWelcome<-factor(EdWelcome_datQ27.9$EdinvsWelcome)
EdWelcome_datQ27.9$Q27.9.a<-factor(EdWelcome_datQ27.9$Q27.9.a)
prep <- analysisPrep(EdWelcome_datQ27.9$Q27.9.a, EdWelcome_datQ27.9$EdinvsWelcome, EdWelcome_datQ27.9)
analysis <- polr(EdWelcome_datQ27.9$Q27.9.a ~ EdWelcome_datQ27.9$EdinvsWelcome, EdWelcome_datQ27.9, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.9.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ27.10<-multidatCleanWelcome(Q27.10.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.10.a + EdinvsWelcome, data = EdWelcome_datQ27.10)
conTable
detach(dat_long)
EdWelcome_datQ27.10$Q27.10.a[(EdWelcome_datQ27.10$Q27.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.10<- ordinaldatWelcomeClean (EdWelcome_datQ27.10, reverse=FALSE)
EdWelcome_datQ27.10$EdinvsWelcome<-factor(EdWelcome_datQ27.10$EdinvsWelcome)
EdWelcome_datQ27.10$Q27.10.a<-factor(EdWelcome_datQ27.10$Q27.10.a)
prep <- analysisPrep(EdWelcome_datQ27.10$Q27.10.a, EdWelcome_datQ27.10$EdinvsWelcome, EdWelcome_datQ27.10)
analysis <- polr(EdWelcome_datQ27.10$Q27.10.a ~ EdWelcome_datQ27.10$EdinvsWelcome, EdWelcome_datQ27.10, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.10.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ27.11<-multidatCleanWelcome(Q27.11.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q27.11.a + EdinvsWelcome, data = EdWelcome_datQ27.11)
conTable
detach(dat_long)
EdWelcome_datQ27.11$Q27.11.a[(EdWelcome_datQ27.11$Q27.11.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ27.11<- ordinaldatWelcomeClean (EdWelcome_datQ27.11, reverse=FALSE)
EdWelcome_datQ27.11$EdinvsWelcome<-factor(EdWelcome_datQ27.11$EdinvsWelcome)
EdWelcome_datQ27.11$Q27.11.a<-factor(EdWelcome_datQ27.11$Q27.11.a)
prep <- analysisPrep(EdWelcome_datQ27.11$Q27.11.a, EdWelcome_datQ27.11$EdinvsWelcome, EdWelcome_datQ27.11)
analysis <- polr(EdWelcome_datQ27.11$Q27.11.a ~ EdWelcome_datQ27.11$EdinvsWelcome, EdWelcome_datQ27.11, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q27.11.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ29<-multidatCleanWelcome(Q29, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q29 + EdinvsWelcome, data = EdWelcome_datQ29)
detach(dat_long)
EdWelcome_datQ29<-ordinaldatCleanbin(EdWelcome_datQ29$Q29, EdWelcome_datQ29)
prep <- analysisPrep(EdWelcome_datQ29$CatOutcome, EdWelcome_datQ29$EdinvsWelcome, EdWelcome_datQ29)
analysis <- glm(EdWelcome_datQ29$CatOutcome ~ EdWelcome_datQ29$EdinvsWelcome, family = binomial, data=EdWelcome_datQ29)
analysisSummary <- summary(analysis)
ci <- confint(analysis) # default method gives profiled CIs
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
Question <- "Q29"
New_OR_Outcomes <- data.frame(Question, OR[2,1], OR[2,2], OR[2,3])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ30<-multidatCleanWelcome(Q30, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q30 + EdinvsWelcome, data = EdWelcome_datQ30)
detach(dat_long)
EdWelcome_datQ30<-ordinaldatCleanbin(EdWelcome_datQ30$Q30, EdWelcome_datQ30)
prep <- analysisPrep(EdWelcome_datQ30$CatOutcome, EdWelcome_datQ30$EdinvsWelcome, EdWelcome_datQ30)
analysis <- glm(EdWelcome_datQ30$CatOutcome ~ EdWelcome_datQ30$EdinvsWelcome, family = binomial, data=EdWelcome_datQ30)
analysisSummary <- summary(analysis)
ci <- confint(analysis) # default method gives profiled CIs
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
Question <- "Q30"
New_OR_Outcomes <- data.frame(Question, OR[2,1], OR[2,2], OR[2,3])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ31<-multidatCleanWelcome(Q31, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q31 + EdinvsWelcome, data = EdWelcome_datQ31)
detach(dat_long)
EdWelcome_datQ31<-ordinaldatCleanbin(EdWelcome_datQ31$Q31, EdWelcome_datQ31)
prep <- analysisPrep(EdWelcome_datQ31$CatOutcome, EdWelcome_datQ31$EdinvsWelcome, EdWelcome_datQ31)
analysis <- glm(EdWelcome_datQ31$CatOutcome ~ EdWelcome_datQ31$EdinvsWelcome, family = binomial, data=EdWelcome_datQ31)
analysisSummary <- summary(analysis)
ci <- confint(analysis) # default method gives profiled CIs
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
Question <- "Q31"
New_OR_Outcomes <- data.frame(Question, OR[2,1], OR[2,2], OR[2,3])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ32<-multidatCleanWelcome(Q32, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q32 + EdinvsWelcome, data = EdWelcome_datQ32)
detach(dat_long)
EdWelcome_datQ32<-ordinaldatCleanbin(EdWelcome_datQ32$Q32, EdWelcome_datQ32)
prep <- analysisPrep(EdWelcome_datQ32$CatOutcome, EdWelcome_datQ32$EdinvsWelcome, EdWelcome_datQ32)
analysis <- glm(EdWelcome_datQ32$CatOutcome ~ EdWelcome_datQ32$EdinvsWelcome, family = binomial, data=EdWelcome_datQ32)
analysisSummary <- summary(analysis)
ci <- confint(analysis) # default method gives profiled CIs
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
Question <- "Q32"
New_OR_Outcomes <- data.frame(Question, OR[2,1], OR[2,2], OR[2,3])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ37.1<-multidatCleanWelcome(Q37.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.1.a + EdinvsWelcome, data = EdWelcome_datQ37.1)
conTable
detach(dat_long)
EdWelcome_datQ37.1$Q37.1.a[(EdWelcome_datQ37.1$Q37.1.a == "8")]=NA
EdWelcome_datQ37.1<-ordinaldatWelcomeClean(EdWelcome_datQ37.1)
EdWelcome_datQ37.1$EdinvsWelcome<-factor(EdWelcome_datQ37.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ37.1$Q37.1.a, EdWelcome_datQ37.1$EdinvsWelcome, EdWelcome_datQ37.1)
analysis <- polr(EdWelcome_datQ37.1$Q37.1.a ~ EdWelcome_datQ37.1$EdinvsWelcome, EdWelcome_datQ37.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ37.2<-multidatCleanWelcome(Q37.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.2.a + EdinvsWelcome, data = EdWelcome_datQ37.2)
conTable
detach(dat_long)
EdWelcome_datQ37.2$Q37.2.a[(EdWelcome_datQ37.2$Q37.2.a == "8")]=NA
EdWelcome_datQ37.2<-ordinaldatWelcomeClean(EdWelcome_datQ37.2)
EdWelcome_datQ37.2$EdinvsWelcome<-factor(EdWelcome_datQ37.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ37.2$Q37.2.a, EdWelcome_datQ37.2$EdinvsWelcome, EdWelcome_datQ37.2)
analysis <- polr(EdWelcome_datQ37.2$Q37.2.a ~ EdWelcome_datQ37.2$EdinvsWelcome, EdWelcome_datQ37.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ37.3<-multidatCleanWelcome(Q37.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.3.a + EdinvsWelcome, data = EdWelcome_datQ37.3)
conTable
detach(dat_long)
EdWelcome_datQ37.3$Q37.3.a[(EdWelcome_datQ37.3$Q37.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.3<-ordinaldatWelcomeClean(EdWelcome_datQ37.3)
EdWelcome_datQ37.3$EdinvsWelcome<-factor(EdWelcome_datQ37.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ37.3$Q37.3.a, EdWelcome_datQ37.3$EdinvsWelcome, EdWelcome_datQ37.3)
analysis <- polr(EdWelcome_datQ37.3$Q37.3.a ~ EdWelcome_datQ37.3$EdinvsWelcome, EdWelcome_datQ37.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.4<-multidatCleanWelcome(Q37.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.4.a + EdinvsWelcome, data = EdWelcome_datQ37.4)
conTable
detach(dat_long)
EdWelcome_datQ37.4$Q37.4.a[(EdWelcome_datQ37.4$Q37.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.4<-ordinaldatWelcomeClean(EdWelcome_datQ37.4)
EdWelcome_datQ37.4$EdinvsWelcome<-factor(EdWelcome_datQ37.4$EdinvsWelcome)
EdWelcome_datQ37.4$Q37.4.a<-factor(EdWelcome_datQ37.4$Q37.4.a)
prep <- analysisPrep(EdWelcome_datQ37.4$Q37.4.a, EdWelcome_datQ37.4$EdinvsWelcome, EdWelcome_datQ37.4)
analysis <- polr(EdWelcome_datQ37.4$Q37.4.a ~ EdWelcome_datQ37.4$EdinvsWelcome, EdWelcome_datQ37.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.5<-multidatCleanWelcome(Q37.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.5.a + EdinvsWelcome, data = EdWelcome_datQ37.5)
conTable
detach(dat_long)
EdWelcome_datQ37.5$Q37.5.a[(EdWelcome_datQ37.5$Q37.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.5<- ordinaldatWelcomeClean (EdWelcome_datQ37.5, reverse=TRUE)
EdWelcome_datQ37.5$EdinvsWelcome<-factor(EdWelcome_datQ37.5$EdinvsWelcome)
EdWelcome_datQ37.5$Q37.5.a<-factor(EdWelcome_datQ37.5$Q37.5.a)
prep <- analysisPrep(EdWelcome_datQ37.5$Q37.5.a, EdWelcome_datQ37.5$EdinvsWelcome, EdWelcome_datQ37.5)
analysis <- polr(EdWelcome_datQ37.5$Q37.5.a ~ EdWelcome_datQ37.5$EdinvsWelcome, EdWelcome_datQ37.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.6<-multidatCleanWelcome(Q37.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.6.a + EdinvsWelcome, data = EdWelcome_datQ37.6)
conTable
detach(dat_long)
EdWelcome_datQ37.6$Q37.6.a[(EdWelcome_datQ37.6$Q37.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.6<- ordinaldatWelcomeClean (EdWelcome_datQ37.6, reverse=TRUE)
EdWelcome_datQ37.6$EdinvsWelcome<-factor(EdWelcome_datQ37.6$EdinvsWelcome)
EdWelcome_datQ37.6$Q37.6.a<-factor(EdWelcome_datQ37.6$Q37.6.a)
prep <- analysisPrep(EdWelcome_datQ37.6$Q37.6.a, EdWelcome_datQ37.6$EdinvsWelcome, EdWelcome_datQ37.6)
analysis <- polr(EdWelcome_datQ37.6$Q37.6.a ~ EdWelcome_datQ37.6$EdinvsWelcome, EdWelcome_datQ37.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.7<-multidatCleanWelcome(Q37.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.7.a + EdinvsWelcome, data = EdWelcome_datQ37.7)
conTable
detach(dat_long)
EdWelcome_datQ37.7$Q37.7.a[(EdWelcome_datQ37.7$Q37.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.7<- ordinaldatWelcomeClean (EdWelcome_datQ37.7, reverse=TRUE)
EdWelcome_datQ37.7$EdinvsWelcome<-factor(EdWelcome_datQ37.7$EdinvsWelcome)
EdWelcome_datQ37.7$Q37.7.a<-factor(EdWelcome_datQ37.7$Q37.7.a)
prep <- analysisPrep(EdWelcome_datQ37.7$Q37.7.a, EdWelcome_datQ37.7$EdinvsWelcome, EdWelcome_datQ37.7)
analysis <- polr(EdWelcome_datQ37.7$Q37.7.a ~ EdWelcome_datQ37.7$EdinvsWelcome, EdWelcome_datQ37.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.8<-multidatCleanWelcome(Q37.8.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.8.a + EdinvsWelcome, data = EdWelcome_datQ37.8)
conTable
detach(dat_long)
EdWelcome_datQ37.8$Q37.8.a[(EdWelcome_datQ37.8$Q37.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.8<- ordinaldatWelcomeClean (EdWelcome_datQ37.8, reverse=FALSE)
EdWelcome_datQ37.8$EdinvsWelcome<-factor(EdWelcome_datQ37.8$EdinvsWelcome)
EdWelcome_datQ37.8$Q37.8.a<-factor(EdWelcome_datQ37.8$Q37.8.a)
prep <- analysisPrep(EdWelcome_datQ37.8$Q37.8.a, EdWelcome_datQ37.8$EdinvsWelcome, EdWelcome_datQ37.8)
analysis <- polr(EdWelcome_datQ37.8$Q37.8.a ~ EdWelcome_datQ37.8$EdinvsWelcome, EdWelcome_datQ37.8, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.8.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.9<-multidatCleanWelcome(Q37.9.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.9.a + EdinvsWelcome, data = EdWelcome_datQ37.9)
conTable
detach(dat_long)
EdWelcome_datQ37.9$Q37.9.a[(EdWelcome_datQ37.9$Q37.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.9<- ordinaldatWelcomeClean(EdWelcome_datQ37.9, reverse=TRUE)
EdWelcome_datQ37.9$EdinvsWelcome<-factor(EdWelcome_datQ37.9$EdinvsWelcome)
EdWelcome_datQ37.9$Q37.9.a<-factor(EdWelcome_datQ37.9$Q37.9.a)
prep <- analysisPrep(EdWelcome_datQ37.9$Q37.9.a, EdWelcome_datQ37.9$EdinvsWelcome, EdWelcome_datQ37.9)
analysis <- polr(EdWelcome_datQ37.9$Q37.9.a ~ EdWelcome_datQ37.9$EdinvsWelcome, EdWelcome_datQ37.9, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.9.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ37.10<-multidatCleanWelcome(Q37.10.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.10.a + EdinvsWelcome, data = EdWelcome_datQ37.10)
conTable
detach(dat_long)
EdWelcome_datQ37.10$Q37.10.a[(EdWelcome_datQ37.10$Q37.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.10<- ordinaldatWelcomeClean (EdWelcome_datQ37.10, reverse=FALSE)
EdWelcome_datQ37.10$EdinvsWelcome<-factor(EdWelcome_datQ37.10$EdinvsWelcome)
EdWelcome_datQ37.10$Q37.10.a<-factor(EdWelcome_datQ37.10$Q37.10.a)
prep <- analysisPrep(EdWelcome_datQ37.10$Q37.10.a, EdWelcome_datQ37.10$EdinvsWelcome, EdWelcome_datQ37.10)
analysis <- polr(EdWelcome_datQ37.10$Q37.10.a ~ EdWelcome_datQ37.10$EdinvsWelcome, EdWelcome_datQ37.10, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.10.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.11<-multidatCleanWelcome(Q37.11.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.11.a + EdinvsWelcome, data = EdWelcome_datQ37.11)
conTable
detach(dat_long)
EdWelcome_datQ37.11$Q37.11.a[(EdWelcome_datQ37.11$Q37.11.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.11<- ordinaldatWelcomeClean (EdWelcome_datQ37.11, reverse=FALSE)
EdWelcome_datQ37.11$EdinvsWelcome<-factor(EdWelcome_datQ37.11$EdinvsWelcome)
EdWelcome_datQ37.11$Q37.11.a<-factor(EdWelcome_datQ37.11$Q37.11.a)
prep <- analysisPrep(EdWelcome_datQ37.11$Q37.11.a, EdWelcome_datQ37.11$EdinvsWelcome, EdWelcome_datQ37.11)
analysis <- polr(EdWelcome_datQ37.11$Q37.11.a ~ EdWelcome_datQ37.11$EdinvsWelcome, EdWelcome_datQ37.11, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.11.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.12<-multidatCleanWelcome(Q37.12.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.12.a + EdinvsWelcome, data = EdWelcome_datQ37.12)
conTable
detach(dat_long)
EdWelcome_datQ37.12$Q37.12.a[(EdWelcome_datQ37.12$Q37.12.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.12<- ordinaldatWelcomeClean (EdWelcome_datQ37.12)
EdWelcome_datQ37.12$EdinvsWelcome<-factor(EdWelcome_datQ37.12$EdinvsWelcome)
EdWelcome_datQ37.12$Q37.12.a<-factor(EdWelcome_datQ37.12$Q37.12.a)
prep <- analysisPrep(EdWelcome_datQ37.12$Q37.12.a, EdWelcome_datQ37.12$EdinvsWelcome, EdWelcome_datQ37.12)
analysis <- polr(EdWelcome_datQ37.12$Q37.12.a ~ EdWelcome_datQ37.12$EdinvsWelcome, EdWelcome_datQ37.12, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.12.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.13<-multidatCleanWelcome(Q37.13.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.13.a + EdinvsWelcome, data = EdWelcome_datQ37.13)
conTable
detach(dat_long)
EdWelcome_datQ37.13$Q37.13.a[(EdWelcome_datQ37.13$Q37.13.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.13<- ordinaldatWelcomeClean (EdWelcome_datQ37.13, reverse=FALSE)
EdWelcome_datQ37.13$EdinvsWelcome<-factor(EdWelcome_datQ37.13$EdinvsWelcome)
EdWelcome_datQ37.13$Q37.13.a<-factor(EdWelcome_datQ37.13$Q37.13.a)
prep <- analysisPrep(EdWelcome_datQ37.13$Q37.13.a, EdWelcome_datQ37.13$EdinvsWelcome, EdWelcome_datQ37.13)
analysis <- polr(EdWelcome_datQ37.13$Q37.13.a ~ EdWelcome_datQ37.13$EdinvsWelcome, EdWelcome_datQ37.13, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.13.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #``` #```

EdWelcome_datQ37.14<-multidatCleanWelcome(Q37.14.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.14.a + EdinvsWelcome, data = EdWelcome_datQ37.14)
conTable
detach(dat_long)
EdWelcome_datQ37.14$Q37.14.a[(EdWelcome_datQ37.14$Q37.14.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.14<-ordinaldatWelcomeClean(EdWelcome_datQ37.14, reverse=TRUE)
EdWelcome_datQ37.14$EdinvsWelcome<-factor(EdWelcome_datQ37.14$EdinvsWelcome)
EdWelcome_datQ37.14$Q37.14.a<-factor(EdWelcome_datQ37.14$Q37.14.a)
prep <- analysisPrep(EdWelcome_datQ37.14$Q37.14.a, EdWelcome_datQ37.14$EdinvsWelcome, EdWelcome_datQ37.14)
analysis <- polr(EdWelcome_datQ37.14$Q37.14.a ~ EdWelcome_datQ37.14$EdinvsWelcome, EdWelcome_datQ37.14, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.14.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ37.15<-multidatCleanWelcome(Q37.15.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q37.15.a + EdinvsWelcome, data = EdWelcome_datQ37.15)
conTable
detach(dat_long)
EdWelcome_datQ37.15$Q37.15.a[(EdWelcome_datQ37.15$Q37.15.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ37.15<-ordinaldatWelcomeClean(EdWelcome_datQ37.15)
EdWelcome_datQ37.15$EdinvsWelcome<-factor(EdWelcome_datQ37.15$EdinvsWelcome)
EdWelcome_datQ37.15$Q37.15.a<-factor(EdWelcome_datQ37.15$Q37.15.a)
prep <- analysisPrep(EdWelcome_datQ37.15$Q37.15.a, EdWelcome_datQ37.15$EdinvsWelcome, EdWelcome_datQ37.15)
analysis <- polr(EdWelcome_datQ37.15$Q37.15.a ~ EdWelcome_datQ37.15$EdinvsWelcome, EdWelcome_datQ37.15, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q37.15.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

####Q38.1
EdWelcome_datQ38.1<-cbind(EdWelcome_dat$Q38.1, EdWelcome_dat$UniqueResponseNumber, EdWelcome_dat$EdinvsWelcome)
colnames(EdWelcome_datQ38.1) <- c("Q38.1","URN","EdinvsWelcome") 
conTable <- xtabs(~Q38.1 + EdinvsWelcome, data = EdWelcome_datQ38.1)
conTable
detach(dat_long)
EdWelcome_datQ38.1[(EdWelcome_datQ38.1 == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ38.1[,3]<-factor(EdWelcome_datQ38.1[,3])
EdWelcome_datQ38.1[,1]<-factor(EdWelcome_datQ38.1[,1])
EdWelcome_datQ38.1 <- as.data.frame(EdWelcome_datQ38.1)
prep <- analysisPrep(EdWelcome_datQ38.1$Q38.1, EdWelcome_datQ38.1$EdinvsWelcome, EdWelcome_datQ38.1)
analysis <- polr(EdWelcome_datQ38.1$Q38.1 ~ EdWelcome_datQ38.1$EdinvsWelcome, EdWelcome_datQ38.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q38.1"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```
EdWelcome_datQ40.1<-multidatCleanWelcome(Q40.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q40.1.a + EdinvsWelcome, data = EdWelcome_datQ40.1)
conTable
detach(dat_long)
EdWelcome_datQ40.1$Q40.1.a[(EdWelcome_datQ40.1$Q40.1.a == "8")]=NA
EdWelcome_datQ40.1<-ordinaldatWelcomeClean(EdWelcome_datQ40.1, reverse=TRUE)
EdWelcome_datQ40.1$EdinvsWelcome<-factor(EdWelcome_datQ40.1$EdinvsWelcome)
EdWelcome_datQ40.1$Q40.1.a<-factor(EdWelcome_datQ40.1$Q40.1.a)
prep <- analysisPrep(EdWelcome_datQ40.1$Q40.1.a, EdWelcome_datQ40.1$EdinvsWelcome, EdWelcome_datQ40.1)
analysis <- polr(EdWelcome_datQ40.1$Q40.1.a ~ EdWelcome_datQ40.1$EdinvsWelcome, EdWelcome_datQ40.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q40.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ40.2<-multidatCleanWelcome(Q40.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q40.2.a + EdinvsWelcome, data = EdWelcome_datQ40.2)
conTable
detach(dat_long)
EdWelcome_datQ40.2$Q40.2.a[(EdWelcome_datQ40.2$Q40.2.a == "8")]=NA
EdWelcome_datQ40.2<-ordinaldatWelcomeClean(EdWelcome_datQ40.2)
EdWelcome_datQ40.2$EdinvsWelcome<-factor(EdWelcome_datQ40.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ40.2$Q40.2.a, EdWelcome_datQ40.2$EdinvsWelcome, EdWelcome_datQ40.2)
analysis <- polr(EdWelcome_datQ40.2$Q40.2.a ~ EdWelcome_datQ40.2$EdinvsWelcome, EdWelcome_datQ40.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q40.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ40.3<-multidatCleanWelcome(Q40.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q40.3.a + EdinvsWelcome, data = EdWelcome_datQ40.3)
conTable
detach(dat_long)
EdWelcome_datQ40.3$Q40.3.a[(EdWelcome_datQ40.3$Q40.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ40.3<-ordinaldatWelcomeClean(EdWelcome_datQ40.3)
EdWelcome_datQ40.3$EdinvsWelcome<-factor(EdWelcome_datQ40.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ40.3$Q40.3.a, EdWelcome_datQ40.3$EdinvsWelcome, EdWelcome_datQ40.3)
analysis <- polr(EdWelcome_datQ40.3$Q40.3.a ~ EdWelcome_datQ40.3$EdinvsWelcome, EdWelcome_datQ40.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q40.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ40.4<-multidatCleanWelcome(Q40.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q40.4.a + EdinvsWelcome, data = EdWelcome_datQ40.4)
conTable
detach(dat_long)
EdWelcome_datQ40.4$Q40.4.a[(EdWelcome_datQ40.4$Q40.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ40.4<-ordinaldatWelcomeClean(EdWelcome_datQ40.4, reverse=TRUE)
EdWelcome_datQ40.4$EdinvsWelcome<-factor(EdWelcome_datQ40.4$EdinvsWelcome)
EdWelcome_datQ40.4$Q40.4.a<-factor(EdWelcome_datQ40.4$Q40.4.a)
prep <- analysisPrep(EdWelcome_datQ40.4$Q40.4.a, EdWelcome_datQ40.4$EdinvsWelcome, EdWelcome_datQ40.4)
analysis <- polr(EdWelcome_datQ40.4$Q40.4.a ~ EdWelcome_datQ40.4$EdinvsWelcome, EdWelcome_datQ40.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q40.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ40.5<-multidatCleanWelcome(Q40.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q40.5.a + EdinvsWelcome, data = EdWelcome_datQ40.5)
conTable
detach(dat_long)
EdWelcome_datQ40.5$Q40.5.a[(EdWelcome_datQ40.5$Q40.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ40.5<- ordinaldatWelcomeClean (EdWelcome_datQ40.5)
EdWelcome_datQ40.5$EdinvsWelcome<-factor(EdWelcome_datQ40.5$EdinvsWelcome)
EdWelcome_datQ40.5$Q40.5.a<-factor(EdWelcome_datQ40.5$Q40.5.a)
prep <- analysisPrep(EdWelcome_datQ40.5$Q40.5.a, EdWelcome_datQ40.5$EdinvsWelcome, EdWelcome_datQ40.5)
analysis <- polr(EdWelcome_datQ40.5$Q40.5.a ~ EdWelcome_datQ40.5$EdinvsWelcome, EdWelcome_datQ40.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q40.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.1<-multidatCleanWelcome(Q41.1.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.1.a + EdinvsWelcome, data = EdWelcome_datQ41.1)
conTable
detach(dat_long)
EdWelcome_datQ41.1$Q41.1.a[(EdWelcome_datQ41.1$Q41.1.a == "8")]=NA
EdWelcome_datQ41.1<-ordinaldatWelcomeClean(EdWelcome_datQ41.1)
EdWelcome_datQ41.1$EdinvsWelcome<-factor(EdWelcome_datQ41.1$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ41.1$Q41.1.a, EdWelcome_datQ41.1$EdinvsWelcome, EdWelcome_datQ41.1)
analysis <- polr(EdWelcome_datQ41.1$Q41.1.a ~ EdWelcome_datQ41.1$EdinvsWelcome, EdWelcome_datQ41.1, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.1.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ41.2<-multidatCleanWelcome(Q41.2.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.2.a + EdinvsWelcome, data = EdWelcome_datQ41.2)
conTable
detach(dat_long)
EdWelcome_datQ41.2$Q41.2.a[(EdWelcome_datQ41.2$Q41.2.a == "8")]=NA
EdWelcome_datQ41.2<-ordinaldatWelcomeClean(EdWelcome_datQ41.2)
EdWelcome_datQ41.2$EdinvsWelcome<-factor(EdWelcome_datQ41.2$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ41.2$Q41.2.a, EdWelcome_datQ41.2$EdinvsWelcome, EdWelcome_datQ41.2)
analysis <- polr(EdWelcome_datQ41.2$Q41.2.a ~ EdWelcome_datQ41.2$EdinvsWelcome, EdWelcome_datQ41.2, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.2.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ41.3<-multidatCleanWelcome(Q41.3.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.3.a + EdinvsWelcome, data = EdWelcome_datQ41.3)
conTable
detach(dat_long)
EdWelcome_datQ41.3$Q41.3.a[(EdWelcome_datQ41.3$Q41.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.3<-ordinaldatWelcomeClean(EdWelcome_datQ41.3)
EdWelcome_datQ41.3$EdinvsWelcome<-factor(EdWelcome_datQ41.3$EdinvsWelcome)
prep <- analysisPrep(EdWelcome_datQ41.3$Q41.3.a, EdWelcome_datQ41.3$EdinvsWelcome, EdWelcome_datQ41.3)
analysis <- polr(EdWelcome_datQ41.3$Q41.3.a ~ EdWelcome_datQ41.3$EdinvsWelcome, EdWelcome_datQ41.3, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.3.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.4<-multidatCleanWelcome(Q41.4.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.4.a + EdinvsWelcome, data = EdWelcome_datQ41.4)
conTable
detach(dat_long)
EdWelcome_datQ41.4$Q41.4.a[(EdWelcome_datQ41.4$Q41.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.4<-ordinaldatWelcomeClean(EdWelcome_datQ41.4, reverse=TRUE)
EdWelcome_datQ41.4$EdinvsWelcome<-factor(EdWelcome_datQ41.4$EdinvsWelcome)
EdWelcome_datQ41.4$Q41.4.a<-factor(EdWelcome_datQ41.4$Q41.4.a)
prep <- analysisPrep(EdWelcome_datQ41.4$Q41.4.a, EdWelcome_datQ41.4$EdinvsWelcome, EdWelcome_datQ41.4)
analysis <- polr(EdWelcome_datQ41.4$Q41.4.a ~ EdWelcome_datQ41.4$EdinvsWelcome, EdWelcome_datQ41.4, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.4.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.5<-multidatCleanWelcome(Q41.5.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.5.a + EdinvsWelcome, data = EdWelcome_datQ41.5)
conTable
detach(dat_long)
EdWelcome_datQ41.5$Q41.5.a[(EdWelcome_datQ41.5$Q41.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.5<- ordinaldatWelcomeClean(EdWelcome_datQ41.5, reverse=TRUE)
EdWelcome_datQ41.5$EdinvsWelcome<-factor(EdWelcome_datQ41.5$EdinvsWelcome)
EdWelcome_datQ41.5$Q41.5.a<-factor(EdWelcome_datQ41.5$Q41.5.a)
prep <- analysisPrep(EdWelcome_datQ41.5$Q41.5.a, EdWelcome_datQ41.5$EdinvsWelcome, EdWelcome_datQ41.5)
analysis <- polr(EdWelcome_datQ41.5$Q41.5.a ~ EdWelcome_datQ41.5$EdinvsWelcome, EdWelcome_datQ41.5, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.5.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.6<-multidatCleanWelcome(Q41.6.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.6.a + EdinvsWelcome, data = EdWelcome_datQ41.6)
conTable
detach(dat_long)
EdWelcome_datQ41.6$Q41.6.a[(EdWelcome_datQ41.6$Q41.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.6<- ordinaldatWelcomeClean (EdWelcome_datQ41.6, reverse=TRUE)
EdWelcome_datQ41.6$EdinvsWelcome<-factor(EdWelcome_datQ41.6$EdinvsWelcome)
EdWelcome_datQ41.6$Q41.6.a<-factor(EdWelcome_datQ41.6$Q41.6.a)
prep <- analysisPrep(EdWelcome_datQ41.6$Q41.6.a, EdWelcome_datQ41.6$EdinvsWelcome, EdWelcome_datQ41.6)
analysis <- polr(EdWelcome_datQ41.6$Q41.6.a ~ EdWelcome_datQ41.6$EdinvsWelcome, EdWelcome_datQ41.6, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.6.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.7<-multidatCleanWelcome(Q41.7.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.7.a + EdinvsWelcome, data = EdWelcome_datQ41.7)
conTable
detach(dat_long)
EdWelcome_datQ41.7$Q41.7.a[(EdWelcome_datQ41.7$Q41.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.7<- ordinaldatWelcomeClean (EdWelcome_datQ41.7)
EdWelcome_datQ41.7$EdinvsWelcome<-factor(EdWelcome_datQ41.7$EdinvsWelcome)
EdWelcome_datQ41.7$Q41.7.a<-factor(EdWelcome_datQ41.7$Q41.7.a)
prep <- analysisPrep(EdWelcome_datQ41.7$Q41.7.a, EdWelcome_datQ41.7$EdinvsWelcome, EdWelcome_datQ41.7)
analysis <- polr(EdWelcome_datQ41.7$Q41.7.a ~ EdWelcome_datQ41.7$EdinvsWelcome, EdWelcome_datQ41.7, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.7.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.8<-multidatCleanWelcome(Q41.8.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.8.a + EdinvsWelcome, data = EdWelcome_datQ41.8)
conTable
detach(dat_long)
EdWelcome_datQ41.8$Q41.8.a[(EdWelcome_datQ41.8$Q41.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.8<- ordinaldatWelcomeClean (EdWelcome_datQ41.8, reverse=FALSE)
EdWelcome_datQ41.8$EdinvsWelcome<-factor(EdWelcome_datQ41.8$EdinvsWelcome)
EdWelcome_datQ41.8$Q41.8.a<-factor(EdWelcome_datQ41.8$Q41.8.a)
prep <- analysisPrep(EdWelcome_datQ41.8$Q41.8.a, EdWelcome_datQ41.8$EdinvsWelcome, EdWelcome_datQ41.8)
analysis <- polr(EdWelcome_datQ41.8$Q41.8.a ~ EdWelcome_datQ41.8$EdinvsWelcome, EdWelcome_datQ41.8, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.8.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

EdWelcome_datQ41.9<-multidatCleanWelcome(Q41.9.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.9.a + EdinvsWelcome, data = EdWelcome_datQ41.9)
conTable
detach(dat_long)
EdWelcome_datQ41.9$Q41.9.a[(EdWelcome_datQ41.9$Q41.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.9<- ordinaldatWelcomeClean(EdWelcome_datQ41.9)
EdWelcome_datQ41.9$EdinvsWelcome<-factor(EdWelcome_datQ41.9$EdinvsWelcome)
EdWelcome_datQ41.9$Q41.9.a<-factor(EdWelcome_datQ41.9$Q41.9.a)
prep <- analysisPrep(EdWelcome_datQ41.9$Q41.9.a, EdWelcome_datQ41.9$EdinvsWelcome, EdWelcome_datQ41.9)
analysis <- polr(EdWelcome_datQ41.9$Q41.9.a ~ EdWelcome_datQ41.9$EdinvsWelcome, EdWelcome_datQ41.9, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.9.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)

EdWelcome_datQ41.10<-multidatCleanWelcome(Q41.10.a, EdinvsWelcome, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q41.10.a + EdinvsWelcome, data = EdWelcome_datQ41.10)
conTable
detach(dat_long)
EdWelcome_datQ41.10$Q41.10.a[(EdWelcome_datQ41.10$Q41.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ41.10<- ordinaldatWelcomeClean (EdWelcome_datQ41.10, reverse=TRUE)
EdWelcome_datQ41.10$EdinvsWelcome<-factor(EdWelcome_datQ41.10$EdinvsWelcome)
EdWelcome_datQ41.10$Q41.10.a<-factor(EdWelcome_datQ41.10$Q41.10.a)
prep <- analysisPrep(EdWelcome_datQ41.10$Q41.10.a, EdWelcome_datQ41.10$EdinvsWelcome, EdWelcome_datQ41.10)
analysis <- polr(EdWelcome_datQ41.10$Q41.10.a ~ EdWelcome_datQ41.10$EdinvsWelcome, EdWelcome_datQ41.10, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
OR <- exp(cbind(OR = coef(analysis), ci))
result <- list(prep=prep, assumption = assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q41.10.a"
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data) #```

"OR > 1 indicates WT responses ranked better"
OR_Outcomes[,5] <- 1/(OR_Outcomes[,2])
OR_Outcomes[,6] <- 1/(OR_Outcomes[,4])
OR_Outcomes[,7] <- 1/(OR_Outcomes[,3])
OR_Outcomes <- OR_Outcomes[,-2]
OR_Outcomes <- OR_Outcomes[,-2]
OR_Outcomes <- OR_Outcomes[,-2]

"Correction so OR > 1 indicates UEdin responses ranked better"
OR_Outcomes
