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
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
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
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
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
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
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
New_OR_Outcomes <- data.frame(Question, OR[1,1], OR[1,2], OR[2,2])
colnames(New_OR_Outcomes) <-(c("Question",  "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, New_OR_Outcomes)
detach(data)





