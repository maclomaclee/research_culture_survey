source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor[c(1,239,109:182, 6, 225,227,229,231:234,240:243)]
employ_dat$Q3 <- as.character(employ_dat$Q3)

"Q29. During your research career have you ever experienced bullying or harassment?"
"Status"
employ_datQ29<-multidatClean(Q29, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q29 + Academic, data = employ_datQ29)
detach(dat_long)
employ_datQ29<-ordinaldatCleanbin(employ_datQ29$Q29, employ_datQ29)
#ordinal(employ_datQ29$CatOutcome, employ_datQ29$Academic, employ_datQ29)
prep <- analysisPrep(employ_datQ29$CatOutcome, employ_datQ29$Academic, employ_datQ29)
analysis <- glm(employ_datQ29$CatOutcome ~ employ_datQ29$Academic, family = binomial, data=employ_datQ29)
##assumption<-brant(analysis)
analysisSummary <- summary(analysis)
##ctable <- coef(summary(analysis))
##p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
##ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q29"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ29Carer<-multidatClean(Q29, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29Carer$Carer<- factor(employ_datQ29Carer$Carer)
employ_datQ29Carer<-ordinaldatCleanbin(employ_datQ29Carer$Q29, employ_datQ29Carer)
#ordinal(employ_datQ29Carer$CatOutcome, employ_datQ29Carer$Carer, employ_datQ29Carer)
prep <- analysisPrep(employ_datQ29Carer$CatOutcome, employ_datQ29Carer$Carer, employ_datQ29Carer)
analysis <- glm(employ_datQ29Carer$CatOutcome ~ employ_datQ29Carer$Carer, family = binomial, data=employ_datQ29Carer)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ29Disability<-multidatClean(Q29, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29Disability$Disability<- factor(employ_datQ29Disability$Disability)
employ_datQ29Disability<-ordinaldatCleanbin(employ_datQ29Disability$Q29, employ_datQ29Disability)
conTable <- xtabs(~Q29 + Disability, data = employ_datQ29Disability)
#ordinal(employ_datQ29Disability$CatOutcome, employ_datQ29Disability$Disability, employ_datQ29Disability)
prep <- analysisPrep(employ_datQ29Disability$CatOutcome, employ_datQ29Disability$Disability, employ_datQ29Disability)
analysis <- glm(employ_datQ29Disability$CatOutcome ~ employ_datQ29Disability$Disability, family = binomial, data=employ_datQ29Disability)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ29Ethnicity<-multidatClean(Q29, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29Ethnicity$Ethnicity<- factor(employ_datQ29Ethnicity$EthnicityCleaned)
employ_datQ29Ethnicity<-ordinaldatCleanbin(employ_datQ29Ethnicity$Q29, employ_datQ29Ethnicity)
conTable <- xtabs(~Q29 + EthnicityCleaned, data = employ_datQ29Ethnicity)
conTable
#ordinal(employ_datQ29Ethnicity$CatOutcome, employ_datQ29Ethnicity$EthnicityCleaned, employ_datQ29Ethnicity)
prep <- analysisPrep(employ_datQ29Ethnicity$CatOutcome, employ_datQ29Ethnicity$EthnicityCleaned, employ_datQ29Ethnicity)
analysis <- glm(employ_datQ29Ethnicity$CatOutcome ~ employ_datQ29Ethnicity$EthnicityCleaned, family = "binomial", data=employ_datQ29Ethnicity)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ29FirstGen<-multidatClean(Q29, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29FirstGen$FirstGen<-factor(employ_datQ29FirstGen$FirstGen)
employ_datQ29FirstGen<-ordinaldatCleanbin(employ_datQ29FirstGen$Q29, employ_datQ29FirstGen)
#ordinal(employ_datQ29FirstGen$CatOutcome, employ_datQ29FirstGen$FirstGen, employ_datQ29FirstGen)
prep <- analysisPrep(employ_datQ29FirstGen$CatOutcome, employ_datQ29FirstGen$FirstGen, employ_datQ29FirstGen)
analysis <- glm(employ_datQ29FirstGen$CatOutcome ~ employ_datQ29FirstGen$FirstGen, family = binomial, data=employ_datQ29FirstGen)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ29Gender<-multidatClean(Q29, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29Gender$Gender<-factor(employ_datQ29Gender$Gender)
employ_datQ29Gender<-ordinaldatCleanbin(employ_datQ29Gender$Q29, employ_datQ29Gender)
#ordinal(employ_datQ29Gender$CatOutcome, employ_datQ29Gender$Gender, employ_datQ29Gender)
prep <- analysisPrep(employ_datQ29Gender$CatOutcome, employ_datQ29Gender$Gender, employ_datQ29Gender)
analysis <- glm(employ_datQ29Gender$CatOutcome ~ employ_datQ29Gender$Gender, family = binomial, data=employ_datQ29Gender)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ29Sexuality<-multidatClean(Q29, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ29Sexuality$Sexuality<-factor(employ_datQ29Sexuality$Sexuality)
employ_datQ29Sexuality<-ordinaldatCleanbin(employ_datQ29Sexuality$Q29, employ_datQ29Sexuality)
#ordinal(employ_datQ29Sexuality$CatOutcome, employ_datQ29Sexuality$Sexuality, employ_datQ29Sexuality)
prep <- analysisPrep(employ_datQ29Sexuality$CatOutcome, employ_datQ29Sexuality$Sexuality, employ_datQ29Sexuality)
analysis <- glm(employ_datQ29Sexuality$CatOutcome ~ employ_datQ29Sexuality$Sexuality, family = binomial, data=employ_datQ29Sexuality)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q29"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q29_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep_binary (OR_Outcomes)

"Q30. During your research career have you ever witnessed bullying or harassment?"
"Status"
employ_datQ30<-multidatClean(Q30, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q30 + Academic, data = employ_datQ30)
detach(dat_long)
employ_datQ30<-ordinaldatCleanbin(employ_datQ30$Q30, employ_datQ30)
#ordinal(employ_datQ30$CatOutcome, employ_datQ30$Academic, employ_datQ30)
prep <- analysisPrep(employ_datQ30$CatOutcome, employ_datQ30$Academic, employ_datQ30)
analysis <- glm(employ_datQ30$CatOutcome ~ employ_datQ30$Academic, family = binomial, data=employ_datQ30)
##assumption<-brant(analysis)
analysisSummary <- summary(analysis)
##ctable <- coef(summary(analysis))
##p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
##ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q30"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ30Carer<-multidatClean(Q30, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30Carer$Carer<- factor(employ_datQ30Carer$Carer)
employ_datQ30Carer<-ordinaldatCleanbin(employ_datQ30Carer$Q30, employ_datQ30Carer)
#ordinal(employ_datQ30Carer$CatOutcome, employ_datQ30Carer$Carer, employ_datQ30Carer)
prep <- analysisPrep(employ_datQ30Carer$CatOutcome, employ_datQ30Carer$Carer, employ_datQ30Carer)
analysis <- glm(employ_datQ30Carer$CatOutcome ~ employ_datQ30Carer$Carer, family = binomial, data=employ_datQ30Carer)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ30Disability<-multidatClean(Q30, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30Disability$Disability<- factor(employ_datQ30Disability$Disability)
employ_datQ30Disability<-ordinaldatCleanbin(employ_datQ30Disability$Q30, employ_datQ30Disability)
conTable <- xtabs(~Q30 + Disability, data = employ_datQ30Disability)
#ordinal(employ_datQ30Disability$CatOutcome, employ_datQ30Disability$Disability, employ_datQ30Disability)
prep <- analysisPrep(employ_datQ30Disability$CatOutcome, employ_datQ30Disability$Disability, employ_datQ30Disability)
analysis <- glm(employ_datQ30Disability$CatOutcome ~ employ_datQ30Disability$Disability, family = binomial, data=employ_datQ30Disability)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ30Ethnicity<-multidatClean(Q30, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30Ethnicity$Ethnicity<- factor(employ_datQ30Ethnicity$EthnicityCleaned)
employ_datQ30Ethnicity<-ordinaldatCleanbin(employ_datQ30Ethnicity$Q30, employ_datQ30Ethnicity)
conTable <- xtabs(~Q30 + EthnicityCleaned, data = employ_datQ30Ethnicity)
conTable
#ordinal(employ_datQ30Ethnicity$CatOutcome, employ_datQ30Ethnicity$EthnicityCleaned, employ_datQ30Ethnicity)
prep <- analysisPrep(employ_datQ30Ethnicity$CatOutcome, employ_datQ30Ethnicity$EthnicityCleaned, employ_datQ30Ethnicity)
analysis <- glm(employ_datQ30Ethnicity$CatOutcome ~ employ_datQ30Ethnicity$EthnicityCleaned, family = "binomial", data=employ_datQ30Ethnicity)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ30FirstGen<-multidatClean(Q30, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30FirstGen$FirstGen<-factor(employ_datQ30FirstGen$FirstGen)
employ_datQ30FirstGen<-ordinaldatCleanbin(employ_datQ30FirstGen$Q30, employ_datQ30FirstGen)
#ordinal(employ_datQ30FirstGen$CatOutcome, employ_datQ30FirstGen$FirstGen, employ_datQ30FirstGen)
prep <- analysisPrep(employ_datQ30FirstGen$CatOutcome, employ_datQ30FirstGen$FirstGen, employ_datQ30FirstGen)
analysis <- glm(employ_datQ30FirstGen$CatOutcome ~ employ_datQ30FirstGen$FirstGen, family = binomial, data=employ_datQ30FirstGen)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ30Gender<-multidatClean(Q30, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30Gender$Gender<-factor(employ_datQ30Gender$Gender)
employ_datQ30Gender<-ordinaldatCleanbin(employ_datQ30Gender$Q30, employ_datQ30Gender)
#ordinal(employ_datQ30Gender$CatOutcome, employ_datQ30Gender$Gender, employ_datQ30Gender)
prep <- analysisPrep(employ_datQ30Gender$CatOutcome, employ_datQ30Gender$Gender, employ_datQ30Gender)
analysis <- glm(employ_datQ30Gender$CatOutcome ~ employ_datQ30Gender$Gender, family = binomial, data=employ_datQ30Gender)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ30Sexuality<-multidatClean(Q30, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ30Sexuality$Sexuality<-factor(employ_datQ30Sexuality$Sexuality)
employ_datQ30Sexuality<-ordinaldatCleanbin(employ_datQ30Sexuality$Q30, employ_datQ30Sexuality)
#ordinal(employ_datQ30Sexuality$CatOutcome, employ_datQ30Sexuality$Sexuality, employ_datQ30Sexuality)
prep <- analysisPrep(employ_datQ30Sexuality$CatOutcome, employ_datQ30Sexuality$Sexuality, employ_datQ30Sexuality)
analysis <- glm(employ_datQ30Sexuality$CatOutcome ~ employ_datQ30Sexuality$Sexuality, family = binomial, data=employ_datQ30Sexuality)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q30"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q30_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep_binary (OR_Outcomes)

"Q31. During your research career have you ever experienced discrimination?"
"Status"
employ_datQ31<-multidatClean(Q31, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q31 + Academic, data = employ_datQ31)
detach(dat_long)
employ_datQ31<-ordinaldatCleanbin(employ_datQ31$Q31, employ_datQ31)
#ordinal(employ_datQ31$CatOutcome, employ_datQ31$Academic, employ_datQ31)
prep <- analysisPrep(employ_datQ31$CatOutcome, employ_datQ31$Academic, employ_datQ31)
analysis <- glm(employ_datQ31$CatOutcome ~ employ_datQ31$Academic, family = binomial, data=employ_datQ31)
##assumption<-brant(analysis)
analysisSummary <- summary(analysis)
##ctable <- coef(summary(analysis))
##p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
##ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q31"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ31Carer<-multidatClean(Q31, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31Carer$Carer<- factor(employ_datQ31Carer$Carer)
employ_datQ31Carer<-ordinaldatCleanbin(employ_datQ31Carer$Q31, employ_datQ31Carer)
#ordinal(employ_datQ31Carer$CatOutcome, employ_datQ31Carer$Carer, employ_datQ31Carer)
prep <- analysisPrep(employ_datQ31Carer$CatOutcome, employ_datQ31Carer$Carer, employ_datQ31Carer)
analysis <- glm(employ_datQ31Carer$CatOutcome ~ employ_datQ31Carer$Carer, family = binomial, data=employ_datQ31Carer)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ31Disability<-multidatClean(Q31, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31Disability$Disability<- factor(employ_datQ31Disability$Disability)
employ_datQ31Disability<-ordinaldatCleanbin(employ_datQ31Disability$Q31, employ_datQ31Disability)
conTable <- xtabs(~Q31 + Disability, data = employ_datQ31Disability)
#ordinal(employ_datQ31Disability$CatOutcome, employ_datQ31Disability$Disability, employ_datQ31Disability)
prep <- analysisPrep(employ_datQ31Disability$CatOutcome, employ_datQ31Disability$Disability, employ_datQ31Disability)
analysis <- glm(employ_datQ31Disability$CatOutcome ~ employ_datQ31Disability$Disability, family = binomial, data=employ_datQ31Disability)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ31Ethnicity<-multidatClean(Q31, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31Ethnicity$Ethnicity<- factor(employ_datQ31Ethnicity$EthnicityCleaned)
employ_datQ31Ethnicity<-ordinaldatCleanbin(employ_datQ31Ethnicity$Q31, employ_datQ31Ethnicity)
conTable <- xtabs(~Q31 + EthnicityCleaned, data = employ_datQ31Ethnicity)
conTable
#ordinal(employ_datQ31Ethnicity$CatOutcome, employ_datQ31Ethnicity$EthnicityCleaned, employ_datQ31Ethnicity)
prep <- analysisPrep(employ_datQ31Ethnicity$CatOutcome, employ_datQ31Ethnicity$EthnicityCleaned, employ_datQ31Ethnicity)
analysis <- glm(employ_datQ31Ethnicity$CatOutcome ~ employ_datQ31Ethnicity$EthnicityCleaned, family = "binomial", data=employ_datQ31Ethnicity)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ31FirstGen<-multidatClean(Q31, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31FirstGen$FirstGen<-factor(employ_datQ31FirstGen$FirstGen)
employ_datQ31FirstGen<-ordinaldatCleanbin(employ_datQ31FirstGen$Q31, employ_datQ31FirstGen)
#ordinal(employ_datQ31FirstGen$CatOutcome, employ_datQ31FirstGen$FirstGen, employ_datQ31FirstGen)
prep <- analysisPrep(employ_datQ31FirstGen$CatOutcome, employ_datQ31FirstGen$FirstGen, employ_datQ31FirstGen)
analysis <- glm(employ_datQ31FirstGen$CatOutcome ~ employ_datQ31FirstGen$FirstGen, family = binomial, data=employ_datQ31FirstGen)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ31Gender<-multidatClean(Q31, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31Gender$Gender<-factor(employ_datQ31Gender$Gender)
employ_datQ31Gender<-ordinaldatCleanbin(employ_datQ31Gender$Q31, employ_datQ31Gender)
#ordinal(employ_datQ31Gender$CatOutcome, employ_datQ31Gender$Gender, employ_datQ31Gender)
prep <- analysisPrep(employ_datQ31Gender$CatOutcome, employ_datQ31Gender$Gender, employ_datQ31Gender)
analysis <- glm(employ_datQ31Gender$CatOutcome ~ employ_datQ31Gender$Gender, family = binomial, data=employ_datQ31Gender)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ31Sexuality<-multidatClean(Q31, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ31Sexuality$Sexuality<-factor(employ_datQ31Sexuality$Sexuality)
employ_datQ31Sexuality<-ordinaldatCleanbin(employ_datQ31Sexuality$Q31, employ_datQ31Sexuality)
#ordinal(employ_datQ31Sexuality$CatOutcome, employ_datQ31Sexuality$Sexuality, employ_datQ31Sexuality)
prep <- analysisPrep(employ_datQ31Sexuality$CatOutcome, employ_datQ31Sexuality$Sexuality, employ_datQ31Sexuality)
analysis <- glm(employ_datQ31Sexuality$CatOutcome ~ employ_datQ31Sexuality$Sexuality, family = binomial, data=employ_datQ31Sexuality)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q31"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q31_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep_binary (OR_Outcomes)

"Q32. During your research career have you ever witnessed discrimination?"
"Status"
employ_datQ32<-multidatClean(Q32, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q32 + Academic, data = employ_datQ32)
detach(dat_long)
employ_datQ32<-ordinaldatCleanbin(employ_datQ32$Q32, employ_datQ32)
#ordinal(employ_datQ32$CatOutcome, employ_datQ32$Academic, employ_datQ32)
prep <- analysisPrep(employ_datQ32$CatOutcome, employ_datQ32$Academic, employ_datQ32)
analysis <- glm(employ_datQ32$CatOutcome ~ employ_datQ32$Academic, family = binomial, data=employ_datQ32)
##assumption<-brant(analysis)
analysisSummary <- summary(analysis)
##ctable <- coef(summary(analysis))
##p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
##ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- (cbind(OR = coef(analysis), ci))
OR[2,] <- OR[1,1]+OR[2,]
OR[3,] <- OR[1,1]+OR[3,]
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <- "Q32"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ32Carer<-multidatClean(Q32, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32Carer$Carer<- factor(employ_datQ32Carer$Carer)
employ_datQ32Carer<-ordinaldatCleanbin(employ_datQ32Carer$Q32, employ_datQ32Carer)
#ordinal(employ_datQ32Carer$CatOutcome, employ_datQ32Carer$Carer, employ_datQ32Carer)
prep <- analysisPrep(employ_datQ32Carer$CatOutcome, employ_datQ32Carer$Carer, employ_datQ32Carer)
analysis <- glm(employ_datQ32Carer$CatOutcome ~ employ_datQ32Carer$Carer, family = binomial, data=employ_datQ32Carer)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ32Disability<-multidatClean(Q32, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32Disability$Disability<- factor(employ_datQ32Disability$Disability)
employ_datQ32Disability<-ordinaldatCleanbin(employ_datQ32Disability$Q32, employ_datQ32Disability)
conTable <- xtabs(~Q32 + Disability, data = employ_datQ32Disability)
#ordinal(employ_datQ32Disability$CatOutcome, employ_datQ32Disability$Disability, employ_datQ32Disability)
prep <- analysisPrep(employ_datQ32Disability$CatOutcome, employ_datQ32Disability$Disability, employ_datQ32Disability)
analysis <- glm(employ_datQ32Disability$CatOutcome ~ employ_datQ32Disability$Disability, family = binomial, data=employ_datQ32Disability)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ32Ethnicity<-multidatClean(Q32, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32Ethnicity$Ethnicity<- factor(employ_datQ32Ethnicity$EthnicityCleaned)
employ_datQ32Ethnicity<-ordinaldatCleanbin(employ_datQ32Ethnicity$Q32, employ_datQ32Ethnicity)
conTable <- xtabs(~Q32 + EthnicityCleaned, data = employ_datQ32Ethnicity)
conTable
#ordinal(employ_datQ32Ethnicity$CatOutcome, employ_datQ32Ethnicity$EthnicityCleaned, employ_datQ32Ethnicity)
prep <- analysisPrep(employ_datQ32Ethnicity$CatOutcome, employ_datQ32Ethnicity$EthnicityCleaned, employ_datQ32Ethnicity)
analysis <- glm(employ_datQ32Ethnicity$CatOutcome ~ employ_datQ32Ethnicity$EthnicityCleaned, family = "binomial", data=employ_datQ32Ethnicity)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ32FirstGen<-multidatClean(Q32, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32FirstGen$FirstGen<-factor(employ_datQ32FirstGen$FirstGen)
employ_datQ32FirstGen<-ordinaldatCleanbin(employ_datQ32FirstGen$Q32, employ_datQ32FirstGen)
#ordinal(employ_datQ32FirstGen$CatOutcome, employ_datQ32FirstGen$FirstGen, employ_datQ32FirstGen)
prep <- analysisPrep(employ_datQ32FirstGen$CatOutcome, employ_datQ32FirstGen$FirstGen, employ_datQ32FirstGen)
analysis <- glm(employ_datQ32FirstGen$CatOutcome ~ employ_datQ32FirstGen$FirstGen, family = binomial, data=employ_datQ32FirstGen)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ32Gender<-multidatClean(Q32, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32Gender$Gender<-factor(employ_datQ32Gender$Gender)
employ_datQ32Gender<-ordinaldatCleanbin(employ_datQ32Gender$Q32, employ_datQ32Gender)
#ordinal(employ_datQ32Gender$CatOutcome, employ_datQ32Gender$Gender, employ_datQ32Gender)
prep <- analysisPrep(employ_datQ32Gender$CatOutcome, employ_datQ32Gender$Gender, employ_datQ32Gender)
analysis <- glm(employ_datQ32Gender$CatOutcome ~ employ_datQ32Gender$Gender, family = binomial, data=employ_datQ32Gender)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ32Sexuality<-multidatClean(Q32, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ32Sexuality$Sexuality<-factor(employ_datQ32Sexuality$Sexuality)
employ_datQ32Sexuality<-ordinaldatCleanbin(employ_datQ32Sexuality$Q32, employ_datQ32Sexuality)
#ordinal(employ_datQ32Sexuality$CatOutcome, employ_datQ32Sexuality$Sexuality, employ_datQ32Sexuality)
prep <- analysisPrep(employ_datQ32Sexuality$CatOutcome, employ_datQ32Sexuality$Sexuality, employ_datQ32Sexuality)
analysis <- glm(employ_datQ32Sexuality$CatOutcome ~ employ_datQ32Sexuality$Sexuality, family = binomial, data=employ_datQ32Sexuality)
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
OR <- exp(OR)
OR <- OR[,]/OR[1,1]
#result <- list(prep=prep, #assumption = #assumption, analysisSummary = analysisSummary, contTable = ctable, CI = ci, OR = OR)
Question <-  "Q32"
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q32_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep_binary (OR_Outcomes)


### Q33 - Would you feel comfortable speaking out about instances of bullying and/or discrimination without negative personal consequences from within your workplace?"
"Status"
employ_datQ33<-multidatClean(Q33, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q33 + Academic, data = employ_datQ33)
detach(dat_long)
employ_datQ33<-UnsureordinaldatClean(employ_datQ33$Q33, employ_datQ33)
#ordinal(employ_datQ33$CatOutcome, employ_datQ33$Academic, employ_datQ33)
prep <- analysisPrep(employ_datQ33$CatOutcome, employ_datQ33$Academic, employ_datQ33)
analysis <- polr(employ_datQ33$CatOutcome ~ employ_datQ33$Academic, data=employ_datQ33, Hess=TRUE)
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
Question <- "Q33"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ33Carer<-multidatClean(Q33, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33Carer$Carer<- factor(employ_datQ33Carer$Carer)
employ_datQ33Carer<-UnsureordinaldatClean(employ_datQ33Carer$Q33, employ_datQ33Carer)
#ordinal(employ_datQ33Carer$CatOutcome, employ_datQ33Carer$Carer, employ_datQ33Carer)
prep <- analysisPrep(employ_datQ33Carer$CatOutcome, employ_datQ33Carer$Carer, employ_datQ33Carer)
analysis <- polr(employ_datQ33Carer$CatOutcome ~ employ_datQ33Carer$Carer, data=employ_datQ33Carer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ33Disability<-multidatClean(Q33, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33Disability$Disability<- factor(employ_datQ33Disability$Disability)
employ_datQ33Disability<-UnsureordinaldatClean(employ_datQ33Disability$Q33, employ_datQ33Disability)
conTable <- xtabs(~Q33 + Disability, data = employ_datQ33Disability)
#ordinal(employ_datQ33Disability$CatOutcome, employ_datQ33Disability$Disability, employ_datQ33Disability)
prep <- analysisPrep(employ_datQ33Disability$CatOutcome, employ_datQ33Disability$Disability, employ_datQ33Disability)
analysis <- polr(employ_datQ33Disability$CatOutcome ~ employ_datQ33Disability$Disability, data=employ_datQ33Disability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ33Ethnicity<-multidatClean(Q33, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33Ethnicity$Ethnicity<- factor(employ_datQ33Ethnicity$EthnicityCleaned)
employ_datQ33Ethnicity<-UnsureordinaldatClean(employ_datQ33Ethnicity$Q33, employ_datQ33Ethnicity)
conTable <- xtabs(~Q33 + EthnicityCleaned, data = employ_datQ33Ethnicity)
conTable
#ordinal(employ_datQ33Ethnicity$CatOutcome, employ_datQ33Ethnicity$EthnicityCleaned, employ_datQ33Ethnicity)
prep <- analysisPrep(employ_datQ33Ethnicity$CatOutcome, employ_datQ33Ethnicity$EthnicityCleaned, employ_datQ33Ethnicity)
analysis <- polr(employ_datQ33Ethnicity$CatOutcome ~ employ_datQ33Ethnicity$EthnicityCleaned, data=employ_datQ33Ethnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ33FirstGen<-multidatClean(Q33, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33FirstGen$FirstGen<-factor(employ_datQ33FirstGen$FirstGen)
employ_datQ33FirstGen<-UnsureordinaldatClean(employ_datQ33FirstGen$Q33, employ_datQ33FirstGen)
#ordinal(employ_datQ33FirstGen$CatOutcome, employ_datQ33FirstGen$FirstGen, employ_datQ33FirstGen)
prep <- analysisPrep(employ_datQ33FirstGen$CatOutcome, employ_datQ33FirstGen$FirstGen, employ_datQ33FirstGen)
analysis <- polr(employ_datQ33FirstGen$CatOutcome ~ employ_datQ33FirstGen$FirstGen, data=employ_datQ33FirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ33Gender<-multidatClean(Q33, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33Gender$Gender<-factor(employ_datQ33Gender$Gender)
employ_datQ33Gender<-UnsureordinaldatClean(employ_datQ33Gender$Q33, employ_datQ33Gender)
#ordinal(employ_datQ33Gender$CatOutcome, employ_datQ33Gender$Gender, employ_datQ33Gender)
prep <- analysisPrep(employ_datQ33Gender$CatOutcome, employ_datQ33Gender$Gender, employ_datQ33Gender)
analysis <- polr(employ_datQ33Gender$CatOutcome ~ employ_datQ33Gender$Gender, data=employ_datQ33Gender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ33Sexuality<-multidatClean(Q33, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ33Sexuality$Sexuality<-factor(employ_datQ33Sexuality$Sexuality)
employ_datQ33Sexuality<-UnsureordinaldatClean(employ_datQ33Sexuality$Q33, employ_datQ33Sexuality)
#ordinal(employ_datQ33Sexuality$CatOutcome, employ_datQ33Sexuality$Sexuality, employ_datQ33Sexuality)
prep <- analysisPrep(employ_datQ33Sexuality$CatOutcome, employ_datQ33Sexuality$Sexuality, employ_datQ33Sexuality)
analysis <- polr(employ_datQ33Sexuality$CatOutcome ~ employ_datQ33Sexuality$Sexuality, data=employ_datQ33Sexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q33_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q34 - Within your workplace, do you feel your concerns relating to experiences of bullying and/or discrimination would be listened to?"
"Status"
employ_datQ34<-multidatClean(Q34, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q34 + Academic, data = employ_datQ34)
detach(dat_long)
employ_datQ34<-UnsureordinaldatClean(employ_datQ34$Q34, employ_datQ34)
#ordinal(employ_datQ34$CatOutcome, employ_datQ34$Academic, employ_datQ34)
prep <- analysisPrep(employ_datQ34$CatOutcome, employ_datQ34$Academic, employ_datQ34)
analysis <- polr(employ_datQ34$CatOutcome ~ employ_datQ34$Academic, data=employ_datQ34, Hess=TRUE)
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
Question <- "Q34"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ34Carer<-multidatClean(Q34, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34Carer$Carer<- factor(employ_datQ34Carer$Carer)
employ_datQ34Carer<-UnsureordinaldatClean(employ_datQ34Carer$Q34, employ_datQ34Carer)
#ordinal(employ_datQ34Carer$CatOutcome, employ_datQ34Carer$Carer, employ_datQ34Carer)
prep <- analysisPrep(employ_datQ34Carer$CatOutcome, employ_datQ34Carer$Carer, employ_datQ34Carer)
analysis <- polr(employ_datQ34Carer$CatOutcome ~ employ_datQ34Carer$Carer, data=employ_datQ34Carer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ34Disability<-multidatClean(Q34, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34Disability$Disability<- factor(employ_datQ34Disability$Disability)
employ_datQ34Disability<-UnsureordinaldatClean(employ_datQ34Disability$Q34, employ_datQ34Disability)
conTable <- xtabs(~Q34 + Disability, data = employ_datQ34Disability)
#ordinal(employ_datQ34Disability$CatOutcome, employ_datQ34Disability$Disability, employ_datQ34Disability)
prep <- analysisPrep(employ_datQ34Disability$CatOutcome, employ_datQ34Disability$Disability, employ_datQ34Disability)
analysis <- polr(employ_datQ34Disability$CatOutcome ~ employ_datQ34Disability$Disability, data=employ_datQ34Disability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ34Ethnicity<-multidatClean(Q34, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34Ethnicity$Ethnicity<- factor(employ_datQ34Ethnicity$EthnicityCleaned)
employ_datQ34Ethnicity<-UnsureordinaldatClean(employ_datQ34Ethnicity$Q34, employ_datQ34Ethnicity)
conTable <- xtabs(~Q34 + EthnicityCleaned, data = employ_datQ34Ethnicity)
conTable
#ordinal(employ_datQ34Ethnicity$CatOutcome, employ_datQ34Ethnicity$EthnicityCleaned, employ_datQ34Ethnicity)
prep <- analysisPrep(employ_datQ34Ethnicity$CatOutcome, employ_datQ34Ethnicity$EthnicityCleaned, employ_datQ34Ethnicity)
analysis <- polr(employ_datQ34Ethnicity$CatOutcome ~ employ_datQ34Ethnicity$EthnicityCleaned, data=employ_datQ34Ethnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ34FirstGen<-multidatClean(Q34, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34FirstGen$FirstGen<-factor(employ_datQ34FirstGen$FirstGen)
employ_datQ34FirstGen<-UnsureordinaldatClean(employ_datQ34FirstGen$Q34, employ_datQ34FirstGen)
#ordinal(employ_datQ34FirstGen$CatOutcome, employ_datQ34FirstGen$FirstGen, employ_datQ34FirstGen)
prep <- analysisPrep(employ_datQ34FirstGen$CatOutcome, employ_datQ34FirstGen$FirstGen, employ_datQ34FirstGen)
analysis <- polr(employ_datQ34FirstGen$CatOutcome ~ employ_datQ34FirstGen$FirstGen, data=employ_datQ34FirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ34Gender<-multidatClean(Q34, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34Gender$Gender<-factor(employ_datQ34Gender$Gender)
employ_datQ34Gender<-UnsureordinaldatClean(employ_datQ34Gender$Q34, employ_datQ34Gender)
#ordinal(employ_datQ34Gender$CatOutcome, employ_datQ34Gender$Gender, employ_datQ34Gender)
prep <- analysisPrep(employ_datQ34Gender$CatOutcome, employ_datQ34Gender$Gender, employ_datQ34Gender)
analysis <- polr(employ_datQ34Gender$CatOutcome ~ employ_datQ34Gender$Gender, data=employ_datQ34Gender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ34Sexuality<-multidatClean(Q34, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ34Sexuality$Sexuality<-factor(employ_datQ34Sexuality$Sexuality)
employ_datQ34Sexuality<-UnsureordinaldatClean(employ_datQ34Sexuality$Q34, employ_datQ34Sexuality)
#ordinal(employ_datQ34Sexuality$CatOutcome, employ_datQ34Sexuality$Sexuality, employ_datQ34Sexuality)
prep <- analysisPrep(employ_datQ34Sexuality$CatOutcome, employ_datQ34Sexuality$Sexuality, employ_datQ34Sexuality)
analysis <- polr(employ_datQ34Sexuality$CatOutcome ~ employ_datQ34Sexuality$Sexuality, data=employ_datQ34Sexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q34_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q35 - Within your workplace, do you feel your concerns relating to experiences of bullying and/or discrimination would be appropriately acted upon?"
"Status"
employ_datQ35<-multidatClean(Q35, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q35 + Academic, data = employ_datQ35)
detach(dat_long)
employ_datQ35<-UnsureordinaldatClean(employ_datQ35$Q35, employ_datQ35)
#ordinal(employ_datQ35$CatOutcome, employ_datQ35$Academic, employ_datQ35)
prep <- analysisPrep(employ_datQ35$CatOutcome, employ_datQ35$Academic, employ_datQ35)
analysis <- polr(employ_datQ35$CatOutcome ~ employ_datQ35$Academic, data=employ_datQ35, Hess=TRUE)
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
Question <- "Q35"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ35Carer<-multidatClean(Q35, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35Carer$Carer<- factor(employ_datQ35Carer$Carer)
employ_datQ35Carer<-UnsureordinaldatClean(employ_datQ35Carer$Q35, employ_datQ35Carer)
#ordinal(employ_datQ35Carer$CatOutcome, employ_datQ35Carer$Carer, employ_datQ35Carer)
prep <- analysisPrep(employ_datQ35Carer$CatOutcome, employ_datQ35Carer$Carer, employ_datQ35Carer)
analysis <- polr(employ_datQ35Carer$CatOutcome ~ employ_datQ35Carer$Carer, data=employ_datQ35Carer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ35Disability<-multidatClean(Q35, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35Disability$Disability<- factor(employ_datQ35Disability$Disability)
employ_datQ35Disability<-UnsureordinaldatClean(employ_datQ35Disability$Q35, employ_datQ35Disability)
conTable <- xtabs(~Q35 + Disability, data = employ_datQ35Disability)
#ordinal(employ_datQ35Disability$CatOutcome, employ_datQ35Disability$Disability, employ_datQ35Disability)
prep <- analysisPrep(employ_datQ35Disability$CatOutcome, employ_datQ35Disability$Disability, employ_datQ35Disability)
analysis <- polr(employ_datQ35Disability$CatOutcome ~ employ_datQ35Disability$Disability, data=employ_datQ35Disability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ35Ethnicity<-multidatClean(Q35, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35Ethnicity$Ethnicity<- factor(employ_datQ35Ethnicity$EthnicityCleaned)
employ_datQ35Ethnicity<-UnsureordinaldatClean(employ_datQ35Ethnicity$Q35, employ_datQ35Ethnicity)
conTable <- xtabs(~Q35 + EthnicityCleaned, data = employ_datQ35Ethnicity)
conTable
#ordinal(employ_datQ35Ethnicity$CatOutcome, employ_datQ35Ethnicity$EthnicityCleaned, employ_datQ35Ethnicity)
prep <- analysisPrep(employ_datQ35Ethnicity$CatOutcome, employ_datQ35Ethnicity$EthnicityCleaned, employ_datQ35Ethnicity)
analysis <- polr(employ_datQ35Ethnicity$CatOutcome ~ employ_datQ35Ethnicity$EthnicityCleaned, data=employ_datQ35Ethnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ35FirstGen<-multidatClean(Q35, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35FirstGen$FirstGen<-factor(employ_datQ35FirstGen$FirstGen)
employ_datQ35FirstGen<-UnsureordinaldatClean(employ_datQ35FirstGen$Q35, employ_datQ35FirstGen)
#ordinal(employ_datQ35FirstGen$CatOutcome, employ_datQ35FirstGen$FirstGen, employ_datQ35FirstGen)
prep <- analysisPrep(employ_datQ35FirstGen$CatOutcome, employ_datQ35FirstGen$FirstGen, employ_datQ35FirstGen)
analysis <- polr(employ_datQ35FirstGen$CatOutcome ~ employ_datQ35FirstGen$FirstGen, data=employ_datQ35FirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ35Gender<-multidatClean(Q35, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35Gender$Gender<-factor(employ_datQ35Gender$Gender)
employ_datQ35Gender<-UnsureordinaldatClean(employ_datQ35Gender$Q35, employ_datQ35Gender)
#ordinal(employ_datQ35Gender$CatOutcome, employ_datQ35Gender$Gender, employ_datQ35Gender)
prep <- analysisPrep(employ_datQ35Gender$CatOutcome, employ_datQ35Gender$Gender, employ_datQ35Gender)
analysis <- polr(employ_datQ35Gender$CatOutcome ~ employ_datQ35Gender$Gender, data=employ_datQ35Gender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ35Sexuality<-multidatClean(Q35, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ35Sexuality$Sexuality<-factor(employ_datQ35Sexuality$Sexuality)
employ_datQ35Sexuality<-UnsureordinaldatClean(employ_datQ35Sexuality$Q35, employ_datQ35Sexuality)
#ordinal(employ_datQ35Sexuality$CatOutcome, employ_datQ35Sexuality$Sexuality, employ_datQ35Sexuality)
prep <- analysisPrep(employ_datQ35Sexuality$CatOutcome, employ_datQ35Sexuality$Sexuality, employ_datQ35Sexuality)
analysis <- polr(employ_datQ35Sexuality$CatOutcome ~ employ_datQ35Sexuality$Sexuality, data=employ_datQ35Sexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q35_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.1
"Status"
employ_datQ37.1.a<-multidatClean(Q37.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.1.a + Academic, data = employ_datQ37.1.a)
detach(dat_long)
employ_datQ37.1.a<-ordinaldatClean(employ_datQ37.1.a$Q37.1.a, employ_datQ37.1.a)
#ordinal(employ_datQ37.1.a$CatOutcome, employ_datQ37.1.a$Academic, employ_datQ37.1.a)
prep <- analysisPrep(employ_datQ37.1.a$CatOutcome, employ_datQ37.1.a$Academic, employ_datQ37.1.a)
analysis <- polr(employ_datQ37.1.a$CatOutcome ~ employ_datQ37.1.a$Academic, data=employ_datQ37.1.a, Hess=TRUE)
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
Question <- "Q37.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.1.aCarer<-multidatClean(Q37.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aCarer$Carer<- factor(employ_datQ37.1.aCarer$Carer)
employ_datQ37.1.aCarer<-ordinaldatClean(employ_datQ37.1.aCarer$Q37.1.a, employ_datQ37.1.aCarer)
#ordinal(employ_datQ37.1.aCarer$CatOutcome, employ_datQ37.1.aCarer$Carer, employ_datQ37.1.aCarer)
prep <- analysisPrep(employ_datQ37.1.aCarer$CatOutcome, employ_datQ37.1.aCarer$Carer, employ_datQ37.1.aCarer)
analysis <- polr(employ_datQ37.1.aCarer$CatOutcome ~ employ_datQ37.1.aCarer$Carer, data=employ_datQ37.1.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.1.aDisability<-multidatClean(Q37.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aDisability$Disability<- factor(employ_datQ37.1.aDisability$Disability)
employ_datQ37.1.aDisability<-ordinaldatClean(employ_datQ37.1.aDisability$Q37.1.a, employ_datQ37.1.aDisability)
conTable <- xtabs(~Q37.1.a + Disability, data = employ_datQ37.1.aDisability)
#ordinal(employ_datQ37.1.aDisability$CatOutcome, employ_datQ37.1.aDisability$Disability, employ_datQ37.1.aDisability)
prep <- analysisPrep(employ_datQ37.1.aDisability$CatOutcome, employ_datQ37.1.aDisability$Disability, employ_datQ37.1.aDisability)
analysis <- polr(employ_datQ37.1.aDisability$CatOutcome ~ employ_datQ37.1.aDisability$Disability, data=employ_datQ37.1.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.1.aEthnicity<-multidatClean(Q37.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aEthnicity$Ethnicity<- factor(employ_datQ37.1.aEthnicity$EthnicityCleaned)
employ_datQ37.1.aEthnicity<-ordinaldatClean(employ_datQ37.1.aEthnicity$Q37.1.a, employ_datQ37.1.aEthnicity)
conTable <- xtabs(~Q37.1.a + EthnicityCleaned, data = employ_datQ37.1.aEthnicity)
conTable
#ordinal(employ_datQ37.1.aEthnicity$CatOutcome, employ_datQ37.1.aEthnicity$EthnicityCleaned, employ_datQ37.1.aEthnicity)
prep <- analysisPrep(employ_datQ37.1.aEthnicity$CatOutcome, employ_datQ37.1.aEthnicity$EthnicityCleaned, employ_datQ37.1.aEthnicity)
analysis <- polr(employ_datQ37.1.aEthnicity$CatOutcome ~ employ_datQ37.1.aEthnicity$EthnicityCleaned, data=employ_datQ37.1.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.1.aFirstGen<-multidatClean(Q37.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aFirstGen$FirstGen<-factor(employ_datQ37.1.aFirstGen$FirstGen)
employ_datQ37.1.aFirstGen<-ordinaldatClean(employ_datQ37.1.aFirstGen$Q37.1.a, employ_datQ37.1.aFirstGen)
#ordinal(employ_datQ37.1.aFirstGen$CatOutcome, employ_datQ37.1.aFirstGen$FirstGen, employ_datQ37.1.aFirstGen)
prep <- analysisPrep(employ_datQ37.1.aFirstGen$CatOutcome, employ_datQ37.1.aFirstGen$FirstGen, employ_datQ37.1.aFirstGen)
analysis <- polr(employ_datQ37.1.aFirstGen$CatOutcome ~ employ_datQ37.1.aFirstGen$FirstGen, data=employ_datQ37.1.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.1.aGender<-multidatClean(Q37.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aGender$Gender<-factor(employ_datQ37.1.aGender$Gender)
employ_datQ37.1.aGender<-ordinaldatClean(employ_datQ37.1.aGender$Q37.1.a, employ_datQ37.1.aGender)
#ordinal(employ_datQ37.1.aGender$CatOutcome, employ_datQ37.1.aGender$Gender, employ_datQ37.1.aGender)
prep <- analysisPrep(employ_datQ37.1.aGender$CatOutcome, employ_datQ37.1.aGender$Gender, employ_datQ37.1.aGender)
analysis <- polr(employ_datQ37.1.aGender$CatOutcome ~ employ_datQ37.1.aGender$Gender, data=employ_datQ37.1.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.1.aSexuality<-multidatClean(Q37.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.1.aSexuality$Sexuality<-factor(employ_datQ37.1.aSexuality$Sexuality)
employ_datQ37.1.aSexuality<-ordinaldatClean(employ_datQ37.1.aSexuality$Q37.1.a, employ_datQ37.1.aSexuality)
#ordinal(employ_datQ37.1.aSexuality$CatOutcome, employ_datQ37.1.aSexuality$Sexuality, employ_datQ37.1.aSexuality)
prep <- analysisPrep(employ_datQ37.1.aSexuality$CatOutcome, employ_datQ37.1.aSexuality$Sexuality, employ_datQ37.1.aSexuality)
analysis <- polr(employ_datQ37.1.aSexuality$CatOutcome ~ employ_datQ37.1.aSexuality$Sexuality, data=employ_datQ37.1.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.2
"Status"
employ_datQ37.2.a<-multidatClean(Q37.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.2.a + Academic, data = employ_datQ37.2.a)
detach(dat_long)
employ_datQ37.2.a<-ordinaldatClean(employ_datQ37.2.a$Q37.2.a, employ_datQ37.2.a)
#ordinal(employ_datQ37.2.a$CatOutcome, employ_datQ37.2.a$Academic, employ_datQ37.2.a)
prep <- analysisPrep(employ_datQ37.2.a$CatOutcome, employ_datQ37.2.a$Academic, employ_datQ37.2.a)
analysis <- polr(employ_datQ37.2.a$CatOutcome ~ employ_datQ37.2.a$Academic, data=employ_datQ37.2.a, Hess=TRUE)
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
Question <- "Q37.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.2.aCarer<-multidatClean(Q37.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aCarer$Carer<- factor(employ_datQ37.2.aCarer$Carer)
employ_datQ37.2.aCarer<-ordinaldatClean(employ_datQ37.2.aCarer$Q37.2.a, employ_datQ37.2.aCarer)
#ordinal(employ_datQ37.2.aCarer$CatOutcome, employ_datQ37.2.aCarer$Carer, employ_datQ37.2.aCarer)
prep <- analysisPrep(employ_datQ37.2.aCarer$CatOutcome, employ_datQ37.2.aCarer$Carer, employ_datQ37.2.aCarer)
analysis <- polr(employ_datQ37.2.aCarer$CatOutcome ~ employ_datQ37.2.aCarer$Carer, data=employ_datQ37.2.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.2.aDisability<-multidatClean(Q37.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aDisability$Disability<- factor(employ_datQ37.2.aDisability$Disability)
employ_datQ37.2.aDisability<-ordinaldatClean(employ_datQ37.2.aDisability$Q37.2.a, employ_datQ37.2.aDisability)
conTable <- xtabs(~Q37.2.a + Disability, data = employ_datQ37.2.aDisability)
#ordinal(employ_datQ37.2.aDisability$CatOutcome, employ_datQ37.2.aDisability$Disability, employ_datQ37.2.aDisability)
prep <- analysisPrep(employ_datQ37.2.aDisability$CatOutcome, employ_datQ37.2.aDisability$Disability, employ_datQ37.2.aDisability)
analysis <- polr(employ_datQ37.2.aDisability$CatOutcome ~ employ_datQ37.2.aDisability$Disability, data=employ_datQ37.2.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.2.aEthnicity<-multidatClean(Q37.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aEthnicity$Ethnicity<- factor(employ_datQ37.2.aEthnicity$EthnicityCleaned)
employ_datQ37.2.aEthnicity<-ordinaldatClean(employ_datQ37.2.aEthnicity$Q37.2.a, employ_datQ37.2.aEthnicity)
conTable <- xtabs(~Q37.2.a + EthnicityCleaned, data = employ_datQ37.2.aEthnicity)
conTable
#ordinal(employ_datQ37.2.aEthnicity$CatOutcome, employ_datQ37.2.aEthnicity$EthnicityCleaned, employ_datQ37.2.aEthnicity)
prep <- analysisPrep(employ_datQ37.2.aEthnicity$CatOutcome, employ_datQ37.2.aEthnicity$EthnicityCleaned, employ_datQ37.2.aEthnicity)
analysis <- polr(employ_datQ37.2.aEthnicity$CatOutcome ~ employ_datQ37.2.aEthnicity$EthnicityCleaned, data=employ_datQ37.2.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.2.aFirstGen<-multidatClean(Q37.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aFirstGen$FirstGen<-factor(employ_datQ37.2.aFirstGen$FirstGen)
employ_datQ37.2.aFirstGen<-ordinaldatClean(employ_datQ37.2.aFirstGen$Q37.2.a, employ_datQ37.2.aFirstGen)
#ordinal(employ_datQ37.2.aFirstGen$CatOutcome, employ_datQ37.2.aFirstGen$FirstGen, employ_datQ37.2.aFirstGen)
prep <- analysisPrep(employ_datQ37.2.aFirstGen$CatOutcome, employ_datQ37.2.aFirstGen$FirstGen, employ_datQ37.2.aFirstGen)
analysis <- polr(employ_datQ37.2.aFirstGen$CatOutcome ~ employ_datQ37.2.aFirstGen$FirstGen, data=employ_datQ37.2.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.2.aGender<-multidatClean(Q37.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aGender$Gender<-factor(employ_datQ37.2.aGender$Gender)
employ_datQ37.2.aGender<-ordinaldatClean(employ_datQ37.2.aGender$Q37.2.a, employ_datQ37.2.aGender)
#ordinal(employ_datQ37.2.aGender$CatOutcome, employ_datQ37.2.aGender$Gender, employ_datQ37.2.aGender)
prep <- analysisPrep(employ_datQ37.2.aGender$CatOutcome, employ_datQ37.2.aGender$Gender, employ_datQ37.2.aGender)
analysis <- polr(employ_datQ37.2.aGender$CatOutcome ~ employ_datQ37.2.aGender$Gender, data=employ_datQ37.2.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.2.aSexuality<-multidatClean(Q37.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.2.aSexuality$Sexuality<-factor(employ_datQ37.2.aSexuality$Sexuality)
employ_datQ37.2.aSexuality<-ordinaldatClean(employ_datQ37.2.aSexuality$Q37.2.a, employ_datQ37.2.aSexuality)
#ordinal(employ_datQ37.2.aSexuality$CatOutcome, employ_datQ37.2.aSexuality$Sexuality, employ_datQ37.2.aSexuality)
prep <- analysisPrep(employ_datQ37.2.aSexuality$CatOutcome, employ_datQ37.2.aSexuality$Sexuality, employ_datQ37.2.aSexuality)
analysis <- polr(employ_datQ37.2.aSexuality$CatOutcome ~ employ_datQ37.2.aSexuality$Sexuality, data=employ_datQ37.2.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.3
"Status"
employ_datQ37.3.a<-multidatClean(Q37.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.3.a + Academic, data = employ_datQ37.3.a)
detach(dat_long)
employ_datQ37.3.a<-ordinaldatClean(employ_datQ37.3.a$Q37.3.a, employ_datQ37.3.a)
#ordinal(employ_datQ37.3.a$CatOutcome, employ_datQ37.3.a$Academic, employ_datQ37.3.a)
prep <- analysisPrep(employ_datQ37.3.a$CatOutcome, employ_datQ37.3.a$Academic, employ_datQ37.3.a)
analysis <- polr(employ_datQ37.3.a$CatOutcome ~ employ_datQ37.3.a$Academic, data=employ_datQ37.3.a, Hess=TRUE)
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
Question <- "Q37.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.3.aCarer<-multidatClean(Q37.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aCarer$Carer<- factor(employ_datQ37.3.aCarer$Carer)
employ_datQ37.3.aCarer<-ordinaldatClean(employ_datQ37.3.aCarer$Q37.3.a, employ_datQ37.3.aCarer)
#ordinal(employ_datQ37.3.aCarer$CatOutcome, employ_datQ37.3.aCarer$Carer, employ_datQ37.3.aCarer)
prep <- analysisPrep(employ_datQ37.3.aCarer$CatOutcome, employ_datQ37.3.aCarer$Carer, employ_datQ37.3.aCarer)
analysis <- polr(employ_datQ37.3.aCarer$CatOutcome ~ employ_datQ37.3.aCarer$Carer, data=employ_datQ37.3.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.3.aDisability<-multidatClean(Q37.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aDisability$Disability<- factor(employ_datQ37.3.aDisability$Disability)
employ_datQ37.3.aDisability<-ordinaldatClean(employ_datQ37.3.aDisability$Q37.3.a, employ_datQ37.3.aDisability)
conTable <- xtabs(~Q37.3.a + Disability, data = employ_datQ37.3.aDisability)
#ordinal(employ_datQ37.3.aDisability$CatOutcome, employ_datQ37.3.aDisability$Disability, employ_datQ37.3.aDisability)
prep <- analysisPrep(employ_datQ37.3.aDisability$CatOutcome, employ_datQ37.3.aDisability$Disability, employ_datQ37.3.aDisability)
analysis <- polr(employ_datQ37.3.aDisability$CatOutcome ~ employ_datQ37.3.aDisability$Disability, data=employ_datQ37.3.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.3.aEthnicity<-multidatClean(Q37.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aEthnicity$Ethnicity<- factor(employ_datQ37.3.aEthnicity$EthnicityCleaned)
employ_datQ37.3.aEthnicity<-ordinaldatClean(employ_datQ37.3.aEthnicity$Q37.3.a, employ_datQ37.3.aEthnicity)
conTable <- xtabs(~Q37.3.a + EthnicityCleaned, data = employ_datQ37.3.aEthnicity)
conTable
#ordinal(employ_datQ37.3.aEthnicity$CatOutcome, employ_datQ37.3.aEthnicity$EthnicityCleaned, employ_datQ37.3.aEthnicity)
prep <- analysisPrep(employ_datQ37.3.aEthnicity$CatOutcome, employ_datQ37.3.aEthnicity$EthnicityCleaned, employ_datQ37.3.aEthnicity)
analysis <- polr(employ_datQ37.3.aEthnicity$CatOutcome ~ employ_datQ37.3.aEthnicity$EthnicityCleaned, data=employ_datQ37.3.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.3.aFirstGen<-multidatClean(Q37.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aFirstGen$FirstGen<-factor(employ_datQ37.3.aFirstGen$FirstGen)
employ_datQ37.3.aFirstGen<-ordinaldatClean(employ_datQ37.3.aFirstGen$Q37.3.a, employ_datQ37.3.aFirstGen)
#ordinal(employ_datQ37.3.aFirstGen$CatOutcome, employ_datQ37.3.aFirstGen$FirstGen, employ_datQ37.3.aFirstGen)
prep <- analysisPrep(employ_datQ37.3.aFirstGen$CatOutcome, employ_datQ37.3.aFirstGen$FirstGen, employ_datQ37.3.aFirstGen)
analysis <- polr(employ_datQ37.3.aFirstGen$CatOutcome ~ employ_datQ37.3.aFirstGen$FirstGen, data=employ_datQ37.3.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.3.aGender<-multidatClean(Q37.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aGender$Gender<-factor(employ_datQ37.3.aGender$Gender)
employ_datQ37.3.aGender<-ordinaldatClean(employ_datQ37.3.aGender$Q37.3.a, employ_datQ37.3.aGender)
#ordinal(employ_datQ37.3.aGender$CatOutcome, employ_datQ37.3.aGender$Gender, employ_datQ37.3.aGender)
prep <- analysisPrep(employ_datQ37.3.aGender$CatOutcome, employ_datQ37.3.aGender$Gender, employ_datQ37.3.aGender)
analysis <- polr(employ_datQ37.3.aGender$CatOutcome ~ employ_datQ37.3.aGender$Gender, data=employ_datQ37.3.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.3.aSexuality<-multidatClean(Q37.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.3.aSexuality$Sexuality<-factor(employ_datQ37.3.aSexuality$Sexuality)
employ_datQ37.3.aSexuality<-ordinaldatClean(employ_datQ37.3.aSexuality$Q37.3.a, employ_datQ37.3.aSexuality)
#ordinal(employ_datQ37.3.aSexuality$CatOutcome, employ_datQ37.3.aSexuality$Sexuality, employ_datQ37.3.aSexuality)
prep <- analysisPrep(employ_datQ37.3.aSexuality$CatOutcome, employ_datQ37.3.aSexuality$Sexuality, employ_datQ37.3.aSexuality)
analysis <- polr(employ_datQ37.3.aSexuality$CatOutcome ~ employ_datQ37.3.aSexuality$Sexuality, data=employ_datQ37.3.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.4
"Status"
employ_datQ37.4.a<-multidatClean(Q37.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.4.a + Academic, data = employ_datQ37.4.a)
detach(dat_long)
employ_datQ37.4.a<-ordinaldatClean(employ_datQ37.4.a$Q37.4.a, employ_datQ37.4.a)
#ordinal(employ_datQ37.4.a$CatOutcome, employ_datQ37.4.a$Academic, employ_datQ37.4.a)
prep <- analysisPrep(employ_datQ37.4.a$CatOutcome, employ_datQ37.4.a$Academic, employ_datQ37.4.a)
analysis <- polr(employ_datQ37.4.a$CatOutcome ~ employ_datQ37.4.a$Academic, data=employ_datQ37.4.a, Hess=TRUE)
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
Question <- "Q37.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.4.aCarer<-multidatClean(Q37.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aCarer$Carer<- factor(employ_datQ37.4.aCarer$Carer)
employ_datQ37.4.aCarer<-ordinaldatClean(employ_datQ37.4.aCarer$Q37.4.a, employ_datQ37.4.aCarer)
#ordinal(employ_datQ37.4.aCarer$CatOutcome, employ_datQ37.4.aCarer$Carer, employ_datQ37.4.aCarer)
prep <- analysisPrep(employ_datQ37.4.aCarer$CatOutcome, employ_datQ37.4.aCarer$Carer, employ_datQ37.4.aCarer)
analysis <- polr(employ_datQ37.4.aCarer$CatOutcome ~ employ_datQ37.4.aCarer$Carer, data=employ_datQ37.4.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.4.aDisability<-multidatClean(Q37.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aDisability$Disability<- factor(employ_datQ37.4.aDisability$Disability)
employ_datQ37.4.aDisability<-ordinaldatClean(employ_datQ37.4.aDisability$Q37.4.a, employ_datQ37.4.aDisability)
conTable <- xtabs(~Q37.4.a + Disability, data = employ_datQ37.4.aDisability)
#ordinal(employ_datQ37.4.aDisability$CatOutcome, employ_datQ37.4.aDisability$Disability, employ_datQ37.4.aDisability)
prep <- analysisPrep(employ_datQ37.4.aDisability$CatOutcome, employ_datQ37.4.aDisability$Disability, employ_datQ37.4.aDisability)
analysis <- polr(employ_datQ37.4.aDisability$CatOutcome ~ employ_datQ37.4.aDisability$Disability, data=employ_datQ37.4.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.4.aEthnicity<-multidatClean(Q37.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aEthnicity$Ethnicity<- factor(employ_datQ37.4.aEthnicity$EthnicityCleaned)
employ_datQ37.4.aEthnicity<-ordinaldatClean(employ_datQ37.4.aEthnicity$Q37.4.a, employ_datQ37.4.aEthnicity)
conTable <- xtabs(~Q37.4.a + EthnicityCleaned, data = employ_datQ37.4.aEthnicity)
conTable
#ordinal(employ_datQ37.4.aEthnicity$CatOutcome, employ_datQ37.4.aEthnicity$EthnicityCleaned, employ_datQ37.4.aEthnicity)
prep <- analysisPrep(employ_datQ37.4.aEthnicity$CatOutcome, employ_datQ37.4.aEthnicity$EthnicityCleaned, employ_datQ37.4.aEthnicity)
analysis <- polr(employ_datQ37.4.aEthnicity$CatOutcome ~ employ_datQ37.4.aEthnicity$EthnicityCleaned, data=employ_datQ37.4.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.4.aFirstGen<-multidatClean(Q37.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aFirstGen$FirstGen<-factor(employ_datQ37.4.aFirstGen$FirstGen)
employ_datQ37.4.aFirstGen<-ordinaldatClean(employ_datQ37.4.aFirstGen$Q37.4.a, employ_datQ37.4.aFirstGen)
#ordinal(employ_datQ37.4.aFirstGen$CatOutcome, employ_datQ37.4.aFirstGen$FirstGen, employ_datQ37.4.aFirstGen)
prep <- analysisPrep(employ_datQ37.4.aFirstGen$CatOutcome, employ_datQ37.4.aFirstGen$FirstGen, employ_datQ37.4.aFirstGen)
analysis <- polr(employ_datQ37.4.aFirstGen$CatOutcome ~ employ_datQ37.4.aFirstGen$FirstGen, data=employ_datQ37.4.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.4.aGender<-multidatClean(Q37.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aGender$Gender<-factor(employ_datQ37.4.aGender$Gender)
employ_datQ37.4.aGender<-ordinaldatClean(employ_datQ37.4.aGender$Q37.4.a, employ_datQ37.4.aGender)
#ordinal(employ_datQ37.4.aGender$CatOutcome, employ_datQ37.4.aGender$Gender, employ_datQ37.4.aGender)
prep <- analysisPrep(employ_datQ37.4.aGender$CatOutcome, employ_datQ37.4.aGender$Gender, employ_datQ37.4.aGender)
analysis <- polr(employ_datQ37.4.aGender$CatOutcome ~ employ_datQ37.4.aGender$Gender, data=employ_datQ37.4.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.4.aSexuality<-multidatClean(Q37.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.4.aSexuality$Sexuality<-factor(employ_datQ37.4.aSexuality$Sexuality)
employ_datQ37.4.aSexuality<-ordinaldatClean(employ_datQ37.4.aSexuality$Q37.4.a, employ_datQ37.4.aSexuality)
#ordinal(employ_datQ37.4.aSexuality$CatOutcome, employ_datQ37.4.aSexuality$Sexuality, employ_datQ37.4.aSexuality)
prep <- analysisPrep(employ_datQ37.4.aSexuality$CatOutcome, employ_datQ37.4.aSexuality$Sexuality, employ_datQ37.4.aSexuality)
analysis <- polr(employ_datQ37.4.aSexuality$CatOutcome ~ employ_datQ37.4.aSexuality$Sexuality, data=employ_datQ37.4.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.5
"Status"
employ_datQ37.5.a<-multidatClean(Q37.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.5.a + Academic, data = employ_datQ37.5.a)
detach(dat_long)
employ_datQ37.5.a<-ordinaldatCleanNegative(employ_datQ37.5.a$Q37.5.a, employ_datQ37.5.a)
#ordinal(employ_datQ37.5.a$CatOutcome, employ_datQ37.5.a$Academic, employ_datQ37.5.a)
prep <- analysisPrep(employ_datQ37.5.a$CatOutcome, employ_datQ37.5.a$Academic, employ_datQ37.5.a)
analysis <- polr(employ_datQ37.5.a$CatOutcome ~ employ_datQ37.5.a$Academic, data=employ_datQ37.5.a, Hess=TRUE)
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
Question <- "Q37.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.5.aCarer<-multidatClean(Q37.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aCarer$Carer<- factor(employ_datQ37.5.aCarer$Carer)
employ_datQ37.5.aCarer<-ordinaldatCleanNegative(employ_datQ37.5.aCarer$Q37.5.a, employ_datQ37.5.aCarer)
#ordinal(employ_datQ37.5.aCarer$CatOutcome, employ_datQ37.5.aCarer$Carer, employ_datQ37.5.aCarer)
prep <- analysisPrep(employ_datQ37.5.aCarer$CatOutcome, employ_datQ37.5.aCarer$Carer, employ_datQ37.5.aCarer)
analysis <- polr(employ_datQ37.5.aCarer$CatOutcome ~ employ_datQ37.5.aCarer$Carer, data=employ_datQ37.5.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.5.aDisability<-multidatClean(Q37.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aDisability$Disability<- factor(employ_datQ37.5.aDisability$Disability)
employ_datQ37.5.aDisability<-ordinaldatCleanNegative(employ_datQ37.5.aDisability$Q37.5.a, employ_datQ37.5.aDisability)
conTable <- xtabs(~Q37.5.a + Disability, data = employ_datQ37.5.aDisability)
#ordinal(employ_datQ37.5.aDisability$CatOutcome, employ_datQ37.5.aDisability$Disability, employ_datQ37.5.aDisability)
prep <- analysisPrep(employ_datQ37.5.aDisability$CatOutcome, employ_datQ37.5.aDisability$Disability, employ_datQ37.5.aDisability)
analysis <- polr(employ_datQ37.5.aDisability$CatOutcome ~ employ_datQ37.5.aDisability$Disability, data=employ_datQ37.5.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.5.aEthnicity<-multidatClean(Q37.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aEthnicity$Ethnicity<- factor(employ_datQ37.5.aEthnicity$EthnicityCleaned)
employ_datQ37.5.aEthnicity<-ordinaldatCleanNegative(employ_datQ37.5.aEthnicity$Q37.5.a, employ_datQ37.5.aEthnicity)
conTable <- xtabs(~Q37.5.a + EthnicityCleaned, data = employ_datQ37.5.aEthnicity)
conTable
#ordinal(employ_datQ37.5.aEthnicity$CatOutcome, employ_datQ37.5.aEthnicity$EthnicityCleaned, employ_datQ37.5.aEthnicity)
prep <- analysisPrep(employ_datQ37.5.aEthnicity$CatOutcome, employ_datQ37.5.aEthnicity$EthnicityCleaned, employ_datQ37.5.aEthnicity)
analysis <- polr(employ_datQ37.5.aEthnicity$CatOutcome ~ employ_datQ37.5.aEthnicity$EthnicityCleaned, data=employ_datQ37.5.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.5.aFirstGen<-multidatClean(Q37.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aFirstGen$FirstGen<-factor(employ_datQ37.5.aFirstGen$FirstGen)
employ_datQ37.5.aFirstGen<-ordinaldatCleanNegative(employ_datQ37.5.aFirstGen$Q37.5.a, employ_datQ37.5.aFirstGen)
#ordinal(employ_datQ37.5.aFirstGen$CatOutcome, employ_datQ37.5.aFirstGen$FirstGen, employ_datQ37.5.aFirstGen)
prep <- analysisPrep(employ_datQ37.5.aFirstGen$CatOutcome, employ_datQ37.5.aFirstGen$FirstGen, employ_datQ37.5.aFirstGen)
analysis <- polr(employ_datQ37.5.aFirstGen$CatOutcome ~ employ_datQ37.5.aFirstGen$FirstGen, data=employ_datQ37.5.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.5.aGender<-multidatClean(Q37.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aGender$Gender<-factor(employ_datQ37.5.aGender$Gender)
employ_datQ37.5.aGender<-ordinaldatCleanNegative(employ_datQ37.5.aGender$Q37.5.a, employ_datQ37.5.aGender)
#ordinal(employ_datQ37.5.aGender$CatOutcome, employ_datQ37.5.aGender$Gender, employ_datQ37.5.aGender)
prep <- analysisPrep(employ_datQ37.5.aGender$CatOutcome, employ_datQ37.5.aGender$Gender, employ_datQ37.5.aGender)
analysis <- polr(employ_datQ37.5.aGender$CatOutcome ~ employ_datQ37.5.aGender$Gender, data=employ_datQ37.5.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.5.aSexuality<-multidatClean(Q37.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.5.aSexuality$Sexuality<-factor(employ_datQ37.5.aSexuality$Sexuality)
employ_datQ37.5.aSexuality<-ordinaldatCleanNegative(employ_datQ37.5.aSexuality$Q37.5.a, employ_datQ37.5.aSexuality)
#ordinal(employ_datQ37.5.aSexuality$CatOutcome, employ_datQ37.5.aSexuality$Sexuality, employ_datQ37.5.aSexuality)
prep <- analysisPrep(employ_datQ37.5.aSexuality$CatOutcome, employ_datQ37.5.aSexuality$Sexuality, employ_datQ37.5.aSexuality)
analysis <- polr(employ_datQ37.5.aSexuality$CatOutcome ~ employ_datQ37.5.aSexuality$Sexuality, data=employ_datQ37.5.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.6
"Status"
employ_datQ37.6.a<-multidatClean(Q37.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.6.a + Academic, data = employ_datQ37.6.a)
detach(dat_long)
employ_datQ37.6.a<-ordinaldatCleanNegative(employ_datQ37.6.a$Q37.6.a, employ_datQ37.6.a)
#ordinal(employ_datQ37.6.a$CatOutcome, employ_datQ37.6.a$Academic, employ_datQ37.6.a)
prep <- analysisPrep(employ_datQ37.6.a$CatOutcome, employ_datQ37.6.a$Academic, employ_datQ37.6.a)
analysis <- polr(employ_datQ37.6.a$CatOutcome ~ employ_datQ37.6.a$Academic, data=employ_datQ37.6.a, Hess=TRUE)
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
Question <- "Q37.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.6.aCarer<-multidatClean(Q37.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aCarer$Carer<- factor(employ_datQ37.6.aCarer$Carer)
employ_datQ37.6.aCarer<-ordinaldatCleanNegative(employ_datQ37.6.aCarer$Q37.6.a, employ_datQ37.6.aCarer)
#ordinal(employ_datQ37.6.aCarer$CatOutcome, employ_datQ37.6.aCarer$Carer, employ_datQ37.6.aCarer)
prep <- analysisPrep(employ_datQ37.6.aCarer$CatOutcome, employ_datQ37.6.aCarer$Carer, employ_datQ37.6.aCarer)
analysis <- polr(employ_datQ37.6.aCarer$CatOutcome ~ employ_datQ37.6.aCarer$Carer, data=employ_datQ37.6.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.6.aDisability<-multidatClean(Q37.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aDisability$Disability<- factor(employ_datQ37.6.aDisability$Disability)
employ_datQ37.6.aDisability<-ordinaldatCleanNegative(employ_datQ37.6.aDisability$Q37.6.a, employ_datQ37.6.aDisability)
conTable <- xtabs(~Q37.6.a + Disability, data = employ_datQ37.6.aDisability)
#ordinal(employ_datQ37.6.aDisability$CatOutcome, employ_datQ37.6.aDisability$Disability, employ_datQ37.6.aDisability)
prep <- analysisPrep(employ_datQ37.6.aDisability$CatOutcome, employ_datQ37.6.aDisability$Disability, employ_datQ37.6.aDisability)
analysis <- polr(employ_datQ37.6.aDisability$CatOutcome ~ employ_datQ37.6.aDisability$Disability, data=employ_datQ37.6.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.6.aEthnicity<-multidatClean(Q37.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aEthnicity$Ethnicity<- factor(employ_datQ37.6.aEthnicity$EthnicityCleaned)
employ_datQ37.6.aEthnicity<-ordinaldatCleanNegative(employ_datQ37.6.aEthnicity$Q37.6.a, employ_datQ37.6.aEthnicity)
conTable <- xtabs(~Q37.6.a + EthnicityCleaned, data = employ_datQ37.6.aEthnicity)
conTable
#ordinal(employ_datQ37.6.aEthnicity$CatOutcome, employ_datQ37.6.aEthnicity$EthnicityCleaned, employ_datQ37.6.aEthnicity)
prep <- analysisPrep(employ_datQ37.6.aEthnicity$CatOutcome, employ_datQ37.6.aEthnicity$EthnicityCleaned, employ_datQ37.6.aEthnicity)
analysis <- polr(employ_datQ37.6.aEthnicity$CatOutcome ~ employ_datQ37.6.aEthnicity$EthnicityCleaned, data=employ_datQ37.6.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.6.aFirstGen<-multidatClean(Q37.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aFirstGen$FirstGen<-factor(employ_datQ37.6.aFirstGen$FirstGen)
employ_datQ37.6.aFirstGen<-ordinaldatCleanNegative(employ_datQ37.6.aFirstGen$Q37.6.a, employ_datQ37.6.aFirstGen)
#ordinal(employ_datQ37.6.aFirstGen$CatOutcome, employ_datQ37.6.aFirstGen$FirstGen, employ_datQ37.6.aFirstGen)
prep <- analysisPrep(employ_datQ37.6.aFirstGen$CatOutcome, employ_datQ37.6.aFirstGen$FirstGen, employ_datQ37.6.aFirstGen)
analysis <- polr(employ_datQ37.6.aFirstGen$CatOutcome ~ employ_datQ37.6.aFirstGen$FirstGen, data=employ_datQ37.6.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.6.aGender<-multidatClean(Q37.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aGender$Gender<-factor(employ_datQ37.6.aGender$Gender)
employ_datQ37.6.aGender<-ordinaldatCleanNegative(employ_datQ37.6.aGender$Q37.6.a, employ_datQ37.6.aGender)
#ordinal(employ_datQ37.6.aGender$CatOutcome, employ_datQ37.6.aGender$Gender, employ_datQ37.6.aGender)
prep <- analysisPrep(employ_datQ37.6.aGender$CatOutcome, employ_datQ37.6.aGender$Gender, employ_datQ37.6.aGender)
analysis <- polr(employ_datQ37.6.aGender$CatOutcome ~ employ_datQ37.6.aGender$Gender, data=employ_datQ37.6.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.6.aSexuality<-multidatClean(Q37.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.6.aSexuality$Sexuality<-factor(employ_datQ37.6.aSexuality$Sexuality)
employ_datQ37.6.aSexuality<-ordinaldatCleanNegative(employ_datQ37.6.aSexuality$Q37.6.a, employ_datQ37.6.aSexuality)
#ordinal(employ_datQ37.6.aSexuality$CatOutcome, employ_datQ37.6.aSexuality$Sexuality, employ_datQ37.6.aSexuality)
prep <- analysisPrep(employ_datQ37.6.aSexuality$CatOutcome, employ_datQ37.6.aSexuality$Sexuality, employ_datQ37.6.aSexuality)
analysis <- polr(employ_datQ37.6.aSexuality$CatOutcome ~ employ_datQ37.6.aSexuality$Sexuality, data=employ_datQ37.6.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.7
"Status"
employ_datQ37.7.a<-multidatClean(Q37.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.7.a + Academic, data = employ_datQ37.7.a)
detach(dat_long)
employ_datQ37.7.a<-ordinaldatCleanNegative(employ_datQ37.7.a$Q37.7.a, employ_datQ37.7.a)
#ordinal(employ_datQ37.7.a$CatOutcome, employ_datQ37.7.a$Academic, employ_datQ37.7.a)
prep <- analysisPrep(employ_datQ37.7.a$CatOutcome, employ_datQ37.7.a$Academic, employ_datQ37.7.a)
analysis <- polr(employ_datQ37.7.a$CatOutcome ~ employ_datQ37.7.a$Academic, data=employ_datQ37.7.a, Hess=TRUE)
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
Question <- "Q37.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.7.aCarer<-multidatClean(Q37.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aCarer$Carer<- factor(employ_datQ37.7.aCarer$Carer)
employ_datQ37.7.aCarer<-ordinaldatCleanNegative(employ_datQ37.7.aCarer$Q37.7.a, employ_datQ37.7.aCarer)
#ordinal(employ_datQ37.7.aCarer$CatOutcome, employ_datQ37.7.aCarer$Carer, employ_datQ37.7.aCarer)
prep <- analysisPrep(employ_datQ37.7.aCarer$CatOutcome, employ_datQ37.7.aCarer$Carer, employ_datQ37.7.aCarer)
analysis <- polr(employ_datQ37.7.aCarer$CatOutcome ~ employ_datQ37.7.aCarer$Carer, data=employ_datQ37.7.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.7.aDisability<-multidatClean(Q37.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aDisability$Disability<- factor(employ_datQ37.7.aDisability$Disability)
employ_datQ37.7.aDisability<-ordinaldatCleanNegative(employ_datQ37.7.aDisability$Q37.7.a, employ_datQ37.7.aDisability)
conTable <- xtabs(~Q37.7.a + Disability, data = employ_datQ37.7.aDisability)
#ordinal(employ_datQ37.7.aDisability$CatOutcome, employ_datQ37.7.aDisability$Disability, employ_datQ37.7.aDisability)
prep <- analysisPrep(employ_datQ37.7.aDisability$CatOutcome, employ_datQ37.7.aDisability$Disability, employ_datQ37.7.aDisability)
analysis <- polr(employ_datQ37.7.aDisability$CatOutcome ~ employ_datQ37.7.aDisability$Disability, data=employ_datQ37.7.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.7.aEthnicity<-multidatClean(Q37.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aEthnicity$Ethnicity<- factor(employ_datQ37.7.aEthnicity$EthnicityCleaned)
employ_datQ37.7.aEthnicity<-ordinaldatCleanNegative(employ_datQ37.7.aEthnicity$Q37.7.a, employ_datQ37.7.aEthnicity)
conTable <- xtabs(~Q37.7.a + EthnicityCleaned, data = employ_datQ37.7.aEthnicity)
conTable
#ordinal(employ_datQ37.7.aEthnicity$CatOutcome, employ_datQ37.7.aEthnicity$EthnicityCleaned, employ_datQ37.7.aEthnicity)
prep <- analysisPrep(employ_datQ37.7.aEthnicity$CatOutcome, employ_datQ37.7.aEthnicity$EthnicityCleaned, employ_datQ37.7.aEthnicity)
analysis <- polr(employ_datQ37.7.aEthnicity$CatOutcome ~ employ_datQ37.7.aEthnicity$EthnicityCleaned, data=employ_datQ37.7.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.7.aFirstGen<-multidatClean(Q37.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aFirstGen$FirstGen<-factor(employ_datQ37.7.aFirstGen$FirstGen)
employ_datQ37.7.aFirstGen<-ordinaldatCleanNegative(employ_datQ37.7.aFirstGen$Q37.7.a, employ_datQ37.7.aFirstGen)
#ordinal(employ_datQ37.7.aFirstGen$CatOutcome, employ_datQ37.7.aFirstGen$FirstGen, employ_datQ37.7.aFirstGen)
prep <- analysisPrep(employ_datQ37.7.aFirstGen$CatOutcome, employ_datQ37.7.aFirstGen$FirstGen, employ_datQ37.7.aFirstGen)
analysis <- polr(employ_datQ37.7.aFirstGen$CatOutcome ~ employ_datQ37.7.aFirstGen$FirstGen, data=employ_datQ37.7.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.7.aGender<-multidatClean(Q37.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aGender$Gender<-factor(employ_datQ37.7.aGender$Gender)
employ_datQ37.7.aGender<-ordinaldatCleanNegative(employ_datQ37.7.aGender$Q37.7.a, employ_datQ37.7.aGender)
#ordinal(employ_datQ37.7.aGender$CatOutcome, employ_datQ37.7.aGender$Gender, employ_datQ37.7.aGender)
prep <- analysisPrep(employ_datQ37.7.aGender$CatOutcome, employ_datQ37.7.aGender$Gender, employ_datQ37.7.aGender)
analysis <- polr(employ_datQ37.7.aGender$CatOutcome ~ employ_datQ37.7.aGender$Gender, data=employ_datQ37.7.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.7.aSexuality<-multidatClean(Q37.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.7.aSexuality$Sexuality<-factor(employ_datQ37.7.aSexuality$Sexuality)
employ_datQ37.7.aSexuality<-ordinaldatCleanNegative(employ_datQ37.7.aSexuality$Q37.7.a, employ_datQ37.7.aSexuality)
#ordinal(employ_datQ37.7.aSexuality$CatOutcome, employ_datQ37.7.aSexuality$Sexuality, employ_datQ37.7.aSexuality)
prep <- analysisPrep(employ_datQ37.7.aSexuality$CatOutcome, employ_datQ37.7.aSexuality$Sexuality, employ_datQ37.7.aSexuality)
analysis <- polr(employ_datQ37.7.aSexuality$CatOutcome ~ employ_datQ37.7.aSexuality$Sexuality, data=employ_datQ37.7.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.8
"Status"
employ_datQ37.8.a<-multidatClean(Q37.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.8.a + Academic, data = employ_datQ37.8.a)
detach(dat_long)
employ_datQ37.8.a<-ordinaldatClean(employ_datQ37.8.a$Q37.8.a, employ_datQ37.8.a)
#ordinal(employ_datQ37.8.a$CatOutcome, employ_datQ37.8.a$Academic, employ_datQ37.8.a)
prep <- analysisPrep(employ_datQ37.8.a$CatOutcome, employ_datQ37.8.a$Academic, employ_datQ37.8.a)
analysis <- polr(employ_datQ37.8.a$CatOutcome ~ employ_datQ37.8.a$Academic, data=employ_datQ37.8.a, Hess=TRUE)
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
Question <- "Q37.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.8.aCarer<-multidatClean(Q37.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aCarer$Carer<- factor(employ_datQ37.8.aCarer$Carer)
employ_datQ37.8.aCarer<-ordinaldatClean(employ_datQ37.8.aCarer$Q37.8.a, employ_datQ37.8.aCarer)
#ordinal(employ_datQ37.8.aCarer$CatOutcome, employ_datQ37.8.aCarer$Carer, employ_datQ37.8.aCarer)
prep <- analysisPrep(employ_datQ37.8.aCarer$CatOutcome, employ_datQ37.8.aCarer$Carer, employ_datQ37.8.aCarer)
analysis <- polr(employ_datQ37.8.aCarer$CatOutcome ~ employ_datQ37.8.aCarer$Carer, data=employ_datQ37.8.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.8.aDisability<-multidatClean(Q37.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aDisability$Disability<- factor(employ_datQ37.8.aDisability$Disability)
employ_datQ37.8.aDisability<-ordinaldatClean(employ_datQ37.8.aDisability$Q37.8.a, employ_datQ37.8.aDisability)
conTable <- xtabs(~Q37.8.a + Disability, data = employ_datQ37.8.aDisability)
#ordinal(employ_datQ37.8.aDisability$CatOutcome, employ_datQ37.8.aDisability$Disability, employ_datQ37.8.aDisability)
prep <- analysisPrep(employ_datQ37.8.aDisability$CatOutcome, employ_datQ37.8.aDisability$Disability, employ_datQ37.8.aDisability)
analysis <- polr(employ_datQ37.8.aDisability$CatOutcome ~ employ_datQ37.8.aDisability$Disability, data=employ_datQ37.8.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.8.aEthnicity<-multidatClean(Q37.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aEthnicity$Ethnicity<- factor(employ_datQ37.8.aEthnicity$EthnicityCleaned)
employ_datQ37.8.aEthnicity<-ordinaldatClean(employ_datQ37.8.aEthnicity$Q37.8.a, employ_datQ37.8.aEthnicity)
conTable <- xtabs(~Q37.8.a + EthnicityCleaned, data = employ_datQ37.8.aEthnicity)
conTable
#ordinal(employ_datQ37.8.aEthnicity$CatOutcome, employ_datQ37.8.aEthnicity$EthnicityCleaned, employ_datQ37.8.aEthnicity)
prep <- analysisPrep(employ_datQ37.8.aEthnicity$CatOutcome, employ_datQ37.8.aEthnicity$EthnicityCleaned, employ_datQ37.8.aEthnicity)
analysis <- polr(employ_datQ37.8.aEthnicity$CatOutcome ~ employ_datQ37.8.aEthnicity$EthnicityCleaned, data=employ_datQ37.8.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.8.aFirstGen<-multidatClean(Q37.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aFirstGen$FirstGen<-factor(employ_datQ37.8.aFirstGen$FirstGen)
employ_datQ37.8.aFirstGen<-ordinaldatClean(employ_datQ37.8.aFirstGen$Q37.8.a, employ_datQ37.8.aFirstGen)
#ordinal(employ_datQ37.8.aFirstGen$CatOutcome, employ_datQ37.8.aFirstGen$FirstGen, employ_datQ37.8.aFirstGen)
prep <- analysisPrep(employ_datQ37.8.aFirstGen$CatOutcome, employ_datQ37.8.aFirstGen$FirstGen, employ_datQ37.8.aFirstGen)
analysis <- polr(employ_datQ37.8.aFirstGen$CatOutcome ~ employ_datQ37.8.aFirstGen$FirstGen, data=employ_datQ37.8.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.8.aGender<-multidatClean(Q37.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aGender$Gender<-factor(employ_datQ37.8.aGender$Gender)
employ_datQ37.8.aGender<-ordinaldatClean(employ_datQ37.8.aGender$Q37.8.a, employ_datQ37.8.aGender)
#ordinal(employ_datQ37.8.aGender$CatOutcome, employ_datQ37.8.aGender$Gender, employ_datQ37.8.aGender)
prep <- analysisPrep(employ_datQ37.8.aGender$CatOutcome, employ_datQ37.8.aGender$Gender, employ_datQ37.8.aGender)
analysis <- polr(employ_datQ37.8.aGender$CatOutcome ~ employ_datQ37.8.aGender$Gender, data=employ_datQ37.8.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.8.aSexuality<-multidatClean(Q37.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.8.aSexuality$Sexuality<-factor(employ_datQ37.8.aSexuality$Sexuality)
employ_datQ37.8.aSexuality<-ordinaldatClean(employ_datQ37.8.aSexuality$Q37.8.a, employ_datQ37.8.aSexuality)
#ordinal(employ_datQ37.8.aSexuality$CatOutcome, employ_datQ37.8.aSexuality$Sexuality, employ_datQ37.8.aSexuality)
prep <- analysisPrep(employ_datQ37.8.aSexuality$CatOutcome, employ_datQ37.8.aSexuality$Sexuality, employ_datQ37.8.aSexuality)
analysis <- polr(employ_datQ37.8.aSexuality$CatOutcome ~ employ_datQ37.8.aSexuality$Sexuality, data=employ_datQ37.8.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.9
"Status"
employ_datQ37.9.a<-multidatClean(Q37.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.9.a + Academic, data = employ_datQ37.9.a)
detach(dat_long)
employ_datQ37.9.a<-ordinaldatCleanNegative(employ_datQ37.9.a$Q37.9.a, employ_datQ37.9.a)
#ordinal(employ_datQ37.9.a$CatOutcome, employ_datQ37.9.a$Academic, employ_datQ37.9.a)
prep <- analysisPrep(employ_datQ37.9.a$CatOutcome, employ_datQ37.9.a$Academic, employ_datQ37.9.a)
analysis <- polr(employ_datQ37.9.a$CatOutcome ~ employ_datQ37.9.a$Academic, data=employ_datQ37.9.a, Hess=TRUE)
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
Question <- "Q37.9.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.9.aCarer<-multidatClean(Q37.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aCarer$Carer<- factor(employ_datQ37.9.aCarer$Carer)
employ_datQ37.9.aCarer<-ordinaldatCleanNegative(employ_datQ37.9.aCarer$Q37.9.a, employ_datQ37.9.aCarer)
#ordinal(employ_datQ37.9.aCarer$CatOutcome, employ_datQ37.9.aCarer$Carer, employ_datQ37.9.aCarer)
prep <- analysisPrep(employ_datQ37.9.aCarer$CatOutcome, employ_datQ37.9.aCarer$Carer, employ_datQ37.9.aCarer)
analysis <- polr(employ_datQ37.9.aCarer$CatOutcome ~ employ_datQ37.9.aCarer$Carer, data=employ_datQ37.9.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.9.aDisability<-multidatClean(Q37.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aDisability$Disability<- factor(employ_datQ37.9.aDisability$Disability)
employ_datQ37.9.aDisability<-ordinaldatCleanNegative(employ_datQ37.9.aDisability$Q37.9.a, employ_datQ37.9.aDisability)
conTable <- xtabs(~Q37.9.a + Disability, data = employ_datQ37.9.aDisability)
#ordinal(employ_datQ37.9.aDisability$CatOutcome, employ_datQ37.9.aDisability$Disability, employ_datQ37.9.aDisability)
prep <- analysisPrep(employ_datQ37.9.aDisability$CatOutcome, employ_datQ37.9.aDisability$Disability, employ_datQ37.9.aDisability)
analysis <- polr(employ_datQ37.9.aDisability$CatOutcome ~ employ_datQ37.9.aDisability$Disability, data=employ_datQ37.9.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.9.aEthnicity<-multidatClean(Q37.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aEthnicity$Ethnicity<- factor(employ_datQ37.9.aEthnicity$EthnicityCleaned)
employ_datQ37.9.aEthnicity<-ordinaldatCleanNegative(employ_datQ37.9.aEthnicity$Q37.9.a, employ_datQ37.9.aEthnicity)
conTable <- xtabs(~Q37.9.a + EthnicityCleaned, data = employ_datQ37.9.aEthnicity)
conTable
#ordinal(employ_datQ37.9.aEthnicity$CatOutcome, employ_datQ37.9.aEthnicity$EthnicityCleaned, employ_datQ37.9.aEthnicity)
prep <- analysisPrep(employ_datQ37.9.aEthnicity$CatOutcome, employ_datQ37.9.aEthnicity$EthnicityCleaned, employ_datQ37.9.aEthnicity)
analysis <- polr(employ_datQ37.9.aEthnicity$CatOutcome ~ employ_datQ37.9.aEthnicity$EthnicityCleaned, data=employ_datQ37.9.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.9.aFirstGen<-multidatClean(Q37.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aFirstGen$FirstGen<-factor(employ_datQ37.9.aFirstGen$FirstGen)
employ_datQ37.9.aFirstGen<-ordinaldatCleanNegative(employ_datQ37.9.aFirstGen$Q37.9.a, employ_datQ37.9.aFirstGen)
#ordinal(employ_datQ37.9.aFirstGen$CatOutcome, employ_datQ37.9.aFirstGen$FirstGen, employ_datQ37.9.aFirstGen)
prep <- analysisPrep(employ_datQ37.9.aFirstGen$CatOutcome, employ_datQ37.9.aFirstGen$FirstGen, employ_datQ37.9.aFirstGen)
analysis <- polr(employ_datQ37.9.aFirstGen$CatOutcome ~ employ_datQ37.9.aFirstGen$FirstGen, data=employ_datQ37.9.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.9.aGender<-multidatClean(Q37.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aGender$Gender<-factor(employ_datQ37.9.aGender$Gender)
employ_datQ37.9.aGender<-ordinaldatCleanNegative(employ_datQ37.9.aGender$Q37.9.a, employ_datQ37.9.aGender)
#ordinal(employ_datQ37.9.aGender$CatOutcome, employ_datQ37.9.aGender$Gender, employ_datQ37.9.aGender)
prep <- analysisPrep(employ_datQ37.9.aGender$CatOutcome, employ_datQ37.9.aGender$Gender, employ_datQ37.9.aGender)
analysis <- polr(employ_datQ37.9.aGender$CatOutcome ~ employ_datQ37.9.aGender$Gender, data=employ_datQ37.9.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.9.aSexuality<-multidatClean(Q37.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.9.aSexuality$Sexuality<-factor(employ_datQ37.9.aSexuality$Sexuality)
employ_datQ37.9.aSexuality<-ordinaldatCleanNegative(employ_datQ37.9.aSexuality$Q37.9.a, employ_datQ37.9.aSexuality)
#ordinal(employ_datQ37.9.aSexuality$CatOutcome, employ_datQ37.9.aSexuality$Sexuality, employ_datQ37.9.aSexuality)
prep <- analysisPrep(employ_datQ37.9.aSexuality$CatOutcome, employ_datQ37.9.aSexuality$Sexuality, employ_datQ37.9.aSexuality)
analysis <- polr(employ_datQ37.9.aSexuality$CatOutcome ~ employ_datQ37.9.aSexuality$Sexuality, data=employ_datQ37.9.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.9.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.10
"Status"
employ_datQ37.10.a<-multidatClean(Q37.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.10.a + Academic, data = employ_datQ37.10.a)
detach(dat_long)
employ_datQ37.10.a<-ordinaldatClean(employ_datQ37.10.a$Q37.10.a, employ_datQ37.10.a)
#ordinal(employ_datQ37.10.a$CatOutcome, employ_datQ37.10.a$Academic, employ_datQ37.10.a)
prep <- analysisPrep(employ_datQ37.10.a$CatOutcome, employ_datQ37.10.a$Academic, employ_datQ37.10.a)
analysis <- polr(employ_datQ37.10.a$CatOutcome ~ employ_datQ37.10.a$Academic, data=employ_datQ37.10.a, Hess=TRUE)
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
Question <- "Q37.10.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.10.aCarer<-multidatClean(Q37.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aCarer$Carer<- factor(employ_datQ37.10.aCarer$Carer)
employ_datQ37.10.aCarer<-ordinaldatClean(employ_datQ37.10.aCarer$Q37.10.a, employ_datQ37.10.aCarer)
#ordinal(employ_datQ37.10.aCarer$CatOutcome, employ_datQ37.10.aCarer$Carer, employ_datQ37.10.aCarer)
prep <- analysisPrep(employ_datQ37.10.aCarer$CatOutcome, employ_datQ37.10.aCarer$Carer, employ_datQ37.10.aCarer)
analysis <- polr(employ_datQ37.10.aCarer$CatOutcome ~ employ_datQ37.10.aCarer$Carer, data=employ_datQ37.10.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.10.aDisability<-multidatClean(Q37.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aDisability$Disability<- factor(employ_datQ37.10.aDisability$Disability)
employ_datQ37.10.aDisability<-ordinaldatClean(employ_datQ37.10.aDisability$Q37.10.a, employ_datQ37.10.aDisability)
conTable <- xtabs(~Q37.10.a + Disability, data = employ_datQ37.10.aDisability)
#ordinal(employ_datQ37.10.aDisability$CatOutcome, employ_datQ37.10.aDisability$Disability, employ_datQ37.10.aDisability)
prep <- analysisPrep(employ_datQ37.10.aDisability$CatOutcome, employ_datQ37.10.aDisability$Disability, employ_datQ37.10.aDisability)
analysis <- polr(employ_datQ37.10.aDisability$CatOutcome ~ employ_datQ37.10.aDisability$Disability, data=employ_datQ37.10.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.10.aEthnicity<-multidatClean(Q37.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aEthnicity$Ethnicity<- factor(employ_datQ37.10.aEthnicity$EthnicityCleaned)
employ_datQ37.10.aEthnicity<-ordinaldatClean(employ_datQ37.10.aEthnicity$Q37.10.a, employ_datQ37.10.aEthnicity)
conTable <- xtabs(~Q37.10.a + EthnicityCleaned, data = employ_datQ37.10.aEthnicity)
conTable
#ordinal(employ_datQ37.10.aEthnicity$CatOutcome, employ_datQ37.10.aEthnicity$EthnicityCleaned, employ_datQ37.10.aEthnicity)
prep <- analysisPrep(employ_datQ37.10.aEthnicity$CatOutcome, employ_datQ37.10.aEthnicity$EthnicityCleaned, employ_datQ37.10.aEthnicity)
analysis <- polr(employ_datQ37.10.aEthnicity$CatOutcome ~ employ_datQ37.10.aEthnicity$EthnicityCleaned, data=employ_datQ37.10.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.10.aFirstGen<-multidatClean(Q37.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aFirstGen$FirstGen<-factor(employ_datQ37.10.aFirstGen$FirstGen)
employ_datQ37.10.aFirstGen<-ordinaldatClean(employ_datQ37.10.aFirstGen$Q37.10.a, employ_datQ37.10.aFirstGen)
#ordinal(employ_datQ37.10.aFirstGen$CatOutcome, employ_datQ37.10.aFirstGen$FirstGen, employ_datQ37.10.aFirstGen)
prep <- analysisPrep(employ_datQ37.10.aFirstGen$CatOutcome, employ_datQ37.10.aFirstGen$FirstGen, employ_datQ37.10.aFirstGen)
analysis <- polr(employ_datQ37.10.aFirstGen$CatOutcome ~ employ_datQ37.10.aFirstGen$FirstGen, data=employ_datQ37.10.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.10.aGender<-multidatClean(Q37.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aGender$Gender<-factor(employ_datQ37.10.aGender$Gender)
employ_datQ37.10.aGender<-ordinaldatClean(employ_datQ37.10.aGender$Q37.10.a, employ_datQ37.10.aGender)
#ordinal(employ_datQ37.10.aGender$CatOutcome, employ_datQ37.10.aGender$Gender, employ_datQ37.10.aGender)
prep <- analysisPrep(employ_datQ37.10.aGender$CatOutcome, employ_datQ37.10.aGender$Gender, employ_datQ37.10.aGender)
analysis <- polr(employ_datQ37.10.aGender$CatOutcome ~ employ_datQ37.10.aGender$Gender, data=employ_datQ37.10.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.10.aSexuality<-multidatClean(Q37.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.10.aSexuality$Sexuality<-factor(employ_datQ37.10.aSexuality$Sexuality)
employ_datQ37.10.aSexuality<-ordinaldatClean(employ_datQ37.10.aSexuality$Q37.10.a, employ_datQ37.10.aSexuality)
#ordinal(employ_datQ37.10.aSexuality$CatOutcome, employ_datQ37.10.aSexuality$Sexuality, employ_datQ37.10.aSexuality)
prep <- analysisPrep(employ_datQ37.10.aSexuality$CatOutcome, employ_datQ37.10.aSexuality$Sexuality, employ_datQ37.10.aSexuality)
analysis <- polr(employ_datQ37.10.aSexuality$CatOutcome ~ employ_datQ37.10.aSexuality$Sexuality, data=employ_datQ37.10.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.10.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.11
"Status"
employ_datQ37.11.a<-multidatClean(Q37.11.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.11.a + Academic, data = employ_datQ37.11.a)
detach(dat_long)
employ_datQ37.11.a<-ordinaldatClean(employ_datQ37.11.a$Q37.11.a, employ_datQ37.11.a)
#ordinal(employ_datQ37.11.a$CatOutcome, employ_datQ37.11.a$Academic, employ_datQ37.11.a)
prep <- analysisPrep(employ_datQ37.11.a$CatOutcome, employ_datQ37.11.a$Academic, employ_datQ37.11.a)
analysis <- polr(employ_datQ37.11.a$CatOutcome ~ employ_datQ37.11.a$Academic, data=employ_datQ37.11.a, Hess=TRUE)
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
Question <- "Q37.11.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.11.aCarer<-multidatClean(Q37.11.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aCarer$Carer<- factor(employ_datQ37.11.aCarer$Carer)
employ_datQ37.11.aCarer<-ordinaldatClean(employ_datQ37.11.aCarer$Q37.11.a, employ_datQ37.11.aCarer)
#ordinal(employ_datQ37.11.aCarer$CatOutcome, employ_datQ37.11.aCarer$Carer, employ_datQ37.11.aCarer)
prep <- analysisPrep(employ_datQ37.11.aCarer$CatOutcome, employ_datQ37.11.aCarer$Carer, employ_datQ37.11.aCarer)
analysis <- polr(employ_datQ37.11.aCarer$CatOutcome ~ employ_datQ37.11.aCarer$Carer, data=employ_datQ37.11.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.11.aDisability<-multidatClean(Q37.11.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aDisability$Disability<- factor(employ_datQ37.11.aDisability$Disability)
employ_datQ37.11.aDisability<-ordinaldatClean(employ_datQ37.11.aDisability$Q37.11.a, employ_datQ37.11.aDisability)
conTable <- xtabs(~Q37.11.a + Disability, data = employ_datQ37.11.aDisability)
#ordinal(employ_datQ37.11.aDisability$CatOutcome, employ_datQ37.11.aDisability$Disability, employ_datQ37.11.aDisability)
prep <- analysisPrep(employ_datQ37.11.aDisability$CatOutcome, employ_datQ37.11.aDisability$Disability, employ_datQ37.11.aDisability)
analysis <- polr(employ_datQ37.11.aDisability$CatOutcome ~ employ_datQ37.11.aDisability$Disability, data=employ_datQ37.11.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.11.aEthnicity<-multidatClean(Q37.11.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aEthnicity$Ethnicity<- factor(employ_datQ37.11.aEthnicity$EthnicityCleaned)
employ_datQ37.11.aEthnicity<-ordinaldatClean(employ_datQ37.11.aEthnicity$Q37.11.a, employ_datQ37.11.aEthnicity)
conTable <- xtabs(~Q37.11.a + EthnicityCleaned, data = employ_datQ37.11.aEthnicity)
conTable
#ordinal(employ_datQ37.11.aEthnicity$CatOutcome, employ_datQ37.11.aEthnicity$EthnicityCleaned, employ_datQ37.11.aEthnicity)
prep <- analysisPrep(employ_datQ37.11.aEthnicity$CatOutcome, employ_datQ37.11.aEthnicity$EthnicityCleaned, employ_datQ37.11.aEthnicity)
analysis <- polr(employ_datQ37.11.aEthnicity$CatOutcome ~ employ_datQ37.11.aEthnicity$EthnicityCleaned, data=employ_datQ37.11.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.11.aFirstGen<-multidatClean(Q37.11.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aFirstGen$FirstGen<-factor(employ_datQ37.11.aFirstGen$FirstGen)
employ_datQ37.11.aFirstGen<-ordinaldatClean(employ_datQ37.11.aFirstGen$Q37.11.a, employ_datQ37.11.aFirstGen)
#ordinal(employ_datQ37.11.aFirstGen$CatOutcome, employ_datQ37.11.aFirstGen$FirstGen, employ_datQ37.11.aFirstGen)
prep <- analysisPrep(employ_datQ37.11.aFirstGen$CatOutcome, employ_datQ37.11.aFirstGen$FirstGen, employ_datQ37.11.aFirstGen)
analysis <- polr(employ_datQ37.11.aFirstGen$CatOutcome ~ employ_datQ37.11.aFirstGen$FirstGen, data=employ_datQ37.11.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.11.aGender<-multidatClean(Q37.11.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aGender$Gender<-factor(employ_datQ37.11.aGender$Gender)
employ_datQ37.11.aGender<-ordinaldatClean(employ_datQ37.11.aGender$Q37.11.a, employ_datQ37.11.aGender)
#ordinal(employ_datQ37.11.aGender$CatOutcome, employ_datQ37.11.aGender$Gender, employ_datQ37.11.aGender)
prep <- analysisPrep(employ_datQ37.11.aGender$CatOutcome, employ_datQ37.11.aGender$Gender, employ_datQ37.11.aGender)
analysis <- polr(employ_datQ37.11.aGender$CatOutcome ~ employ_datQ37.11.aGender$Gender, data=employ_datQ37.11.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.11.aSexuality<-multidatClean(Q37.11.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.11.aSexuality$Sexuality<-factor(employ_datQ37.11.aSexuality$Sexuality)
employ_datQ37.11.aSexuality<-ordinaldatClean(employ_datQ37.11.aSexuality$Q37.11.a, employ_datQ37.11.aSexuality)
#ordinal(employ_datQ37.11.aSexuality$CatOutcome, employ_datQ37.11.aSexuality$Sexuality, employ_datQ37.11.aSexuality)
prep <- analysisPrep(employ_datQ37.11.aSexuality$CatOutcome, employ_datQ37.11.aSexuality$Sexuality, employ_datQ37.11.aSexuality)
analysis <- polr(employ_datQ37.11.aSexuality$CatOutcome ~ employ_datQ37.11.aSexuality$Sexuality, data=employ_datQ37.11.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.11.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.12
"Status"
employ_datQ37.12.a<-multidatClean(Q37.12.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.12.a + Academic, data = employ_datQ37.12.a)
detach(dat_long)
employ_datQ37.12.a<-ordinaldatClean(employ_datQ37.12.a$Q37.12.a, employ_datQ37.12.a)
#ordinal(employ_datQ37.12.a$CatOutcome, employ_datQ37.12.a$Academic, employ_datQ37.12.a)
prep <- analysisPrep(employ_datQ37.12.a$CatOutcome, employ_datQ37.12.a$Academic, employ_datQ37.12.a)
analysis <- polr(employ_datQ37.12.a$CatOutcome ~ employ_datQ37.12.a$Academic, data=employ_datQ37.12.a, Hess=TRUE)
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
Question <- "Q37.12.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.12.aCarer<-multidatClean(Q37.12.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aCarer$Carer<- factor(employ_datQ37.12.aCarer$Carer)
employ_datQ37.12.aCarer<-ordinaldatClean(employ_datQ37.12.aCarer$Q37.12.a, employ_datQ37.12.aCarer)
#ordinal(employ_datQ37.12.aCarer$CatOutcome, employ_datQ37.12.aCarer$Carer, employ_datQ37.12.aCarer)
prep <- analysisPrep(employ_datQ37.12.aCarer$CatOutcome, employ_datQ37.12.aCarer$Carer, employ_datQ37.12.aCarer)
analysis <- polr(employ_datQ37.12.aCarer$CatOutcome ~ employ_datQ37.12.aCarer$Carer, data=employ_datQ37.12.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.12.aDisability<-multidatClean(Q37.12.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aDisability$Disability<- factor(employ_datQ37.12.aDisability$Disability)
employ_datQ37.12.aDisability<-ordinaldatClean(employ_datQ37.12.aDisability$Q37.12.a, employ_datQ37.12.aDisability)
conTable <- xtabs(~Q37.12.a + Disability, data = employ_datQ37.12.aDisability)
#ordinal(employ_datQ37.12.aDisability$CatOutcome, employ_datQ37.12.aDisability$Disability, employ_datQ37.12.aDisability)
prep <- analysisPrep(employ_datQ37.12.aDisability$CatOutcome, employ_datQ37.12.aDisability$Disability, employ_datQ37.12.aDisability)
analysis <- polr(employ_datQ37.12.aDisability$CatOutcome ~ employ_datQ37.12.aDisability$Disability, data=employ_datQ37.12.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.12.aEthnicity<-multidatClean(Q37.12.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aEthnicity$Ethnicity<- factor(employ_datQ37.12.aEthnicity$EthnicityCleaned)
employ_datQ37.12.aEthnicity<-ordinaldatClean(employ_datQ37.12.aEthnicity$Q37.12.a, employ_datQ37.12.aEthnicity)
conTable <- xtabs(~Q37.12.a + EthnicityCleaned, data = employ_datQ37.12.aEthnicity)
conTable
#ordinal(employ_datQ37.12.aEthnicity$CatOutcome, employ_datQ37.12.aEthnicity$EthnicityCleaned, employ_datQ37.12.aEthnicity)
prep <- analysisPrep(employ_datQ37.12.aEthnicity$CatOutcome, employ_datQ37.12.aEthnicity$EthnicityCleaned, employ_datQ37.12.aEthnicity)
analysis <- polr(employ_datQ37.12.aEthnicity$CatOutcome ~ employ_datQ37.12.aEthnicity$EthnicityCleaned, data=employ_datQ37.12.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.12.aFirstGen<-multidatClean(Q37.12.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aFirstGen$FirstGen<-factor(employ_datQ37.12.aFirstGen$FirstGen)
employ_datQ37.12.aFirstGen<-ordinaldatClean(employ_datQ37.12.aFirstGen$Q37.12.a, employ_datQ37.12.aFirstGen)
#ordinal(employ_datQ37.12.aFirstGen$CatOutcome, employ_datQ37.12.aFirstGen$FirstGen, employ_datQ37.12.aFirstGen)
prep <- analysisPrep(employ_datQ37.12.aFirstGen$CatOutcome, employ_datQ37.12.aFirstGen$FirstGen, employ_datQ37.12.aFirstGen)
analysis <- polr(employ_datQ37.12.aFirstGen$CatOutcome ~ employ_datQ37.12.aFirstGen$FirstGen, data=employ_datQ37.12.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.12.aGender<-multidatClean(Q37.12.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aGender$Gender<-factor(employ_datQ37.12.aGender$Gender)
employ_datQ37.12.aGender<-ordinaldatClean(employ_datQ37.12.aGender$Q37.12.a, employ_datQ37.12.aGender)
#ordinal(employ_datQ37.12.aGender$CatOutcome, employ_datQ37.12.aGender$Gender, employ_datQ37.12.aGender)
prep <- analysisPrep(employ_datQ37.12.aGender$CatOutcome, employ_datQ37.12.aGender$Gender, employ_datQ37.12.aGender)
analysis <- polr(employ_datQ37.12.aGender$CatOutcome ~ employ_datQ37.12.aGender$Gender, data=employ_datQ37.12.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.12.aSexuality<-multidatClean(Q37.12.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.12.aSexuality$Sexuality<-factor(employ_datQ37.12.aSexuality$Sexuality)
employ_datQ37.12.aSexuality<-ordinaldatClean(employ_datQ37.12.aSexuality$Q37.12.a, employ_datQ37.12.aSexuality)
#ordinal(employ_datQ37.12.aSexuality$CatOutcome, employ_datQ37.12.aSexuality$Sexuality, employ_datQ37.12.aSexuality)
prep <- analysisPrep(employ_datQ37.12.aSexuality$CatOutcome, employ_datQ37.12.aSexuality$Sexuality, employ_datQ37.12.aSexuality)
analysis <- polr(employ_datQ37.12.aSexuality$CatOutcome ~ employ_datQ37.12.aSexuality$Sexuality, data=employ_datQ37.12.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.12.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.13
"Status"
employ_datQ37.13.a<-multidatClean(Q37.13.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.13.a + Academic, data = employ_datQ37.13.a)
detach(dat_long)
employ_datQ37.13.a<-ordinaldatClean(employ_datQ37.13.a$Q37.13.a, employ_datQ37.13.a)
#ordinal(employ_datQ37.13.a$CatOutcome, employ_datQ37.13.a$Academic, employ_datQ37.13.a)
prep <- analysisPrep(employ_datQ37.13.a$CatOutcome, employ_datQ37.13.a$Academic, employ_datQ37.13.a)
analysis <- polr(employ_datQ37.13.a$CatOutcome ~ employ_datQ37.13.a$Academic, data=employ_datQ37.13.a, Hess=TRUE)
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
Question <- "Q37.13.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.13.aCarer<-multidatClean(Q37.13.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aCarer$Carer<- factor(employ_datQ37.13.aCarer$Carer)
employ_datQ37.13.aCarer<-ordinaldatClean(employ_datQ37.13.aCarer$Q37.13.a, employ_datQ37.13.aCarer)
#ordinal(employ_datQ37.13.aCarer$CatOutcome, employ_datQ37.13.aCarer$Carer, employ_datQ37.13.aCarer)
prep <- analysisPrep(employ_datQ37.13.aCarer$CatOutcome, employ_datQ37.13.aCarer$Carer, employ_datQ37.13.aCarer)
analysis <- polr(employ_datQ37.13.aCarer$CatOutcome ~ employ_datQ37.13.aCarer$Carer, data=employ_datQ37.13.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.13.aDisability<-multidatClean(Q37.13.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aDisability$Disability<- factor(employ_datQ37.13.aDisability$Disability)
employ_datQ37.13.aDisability<-ordinaldatClean(employ_datQ37.13.aDisability$Q37.13.a, employ_datQ37.13.aDisability)
conTable <- xtabs(~Q37.13.a + Disability, data = employ_datQ37.13.aDisability)
#ordinal(employ_datQ37.13.aDisability$CatOutcome, employ_datQ37.13.aDisability$Disability, employ_datQ37.13.aDisability)
prep <- analysisPrep(employ_datQ37.13.aDisability$CatOutcome, employ_datQ37.13.aDisability$Disability, employ_datQ37.13.aDisability)
analysis <- polr(employ_datQ37.13.aDisability$CatOutcome ~ employ_datQ37.13.aDisability$Disability, data=employ_datQ37.13.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.13.aEthnicity<-multidatClean(Q37.13.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aEthnicity$Ethnicity<- factor(employ_datQ37.13.aEthnicity$EthnicityCleaned)
employ_datQ37.13.aEthnicity<-ordinaldatClean(employ_datQ37.13.aEthnicity$Q37.13.a, employ_datQ37.13.aEthnicity)
conTable <- xtabs(~Q37.13.a + EthnicityCleaned, data = employ_datQ37.13.aEthnicity)
conTable
#ordinal(employ_datQ37.13.aEthnicity$CatOutcome, employ_datQ37.13.aEthnicity$EthnicityCleaned, employ_datQ37.13.aEthnicity)
prep <- analysisPrep(employ_datQ37.13.aEthnicity$CatOutcome, employ_datQ37.13.aEthnicity$EthnicityCleaned, employ_datQ37.13.aEthnicity)
analysis <- polr(employ_datQ37.13.aEthnicity$CatOutcome ~ employ_datQ37.13.aEthnicity$EthnicityCleaned, data=employ_datQ37.13.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.13.aFirstGen<-multidatClean(Q37.13.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aFirstGen$FirstGen<-factor(employ_datQ37.13.aFirstGen$FirstGen)
employ_datQ37.13.aFirstGen<-ordinaldatClean(employ_datQ37.13.aFirstGen$Q37.13.a, employ_datQ37.13.aFirstGen)
#ordinal(employ_datQ37.13.aFirstGen$CatOutcome, employ_datQ37.13.aFirstGen$FirstGen, employ_datQ37.13.aFirstGen)
prep <- analysisPrep(employ_datQ37.13.aFirstGen$CatOutcome, employ_datQ37.13.aFirstGen$FirstGen, employ_datQ37.13.aFirstGen)
analysis <- polr(employ_datQ37.13.aFirstGen$CatOutcome ~ employ_datQ37.13.aFirstGen$FirstGen, data=employ_datQ37.13.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.13.aGender<-multidatClean(Q37.13.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aGender$Gender<-factor(employ_datQ37.13.aGender$Gender)
employ_datQ37.13.aGender<-ordinaldatClean(employ_datQ37.13.aGender$Q37.13.a, employ_datQ37.13.aGender)
#ordinal(employ_datQ37.13.aGender$CatOutcome, employ_datQ37.13.aGender$Gender, employ_datQ37.13.aGender)
prep <- analysisPrep(employ_datQ37.13.aGender$CatOutcome, employ_datQ37.13.aGender$Gender, employ_datQ37.13.aGender)
analysis <- polr(employ_datQ37.13.aGender$CatOutcome ~ employ_datQ37.13.aGender$Gender, data=employ_datQ37.13.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.13.aSexuality<-multidatClean(Q37.13.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.13.aSexuality$Sexuality<-factor(employ_datQ37.13.aSexuality$Sexuality)
employ_datQ37.13.aSexuality<-ordinaldatClean(employ_datQ37.13.aSexuality$Q37.13.a, employ_datQ37.13.aSexuality)
#ordinal(employ_datQ37.13.aSexuality$CatOutcome, employ_datQ37.13.aSexuality$Sexuality, employ_datQ37.13.aSexuality)
prep <- analysisPrep(employ_datQ37.13.aSexuality$CatOutcome, employ_datQ37.13.aSexuality$Sexuality, employ_datQ37.13.aSexuality)
analysis <- polr(employ_datQ37.13.aSexuality$CatOutcome ~ employ_datQ37.13.aSexuality$Sexuality, data=employ_datQ37.13.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.13.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.14
"Status"
employ_datQ37.14.a<-multidatClean(Q37.14.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.14.a + Academic, data = employ_datQ37.14.a)
detach(dat_long)
employ_datQ37.14.a<-ordinaldatClean(employ_datQ37.14.a$Q37.14.a, employ_datQ37.14.a)
#ordinal(employ_datQ37.14.a$CatOutcome, employ_datQ37.14.a$Academic, employ_datQ37.14.a)
prep <- analysisPrep(employ_datQ37.14.a$CatOutcome, employ_datQ37.14.a$Academic, employ_datQ37.14.a)
analysis <- polr(employ_datQ37.14.a$CatOutcome ~ employ_datQ37.14.a$Academic, data=employ_datQ37.14.a, Hess=TRUE)
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
Question <- "Q37.14.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.14.aCarer<-multidatClean(Q37.14.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aCarer$Carer<- factor(employ_datQ37.14.aCarer$Carer)
employ_datQ37.14.aCarer<-ordinaldatClean(employ_datQ37.14.aCarer$Q37.14.a, employ_datQ37.14.aCarer)
#ordinal(employ_datQ37.14.aCarer$CatOutcome, employ_datQ37.14.aCarer$Carer, employ_datQ37.14.aCarer)
prep <- analysisPrep(employ_datQ37.14.aCarer$CatOutcome, employ_datQ37.14.aCarer$Carer, employ_datQ37.14.aCarer)
analysis <- polr(employ_datQ37.14.aCarer$CatOutcome ~ employ_datQ37.14.aCarer$Carer, data=employ_datQ37.14.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.14.aDisability<-multidatClean(Q37.14.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aDisability$Disability<- factor(employ_datQ37.14.aDisability$Disability)
employ_datQ37.14.aDisability<-ordinaldatClean(employ_datQ37.14.aDisability$Q37.14.a, employ_datQ37.14.aDisability)
conTable <- xtabs(~Q37.14.a + Disability, data = employ_datQ37.14.aDisability)
#ordinal(employ_datQ37.14.aDisability$CatOutcome, employ_datQ37.14.aDisability$Disability, employ_datQ37.14.aDisability)
prep <- analysisPrep(employ_datQ37.14.aDisability$CatOutcome, employ_datQ37.14.aDisability$Disability, employ_datQ37.14.aDisability)
analysis <- polr(employ_datQ37.14.aDisability$CatOutcome ~ employ_datQ37.14.aDisability$Disability, data=employ_datQ37.14.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.14.aEthnicity<-multidatClean(Q37.14.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aEthnicity$Ethnicity<- factor(employ_datQ37.14.aEthnicity$EthnicityCleaned)
employ_datQ37.14.aEthnicity<-ordinaldatClean(employ_datQ37.14.aEthnicity$Q37.14.a, employ_datQ37.14.aEthnicity)
conTable <- xtabs(~Q37.14.a + EthnicityCleaned, data = employ_datQ37.14.aEthnicity)
conTable
#ordinal(employ_datQ37.14.aEthnicity$CatOutcome, employ_datQ37.14.aEthnicity$EthnicityCleaned, employ_datQ37.14.aEthnicity)
prep <- analysisPrep(employ_datQ37.14.aEthnicity$CatOutcome, employ_datQ37.14.aEthnicity$EthnicityCleaned, employ_datQ37.14.aEthnicity)
analysis <- polr(employ_datQ37.14.aEthnicity$CatOutcome ~ employ_datQ37.14.aEthnicity$EthnicityCleaned, data=employ_datQ37.14.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.14.aFirstGen<-multidatClean(Q37.14.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aFirstGen$FirstGen<-factor(employ_datQ37.14.aFirstGen$FirstGen)
employ_datQ37.14.aFirstGen<-ordinaldatClean(employ_datQ37.14.aFirstGen$Q37.14.a, employ_datQ37.14.aFirstGen)
#ordinal(employ_datQ37.14.aFirstGen$CatOutcome, employ_datQ37.14.aFirstGen$FirstGen, employ_datQ37.14.aFirstGen)
prep <- analysisPrep(employ_datQ37.14.aFirstGen$CatOutcome, employ_datQ37.14.aFirstGen$FirstGen, employ_datQ37.14.aFirstGen)
analysis <- polr(employ_datQ37.14.aFirstGen$CatOutcome ~ employ_datQ37.14.aFirstGen$FirstGen, data=employ_datQ37.14.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.14.aGender<-multidatClean(Q37.14.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aGender$Gender<-factor(employ_datQ37.14.aGender$Gender)
employ_datQ37.14.aGender<-ordinaldatClean(employ_datQ37.14.aGender$Q37.14.a, employ_datQ37.14.aGender)
#ordinal(employ_datQ37.14.aGender$CatOutcome, employ_datQ37.14.aGender$Gender, employ_datQ37.14.aGender)
prep <- analysisPrep(employ_datQ37.14.aGender$CatOutcome, employ_datQ37.14.aGender$Gender, employ_datQ37.14.aGender)
analysis <- polr(employ_datQ37.14.aGender$CatOutcome ~ employ_datQ37.14.aGender$Gender, data=employ_datQ37.14.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.14.aSexuality<-multidatClean(Q37.14.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.14.aSexuality$Sexuality<-factor(employ_datQ37.14.aSexuality$Sexuality)
employ_datQ37.14.aSexuality<-ordinaldatClean(employ_datQ37.14.aSexuality$Q37.14.a, employ_datQ37.14.aSexuality)
#ordinal(employ_datQ37.14.aSexuality$CatOutcome, employ_datQ37.14.aSexuality$Sexuality, employ_datQ37.14.aSexuality)
prep <- analysisPrep(employ_datQ37.14.aSexuality$CatOutcome, employ_datQ37.14.aSexuality$Sexuality, employ_datQ37.14.aSexuality)
analysis <- polr(employ_datQ37.14.aSexuality$CatOutcome ~ employ_datQ37.14.aSexuality$Sexuality, data=employ_datQ37.14.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.14.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q37.15
"Status"
employ_datQ37.15.a<-multidatClean(Q37.15.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q37.15.a + Academic, data = employ_datQ37.15.a)
detach(dat_long)
employ_datQ37.15.a<-ordinaldatCleanNegative(employ_datQ37.15.a$Q37.15.a, employ_datQ37.15.a)
#ordinal(employ_datQ37.15.a$CatOutcome, employ_datQ37.15.a$Academic, employ_datQ37.15.a)
prep <- analysisPrep(employ_datQ37.15.a$CatOutcome, employ_datQ37.15.a$Academic, employ_datQ37.15.a)
analysis <- polr(employ_datQ37.15.a$CatOutcome ~ employ_datQ37.15.a$Academic, data=employ_datQ37.15.a, Hess=TRUE)
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
Question <- "Q37.15.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ37.15.aCarer<-multidatClean(Q37.15.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aCarer$Carer<- factor(employ_datQ37.15.aCarer$Carer)
employ_datQ37.15.aCarer<-ordinaldatCleanNegative(employ_datQ37.15.aCarer$Q37.15.a, employ_datQ37.15.aCarer)
#ordinal(employ_datQ37.15.aCarer$CatOutcome, employ_datQ37.15.aCarer$Carer, employ_datQ37.15.aCarer)
prep <- analysisPrep(employ_datQ37.15.aCarer$CatOutcome, employ_datQ37.15.aCarer$Carer, employ_datQ37.15.aCarer)
analysis <- polr(employ_datQ37.15.aCarer$CatOutcome ~ employ_datQ37.15.aCarer$Carer, data=employ_datQ37.15.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ37.15.aDisability<-multidatClean(Q37.15.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aDisability$Disability<- factor(employ_datQ37.15.aDisability$Disability)
employ_datQ37.15.aDisability<-ordinaldatCleanNegative(employ_datQ37.15.aDisability$Q37.15.a, employ_datQ37.15.aDisability)
conTable <- xtabs(~Q37.15.a + Disability, data = employ_datQ37.15.aDisability)
#ordinal(employ_datQ37.15.aDisability$CatOutcome, employ_datQ37.15.aDisability$Disability, employ_datQ37.15.aDisability)
prep <- analysisPrep(employ_datQ37.15.aDisability$CatOutcome, employ_datQ37.15.aDisability$Disability, employ_datQ37.15.aDisability)
analysis <- polr(employ_datQ37.15.aDisability$CatOutcome ~ employ_datQ37.15.aDisability$Disability, data=employ_datQ37.15.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ37.15.aEthnicity<-multidatClean(Q37.15.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aEthnicity$Ethnicity<- factor(employ_datQ37.15.aEthnicity$EthnicityCleaned)
employ_datQ37.15.aEthnicity<-ordinaldatCleanNegative(employ_datQ37.15.aEthnicity$Q37.15.a, employ_datQ37.15.aEthnicity)
conTable <- xtabs(~Q37.15.a + EthnicityCleaned, data = employ_datQ37.15.aEthnicity)
conTable
#ordinal(employ_datQ37.15.aEthnicity$CatOutcome, employ_datQ37.15.aEthnicity$EthnicityCleaned, employ_datQ37.15.aEthnicity)
prep <- analysisPrep(employ_datQ37.15.aEthnicity$CatOutcome, employ_datQ37.15.aEthnicity$EthnicityCleaned, employ_datQ37.15.aEthnicity)
analysis <- polr(employ_datQ37.15.aEthnicity$CatOutcome ~ employ_datQ37.15.aEthnicity$EthnicityCleaned, data=employ_datQ37.15.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ37.15.aFirstGen<-multidatClean(Q37.15.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aFirstGen$FirstGen<-factor(employ_datQ37.15.aFirstGen$FirstGen)
employ_datQ37.15.aFirstGen<-ordinaldatCleanNegative(employ_datQ37.15.aFirstGen$Q37.15.a, employ_datQ37.15.aFirstGen)
#ordinal(employ_datQ37.15.aFirstGen$CatOutcome, employ_datQ37.15.aFirstGen$FirstGen, employ_datQ37.15.aFirstGen)
prep <- analysisPrep(employ_datQ37.15.aFirstGen$CatOutcome, employ_datQ37.15.aFirstGen$FirstGen, employ_datQ37.15.aFirstGen)
analysis <- polr(employ_datQ37.15.aFirstGen$CatOutcome ~ employ_datQ37.15.aFirstGen$FirstGen, data=employ_datQ37.15.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ37.15.aGender<-multidatClean(Q37.15.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aGender$Gender<-factor(employ_datQ37.15.aGender$Gender)
employ_datQ37.15.aGender<-ordinaldatCleanNegative(employ_datQ37.15.aGender$Q37.15.a, employ_datQ37.15.aGender)
#ordinal(employ_datQ37.15.aGender$CatOutcome, employ_datQ37.15.aGender$Gender, employ_datQ37.15.aGender)
prep <- analysisPrep(employ_datQ37.15.aGender$CatOutcome, employ_datQ37.15.aGender$Gender, employ_datQ37.15.aGender)
analysis <- polr(employ_datQ37.15.aGender$CatOutcome ~ employ_datQ37.15.aGender$Gender, data=employ_datQ37.15.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ37.15.aSexuality<-multidatClean(Q37.15.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ37.15.aSexuality$Sexuality<-factor(employ_datQ37.15.aSexuality$Sexuality)
employ_datQ37.15.aSexuality<-ordinaldatCleanNegative(employ_datQ37.15.aSexuality$Q37.15.a, employ_datQ37.15.aSexuality)
#ordinal(employ_datQ37.15.aSexuality$CatOutcome, employ_datQ37.15.aSexuality$Sexuality, employ_datQ37.15.aSexuality)
prep <- analysisPrep(employ_datQ37.15.aSexuality$CatOutcome, employ_datQ37.15.aSexuality$Sexuality, employ_datQ37.15.aSexuality)
analysis <- polr(employ_datQ37.15.aSexuality$CatOutcome ~ employ_datQ37.15.aSexuality$Sexuality, data=employ_datQ37.15.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q37.15.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q38
"Status"
employ_datQ38.1<-multidatClean(Q38.1, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q38.1 + Academic, data = employ_datQ38.1)
detach(dat_long)
employ_datQ38.1<-ordinaldatClean38(employ_datQ38.1$Q38.1, employ_datQ38.1)
#ordinal(employ_datQ38.1$CatOutcome, employ_datQ38.1$Academic, employ_datQ38.1)
prep <- analysisPrep(employ_datQ38.1$CatOutcome, employ_datQ38.1$Academic, employ_datQ38.1)
analysis <- polr(employ_datQ38.1$CatOutcome ~ employ_datQ38.1$Academic, data=employ_datQ38.1, Hess=TRUE)
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
Question <- "Q38.1"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ38.1Carer<-multidatClean(Q38.1, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Carer$Carer<- factor(employ_datQ38.1Carer$Carer)
employ_datQ38.1Carer<-ordinaldatClean38(employ_datQ38.1Carer$Q38.1, employ_datQ38.1Carer)
#ordinal(employ_datQ38.1Carer$CatOutcome, employ_datQ38.1Carer$Carer, employ_datQ38.1Carer)
prep <- analysisPrep(employ_datQ38.1Carer$CatOutcome, employ_datQ38.1Carer$Carer, employ_datQ38.1Carer)
analysis <- polr(employ_datQ38.1Carer$CatOutcome ~ employ_datQ38.1Carer$Carer, data=employ_datQ38.1Carer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ38.1Disability<-multidatClean(Q38.1, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Disability$Disability<- factor(employ_datQ38.1Disability$Disability)
employ_datQ38.1Disability<-ordinaldatClean38(employ_datQ38.1Disability$Q38.1, employ_datQ38.1Disability)
conTable <- xtabs(~Q38.1 + Disability, data = employ_datQ38.1Disability)
#ordinal(employ_datQ38.1Disability$CatOutcome, employ_datQ38.1Disability$Disability, employ_datQ38.1Disability)
prep <- analysisPrep(employ_datQ38.1Disability$CatOutcome, employ_datQ38.1Disability$Disability, employ_datQ38.1Disability)
analysis <- polr(employ_datQ38.1Disability$CatOutcome ~ employ_datQ38.1Disability$Disability, data=employ_datQ38.1Disability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ38.1Ethnicity<-multidatClean(Q38.1, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Ethnicity$Ethnicity<- factor(employ_datQ38.1Ethnicity$EthnicityCleaned)
employ_datQ38.1Ethnicity<-ordinaldatClean38(employ_datQ38.1Ethnicity$Q38.1, employ_datQ38.1Ethnicity)
conTable <- xtabs(~Q38.1 + EthnicityCleaned, data = employ_datQ38.1Ethnicity)
conTable
#ordinal(employ_datQ38.1Ethnicity$CatOutcome, employ_datQ38.1Ethnicity$EthnicityCleaned, employ_datQ38.1Ethnicity)
prep <- analysisPrep(employ_datQ38.1Ethnicity$CatOutcome, employ_datQ38.1Ethnicity$EthnicityCleaned, employ_datQ38.1Ethnicity)
analysis <- polr(employ_datQ38.1Ethnicity$CatOutcome ~ employ_datQ38.1Ethnicity$EthnicityCleaned, data=employ_datQ38.1Ethnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ38.1FirstGen<-multidatClean(Q38.1, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1FirstGen$FirstGen<-factor(employ_datQ38.1FirstGen$FirstGen)
employ_datQ38.1FirstGen<-ordinaldatClean38(employ_datQ38.1FirstGen$Q38.1, employ_datQ38.1FirstGen)
#ordinal(employ_datQ38.1FirstGen$CatOutcome, employ_datQ38.1FirstGen$FirstGen, employ_datQ38.1FirstGen)
prep <- analysisPrep(employ_datQ38.1FirstGen$CatOutcome, employ_datQ38.1FirstGen$FirstGen, employ_datQ38.1FirstGen)
analysis <- polr(employ_datQ38.1FirstGen$CatOutcome ~ employ_datQ38.1FirstGen$FirstGen, data=employ_datQ38.1FirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ38.1Gender<-multidatClean(Q38.1, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Gender$Gender<-factor(employ_datQ38.1Gender$Gender)
employ_datQ38.1Gender<-ordinaldatClean38(employ_datQ38.1Gender$Q38.1, employ_datQ38.1Gender)
#ordinal(employ_datQ38.1Gender$CatOutcome, employ_datQ38.1Gender$Gender, employ_datQ38.1Gender)
prep <- analysisPrep(employ_datQ38.1Gender$CatOutcome, employ_datQ38.1Gender$Gender, employ_datQ38.1Gender)
analysis <- polr(employ_datQ38.1Gender$CatOutcome ~ employ_datQ38.1Gender$Gender, data=employ_datQ38.1Gender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ38.1Sexuality<-multidatClean(Q38.1, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Sexuality$Sexuality<-factor(employ_datQ38.1Sexuality$Sexuality)
employ_datQ38.1Sexuality<-ordinaldatClean38(employ_datQ38.1Sexuality$Q38.1, employ_datQ38.1Sexuality)
#ordinal(employ_datQ38.1Sexuality$CatOutcome, employ_datQ38.1Sexuality$Sexuality, employ_datQ38.1Sexuality)
prep <- analysisPrep(employ_datQ38.1Sexuality$CatOutcome, employ_datQ38.1Sexuality$Sexuality, employ_datQ38.1Sexuality)
analysis <- polr(employ_datQ38.1Sexuality$CatOutcome ~ employ_datQ38.1Sexuality$Sexuality, data=employ_datQ38.1Sexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q38.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q40
"Status"
employ_datQ40.1.a<-multidatClean(Q40.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.1.a + Academic, data = employ_datQ40.1.a)
detach(dat_long)
employ_datQ40.1.a<-ordinaldatCleanNegative(employ_datQ40.1.a$Q40.1.a, employ_datQ40.1.a)
#ordinal(employ_datQ40.1.a$CatOutcome, employ_datQ40.1.a$Academic, employ_datQ40.1.a)
prep <- analysisPrep(employ_datQ40.1.a$CatOutcome, employ_datQ40.1.a$Academic, employ_datQ40.1.a)
analysis <- polr(employ_datQ40.1.a$CatOutcome ~ employ_datQ40.1.a$Academic, data=employ_datQ40.1.a, Hess=TRUE)
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
Question <- "Q40.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ40.1.aCarer<-multidatClean(Q40.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aCarer$Carer<- factor(employ_datQ40.1.aCarer$Carer)
employ_datQ40.1.aCarer<-ordinaldatCleanNegative(employ_datQ40.1.aCarer$Q40.1.a, employ_datQ40.1.aCarer)
#ordinal(employ_datQ40.1.aCarer$CatOutcome, employ_datQ40.1.aCarer$Carer, employ_datQ40.1.aCarer)
prep <- analysisPrep(employ_datQ40.1.aCarer$CatOutcome, employ_datQ40.1.aCarer$Carer, employ_datQ40.1.aCarer)
analysis <- polr(employ_datQ40.1.aCarer$CatOutcome ~ employ_datQ40.1.aCarer$Carer, data=employ_datQ40.1.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ40.1.aDisability<-multidatClean(Q40.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aDisability$Disability<- factor(employ_datQ40.1.aDisability$Disability)
employ_datQ40.1.aDisability<-ordinaldatCleanNegative(employ_datQ40.1.aDisability$Q40.1.a, employ_datQ40.1.aDisability)
conTable <- xtabs(~Q40.1.a + Disability, data = employ_datQ40.1.aDisability)
#ordinal(employ_datQ40.1.aDisability$CatOutcome, employ_datQ40.1.aDisability$Disability, employ_datQ40.1.aDisability)
prep <- analysisPrep(employ_datQ40.1.aDisability$CatOutcome, employ_datQ40.1.aDisability$Disability, employ_datQ40.1.aDisability)
analysis <- polr(employ_datQ40.1.aDisability$CatOutcome ~ employ_datQ40.1.aDisability$Disability, data=employ_datQ40.1.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ40.1.aEthnicity<-multidatClean(Q40.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aEthnicity$Ethnicity<- factor(employ_datQ40.1.aEthnicity$EthnicityCleaned)
employ_datQ40.1.aEthnicity<-ordinaldatCleanNegative(employ_datQ40.1.aEthnicity$Q40.1.a, employ_datQ40.1.aEthnicity)
conTable <- xtabs(~Q40.1.a + EthnicityCleaned, data = employ_datQ40.1.aEthnicity)
conTable
#ordinal(employ_datQ40.1.aEthnicity$CatOutcome, employ_datQ40.1.aEthnicity$EthnicityCleaned, employ_datQ40.1.aEthnicity)
prep <- analysisPrep(employ_datQ40.1.aEthnicity$CatOutcome, employ_datQ40.1.aEthnicity$EthnicityCleaned, employ_datQ40.1.aEthnicity)
analysis <- polr(employ_datQ40.1.aEthnicity$CatOutcome ~ employ_datQ40.1.aEthnicity$EthnicityCleaned, data=employ_datQ40.1.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ40.1.aFirstGen<-multidatClean(Q40.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aFirstGen$FirstGen<-factor(employ_datQ40.1.aFirstGen$FirstGen)
employ_datQ40.1.aFirstGen<-ordinaldatCleanNegative(employ_datQ40.1.aFirstGen$Q40.1.a, employ_datQ40.1.aFirstGen)
#ordinal(employ_datQ40.1.aFirstGen$CatOutcome, employ_datQ40.1.aFirstGen$FirstGen, employ_datQ40.1.aFirstGen)
prep <- analysisPrep(employ_datQ40.1.aFirstGen$CatOutcome, employ_datQ40.1.aFirstGen$FirstGen, employ_datQ40.1.aFirstGen)
analysis <- polr(employ_datQ40.1.aFirstGen$CatOutcome ~ employ_datQ40.1.aFirstGen$FirstGen, data=employ_datQ40.1.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ40.1.aGender<-multidatClean(Q40.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aGender$Gender<-factor(employ_datQ40.1.aGender$Gender)
employ_datQ40.1.aGender<-ordinaldatCleanNegative(employ_datQ40.1.aGender$Q40.1.a, employ_datQ40.1.aGender)
#ordinal(employ_datQ40.1.aGender$CatOutcome, employ_datQ40.1.aGender$Gender, employ_datQ40.1.aGender)
prep <- analysisPrep(employ_datQ40.1.aGender$CatOutcome, employ_datQ40.1.aGender$Gender, employ_datQ40.1.aGender)
analysis <- polr(employ_datQ40.1.aGender$CatOutcome ~ employ_datQ40.1.aGender$Gender, data=employ_datQ40.1.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ40.1.aSexuality<-multidatClean(Q40.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.1.aSexuality$Sexuality<-factor(employ_datQ40.1.aSexuality$Sexuality)
employ_datQ40.1.aSexuality<-ordinaldatCleanNegative(employ_datQ40.1.aSexuality$Q40.1.a, employ_datQ40.1.aSexuality)
#ordinal(employ_datQ40.1.aSexuality$CatOutcome, employ_datQ40.1.aSexuality$Sexuality, employ_datQ40.1.aSexuality)
prep <- analysisPrep(employ_datQ40.1.aSexuality$CatOutcome, employ_datQ40.1.aSexuality$Sexuality, employ_datQ40.1.aSexuality)
analysis <- polr(employ_datQ40.1.aSexuality$CatOutcome ~ employ_datQ40.1.aSexuality$Sexuality, data=employ_datQ40.1.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q40.1_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q40
"Status"
employ_datQ40.2.a<-multidatClean(Q40.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.2.a + Academic, data = employ_datQ40.2.a)
detach(dat_long)
employ_datQ40.2.a<-ordinaldatClean(employ_datQ40.2.a$Q40.2.a, employ_datQ40.2.a)
#ordinal(employ_datQ40.2.a$CatOutcome, employ_datQ40.2.a$Academic, employ_datQ40.2.a)
prep <- analysisPrep(employ_datQ40.2.a$CatOutcome, employ_datQ40.2.a$Academic, employ_datQ40.2.a)
analysis <- polr(employ_datQ40.2.a$CatOutcome ~ employ_datQ40.2.a$Academic, data=employ_datQ40.2.a, Hess=TRUE)
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
Question <- "Q40.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ40.2.aCarer<-multidatClean(Q40.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aCarer$Carer<- factor(employ_datQ40.2.aCarer$Carer)
employ_datQ40.2.aCarer<-ordinaldatClean(employ_datQ40.2.aCarer$Q40.2.a, employ_datQ40.2.aCarer)
#ordinal(employ_datQ40.2.aCarer$CatOutcome, employ_datQ40.2.aCarer$Carer, employ_datQ40.2.aCarer)
prep <- analysisPrep(employ_datQ40.2.aCarer$CatOutcome, employ_datQ40.2.aCarer$Carer, employ_datQ40.2.aCarer)
analysis <- polr(employ_datQ40.2.aCarer$CatOutcome ~ employ_datQ40.2.aCarer$Carer, data=employ_datQ40.2.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ40.2.aDisability<-multidatClean(Q40.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aDisability$Disability<- factor(employ_datQ40.2.aDisability$Disability)
employ_datQ40.2.aDisability<-ordinaldatClean(employ_datQ40.2.aDisability$Q40.2.a, employ_datQ40.2.aDisability)
conTable <- xtabs(~Q40.2.a + Disability, data = employ_datQ40.2.aDisability)
#ordinal(employ_datQ40.2.aDisability$CatOutcome, employ_datQ40.2.aDisability$Disability, employ_datQ40.2.aDisability)
prep <- analysisPrep(employ_datQ40.2.aDisability$CatOutcome, employ_datQ40.2.aDisability$Disability, employ_datQ40.2.aDisability)
analysis <- polr(employ_datQ40.2.aDisability$CatOutcome ~ employ_datQ40.2.aDisability$Disability, data=employ_datQ40.2.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ40.2.aEthnicity<-multidatClean(Q40.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aEthnicity$Ethnicity<- factor(employ_datQ40.2.aEthnicity$EthnicityCleaned)
employ_datQ40.2.aEthnicity<-ordinaldatClean(employ_datQ40.2.aEthnicity$Q40.2.a, employ_datQ40.2.aEthnicity)
conTable <- xtabs(~Q40.2.a + EthnicityCleaned, data = employ_datQ40.2.aEthnicity)
conTable
#ordinal(employ_datQ40.2.aEthnicity$CatOutcome, employ_datQ40.2.aEthnicity$EthnicityCleaned, employ_datQ40.2.aEthnicity)
prep <- analysisPrep(employ_datQ40.2.aEthnicity$CatOutcome, employ_datQ40.2.aEthnicity$EthnicityCleaned, employ_datQ40.2.aEthnicity)
analysis <- polr(employ_datQ40.2.aEthnicity$CatOutcome ~ employ_datQ40.2.aEthnicity$EthnicityCleaned, data=employ_datQ40.2.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ40.2.aFirstGen<-multidatClean(Q40.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aFirstGen$FirstGen<-factor(employ_datQ40.2.aFirstGen$FirstGen)
employ_datQ40.2.aFirstGen<-ordinaldatClean(employ_datQ40.2.aFirstGen$Q40.2.a, employ_datQ40.2.aFirstGen)
#ordinal(employ_datQ40.2.aFirstGen$CatOutcome, employ_datQ40.2.aFirstGen$FirstGen, employ_datQ40.2.aFirstGen)
prep <- analysisPrep(employ_datQ40.2.aFirstGen$CatOutcome, employ_datQ40.2.aFirstGen$FirstGen, employ_datQ40.2.aFirstGen)
analysis <- polr(employ_datQ40.2.aFirstGen$CatOutcome ~ employ_datQ40.2.aFirstGen$FirstGen, data=employ_datQ40.2.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ40.2.aGender<-multidatClean(Q40.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aGender$Gender<-factor(employ_datQ40.2.aGender$Gender)
employ_datQ40.2.aGender<-ordinaldatClean(employ_datQ40.2.aGender$Q40.2.a, employ_datQ40.2.aGender)
#ordinal(employ_datQ40.2.aGender$CatOutcome, employ_datQ40.2.aGender$Gender, employ_datQ40.2.aGender)
prep <- analysisPrep(employ_datQ40.2.aGender$CatOutcome, employ_datQ40.2.aGender$Gender, employ_datQ40.2.aGender)
analysis <- polr(employ_datQ40.2.aGender$CatOutcome ~ employ_datQ40.2.aGender$Gender, data=employ_datQ40.2.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ40.2.aSexuality<-multidatClean(Q40.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.2.aSexuality$Sexuality<-factor(employ_datQ40.2.aSexuality$Sexuality)
employ_datQ40.2.aSexuality<-ordinaldatClean(employ_datQ40.2.aSexuality$Q40.2.a, employ_datQ40.2.aSexuality)
#ordinal(employ_datQ40.2.aSexuality$CatOutcome, employ_datQ40.2.aSexuality$Sexuality, employ_datQ40.2.aSexuality)
prep <- analysisPrep(employ_datQ40.2.aSexuality$CatOutcome, employ_datQ40.2.aSexuality$Sexuality, employ_datQ40.2.aSexuality)
analysis <- polr(employ_datQ40.2.aSexuality$CatOutcome ~ employ_datQ40.2.aSexuality$Sexuality, data=employ_datQ40.2.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q40.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q40
"Status"
employ_datQ40.3.a<-multidatClean(Q40.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.3.a + Academic, data = employ_datQ40.3.a)
detach(dat_long)
employ_datQ40.3.a<-ordinaldatClean(employ_datQ40.3.a$Q40.3.a, employ_datQ40.3.a)
#ordinal(employ_datQ40.3.a$CatOutcome, employ_datQ40.3.a$Academic, employ_datQ40.3.a)
prep <- analysisPrep(employ_datQ40.3.a$CatOutcome, employ_datQ40.3.a$Academic, employ_datQ40.3.a)
analysis <- polr(employ_datQ40.3.a$CatOutcome ~ employ_datQ40.3.a$Academic, data=employ_datQ40.3.a, Hess=TRUE)
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
Question <- "Q40.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ40.3.aCarer<-multidatClean(Q40.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aCarer$Carer<- factor(employ_datQ40.3.aCarer$Carer)
employ_datQ40.3.aCarer<-ordinaldatClean(employ_datQ40.3.aCarer$Q40.3.a, employ_datQ40.3.aCarer)
#ordinal(employ_datQ40.3.aCarer$CatOutcome, employ_datQ40.3.aCarer$Carer, employ_datQ40.3.aCarer)
prep <- analysisPrep(employ_datQ40.3.aCarer$CatOutcome, employ_datQ40.3.aCarer$Carer, employ_datQ40.3.aCarer)
analysis <- polr(employ_datQ40.3.aCarer$CatOutcome ~ employ_datQ40.3.aCarer$Carer, data=employ_datQ40.3.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ40.3.aDisability<-multidatClean(Q40.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aDisability$Disability<- factor(employ_datQ40.3.aDisability$Disability)
employ_datQ40.3.aDisability<-ordinaldatClean(employ_datQ40.3.aDisability$Q40.3.a, employ_datQ40.3.aDisability)
conTable <- xtabs(~Q40.3.a + Disability, data = employ_datQ40.3.aDisability)
#ordinal(employ_datQ40.3.aDisability$CatOutcome, employ_datQ40.3.aDisability$Disability, employ_datQ40.3.aDisability)
prep <- analysisPrep(employ_datQ40.3.aDisability$CatOutcome, employ_datQ40.3.aDisability$Disability, employ_datQ40.3.aDisability)
analysis <- polr(employ_datQ40.3.aDisability$CatOutcome ~ employ_datQ40.3.aDisability$Disability, data=employ_datQ40.3.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ40.3.aEthnicity<-multidatClean(Q40.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aEthnicity$Ethnicity<- factor(employ_datQ40.3.aEthnicity$EthnicityCleaned)
employ_datQ40.3.aEthnicity<-ordinaldatClean(employ_datQ40.3.aEthnicity$Q40.3.a, employ_datQ40.3.aEthnicity)
conTable <- xtabs(~Q40.3.a + EthnicityCleaned, data = employ_datQ40.3.aEthnicity)
conTable
#ordinal(employ_datQ40.3.aEthnicity$CatOutcome, employ_datQ40.3.aEthnicity$EthnicityCleaned, employ_datQ40.3.aEthnicity)
prep <- analysisPrep(employ_datQ40.3.aEthnicity$CatOutcome, employ_datQ40.3.aEthnicity$EthnicityCleaned, employ_datQ40.3.aEthnicity)
analysis <- polr(employ_datQ40.3.aEthnicity$CatOutcome ~ employ_datQ40.3.aEthnicity$EthnicityCleaned, data=employ_datQ40.3.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ40.3.aFirstGen<-multidatClean(Q40.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aFirstGen$FirstGen<-factor(employ_datQ40.3.aFirstGen$FirstGen)
employ_datQ40.3.aFirstGen<-ordinaldatClean(employ_datQ40.3.aFirstGen$Q40.3.a, employ_datQ40.3.aFirstGen)
#ordinal(employ_datQ40.3.aFirstGen$CatOutcome, employ_datQ40.3.aFirstGen$FirstGen, employ_datQ40.3.aFirstGen)
prep <- analysisPrep(employ_datQ40.3.aFirstGen$CatOutcome, employ_datQ40.3.aFirstGen$FirstGen, employ_datQ40.3.aFirstGen)
analysis <- polr(employ_datQ40.3.aFirstGen$CatOutcome ~ employ_datQ40.3.aFirstGen$FirstGen, data=employ_datQ40.3.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ40.3.aGender<-multidatClean(Q40.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aGender$Gender<-factor(employ_datQ40.3.aGender$Gender)
employ_datQ40.3.aGender<-ordinaldatClean(employ_datQ40.3.aGender$Q40.3.a, employ_datQ40.3.aGender)
#ordinal(employ_datQ40.3.aGender$CatOutcome, employ_datQ40.3.aGender$Gender, employ_datQ40.3.aGender)
prep <- analysisPrep(employ_datQ40.3.aGender$CatOutcome, employ_datQ40.3.aGender$Gender, employ_datQ40.3.aGender)
analysis <- polr(employ_datQ40.3.aGender$CatOutcome ~ employ_datQ40.3.aGender$Gender, data=employ_datQ40.3.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ40.3.aSexuality<-multidatClean(Q40.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.3.aSexuality$Sexuality<-factor(employ_datQ40.3.aSexuality$Sexuality)
employ_datQ40.3.aSexuality<-ordinaldatClean(employ_datQ40.3.aSexuality$Q40.3.a, employ_datQ40.3.aSexuality)
#ordinal(employ_datQ40.3.aSexuality$CatOutcome, employ_datQ40.3.aSexuality$Sexuality, employ_datQ40.3.aSexuality)
prep <- analysisPrep(employ_datQ40.3.aSexuality$CatOutcome, employ_datQ40.3.aSexuality$Sexuality, employ_datQ40.3.aSexuality)
analysis <- polr(employ_datQ40.3.aSexuality$CatOutcome ~ employ_datQ40.3.aSexuality$Sexuality, data=employ_datQ40.3.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q40.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q40
"Status"
employ_datQ40.4.a<-multidatClean(Q40.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.4.a + Academic, data = employ_datQ40.4.a)
detach(dat_long)
employ_datQ40.4.a<-ordinaldatCleanNegative(employ_datQ40.4.a$Q40.4.a, employ_datQ40.4.a)
#ordinal(employ_datQ40.4.a$CatOutcome, employ_datQ40.4.a$Academic, employ_datQ40.4.a)
prep <- analysisPrep(employ_datQ40.4.a$CatOutcome, employ_datQ40.4.a$Academic, employ_datQ40.4.a)
analysis <- polr(employ_datQ40.4.a$CatOutcome ~ employ_datQ40.4.a$Academic, data=employ_datQ40.4.a, Hess=TRUE)
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
Question <- "Q40.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ40.4.aCarer<-multidatClean(Q40.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aCarer$Carer<- factor(employ_datQ40.4.aCarer$Carer)
employ_datQ40.4.aCarer<-ordinaldatCleanNegative(employ_datQ40.4.aCarer$Q40.4.a, employ_datQ40.4.aCarer)
#ordinal(employ_datQ40.4.aCarer$CatOutcome, employ_datQ40.4.aCarer$Carer, employ_datQ40.4.aCarer)
prep <- analysisPrep(employ_datQ40.4.aCarer$CatOutcome, employ_datQ40.4.aCarer$Carer, employ_datQ40.4.aCarer)
analysis <- polr(employ_datQ40.4.aCarer$CatOutcome ~ employ_datQ40.4.aCarer$Carer, data=employ_datQ40.4.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ40.4.aDisability<-multidatClean(Q40.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aDisability$Disability<- factor(employ_datQ40.4.aDisability$Disability)
employ_datQ40.4.aDisability<-ordinaldatCleanNegative(employ_datQ40.4.aDisability$Q40.4.a, employ_datQ40.4.aDisability)
conTable <- xtabs(~Q40.4.a + Disability, data = employ_datQ40.4.aDisability)
#ordinal(employ_datQ40.4.aDisability$CatOutcome, employ_datQ40.4.aDisability$Disability, employ_datQ40.4.aDisability)
prep <- analysisPrep(employ_datQ40.4.aDisability$CatOutcome, employ_datQ40.4.aDisability$Disability, employ_datQ40.4.aDisability)
analysis <- polr(employ_datQ40.4.aDisability$CatOutcome ~ employ_datQ40.4.aDisability$Disability, data=employ_datQ40.4.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ40.4.aEthnicity<-multidatClean(Q40.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aEthnicity$Ethnicity<- factor(employ_datQ40.4.aEthnicity$EthnicityCleaned)
employ_datQ40.4.aEthnicity<-ordinaldatCleanNegative(employ_datQ40.4.aEthnicity$Q40.4.a, employ_datQ40.4.aEthnicity)
conTable <- xtabs(~Q40.4.a + EthnicityCleaned, data = employ_datQ40.4.aEthnicity)
conTable
#ordinal(employ_datQ40.4.aEthnicity$CatOutcome, employ_datQ40.4.aEthnicity$EthnicityCleaned, employ_datQ40.4.aEthnicity)
prep <- analysisPrep(employ_datQ40.4.aEthnicity$CatOutcome, employ_datQ40.4.aEthnicity$EthnicityCleaned, employ_datQ40.4.aEthnicity)
analysis <- polr(employ_datQ40.4.aEthnicity$CatOutcome ~ employ_datQ40.4.aEthnicity$EthnicityCleaned, data=employ_datQ40.4.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ40.4.aFirstGen<-multidatClean(Q40.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aFirstGen$FirstGen<-factor(employ_datQ40.4.aFirstGen$FirstGen)
employ_datQ40.4.aFirstGen<-ordinaldatCleanNegative(employ_datQ40.4.aFirstGen$Q40.4.a, employ_datQ40.4.aFirstGen)
#ordinal(employ_datQ40.4.aFirstGen$CatOutcome, employ_datQ40.4.aFirstGen$FirstGen, employ_datQ40.4.aFirstGen)
prep <- analysisPrep(employ_datQ40.4.aFirstGen$CatOutcome, employ_datQ40.4.aFirstGen$FirstGen, employ_datQ40.4.aFirstGen)
analysis <- polr(employ_datQ40.4.aFirstGen$CatOutcome ~ employ_datQ40.4.aFirstGen$FirstGen, data=employ_datQ40.4.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ40.4.aGender<-multidatClean(Q40.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aGender$Gender<-factor(employ_datQ40.4.aGender$Gender)
employ_datQ40.4.aGender<-ordinaldatCleanNegative(employ_datQ40.4.aGender$Q40.4.a, employ_datQ40.4.aGender)
#ordinal(employ_datQ40.4.aGender$CatOutcome, employ_datQ40.4.aGender$Gender, employ_datQ40.4.aGender)
prep <- analysisPrep(employ_datQ40.4.aGender$CatOutcome, employ_datQ40.4.aGender$Gender, employ_datQ40.4.aGender)
analysis <- polr(employ_datQ40.4.aGender$CatOutcome ~ employ_datQ40.4.aGender$Gender, data=employ_datQ40.4.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ40.4.aSexuality<-multidatClean(Q40.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.4.aSexuality$Sexuality<-factor(employ_datQ40.4.aSexuality$Sexuality)
employ_datQ40.4.aSexuality<-ordinaldatCleanNegative(employ_datQ40.4.aSexuality$Q40.4.a, employ_datQ40.4.aSexuality)
#ordinal(employ_datQ40.4.aSexuality$CatOutcome, employ_datQ40.4.aSexuality$Sexuality, employ_datQ40.4.aSexuality)
prep <- analysisPrep(employ_datQ40.4.aSexuality$CatOutcome, employ_datQ40.4.aSexuality$Sexuality, employ_datQ40.4.aSexuality)
analysis <- polr(employ_datQ40.4.aSexuality$CatOutcome ~ employ_datQ40.4.aSexuality$Sexuality, data=employ_datQ40.4.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q40.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q40
"Status"
employ_datQ40.5.a<-multidatClean(Q40.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q40.5.a + Academic, data = employ_datQ40.5.a)
detach(dat_long)
employ_datQ40.5.a<-ordinaldatClean(employ_datQ40.5.a$Q40.5.a, employ_datQ40.5.a)
#ordinal(employ_datQ40.5.a$CatOutcome, employ_datQ40.5.a$Academic, employ_datQ40.5.a)
prep <- analysisPrep(employ_datQ40.5.a$CatOutcome, employ_datQ40.5.a$Academic, employ_datQ40.5.a)
analysis <- polr(employ_datQ40.5.a$CatOutcome ~ employ_datQ40.5.a$Academic, data=employ_datQ40.5.a, Hess=TRUE)
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
Question <- "Q40.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ40.5.aCarer<-multidatClean(Q40.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aCarer$Carer<- factor(employ_datQ40.5.aCarer$Carer)
employ_datQ40.5.aCarer<-ordinaldatClean(employ_datQ40.5.aCarer$Q40.5.a, employ_datQ40.5.aCarer)
#ordinal(employ_datQ40.5.aCarer$CatOutcome, employ_datQ40.5.aCarer$Carer, employ_datQ40.5.aCarer)
prep <- analysisPrep(employ_datQ40.5.aCarer$CatOutcome, employ_datQ40.5.aCarer$Carer, employ_datQ40.5.aCarer)
analysis <- polr(employ_datQ40.5.aCarer$CatOutcome ~ employ_datQ40.5.aCarer$Carer, data=employ_datQ40.5.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ40.5.aDisability<-multidatClean(Q40.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aDisability$Disability<- factor(employ_datQ40.5.aDisability$Disability)
employ_datQ40.5.aDisability<-ordinaldatClean(employ_datQ40.5.aDisability$Q40.5.a, employ_datQ40.5.aDisability)
conTable <- xtabs(~Q40.5.a + Disability, data = employ_datQ40.5.aDisability)
#ordinal(employ_datQ40.5.aDisability$CatOutcome, employ_datQ40.5.aDisability$Disability, employ_datQ40.5.aDisability)
prep <- analysisPrep(employ_datQ40.5.aDisability$CatOutcome, employ_datQ40.5.aDisability$Disability, employ_datQ40.5.aDisability)
analysis <- polr(employ_datQ40.5.aDisability$CatOutcome ~ employ_datQ40.5.aDisability$Disability, data=employ_datQ40.5.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ40.5.aEthnicity<-multidatClean(Q40.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aEthnicity$Ethnicity<- factor(employ_datQ40.5.aEthnicity$EthnicityCleaned)
employ_datQ40.5.aEthnicity<-ordinaldatClean(employ_datQ40.5.aEthnicity$Q40.5.a, employ_datQ40.5.aEthnicity)
conTable <- xtabs(~Q40.5.a + EthnicityCleaned, data = employ_datQ40.5.aEthnicity)
conTable
#ordinal(employ_datQ40.5.aEthnicity$CatOutcome, employ_datQ40.5.aEthnicity$EthnicityCleaned, employ_datQ40.5.aEthnicity)
prep <- analysisPrep(employ_datQ40.5.aEthnicity$CatOutcome, employ_datQ40.5.aEthnicity$EthnicityCleaned, employ_datQ40.5.aEthnicity)
analysis <- polr(employ_datQ40.5.aEthnicity$CatOutcome ~ employ_datQ40.5.aEthnicity$EthnicityCleaned, data=employ_datQ40.5.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ40.5.aFirstGen<-multidatClean(Q40.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aFirstGen$FirstGen<-factor(employ_datQ40.5.aFirstGen$FirstGen)
employ_datQ40.5.aFirstGen<-ordinaldatClean(employ_datQ40.5.aFirstGen$Q40.5.a, employ_datQ40.5.aFirstGen)
#ordinal(employ_datQ40.5.aFirstGen$CatOutcome, employ_datQ40.5.aFirstGen$FirstGen, employ_datQ40.5.aFirstGen)
prep <- analysisPrep(employ_datQ40.5.aFirstGen$CatOutcome, employ_datQ40.5.aFirstGen$FirstGen, employ_datQ40.5.aFirstGen)
analysis <- polr(employ_datQ40.5.aFirstGen$CatOutcome ~ employ_datQ40.5.aFirstGen$FirstGen, data=employ_datQ40.5.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ40.5.aGender<-multidatClean(Q40.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aGender$Gender<-factor(employ_datQ40.5.aGender$Gender)
employ_datQ40.5.aGender<-ordinaldatClean(employ_datQ40.5.aGender$Q40.5.a, employ_datQ40.5.aGender)
#ordinal(employ_datQ40.5.aGender$CatOutcome, employ_datQ40.5.aGender$Gender, employ_datQ40.5.aGender)
prep <- analysisPrep(employ_datQ40.5.aGender$CatOutcome, employ_datQ40.5.aGender$Gender, employ_datQ40.5.aGender)
analysis <- polr(employ_datQ40.5.aGender$CatOutcome ~ employ_datQ40.5.aGender$Gender, data=employ_datQ40.5.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ40.5.aSexuality<-multidatClean(Q40.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ40.5.aSexuality$Sexuality<-factor(employ_datQ40.5.aSexuality$Sexuality)
employ_datQ40.5.aSexuality<-ordinaldatClean(employ_datQ40.5.aSexuality$Q40.5.a, employ_datQ40.5.aSexuality)
#ordinal(employ_datQ40.5.aSexuality$CatOutcome, employ_datQ40.5.aSexuality$Sexuality, employ_datQ40.5.aSexuality)
prep <- analysisPrep(employ_datQ40.5.aSexuality$CatOutcome, employ_datQ40.5.aSexuality$Sexuality, employ_datQ40.5.aSexuality)
analysis <- polr(employ_datQ40.5.aSexuality$CatOutcome ~ employ_datQ40.5.aSexuality$Sexuality, data=employ_datQ40.5.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q40.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.1.a<-multidatClean(Q41.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.1.a + Academic, data = employ_datQ41.1.a)
detach(dat_long)
employ_datQ41.1.a<-ordinaldatClean(employ_datQ41.1.a$Q41.1.a, employ_datQ41.1.a)
#ordinal(employ_datQ41.1.a$CatOutcome, employ_datQ41.1.a$Academic, employ_datQ41.1.a)
prep <- analysisPrep(employ_datQ41.1.a$CatOutcome, employ_datQ41.1.a$Academic, employ_datQ41.1.a)
analysis <- polr(employ_datQ41.1.a$CatOutcome ~ employ_datQ41.1.a$Academic, data=employ_datQ41.1.a, Hess=TRUE)
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
Question <- "Q41.1.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.1.aCarer<-multidatClean(Q41.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aCarer$Carer<- factor(employ_datQ41.1.aCarer$Carer)
employ_datQ41.1.aCarer<-ordinaldatClean(employ_datQ41.1.aCarer$Q41.1.a, employ_datQ41.1.aCarer)
#ordinal(employ_datQ41.1.aCarer$CatOutcome, employ_datQ41.1.aCarer$Carer, employ_datQ41.1.aCarer)
prep <- analysisPrep(employ_datQ41.1.aCarer$CatOutcome, employ_datQ41.1.aCarer$Carer, employ_datQ41.1.aCarer)
analysis <- polr(employ_datQ41.1.aCarer$CatOutcome ~ employ_datQ41.1.aCarer$Carer, data=employ_datQ41.1.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.1.aDisability<-multidatClean(Q41.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aDisability$Disability<- factor(employ_datQ41.1.aDisability$Disability)
employ_datQ41.1.aDisability<-ordinaldatClean(employ_datQ41.1.aDisability$Q41.1.a, employ_datQ41.1.aDisability)
conTable <- xtabs(~Q41.1.a + Disability, data = employ_datQ41.1.aDisability)
#ordinal(employ_datQ41.1.aDisability$CatOutcome, employ_datQ41.1.aDisability$Disability, employ_datQ41.1.aDisability)
prep <- analysisPrep(employ_datQ41.1.aDisability$CatOutcome, employ_datQ41.1.aDisability$Disability, employ_datQ41.1.aDisability)
analysis <- polr(employ_datQ41.1.aDisability$CatOutcome ~ employ_datQ41.1.aDisability$Disability, data=employ_datQ41.1.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.1.aEthnicity<-multidatClean(Q41.1.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aEthnicity$Ethnicity<- factor(employ_datQ41.1.aEthnicity$EthnicityCleaned)
employ_datQ41.1.aEthnicity<-ordinaldatClean(employ_datQ41.1.aEthnicity$Q41.1.a, employ_datQ41.1.aEthnicity)
conTable <- xtabs(~Q41.1.a + EthnicityCleaned, data = employ_datQ41.1.aEthnicity)
conTable
#ordinal(employ_datQ41.1.aEthnicity$CatOutcome, employ_datQ41.1.aEthnicity$EthnicityCleaned, employ_datQ41.1.aEthnicity)
prep <- analysisPrep(employ_datQ41.1.aEthnicity$CatOutcome, employ_datQ41.1.aEthnicity$EthnicityCleaned, employ_datQ41.1.aEthnicity)
analysis <- polr(employ_datQ41.1.aEthnicity$CatOutcome ~ employ_datQ41.1.aEthnicity$EthnicityCleaned, data=employ_datQ41.1.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.1.aFirstGen<-multidatClean(Q41.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aFirstGen$FirstGen<-factor(employ_datQ41.1.aFirstGen$FirstGen)
employ_datQ41.1.aFirstGen<-ordinaldatClean(employ_datQ41.1.aFirstGen$Q41.1.a, employ_datQ41.1.aFirstGen)
#ordinal(employ_datQ41.1.aFirstGen$CatOutcome, employ_datQ41.1.aFirstGen$FirstGen, employ_datQ41.1.aFirstGen)
prep <- analysisPrep(employ_datQ41.1.aFirstGen$CatOutcome, employ_datQ41.1.aFirstGen$FirstGen, employ_datQ41.1.aFirstGen)
analysis <- polr(employ_datQ41.1.aFirstGen$CatOutcome ~ employ_datQ41.1.aFirstGen$FirstGen, data=employ_datQ41.1.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.1.aGender<-multidatClean(Q41.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aGender$Gender<-factor(employ_datQ41.1.aGender$Gender)
employ_datQ41.1.aGender<-ordinaldatClean(employ_datQ41.1.aGender$Q41.1.a, employ_datQ41.1.aGender)
#ordinal(employ_datQ41.1.aGender$CatOutcome, employ_datQ41.1.aGender$Gender, employ_datQ41.1.aGender)
prep <- analysisPrep(employ_datQ41.1.aGender$CatOutcome, employ_datQ41.1.aGender$Gender, employ_datQ41.1.aGender)
analysis <- polr(employ_datQ41.1.aGender$CatOutcome ~ employ_datQ41.1.aGender$Gender, data=employ_datQ41.1.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.1.aSexuality<-multidatClean(Q41.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.1.aSexuality$Sexuality<-factor(employ_datQ41.1.aSexuality$Sexuality)
employ_datQ41.1.aSexuality<-ordinaldatClean(employ_datQ41.1.aSexuality$Q41.1.a, employ_datQ41.1.aSexuality)
#ordinal(employ_datQ41.1.aSexuality$CatOutcome, employ_datQ41.1.aSexuality$Sexuality, employ_datQ41.1.aSexuality)
prep <- analysisPrep(employ_datQ41.1.aSexuality$CatOutcome, employ_datQ41.1.aSexuality$Sexuality, employ_datQ41.1.aSexuality)
analysis <- polr(employ_datQ41.1.aSexuality$CatOutcome ~ employ_datQ41.1.aSexuality$Sexuality, data=employ_datQ41.1.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.1.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.2.a<-multidatClean(Q41.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.2.a + Academic, data = employ_datQ41.2.a)
detach(dat_long)
employ_datQ41.2.a<-ordinaldatClean(employ_datQ41.2.a$Q41.2.a, employ_datQ41.2.a)
#ordinal(employ_datQ41.2.a$CatOutcome, employ_datQ41.2.a$Academic, employ_datQ41.2.a)
prep <- analysisPrep(employ_datQ41.2.a$CatOutcome, employ_datQ41.2.a$Academic, employ_datQ41.2.a)
analysis <- polr(employ_datQ41.2.a$CatOutcome ~ employ_datQ41.2.a$Academic, data=employ_datQ41.2.a, Hess=TRUE)
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
Question <- "Q41.2.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.2.aCarer<-multidatClean(Q41.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aCarer$Carer<- factor(employ_datQ41.2.aCarer$Carer)
employ_datQ41.2.aCarer<-ordinaldatClean(employ_datQ41.2.aCarer$Q41.2.a, employ_datQ41.2.aCarer)
#ordinal(employ_datQ41.2.aCarer$CatOutcome, employ_datQ41.2.aCarer$Carer, employ_datQ41.2.aCarer)
prep <- analysisPrep(employ_datQ41.2.aCarer$CatOutcome, employ_datQ41.2.aCarer$Carer, employ_datQ41.2.aCarer)
analysis <- polr(employ_datQ41.2.aCarer$CatOutcome ~ employ_datQ41.2.aCarer$Carer, data=employ_datQ41.2.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.2.aDisability<-multidatClean(Q41.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aDisability$Disability<- factor(employ_datQ41.2.aDisability$Disability)
employ_datQ41.2.aDisability<-ordinaldatClean(employ_datQ41.2.aDisability$Q41.2.a, employ_datQ41.2.aDisability)
conTable <- xtabs(~Q41.2.a + Disability, data = employ_datQ41.2.aDisability)
#ordinal(employ_datQ41.2.aDisability$CatOutcome, employ_datQ41.2.aDisability$Disability, employ_datQ41.2.aDisability)
prep <- analysisPrep(employ_datQ41.2.aDisability$CatOutcome, employ_datQ41.2.aDisability$Disability, employ_datQ41.2.aDisability)
analysis <- polr(employ_datQ41.2.aDisability$CatOutcome ~ employ_datQ41.2.aDisability$Disability, data=employ_datQ41.2.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.2.aEthnicity<-multidatClean(Q41.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aEthnicity$Ethnicity<- factor(employ_datQ41.2.aEthnicity$EthnicityCleaned)
employ_datQ41.2.aEthnicity<-ordinaldatClean(employ_datQ41.2.aEthnicity$Q41.2.a, employ_datQ41.2.aEthnicity)
conTable <- xtabs(~Q41.2.a + EthnicityCleaned, data = employ_datQ41.2.aEthnicity)
conTable
#ordinal(employ_datQ41.2.aEthnicity$CatOutcome, employ_datQ41.2.aEthnicity$EthnicityCleaned, employ_datQ41.2.aEthnicity)
prep <- analysisPrep(employ_datQ41.2.aEthnicity$CatOutcome, employ_datQ41.2.aEthnicity$EthnicityCleaned, employ_datQ41.2.aEthnicity)
analysis <- polr(employ_datQ41.2.aEthnicity$CatOutcome ~ employ_datQ41.2.aEthnicity$EthnicityCleaned, data=employ_datQ41.2.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.2.aFirstGen<-multidatClean(Q41.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aFirstGen$FirstGen<-factor(employ_datQ41.2.aFirstGen$FirstGen)
employ_datQ41.2.aFirstGen<-ordinaldatClean(employ_datQ41.2.aFirstGen$Q41.2.a, employ_datQ41.2.aFirstGen)
#ordinal(employ_datQ41.2.aFirstGen$CatOutcome, employ_datQ41.2.aFirstGen$FirstGen, employ_datQ41.2.aFirstGen)
prep <- analysisPrep(employ_datQ41.2.aFirstGen$CatOutcome, employ_datQ41.2.aFirstGen$FirstGen, employ_datQ41.2.aFirstGen)
analysis <- polr(employ_datQ41.2.aFirstGen$CatOutcome ~ employ_datQ41.2.aFirstGen$FirstGen, data=employ_datQ41.2.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.2.aGender<-multidatClean(Q41.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aGender$Gender<-factor(employ_datQ41.2.aGender$Gender)
employ_datQ41.2.aGender<-ordinaldatClean(employ_datQ41.2.aGender$Q41.2.a, employ_datQ41.2.aGender)
#ordinal(employ_datQ41.2.aGender$CatOutcome, employ_datQ41.2.aGender$Gender, employ_datQ41.2.aGender)
prep <- analysisPrep(employ_datQ41.2.aGender$CatOutcome, employ_datQ41.2.aGender$Gender, employ_datQ41.2.aGender)
analysis <- polr(employ_datQ41.2.aGender$CatOutcome ~ employ_datQ41.2.aGender$Gender, data=employ_datQ41.2.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.2.aSexuality<-multidatClean(Q41.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.2.aSexuality$Sexuality<-factor(employ_datQ41.2.aSexuality$Sexuality)
employ_datQ41.2.aSexuality<-ordinaldatClean(employ_datQ41.2.aSexuality$Q41.2.a, employ_datQ41.2.aSexuality)
#ordinal(employ_datQ41.2.aSexuality$CatOutcome, employ_datQ41.2.aSexuality$Sexuality, employ_datQ41.2.aSexuality)
prep <- analysisPrep(employ_datQ41.2.aSexuality$CatOutcome, employ_datQ41.2.aSexuality$Sexuality, employ_datQ41.2.aSexuality)
analysis <- polr(employ_datQ41.2.aSexuality$CatOutcome ~ employ_datQ41.2.aSexuality$Sexuality, data=employ_datQ41.2.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.2.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.3.a<-multidatClean(Q41.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.3.a + Academic, data = employ_datQ41.3.a)
detach(dat_long)
employ_datQ41.3.a<-ordinaldatClean(employ_datQ41.3.a$Q41.3.a, employ_datQ41.3.a)
#ordinal(employ_datQ41.3.a$CatOutcome, employ_datQ41.3.a$Academic, employ_datQ41.3.a)
prep <- analysisPrep(employ_datQ41.3.a$CatOutcome, employ_datQ41.3.a$Academic, employ_datQ41.3.a)
analysis <- polr(employ_datQ41.3.a$CatOutcome ~ employ_datQ41.3.a$Academic, data=employ_datQ41.3.a, Hess=TRUE)
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
Question <- "Q41.3.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.3.aCarer<-multidatClean(Q41.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aCarer$Carer<- factor(employ_datQ41.3.aCarer$Carer)
employ_datQ41.3.aCarer<-ordinaldatClean(employ_datQ41.3.aCarer$Q41.3.a, employ_datQ41.3.aCarer)
#ordinal(employ_datQ41.3.aCarer$CatOutcome, employ_datQ41.3.aCarer$Carer, employ_datQ41.3.aCarer)
prep <- analysisPrep(employ_datQ41.3.aCarer$CatOutcome, employ_datQ41.3.aCarer$Carer, employ_datQ41.3.aCarer)
analysis <- polr(employ_datQ41.3.aCarer$CatOutcome ~ employ_datQ41.3.aCarer$Carer, data=employ_datQ41.3.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.3.aDisability<-multidatClean(Q41.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aDisability$Disability<- factor(employ_datQ41.3.aDisability$Disability)
employ_datQ41.3.aDisability<-ordinaldatClean(employ_datQ41.3.aDisability$Q41.3.a, employ_datQ41.3.aDisability)
conTable <- xtabs(~Q41.3.a + Disability, data = employ_datQ41.3.aDisability)
#ordinal(employ_datQ41.3.aDisability$CatOutcome, employ_datQ41.3.aDisability$Disability, employ_datQ41.3.aDisability)
prep <- analysisPrep(employ_datQ41.3.aDisability$CatOutcome, employ_datQ41.3.aDisability$Disability, employ_datQ41.3.aDisability)
analysis <- polr(employ_datQ41.3.aDisability$CatOutcome ~ employ_datQ41.3.aDisability$Disability, data=employ_datQ41.3.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.3.aEthnicity<-multidatClean(Q41.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aEthnicity$Ethnicity<- factor(employ_datQ41.3.aEthnicity$EthnicityCleaned)
employ_datQ41.3.aEthnicity<-ordinaldatClean(employ_datQ41.3.aEthnicity$Q41.3.a, employ_datQ41.3.aEthnicity)
conTable <- xtabs(~Q41.3.a + EthnicityCleaned, data = employ_datQ41.3.aEthnicity)
conTable
#ordinal(employ_datQ41.3.aEthnicity$CatOutcome, employ_datQ41.3.aEthnicity$EthnicityCleaned, employ_datQ41.3.aEthnicity)
prep <- analysisPrep(employ_datQ41.3.aEthnicity$CatOutcome, employ_datQ41.3.aEthnicity$EthnicityCleaned, employ_datQ41.3.aEthnicity)
analysis <- polr(employ_datQ41.3.aEthnicity$CatOutcome ~ employ_datQ41.3.aEthnicity$EthnicityCleaned, data=employ_datQ41.3.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.3.aFirstGen<-multidatClean(Q41.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aFirstGen$FirstGen<-factor(employ_datQ41.3.aFirstGen$FirstGen)
employ_datQ41.3.aFirstGen<-ordinaldatClean(employ_datQ41.3.aFirstGen$Q41.3.a, employ_datQ41.3.aFirstGen)
#ordinal(employ_datQ41.3.aFirstGen$CatOutcome, employ_datQ41.3.aFirstGen$FirstGen, employ_datQ41.3.aFirstGen)
prep <- analysisPrep(employ_datQ41.3.aFirstGen$CatOutcome, employ_datQ41.3.aFirstGen$FirstGen, employ_datQ41.3.aFirstGen)
analysis <- polr(employ_datQ41.3.aFirstGen$CatOutcome ~ employ_datQ41.3.aFirstGen$FirstGen, data=employ_datQ41.3.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.3.aGender<-multidatClean(Q41.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aGender$Gender<-factor(employ_datQ41.3.aGender$Gender)
employ_datQ41.3.aGender<-ordinaldatClean(employ_datQ41.3.aGender$Q41.3.a, employ_datQ41.3.aGender)
#ordinal(employ_datQ41.3.aGender$CatOutcome, employ_datQ41.3.aGender$Gender, employ_datQ41.3.aGender)
prep <- analysisPrep(employ_datQ41.3.aGender$CatOutcome, employ_datQ41.3.aGender$Gender, employ_datQ41.3.aGender)
analysis <- polr(employ_datQ41.3.aGender$CatOutcome ~ employ_datQ41.3.aGender$Gender, data=employ_datQ41.3.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.3.aSexuality<-multidatClean(Q41.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.3.aSexuality$Sexuality<-factor(employ_datQ41.3.aSexuality$Sexuality)
employ_datQ41.3.aSexuality<-ordinaldatClean(employ_datQ41.3.aSexuality$Q41.3.a, employ_datQ41.3.aSexuality)
#ordinal(employ_datQ41.3.aSexuality$CatOutcome, employ_datQ41.3.aSexuality$Sexuality, employ_datQ41.3.aSexuality)
prep <- analysisPrep(employ_datQ41.3.aSexuality$CatOutcome, employ_datQ41.3.aSexuality$Sexuality, employ_datQ41.3.aSexuality)
analysis <- polr(employ_datQ41.3.aSexuality$CatOutcome ~ employ_datQ41.3.aSexuality$Sexuality, data=employ_datQ41.3.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.3.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.4.a<-multidatClean(Q41.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.4.a + Academic, data = employ_datQ41.4.a)
detach(dat_long)
employ_datQ41.4.a<-ordinaldatCleanNegative(employ_datQ41.4.a$Q41.4.a, employ_datQ41.4.a)
#ordinal(employ_datQ41.4.a$CatOutcome, employ_datQ41.4.a$Academic, employ_datQ41.4.a)
prep <- analysisPrep(employ_datQ41.4.a$CatOutcome, employ_datQ41.4.a$Academic, employ_datQ41.4.a)
analysis <- polr(employ_datQ41.4.a$CatOutcome ~ employ_datQ41.4.a$Academic, data=employ_datQ41.4.a, Hess=TRUE)
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
Question <- "Q41.4.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.4.aCarer<-multidatClean(Q41.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aCarer$Carer<- factor(employ_datQ41.4.aCarer$Carer)
employ_datQ41.4.aCarer<-ordinaldatCleanNegative(employ_datQ41.4.aCarer$Q41.4.a, employ_datQ41.4.aCarer)
#ordinal(employ_datQ41.4.aCarer$CatOutcome, employ_datQ41.4.aCarer$Carer, employ_datQ41.4.aCarer)
prep <- analysisPrep(employ_datQ41.4.aCarer$CatOutcome, employ_datQ41.4.aCarer$Carer, employ_datQ41.4.aCarer)
analysis <- polr(employ_datQ41.4.aCarer$CatOutcome ~ employ_datQ41.4.aCarer$Carer, data=employ_datQ41.4.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.4.aDisability<-multidatClean(Q41.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aDisability$Disability<- factor(employ_datQ41.4.aDisability$Disability)
employ_datQ41.4.aDisability<-ordinaldatCleanNegative(employ_datQ41.4.aDisability$Q41.4.a, employ_datQ41.4.aDisability)
conTable <- xtabs(~Q41.4.a + Disability, data = employ_datQ41.4.aDisability)
#ordinal(employ_datQ41.4.aDisability$CatOutcome, employ_datQ41.4.aDisability$Disability, employ_datQ41.4.aDisability)
prep <- analysisPrep(employ_datQ41.4.aDisability$CatOutcome, employ_datQ41.4.aDisability$Disability, employ_datQ41.4.aDisability)
analysis <- polr(employ_datQ41.4.aDisability$CatOutcome ~ employ_datQ41.4.aDisability$Disability, data=employ_datQ41.4.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.4.aEthnicity<-multidatClean(Q41.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aEthnicity$Ethnicity<- factor(employ_datQ41.4.aEthnicity$EthnicityCleaned)
employ_datQ41.4.aEthnicity<-ordinaldatCleanNegative(employ_datQ41.4.aEthnicity$Q41.4.a, employ_datQ41.4.aEthnicity)
conTable <- xtabs(~Q41.4.a + EthnicityCleaned, data = employ_datQ41.4.aEthnicity)
conTable
#ordinal(employ_datQ41.4.aEthnicity$CatOutcome, employ_datQ41.4.aEthnicity$EthnicityCleaned, employ_datQ41.4.aEthnicity)
prep <- analysisPrep(employ_datQ41.4.aEthnicity$CatOutcome, employ_datQ41.4.aEthnicity$EthnicityCleaned, employ_datQ41.4.aEthnicity)
analysis <- polr(employ_datQ41.4.aEthnicity$CatOutcome ~ employ_datQ41.4.aEthnicity$EthnicityCleaned, data=employ_datQ41.4.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.4.aFirstGen<-multidatClean(Q41.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aFirstGen$FirstGen<-factor(employ_datQ41.4.aFirstGen$FirstGen)
employ_datQ41.4.aFirstGen<-ordinaldatCleanNegative(employ_datQ41.4.aFirstGen$Q41.4.a, employ_datQ41.4.aFirstGen)
#ordinal(employ_datQ41.4.aFirstGen$CatOutcome, employ_datQ41.4.aFirstGen$FirstGen, employ_datQ41.4.aFirstGen)
prep <- analysisPrep(employ_datQ41.4.aFirstGen$CatOutcome, employ_datQ41.4.aFirstGen$FirstGen, employ_datQ41.4.aFirstGen)
analysis <- polr(employ_datQ41.4.aFirstGen$CatOutcome ~ employ_datQ41.4.aFirstGen$FirstGen, data=employ_datQ41.4.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.4.aGender<-multidatClean(Q41.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aGender$Gender<-factor(employ_datQ41.4.aGender$Gender)
employ_datQ41.4.aGender<-ordinaldatCleanNegative(employ_datQ41.4.aGender$Q41.4.a, employ_datQ41.4.aGender)
#ordinal(employ_datQ41.4.aGender$CatOutcome, employ_datQ41.4.aGender$Gender, employ_datQ41.4.aGender)
prep <- analysisPrep(employ_datQ41.4.aGender$CatOutcome, employ_datQ41.4.aGender$Gender, employ_datQ41.4.aGender)
analysis <- polr(employ_datQ41.4.aGender$CatOutcome ~ employ_datQ41.4.aGender$Gender, data=employ_datQ41.4.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.4.aSexuality<-multidatClean(Q41.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.4.aSexuality$Sexuality<-factor(employ_datQ41.4.aSexuality$Sexuality)
employ_datQ41.4.aSexuality<-ordinaldatCleanNegative(employ_datQ41.4.aSexuality$Q41.4.a, employ_datQ41.4.aSexuality)
#ordinal(employ_datQ41.4.aSexuality$CatOutcome, employ_datQ41.4.aSexuality$Sexuality, employ_datQ41.4.aSexuality)
prep <- analysisPrep(employ_datQ41.4.aSexuality$CatOutcome, employ_datQ41.4.aSexuality$Sexuality, employ_datQ41.4.aSexuality)
analysis <- polr(employ_datQ41.4.aSexuality$CatOutcome ~ employ_datQ41.4.aSexuality$Sexuality, data=employ_datQ41.4.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.4.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.5.a<-multidatClean(Q41.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.5.a + Academic, data = employ_datQ41.5.a)
detach(dat_long)
employ_datQ41.5.a<-ordinaldatCleanNegative(employ_datQ41.5.a$Q41.5.a, employ_datQ41.5.a)
#ordinal(employ_datQ41.5.a$CatOutcome, employ_datQ41.5.a$Academic, employ_datQ41.5.a)
prep <- analysisPrep(employ_datQ41.5.a$CatOutcome, employ_datQ41.5.a$Academic, employ_datQ41.5.a)
analysis <- polr(employ_datQ41.5.a$CatOutcome ~ employ_datQ41.5.a$Academic, data=employ_datQ41.5.a, Hess=TRUE)
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
Question <- "Q41.5.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.5.aCarer<-multidatClean(Q41.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aCarer$Carer<- factor(employ_datQ41.5.aCarer$Carer)
employ_datQ41.5.aCarer<-ordinaldatCleanNegative(employ_datQ41.5.aCarer$Q41.5.a, employ_datQ41.5.aCarer)
#ordinal(employ_datQ41.5.aCarer$CatOutcome, employ_datQ41.5.aCarer$Carer, employ_datQ41.5.aCarer)
prep <- analysisPrep(employ_datQ41.5.aCarer$CatOutcome, employ_datQ41.5.aCarer$Carer, employ_datQ41.5.aCarer)
analysis <- polr(employ_datQ41.5.aCarer$CatOutcome ~ employ_datQ41.5.aCarer$Carer, data=employ_datQ41.5.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.5.aDisability<-multidatClean(Q41.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aDisability$Disability<- factor(employ_datQ41.5.aDisability$Disability)
employ_datQ41.5.aDisability<-ordinaldatCleanNegative(employ_datQ41.5.aDisability$Q41.5.a, employ_datQ41.5.aDisability)
conTable <- xtabs(~Q41.5.a + Disability, data = employ_datQ41.5.aDisability)
#ordinal(employ_datQ41.5.aDisability$CatOutcome, employ_datQ41.5.aDisability$Disability, employ_datQ41.5.aDisability)
prep <- analysisPrep(employ_datQ41.5.aDisability$CatOutcome, employ_datQ41.5.aDisability$Disability, employ_datQ41.5.aDisability)
analysis <- polr(employ_datQ41.5.aDisability$CatOutcome ~ employ_datQ41.5.aDisability$Disability, data=employ_datQ41.5.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.5.aEthnicity<-multidatClean(Q41.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aEthnicity$Ethnicity<- factor(employ_datQ41.5.aEthnicity$EthnicityCleaned)
employ_datQ41.5.aEthnicity<-ordinaldatCleanNegative(employ_datQ41.5.aEthnicity$Q41.5.a, employ_datQ41.5.aEthnicity)
conTable <- xtabs(~Q41.5.a + EthnicityCleaned, data = employ_datQ41.5.aEthnicity)
conTable
#ordinal(employ_datQ41.5.aEthnicity$CatOutcome, employ_datQ41.5.aEthnicity$EthnicityCleaned, employ_datQ41.5.aEthnicity)
prep <- analysisPrep(employ_datQ41.5.aEthnicity$CatOutcome, employ_datQ41.5.aEthnicity$EthnicityCleaned, employ_datQ41.5.aEthnicity)
analysis <- polr(employ_datQ41.5.aEthnicity$CatOutcome ~ employ_datQ41.5.aEthnicity$EthnicityCleaned, data=employ_datQ41.5.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.5.aFirstGen<-multidatClean(Q41.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aFirstGen$FirstGen<-factor(employ_datQ41.5.aFirstGen$FirstGen)
employ_datQ41.5.aFirstGen<-ordinaldatCleanNegative(employ_datQ41.5.aFirstGen$Q41.5.a, employ_datQ41.5.aFirstGen)
#ordinal(employ_datQ41.5.aFirstGen$CatOutcome, employ_datQ41.5.aFirstGen$FirstGen, employ_datQ41.5.aFirstGen)
prep <- analysisPrep(employ_datQ41.5.aFirstGen$CatOutcome, employ_datQ41.5.aFirstGen$FirstGen, employ_datQ41.5.aFirstGen)
analysis <- polr(employ_datQ41.5.aFirstGen$CatOutcome ~ employ_datQ41.5.aFirstGen$FirstGen, data=employ_datQ41.5.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.5.aGender<-multidatClean(Q41.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aGender$Gender<-factor(employ_datQ41.5.aGender$Gender)
employ_datQ41.5.aGender<-ordinaldatCleanNegative(employ_datQ41.5.aGender$Q41.5.a, employ_datQ41.5.aGender)
#ordinal(employ_datQ41.5.aGender$CatOutcome, employ_datQ41.5.aGender$Gender, employ_datQ41.5.aGender)
prep <- analysisPrep(employ_datQ41.5.aGender$CatOutcome, employ_datQ41.5.aGender$Gender, employ_datQ41.5.aGender)
analysis <- polr(employ_datQ41.5.aGender$CatOutcome ~ employ_datQ41.5.aGender$Gender, data=employ_datQ41.5.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.5.aSexuality<-multidatClean(Q41.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.5.aSexuality$Sexuality<-factor(employ_datQ41.5.aSexuality$Sexuality)
employ_datQ41.5.aSexuality<-ordinaldatCleanNegative(employ_datQ41.5.aSexuality$Q41.5.a, employ_datQ41.5.aSexuality)
#ordinal(employ_datQ41.5.aSexuality$CatOutcome, employ_datQ41.5.aSexuality$Sexuality, employ_datQ41.5.aSexuality)
prep <- analysisPrep(employ_datQ41.5.aSexuality$CatOutcome, employ_datQ41.5.aSexuality$Sexuality, employ_datQ41.5.aSexuality)
analysis <- polr(employ_datQ41.5.aSexuality$CatOutcome ~ employ_datQ41.5.aSexuality$Sexuality, data=employ_datQ41.5.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.5.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.6.a<-multidatClean(Q41.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.6.a + Academic, data = employ_datQ41.6.a)
detach(dat_long)
employ_datQ41.6.a<-ordinaldatCleanNegative(employ_datQ41.6.a$Q41.6.a, employ_datQ41.6.a)
#ordinal(employ_datQ41.6.a$CatOutcome, employ_datQ41.6.a$Academic, employ_datQ41.6.a)
prep <- analysisPrep(employ_datQ41.6.a$CatOutcome, employ_datQ41.6.a$Academic, employ_datQ41.6.a)
analysis <- polr(employ_datQ41.6.a$CatOutcome ~ employ_datQ41.6.a$Academic, data=employ_datQ41.6.a, Hess=TRUE)
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
Question <- "Q41.6.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.6.aCarer<-multidatClean(Q41.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aCarer$Carer<- factor(employ_datQ41.6.aCarer$Carer)
employ_datQ41.6.aCarer<-ordinaldatCleanNegative(employ_datQ41.6.aCarer$Q41.6.a, employ_datQ41.6.aCarer)
#ordinal(employ_datQ41.6.aCarer$CatOutcome, employ_datQ41.6.aCarer$Carer, employ_datQ41.6.aCarer)
prep <- analysisPrep(employ_datQ41.6.aCarer$CatOutcome, employ_datQ41.6.aCarer$Carer, employ_datQ41.6.aCarer)
analysis <- polr(employ_datQ41.6.aCarer$CatOutcome ~ employ_datQ41.6.aCarer$Carer, data=employ_datQ41.6.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.6.aDisability<-multidatClean(Q41.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aDisability$Disability<- factor(employ_datQ41.6.aDisability$Disability)
employ_datQ41.6.aDisability<-ordinaldatCleanNegative(employ_datQ41.6.aDisability$Q41.6.a, employ_datQ41.6.aDisability)
conTable <- xtabs(~Q41.6.a + Disability, data = employ_datQ41.6.aDisability)
#ordinal(employ_datQ41.6.aDisability$CatOutcome, employ_datQ41.6.aDisability$Disability, employ_datQ41.6.aDisability)
prep <- analysisPrep(employ_datQ41.6.aDisability$CatOutcome, employ_datQ41.6.aDisability$Disability, employ_datQ41.6.aDisability)
analysis <- polr(employ_datQ41.6.aDisability$CatOutcome ~ employ_datQ41.6.aDisability$Disability, data=employ_datQ41.6.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.6.aEthnicity<-multidatClean(Q41.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aEthnicity$Ethnicity<- factor(employ_datQ41.6.aEthnicity$EthnicityCleaned)
employ_datQ41.6.aEthnicity<-ordinaldatCleanNegative(employ_datQ41.6.aEthnicity$Q41.6.a, employ_datQ41.6.aEthnicity)
conTable <- xtabs(~Q41.6.a + EthnicityCleaned, data = employ_datQ41.6.aEthnicity)
conTable
#ordinal(employ_datQ41.6.aEthnicity$CatOutcome, employ_datQ41.6.aEthnicity$EthnicityCleaned, employ_datQ41.6.aEthnicity)
prep <- analysisPrep(employ_datQ41.6.aEthnicity$CatOutcome, employ_datQ41.6.aEthnicity$EthnicityCleaned, employ_datQ41.6.aEthnicity)
analysis <- polr(employ_datQ41.6.aEthnicity$CatOutcome ~ employ_datQ41.6.aEthnicity$EthnicityCleaned, data=employ_datQ41.6.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.6.aFirstGen<-multidatClean(Q41.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aFirstGen$FirstGen<-factor(employ_datQ41.6.aFirstGen$FirstGen)
employ_datQ41.6.aFirstGen<-ordinaldatCleanNegative(employ_datQ41.6.aFirstGen$Q41.6.a, employ_datQ41.6.aFirstGen)
#ordinal(employ_datQ41.6.aFirstGen$CatOutcome, employ_datQ41.6.aFirstGen$FirstGen, employ_datQ41.6.aFirstGen)
prep <- analysisPrep(employ_datQ41.6.aFirstGen$CatOutcome, employ_datQ41.6.aFirstGen$FirstGen, employ_datQ41.6.aFirstGen)
analysis <- polr(employ_datQ41.6.aFirstGen$CatOutcome ~ employ_datQ41.6.aFirstGen$FirstGen, data=employ_datQ41.6.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.6.aGender<-multidatClean(Q41.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aGender$Gender<-factor(employ_datQ41.6.aGender$Gender)
employ_datQ41.6.aGender<-ordinaldatCleanNegative(employ_datQ41.6.aGender$Q41.6.a, employ_datQ41.6.aGender)
#ordinal(employ_datQ41.6.aGender$CatOutcome, employ_datQ41.6.aGender$Gender, employ_datQ41.6.aGender)
prep <- analysisPrep(employ_datQ41.6.aGender$CatOutcome, employ_datQ41.6.aGender$Gender, employ_datQ41.6.aGender)
analysis <- polr(employ_datQ41.6.aGender$CatOutcome ~ employ_datQ41.6.aGender$Gender, data=employ_datQ41.6.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.6.aSexuality<-multidatClean(Q41.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.6.aSexuality$Sexuality<-factor(employ_datQ41.6.aSexuality$Sexuality)
employ_datQ41.6.aSexuality<-ordinaldatCleanNegative(employ_datQ41.6.aSexuality$Q41.6.a, employ_datQ41.6.aSexuality)
#ordinal(employ_datQ41.6.aSexuality$CatOutcome, employ_datQ41.6.aSexuality$Sexuality, employ_datQ41.6.aSexuality)
prep <- analysisPrep(employ_datQ41.6.aSexuality$CatOutcome, employ_datQ41.6.aSexuality$Sexuality, employ_datQ41.6.aSexuality)
analysis <- polr(employ_datQ41.6.aSexuality$CatOutcome ~ employ_datQ41.6.aSexuality$Sexuality, data=employ_datQ41.6.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.6.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.7.a<-multidatClean(Q41.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.7.a + Academic, data = employ_datQ41.7.a)
detach(dat_long)
employ_datQ41.7.a<-ordinaldatClean(employ_datQ41.7.a$Q41.7.a, employ_datQ41.7.a)
#ordinal(employ_datQ41.7.a$CatOutcome, employ_datQ41.7.a$Academic, employ_datQ41.7.a)
prep <- analysisPrep(employ_datQ41.7.a$CatOutcome, employ_datQ41.7.a$Academic, employ_datQ41.7.a)
analysis <- polr(employ_datQ41.7.a$CatOutcome ~ employ_datQ41.7.a$Academic, data=employ_datQ41.7.a, Hess=TRUE)
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
Question <- "Q41.7.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.7.aCarer<-multidatClean(Q41.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aCarer$Carer<- factor(employ_datQ41.7.aCarer$Carer)
employ_datQ41.7.aCarer<-ordinaldatClean(employ_datQ41.7.aCarer$Q41.7.a, employ_datQ41.7.aCarer)
#ordinal(employ_datQ41.7.aCarer$CatOutcome, employ_datQ41.7.aCarer$Carer, employ_datQ41.7.aCarer)
prep <- analysisPrep(employ_datQ41.7.aCarer$CatOutcome, employ_datQ41.7.aCarer$Carer, employ_datQ41.7.aCarer)
analysis <- polr(employ_datQ41.7.aCarer$CatOutcome ~ employ_datQ41.7.aCarer$Carer, data=employ_datQ41.7.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.7.aDisability<-multidatClean(Q41.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aDisability$Disability<- factor(employ_datQ41.7.aDisability$Disability)
employ_datQ41.7.aDisability<-ordinaldatClean(employ_datQ41.7.aDisability$Q41.7.a, employ_datQ41.7.aDisability)
conTable <- xtabs(~Q41.7.a + Disability, data = employ_datQ41.7.aDisability)
#ordinal(employ_datQ41.7.aDisability$CatOutcome, employ_datQ41.7.aDisability$Disability, employ_datQ41.7.aDisability)
prep <- analysisPrep(employ_datQ41.7.aDisability$CatOutcome, employ_datQ41.7.aDisability$Disability, employ_datQ41.7.aDisability)
analysis <- polr(employ_datQ41.7.aDisability$CatOutcome ~ employ_datQ41.7.aDisability$Disability, data=employ_datQ41.7.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.7.aEthnicity<-multidatClean(Q41.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aEthnicity$Ethnicity<- factor(employ_datQ41.7.aEthnicity$EthnicityCleaned)
employ_datQ41.7.aEthnicity<-ordinaldatClean(employ_datQ41.7.aEthnicity$Q41.7.a, employ_datQ41.7.aEthnicity)
conTable <- xtabs(~Q41.7.a + EthnicityCleaned, data = employ_datQ41.7.aEthnicity)
conTable
#ordinal(employ_datQ41.7.aEthnicity$CatOutcome, employ_datQ41.7.aEthnicity$EthnicityCleaned, employ_datQ41.7.aEthnicity)
prep <- analysisPrep(employ_datQ41.7.aEthnicity$CatOutcome, employ_datQ41.7.aEthnicity$EthnicityCleaned, employ_datQ41.7.aEthnicity)
analysis <- polr(employ_datQ41.7.aEthnicity$CatOutcome ~ employ_datQ41.7.aEthnicity$EthnicityCleaned, data=employ_datQ41.7.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.7.aFirstGen<-multidatClean(Q41.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aFirstGen$FirstGen<-factor(employ_datQ41.7.aFirstGen$FirstGen)
employ_datQ41.7.aFirstGen<-ordinaldatClean(employ_datQ41.7.aFirstGen$Q41.7.a, employ_datQ41.7.aFirstGen)
#ordinal(employ_datQ41.7.aFirstGen$CatOutcome, employ_datQ41.7.aFirstGen$FirstGen, employ_datQ41.7.aFirstGen)
prep <- analysisPrep(employ_datQ41.7.aFirstGen$CatOutcome, employ_datQ41.7.aFirstGen$FirstGen, employ_datQ41.7.aFirstGen)
analysis <- polr(employ_datQ41.7.aFirstGen$CatOutcome ~ employ_datQ41.7.aFirstGen$FirstGen, data=employ_datQ41.7.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.7.aGender<-multidatClean(Q41.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aGender$Gender<-factor(employ_datQ41.7.aGender$Gender)
employ_datQ41.7.aGender<-ordinaldatClean(employ_datQ41.7.aGender$Q41.7.a, employ_datQ41.7.aGender)
#ordinal(employ_datQ41.7.aGender$CatOutcome, employ_datQ41.7.aGender$Gender, employ_datQ41.7.aGender)
prep <- analysisPrep(employ_datQ41.7.aGender$CatOutcome, employ_datQ41.7.aGender$Gender, employ_datQ41.7.aGender)
analysis <- polr(employ_datQ41.7.aGender$CatOutcome ~ employ_datQ41.7.aGender$Gender, data=employ_datQ41.7.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.7.aSexuality<-multidatClean(Q41.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.7.aSexuality$Sexuality<-factor(employ_datQ41.7.aSexuality$Sexuality)
employ_datQ41.7.aSexuality<-ordinaldatClean(employ_datQ41.7.aSexuality$Q41.7.a, employ_datQ41.7.aSexuality)
#ordinal(employ_datQ41.7.aSexuality$CatOutcome, employ_datQ41.7.aSexuality$Sexuality, employ_datQ41.7.aSexuality)
prep <- analysisPrep(employ_datQ41.7.aSexuality$CatOutcome, employ_datQ41.7.aSexuality$Sexuality, employ_datQ41.7.aSexuality)
analysis <- polr(employ_datQ41.7.aSexuality$CatOutcome ~ employ_datQ41.7.aSexuality$Sexuality, data=employ_datQ41.7.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.7.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.8.a<-multidatClean(Q41.8.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.8.a + Academic, data = employ_datQ41.8.a)
detach(dat_long)
employ_datQ41.8.a<-ordinaldatClean(employ_datQ41.8.a$Q41.8.a, employ_datQ41.8.a)
#ordinal(employ_datQ41.8.a$CatOutcome, employ_datQ41.8.a$Academic, employ_datQ41.8.a)
prep <- analysisPrep(employ_datQ41.8.a$CatOutcome, employ_datQ41.8.a$Academic, employ_datQ41.8.a)
analysis <- polr(employ_datQ41.8.a$CatOutcome ~ employ_datQ41.8.a$Academic, data=employ_datQ41.8.a, Hess=TRUE)
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
Question <- "Q41.8.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.8.aCarer<-multidatClean(Q41.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aCarer$Carer<- factor(employ_datQ41.8.aCarer$Carer)
employ_datQ41.8.aCarer<-ordinaldatClean(employ_datQ41.8.aCarer$Q41.8.a, employ_datQ41.8.aCarer)
#ordinal(employ_datQ41.8.aCarer$CatOutcome, employ_datQ41.8.aCarer$Carer, employ_datQ41.8.aCarer)
prep <- analysisPrep(employ_datQ41.8.aCarer$CatOutcome, employ_datQ41.8.aCarer$Carer, employ_datQ41.8.aCarer)
analysis <- polr(employ_datQ41.8.aCarer$CatOutcome ~ employ_datQ41.8.aCarer$Carer, data=employ_datQ41.8.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.8.aDisability<-multidatClean(Q41.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aDisability$Disability<- factor(employ_datQ41.8.aDisability$Disability)
employ_datQ41.8.aDisability<-ordinaldatClean(employ_datQ41.8.aDisability$Q41.8.a, employ_datQ41.8.aDisability)
conTable <- xtabs(~Q41.8.a + Disability, data = employ_datQ41.8.aDisability)
#ordinal(employ_datQ41.8.aDisability$CatOutcome, employ_datQ41.8.aDisability$Disability, employ_datQ41.8.aDisability)
prep <- analysisPrep(employ_datQ41.8.aDisability$CatOutcome, employ_datQ41.8.aDisability$Disability, employ_datQ41.8.aDisability)
analysis <- polr(employ_datQ41.8.aDisability$CatOutcome ~ employ_datQ41.8.aDisability$Disability, data=employ_datQ41.8.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.8.aEthnicity<-multidatClean(Q41.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aEthnicity$Ethnicity<- factor(employ_datQ41.8.aEthnicity$EthnicityCleaned)
employ_datQ41.8.aEthnicity<-ordinaldatClean(employ_datQ41.8.aEthnicity$Q41.8.a, employ_datQ41.8.aEthnicity)
conTable <- xtabs(~Q41.8.a + EthnicityCleaned, data = employ_datQ41.8.aEthnicity)
conTable
#ordinal(employ_datQ41.8.aEthnicity$CatOutcome, employ_datQ41.8.aEthnicity$EthnicityCleaned, employ_datQ41.8.aEthnicity)
prep <- analysisPrep(employ_datQ41.8.aEthnicity$CatOutcome, employ_datQ41.8.aEthnicity$EthnicityCleaned, employ_datQ41.8.aEthnicity)
analysis <- polr(employ_datQ41.8.aEthnicity$CatOutcome ~ employ_datQ41.8.aEthnicity$EthnicityCleaned, data=employ_datQ41.8.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.8.aFirstGen<-multidatClean(Q41.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aFirstGen$FirstGen<-factor(employ_datQ41.8.aFirstGen$FirstGen)
employ_datQ41.8.aFirstGen<-ordinaldatClean(employ_datQ41.8.aFirstGen$Q41.8.a, employ_datQ41.8.aFirstGen)
#ordinal(employ_datQ41.8.aFirstGen$CatOutcome, employ_datQ41.8.aFirstGen$FirstGen, employ_datQ41.8.aFirstGen)
prep <- analysisPrep(employ_datQ41.8.aFirstGen$CatOutcome, employ_datQ41.8.aFirstGen$FirstGen, employ_datQ41.8.aFirstGen)
analysis <- polr(employ_datQ41.8.aFirstGen$CatOutcome ~ employ_datQ41.8.aFirstGen$FirstGen, data=employ_datQ41.8.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.8.aGender<-multidatClean(Q41.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aGender$Gender<-factor(employ_datQ41.8.aGender$Gender)
employ_datQ41.8.aGender<-ordinaldatClean(employ_datQ41.8.aGender$Q41.8.a, employ_datQ41.8.aGender)
#ordinal(employ_datQ41.8.aGender$CatOutcome, employ_datQ41.8.aGender$Gender, employ_datQ41.8.aGender)
prep <- analysisPrep(employ_datQ41.8.aGender$CatOutcome, employ_datQ41.8.aGender$Gender, employ_datQ41.8.aGender)
analysis <- polr(employ_datQ41.8.aGender$CatOutcome ~ employ_datQ41.8.aGender$Gender, data=employ_datQ41.8.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.8.aSexuality<-multidatClean(Q41.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.8.aSexuality$Sexuality<-factor(employ_datQ41.8.aSexuality$Sexuality)
employ_datQ41.8.aSexuality<-ordinaldatClean(employ_datQ41.8.aSexuality$Q41.8.a, employ_datQ41.8.aSexuality)
#ordinal(employ_datQ41.8.aSexuality$CatOutcome, employ_datQ41.8.aSexuality$Sexuality, employ_datQ41.8.aSexuality)
prep <- analysisPrep(employ_datQ41.8.aSexuality$CatOutcome, employ_datQ41.8.aSexuality$Sexuality, employ_datQ41.8.aSexuality)
analysis <- polr(employ_datQ41.8.aSexuality$CatOutcome ~ employ_datQ41.8.aSexuality$Sexuality, data=employ_datQ41.8.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.8.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.9.a<-multidatClean(Q41.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.9.a + Academic, data = employ_datQ41.9.a)
detach(dat_long)
employ_datQ41.9.a<-ordinaldatClean(employ_datQ41.9.a$Q41.9.a, employ_datQ41.9.a)
#ordinal(employ_datQ41.9.a$CatOutcome, employ_datQ41.9.a$Academic, employ_datQ41.9.a)
prep <- analysisPrep(employ_datQ41.9.a$CatOutcome, employ_datQ41.9.a$Academic, employ_datQ41.9.a)
analysis <- polr(employ_datQ41.9.a$CatOutcome ~ employ_datQ41.9.a$Academic, data=employ_datQ41.9.a, Hess=TRUE)
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
Question <- "Q41.9.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.9.aCarer<-multidatClean(Q41.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aCarer$Carer<- factor(employ_datQ41.9.aCarer$Carer)
employ_datQ41.9.aCarer<-ordinaldatClean(employ_datQ41.9.aCarer$Q41.9.a, employ_datQ41.9.aCarer)
#ordinal(employ_datQ41.9.aCarer$CatOutcome, employ_datQ41.9.aCarer$Carer, employ_datQ41.9.aCarer)
prep <- analysisPrep(employ_datQ41.9.aCarer$CatOutcome, employ_datQ41.9.aCarer$Carer, employ_datQ41.9.aCarer)
analysis <- polr(employ_datQ41.9.aCarer$CatOutcome ~ employ_datQ41.9.aCarer$Carer, data=employ_datQ41.9.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.9.aDisability<-multidatClean(Q41.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aDisability$Disability<- factor(employ_datQ41.9.aDisability$Disability)
employ_datQ41.9.aDisability<-ordinaldatClean(employ_datQ41.9.aDisability$Q41.9.a, employ_datQ41.9.aDisability)
conTable <- xtabs(~Q41.9.a + Disability, data = employ_datQ41.9.aDisability)
#ordinal(employ_datQ41.9.aDisability$CatOutcome, employ_datQ41.9.aDisability$Disability, employ_datQ41.9.aDisability)
prep <- analysisPrep(employ_datQ41.9.aDisability$CatOutcome, employ_datQ41.9.aDisability$Disability, employ_datQ41.9.aDisability)
analysis <- polr(employ_datQ41.9.aDisability$CatOutcome ~ employ_datQ41.9.aDisability$Disability, data=employ_datQ41.9.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.9.aEthnicity<-multidatClean(Q41.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aEthnicity$Ethnicity<- factor(employ_datQ41.9.aEthnicity$EthnicityCleaned)
employ_datQ41.9.aEthnicity<-ordinaldatClean(employ_datQ41.9.aEthnicity$Q41.9.a, employ_datQ41.9.aEthnicity)
conTable <- xtabs(~Q41.9.a + EthnicityCleaned, data = employ_datQ41.9.aEthnicity)
conTable
#ordinal(employ_datQ41.9.aEthnicity$CatOutcome, employ_datQ41.9.aEthnicity$EthnicityCleaned, employ_datQ41.9.aEthnicity)
prep <- analysisPrep(employ_datQ41.9.aEthnicity$CatOutcome, employ_datQ41.9.aEthnicity$EthnicityCleaned, employ_datQ41.9.aEthnicity)
analysis <- polr(employ_datQ41.9.aEthnicity$CatOutcome ~ employ_datQ41.9.aEthnicity$EthnicityCleaned, data=employ_datQ41.9.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.9.aFirstGen<-multidatClean(Q41.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aFirstGen$FirstGen<-factor(employ_datQ41.9.aFirstGen$FirstGen)
employ_datQ41.9.aFirstGen<-ordinaldatClean(employ_datQ41.9.aFirstGen$Q41.9.a, employ_datQ41.9.aFirstGen)
#ordinal(employ_datQ41.9.aFirstGen$CatOutcome, employ_datQ41.9.aFirstGen$FirstGen, employ_datQ41.9.aFirstGen)
prep <- analysisPrep(employ_datQ41.9.aFirstGen$CatOutcome, employ_datQ41.9.aFirstGen$FirstGen, employ_datQ41.9.aFirstGen)
analysis <- polr(employ_datQ41.9.aFirstGen$CatOutcome ~ employ_datQ41.9.aFirstGen$FirstGen, data=employ_datQ41.9.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.9.aGender<-multidatClean(Q41.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aGender$Gender<-factor(employ_datQ41.9.aGender$Gender)
employ_datQ41.9.aGender<-ordinaldatClean(employ_datQ41.9.aGender$Q41.9.a, employ_datQ41.9.aGender)
#ordinal(employ_datQ41.9.aGender$CatOutcome, employ_datQ41.9.aGender$Gender, employ_datQ41.9.aGender)
prep <- analysisPrep(employ_datQ41.9.aGender$CatOutcome, employ_datQ41.9.aGender$Gender, employ_datQ41.9.aGender)
analysis <- polr(employ_datQ41.9.aGender$CatOutcome ~ employ_datQ41.9.aGender$Gender, data=employ_datQ41.9.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.9.aSexuality<-multidatClean(Q41.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.9.aSexuality$Sexuality<-factor(employ_datQ41.9.aSexuality$Sexuality)
employ_datQ41.9.aSexuality<-ordinaldatClean(employ_datQ41.9.aSexuality$Q41.9.a, employ_datQ41.9.aSexuality)
#ordinal(employ_datQ41.9.aSexuality$CatOutcome, employ_datQ41.9.aSexuality$Sexuality, employ_datQ41.9.aSexuality)
prep <- analysisPrep(employ_datQ41.9.aSexuality$CatOutcome, employ_datQ41.9.aSexuality$Sexuality, employ_datQ41.9.aSexuality)
analysis <- polr(employ_datQ41.9.aSexuality$CatOutcome ~ employ_datQ41.9.aSexuality$Sexuality, data=employ_datQ41.9.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.9.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)

### Q41
"Status"
employ_datQ41.10.a<-multidatClean(Q41.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q41.10.a + Academic, data = employ_datQ41.10.a)
detach(dat_long)
employ_datQ41.10.a<-ordinaldatCleanNegative(employ_datQ41.10.a$Q41.10.a, employ_datQ41.10.a)
#ordinal(employ_datQ41.10.a$CatOutcome, employ_datQ41.10.a$Academic, employ_datQ41.10.a)
prep <- analysisPrep(employ_datQ41.10.a$CatOutcome, employ_datQ41.10.a$Academic, employ_datQ41.10.a)
analysis <- polr(employ_datQ41.10.a$CatOutcome ~ employ_datQ41.10.a$Academic, data=employ_datQ41.10.a, Hess=TRUE)
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
Question <- "Q41.10.a"
Dimension <- "Status"
OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
"College"
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

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
"Caring Responsibilities"
employ_datQ41.10.aCarer<-multidatClean(Q41.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aCarer$Carer<- factor(employ_datQ41.10.aCarer$Carer)
employ_datQ41.10.aCarer<-ordinaldatCleanNegative(employ_datQ41.10.aCarer$Q41.10.a, employ_datQ41.10.aCarer)
#ordinal(employ_datQ41.10.aCarer$CatOutcome, employ_datQ41.10.aCarer$Carer, employ_datQ41.10.aCarer)
prep <- analysisPrep(employ_datQ41.10.aCarer$CatOutcome, employ_datQ41.10.aCarer$Carer, employ_datQ41.10.aCarer)
analysis <- polr(employ_datQ41.10.aCarer$CatOutcome ~ employ_datQ41.10.aCarer$Carer, data=employ_datQ41.10.aCarer, Hess=TRUE)
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
Dimension <- "Carer"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
"Disability"
employ_datQ41.10.aDisability<-multidatClean(Q41.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aDisability$Disability<- factor(employ_datQ41.10.aDisability$Disability)
employ_datQ41.10.aDisability<-ordinaldatCleanNegative(employ_datQ41.10.aDisability$Q41.10.a, employ_datQ41.10.aDisability)
conTable <- xtabs(~Q41.10.a + Disability, data = employ_datQ41.10.aDisability)
#ordinal(employ_datQ41.10.aDisability$CatOutcome, employ_datQ41.10.aDisability$Disability, employ_datQ41.10.aDisability)
prep <- analysisPrep(employ_datQ41.10.aDisability$CatOutcome, employ_datQ41.10.aDisability$Disability, employ_datQ41.10.aDisability)
analysis <- polr(employ_datQ41.10.aDisability$CatOutcome ~ employ_datQ41.10.aDisability$Disability, data=employ_datQ41.10.aDisability, Hess=TRUE)
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
Dimension <- "Disability"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
"Ethnicity"
employ_datQ41.10.aEthnicity<-multidatClean(Q41.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aEthnicity$Ethnicity<- factor(employ_datQ41.10.aEthnicity$EthnicityCleaned)
employ_datQ41.10.aEthnicity<-ordinaldatCleanNegative(employ_datQ41.10.aEthnicity$Q41.10.a, employ_datQ41.10.aEthnicity)
conTable <- xtabs(~Q41.10.a + EthnicityCleaned, data = employ_datQ41.10.aEthnicity)
conTable
#ordinal(employ_datQ41.10.aEthnicity$CatOutcome, employ_datQ41.10.aEthnicity$EthnicityCleaned, employ_datQ41.10.aEthnicity)
prep <- analysisPrep(employ_datQ41.10.aEthnicity$CatOutcome, employ_datQ41.10.aEthnicity$EthnicityCleaned, employ_datQ41.10.aEthnicity)
analysis <- polr(employ_datQ41.10.aEthnicity$CatOutcome ~ employ_datQ41.10.aEthnicity$EthnicityCleaned, data=employ_datQ41.10.aEthnicity, Hess=TRUE)
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
Dimension <- "Ethnicity"
new_OR_Outcomes <- data.frame(Question, Dimension, OR)
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes)
detach(data)

#### Analysis for Edinburgh people comparing the answer across FirstGen
"First Generation to go to University"
employ_datQ41.10.aFirstGen<-multidatClean(Q41.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aFirstGen$FirstGen<-factor(employ_datQ41.10.aFirstGen$FirstGen)
employ_datQ41.10.aFirstGen<-ordinaldatCleanNegative(employ_datQ41.10.aFirstGen$Q41.10.a, employ_datQ41.10.aFirstGen)
#ordinal(employ_datQ41.10.aFirstGen$CatOutcome, employ_datQ41.10.aFirstGen$FirstGen, employ_datQ41.10.aFirstGen)
prep <- analysisPrep(employ_datQ41.10.aFirstGen$CatOutcome, employ_datQ41.10.aFirstGen$FirstGen, employ_datQ41.10.aFirstGen)
analysis <- polr(employ_datQ41.10.aFirstGen$CatOutcome ~ employ_datQ41.10.aFirstGen$FirstGen, data=employ_datQ41.10.aFirstGen, Hess=TRUE)
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
Dimension <- "First generation to go to university"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

#### Analysis for Edinburgh people comparing the answer across Gender
"Gender"
employ_datQ41.10.aGender<-multidatClean(Q41.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aGender$Gender<-factor(employ_datQ41.10.aGender$Gender)
employ_datQ41.10.aGender<-ordinaldatCleanNegative(employ_datQ41.10.aGender$Q41.10.a, employ_datQ41.10.aGender)
#ordinal(employ_datQ41.10.aGender$CatOutcome, employ_datQ41.10.aGender$Gender, employ_datQ41.10.aGender)
prep <- analysisPrep(employ_datQ41.10.aGender$CatOutcome, employ_datQ41.10.aGender$Gender, employ_datQ41.10.aGender)
analysis <- polr(employ_datQ41.10.aGender$CatOutcome ~ employ_datQ41.10.aGender$Gender, data=employ_datQ41.10.aGender, Hess=TRUE)
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
Dimension <- "Gender - Male"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

### Analysis for Edinburgh people comparing the answer across Sexuality
"Sexuality"
employ_datQ41.10.aSexuality<-multidatClean(Q41.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ41.10.aSexuality$Sexuality<-factor(employ_datQ41.10.aSexuality$Sexuality)
employ_datQ41.10.aSexuality<-ordinaldatCleanNegative(employ_datQ41.10.aSexuality$Q41.10.a, employ_datQ41.10.aSexuality)
#ordinal(employ_datQ41.10.aSexuality$CatOutcome, employ_datQ41.10.aSexuality$Sexuality, employ_datQ41.10.aSexuality)
prep <- analysisPrep(employ_datQ41.10.aSexuality$CatOutcome, employ_datQ41.10.aSexuality$Sexuality, employ_datQ41.10.aSexuality)
analysis <- polr(employ_datQ41.10.aSexuality$CatOutcome ~ employ_datQ41.10.aSexuality$Sexuality, data=employ_datQ41.10.aSexuality, Hess=TRUE)
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
Dimension <- "Sexuality - other"
new_OR_Outcomes <- data.frame(Question, Dimension, OR[1,1], OR[1,2], OR[2,2])
colnames(new_OR_Outcomes) <-(c("Question", "Dimension", "OR", "l95", "u95"))
OR_Outcomes <- rbind(OR_Outcomes, new_OR_Outcomes) 
detach(data)

Q41.10.a_OR_Table <- OR_Outcomes
OR_Outcomes
graph_prep (OR_Outcomes)




