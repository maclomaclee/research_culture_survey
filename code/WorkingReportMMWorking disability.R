source("DataCleaning.R")
source("analysisfunctions.R")
library(forcats)

employ_dat<- cleandata_factor
employ_dat$Q3 <- as.character(employ_dat$Q3)
employ_dat$Q66 <- gsub("back, neck", "back neck", employ_dat$Q66)

employ_dat$Q66 <- as.character(employ_dat$Q66)
employ_datQ38.1Distype<-multidatClean(Q38.1, Q66, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ38.1Distype$Q66<- factor(employ_datQ38.1Distype$Q66)
employ_datQ38.1Distype<-ordinaldatClean(employ_datQ38.1Distype$Q38.1, employ_datQ38.1Distype)
conTable <- xtabs(~Q38.1 + Q66, data = employ_datQ38.1Distype)
#ordinal(employ_datQ38.1Disability$CatOutcome, employ_datQ38.1Disability$Disability, employ_datQ38.1Disability)
prep <- analysisPrep(employ_datQ38.1Distype$CatOutcome, employ_datQ38.1Distype$Q66, employ_datQ38.1Distype)
analysis <- polr(employ_datQ38.1Distype$CatOutcome ~ employ_datQ38.1Distype$Q66, data=employ_datQ38.1Distype, Hess=TRUE)
assumption<-brant(analysis)
analysisSummary <- summary(analysis)
ctable <- coef(summary(analysis))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(analysis) # default method gives profiled CIs
#confint.default(analysis) # CIs assuming normality
#exp(coef(analysis))
OR <- exp(cbind(OR = coef(analysis), ci))
OR
detach(data)









