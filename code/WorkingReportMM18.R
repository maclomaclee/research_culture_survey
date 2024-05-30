source("code/DataCleaning.R")
source("code/analysisfunctions.R")
library(forcats)
library(MASS)

employ_dat<- cleandata_factor[c(1,4, 142,37:45,130:137)]
#EdWelcome_dat_Work<-EdWelcome_dat[c(1,149:150,8:43)]

## Question 18 - How far do you agree or disagree with the following statements relating to your current working environment?


### Q18.1.a - My working environment promotes a good work-life balance, Very strongly agree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
employ_datQ18.1<-multidatClean(Q18.1.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.1.a + Academic + UniqueResponseNumber, data = employ_datQ18.1)
detach(dat_long)
employ_datQ18.1<-ordinaldatClean(employ_datQ18.1$Q18.1.a, employ_datQ18.1)
ordinal(employ_datQ18.1$CatOutcome, employ_datQ18.1$Academic, employ_datQ18.1)
detach(data)

#### Analysis for Edinburgh people comparing the answer across College
employ_datQ18.1College<-multidatClean(Q18.1.a, Q2.a, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.1.a + Q2.a, data = employ_datQ18.1College)
conTable
detach(dat_long)
employ_datQ18.1College$Q2.a[(employ_datQ18.1College$Q2.a == "Research Professional Staff")]="Other"
employ_datQ18.1College$Q2.a<- factor(employ_datQ18.1College$Q2.a)
employ_datQ18.1College<-ordinaldatClean(employ_datQ18.1College$Q18.1.a, employ_datQ18.1College)
ordinal(employ_datQ18.1College$CatOutcome, employ_datQ18.1College$Q2.a, employ_datQ18.1College)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
employ_datQ18.1Carer<-multidatClean(Q18.1.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1Carer$Carer<- factor(employ_datQ18.1Carer$Carer)
employ_datQ18.1Carer<-ordinaldatClean(employ_datQ18.1Carer$Q18.1.a, employ_datQ18.1Carer)
ordinal(employ_datQ18.1Carer$CatOutcome, employ_datQ18.1Carer$Carer, employ_datQ18.1Carer)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Disability
employ_datQ18.1Disability<-multidatClean(Q18.1.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1Disability$Disability<- factor(employ_datQ18.1Disability$Disability)
employ_datQ18.1Disability<-ordinaldatClean(employ_datQ18.1Disability$Q18.1.a, employ_datQ18.1Disability)
conTable <- xtabs(~Q18.1.a + Disability, data = employ_datQ18.1Disability)
conTable
ordinal(employ_datQ18.1Disability$CatOutcome, employ_datQ18.1Disability$Disability, employ_datQ18.1Disability)
detach(data)

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
employ_datQ18.1Ethnicity<-multidatClean(Q18.1.a, Ethnicity, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1Ethnicity$Ethnicity<- factor(employ_datQ18.1Ethnicity$Ethnicity)
employ_datQ18.1Ethnicity<-ordinaldatClean(employ_datQ18.1Ethnicity$Q18.1.a, employ_datQ18.1Ethnicity)
conTable <- xtabs(~Q18.1.a + Ethnicity, data = employ_datQ18.1Ethnicity)
conTable
ordinal(employ_datQ18.1Ethnicity$CatOutcome, employ_datQ18.1Ethnicity$Ethnicity, employ_datQ18.1Ethnicity)


detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.1-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.1FirstGen<-multidatClean(Q18.1.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1FirstGen$FirstGen<-factor(employ_datQ18.1FirstGen$FirstGen)
employ_datQ18.1FirstGen<-ordinaldatClean(employ_datQ18.1FirstGen$Q18.1.a, employ_datQ18.1FirstGen)
ordinal(employ_datQ18.1FirstGen$CatOutcome, employ_datQ18.1FirstGen$FirstGen, employ_datQ18.1FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.1-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.1Gender<-multidatClean(Q18.1.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1Gender$Gender<-factor(employ_datQ18.1Gender$Gender)
employ_datQ18.1Gender<-ordinaldatClean(employ_datQ18.1Gender$Q18.1.a, employ_datQ18.1Gender)
ordinal(employ_datQ18.1Gender$CatOutcome, employ_datQ18.1Gender$Gender, employ_datQ18.1Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.1-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.1GenderAssignedAtBirth<-multidatClean(Q18.1.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.1GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.1GenderAssignedAtBirth<-ordinaldatClean(employ_datQ18.1GenderAssignedAtBirth$Q18.1.a, employ_datQ18.1GenderAssignedAtBirth)
conTable <- xtabs(~Q18.1.a + GenderAssignedAtBirth, data = employ_datQ18.1GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.1GenderAssignedAtBirth$CatOutcome, employ_datQ18.1GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.1GenderAssignedAtBirth)


#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.1-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.1Sexuality<-multidatClean(Q18.1.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.1Sexuality$Sexuality<-factor(employ_datQ18.1Sexuality$Sexuality)
employ_datQ18.1Sexuality<-ordinaldatClean(employ_datQ18.1Sexuality$Q18.1.a, employ_datQ18.1Sexuality)
ordinal(employ_datQ18.1Sexuality$CatOutcome, employ_datQ18.1Sexuality$Sexuality, employ_datQ18.1Sexuality)

detach(data) #```

#### Analysis for Edinburgh 2022 v 2020
#```r Question18.1-10, warning=FALSE, echo = FALSE, message=FALSE}
v2020Q18.1<-multidatClean(Q18.1.a, surv, UniqueResponseNumber, Comb_dat)
conTable <- xtabs(~Q18.1.a + surv, data = v2020Q18.1)
conTable
detach(dat_long)
v2020Q18.1<-ordinaldatClean(v2020Q18.1$Q18.1.a, v2020Q18.1)
v2020Q18.1$surv<-factor(v2020Q18.1$surv)
ordinal(v2020Q18.1$Q18.1.a, v2020Q18.1$surv, v2020Q18.1)

detach(data) #```
### Q18.2.a - My working environment promotes a collaborative culture, Very strongly agree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.2-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2<-multidatClean(Q18.2.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.2.a + Academic, data = employ_datQ18.2)
detach(dat_long)
employ_datQ18.2<-ordinaldatClean(employ_datQ18.2$Q18.2.a, employ_datQ18.2)
ordinal(employ_datQ18.2$CatOutcome, employ_datQ18.2$Academic, employ_datQ18.2)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#{r Question18.2-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2College<-multidatClean(Q18.2.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.2.a + Q3, data = employ_datQ18.2College)
conTable
detach(dat_long)
employ_datQ18.2College$Q3[(employ_datQ18.2College$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.2College$Q3<- factor(employ_datQ18.2College$Q3)
employ_datQ18.2College<-ordinaldatClean(employ_datQ18.2College$Q18.2.a, employ_datQ18.2College)
ordinal(employ_datQ18.2College$CatOutcome, employ_datQ18.2College$Q3, employ_datQ18.2College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
##```r Question18.2-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2Carer<-multidatClean(Q18.2.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2Carer$Carer<- factor(employ_datQ18.2Carer$Carer)
employ_datQ18.2Carer<-ordinaldatClean(employ_datQ18.2Carer$Q18.2.a, employ_datQ18.2Carer)
ordinal(employ_datQ18.2Carer$CatOutcome, employ_datQ18.2Carer$Carer, employ_datQ18.2Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
##```r Question18.2-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2Disability<-multidatClean(Q18.2.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2Disability$Disability<- factor(employ_datQ18.2Disability$Disability)
employ_datQ18.2Disability<-ordinaldatClean(employ_datQ18.2Disability$Q18.2.a, employ_datQ18.2Disability)
conTable <- xtabs(~Q18.2.a + Disability, data = employ_datQ18.2Disability)
conTable
ordinal(employ_datQ18.2Disability$CatOutcome, employ_datQ18.2Disability$Disability, employ_datQ18.2Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
##```r Question18.2-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2Ethnicity<-multidatClean(Q18.2.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2Ethnicity$Ethnicity<- factor(employ_datQ18.2Ethnicity$EthnicityCleaned)
employ_datQ18.2Ethnicity<-ordinaldatClean(employ_datQ18.2Ethnicity$Q18.2.a, employ_datQ18.2Ethnicity)
conTable <- xtabs(~Q18.2.a + EthnicityCleaned, data = employ_datQ18.2Ethnicity)
conTable
ordinal(employ_datQ18.2Ethnicity$CatOutcome, employ_datQ18.2Ethnicity$EthnicityCleaned, employ_datQ18.2Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
##```r Question18.2-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2FirstGen<-multidatClean(Q18.2.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2FirstGen$FirstGen<-factor(employ_datQ18.2FirstGen$FirstGen)
employ_datQ18.2FirstGen<-ordinaldatClean(employ_datQ18.2FirstGen$Q18.2.a, employ_datQ18.2FirstGen)
ordinal(employ_datQ18.2FirstGen$CatOutcome, employ_datQ18.2FirstGen$FirstGen, employ_datQ18.2FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
##```r Question18.2-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2Gender<-multidatClean(Q18.2.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2Gender$Gender<-factor(employ_datQ18.2Gender$Gender)
employ_datQ18.2Gender<-ordinaldatClean(employ_datQ18.2Gender$Q18.2.a, employ_datQ18.2Gender)
ordinal(employ_datQ18.2Gender$CatOutcome, employ_datQ18.2Gender$Gender, employ_datQ18.2Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
##```r Question18.2-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2GenderAssignedAtBirth<-multidatClean(Q18.2.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.2GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.2GenderAssignedAtBirth<-ordinaldatClean(employ_datQ18.2GenderAssignedAtBirth$Q18.2.a, employ_datQ18.2GenderAssignedAtBirth)
conTable <- xtabs(~Q18.2.a + GenderAssignedAtBirth, data = employ_datQ18.2GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.2GenderAssignedAtBirth$CatOutcome, employ_datQ18.2GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.2GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
##```r Question18.2-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.2Sexuality<-multidatClean(Q18.2.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.2Sexuality$Sexuality<-factor(employ_datQ18.2Sexuality$Sexuality)
employ_datQ18.2Sexuality<-ordinaldatClean(employ_datQ18.2Sexuality$Q18.2.a, employ_datQ18.2Sexuality)
ordinal(employ_datQ18.2Sexuality$CatOutcome, employ_datQ18.2Sexuality$Sexuality, employ_datQ18.2Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
##```r Question18.2-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.2<-multidatCleanWelcome(Q18.2.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.2.a + Survey, data = EdWelcome_datQ18.2)
conTable
detach(dat_long)
EdWelcome_datQ18.2$Q18.2.a[(EdWelcome_datQ18.2$Q18.2.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.2<-ordinaldatWelcomeClean(EdWelcome_datQ18.2)
EdWelcome_datQ18.2$Survey<-factor(EdWelcome_datQ18.2$Survey)
ordinal(EdWelcome_datQ18.2$Q18.2.a, EdWelcome_datQ18.2$Survey, EdWelcome_datQ18.2)

detach(data) #```
### Q18.3.a - Creativity is welcomed within my working environment in all its forms, Very strongly agree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
##```r Question18.3-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3<-multidatClean(Q18.3.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.3.a + Academic, data = employ_datQ18.3)
detach(dat_long)
employ_datQ18.3<-ordinaldatClean(employ_datQ18.3$Q18.3.a, employ_datQ18.3)
ordinal(employ_datQ18.3$CatOutcome, employ_datQ18.3$Academic, employ_datQ18.3)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
##```r Question18.3-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3College<-multidatClean(Q18.3.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.3.a + Q3, data = employ_datQ18.3College)
conTable
detach(dat_long)
employ_datQ18.3College$Q3[(employ_datQ18.3College$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.3College$Q3<- factor(employ_datQ18.3College$Q3)
employ_datQ18.3College<-ordinaldatClean(employ_datQ18.3College$Q18.3.a, employ_datQ18.3College)
ordinal(employ_datQ18.3College$CatOutcome, employ_datQ18.3College$Q3, employ_datQ18.3College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.3-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3Carer<-multidatClean(Q18.3.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3Carer$Carer<- factor(employ_datQ18.3Carer$Carer)
employ_datQ18.3Carer<-ordinaldatClean(employ_datQ18.3Carer$Q18.3.a, employ_datQ18.3Carer)
ordinal(employ_datQ18.3Carer$CatOutcome, employ_datQ18.3Carer$Carer, employ_datQ18.3Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.3-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3Disability<-multidatClean(Q18.3.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3Disability$Disability<- factor(employ_datQ18.3Disability$Disability)
employ_datQ18.3Disability<-ordinaldatClean(employ_datQ18.3Disability$Q18.3.a, employ_datQ18.3Disability)
conTable <- xtabs(~Q18.3.a + Disability, data = employ_datQ18.3Disability)
conTable
ordinal(employ_datQ18.3Disability$CatOutcome, employ_datQ18.3Disability$Disability, employ_datQ18.3Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.3-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3Ethnicity<-multidatClean(Q18.3.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3Ethnicity$Ethnicity<- factor(employ_datQ18.3Ethnicity$EthnicityCleaned)
employ_datQ18.3Ethnicity<-ordinaldatClean(employ_datQ18.3Ethnicity$Q18.3.a, employ_datQ18.3Ethnicity)
conTable <- xtabs(~Q18.3.a + EthnicityCleaned, data = employ_datQ18.3Ethnicity)
conTable
ordinal(employ_datQ18.3Ethnicity$CatOutcome, employ_datQ18.3Ethnicity$EthnicityCleaned, employ_datQ18.3Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.3-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3FirstGen<-multidatClean(Q18.3.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3FirstGen$FirstGen<-factor(employ_datQ18.3FirstGen$FirstGen)
employ_datQ18.3FirstGen<-ordinaldatClean(employ_datQ18.3FirstGen$Q18.3.a, employ_datQ18.3FirstGen)
ordinal(employ_datQ18.3FirstGen$CatOutcome, employ_datQ18.3FirstGen$FirstGen, employ_datQ18.3FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.3-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3Gender<-multidatClean(Q18.3.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3Gender$Gender<-factor(employ_datQ18.3Gender$Gender)
employ_datQ18.3Gender<-ordinaldatClean(employ_datQ18.3Gender$Q18.3.a, employ_datQ18.3Gender)
ordinal(employ_datQ18.3Gender$CatOutcome, employ_datQ18.3Gender$Gender, employ_datQ18.3Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.3-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3GenderAssignedAtBirth<-multidatClean(Q18.3.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.3GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.3GenderAssignedAtBirth<-ordinaldatClean(employ_datQ18.3GenderAssignedAtBirth$Q18.3.a, employ_datQ18.3GenderAssignedAtBirth)
conTable <- xtabs(~Q18.3.a + GenderAssignedAtBirth, data = employ_datQ18.3GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.3GenderAssignedAtBirth$CatOutcome, employ_datQ18.3GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.3GenderAssignedAtBirth)

 #```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.3-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.3Sexuality<-multidatClean(Q18.3.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.3Sexuality$Sexuality<-factor(employ_datQ18.3Sexuality$Sexuality)
employ_datQ18.3Sexuality<-ordinaldatClean(employ_datQ18.3Sexuality$Q18.3.a, employ_datQ18.3Sexuality)
ordinal(employ_datQ18.3Sexuality$CatOutcome, employ_datQ18.3Sexuality$Sexuality, employ_datQ18.3Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.3-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.3<-multidatCleanWelcome(Q18.3.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.3.a + Survey, data = EdWelcome_datQ18.3)
conTable
detach(dat_long)
EdWelcome_datQ18.3$Q18.3.a[(EdWelcome_datQ18.3$Q18.3.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.3<-ordinaldatWelcomeClean(EdWelcome_datQ18.3)
EdWelcome_datQ18.3$Survey<-factor(EdWelcome_datQ18.3$Survey)
ordinal(EdWelcome_datQ18.3$Q18.3.a, EdWelcome_datQ18.3$Survey, EdWelcome_datQ18.3)

detach(data) #```
### Q18.4.a - Healthy competition is encouraged within my working environment Very strongly agree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.4-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4<-multidatClean(Q18.4.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.4.a + Academic, data = employ_datQ18.4)
detach(dat_long)
employ_datQ18.4<-ordinaldatClean(employ_datQ18.4$Q18.4.a, employ_datQ18.4)
ordinal(employ_datQ18.4$CatOutcome, employ_datQ18.4$Academic, employ_datQ18.4)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.4-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4College<-multidatClean(Q18.4.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.4.a + Q3, data = employ_datQ18.4College)
conTable
detach(dat_long)
employ_datQ18.4College$Q3[(employ_datQ18.4College$Q3 == "Research Professional Staff"| employ_datQ18.4College$Q3 == "Other")]=NA
employ_datQ18.4College$Q3<- factor(employ_datQ18.4College$Q3)
employ_datQ18.4College<-ordinaldatClean(employ_datQ18.4College$Q18.4.a, employ_datQ18.4College)
ordinal(employ_datQ18.4College$CatOutcome, employ_datQ18.4College$Q3, employ_datQ18.4College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.4-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4Carer<-multidatClean(Q18.4.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4Carer$Carer<- factor(employ_datQ18.4Carer$Carer)
employ_datQ18.4Carer<-ordinaldatClean(employ_datQ18.4Carer$Q18.4.a, employ_datQ18.4Carer)
ordinal(employ_datQ18.4Carer$CatOutcome, employ_datQ18.4Carer$Carer, employ_datQ18.4Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#Very unbalanced, just a few yes answers across likert scales
#```r Question18.4-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4Disability<-multidatClean(Q18.4.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4Disability$Disability<- factor(employ_datQ18.4Disability$Disability)
employ_datQ18.4Disability<-ordinaldatClean(employ_datQ18.4Disability$Q18.4.a, employ_datQ18.4Disability)
conTable <- xtabs(~Q18.4.a + Disability, data = employ_datQ18.4Disability)
conTable
ordinal(employ_datQ18.4Disability$CatOutcome, employ_datQ18.4Disability$Disability, employ_datQ18.4Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.4-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4Ethnicity<-multidatClean(Q18.4.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4Ethnicity$Ethnicity<- factor(employ_datQ18.4Ethnicity$EthnicityCleaned)
employ_datQ18.4Ethnicity<-ordinaldatClean(employ_datQ18.4Ethnicity$Q18.4.a, employ_datQ18.4Ethnicity)
conTable <- xtabs(~Q18.4.a + EthnicityCleaned, data = employ_datQ18.4Ethnicity)
conTable
ordinal(employ_datQ18.4Ethnicity$CatOutcome, employ_datQ18.4Ethnicity$EthnicityCleaned, employ_datQ18.4Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.4-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4FirstGen<-multidatClean(Q18.4.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4FirstGen$FirstGen<-factor(employ_datQ18.4FirstGen$FirstGen)
employ_datQ18.4FirstGen<-ordinaldatClean(employ_datQ18.4FirstGen$Q18.4.a, employ_datQ18.4FirstGen)
ordinal(employ_datQ18.4FirstGen$CatOutcome, employ_datQ18.4FirstGen$FirstGen, employ_datQ18.4FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.4-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4Gender<-multidatClean(Q18.4.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4Gender$Gender<-factor(employ_datQ18.4Gender$Gender)
employ_datQ18.4Gender<-ordinaldatClean(employ_datQ18.4Gender$Q18.4.a, employ_datQ18.4Gender)
ordinal(employ_datQ18.4Gender$CatOutcome, employ_datQ18.4Gender$Gender, employ_datQ18.4Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.4-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4GenderAssignedAtBirth<-multidatClean(Q18.4.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.4GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.4GenderAssignedAtBirth<-ordinaldatClean(employ_datQ18.4GenderAssignedAtBirth$Q18.4.a, employ_datQ18.4GenderAssignedAtBirth)
conTable <- xtabs(~Q18.4.a + GenderAssignedAtBirth, data = employ_datQ18.4GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.4GenderAssignedAtBirth$CatOutcome, employ_datQ18.4GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.4GenderAssignedAtBirth)

 #```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.4-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.4Sexuality<-multidatClean(Q18.4.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.4Sexuality$Sexuality<-factor(employ_datQ18.4Sexuality$Sexuality)
employ_datQ18.4Sexuality<-ordinaldatClean(employ_datQ18.4Sexuality$Q18.4.a, employ_datQ18.4Sexuality)
ordinal(employ_datQ18.4Sexuality$CatOutcome, employ_datQ18.4Sexuality$Sexuality, employ_datQ18.4Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.4-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.4<-multidatCleanWelcome(Q18.4.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.4.a + Survey, data = EdWelcome_datQ18.4)
conTable
detach(dat_long)
EdWelcome_datQ18.4$Q18.4.a[(EdWelcome_datQ18.4$Q18.4.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.4<-ordinaldatWelcomeClean(EdWelcome_datQ18.4)
EdWelcome_datQ18.4$Survey<-factor(EdWelcome_datQ18.4$Survey)
ordinal(EdWelcome_datQ18.4$Q18.4.a, EdWelcome_datQ18.4$Survey, EdWelcome_datQ18.4)
detach(data) #```
### Q18.5.a - Unhealthy competition is present within my working environment Very strongly disagree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.5-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5<-multidatClean(Q18.5.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.5.a + Academic, data = employ_datQ18.5)
detach(dat_long)
employ_datQ18.5<- ordinaldatCleanNegative(employ_datQ18.5$Q18.5.a,employ_datQ18.5)
ordinal(employ_datQ18.5$CatOutcome, employ_datQ18.5$Academic, employ_datQ18.5)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.5-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5College<-multidatClean(Q18.5.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.5.a + Q3, data = employ_datQ18.5College)
conTable
detach(dat_long)
employ_datQ18.5College$Q3[(employ_datQ18.5College$Q3 == "Research Professional Staff" |employ_datQ18.5College$Q3 == "Other")]=NA
employ_datQ18.5College$Q3<- factor(employ_datQ18.5College$Q3)
employ_datQ18.5College<- ordinaldatCleanNegative(employ_datQ18.5College$Q18.5.a,employ_datQ18.5College)
ordinal(employ_datQ18.5College$CatOutcome, employ_datQ18.5College$Q3, employ_datQ18.5College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.5-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5Carer<-multidatClean(Q18.5.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5Carer$Carer<- factor(employ_datQ18.5Carer$Carer)
conTable <- xtabs(~Q18.5.a + Carer, data = employ_datQ18.5Carer)
conTable
employ_datQ18.5Carer<- ordinaldatCleanNegative (employ_datQ18.5Carer$Q18.5.a, employ_datQ18.5Carer)
ordinal(employ_datQ18.5Carer$CatOutcome, employ_datQ18.5Carer$Carer, employ_datQ18.5Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.5-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5Disability<-multidatClean(Q18.5.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5Disability$Disability<- factor(employ_datQ18.5Disability$Disability)
employ_datQ18.5Disability<- ordinaldatCleanNegative (employ_datQ18.5Disability$Q18.5.a, employ_datQ18.5Disability)
conTable <- xtabs(~Q18.5.a + Disability, data = employ_datQ18.5Disability)
conTable
ordinal(employ_datQ18.5Disability$CatOutcome, employ_datQ18.5Disability$Disability, employ_datQ18.5Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.5-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5Ethnicity<-multidatClean(Q18.5.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5Ethnicity$Ethnicity<- factor(employ_datQ18.5Ethnicity$EthnicityCleaned)
employ_datQ18.5Ethnicity<- ordinaldatCleanNegative(employ_datQ18.5Ethnicity$Q18.5.a,employ_datQ18.5Ethnicity)
conTable <- xtabs(~Q18.5.a + EthnicityCleaned, data = employ_datQ18.5Ethnicity)
conTable
ordinal(employ_datQ18.5Ethnicity$CatOutcome, employ_datQ18.5Ethnicity$EthnicityCleaned, employ_datQ18.5Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.5-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5FirstGen<-multidatClean(Q18.5.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5FirstGen$FirstGen<-factor(employ_datQ18.5FirstGen$FirstGen)
employ_datQ18.5FirstGen<- ordinaldatCleanNegative (employ_datQ18.5FirstGen$Q18.5.a,employ_datQ18.5FirstGen)
ordinal(employ_datQ18.5FirstGen$CatOutcome, employ_datQ18.5FirstGen$FirstGen, employ_datQ18.5FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.5-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5Gender<-multidatClean(Q18.5.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5Gender$Gender<-factor(employ_datQ18.5Gender$Gender)
employ_datQ18.5Gender<- ordinaldatCleanNegative (employ_datQ18.5Gender$Q18.5.a, employ_datQ18.5Gender)
ordinal(employ_datQ18.5Gender$CatOutcome, employ_datQ18.5Gender$Gender, employ_datQ18.5Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.5-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5GenderAssignedAtBirth<-multidatClean(Q18.5.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.5GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.5GenderAssignedAtBirth<- ordinaldatCleanNegative (employ_datQ18.5GenderAssignedAtBirth$Q18.5.a, employ_datQ18.5GenderAssignedAtBirth)
conTable <- xtabs(~Q18.5.a + GenderAssignedAtBirth, data = employ_datQ18.5GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.5GenderAssignedAtBirth$CatOutcome, employ_datQ18.5GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.5GenderAssignedAtBirth)

 #```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.5-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.5Sexuality<-multidatClean(Q18.5.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.5Sexuality$Sexuality<-factor(employ_datQ18.5Sexuality$Sexuality)
employ_datQ18.5Sexuality<- ordinaldatCleanNegative (employ_datQ18.5Sexuality$Q18.5.a, employ_datQ18.5Sexuality)
ordinal(employ_datQ18.5Sexuality$CatOutcome, employ_datQ18.5Sexuality$Sexuality, employ_datQ18.5Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.5-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.5<-multidatCleanWelcome(Q18.5.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.5.a + Survey, data = EdWelcome_datQ18.5)
conTable
detach(dat_long)
EdWelcome_datQ18.5$Q18.5.a[(EdWelcome_datQ18.5$Q18.5.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.5<- ordinaldatWelcomeClean (EdWelcome_datQ18.5, reverse=TRUE)
EdWelcome_datQ18.5$Survey<-factor(EdWelcome_datQ18.5$Survey)
EdWelcome_datQ18.5$Q18.5.a<-factor(EdWelcome_datQ18.5$Q18.5.a)
ordinal(EdWelcome_datQ18.5$Q18.5.a, EdWelcome_datQ18.5$Survey, EdWelcome_datQ18.5)

detach(data) #```
### Q18.6.a - The University values speed of results over quality Very strongly disagree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.6-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6<-multidatClean(Q18.6.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.6.a + Academic, data = employ_datQ18.6)
detach(dat_long)
employ_datQ18.6<- ordinaldatCleanNegative (employ_datQ18.6$Q18.6.a,employ_datQ18.6)
ordinal(employ_datQ18.6$CatOutcome, employ_datQ18.6$Academic, employ_datQ18.6)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.6-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6College<-multidatClean(Q18.6.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.6.a + Q3, data = employ_datQ18.6College)
conTable
detach(dat_long)
employ_datQ18.6College$Q3[(employ_datQ18.6College$Q3 == "Research Professional Staff"|employ_datQ18.6College$Q3 =="Other")]=NA
employ_datQ18.6College$Q3<- factor(employ_datQ18.6College$Q3)
employ_datQ18.6College<- ordinaldatCleanNegative (employ_datQ18.6College$Q18.6.a, employ_datQ18.6College)
ordinal(employ_datQ18.6College$CatOutcome, employ_datQ18.6College$Q3, employ_datQ18.6College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.6-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6Carer<-multidatClean(Q18.6.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6Carer$Carer<- factor(employ_datQ18.6Carer$Carer)
employ_datQ18.6Carer<- ordinaldatCleanNegative (employ_datQ18.6Carer$Q18.6.a, employ_datQ18.6Carer)
ordinal(employ_datQ18.6Carer$CatOutcome, employ_datQ18.6Carer$Carer, employ_datQ18.6Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.6-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6Disability<-multidatClean(Q18.6.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6Disability$Disability<- factor(employ_datQ18.6Disability$Disability)
employ_datQ18.6Disability<- ordinaldatCleanNegative (employ_datQ18.6Disability$Q18.6.a, employ_datQ18.6Disability)
conTable <- xtabs(~Q18.6.a + Disability, data = employ_datQ18.6Disability)
conTable
ordinal(employ_datQ18.6Disability$CatOutcome, employ_datQ18.6Disability$Disability, employ_datQ18.6Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.6-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6Ethnicity<-multidatClean(Q18.6.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6Ethnicity$Ethnicity<- factor(employ_datQ18.6Ethnicity$EthnicityCleaned)
employ_datQ18.6Ethnicity<-  ordinaldatCleanNegative (employ_datQ18.6Ethnicity$Q18.6.a, employ_datQ18.6Ethnicity)
conTable <- xtabs(~Q18.6.a + EthnicityCleaned, data = employ_datQ18.6Ethnicity)
conTable
ordinal(employ_datQ18.6Ethnicity$CatOutcome, employ_datQ18.6Ethnicity$EthnicityCleaned, employ_datQ18.6Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.6-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6FirstGen<-multidatClean(Q18.6.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6FirstGen$FirstGen<-factor(employ_datQ18.6FirstGen$FirstGen)
employ_datQ18.6FirstGen<- ordinaldatCleanNegative (employ_datQ18.6FirstGen$Q18.6.a,employ_datQ18.6FirstGen)
ordinal(employ_datQ18.6FirstGen$CatOutcome, employ_datQ18.6FirstGen$FirstGen, employ_datQ18.6FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.6-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6Gender<-multidatClean(Q18.6.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6Gender$Gender<-factor(employ_datQ18.6Gender$Gender)
employ_datQ18.6Gender<-  ordinaldatCleanNegative (employ_datQ18.6Gender$Q18.6.a,employ_datQ18.6Gender)
ordinal(employ_datQ18.6Gender$CatOutcome, employ_datQ18.6Gender$Gender, employ_datQ18.6Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.6-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6GenderAssignedAtBirth<-multidatClean(Q18.6.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.6GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.6GenderAssignedAtBirth<-  ordinaldatCleanNegative (employ_datQ18.6GenderAssignedAtBirth$Q18.6.a,employ_datQ18.6GenderAssignedAtBirth)
conTable <- xtabs(~Q18.6.a + GenderAssignedAtBirth, data = employ_datQ18.6GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.6GenderAssignedAtBirth$CatOutcome, employ_datQ18.6GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.6GenderAssignedAtBirth)

 #```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.6-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.6Sexuality<-multidatClean(Q18.6.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.6Sexuality$Sexuality<-factor(employ_datQ18.6Sexuality$Sexuality)
employ_datQ18.6Sexuality<- ordinaldatCleanNegative (employ_datQ18.6Sexuality$Q18.6.a,employ_datQ18.6Sexuality)
ordinal(employ_datQ18.6Sexuality$CatOutcome, employ_datQ18.6Sexuality$Sexuality, employ_datQ18.6Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.6-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.6<-multidatCleanWelcome(Q18.6.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.6.a + Survey, data = EdWelcome_datQ18.6)
conTable
detach(dat_long)
EdWelcome_datQ18.6$Q18.6.a[(EdWelcome_datQ18.6$Q18.6.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.6<- ordinaldatWelcomeClean (EdWelcome_datQ18.6, reverse=TRUE)
EdWelcome_datQ18.6$Survey<-factor(EdWelcome_datQ18.6$Survey)
EdWelcome_datQ18.6$Q18.6.a<-factor(EdWelcome_datQ18.6$Q18.6.a)
ordinal(EdWelcome_datQ18.6$Q18.6.a, EdWelcome_datQ18.6$Survey, EdWelcome_datQ18.6)

detach(data) #```
### Q18.7.a - The University could do more to ensure research practices do not cut corners Very strongly disagree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.7-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7<-multidatClean(Q18.7.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.7.a + Academic, data = employ_datQ18.7)
detach(dat_long)
employ_datQ18.7<- ordinaldatCleanNegative (employ_datQ18.7$Q18.7.a, employ_datQ18.7)
ordinal(employ_datQ18.7$CatOutcome, employ_datQ18.7$Academic, employ_datQ18.7)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.7-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7College<-multidatClean(Q18.7.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.7.a + Q3, data = employ_datQ18.7College)
conTable
detach(dat_long)
employ_datQ18.7College$Q3[(employ_datQ18.7College$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.7College$Q3<- factor(employ_datQ18.7College$Q3)
employ_datQ18.7College<- ordinaldatCleanNegative (employ_datQ18.7College$Q18.7.a, employ_datQ18.7College)
ordinal(employ_datQ18.7College$CatOutcome, employ_datQ18.7College$Q3, employ_datQ18.7College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.7-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7Carer<-multidatClean(Q18.7.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7Carer<- ordinaldatCleanNegative (employ_datQ18.7Carer$Q18.7.a, employ_datQ18.7Carer)
conTable <- xtabs(~Q18.7.a + Carer, data = employ_datQ18.7Carer)
employ_datQ18.7Carer$Carer<- factor(employ_datQ18.7Carer$Carer)
ordinal(employ_datQ18.7Carer$CatOutcome, employ_datQ18.7Carer$Carer, employ_datQ18.7Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.7-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7Disability<-multidatClean(Q18.7.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7Disability$Disability<- factor(employ_datQ18.7Disability$Disability)
employ_datQ18.7Disability<- ordinaldatCleanNegative (employ_datQ18.7Disability$Q18.7.a, employ_datQ18.7Disability)
conTable <- xtabs(~Q18.7.a + Disability, data = employ_datQ18.7Disability)
conTable
#ordinal(employ_datQ18.7Disability$CatOutcome, employ_datQ18.7Disability$Disability, employ_datQ18.7Disability)
 #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.7-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7Ethnicity<-multidatClean(Q18.7.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7Ethnicity$Ethnicity<- factor(employ_datQ18.7Ethnicity$EthnicityCleaned)
employ_datQ18.7Ethnicity<- ordinaldatCleanNegative (employ_datQ18.7Ethnicity$Q18.7.a, employ_datQ18.7Ethnicity)
conTable <- xtabs(~Q18.7.a + EthnicityCleaned, data = employ_datQ18.7Ethnicity)
conTable
ordinal(employ_datQ18.7Ethnicity$CatOutcome, employ_datQ18.7Ethnicity$EthnicityCleaned, employ_datQ18.7Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.7-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7FirstGen<-multidatClean(Q18.7.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7FirstGen$FirstGen<-factor(employ_datQ18.7FirstGen$FirstGen)
employ_datQ18.7FirstGen<- ordinaldatCleanNegative (employ_datQ18.7FirstGen$Q18.7.a,employ_datQ18.7FirstGen)
ordinal(employ_datQ18.7FirstGen$CatOutcome, employ_datQ18.7FirstGen$FirstGen, employ_datQ18.7FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.7-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7Gender<-multidatClean(Q18.7.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7Gender$Gender<-factor(employ_datQ18.7Gender$Gender)
employ_datQ18.7Gender<- ordinaldatCleanNegative (employ_datQ18.7Gender$Q18.7.a,employ_datQ18.7Gender)
ordinal(employ_datQ18.7Gender$CatOutcome, employ_datQ18.7Gender$Gender, employ_datQ18.7Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.7-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7GenderAssignedAtBirth<-multidatClean(Q18.7.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.7GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.7GenderAssignedAtBirth<- ordinaldatCleanNegative (employ_datQ18.7GenderAssignedAtBirth$Q18.7.a,employ_datQ18.7GenderAssignedAtBirth)
conTable <- xtabs(~Q18.7.a + GenderAssignedAtBirth, data = employ_datQ18.7GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.7GenderAssignedAtBirth$CatOutcome, employ_datQ18.7GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.7GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.7-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.7Sexuality<-multidatClean(Q18.7.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.7Sexuality$Sexuality<-factor(employ_datQ18.7Sexuality$Sexuality)
employ_datQ18.7Sexuality<- ordinaldatCleanNegative (employ_datQ18.7Sexuality$Q18.7.a,employ_datQ18.7Sexuality)
ordinal(employ_datQ18.7Sexuality$CatOutcome, employ_datQ18.7Sexuality$Sexuality, employ_datQ18.7Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.7-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.7<-multidatCleanWelcome(Q18.7.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.7.a + Survey, data = EdWelcome_datQ18.7)
conTable
detach(dat_long)
EdWelcome_datQ18.7$Q18.7.a[(EdWelcome_datQ18.7$Q18.7.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.7<- ordinaldatWelcomeClean (EdWelcome_datQ18.7, reverse=TRUE)
EdWelcome_datQ18.7$Survey<-factor(EdWelcome_datQ18.7$Survey)
EdWelcome_datQ18.7$Q18.7.a<-factor(EdWelcome_datQ18.7$Q18.7.a)
ordinal(EdWelcome_datQ18.7$Q18.7.a, EdWelcome_datQ18.7$Survey, EdWelcome_datQ18.7)

detach(data) #```
### Q18.8.a - Rigour of results is considered an important research outcome by the University Very strongly agree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.8-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8<-multidatClean(Q18.8.a, Academic, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8$Q18.8.a<-factor(employ_datQ18.8$Q18.8.a)
employ_datQ18.8$Academic<-factor(employ_datQ18.8$Academic)
conTable <- xtabs(~Q18.8.a + Academic, data = employ_datQ18.8)
employ_datQ18.8<- ordinaldatClean(employ_datQ18.8$Q18.8.a, employ_datQ18.8)
ordinal(employ_datQ18.8$CatOutcome, employ_datQ18.8$Academic, employ_datQ18.8)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.8-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8College<-multidatClean(Q18.8.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.8.a + Q3, data = employ_datQ18.8College)
conTable
detach(dat_long)
employ_datQ18.8College$Q3[(employ_datQ18.8College$Q3 == "Research Professional Staff"|employ_datQ18.8College$Q3 == "Other")]=NA
employ_datQ18.8College$Q3<- factor(employ_datQ18.8College$Q3)
employ_datQ18.8College<- ordinaldatClean(employ_datQ18.8College$Q18.8.a,employ_datQ18.8College)
ordinal(employ_datQ18.8College$CatOutcome, employ_datQ18.8College$Q3, employ_datQ18.8College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.8-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8Carer<-multidatClean(Q18.8.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8Carer$Carer<- factor(employ_datQ18.8Carer$Carer)
employ_datQ18.8Carer<- ordinaldatClean(employ_datQ18.8Carer$Q18.8.a, employ_datQ18.8Carer)
ordinal(employ_datQ18.8Carer$CatOutcome, employ_datQ18.8Carer$Carer, employ_datQ18.8Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.8-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8Disability<-multidatClean(Q18.8.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8Disability$Disability<- factor(employ_datQ18.8Disability$Disability)
employ_datQ18.8Disability<- ordinaldatClean(employ_datQ18.8Disability$Q18.8.a,employ_datQ18.8Disability)
conTable <- xtabs(~Q18.8.a + Disability, data = employ_datQ18.8Disability)
conTable
ordinal(employ_datQ18.8Disability$CatOutcome, employ_datQ18.8Disability$Disability, employ_datQ18.8Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.8-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8Ethnicity<-multidatClean(Q18.8.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8Ethnicity$Ethnicity<- factor(employ_datQ18.8Ethnicity$EthnicityCleaned)
employ_datQ18.8Ethnicity<- ordinaldatClean(employ_datQ18.8Ethnicity$Q18.8.a,employ_datQ18.8Ethnicity)
conTable <- xtabs(~Q18.8.a + EthnicityCleaned, data = employ_datQ18.8Ethnicity)
conTable
ordinal(employ_datQ18.8Ethnicity$CatOutcome, employ_datQ18.8Ethnicity$EthnicityCleaned, employ_datQ18.8Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.8-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8FirstGen<-multidatClean(Q18.8.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8FirstGen$FirstGen<-factor(employ_datQ18.8FirstGen$FirstGen)
employ_datQ18.8FirstGen<- ordinaldatClean(employ_datQ18.8FirstGen$Q18.8.a,employ_datQ18.8FirstGen)
ordinal(employ_datQ18.8FirstGen$CatOutcome, employ_datQ18.8FirstGen$FirstGen, employ_datQ18.8FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.8-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8Gender<-multidatClean(Q18.8.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8Gender$Gender<-factor(employ_datQ18.8Gender$Gender)
employ_datQ18.8Gender<- ordinaldatClean(employ_datQ18.8Gender$Q18.8.a, employ_datQ18.8Gender)
ordinal(employ_datQ18.8Gender$CatOutcome, employ_datQ18.8Gender$Gender, employ_datQ18.8Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.8-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8GenderAssignedAtBirth<-multidatClean(Q18.8.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.8GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.8GenderAssignedAtBirth<- ordinaldatClean(employ_datQ18.8GenderAssignedAtBirth$Q18.8.a,employ_datQ18.8GenderAssignedAtBirth)
conTable <- xtabs(~Q18.8.a + GenderAssignedAtBirth, data = employ_datQ18.8GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.8GenderAssignedAtBirth$CatOutcome, employ_datQ18.8GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.8GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.8-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.8Sexuality<-multidatClean(Q18.8.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.8Sexuality$Sexuality<-factor(employ_datQ18.8Sexuality$Sexuality)
employ_datQ18.8Sexuality<- ordinaldatClean(employ_datQ18.8Sexuality$Q18.8.a, employ_datQ18.8Sexuality)
ordinal(employ_datQ18.8Sexuality$CatOutcome, employ_datQ18.8Sexuality$Sexuality, employ_datQ18.8Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.8-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.8<-multidatCleanWelcome(Q18.8.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.8.a + Survey, data = EdWelcome_datQ18.8)
conTable
detach(dat_long)
EdWelcome_datQ18.8$Q18.8.a[(EdWelcome_datQ18.8$Q18.8.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.8<- ordinaldatWelcomeClean (EdWelcome_datQ18.8, reverse=FALSE)
EdWelcome_datQ18.8$Survey<-factor(EdWelcome_datQ18.8$Survey)
EdWelcome_datQ18.8$Q18.8.a<-factor(EdWelcome_datQ18.8$Q18.8.a)
ordinal(EdWelcome_datQ18.8$Q18.8.a, EdWelcome_datQ18.8$Survey, EdWelcome_datQ18.8)

detach(data) #```

### Q18.9.a., The University places more value on meeting metrics, than it does on research quality strongly disagree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.9-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9<-multidatClean(Q18.9.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.9.a + Academic, data = employ_datQ18.9)
detach(dat_long)
employ_datQ18.9<- ordinaldatCleanNegative(employ_datQ18.9$Q18.9.a,employ_datQ18.9)
ordinal(employ_datQ18.9$CatOutcome, employ_datQ18.9$Academic, employ_datQ18.9)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.9-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9College<-multidatClean(Q18.9.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.9.a + Q3, data = employ_datQ18.9College)
conTable
detach(dat_long)
employ_datQ18.9College$Q3[(employ_datQ18.9College$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.9College$Q3<- factor(employ_datQ18.9College$Q3)
employ_datQ18.9College<- ordinaldatCleanNegative(employ_datQ18.9College$Q18.9.a,employ_datQ18.9College)
ordinal(employ_datQ18.9College$CatOutcome, employ_datQ18.9College$Q3, employ_datQ18.9College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.9-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9Carer<-multidatClean(Q18.9.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9Carer$Carer<- factor(employ_datQ18.9Carer$Carer)
employ_datQ18.9Carer<- ordinaldatCleanNegative(employ_datQ18.9Carer$Q18.9.a,employ_datQ18.9Carer)
ordinal(employ_datQ18.9Carer$CatOutcome, employ_datQ18.9Carer$Carer, employ_datQ18.9Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.9-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9Disability<-multidatClean(Q18.9.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9Disability$Disability<- factor(employ_datQ18.9Disability$Disability)
employ_datQ18.9Disability<- ordinaldatCleanNegative(employ_datQ18.9Disability$Q18.9.a,employ_datQ18.9Disability)
conTable <- xtabs(~Q18.9.a + Disability, data = employ_datQ18.9Disability)
conTable
ordinal(employ_datQ18.9Disability$CatOutcome, employ_datQ18.9Disability$Disability, employ_datQ18.9Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.9-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9Ethnicity<-multidatClean(Q18.9.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9Ethnicity$Ethnicity<- factor(employ_datQ18.9Ethnicity$EthnicityCleaned)
employ_datQ18.9Ethnicity<- ordinaldatCleanNegative(employ_datQ18.9Ethnicity$Q18.9.a,employ_datQ18.9Ethnicity)
conTable <- xtabs(~Q18.9.a + EthnicityCleaned, data = employ_datQ18.9Ethnicity)
conTable
ordinal(employ_datQ18.9Ethnicity$CatOutcome, employ_datQ18.9Ethnicity$EthnicityCleaned, employ_datQ18.9Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.9-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9FirstGen<-multidatClean(Q18.9.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9FirstGen$FirstGen<-factor(employ_datQ18.9FirstGen$FirstGen)
employ_datQ18.9FirstGen<- ordinaldatCleanNegative(employ_datQ18.9FirstGen$Q18.9.a,employ_datQ18.9FirstGen)
ordinal(employ_datQ18.9FirstGen$CatOutcome, employ_datQ18.9FirstGen$FirstGen, employ_datQ18.9FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.9-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9Gender<-multidatClean(Q18.9.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9Gender$Gender<-factor(employ_datQ18.9Gender$Gender)
employ_datQ18.9Gender<- ordinaldatCleanNegative(employ_datQ18.9Gender$Q18.9.a,employ_datQ18.9Gender)
ordinal(employ_datQ18.9Gender$CatOutcome, employ_datQ18.9Gender$Gender, employ_datQ18.9Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.9-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9GenderAssignedAtBirth<-multidatClean(Q18.9.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.9GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.9GenderAssignedAtBirth<- ordinaldatCleanNegative(employ_datQ18.9GenderAssignedAtBirth$Q18.9.a,employ_datQ18.9GenderAssignedAtBirth)
conTable <- xtabs(~Q18.9.a + GenderAssignedAtBirth, data = employ_datQ18.9GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.9GenderAssignedAtBirth$CatOutcome, employ_datQ18.9GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.9GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.9-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.9Sexuality<-multidatClean(Q18.9.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.9Sexuality$Sexuality<-factor(employ_datQ18.9Sexuality$Sexuality)
employ_datQ18.9Sexuality<- ordinaldatCleanNegative(employ_datQ18.9Sexuality$Q18.9.a,employ_datQ18.9Sexuality)
ordinal(employ_datQ18.9Sexuality$CatOutcome, employ_datQ18.9Sexuality$Sexuality, employ_datQ18.9Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.9-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.9<-multidatCleanWelcome(Q18.9.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.9.a + Survey, data = EdWelcome_datQ18.9)
conTable
detach(dat_long)
EdWelcome_datQ18.9$Q18.9.a[(EdWelcome_datQ18.9$Q18.9.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.9<- ordinaldatWelcomeClean(EdWelcome_datQ18.9, reverse=TRUE)
EdWelcome_datQ18.9$Survey<-factor(EdWelcome_datQ18.9$Survey)
EdWelcome_datQ18.9$Q18.9.a<-factor(EdWelcome_datQ18.9$Q18.9.a)
ordinal(EdWelcome_datQ18.9$Q18.9.a, EdWelcome_datQ18.9$Survey, EdWelcome_datQ18.9)

detach(data) #```
### Q18.10.a - I am confident that the University would listen and take action if I raised a concernVery strongly agree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.10-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10<-multidatClean(Q18.10.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.10.a + Academic, data = employ_datQ18.10)
detach(dat_long)
employ_datQ18.10<- ordinaldatClean(employ_datQ18.10$Q18.10.a,employ_datQ18.10)
ordinal(employ_datQ18.10$CatOutcome, employ_datQ18.10$Academic, employ_datQ18.10)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.10-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10College<-multidatClean(Q18.10.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.10.a + Q3, data = employ_datQ18.10College)
conTable
detach(dat_long)
employ_datQ18.10College$Q3[(employ_datQ18.10College$Q3 == "Research Professional Staff")]="Other"
employ_datQ18.10College$Q3<- factor(employ_datQ18.10College$Q3)
employ_datQ18.10College<- ordinaldatClean(employ_datQ18.10College$Q18.10.a,employ_datQ18.10College)
ordinal(employ_datQ18.10College$CatOutcome, employ_datQ18.10College$Q3, employ_datQ18.10College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.10-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10Carer<-multidatClean(Q18.10.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10Carer$Carer<- factor(employ_datQ18.10Carer$Carer)
employ_datQ18.10Carer<- ordinaldatClean(employ_datQ18.10Carer$Q18.10.a,employ_datQ18.10Carer)
ordinal(employ_datQ18.10Carer$CatOutcome, employ_datQ18.10Carer$Carer, employ_datQ18.10Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.10-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10Disability<-multidatClean(Q18.10.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10Disability$Disability<- factor(employ_datQ18.10Disability$Disability)
employ_datQ18.10Disability<-ordinaldatClean(employ_datQ18.10Disability$Q18.10.a,employ_datQ18.10Disability)
conTable <- xtabs(~Q18.10.a + Disability, data = employ_datQ18.10Disability)
conTable
ordinal(employ_datQ18.10Disability$CatOutcome, employ_datQ18.10Disability$Disability, employ_datQ18.10Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.10-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10Ethnicity<-multidatClean(Q18.10.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10Ethnicity$Ethnicity<- factor(employ_datQ18.10Ethnicity$EthnicityCleaned)
employ_datQ18.10Ethnicity<- ordinaldatClean(employ_datQ18.10Ethnicity$Q18.10.a,employ_datQ18.10Ethnicity)
conTable <- xtabs(~Q18.10.a + EthnicityCleaned, data = employ_datQ18.10Ethnicity)
conTable
ordinal(employ_datQ18.10Ethnicity$CatOutcome, employ_datQ18.10Ethnicity$EthnicityCleaned, employ_datQ18.10Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.10-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10FirstGen<-multidatClean(Q18.10.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10FirstGen$FirstGen<-factor(employ_datQ18.10FirstGen$FirstGen)
employ_datQ18.10FirstGen<- ordinaldatClean(employ_datQ18.10FirstGen$Q18.10.a,employ_datQ18.10FirstGen)
ordinal(employ_datQ18.10FirstGen$CatOutcome, employ_datQ18.10FirstGen$FirstGen, employ_datQ18.10FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.10-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10Gender<-multidatClean(Q18.10.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10Gender$Gender<-factor(employ_datQ18.10Gender$Gender)
employ_datQ18.10Gender<- ordinaldatClean(employ_datQ18.10Gender$Q18.10.a,employ_datQ18.10Gender)
ordinal(employ_datQ18.10Gender$CatOutcome, employ_datQ18.10Gender$Gender, employ_datQ18.10Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.10-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10GenderAssignedAtBirth<-multidatClean(Q18.10.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.10GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.10GenderAssignedAtBirth<- ordinaldatClean(employ_datQ18.10GenderAssignedAtBirth$Q18.10.a, employ_datQ18.10GenderAssignedAtBirth)
conTable <- xtabs(~Q18.10.a + GenderAssignedAtBirth, data = employ_datQ18.10GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.10GenderAssignedAtBirth$CatOutcome, employ_datQ18.10GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.10GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.10-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.10Sexuality<-multidatClean(Q18.10.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.10Sexuality$Sexuality<-factor(employ_datQ18.10Sexuality$Sexuality)
employ_datQ18.10Sexuality<-ordinaldatClean(employ_datQ18.10Sexuality$Q18.10.a,employ_datQ18.10Sexuality)
ordinal(employ_datQ18.10Sexuality$CatOutcome, employ_datQ18.10Sexuality$Sexuality, employ_datQ18.10Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.10-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.10<-multidatCleanWelcome(Q18.10.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.10.a + Survey, data = EdWelcome_datQ18.10)
conTable
detach(dat_long)
EdWelcome_datQ18.10$Q18.10.a[(EdWelcome_datQ18.10$Q18.10.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.10<- ordinaldatWelcomeClean (EdWelcome_datQ18.10, reverse=FALSE)
EdWelcome_datQ18.10$Survey<-factor(EdWelcome_datQ18.10$Survey)
EdWelcome_datQ18.10$Q18.10.a<-factor(EdWelcome_datQ18.10$Q18.10.a)
ordinal(EdWelcome_datQ18.10$Q18.10.a, EdWelcome_datQ18.10$Survey, EdWelcome_datQ18.10)

detach(data) #```
### Q18.11.a - The culture around research in my working environment supports my ability to do good quality research Very strongly agree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.11-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11<-multidatClean(Q18.11.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.11.a + Academic, data = employ_datQ18.11)
detach(dat_long)
employ_datQ18.11<- ordinaldatClean(employ_datQ18.11$Q18.11.a,employ_datQ18.11)
ordinal(employ_datQ18.11$CatOutcome, employ_datQ18.11$Academic, employ_datQ18.11)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.11-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11College<-multidatClean(Q18.11.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.11.a + Q3, data = employ_datQ18.11College)
conTable
detach(dat_long)
employ_datQ18.11College$Q3[(employ_datQ18.11College$Q3 == "Research Professional Staff"|employ_datQ18.11College$Q3 == "Other")]=NA
employ_datQ18.11College$Q3<- factor(employ_datQ18.11College$Q3)
employ_datQ18.11College<- ordinaldatClean(employ_datQ18.11College$Q18.11.a,employ_datQ18.11College)
ordinal(employ_datQ18.11College$CatOutcome, employ_datQ18.11College$Q3, employ_datQ18.11College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.11-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11Carer<-multidatClean(Q18.11.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11Carer$Carer<- factor(employ_datQ18.11Carer$Carer)
employ_datQ18.11Carer<- ordinaldatClean(employ_datQ18.11Carer$Q18.11.a,employ_datQ18.11Carer)
ordinal(employ_datQ18.11Carer$CatOutcome, employ_datQ18.11Carer$Carer, employ_datQ18.11Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.11-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11Disability<-multidatClean(Q18.11.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11Disability$Disability<- factor(employ_datQ18.11Disability$Disability)
employ_datQ18.11Disability<- ordinaldatClean(employ_datQ18.11Disability$Q18.11.a,employ_datQ18.11Disability)
conTable <- xtabs(~Q18.11.a + Disability, data = employ_datQ18.11Disability)
conTable
ordinal(employ_datQ18.11Disability$CatOutcome, employ_datQ18.11Disability$Disability, employ_datQ18.11Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.11-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11Ethnicity<-multidatClean(Q18.11.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11Ethnicity$Ethnicity<- factor(employ_datQ18.11Ethnicity$EthnicityCleaned)
employ_datQ18.11Ethnicity<- ordinaldatClean(employ_datQ18.11Ethnicity$Q18.11.a,employ_datQ18.11Ethnicity)
conTable <- xtabs(~Q18.11.a + EthnicityCleaned, data = employ_datQ18.11Ethnicity)
conTable
ordinal(employ_datQ18.11Ethnicity$CatOutcome, employ_datQ18.11Ethnicity$EthnicityCleaned, employ_datQ18.11Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.11-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11FirstGen<-multidatClean(Q18.11.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11FirstGen$FirstGen<-factor(employ_datQ18.11FirstGen$FirstGen)
employ_datQ18.11FirstGen<-ordinaldatClean(employ_datQ18.11FirstGen$Q18.11.a,employ_datQ18.11FirstGen)
ordinal(employ_datQ18.11FirstGen$CatOutcome, employ_datQ18.11FirstGen$FirstGen, employ_datQ18.11FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.11-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11Gender<-multidatClean(Q18.11.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11Gender$Gender<-factor(employ_datQ18.11Gender$Gender)
employ_datQ18.11Gender<- ordinaldatClean(employ_datQ18.11Gender$Q18.11.a,employ_datQ18.11Gender)
ordinal(employ_datQ18.11Gender$CatOutcome, employ_datQ18.11Gender$Gender, employ_datQ18.11Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.11-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11GenderAssignedAtBirth<-multidatClean(Q18.11.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.11GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.11GenderAssignedAtBirth<- ordinaldatClean(employ_datQ18.11GenderAssignedAtBirth$Q18.11,employ_datQ18.11GenderAssignedAtBirth)
conTable <- xtabs(~Q18.11.a + GenderAssignedAtBirth, data = employ_datQ18.11GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.11GenderAssignedAtBirth$CatOutcome, employ_datQ18.11GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.11GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.11-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.11Sexuality<-multidatClean(Q18.11.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.11Sexuality$Sexuality<-factor(employ_datQ18.11Sexuality$Sexuality)
employ_datQ18.11Sexuality<-  ordinaldatClean(employ_datQ18.11Sexuality$Q18.11,employ_datQ18.11Sexuality)
ordinal(employ_datQ18.11Sexuality$CatOutcome, employ_datQ18.11Sexuality$Sexuality, employ_datQ18.11Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.11-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.11<-multidatCleanWelcome(Q18.11.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.11.a + Survey, data = EdWelcome_datQ18.11)
conTable
detach(dat_long)
EdWelcome_datQ18.11$Q18.11.a[(EdWelcome_datQ18.11$Q18.11.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.11<- ordinaldatWelcomeClean (EdWelcome_datQ18.11, reverse=FALSE)
EdWelcome_datQ18.11$Survey<-factor(EdWelcome_datQ18.11$Survey)
EdWelcome_datQ18.11$Q18.11.a<-factor(EdWelcome_datQ18.11$Q18.11.a)
ordinal(EdWelcome_datQ18.11$Q18.11.a, EdWelcome_datQ18.11$Survey, EdWelcome_datQ18.11)

detach(data) #```
### Q18.12.a., The University's expectations of me to undertake a number of roles leaves me little time for research strongly disagree:7

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.12-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12<-multidatClean(Q18.12.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.12.a + Academic, data = employ_datQ18.12)
detach(dat_long)
employ_datQ18.12<- ordinaldatCleanNegative(employ_datQ18.12$Q18.12.a,employ_datQ18.12)
ordinal(employ_datQ18.12$CatOutcome, employ_datQ18.12$Academic, employ_datQ18.12)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.12-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12College<-multidatClean(Q18.12.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.12.a + Q3, data = employ_datQ18.12College)
conTable
detach(dat_long)
employ_datQ18.12College$Q3[(employ_datQ18.12College$Q3 == "Research Professional Staff" | employ_datQ18.12College$Q3 ==  "Other")]=NA
employ_datQ18.12College$Q3<- factor(employ_datQ18.12College$Q3)
employ_datQ18.12College<- ordinaldatCleanNegative(employ_datQ18.12College$Q18.12.a,employ_datQ18.12College)
ordinal(employ_datQ18.12College$CatOutcome, employ_datQ18.12College$Q3, employ_datQ18.12College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.12-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12Carer<-multidatClean(Q18.12.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12Carer$Carer<- factor(employ_datQ18.12Carer$Carer)
employ_datQ18.12Carer<- ordinaldatCleanNegative(employ_datQ18.12Carer$Q18.12.a,employ_datQ18.12Carer)
ordinal(employ_datQ18.12Carer$CatOutcome, employ_datQ18.12Carer$Carer, employ_datQ18.12Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.12-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12Disability<-multidatClean(Q18.12.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12Disability$Disability<- factor(employ_datQ18.12Disability$Disability)
employ_datQ18.12Disability<- ordinaldatCleanNegative(employ_datQ18.12Disability$Q18.12.a,employ_datQ18.12Disability)
conTable <- xtabs(~Q18.12.a + Disability, data = employ_datQ18.12Disability)
conTable
ordinal(employ_datQ18.12Disability$CatOutcome, employ_datQ18.12Disability$Disability, employ_datQ18.12Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.12-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12Ethnicity<-multidatClean(Q18.12.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12Ethnicity$Ethnicity<- factor(employ_datQ18.12Ethnicity$EthnicityCleaned)
employ_datQ18.12Ethnicity<- ordinaldatCleanNegative(employ_datQ18.12Ethnicity$Q18.12.a,employ_datQ18.12Ethnicity)
conTable <- xtabs(~Q18.12.a + EthnicityCleaned, data = employ_datQ18.12Ethnicity)
conTable
ordinal(employ_datQ18.12Ethnicity$CatOutcome, employ_datQ18.12Ethnicity$EthnicityCleaned, employ_datQ18.12Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.12-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12FirstGen<-multidatClean(Q18.12.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12FirstGen$FirstGen<-factor(employ_datQ18.12FirstGen$FirstGen)
employ_datQ18.12FirstGen<- ordinaldatCleanNegative(employ_datQ18.12FirstGen$Q18.12.a,employ_datQ18.12FirstGen)
ordinal(employ_datQ18.12FirstGen$CatOutcome, employ_datQ18.12FirstGen$FirstGen, employ_datQ18.12FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.12-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12Gender<-multidatClean(Q18.12.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12Gender$Gender<-factor(employ_datQ18.12Gender$Gender)
employ_datQ18.12Gender<- ordinaldatCleanNegative(employ_datQ18.12Gender$Q18.12.a,employ_datQ18.12Gender)
ordinal(employ_datQ18.12Gender$CatOutcome, employ_datQ18.12Gender$Gender, employ_datQ18.12Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.12-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12GenderAssignedAtBirth<-multidatClean(Q18.12.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.12GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.12GenderAssignedAtBirth<- ordinaldatCleanNegative(employ_datQ18.12GenderAssignedAtBirth$Q18.12.a,employ_datQ18.12GenderAssignedAtBirth)
conTable <- xtabs(~Q18.12.a + GenderAssignedAtBirth, data = employ_datQ18.12GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.12GenderAssignedAtBirth$CatOutcome, employ_datQ18.12GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.12GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.12-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.12Sexuality<-multidatClean(Q18.12.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.12Sexuality$Sexuality<-factor(employ_datQ18.12Sexuality$Sexuality)
employ_datQ18.12Sexuality<- ordinaldatCleanNegative(employ_datQ18.12Sexuality$Q18.12.a,employ_datQ18.12Sexuality)
ordinal(employ_datQ18.12Sexuality$CatOutcome, employ_datQ18.12Sexuality$Sexuality, employ_datQ18.12Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.12-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.12<-multidatCleanWelcome(Q18.12.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.12.a + Survey, data = EdWelcome_datQ18.12)
conTable
detach(dat_long)
EdWelcome_datQ18.12$Q18.12.a[(EdWelcome_datQ18.12$Q18.12.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.12<- ordinaldatWelcomeClean (EdWelcome_datQ18.12, reverse=TRUE)
EdWelcome_datQ18.12$Survey<-factor(EdWelcome_datQ18.12$Survey)
EdWelcome_datQ18.12$Q18.12.a<-factor(EdWelcome_datQ18.12$Q18.12.a)
ordinal(EdWelcome_datQ18.12$Q18.12.a, EdWelcome_datQ18.12$Survey, EdWelcome_datQ18.12)

detach(data) #```
### Q18.13.a The University provides me with support to navigate the grant application process-Very strongly agree:7  

#### Analysis for Edinburgh only data comparing the answer across Edinburgh Academic, Student, Other
#```r Question18.13-1, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13<-multidatClean(Q18.13.a, Academic, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.13.a + Academic, data = employ_datQ18.13)
detach(dat_long)
employ_datQ18.13<- ordinaldatClean (employ_datQ18.13$Q18.13.a,employ_datQ18.13)
ordinal(employ_datQ18.13$CatOutcome, employ_datQ18.13$Academic, employ_datQ18.13)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across College
#We had to remove Research Professional Staff and Other category data as there were not enough observations across the likert scales and model did not converge
#```r Question18.13-2, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13College<-multidatClean(Q18.13.a, Q3, UniqueResponseNumber, employ_dat)
conTable <- xtabs(~Q18.13.a + Q3, data = employ_datQ18.13College)
conTable
detach(dat_long)
employ_datQ18.13College$Q3[(employ_datQ18.13College$Q3 == "Research Professional Staff"| employ_datQ18.13College$Q3 == "Other")]=NA
employ_datQ18.13College$Q3<- factor(employ_datQ18.13College$Q3)
employ_datQ18.13College<- ordinaldatClean (employ_datQ18.13College$Q18.13.a,employ_datQ18.13College)
ordinal(employ_datQ18.13College$CatOutcome, employ_datQ18.13College$Q3, employ_datQ18.13College)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Carer- Yes/No
#```r Question18.13-3, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13Carer<-multidatClean(Q18.13.a, Carer, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13Carer$Carer<- factor(employ_datQ18.13Carer$Carer)
employ_datQ18.13Carer<- ordinaldatClean (employ_datQ18.13Carer$Q18.13.a,employ_datQ18.13Carer)
ordinal(employ_datQ18.13Carer$CatOutcome, employ_datQ18.13Carer$Carer, employ_datQ18.13Carer)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Disability
#```r Question18.13-4, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13Disability<-multidatClean(Q18.13.a, Disability, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13Disability$Disability<- factor(employ_datQ18.13Disability$Disability)
employ_datQ18.13Disability<- ordinaldatClean (employ_datQ18.13Disability$Q18.13.a,employ_datQ18.13Disability)
conTable <- xtabs(~Q18.13.a + Disability, data = employ_datQ18.13Disability)
conTable
ordinal(employ_datQ18.13Disability$CatOutcome, employ_datQ18.13Disability$Disability, employ_datQ18.13Disability)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Ethnicity 
#```r Question18.13-5, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13Ethnicity<-multidatClean(Q18.13.a, EthnicityCleaned, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13Ethnicity$Ethnicity<- factor(employ_datQ18.13Ethnicity$EthnicityCleaned)
employ_datQ18.13Ethnicity<- ordinaldatClean (employ_datQ18.13Ethnicity$Q18.13.a,employ_datQ18.13Ethnicity)
conTable <- xtabs(~Q18.13.a + EthnicityCleaned, data = employ_datQ18.13Ethnicity)
conTable
ordinal(employ_datQ18.13Ethnicity$CatOutcome, employ_datQ18.13Ethnicity$EthnicityCleaned, employ_datQ18.13Ethnicity)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across FirstGen
#```r Question18.13-6, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13FirstGen<-multidatClean(Q18.13.a, FirstGen, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13FirstGen$FirstGen<-factor(employ_datQ18.13FirstGen$FirstGen)
employ_datQ18.13FirstGen<- ordinaldatClean (employ_datQ18.13FirstGen$Q18.13.a,employ_datQ18.13FirstGen)
ordinal(employ_datQ18.13FirstGen$CatOutcome, employ_datQ18.13FirstGen$FirstGen, employ_datQ18.13FirstGen)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across Gender
#```r Question18.13-7, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13Gender<-multidatClean(Q18.13.a, Gender, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13Gender$Gender<-factor(employ_datQ18.13Gender$Gender)
employ_datQ18.13Gender<- ordinaldatClean (employ_datQ18.13Gender$Q18.13.a,employ_datQ18.13Gender)
ordinal(employ_datQ18.13Gender$CatOutcome, employ_datQ18.13Gender$Gender, employ_datQ18.13Gender)

detach(data) #```

#### Analysis for Edinburgh people comparing the answer across GenderAssignedBirth
#Not able to look as there are not many observations across the likert scales for No category- model does not converge
#```r Question18.13-8, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13GenderAssignedAtBirth<-multidatClean(Q18.13.a, GenderAssignedAtBirth, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13GenderAssignedAtBirth$GenderAssignedAtBirth<-factor(employ_datQ18.13GenderAssignedAtBirth$GenderAssignedAtBirth)
employ_datQ18.13GenderAssignedAtBirth<- ordinaldatClean (employ_datQ18.13GenderAssignedAtBirth$Q18.13.a,employ_datQ18.13GenderAssignedAtBirth)
conTable <- xtabs(~Q18.13.a + GenderAssignedAtBirth, data = employ_datQ18.13GenderAssignedAtBirth)
conTable
#ordinal(employ_datQ18.13GenderAssignedAtBirth$CatOutcome, employ_datQ18.13GenderAssignedAtBirth$GenderAssignedAtBirth, employ_datQ18.13GenderAssignedAtBirth)

#```

#### Analysis for Edinburgh people comparing the answer across Sexuality
#```r Question18.13-9, warning=FALSE, echo = FALSE, message=FALSE}
employ_datQ18.13Sexuality<-multidatClean(Q18.13.a, Sexuality, UniqueResponseNumber, employ_dat)
detach(dat_long)
employ_datQ18.13Sexuality$Sexuality<-factor(employ_datQ18.13Sexuality$Sexuality)
employ_datQ18.13Sexuality<- ordinaldatClean (employ_datQ18.13Sexuality$Q18.13.a,employ_datQ18.13Sexuality)
ordinal(employ_datQ18.13Sexuality$CatOutcome, employ_datQ18.13Sexuality$Sexuality, employ_datQ18.13Sexuality)

detach(data) #```

#### Analysis for Edinburgh vs Welcome Survey (AcademicUK)
#```r Question18.13-10, warning=FALSE, echo = FALSE, message=FALSE}
EdWelcome_datQ18.13<-multidatCleanWelcome(Q18.13.a, Survey, UniqueResponseNumber, EdWelcome_dat_Work)
conTable <- xtabs(~Q18.13.a + Survey, data = EdWelcome_datQ18.13)
conTable
detach(dat_long)
EdWelcome_datQ18.13$Q18.13.a[(EdWelcome_datQ18.13$Q18.13.a == "8")]=NA #Remove category 8- not applicable in welcome data
EdWelcome_datQ18.13<- ordinaldatWelcomeClean (EdWelcome_datQ18.13, reverse=FALSE)
EdWelcome_datQ18.13$Survey<-factor(EdWelcome_datQ18.13$Survey)
EdWelcome_datQ18.13$Q18.13.a<-factor(EdWelcome_datQ18.13$Q18.13.a)
ordinal(EdWelcome_datQ18.13$Q18.13.a, EdWelcome_datQ18.13$Survey, EdWelcome_datQ18.13)

detach(data) #```