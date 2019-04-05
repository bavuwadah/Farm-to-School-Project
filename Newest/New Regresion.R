#####################################################
#                                                   #
# Farm to School Project System - OLS Regressions   #
#                                                   #
#                                                   #
#####################################################

library(readr)
library(devtools)
install_github("dgrtwo/broom")
library(broom)
library(plm)
library(sandwich)


# Read data
FTS.Data <- read.csv("Newest/FTS.Data2.csv")
View(FTS_Data2)

FTS.Data <- read.csv("/Users/Hanh/Desktop/Research/Farm to School Program/Newest/Entire Data.csv", header = T)
FTS.Data2 <- read.csv("/Users/Hanh/Desktop/Research/Farm to School Program/Newest/FTS.csv", header = T)
FTS.Data$date2 <- as.Date(FTS.Data$date, format="%m/%d/%Y")

####FTS start days for school
Finley <- ifelse(FTS.Data$site=="Finley" & FTS.Data$date2>="2015-10-26",1,0)
Foster <- ifelse(FTS.Data$site=="Foster" & FTS.Data$date2>="2015-10-14",1,0)
Glen <- ifelse(FTS.Data$site=="Glen" & FTS.Data$date2>="2015-11-2",1,0)
Idylwild <- ifelse(FTS.Data$site=="Idylwild" & FTS.Data$date2>="2015-10-14",1,0)
Lakeforest <- ifelse(FTS.Data$site=="Lake.forest" & FTS.Data$date2>="2015-10-14",1,0)
Littlewood <- ifelse(FTS.Data$site=="Littlewood" & FTS.Data$date2>="2015-10-14",1,0)
Meadowbrook <- ifelse(FTS.Data$site=="Meadowbrook" & FTS.Data$date2>="2015-12-18",1,0)
Metcalfe <- ifelse(FTS.Data$site=="Metcalfe" & FTS.Data$date2>="2015-12-2",1,0)
Newberry <- ifelse(FTS.Data$site=="Newberry" & FTS.Data$date2>="2015-12-2",1,0)
Norton <- ifelse(FTS.Data$site=="Norton " & FTS.Data$date2>="2015-10-14",1,0)
Rawlings <- ifelse(FTS.Data$site=="Rawlings" & FTS.Data$date2>="2015-12-14",1,0)
Terwilliger <- ifelse(FTS.Data$site=="Terwilliger" & FTS.Data$date2>="2015-10-14",1,0)
William <- ifelse(FTS.Data$site=="William" & FTS.Data$date2>="2015-12-2",1,0)


###Creating School and Product.source categorical variables
attach(FTS.Data)
school <- factor(site)
Product.source <- factor(Product.source)

attach(FTS.Data2)
School2 <- factor(site)
Product.source2 <- factor(Product.source)

library("multiwayvcov")
library("sandwich")
library(lmtest)

#####Regression analysis FTS impact on item selection with school-fixed effects
####Entire school year
FTS.fit <- lm(salad.ratio ~ FTS + Florida + FTS.Start + Mandarin + Chef + Fajita 
              + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop
              + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
              + Offered.Sandwich + Offered.Other + school, data=FTS.Data)

summary(FTS.fit)
vcov_school <- coeftest(FTS.fit, vcov.=vcovBK(FTS.fit, type="HC1"))
vcov_school <- cluster.vcov(FTS.fit, FTS.Data$site)
coeftest(FTS.fit, vcov_school)
FTS.fit1 <- coeftest(FTS.fit, df = Inf, vcov = vcovHC)
coeftest(FTS.fit, df = Inf, vcov = vcovHAC)


####Start Of FTS Program
FTS.fit2 <- lm(salad ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
               + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop 
               + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
               + Offered.Sandwich + Offered.Other + school, data=FTS.Data)

FTS.fit2 <- plm(salad ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
               + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop 
               + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
               + Offered.Sandwich + Offered.Other + school, data=FTS.Data,
               model = "within")

FTS.fit3 <- lm(salad ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
               + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop 
               + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
               + Offered.Sandwich + Offered.Other + school, data=FTS.Data)

summary(FTS.fit3)

vcovPC(FTS.fit3, cluster = ~ school + date2, pairwise = TRUE)
vcov_school <- coeftest(FTS.fit3, vcov.=vcovPC(FTS.fit3, cluster = ~ school + date2, pairwise = TRUE))
vcov_school <- coeftest(FTS.fit2, vcov.=vcovBK(FTS.fit2, type="HC1"))
vcov_school <- cluster.vcov(FTS.fit2, FTS.Data$site)
coeftest(FTS.fit2, vcov_school)
FTS.fit3 <- coeftest(FTS.fit2, df = Inf, vcov = vcovHC)
coeftest(FTS.fit2, df = Inf, vcov = vcovHAC)


#####Regression analysis FTS impact on NSLP participation with school-fixed effects
####Entire school year
NSLP.fit <- lm(Total.meals ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
                + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop 
                + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
                + Offered.Sandwich + Offered.Other + school, data=FTS.Data)
                
summary(NSLP.fit)
vcov_school <- cluster.vcov(NSLP.fit, FTS.Data$site)
coeftest(NSLP.fit, vcov_school)
NSLP.fit1 <- coeftest(NSLP.fit, df = Inf, vcov = vcovHC)
coeftest(NSLP.fit, df = Inf, vcov = vcovHAC)

####From start of FTS program
NSLP.fit2 <- lm(Total.meals ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
                + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop 
                + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza 
                + Offered.Sandwich + Offered.Other + School2, data=FTS.Data2)
summary(NSLP.fit2)
NSLP.fit3 <- coeftest(NSLP.fit2, df = Inf, vcov = vcovHC)
coeftest(NSLP.fit2, df = Inf, vcov = vcovHAC)


#####Regression analysis FTS impact on item selection with Race/ethnicity controls
####Entire school year
Pop.fit <- lm(salad.ratio ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita + Chicken.Tender 
              + Salad.LO + Chicken.Smacker + Chicken.Chop + Black + Asian + Hispanic + Other
              + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza  
              + Offered.Sandwich + Offered.Other, data=FTS.Data)

summary(Pop.fit)
vcov_school <- cluster.vcov(Pop.fit, FTS.Data$site)
coeftest(Pop.fit, vcov_school)
Pop.fit1 <- coeftest(Pop.fit, df = Inf, vcov = vcovHC)
coeftest(Pop.fit, df = Inf, vcov = vcovHAC)

####From start of FTS program
Pop.fit2 <- lm(salad ~ FTS + Florida + FTS.Start + Mandarin + Chef + Fajita + Chicken.Tender 
               + Salad.LO + Chicken.Smacker + Chicken.Chop + Black + Asian + Hispanic + Other
               + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza  
               + Offered.Sandwich + Offered.Other, data=FTS.Data)

summary(Pop.fit2)
vcov_school <- cluster.vcov(Pop.fit2, FTS.Data$site)
coeftest(Pop.fit2, vcov_school)
Pop.fit3 <- coeftest(Pop.fit2, df = Inf, vcov = vcovHC)
coeftest(Pop.fit2, df = Inf, vcov = vcovHAC)


#####Regression analysis FTS impact on item selection with Race/ethnicity controls
####Entire school year
TPop.fit <- lm(Total.meals ~ FTS + Florida + FTS.Start + Mandarin + Chef + Fajita + Chicken.Tender 
               + Salad.LO + Chicken.Smacker + Chicken.Chop + Black + Asian + Hispanic + Other
               + Offered.HB + Offered.Italian + Offered.Mexican + Offered.Pizza  
               + Offered.Sandwich + Offered.Other, data=FTS.Data)
summary(TPop.fit)
vcov_school <- cluster.vcov(TPop.fit, FTS.Data$site)
coeftest(TPop.fit, vcov_school)
TPop.fit1 <- coeftest(TPop.fit, df = Inf, vcov = vcovHC)
coeftest(TPop.fit, df = Inf, vcov = vcovHAC)


####From start of FTS program
TPop.fit2 <- lm(Total.meals ~ FTS + Florida + FTS.Start + Mandarin + Chef  + Fajita 
                + Chicken.Tender + Salad.LO + Chicken.Smacker + Chicken.Chop  + Black
                + Asian + Hispanic + Other + Offered.HB + Offered.Italian + Offered.Mexican
                + Offered.Pizza + Offered.Sandwich + Offered.Other, data=FTS.Data2)

summary(TPop.fit2)
TPop.fit3 <- coeftest(TPop.fit2, df = Inf, vcov = vcovHC)
coeftest(TPop.fit2, df = Inf, vcov = vcovHAC)

library(stargazer)
stargazer(FTS.fit1, FTS.fit3, Pop.fit1, Pop.fit3, title="Table 6: Predicted Impacts of Product Source on Item Selection", 
          type = "text", digits=3, out="modelFit1.txt")

stargazer(NSLP.fit1, NSLP.fit3, TPop.fit1, TPop.fit3, title="Table 6: Predicted Impacts of Product Source on Item Selection", 
          type = "text", digits=3, out="modelFit2.txt")
