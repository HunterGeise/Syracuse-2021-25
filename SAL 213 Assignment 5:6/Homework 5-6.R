library(tidyverse)
library(stargazer)
library(car)
library(ggplot2)
library(lmtest)
library(skedastic)
library(sandwich)

setwd("~/Desktop/SAL 213 R Folder/Assignment 5:6")

#Question 1 WNBA#
#b#
wnba <- read.csv("WNBA-Team-Data.csv")
view(wnba)
wnba_model1 <- lm(WIN.~ FG.Per + X3P.Per + DREB + TRB + STL + TOV, data = wnba)
summary(wnba_model1)
stargazer(wnba_model1, type = "html", out = "wnba.html")

#c#
wnba_vif <- vif(wnba_model1)
stargazer(wnba_vif, type = "html", out = "wnba_vif.html")

#Question 2 WNBA#
#a#
wnba_model2 <- lm(WIN. ~ Team_FG.Per + Team_3P.Per + Team_DREB + Team_TRB + Team_STL + Team_TOV,
                 data = wnba)
summary(wnba_model2)
stargazer(wnba_model2, type = "html", out = "wnba2.html")

#Question 4 2015 CFB#
#a#
cfb2015 <- read.csv("CFBAttendance2015.csv")
view(cfb2015)

cfb2015model <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                     Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                     Season_Win_Total, data=cfb2015)
summary(cfb2015model)
stargazer(cfb2015model, type = "html", out = "cfb2015.html")
#b#
cfbResiduals <- cfb2015model$residuals
cfbFitted <- cfb2015model$fitted.values
cfbtable <- data.frame(cfbFitted, 
                    cfbResiduals)
view(cfbtable)
ggplot(cfbtable, 
       aes(x = cfbFitted, 
           y = cfbResiduals ^ 2)) +
  geom_point(shape = 18, 
             color="blue") +
  geom_smooth(method = lm, 
              color="black", 
              se = FALSE) +
  labs(title = "2015 College Football Average Attendance", 
       x = "Fitted Average Attendance", 
       y = "Squared Residuals") +
  theme_classic()

#c#
bptest(cfb2015model)
White_Model <- lm(I(cfbResiduals ^ 2) ~ cfbFitted + I(cfbFitted ^ 2))
summary(White_Model)

#d#
cfb2015model2 <- lm(log(AVG_Attend)~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                     Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                     Season_Win_Total, data=cfb2015)

cfbResiduals2 <- cfb2015model2$residuals
cfbFitted2 <- cfb2015model2$fitted.values

bptest(cfb2015model2)
White_Model2 <- lm(I(cfbResiduals2 ^ 2) ~ cfbFitted2 + I(cfbFitted2 ^ 2))
summary(White_Model2)

#e#
robust_errors <- coeftest(cfb2015model,
         vcov = vcovHC(cfb2015model, type = "HC0"))
stargazer(robust_errors, type = "html", out = "robust_errors.html")

#Question 5 2011-19 CFB#
#a#
allcfb <- read.csv("CFBAttendanceAll.csv")
view(allcfb)

home_team = "Boston College"
acf(allcfb$AVG_Attend[allcfb$home_team == home_team], 
    plot = FALSE)
acf(allcfb$AVG_Attend[allcfb$home_team == home_team],
    plot = TRUE,
    main = "Autocorrelation by Lag")

accfb <- by(allcfb$AVG_Attend, 
         allcfb$home_team, 
         function(i) {acf(i, 
                          plot = FALSE, 
                          lag.max = 5)})
view(accfb)
accfb_team <- data.frame(t(sapply(accfb, "[[", 1))[, 2:6])
view(accfb_team)

#b#
allcfbmodel <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                      Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                      Season_Win_Total + home_conference + season, data=allcfb)
summary(allcfbmodel)
allcfb_bgtest <- bgtest(allcfbmodel, order = 3) #plm package

#c#
HAC_SE <- sqrt(diag(NeweyWest(allcfbmodel)))
stargazer(HAC_SE, type = "html", out = "HAC.html")

#d#
no_age_quadratic <- lm(AVG_Attend~ stadium_age + Recruiting_Strength + 
          Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
          Season_Win_Total + home_conference + season, data=allcfb)
age_quadratic <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
           Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
           Season_Win_Total + home_conference + season, data=allcfb)
anova_table <- anova(no_age_quadratic, age_quadratic)

#e#
no_conference <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                    Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                    Season_Win_Total  + season, data=allcfb)
conference <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                    Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                    Season_Win_Total + home_conference + season, data=allcfb)
conference_table <- anova(no_conference, conference)

no_season <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
                  Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
                  Season_Win_Total + home_conference, data=allcfb)
season <- lm(AVG_Attend~ stadium_age + I(stadium_age^2) + Recruiting_Strength + 
               Ranked_Opp + Nationally_Televised_Home_Games + Power5_School +
               Season_Win_Total + home_conference + season, data=allcfb)
season_table <- anova(no_season, season)
