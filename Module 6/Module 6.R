library(tidyverse)
library(stargazer)
library(car)
library(ggplot2)
library(lmtest)
library(skedastic)
library(sandwich)

setwd("~/Desktop/SAL 213 R Folder/Module 6")

#Module 4 Step 1#
weather_data <- read.csv("Mod 4 data.csv", stringsAsFactors = TRUE)
weather_data$Precipitation_mm
weather_model <- lm(Att. ~ Min_Temp + Max_Temp + Precipitation_mm + feels_like + 
              total_snow_cm + wind_speed_Kmph + wind_gusts_Kmph + humidity, data = weather_data)
summary(weather_model)
stargazer(weather_model, type = 'html', out = "weather model.html" )

nhlatt <- read.csv("Mod 4 data.csv")
view(nhlatt)
nhlatt$Start.Time..locally. <- relevel(factor(nhlatt$Start.Time..locally.), ref = '7:00 p.m.')
nhlatt$Day.of.Week <- relevel(factor(nhlatt$Day.of.Week), ref = 'Mon')
nhlatt_model <- lm(Att. ~ Day.of.Week + Start.Time..locally. + Away.quality + 
                   Home.quality + Game.Importance + Intra.Division + Canadien.Home.Team + 
                   Total_Star_Players + Home.quality:Canadien.Home.Team, data = nhlatt)
summary(nhlatt_model)
stargazer(nhlatt_model, type = "html", out = "hockey_model.html")


#Step 2#
weather_vif <- vif(weather_model)
stargazer(weather_vif, type = "html", out = "weather_vif.html")
nhlatt_vif <- vif(nhlatt_model)
stargazer(nhlatt_vif, type = "html", out = "nhlatt_vif.html")

no_interaction<- lm(Att. ~ Day.of.Week + Start.Time..locally. + Away.quality + 
                      Home.quality + Game.Importance + Intra.Division + Canadien.Home.Team + 
                      Total_Star_Players, data = nhlatt)
anova(no_interaction,nhlatt_mod)

interaction <- lm(Home.quality:Canadien.Home.Team ~ Day.of.Week + Start.Time..locally. + Away.quality + 
                    Home.quality + Game.Importance + Intra.Division + Canadien.Home.Team + 
                    Total_Star_Players, data = nhlatt )

#Step 3#
allnhlmodel <- lm(Att. ~ Precipitation_mm + total_snow_cm + 
                    Day.of.Week + Start.Time..locally. + Game.Importance + 
                    Intra.Division + Canadien.Home.Team + Total_Star_Players +
                    Home.quality:Canadien.Home.Team, data = nhlatt)
summary(allnhlmodel)
stargazer(allnhlmodel, type = "html", out = "allnhl.html")

#Step 4#
#a#
nhl2 <- lm(Att. ~ Precipitation_mm + total_snow_cm + I(total_snow_cm^2) + 
             Day.of.Week + Start.Time..locally. + Game.Importance + 
             Intra.Division + Canadien.Home.Team + Total_Star_Players + 
             Home.quality:Canadien.Home.Team , data = nhlatt)
summary(nhl2)
anova(allnhlmodel, nhl2)

#b#
allnhl_ssr <- sum(resid(allnhlmodel)^2)
nhl3 <- lm(log(Att.) ~ Precipitation_mm + total_snow_cm + 
             Day.of.Week + Start.Time..locally. + Game.Importance + 
             Intra.Division + Canadien.Home.Team + Total_Star_Players + 
             Home.quality:Canadien.Home.Team, data = nhlatt)
nhl3ssr <- sum(resid(nhl3)^2)


#Step 5#
nhl3 <- lm(log(Att.) ~ Precipitation_mm + total_snow_cm + 
             Day.of.Week + Start.Time..locally. + Game.Importance + 
             Intra.Division + Canadien.Home.Team + Total_Star_Players +
             Home.quality:Canadien.Home.Team, data = nhlatt)
summary(nhl3)
stargazer(nhl3, type = "html", out = "nhl3.html")

#Step 6#
bptest(nhl3)

#Step 7#
bgtest(nhl3)

#Step 8#
nhlHAC_errors <- coeftest(nhl3,
                          vcov = vcovHC(nhl3, type = "HC0"))
stargazer(nhl3, nhlHAC_errors, type = "html", out = "nhlHAC errors.html")

#Module 5 Step 1#
guardians_data <-read.csv("Module 5.csv")
view(guardians_data)
model1 <-lm(Num.W.L ~ as.factor(Home.Away) + as.factor(Opp.Division) + Days.Rested +
              IP + ER + SO + BF + Pit + aLI + RE24, data = guardians_data)
summary(model1)
stargazer(model1, type = "html", out = "guardians.html")

#Step 2#
guardian_vif <- vif(model1)
stargazer(guardian_vif, type = "html", out = "guardians_vif.html")

#Steep 3#
guardians2 <-lm(Num.W.L ~ as.factor(Home.Away) + as.factor(Opp.Division) + Days.Rested +
              IP + ER + SO + BF + Pit + aLI, data = guardians_data)
summary(guardians2)
stargazer(guardians2, type = "html", out = "guardian2.html")


#Step 4#
guardians3 <- lm(Num.W.L ~ as.factor(Player) + as.factor(Home.Away) + as.factor(Opp.Division) + Days.Rested +
                    IP + ER + SO + BF + Pit + aLI, data = guardians_data)
summary(guardians3)

anova(guardians2, guardians3)

guardians4 <- lm(log(Num.W.L) ~ as.factor(Home.Away) + as.factor(Opp.Division) + Days.Rested +
                   IP + ER + SO + BF + Pit + aLI, data = guardians_data)

#Step 6#
bptest(guardians2)

#Step 7#
bgtest(guardians2)

#Step 8#
robust_errors <- coeftest(guardians2,
                          vcov = vcovHC(guardians2, type = "HC0"))

stargazer(guardians2, robust_errors, type = "html", out = "gurdians_robust_errors.html")

