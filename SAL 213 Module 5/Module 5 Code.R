setwd("~/Desktop/SAL 213 R Folder/Module 5")
library(tidyverse)
library(stargazer)

guardians_data <-read.csv("Module 5.csv")
view(guardians_data)

model1 <-lm(Num.W.L ~ as.factor(Home.Away) + as.factor(Opp.Division) + Days.Rested +
            IP + ER + SO + BF + Pit + aLI + RE24, data = guardians_data)
summary(model1)
stargazer(model1, type = "html", out = "guardians.html")
