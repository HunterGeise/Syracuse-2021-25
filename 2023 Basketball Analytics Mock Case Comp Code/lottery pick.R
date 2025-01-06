library(tidyverse)
library(nbastatR)
library(dplyr)
library(openxlsx)
library(randomForest)
library(modelr)
library(DescTools)
Sys.setenv(VROOM_CONNECTION_SIZE = "300000")

#Scraping and Cleaning Data#
data <- drafts(draft_years = 2000:2016)
lottery_picks <- data %>% 
  filter(numberPickOverall <= 14) %>%
  select(yearDraft, numberPickOverall, namePlayer,
         idPlayer, nameTeam)

bref_data <- players_careers(players = lottery_picks$namePlayer, 
                             modes = "Totals",
                             add_mode_names = TRUE)

#Merging data#
merged_data <- merge(lottery_picks, dataPlayerCareerTotalsRegularSeason, by = "namePlayer")
merged_data <- merged_data %>%
  select(-c(idTeam, idPlayer.x, slugSeasonType, urlNBAAPI, idPlayer.y, nameTeam))
  
merged_data <- merged_data %>%
  arrange(yearDraft, numberPickOverall)

write.xlsx(merged_data, file = "mock_comp.xlsx")

updated_data <- read.csv("mock_comp.csv")

updated_data <- updated_data %>%
  select(-c(pctFG, pctFG3, pctFT, pctFG2, trebTotals, fgmTotals, fgaTotals))

#Weights for data set using random forest#
rf_data <- updated_data %>%
  select(-namePlayer)
#splitting the data
ind <- sample(1:nrow(rf_data), nrow(rf_data) * .7, replace = TRUE)
train_data <- rf_data[ind, ]
test_data <- rf_data[-ind, ]

#optimiziing minimum node size
find_nodes1 <- function(node) {
  mod1 <- randomForest(
    WS ~ yearDraft + numberPickOverall + gp + gs + fg3mTotals + fg3aTotals + 
      fg2mTotals + fg2aTotals + minutesTotals + ftmTotals + ftaTotals + orebTotals +
      drebTotals + astTotals + stlTotals + blkTotals + tovTotals + pfTotals + ptsTotals + 
      allStar + allNBA + Champion, 
    data = train_data,
    nodesize = ceiling(node)
  )
  rmse(mod1, test_data)
}

min_node1 <- optimize(find_nodes1, c(1,14))
print(min_node1)

#optimizing tree number
find_trees1 <- function(trees) {
  mod2 <- randomForest(
    WS ~ yearDraft + numberPickOverall + gp + gs + fg3mTotals + fg3aTotals + 
      fg2mTotals + fg2aTotals + minutesTotals + ftmTotals + ftaTotals + orebTotals +
      drebTotals + astTotals + stlTotals + blkTotals + tovTotals + pfTotals + ptsTotals + 
      allStar + allNBA + Champion,
    data = train_data,
    ntree = ceiling(trees)
  )
  rmse(mod2, test_data)
}

opt_tree1 <- optimize(find_trees1, c(5,100))
print(opt_tree1)

#Final Models With Random Forest Params
final_model1 <- randomForest(WS ~ yearDraft + numberPickOverall + gp + gs + fg3mTotals + fg3aTotals + 
                               fg2mTotals + fg2aTotals + minutesTotals + ftmTotals + ftaTotals + orebTotals +
                               drebTotals + astTotals + stlTotals + blkTotals + tovTotals + pfTotals + ptsTotals + 
                               allStar + allNBA + Champion,
                             data = rf_data,
                             ntree = opt_tree1$minimum,
                             nodesize = min_node1$minimum)

#Creating weights and applying to data set
weights <- importance(final_model1)
normalized_weights <- weights/ sum(weights)

updated_data %>% 
  mutate(player_score = ((yearDraft * normalized_weights[1]) +
           (numberPickOverall * normalized_weights[2]) +
           (gp * normalized_weights[3]) +
           (gs * normalized_weights[4]) +
           (fg3mTotals * normalized_weights[5]) +
           (fg3aTotals * normalized_weights[6]) +
           (fg2mTotals * normalized_weights[7]) +
           (fg2aTotals * normalized_weights[8]) +
           (minutesTotals * normalized_weights[9]) +
           (ftmTotals * normalized_weights[10]) +
           (ftaTotals * normalized_weights[11]) +
           (orebTotals * normalized_weights[12]) +
           (drebTotals * normalized_weights[13]) +
           (astTotals * normalized_weights[14]) +
           (stlTotals * normalized_weights[15]) +
           (blkTotals * normalized_weights[16]) -
           (tovTotals * normalized_weights[17]) -
           (pfTotals * normalized_weights[18]) +
           (ptsTotals * normalized_weights[19]) +
           (allStar * normalized_weights[20]) +
           (allNBA * normalized_weights[21]) +
           (Champion * normalized_weights[22]))) -> updated_data

#Creating 5 quartiles for grading
column_to_split <- updated_data$player_score

quartiles <- quantile(column_to_split, probs = seq(0, 1, by = 1/5))

quartile_labels <- cut(column_to_split, breaks = quartiles, labels = FALSE, include.lowest = TRUE)

quartile_data <- updated_data %>%
  select(c(namePlayer, yearDraft, numberPickOverall, player_score))

quartile_data$quartile <- quartile_labels

#Assigning grades to quartiles
quartile_data %>%
  mutate(grade = case_when(
    quartile == 5 ~ "A",
    quartile == 4 ~ "B",
    quartile == 3 ~ "C",
    quartile == 2 ~ "D",
    quartile == 1 ~ "F",
  )) -> quartile_data

#2000 grading
data_2000 <- updated_data %>%
  filter(yearDraft == 2000)

column_to_split <- data_2000$player_score

quartiles <- quantile(column_to_split, probs = seq(0, 1, by = 1/12))

quartile_labels <- cut(column_to_split, breaks = quartiles, labels = FALSE, include.lowest = TRUE)

quartile_data <- data_2000 %>%
  select(c(namePlayer, yearDraft, numberPickOverall, player_score))

quartile_data$quartile <- quartile_labels

#Assigning grades to quartiles
quartile_data %>%
  mutate(grade = case_when(
    quartile == 12 ~ "A",
    quartile == 11 ~ "A-",
    quartile == 10 ~ "B+",
    quartile == 9 ~ "B",
    quartile == 8 ~ "B-",
    quartile == 7 ~ "C+",
    quartile == 6 ~ "C",
    quartile == 5 ~ "C-",
    quartile == 4 ~ "D+",
    quartile == 3 ~ "D",
    quartile == 2 ~ "D-",
    quartile == 1 ~ "F",
  )) -> quartile_data_2000

#2001_grading
data_2001 <- updated_data %>%
  filter(yearDraft == 2001)

column_to_split <- data_2001$player_score

quartiles <- quantile(column_to_split, probs = seq(0, 1, by = 1/12))

quartile_labels <- cut(column_to_split, breaks = quartiles, labels = FALSE, include.lowest = TRUE)

quartile_data <- data_2001 %>%
  select(c(namePlayer, yearDraft, numberPickOverall, player_score))

quartile_data$quartile <- quartile_labels

#Assigning grades to quartiles
quartile_data %>%
  mutate(grade = case_when(
    quartile == 12 ~ "A",
    quartile == 11 ~ "A-",
    quartile == 10 ~ "B+",
    quartile == 9 ~ "B",
    quartile == 8 ~ "B-",
    quartile == 7 ~ "C+",
    quartile == 6 ~ "C",
    quartile == 5 ~ "C-",
    quartile == 4 ~ "D+",
    quartile == 3 ~ "D",
    quartile == 2 ~ "D-",
    quartile == 1 ~ "F",
  )) -> quartile_data_2001

#Filtering by position#
