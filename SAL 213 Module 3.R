
#################### Module 3 Code Template ####################

########## Packages ##########
library(tidyverse)
library(lpSolve)

########## Part 0: Setting Up The Module ##########

team1 <- c("A", "B", "C", "D", "E", "F")
team2 <- c("A", "B", "C", "D", "E", "F")
size <- c("L", "L", "M", "M", "S", "S")
wc <- c(0, 1, 0, 1, 0, 1)

classes <- c("SS", "SM", "SL", 
             "MM", "ML", "LL")
b1 <- c(4000,  8000,  10000, 11000, 12500, 15000)
b4 <- c(5000,  9600,  11500, 13200, 14800, 16000)
b8 <- c(12000, 14000, 15000, 15500, 17000, 20000)

pre <- data.frame(matrix(data = c(1.5, 1.2, 1.4, 
                                  1,   1,   1,
                                  1,   1,   0.75,
                                  0.9, 0.9, 0.85,
                                  1,   1,   1), 
                         nrow = 5,
                         byrow = T),
                  row.names = c("W1", "W2", "W3", 
                                "W4", "W5"))
names(pre) <- c("one", "four", "eight")

matches <- crossing(team1, team2) %>%
  mutate(teamCom = ifelse(team1 < team2,
                          str_c(team1, team2),
                          str_c(team2, team1))) %>%
  filter(team1 != team2) %>%
  distinct(teamCom) 

matches <- matches %>%
  crossing(c("1pm","4pm","8pm")) %>% 
  mutate(team1 = substr(teamCom, 1, 1),
         team2 = substr(teamCom, 2, 2))

names(matches) = c("matchup","time","team1","team2")

matches <- matches %>%
  mutate(A = ifelse(team1 == "A" | team2 == "A", 1, 0),
         B = ifelse(team1 == "B" | team2 == "B", 1, 0),
         C = ifelse(team1 == "C" | team2 == "C", 1, 0),
         D = ifelse(team1 == "D" | team2 == "D", 1, 0),
         E = ifelse(team1 == "E" | team2 == "E", 1, 0),
         F = ifelse(team1 == "F" | team2 == "F", 1, 0),
         one = ifelse(time == "1pm", 1, 0),
         four = ifelse(time == "4pm", 1, 0),
         eight = ifelse(time == "8pm", 1, 0)
  )


########## Part 1: A Single Week ##########

########## Step 1 #####

views = function(slot, week, t1, t2){
  in1 <- team1 == t1
  in2 <- team2 == t2
  class <- paste(sort(c(size[in1], 
                        size[in2]), 
                      decreasing = T),
                 collapse = "")
  if (slot == "1pm") {
    b1[classes == class] * 
      pre[week, 1] * 
      ifelse(wc[in1] == 1 | wc[in2] == 1, 
             0.8, 1)
  } else if (slot == "4pm") {
    b4[classes == class] * pre[week, 2]
  } else {
    b8[classes == class] * pre[week, 3]
  }
}  

#Question: Which matchup greatest viewership in the 1pm timeslot? 
#Question: Which matchup greatest viewership in the 4pm timeslot? 
#Question: Which matchup greatest viewership in the 8pm timeslot? 

########## Step 2 #####

objective.arr   <- c(
  as.numeric(mapply(views, matches$time, week = 1, matches$team1, matches$team2))
)

constraint.mat  <- rbind(matrix(data = c(matches$A, #No team can play more than once
                                         matches$B, #in a given week 
                                         matches$C,
                                         matches$D,
                                         matches$E,
                                         matches$F),
                                nrow = 6,
                                byrow = T),
                         matrix(data = c(matches$one, #No more than one game can be
                                         matches$four, #played in a given time slot
                                         matches$eight),
                                nrow = 3,
                                byrow = T)
)

constraint.lim  <- c(rep(1, 6), rep(1, 3))
directional.arr <- c(rep("==", 6), rep("==", 3))

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) <- c(matches$matchup)
matches[linear.opt$solution > 0, 1:2]
objective.arr[linear.opt$solution == 1]

#Question: Which teams should play each other?
#Question: Total viewership?

########## Step 3 #####

#You will need to add the next constraint on your own! No prime time matchup can 
#consist of two small market teams. Update the constraint.mat, constraint.lim,
#and directional.arr objects to account for the new constraint. 
constraint.mat  <- rbind(matrix(data = c(matches$A, #No team can play more than once
                                         matches$B, #in a given week 
                                         matches$C,
                                         matches$D,
                                         matches$E,
                                         matches$F),
                                nrow = 6,
                                byrow = T),
                         matrix(data = c(matches$one, #No more than one game can be
                                         matches$four, #played in a given time slot
                                         matches$eight),
                                nrow = 3,
                                byrow = T),
                         matrix(data = c(rep(0,44),1), #2 small markets can't play on primetime #
                                nrow = 1,
                                byrow = T)
)

constraint.lim  <- c(rep(1, 6), rep(1, 3), 0)
directional.arr <- c(rep("==", 6), rep("==", 3), "==")

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) <- c(matches$matchup)
matches[linear.opt$solution > 0, 1:2]
objective.arr[linear.opt$solution == 1]

#Question: Which teams should play each other?
#Question: Total viewership?

########## Part 2: Expanding to Week 2 ##########

#Part 4#
objective.arr   <- c(
  as.numeric(mapply(views, matches$time, week = 1, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 2, matches$team1, matches$team2))
)


#I coded the first constraint for you, but you are responsible for expanding it 
#to cover the other constraints
constraint.mat  <- rbind(matrix(data = c(matches$A, rep(0, 45), #can't play more than once in a week#
                                         matches$B, rep(0, 45),
                                         matches$C, rep(0, 45),
                                         matches$D, rep(0, 45),
                                         matches$E, rep(0, 45),
                                         matches$F, rep(0, 45),
                                         rep(0, 45), matches$A, 
                                         rep(0, 45), matches$B,
                                         rep(0, 45), matches$C,
                                         rep(0, 45), matches$D,
                                         rep(0, 45), matches$E,
                                         rep(0, 45), matches$F),
                                nrow=12,
                                byrow = T),
                         matrix(data = c(matches$one, rep(0, 45), #can't be played in#
                                         matches$four, rep(0, 45), #more than one time slot#
                                         matches$eight, rep(0, 45),
                                         rep(0, 45), matches$one,
                                         rep(0, 45), matches$four,
                                         rep(0, 45), matches$eight),
                                nrow = 6,
                                byrow = T),
                         matrix(data = c(rep(0,44),1, #2 small markets can't play in primetime#
                                         rep(0,44),1),
                                nrow = 1,
                                byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$B == 1, 1,0),2), #no matchup#
                                       rep(ifelse(matches$A == 1 & matches$C == 1, 1,0),2), #more than once#
                                       rep(ifelse(matches$A == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$A == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$A == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$C == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$D == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$D == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$E == 1 & matches$F == 1, 1,0),2)),
                                nrow = 15,
                                byrow=T) 
)

constraint.lim  <- c(rep(1, 12), rep(1,6),0, rep(1,15))
directional.arr <- c(rep("==", 12), rep("==", 6), "==", rep("<=",15)) 

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) = c(paste(matches$matchup, 1), paste(matches$matchup, 2))
rbind(data.frame(week = 1, matches[linear.opt$solution[1:45] > 0,1:2]),
      data.frame(week = 2, matches[linear.opt$solution[46:90] > 0,1:2]))
objective.arr[linear.opt$solution == 1]

#Part 5#
objective.arr   <- c(
  as.numeric(mapply(views, matches$time, week = 1, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 2, matches$team1, matches$team2))
)

constraint.mat  <- rbind(matrix(data = c(matches$A, rep(0, 45), #can't play more than once in a week#
                                         matches$B, rep(0, 45),
                                         matches$C, rep(0, 45),
                                         matches$D, rep(0, 45),
                                         matches$E, rep(0, 45),
                                         matches$F, rep(0, 45),
                                         rep(0, 45), matches$A, 
                                         rep(0, 45), matches$B,
                                         rep(0, 45), matches$C,
                                         rep(0, 45), matches$D,
                                         rep(0, 45), matches$E,
                                         rep(0, 45), matches$F),
                                nrow=12,
                                byrow = T),
                         matrix(data = c(matches$one, rep(0, 45), #can't be played in#
                                         matches$four, rep(0, 45), #more than one time slot#
                                         matches$eight, rep(0, 45),
                                         rep(0, 45), matches$one,
                                         rep(0, 45), matches$four,
                                         rep(0, 45), matches$eight),
                                nrow = 6,
                                byrow = T),
                         matrix(data = c(rep(0,44),1, #2 small markets can't play in primetime#
                                         rep(0,44),1),
                                nrow = 1,
                                byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$B == 1, 1,0),2), #no matchup#
                                       rep(ifelse(matches$A == 1 & matches$C == 1, 1,0),2), #more than once#
                                       rep(ifelse(matches$A == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$A == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$A == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$C == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$B == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$D == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$C == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$D == 1 & matches$E == 1, 1,0),2),
                                       rep(ifelse(matches$D == 1 & matches$F == 1, 1,0),2),
                                       rep(ifelse(matches$E == 1 & matches$F == 1, 1,0),2)
                                       ),
                                nrow = 15,
                                byrow=T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$eight == 1, 1,0),2,), #no more#
                                       rep(ifelse(matches$B == 1 & matches$eight == 1, 1,0),2,), #than one#
                                       rep(ifelse(matches$C == 1 & matches$eight == 1, 1,0),2,), #prime time#
                                       rep(ifelse(matches$D == 1 & matches$eight == 1, 1,0),2,),
                                       rep(ifelse(matches$E == 1 & matches$eight == 1, 1,0),2,),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1, 1,0),2,)
                                       ),
                                nrow = 6,
                                byrow = T)
)

constraint.lim  <- c(rep(1, 12), rep(1,6),0, rep(1,15), rep(1,6))
directional.arr <- c(rep("==", 12), rep("==", 6), "==", rep("<=",15), rep("<=", 6)) 

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) = c(paste(matches$matchup, 1), paste(matches$matchup, 2))
rbind(data.frame(week = 1, matches[linear.opt$solution[1:45] > 0,1:2]),
      data.frame(week = 2, matches[linear.opt$solution[46:90] > 0,1:2]))
objective.arr[linear.opt$solution == 1]

#Part 6 revise step 5# 
objective.arr   <- c(
  as.numeric(mapply(views, matches$time, week = 1, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 2, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 3, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 4, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 5, matches$team1, matches$team2))
)

constraint.mat  <- rbind(matrix(data = c(matches$A, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #can't play#
                                         matches$B, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #more than#
                                         matches$C, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #once in a#
                                         matches$D, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #weeks#
                                         matches$E, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45),
                                         matches$F, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$A, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$B, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$C, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$D, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$E, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$F, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$A,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$B,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$C,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$D,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$E,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$F,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$A,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$B,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$C,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$D,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$E,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$F,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$A,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$B,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$C,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$D,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$E,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$F),
                                nrow=30,
                                byrow = T),
                         matrix(data = c(matches$one, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #can't#
                                         matches$four,rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #play in#
                                         matches$eight,rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45), #more than#
                                         rep(0, 45),matches$one,rep(0, 45),rep(0, 45),rep(0, 45), #one time#
                                         rep(0, 45),matches$four,rep(0, 45),rep(0, 45),rep(0, 45), #slot#
                                         rep(0, 45),matches$eight,rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$one,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$four,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$eight,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$one,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$four,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$eight,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$one,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$four,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$eight),
                                nrow = 15,
                                byrow = T,),
                         matrix(data = c(rep(0,44),1, #2 small markets can't play primetime#
                                         rep(0,44),1,
                                         rep(0,44),1,
                                         rep(0,44),1,
                                         rep(0,44),1),
                                nrow = 1,
                                byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$B == 1, 1,0),5), #no matchup more#
                                       rep(ifelse(matches$A == 1 & matches$C == 1, 1,0),5), #than once#
                                       rep(ifelse(matches$A == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$A == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$A == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$C == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$D == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$D == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$E == 1 & matches$F == 1, 1,0),5)),
                                nrow = 15,
                                byrow=T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$eight == 1, 1,0),5,), #more than#
                                       rep(ifelse(matches$B == 1 & matches$eight == 1, 1,0),5,), #one#
                                       rep(ifelse(matches$C == 1 & matches$eight == 1, 1,0),5,), #primetime#
                                       rep(ifelse(matches$D == 1 & matches$eight == 1, 1,0),5,), #less thqn 3# 
                                       rep(ifelse(matches$E == 1 & matches$eight == 1, 1,0),5,),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1, 1,0),5,)
                         ),
                         nrow = 6,
                         byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$eight == 1, 1,0),5,), #more than#
                                       rep(ifelse(matches$B == 1 & matches$eight == 1, 1,0),5,), #one#
                                       rep(ifelse(matches$C == 1 & matches$eight == 1, 1,0),5,), #primetime#
                                       rep(ifelse(matches$D == 1 & matches$eight == 1, 1,0),5,), #less thqn 3# 
                                       rep(ifelse(matches$E == 1 & matches$eight == 1, 1,0),5,),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1, 1,0),5,)
                         ),
                         nrow = 6,
                         byrow = T)
)

constraint.lim  <- c(rep(1, 30), rep(1,15),0, rep(1,15), rep(1,6), rep(3,6))
directional.arr <- c(rep("==", 30), rep("==", 15), "==", rep("<=", 15), rep(">=", 6), rep("<=",6))

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) = c(paste(matches$matchup, 1), paste(matches$matchup, 2), paste(matches$matchup,3),
                               paste(matches$matchup, 4), paste(matches$matchup, 5))
rbind(data.frame(week = 1, matches[linear.opt$solution[1:45] > 0,1:2]),
      data.frame(week = 2, matches[linear.opt$solution[46:90] > 0,1:2]),
      data.frame(week = 3, matches[linear.opt$solution[91:135] > 0,1:2]),
      data.frame(week = 4, matches[linear.opt$solution[136:180] > 0,1:2]),
      data.frame(week = 5, matches[linear.opt$solution[181:225] > 0,1:2]))
objective.arr[linear.opt$solution == 1]

#Part 7#
objective.arr   <- c(
  as.numeric(mapply(views, matches$time, week = 1, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 2, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 3, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 4, matches$team1, matches$team2)),
  as.numeric(mapply(views, matches$time, week = 5, matches$team1, matches$team2))
)

constraint.mat  <- rbind(matrix(data = c(matches$A, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #can't play#
                                         matches$B, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #more than#
                                         matches$C, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #once in a#
                                         matches$D, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #weeks#
                                         matches$E, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45),
                                         matches$F, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$A, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$B, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$C, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$D, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$E, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45), matches$F, rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$A,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$B,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$C,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$D,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$E,rep(0, 45),rep(0, 45),
                                         rep(0,45), rep(0, 45), matches$F,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$A,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$B,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$C,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$D,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$E,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$F,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$A,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$B,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$C,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$D,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$E,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$F),
                                nrow=30,
                                byrow = T),
                         matrix(data = c(matches$one, rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #can't#
                                         matches$four,rep(0, 45), rep(0, 45),rep(0, 45),rep(0, 45), #play in#
                                         matches$eight,rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45), #more than#
                                         rep(0, 45),matches$one,rep(0, 45),rep(0, 45),rep(0, 45), #one time#
                                         rep(0, 45),matches$four,rep(0, 45),rep(0, 45),rep(0, 45), #slot#
                                         rep(0, 45),matches$eight,rep(0, 45),rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$one,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$four,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),matches$eight,rep(0, 45),rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$one,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$four,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),matches$eight,rep(0, 45),
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$one,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$four,
                                         rep(0, 45),rep(0, 45),rep(0, 45),rep(0, 45),matches$eight),
                                nrow = 15,
                                byrow = T,),
                         matrix(data = c(rep(0,44),1, #2 small markets can't play primetime#
                                         rep(0,44),1,
                                         rep(0,44),1,
                                         rep(0,44),1,
                                         rep(0,44),1),
                                nrow = 1,
                                byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$B == 1, 1,0),5), #no matchup more#
                                       rep(ifelse(matches$A == 1 & matches$C == 1, 1,0),5), #than once#
                                       rep(ifelse(matches$A == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$A == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$A == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$C == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$B == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$D == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$C == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$D == 1 & matches$E == 1, 1,0),5),
                                       rep(ifelse(matches$D == 1 & matches$F == 1, 1,0),5),
                                       rep(ifelse(matches$E == 1 & matches$F == 1, 1,0),5)),
                                nrow = 15,
                                byrow=T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$eight == 1, 1,0),5,), #more than#
                                       rep(ifelse(matches$B == 1 & matches$eight == 1, 1,0),5,), #one#
                                       rep(ifelse(matches$C == 1 & matches$eight == 1, 1,0),5,), #primetime#
                                       rep(ifelse(matches$D == 1 & matches$eight == 1, 1,0),5,), #less thqn 3# 
                                       rep(ifelse(matches$E == 1 & matches$eight == 1, 1,0),5,),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1, 1,0),5,)
                         ),
                         nrow = 6,
                         byrow = T),
                         matrix(data=c(rep(ifelse(matches$A == 1 & matches$eight == 1, 1,0),5,), #more than#
                                       rep(ifelse(matches$B == 1 & matches$eight == 1, 1,0),5,), #one#
                                       rep(ifelse(matches$C == 1 & matches$eight == 1, 1,0),5,), #primetime#
                                       rep(ifelse(matches$D == 1 & matches$eight == 1, 1,0),5,), #less thqn 3# 
                                       rep(ifelse(matches$E == 1 & matches$eight == 1, 1,0),5,),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1, 1,0),5,)
                         ),
                         nrow = 6,
                         byrow = T),
                         matrix(data=c(rep(ifelse(matches$E == 1 & matches$eight == 1,1,0),5),
                                       rep(ifelse(matches$F == 1 & matches$eight == 1,1,0),5)),
                                nrow = 2,
                                byrow = T),
                         matrix(data=c(rep(ifelse(matches$E == 1 & matches$eight == 1 |
                                                   matches$F == 1 & matches$eight == 1,1,0),5)),
                                nrow = 1,
                                byrow = T),
                         matrix(data=c(ifelse(matches$A == 1 & matches$eight == 1 | 
                                                matches$B == 1 & matches$eight == 1,1,0),
                                       rep(0,180)),
                                nrow = 1,
                                byrow = T)
)

constraint.lim  <- c(rep(1, 30), rep(1,15),0, rep(1,15), rep(1,6), rep(3,6), rep(2,2), 3, 1, 0)
directional.arr <- c(rep("==", 30), rep("==", 15), "==", rep("<=", 15), rep(">=", 6), rep("<=",6), rep("<=",2), "<=", ">=", "==")

linear.opt <- lp(direction = "max",
                 objective.in = objective.arr,
                 const.mat = constraint.mat,
                 const.rhs = constraint.lim,
                 const.dir = directional.arr,
                 all.int = TRUE)

names(linear.opt$solution) = c(paste(matches$matchup, 1), paste(matches$matchup, 2), paste(matches$matchup,3),
                               paste(matches$matchup, 4), paste(matches$matchup, 5))

rbind(data.frame(week = 1, matches[linear.opt$solution[1:45] > 0,1:2]),
      data.frame(week = 2, matches[linear.opt$solution[46:90] > 0,1:2]),
      data.frame(week = 3, matches[linear.opt$solution[91:135] > 0,1:2]),
      data.frame(week = 4, matches[linear.opt$solution[136:180] > 0,1:2]),
      data.frame(week = 5, matches[linear.opt$solution[181:225] > 0,1:2]))
objective.arr[linear.opt$solution == 1]
