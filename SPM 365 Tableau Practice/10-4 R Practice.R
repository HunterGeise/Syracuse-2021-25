df_x = c(1,2,3)
df_y = c(10,20,30)
df <- data.frame(df_x , df_y)

View(df)
str(df)

df$df_x

df$df_z <- df_x * df_y

View(df)

colnames(df) <- c("x", "y", "z")

# Create dataframe with Celtics data

names <- c("JT", "JB", "MS")
height <- c( 80, 75, 75)
weight <- c(210,223,220)
ppg <- c(26.9,23.6,12.1)

celts<- data.frame(names,height,weight,ppg)
str(celts)

max(celts$ppg)
min(celts$height)

celts$names[celts$ppg==max(celts$ppg)]
celts$height[celts$names == "MS"]
