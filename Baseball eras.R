# Comparing the batting statistics of the 8 eras of baseball.


# Find Batting Averages, total home runs, average home runs per year, and average home runs per player for each era. 

library(dplyr)
library(ggplot2)

batting <- read.csv("batting.csv", header = TRUE)

# Create "batting average" column

# Create "Extra Base Hits" column 


batting$BA <- round(batting$H/batting$AB, digits = 3)


# Create an "era" column. 


batting$era <- ""
batting[batting$yearID %in% c(1871:1875), "era"] <- "Nascent"
batting[batting$yearID %in% c(1876:1900), "era"] <- "nineteenthcentury1"
batting[batting$yearID %in% c(1901:1919), "era"] <- "Deadball"
batting[batting$yearID %in% c(1920:1941), "era"] <- "Livelyball"
batting[batting$yearID %in% c(1942:1960), "era"] <- "Integration"
batting[batting$yearID %in% c(1961:1976), "era"] <- "Expansion"
batting[batting$yearID %in% c(1977:1993), "era"] <- "Free Agency"
batting[batting$yearID %in% c(1994:2005), "era"] <- "Longball"
batting[batting$yearID %in% c(2006:2015), "era"] <- "Modern"


# Create Era Batting Averages :

era_BA <- ddply(.data = batting, .variables = "era", .fun = function(df){round(sum(df$H, na.rm = T)/sum(df$AB, na.rm = T), digits = 3)})


## Batting Averages  

# Nascent era- .275
# Nineteenth Century  - .264
# Deadball Era  - .254
# Lively Ball Era  - .280
# Integration Era  - .259
# Expansion Era - .251 
# Free Agency Era- .260
# Longball Era - .267
# Modern Era - .259


#Create Bar Chart: 

ggplot(era_BA, aes(x = reorder(era, era, length), y = V1, label= V1, vjust = 2)) + coord_cartesian(ylim = c(.24,.285)) + geom_bar(stat = "Identity", color = "black", fill = "blue") + xlab("Eras") + ylab("Batting Averages") + geom_text()




#Create Era Home Runs

era_HR <- ddply(.data = batting, .variables = "era", .fun = summarize, HomeR = sum(HR, na.rm = T))


## Home Runs

# Nascent Era- 208 
# Nineteenth century - 8927
# Deadball Era - 7378
# Lively Ball Era - 26238 
# Integration Era- 33044
# Expansion Era - 43551
# Freeagency Era- 56743
# Longball Era - 59466
# Modern Era - 48118

#line chart- xaxis - years


year_HR <- ddply(.data = batting, .variables = "yearID", .fun = summarize, HomeR = sum(HR, na.rm = T))
ggplot(data = year_HR, aes(x = yearID, y = HomeR)) + geom_line() + geom_point() + ggtitle("Home Runs Per Year")





## Average HR per year 

ddply(.data = batting, .variables = "era", .fun = function(df){
  round(sum(df$HR, na.rm = T) / (max(df$yearID) - (min(df$yearID))), digits = 1)
})

# Nascent era- 52
# Nineteenth century- 372
# Deadball Era - 409.9
# Lively Ball Era- 1249.4
# Integration Era - 1835.8
# Expansion Era- 2903.4
# Free Agency Era - 3546.4
# Longball Era- 5406
# Modern Era - 5346.4

# Average HR per player 



era_HR_perplayer <- ddply(.data = batting, .variables = "era", .fun = function(df){
  round(sum(df$HR, na.rm = T) / (length(unique(df$playerID, na.rm = T))), digits = 1) 
  })


# Average HR per player

# Nascent era - 0.6 
# Nineteenthcentury - 4.41
# Deadball_era - 2.54
# Livelyball_era - 8.91
# Integration_era - 12.08 
# Expansion_era- 16.96
# Freeagency_era 16.65
# Longball_era- 17.68
# Modern_era - 14.19




