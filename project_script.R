# Library
library(dplyr)
library(tidyr)
library(ggplot2)


# Set wd
setwd("D:/R Directories/Coursera/Reproducible Research/Course Project 2")

# Read in data

if (!file.exists("Stormdata.csv.bz2")) {
      fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
      download.file(fileUrl, "Stormdata.csv.bz2")
      df <- read.csv("Stormdata.csv.bz2")
      invisible(rm(fileUrl))
} else {
      df <- read.csv("Stormdata.csv.bz2") 
}

# Get a sense of the data
dim(df)
names(df)

# Select the required data
dfs <- select(df, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, 
              CROPDMGEXP)

# Check the data
str(dfs)

# Check missing values
mv <- colMeans(!is.na(dfs), na.rm=FALSE)
mv <- data.frame(Variables = names(mv), Percentage_filled=mv)
mv <- mv[order(mv$Percentage_filled, decreasing = TRUE), ]
row.names(mv) <- 1:length(mv$Variables)
View(mv)

# Further check EXP variables
## Propdmg
table(dfs$PROPDMGEXP)
dfs$PROPDMGEXP <- as.character(dfs$PROPDMGEXP)


dfs$PROPDMGEXP[dfs$PROPDMGEXP == "1"] <- 10^1
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "2"] <- 10^2
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "3"] <- 10^3
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "4"] <- 10^4
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "5"] <- 10^5
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "6"] <- 10^6
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "7"] <- 10^7
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "8"] <- 10^8
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "B"] <- 10^9
dfs$PROPDMGEXP[dfs$PROPDMGEXP %in% c("h", "H")] <- 10^2
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "K"] <- 10^3
dfs$PROPDMGEXP[dfs$PROPDMGEXP %in% c("m", "M")] <- 10^6

dfs$PROPDMGEXP[dfs$PROPDMGEXP == "0"] <- 10^0
dfs$PROPDMGEXP[dfs$PROPDMGEXP == ""] <- 1
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "+"] <- 0
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "?"] <- 0
dfs$PROPDMGEXP[dfs$PROPDMGEXP == "-"] <- 0

dfs$PROPDMGEXP <- as.numeric(dfs$PROPDMGEXP)

## Cropdmg
table(dfs$CROPDMGEXP)
dfs$CROPDMGEXP <- as.character(dfs$CROPDMGEXP)

dfs$CROPDMGEXP[dfs$CROPDMGEXP=="2"] <- 10^2
dfs$CROPDMGEXP[dfs$CROPDMGEXP=="B"] <- 10^9
dfs$CROPDMGEXP[dfs$CROPDMGEXP %in% c("k", "K")] <- 10^3
dfs$CROPDMGEXP[dfs$CROPDMGEXP %in% c("m", "M")] <- 10^6

dfs$CROPDMGEXP[dfs$CROPDMGEXP=="0"] <- 10^0
dfs$CROPDMGEXP[dfs$CROPDMGEXP=="?"] <- 0
dfs$CROPDMGEXP[dfs$CROPDMGEXP==""] <- 0

dfs$CROPDMGEXP <- as.numeric(dfs$CROPDMGEXP)

# New variables

dfs %>%
      mutate(Property_damage = PROPDMG*PROPDMGEXP) %>%
      mutate(Crop_damage = CROPDMG*CROPDMGEXP) -> dfs

# ggplot df for first question
dfs_g <- select(dfs, EVTYPE, INJURIES, FATALITIES)

dfs_g %>% 
      mutate(TOTAL = INJURIES+FATALITIES) %>%
      group_by(EVTYPE) %>%
      summarise(Injuries = sum(INJURIES), Fatalities = sum(FATALITIES),
                Total = sum(TOTAL)) %>%
      arrange(desc(Total)) %>%
      slice(1:10) %>%
      gather(Population_harm, amount, -EVTYPE) -> dfs_g

# plot
g <- ggplot(dfs_g, aes(x=reorder(EVTYPE, -amount), y=amount, group=Population_harm))

g +
      geom_bar(stat="identity", aes(fill=Population_harm), position="dodge") +
      scale_y_sqrt() +
      labs(x="Event", y="Frequency count", title="Population health impact top 10 of storm events in the United States") +
      theme(axis.text.x = element_text(angle=45, hjust=1))

# ggplot df for second question
dfs_g <- select(dfs, EVTYPE, Property_damage, Crop_damage)

dfs_g %>% 
      mutate(Total = Property_damage + Crop_damage) %>%
      group_by(EVTYPE) %>%
      summarise(Property_damage = sum(Property_damage), Crop_damage = sum(Crop_damage),
                Total = sum(Total)) %>%
      arrange(desc(Total)) %>%
      slice(1:10) %>%
      gather(Damage_type, Amount, -EVTYPE) -> dfs_g

# plot
g <- ggplot(dfs_g, aes(x=reorder(EVTYPE, -Amount), y=Amount, group=Damage_type))

g +
      geom_bar(stat="identity", aes(fill=Damage_type), position="dodge") +
      scale_y_sqrt() +
      labs(x="Event", y="Damage (in Dollars)", title="Damage impact top 10 of storm events in the United States") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
      
