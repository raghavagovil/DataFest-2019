#Packages
library(tidyverse)
library(data.table)
library(hexbin)
library(RColorBrewer)

#Reading Data
games <- read_csv("games.csv")
wellness <- read_csv("wellness.csv")
gps <- fread("gps.csv")
rpe <- read_csv("rpe.csv")

rpe[rpe$Training == "No","SessionType"] <- "Rest"
rpe[rpe$RPE %>% is.na(),6] <- 0

rpe2 <- rpe %>% group_by(Date,PlayerID) %>% 
  mutate(w.rpe = weighted.mean(x=RPE,w=Duration,na.rm = T))

rpefilter <- with(rpe,is.na(DailyLoad) & Training == "Yes")

rpe1 <- rpe[!rpefilter,]

rpe1[is.na(rpe1$DailyLoad),"DailyLoad"] <- 0

rpe.useful <- rpe1[,-c(4,12:14)]

#Getting Miles ran during game
gps.merge <- gps %>% group_by(PlayerID,GameID) %>% 
  summarise(avg.speed = mean(Speed),
            avg.aimpulse = mean(AccelImpulse),
            avg.aload = mean(AccelLoad))
games$bin.wl <- ifelse(games$Outcome == "W",1,0)


games.merge <- games %>% group_by(GameID,Date) %>% 
  summarise(total.points.allowed = sum(TeamPointsAllowed),
            total.team.points = sum(TeamPoints),
            wl.ratio = mean(bin.wl))


#Attached dates from games to gps data
games.gps <- full_join(games.merge,gps.merge,by = "GameID")

games.gps.merge <- games.gps %>% group_by(Date,PlayerID) %>% 
  summarise(total.points.allowed = sum(total.points.allowed),
            total.team.points = sum(total.team.points),
            wl.ratio = mean(wl.ratio),
            avg.speed = mean(avg.speed),
            avg.aload = mean(avg.aload),
            avg.aimpulse = mean(avg.aimpulse))

games.gps.rpe <- full_join(games.gps.merge,rpe.useful,by = c("Date","PlayerID"))

games.gps.rpe$DateC <- games.gps.rpe$Date 

games.gps.rpe$Date <- games.gps.rpe$DateC + 1

wellness.rpe <- full_join(wellness,games.gps.rpe,by = c("Date","PlayerID"))

wellness.rpe <- wellness.rpe %>% group_by(PlayerID) %>% 
  mutate(std.fatigue = (Fatigue-mean(Fatigue,na.rm = T))/sd(Fatigue,na.rm = T),
         std.soreness = (Soreness-mean(Soreness,na.rm = T))/sd(Soreness,na.rm = T),
         std.desire = (Desire-mean(Desire,na.rm = T))/sd(Desire,na.rm = T),
         std.irritability = (Irritability-mean(Irritability,na.rm = T))/sd(Irritability,na.rm = T),
         distance = 60*avg.speed*Duration)

wellness.rpe$w.rpe <- ifelse(wellness.rpe$Training == "No",0,wellness.rpe$w.rpe)

wellness.rpe$fatigue.bins <- ifelse(wellness.rpe$std.fatigue > 1,"H",ifelse(wellness.rpe$std.fatigue < -1,"L","M" )) 

wellness.rpe <- wellness.rpe %>% group_by(PlayerID) %>% 
  mutate(std.DailyLoad = (DailyLoad-mean(DailyLoad,na.rm = T))/sd(DailyLoad,na.rm = T),
         std.distance = (distance-mean(distance,na.rm = T))/sd(distance,na.rm = T),
         std.sleepquality = scale(SleepQuality),
         std.sleephours = scale(SleepHours),
         std.duration = scale(Duration),
         std.avg.speed = scale(avg.speed)
  )
wellness.rpe[!is.na(wellness.rpe$avg.speed),"gameyest"] <- "Yes" 
wellness.rpe[is.na(wellness.rpe$avg.speed),"gameyest"] <- "No"

wellness.rpe <- wellness.rpe[!wellness.rpe$PlayerID > 17,]


# VISUALIZATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyr)
library(dplyr)
library(sugrrants)
library(tsibble)
library(lubridate)

#You should have rpe.useful from the data.R file
calendar <- rpe.useful %>% 
  group_by(Date) %>% 
  summarise(ADL = mean(DailyLoad,na.rm = T))

#Filtering for tournament dates
commonwealth <- calendar[calendar$Date>=('2018-03-23') & calendar<=('2018-04-13'),]
kita <- calendar[calendar$Date>=('2018-03-31') & calendar$Date<=('2018-04-21'),]

#Calendar plot for Commonwealth tournament fatigue
cw <- commonwealth %>%
  frame_calendar(x = 1, y = 1, date = Date, calendar = "daily")

cwp <- cw %>%
  ggplot(aes(x = .x, y = .y)) +
  geom_tile(aes(fill = ADL), colour = "grey50") +
  scale_fill_gradient(high = "#FF0000",low = "#ffffff",name = "ADL") +
  ggtitle("Commonwealth Average Daily Load") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
cwp


#Calendar plot for Kitakyshu tournament fatigue
ka <- kita %>%
  frame_calendar(x = 1, y = 1, date = Date, calendar = "daily")

kap <- ka %>%
  ggplot(aes(x = .x, y = .y)) +
  geom_tile(aes(fill = ADL), colour = "grey50") +
  scale_fill_gradient(high = "#FF0000",low = "#ffffff") +
  ggtitle("Kitakyushu Average Daily Load") +
  guides(fill=guide_legend(title="ADL")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
kap

#Density Plot for Distance Run Vs Fatigue
g +
  stat_density_2d(aes(x=std.fatigue,y= std.distance,fill=..level.., alpha=..level..),
                  geom="polygon", size=0.01, bins=5) +
  geom_point(aes(x=std.fatigue,y= std.distance)) +
  scale_alpha(range=c(0.2, 0.4), guide=FALSE)+
  labs(x="Standardized Distance Run", y="Standardized Fatigue") +
  ggtitle("Distance Run vs Fatigue") +
  scale_fill_gradient(high = "#FF0000",low = "#000000") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())