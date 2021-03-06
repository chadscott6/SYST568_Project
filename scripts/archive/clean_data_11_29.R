# SYST 568 Project
# Data Cleaning. Created by Chad Scott. Last updated 11/29/2020
  options(max.print = 10000)
# Install Lahman package
#  install.packages("Lahman")

# load libraries
  library(Lahman)
  library(data.table)
  library(dplyr)

# view all available data sets in the Lahman package  
  data( package = "Lahman")
  
# load in Teams data
  data("Teams")
  Teams <- data.table(Teams)
# explore Teams data
  str(Teams)
  head(Teams)  

# filter Teams to only include 1960 - 2019
  Teams <- Teams[yearID>1968,]
  summary(Teams)  
  head(Teams)

### Playoffs Variable ##########################################################################
# Teams that made the playoffs 
  #2012 - Present (not inlcuding 2020) 10 teams made the playoffs
  Teams[yearID==2019,]
  Teams[yearID==2019 & (DivWin=="Y" | WCWin == "Y"),]
  
  Teams[yearID==2012 & (DivWin=="Y" | WCWin == "Y"),1:40]
  # 1994 -2011 8 teams made the playoffs 1994 there was NO POST SEASON
  Teams[yearID==2011,1:40]
  Teams[yearID==2011 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  Teams[yearID==1995,1:40]
  Teams[yearID==1995 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  Teams[yearID==1994,]
  Teams[yearID==1994 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  # 1969 -1993 4 teams made the playoffs
  Teams[yearID==1993,]
  Teams[yearID==1993 & DivWin=="Y",]
  
  Teams[yearID==1969,1:40]
  Teams[yearID==1969 & DivWin=="Y",1:40]
  
# Creating playoffs variable
  Teams$Playoffs <- "N"
  Teams[yearID>1993 & (DivWin=="Y" | WCWin=="Y"), Playoffs:= "Y"]
  Teams[yearID<1994 & DivWin=="Y", Playoffs:="Y"]

# creating playoffs Lag variable
  setkey(Teams, teamID, yearID) # important for ordering
  Teams[,playoff_prevyear:=(shift(Playoffs, 1)),by=teamID]  
  Teams[teamID=='ATL',c(1:20,46:51)] 

  
#### Team Salaries ################################################################################
  data("Salaries")
  Salaries <- data.table(Salaries)
  str(Salaries)  
  head(Salaries)
  Salaries[,TeamSalary:=sum(salary), by = c("yearID","teamID")]
  Salaries <- unique(Salaries[,c("yearID","teamID","TeamSalary")])

# merge Salries with Teams data
  Teams <- merge(Teams, Salaries,  by = c("yearID","teamID"), all.x = TRUE)
  Teams 
  