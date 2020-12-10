# SYST 568 Project
# Data Cleaning. Created by Chad Scott. Last updated 12/01/2020 by Jonathan Nelson.
  #options(max.print = 10000)
# Install Lahman package
#  install.packages("Lahman")

# load libraries
  library(Lahman)
  library(data.table)
  library(dplyr)

# view all available data sets in the Lahman package  
  #data( package = "Lahman")
  
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

# Creating playoffs variable
  # creating DivWin and WCWin for 1994 season
  Teams$lg_div <-paste(Teams$lgID,Teams$divID)
  Teams$DivWin[Teams[yearID==1994,.I[W == max(W)],by=lg_div]$V1] <- 'Y'
  Teams$WCWin[Teams[yearID==1994 & is.na(DivWin),.I[W == max(W)],by=lgID]$V1] <- 'Y'
  Teams[yearID==1994,]
  
  Teams$Playoffs <- "N"
  Teams[yearID>1993 & (DivWin=="Y" | WCWin=="Y"), Playoffs:= "Y"]
  Teams[yearID<1994 & DivWin=="Y", Playoffs:="Y"]

# creating playoffs Lag variable
  setkey(Teams, teamID, yearID) # important for ordering
  Teams[,playoff_nextyear:=(shift(Playoffs, -1)),by=teamID]  
  Teams[teamID=='ATL',c(1:20,46:50)] 

  
### Team Salaries ############################################################
  data("Salaries")
  Salaries <- data.table(Salaries)
  str(Salaries)  
  head(Salaries)
  Salaries$salary <- as.numeric(Salaries$salary)
  Salaries[,TeamSalary:=sum(salary), by = c("yearID","teamID")]
  Salaries <- unique(Salaries[,c("yearID","teamID","TeamSalary")])
  Salaries[,TeamSalary:=TeamSalary/mean(TeamSalary), by = c("yearID")]
  Salaries
  
# add 2017 - 2019 salaries
  Sal_17_19 <- read.csv('../data/input/salaries2017-19.csv')
  setnames(Sal_17_19, "salary","TeamSalary")
  Salaries <- rbind(Salaries,Sal_17_19)
  
# merge Salaries with Teams data
  Teams_w_salary <- merge(Teams, Salaries,  by = c("yearID","teamID"), all.x = TRUE)
# filter to only years with team salary data
  Teams_w_salary <- Teams_w_salary[yearID>1984,]
  
### Standardizing STATS by games played and win percentage ##########################################
  # names(Teams_w_salary)[c(15:28,30:39)] # list of columns that are being standardized
  setDT(Teams_w_salary)[, paste0(names(Teams_w_salary)[c(15:28,30:39)],"_G") :=
                           lapply(.SD, '/', Teams_w_salary$G), .SDcols = c(15:28,30:39)]
  
  Teams_w_salary$W_pct <- (Teams_w_salary$W / Teams_w_salary$G)

### Features for modeling ##########################################

### Data with salary ### 

  final_teams_salary <- Teams_w_salary[,c(1,3:6,52:77,50:51)]

# write out to csv
  dir.create('data', showWarning=FALSE)
  #write.csv(final_teams, file.path('data', 'final_teams.csv'))
  write.csv(final_teams_salary, file.path('data', 'final_teams_salary.csv'))
