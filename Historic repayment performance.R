### Historical prepayment performance


#cleaning the environment
rm(list = ls()); cat("\014")

#setting directories
dd <- "D:/Drop analysis/Repayment/data"
od <- "D:/Drop analysis/Repayment/output"

# Load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape", "anytime")
# install.packages("plyr", dependencies = TRUE)

lapply(libs, require, character.only = T)

#############################################################################################
# Read data
d20 <- read.csv(paste(dd, "SC_2020_2020.07.08.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d20 <- subset(d20, d20$TotalCredit >0)
nrow(d20)
d19 <- read.csv(paste(dd, "SC_2019_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d19 <- subset(d19, d19$TotalCredit >0)
nrow(d19)
d18 <- read.csv(paste(dd, "SC_2018_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d18 <- subset(d18, d18$TotalCredit >0)
nrow(d18)
d17 <- read.csv(paste(dd, "SC_2017_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d17 <- subset(d17, d17$TotalCredit >0)
nrow(d17)

v20 <- read.csv(paste(dd, "V_2020_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v19 <- read.csv(paste(dd, "V_2019_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v18 <- read.csv(paste(dd, "V_2018_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v17 <- read.csv(paste(dd, "V_2017_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
#--------------------------------------------------------------------------
d20$Region.Name <- ifelse(d20$DistrictName=="Ruhango", "South", d20$RegionName)
d19$Region.Name <- ifelse(d19$DistrictName=="Ruhango", "South", d19$RegionName)
# Sort out 0 credit client
d20 <- subset(d20, d20$TotalCredit > 0)
nrow(d20)
d19 <- subset(d19, d19$TotalCredit >0)
nrow(d19)
d18 <- subset(d18, d18$TotalCredit >0)
nrow(d18)
d17 <- subset(d17, d17$TotalCredit >0)
nrow(d17)
#---------------------------------------------------------------
#To put dates in standard format,
#2020
class(v20$RepaymentDate)
v20$date <- substr(v20$RepaymentDate, 1, 10)
v20$date <- gsub(" ", "", v20$date)
v20$date <- gsub("-", "/", v20$date)
v20$date <- as.Date(v20$date)
class(v20$date)
#2019
class(v19$RepaymentDate)
v19$date <- substr(v19$RepaymentDate, 1, 10)
v19$date <- gsub(" ", "", v19$date)
v19$date <- gsub("-", "/", v19$date)
v19$date <- as.Date(v19$date)
class(v19$date)
#2018
class(v18$RepaymentDate)
v18$date <- substr(v18$RepaymentDate, 1, 10)
v18$date <- gsub(" ", "", v18$date)
v18$date <- gsub("-", "/", v18$date)
v18$date <- as.Date(v18$date)
class(v18$date)
#2017
class(v17$RepaymentDate)
v17$date <- substr(v17$RepaymentDate, 1, 10)
v17$date <- gsub(" ", "", v17$date)
v17$date <- gsub("-", "/", v17$date)
v17$date <- as.Date(v17$date)
class(v17$date)
#-------------------------------------------------------------
# Separate clients payments in monthly.
# Subset only payments form clients
# 2020
#table(v20$Type)
#v201 <- subset(v20, v20$Type=="MobileMoney" | v20$Type=="Receipt")
#table(v201$Type)
#nrow(v201)
#------------------------------------------------------------------
#repayment monthly performance on Sites and district level 
# subset monthly payment
Aug_20 <-subset(v20, v20$date <= "2019-08-31")
Aug_20.pymt <- Aug_20%>% group_by(GlobalClientID)%>% summarise(Aug_20.pymt=sum(Amount))

Sept_20 <-subset(v20,v20$date <= "2019-09-30")
Sept_20.pymt <- Sept_20%>% group_by(GlobalClientID)%>% summarise(Sept_20.pymt=sum(Amount))

Octor_20 <-subset(v20,v20$date <= "2019-10-31")
Octor_20.pymt <- Octor_20%>% group_by(GlobalClientID)%>% summarise(Octor_20.pymt=sum(Amount))

Nov_20 <-subset(v20,v20$date <= "2019-11-30")
Nov_20.pymt <- Nov_20%>% group_by(GlobalClientID)%>% summarise(Nov_20.pymt=sum(Amount))

Dec_20 <-subset(v20,v20$date <= "2019-12-31")
Dec_20.pymt <- Dec_20%>% group_by(GlobalClientID)%>% summarise(Dec_20.pymt=sum(Amount))

Jan_20 <-subset(v20,v20$date <= "2020-01-31")
Jan_20.pymt <- Jan_20%>% group_by(GlobalClientID)%>% summarise(Jan_20.pymt=sum(Amount))

Feb_20 <-subset(v20,v20$date <= "2020-02-29")
Feb_20.pymt <-Feb_20%>% group_by(GlobalClientID)%>% summarise(Feb_20.pymt=sum(Amount))

Mar_20 <-subset(v20,v20$date <= "2020-03-31")
Mar_20.pymt <- Mar_20%>% group_by(GlobalClientID)%>% summarise(Mar_20.pymt=sum(Amount))

Apr_20 <-subset(v20,v20$date <= "2020-04-30")
Apr_20.pymt <- Apr_20%>% group_by(GlobalClientID)%>% summarise(Apr_20.pymt=sum(Amount))

May_20 <-subset(v20,v20$date <= "2020-05-31")
May_20.pymt <- May_20%>% group_by(GlobalClientID)%>% summarise(May_20.pymt=sum(Amount))

Jun_20 <-subset(v20,v20$date <= "2020-06-30")
Jun_20.pymt <- Jun_20%>% group_by(GlobalClientID)%>% summarise(Jun_20.pymt=sum(Amount))

Jul_20 <-subset(v20,v20$date <= "2020-07-20")
Jul_20.pymt <- Jul_20%>% group_by(GlobalClientID)%>% summarise(Jul_20.pymt=sum(Amount))

#create columns in the SC for each month and put inside amount paid by each farmer

d20$Aug_20.pymt  <- Aug_20.pymt$Aug_20.pymt[match(d20$GlobalClientID,Aug_20.pymt$GlobalClientID)]
d20$Sept_20.pymt <-Sept_20.pymt$Sept_20.pymt[match(d20$GlobalClientID,Sept_20.pymt$GlobalClientID)]
d20$Oct_20.pymt  <- Octor_20.pymt$Octor_20.pymt[match(d20$GlobalClientID,Octor_20.pymt$GlobalClientID)]
d20$Nov_20.pymt  <- Nov_20.pymt$Nov_20.pymt[match(d20$GlobalClientID,Nov_20.pymt$GlobalClientID)]
d20$Dec_20.pymt  <- Dec_20.pymt$Dec_20.pymt[match(d20$GlobalClientID,Dec_20.pymt$GlobalClientID)]
d20$Jan_20.pymt  <- Jan_20.pymt$Jan_20.pymt[match(d20$GlobalClientID,Jan_20.pymt$GlobalClientID)]
d20$Feb_20.pymt  <- Feb_20.pymt$Feb_20.pymt[match(d20$GlobalClientID,Feb_20.pymt$GlobalClientID)]
d20$Mar_20.pymt  <- Mar_20.pymt$Mar_20.pymt[match(d20$GlobalClientID,Mar_20.pymt$GlobalClientID)]
d20$Apr_20.pymt  <- Apr_20.pymt$Apr_20.pymt[match(d20$GlobalClientID,Apr_20.pymt$GlobalClientID)]
d20$May_20.pymt  <- May_20.pymt$May_20.pymt[match(d20$GlobalClientID,May_20.pymt$GlobalClientID)]
d20$Jun_20.pymt  <- Jun_20.pymt$Jun_20.pymt[match(d20$GlobalClientID,Jun_20.pymt$GlobalClientID)]
d20$Jul_20.pymt  <- Jul_20.pymt$Jul_20.pymt[match(d20$GlobalClientID,Jul_20.pymt$GlobalClientID)]

d20[is.na(d20)] <- 0
#Check if it works
names(d20)
d20$clt.made.paymt <- ifelse((d20$Aug_20.pymt >0
                              |d20$Sept_20.pymt >0
                              |d20$Oct_20.pymt >0
                              |d20$Nov_20.pymt >0
                              |d20$Dec_20.pymt >0
                              |d20$Jan_20.pymt >0                     
                              |d20$Feb_20.pymt >0                 
                              |d20$Mar_20.pymt >0                 
                              |d20$Apr_20.pymt >0                 
                              |d20$May_20.pymt >0                 
                              |d20$Jun_20.pymt >0                 
                              |d20$Jul_20.pymt >0),1,0)
table(d20$clt.made.paymt, useNA = "ifany")

# 2019
Aug_19 <-subset(v19, v19$date <= "2018-08-31")
Aug_19.pymt <- Aug_19%>% group_by(GlobalClientID)%>% summarise(Aug_19.pymt=sum(Amount))

Sept_19 <-subset(v19,v19$date <= "2018-09-30")
Sept_19.pymt <- Sept_19%>% group_by(GlobalClientID)%>% summarise(Sept_19.pymt=sum(Amount))

Octor_19 <-subset(v19,v19$date <= "2018-10-31")
Octor_19.pymt <- Octor_19%>% group_by(GlobalClientID)%>% summarise(Octor_19.pymt=sum(Amount))

Nov_19 <-subset(v19,v19$date <= "2018-11-30")
Nov_19.pymt <- Nov_19%>% group_by(GlobalClientID)%>% summarise(Nov_19.pymt=sum(Amount))

Dec_19 <-subset(v19,v19$date <= "2018-12-31")
Dec_19.pymt <- Dec_19%>% group_by(GlobalClientID)%>% summarise(Dec_19.pymt=sum(Amount))

Jan_19 <-subset(v19,v19$date <= "2019-01-31")
Jan_19.pymt <- Jan_19%>% group_by(GlobalClientID)%>% summarise(Jan_19.pymt=sum(Amount))

Feb_19 <-subset(v19,v19$date <= "2019-02-28")
Feb_19.pymt <-Feb_19%>% group_by(GlobalClientID)%>% summarise(Feb_19.pymt=sum(Amount))

Mar_19 <-subset(v19,v19$date <= "2019-03-31")
Mar_19.pymt <- Mar_19%>% group_by(GlobalClientID)%>% summarise(Mar_19.pymt=sum(Amount))

Apr_19 <-subset(v19,v19$date <= "2019-04-30")
Apr_19.pymt <- Apr_19%>% group_by(GlobalClientID)%>% summarise(Apr_19.pymt=sum(Amount))

May_19 <-subset(v19,v19$date <= "2019-05-31")
May_19.pymt <- May_19%>% group_by(GlobalClientID)%>% summarise(May_19.pymt=sum(Amount))

Jun_19 <-subset(v19,v19$date <= "2019-06-30")
Jun_19.pymt <- Jun_19%>% group_by(GlobalClientID)%>% summarise(Jun_19.pymt=sum(Amount))

Jul_19 <-subset(v19,v19$date <= "2019-07-20")
Jul_19.pymt <- Jul_19%>% group_by(GlobalClientID)%>% summarise(Jul_19.pymt=sum(Amount))

# #create columns in the SC for each month and put inside amount paid by each farmer
d19$Aug_19.pymt <- Aug_19.pymt$Aug_19.pymt[match(d19$GlobalClientID,Aug_19.pymt$GlobalClientID)]
d19$Sept_19.pymt <- Sept_19.pymt$Sept_19.pymt[match(d19$GlobalClientID,Sept_19.pymt$GlobalClientID)]
d19$Oct_19.pymt <- Octor_19.pymt$Octor_19.pymt[match(d19$GlobalClientID,Octor_19.pymt$GlobalClientID)]
d19$Nov_19.pymt <- Nov_19.pymt$Nov_19.pymt[match(d19$GlobalClientID,Nov_19.pymt$GlobalClientID)]
d19$Dec_19.pymt <- Dec_19.pymt$Dec_19.pymt[match(d19$GlobalClientID,Dec_19.pymt$GlobalClientID)]
d19$Jan_19.pymt <- Jan_19.pymt$Jan_19.pymt[match(d19$GlobalClientID,Jan_19.pymt$GlobalClientID)]
d19$Feb_19.pymt <- Feb_19.pymt$Feb_19.pymt[match(d19$GlobalClientID,Feb_19.pymt$GlobalClientID)]
d19$Mar_19.pymt <- Mar_19.pymt$Mar_19.pymt[match(d19$GlobalClientID,Mar_19.pymt$GlobalClientID)]
d19$Apr_19.pymt <- Apr_19.pymt$Apr_19.pymt[match(d19$GlobalClientID,Apr_19.pymt$GlobalClientID)]
d19$May_19.pymt <- May_19.pymt$May_19.pymt[match(d19$GlobalClientID,May_19.pymt$GlobalClientID)]
d19$Jun_19.pymt <- Jun_19.pymt$Jun_19.pymt[match(d19$GlobalClientID,Jun_19.pymt$GlobalClientID)]
d19$Jul_19.pymt <- Jul_19.pymt$Jul_19.pymt[match(d19$GlobalClientID,Jul_19.pymt$GlobalClientID)]

d19[is.na(d19)] <- 0
#Check if it works
names(d19)
d19$clt.made.paymt <- ifelse((d19$Aug_19.pymt >0
                              |d19$Sept_19.pymt >0
                              |d19$Oct_19.pymt >0
                              |d19$Nov_19.pymt >0
                              |d19$Dec_19.pymt >0
                              |d19$Jan_19.pymt >0                     
                              |d19$Feb_19.pymt >0                 
                              |d19$Mar_19.pymt >0                 
                              |d19$Apr_19.pymt >0                 
                              |d19$May_19.pymt >0                 
                              |d19$Jun_19.pymt >0                 
                              |d19$Jul_19.pymt >0),1,0)
table(d19$clt.made.paymt, useNA = "ifany")

#2018
Aug_18 <-subset(v18, v18$date <= "2017-08-31")
Aug_18.pymt <- Aug_18%>% group_by(GlobalClientID)%>% summarise(Aug_18.pymt=sum(Amount))

Sept_18 <-subset(v18,v18$date <= "2017-09-30")
Sept_18.pymt <- Sept_18%>% group_by(GlobalClientID)%>% summarise(Sept_18.pymt=sum(Amount))

Octor_18 <-subset(v18,v18$date <= "2017-10-31")
Octor_18.pymt <- Octor_18%>% group_by(GlobalClientID)%>% summarise(Octor_18.pymt=sum(Amount))

Nov_18 <-subset(v18,v18$date <= "2017-11-30")
Nov_18.pymt <- Nov_18%>% group_by(GlobalClientID)%>% summarise(Nov_18.pymt=sum(Amount))

Dec_18 <-subset(v18,v18$date <= "2017-12-31")
Dec_18.pymt <- Dec_18%>% group_by(GlobalClientID)%>% summarise(Dec_18.pymt=sum(Amount))

Jan_18 <-subset(v18,v18$date <= "2018-01-31")
Jan_18.pymt <- Jan_18%>% group_by(GlobalClientID)%>% summarise(Jan_18.pymt=sum(Amount))

Feb_18 <-subset(v18,v18$date <= "2018-02-28")
Feb_18.pymt <-Feb_18%>% group_by(GlobalClientID)%>% summarise(Feb_18.pymt=sum(Amount))

Mar_18 <-subset(v18,v18$date <= "2018-03-31")
Mar_18.pymt <- Mar_18%>% group_by(GlobalClientID)%>% summarise(Mar_18.pymt=sum(Amount))

Apr_18 <-subset(v18,v18$date <= "2018-04-30")
Apr_18.pymt <- Apr_18%>% group_by(GlobalClientID)%>% summarise(Apr_18.pymt=sum(Amount))

May_18 <-subset(v18,v18$date <= "2018-05-31")
May_18.pymt <- May_18%>% group_by(GlobalClientID)%>% summarise(May_18.pymt=sum(Amount))

Jun_18 <-subset(v18,v18$date <= "2018-06-30")
Jun_18.pymt <- Jun_18%>% group_by(GlobalClientID)%>% summarise(Jun_18.pymt=sum(Amount))

Jul_18 <-subset(v18,v18$date <= "2018-07-20")
Jul_18.pymt <- Jul_18%>% group_by(GlobalClientID)%>% summarise(Jul_18.pymt=sum(Amount))

#create columns in the SC for each month and put inside amount paid by each farmer
d18$Aug_18.pymt <- Aug_18.pymt$Aug_18.pymt[match(d18$GlobalClientID,Aug_18.pymt$GlobalClientID)]
d18$Sept_18.pymt <- Sept_18.pymt$Sept_18.pymt[match(d18$GlobalClientID,Sept_18.pymt$GlobalClientID)]
d18$Oct_18.pymt <- Octor_18.pymt$Octor_18.pymt[match(d18$GlobalClientID,Octor_18.pymt$GlobalClientID)]
d18$Nov_18.pymt <- Nov_18.pymt$Nov_18.pymt[match(d18$GlobalClientID,Nov_18.pymt$GlobalClientID)]
d18$Dec_18.pymt <- Dec_18.pymt$Dec_18.pymt[match(d18$GlobalClientID,Dec_18.pymt$GlobalClientID)]
d18$Jan_18.pymt <- Jan_18.pymt$Jan_18.pymt[match(d18$GlobalClientID,Jan_18.pymt$GlobalClientID)]
d18$Feb_18.pymt <- Feb_18.pymt$Feb_18.pymt[match(d18$GlobalClientID,Feb_18.pymt$GlobalClientID)]
d18$Mar_18.pymt <- Mar_18.pymt$Mar_18.pymt[match(d18$GlobalClientID,Mar_18.pymt$GlobalClientID)]
d18$Apr_18.pymt <- Apr_18.pymt$Apr_18.pymt[match(d18$GlobalClientID,Apr_18.pymt$GlobalClientID)]
d18$May_18.pymt <- May_18.pymt$May_18.pymt[match(d18$GlobalClientID,May_18.pymt$GlobalClientID)]
d18$Jun_18.pymt <- Jun_18.pymt$Jun_18.pymt[match(d18$GlobalClientID,Jun_18.pymt$GlobalClientID)]
d18$Jul_18.pymt <- Jul_18.pymt$Jul_18.pymt[match(d18$GlobalClientID,Jul_18.pymt$GlobalClientID)]

d18[is.na(d18)] <- 0
#Check if it works
names(d18)
d18$clt.made.paymt <- ifelse((d18$Aug_18.pymt >0
                              |d18$Sept_18.pymt >0
                              |d18$Oct_18.pymt >0
                              |d18$Nov_18.pymt >0
                              |d18$Dec_18.pymt >0
                              |d18$Jan_18.pymt >0                     
                              |d18$Feb_18.pymt >0                 
                              |d18$Mar_18.pymt >0                 
                              |d18$Apr_18.pymt >0                 
                              |d18$May_18.pymt >0                 
                              |d18$Jun_18.pymt >0                 
                              |d18$Jul_18.pymt >0),1,0)
table(d18$clt.made.paymt, useNA = "ifany")

#2017
Aug_17 <-subset(v17, v17$date <= "2016-08-31")
Aug_17.pymt <- Aug_17%>% group_by(GlobalClientID)%>% summarise(Aug_17.pymt=sum(Amount))

Sept_17 <-subset(v17,v17$date <= "2016-09-30")
Sept_17.pymt <- Sept_17%>% group_by(GlobalClientID)%>% summarise(Sept_17.pymt=sum(Amount))

Octor_17 <-subset(v17,v17$date <= "2016-10-31")
Octor_17.pymt <- Octor_17%>% group_by(GlobalClientID)%>% summarise(Octor_17.pymt=sum(Amount))

Nov_17 <-subset(v17,v17$date <= "2016-11-30")
Nov_17.pymt <- Nov_17%>% group_by(GlobalClientID)%>% summarise(Nov_17.pymt=sum(Amount))

Dec_17 <-subset(v17,v17$date <= "2016-12-31")
Dec_17.pymt <- Dec_17%>% group_by(GlobalClientID)%>% summarise(Dec_17.pymt=sum(Amount))

Jan_17 <-subset(v17,v17$date <= "2017-01-31")
Jan_17.pymt <- Jan_17%>% group_by(GlobalClientID)%>% summarise(Jan_17.pymt=sum(Amount))

Feb_17 <-subset(v17,v17$date <= "2017-02-28")
Feb_17.pymt <-Feb_17%>% group_by(GlobalClientID)%>% summarise(Feb_17.pymt=sum(Amount))

Mar_17 <-subset(v17,v17$date <= "2017-03-31")
Mar_17.pymt <- Mar_17%>% group_by(GlobalClientID)%>% summarise(Mar_17.pymt=sum(Amount))

Apr_17 <-subset(v17,v17$date <= "2017-04-30")
Apr_17.pymt <- Apr_17%>% group_by(GlobalClientID)%>% summarise(Apr_17.pymt=sum(Amount))

May_17 <-subset(v17,v17$date <= "2017-05-31")
May_17.pymt <- May_17%>% group_by(GlobalClientID)%>% summarise(May_17.pymt=sum(Amount))

Jun_17 <-subset(v17,v17$date <= "2017-06-30")
Jun_17.pymt <- Jun_17%>% group_by(GlobalClientID)%>% summarise(Jun_17.pymt=sum(Amount))

Jul_17 <-subset(v17,v17$date <= "2017-07-20")
Jul_17.pymt <- Jul_17%>% group_by(GlobalClientID)%>% summarise(Jul_17.pymt=sum(Amount))

# #create columns in the SC for each month and put inside amount paid by each farmer
names(Aug_17.pymt)
names(d17)
d17$Aug_17.pymt <- Aug_17.pymt$Aug_17.pymt[match(d17$GlobalClientID,Aug_17.pymt$GlobalClientID)]
d17$Sept_17.pymt <- Sept_17.pymt$Sept_17.pymt[match(d17$GlobalClientID,Sept_17.pymt$GlobalClientID)]
d17$Oct_17.pymt <- Octor_17.pymt$Octor_17.pymt[match(d17$GlobalClientID,Octor_17.pymt$GlobalClientID)]
d17$Nov_17.pymt <- Nov_17.pymt$Nov_17.pymt[match(d17$GlobalClientID,Nov_17.pymt$GlobalClientID)]
d17$Dec_17.pymt <- Dec_17.pymt$Dec_17.pymt[match(d17$GlobalClientID,Dec_17.pymt$GlobalClientID)]
d17$Jan_17.pymt <- Jan_17.pymt$Jan_17.pymt[match(d17$GlobalClientID,Jan_17.pymt$GlobalClientID)]
d17$Feb_17.pymt <- Feb_17.pymt$Feb_17.pymt[match(d17$GlobalClientID,Feb_17.pymt$GlobalClientID)]
d17$Mar_17.pymt <- Mar_17.pymt$Mar_17.pymt[match(d17$GlobalClientID,Mar_17.pymt$GlobalClientID)]
d17$Apr_17.pymt <- Apr_17.pymt$Apr_17.pymt[match(d17$GlobalClientID,Apr_17.pymt$GlobalClientID)]
d17$May_17.pymt <- May_17.pymt$May_17.pymt[match(d17$GlobalClientID,May_17.pymt$GlobalClientID)]
d17$Jun_17.pymt <- Jun_17.pymt$Jun_17.pymt[match(d17$GlobalClientID,Jun_17.pymt$GlobalClientID)]
d17$Jul_17.pymt <- Jul_17.pymt$Jul_17.pymt[match(d17$GlobalClientID,Jul_17.pymt$GlobalClientID)]

d17[is.na(d17)] <- 0
#Check if it works
names(d17)
d17$clt.made.paymt <- ifelse((d17$Aug_17.pymt >0
                              |d17$Sept_17.pymt >0
                              |d17$Oct_17.pymt >0
                              |d17$Nov_17.pymt >0
                              |d17$Dec_17.pymt >0
                              |d17$Jan_17.pymt >0                     
                              |d17$Feb_17.pymt >0                 
                              |d17$Mar_17.pymt >0                 
                              |d17$Apr_17.pymt >0                 
                              |d17$May_17.pymt >0                 
                              |d17$Jun_17.pymt >0                 
                              |d17$Jul_17.pymt >0),1,0)
table(d17$clt.made.paymt, useNA = "ifany")

#Calculate monthly % repaid
#2020
monthly_pymt_20 <- d20%>% group_by(Region.Name)%>% summarise(
                  Aug_x.repaid = 100*sum(Aug_20.pymt)/sum(X2020A_CycleCredit),
                  Sept_x.repaid = 100*sum(Sept_20.pymt)/sum(X2020A_CycleCredit),
                  Oct_x.repaid = 100*sum(Oct_20.pymt)/sum(X2020A_CycleCredit),
                  Nov_x.repaid = 100*sum(Nov_20.pymt)/sum(X2020A_CycleCredit),
                  Dec_x.repaid = 100*sum(Dec_20.pymt)/sum(X2020A_CycleCredit),
                  Jan_x.repaid = 100*sum(Jan_20.pymt)/sum(TotalCredit),
                  Feb_x.repaid = 100*sum(Feb_20.pymt)/sum(TotalCredit),
                  Mar_x.repaid = 100*sum(Mar_20.pymt)/sum(TotalCredit),
                  Apr_x.repaid = 100*sum(Apr_20.pymt)/sum(TotalCredit),
                  May_x.repaid = 100*sum(May_20.pymt)/sum(TotalCredit),
                  Jun_x.repaid = 100*sum(Jun_20.pymt)/sum(TotalCredit),
                  Jul_x.repaid = 100*sum(Jul_20.pymt)/sum(TotalCredit))
View(monthly_pymt_20)
#2019
names(d18)
monthly_pymt_19 <- d19%>% group_by(Region.Name)%>% summarise(
                   Aug_x.repaid = 100*sum(Aug_19.pymt)/sum(X2019A_CycleCredit),
                   Sept_x.repaid = 100*sum(Sept_19.pymt)/sum(X2019A_CycleCredit),
                   Oct_x.repaid = 100*sum(Oct_19.pymt)/sum(X2019A_CycleCredit),
                   Nov_x.repaid = 100*sum(Nov_19.pymt)/sum(X2019A_CycleCredit),
                   Dec_x.repaid = 100*sum(Dec_19.pymt)/sum(X2019A_CycleCredit),
                   Jan_x.repaid = 100*sum(Jan_19.pymt)/sum(TotalCredit),
                   Feb_x.repaid = 100*sum(Feb_19.pymt)/sum(TotalCredit),
                   Mar_x.repaid = 100*sum(Mar_19.pymt)/sum(TotalCredit),
                   Apr_x.repaid = 100*sum(Apr_19.pymt)/sum(TotalCredit),
                   May_x.repaid = 100*sum(May_19.pymt)/sum(TotalCredit),
                   Jun_x.repaid = 100*sum(Jun_19.pymt)/sum(TotalCredit),
                   Jul_x.repaid = 100*sum(Jul_19.pymt)/sum(TotalCredit))
View(monthly_pymt_19)
#2018
monthly_pymt_18 <- d18%>% group_by(RegionName)%>% summarise(
                   Aug_x.repaid = 100*sum(Aug_18.pymt)/sum(X2018A_CycleCredit),
                   Sept_x.repaid = 100*sum(Sept_18.pymt)/sum(X2018A_CycleCredit),
                   Oct_x.repaid = 100*sum(Oct_18.pymt)/sum(X2018A_CycleCredit),
                   Nov_x.repaid = 100*sum(Nov_18.pymt)/sum(X2018A_CycleCredit),
                   Dec_x.repaid = 100*sum(Dec_18.pymt)/sum(X2018A_CycleCredit),
                   Jan_x.repaid = 100*sum(Jan_18.pymt)/sum(TotalCredit),
                   Feb_x.repaid = 100*sum(Feb_18.pymt)/sum(TotalCredit),
                   Mar_x.repaid = 100*sum(Mar_18.pymt)/sum(TotalCredit),
                   Apr_x.repaid = 100*sum(Apr_18.pymt)/sum(TotalCredit),
                   May_x.repaid = 100*sum(May_18.pymt)/sum(TotalCredit),
                   Jun_x.repaid = 100*sum(Jun_18.pymt)/sum(TotalCredit),
                   Jul_x.repaid = 100*sum(Jul_18.pymt)/sum(TotalCredit))
View(monthly_pymt_18)
#2017
monthly_pymt_17 <- d17%>% group_by(RegionName)%>% summarise(
                   Aug_x.repaid = 100*sum(Aug_17.pymt)/sum(X2017A_CycleCredit),
                   Sept_x.repaid = 100*sum(Sept_17.pymt)/sum(X2017A_CycleCredit),
                   Oct_x.repaid = 100*sum(Oct_17.pymt)/sum(X2017A_CycleCredit),
                   Nov_x.repaid = 100*sum(Nov_17.pymt)/sum(X2017A_CycleCredit),
                   Dec_x.repaid = 100*sum(Dec_17.pymt)/sum(X2017A_CycleCredit),
                   Jan_x.repaid = 100*sum(Jan_17.pymt)/sum(TotalCredit),
                   Feb_x.repaid = 100*sum(Feb_17.pymt)/sum(TotalCredit),
                   Mar_x.repaid = 100*sum(Mar_17.pymt)/sum(TotalCredit),
                   Apr_x.repaid = 100*sum(Apr_17.pymt)/sum(TotalCredit),
                   May_x.repaid = 100*sum(May_17.pymt)/sum(TotalCredit),
                   Jun_x.repaid = 100*sum(Jun_17.pymt)/sum(TotalCredit),
                   Jul_x.repaid = 100*sum(Jul_17.pymt)/sum(TotalCredit))
View(monthly_pymt_17)

