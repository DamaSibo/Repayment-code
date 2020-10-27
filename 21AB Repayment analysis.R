#cleaning the environment
rm(list = ls()); cat("\014")

#setting directories
dd <- "D:/Drop analysis/Repayment/data"
od <- "D:/Drop analysis/Repayment/output"

# Load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape", "anytime")
# install.packages("plyr", dependencies = TRUE)

lapply(libs, require, character.only = T)
# read data
d21 <- read.csv(paste(dd, "SC_2021_2020.09.24.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

v21 <- read.csv(paste(dd, "V_2021_2020.09.24.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

d21$Region.Name <- ifelse(d21$DistrictName=="Ruhango", "South", d21$RegionName)
d21$Region.Name <- ifelse(d21$DistrictName=="Rubavu", "North", d21$RegionName)

# Add RegionName in v
v21$Region.Name <- d21$Region.Name[match(v21$District,d21$DistrictName)]
length(unique(v21$site.id[is.na(v21$Region.Name)]))
table(v21$Region.Name, useNA = "ifany")
# check
sum(d21$TotalCredit)
sum(d21$TotalRepaid)
sum(d21$TotalRepaid)/sum(d21$TotalCredit)*100
sum(d21$TotalRepaid_IncludingOverpayments)/sum(d21$TotalCredit)*100
length(unique(d21$GlobalClientID[d21$X..Repaid>=60]))
length(unique(d21$GlobalClientID[d21$TotalCredit>0]))
length(unique(d21$GlobalClientID[d21$X..Repaid>=60]))/length(unique(d21$GlobalClientID[d21$TotalCredit>0]))*100
length(unique(d21$GlobalClientID[d21$X..Repaid==100]))
length(unique(d21$GlobalClientID[d21$X..Repaid==100]))/length(unique(d21$GlobalClientID[d21$TotalCredit>0]))*100

#2021 ----------
#To put dates in standard format,
v21$date <- substr(v21$RepaymentDate, 1, 10)
v21$date <- gsub(" ", "", v21$date)
v21$date <- gsub("-", "/", v21$date)
v21$date <- as.Date(v21$date)
#To check if we are in date format
class(v21$date)
summary(v21$date)

# subset weekly payment
Sept_W.1 <-subset(v21, v21$date > "2020-08-31" & v21$date <= "2020-09-07")
Sept_W.1s <- Sept_W.1%>% group_by(GlobalClientID)%>% summarise(Sept_W.1=sum(Amount))

Sept_W.2 <-subset(v21, v21$date > "2020-09-07" & v21$date <= "2020-09-14")
Sept_W.2s <- Sept_W.2%>% group_by(GlobalClientID)%>% summarise(Sept_W.2=sum(Amount))

Sept_W.3 <-subset(v21, v21$date > "2020-09-14" & v21$date <= "2020-09-21")
Sept_W.3s <- Sept_W.3%>% group_by(GlobalClientID)%>% summarise(Sept_W.3=sum(Amount))

# Number of payment + total amount in each
Sept_reg_weekly_rep_clt <- v21%>% group_by (Region.Name)%>% summarise(
                      Sept_w1.pymt= sum(Amount[date > "2020-08-31" & date <= "2020-09-07"],na.rm=T),
                      Sept_w2.pymt= sum(Amount[date > "2020-09-07" & date <= "2020-09-14"],na.rm=T),
                      Sept_w3.pymt= sum(Amount[date > "2020-09-14" & date <= "2020-09-21"],na.rm=T))
  
  
View(Sept_reg_weekly_rep_clt)

write.table(Sept_reg_weekly_rep_clt, file = paste(od, "Sept_reg_weekly_rep_clt1.csv", sep = "/"),
              row.names = FALSE, col.names = TRUE, sep = ",")
  
#2021 SC
d21$Sept_W.1_pymt  <- Sept_W.1s$Sept_W.1[match(d21$GlobalClientID,Sept_W.1s$GlobalClientID)]
d21$Sept_W.2_pymt  <- Sept_W.2s$Sept_W.2[match(d21$GlobalClientID,Sept_W.2s$GlobalClientID)] 
d21$Sept_W.3_pymt  <- Sept_W.3s$Sept_W.3[match(d21$GlobalClientID,Sept_W.3s$GlobalClientID)]
d21[is.na(d21)] <- 0

reg_partic_Sept <- d21%>% group_by(Region.Name)%>% summarise(
                  tot.credit = sum(TotalCredit),
                  Sept_w.1.prp = length(unique(GlobalClientID[Sept_W.1_pymt>0])),
                  Sept_w.2.prp = length(unique(GlobalClientID[Sept_W.2_pymt>0])),
                  Sept_w.3.prp = length(unique(GlobalClientID[Sept_W.3_pymt>0]))
)

View(reg_partic_Sept)


current.repaid_clt <- d21%>% group_by(RegionName)%>% summarise(
                             total.credit = sum(TotalCredit),
                             total.repaid = sum(TotalRepaid),
                             tot.clt= length(unique[GlobalClientID]),
                             clt.HP1 = length(unique(GlobalClientID[X..Repaid>=31.5])))
                             clt.HP2 = length(unique(GlobalClientID[X..Repaid>=28.5]))
View(current.repaid_clt)


write.table(reg_partic_Sept, file = paste(od, "reg_partic_Sept.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

