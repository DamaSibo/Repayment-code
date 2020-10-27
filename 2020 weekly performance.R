### Monthly actual repayment + Team performance


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
d20 <- read.csv(paste(dd, "SC_2020_2020.07.20.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d20 <- subset(d20, d20$TotalCredit >0)
nrow(d20)

v20 <- read.csv(paste(dd, "V_2020_2020.08.19.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")

#--------------------------------------------------------------------------------------------
d20$Region.Name <- ifelse(d20$DistrictName=="Ruhango", "South", d20$RegionName)
table(d20$RegionName, useNA = "ifany")
table(d20$Region.Name, useNA = "ifany")
# RegionName
v20$Region <- d20$Region.Name[match(v20$District,d20$DistrictName)]
length(unique(v20$site.id[is.na(v20$Region)]))
table(v20$Region, useNA = "ifany")

#2020 ----------
#To put dates in standard format,
v20$date <- substr(v20$RepaymentDate, 1, 10)
v20$date <- gsub(" ", "", v20$date)
v20$date <- gsub("-", "/", v20$date)
v20$date <- as.Date(v20$date)
#To check if we are in date format
class(v20$date)
summary(v20$date)

# subset only active clients in VR ----------------------------------
names(v20)
v20$active.clt <- d20$GlobalClientID[match(v20$GlobalClientID, d20$GlobalClientID)]
length(unique(v20$GlobalClientID[is.na(v20$active.clt)]))
length(unique(v20$GlobalClientID[!is.na(v20$active.clt)]))
length(unique(v20$GlobalClientID[v20$Dropped=="False"]))
nrow(d20)
length(unique(d20$GlobalClientID[d20$TotalRepaid==0]))

sum(v20$Amount[v20$Dropped=="True"])
sum(v20$Amount[is.na(v20$active.clt)])
v201 <- subset(v20, !is.na(v20$active.clt))
length(unique(v201$GlobalClientID[!is.na(v201$active.clt)]))
nrow(v201)
sum(v20$Amount)
sum(v201$Amount)
# III. Subsetting needed columns and payments
########################################################################################
names(v201)
v201 <- select(v201,Region,District,GlobalClientID,Amount,date )
names(v201)
# Calculating the repayment trends on weekly basis
########################################################################################
#Region level
reg_weekly_rep_clt_20 <- v201%>% 
  group_by (Region)%>%
  summarise( #20A
             w1_20.pymt= sum(Amount[date  <= "2019-07-29"],na.rm=T),
             w2_20.pymt= sum(Amount[date  <= "2019-08-05"],na.rm=T),
             w3_20.pymt= sum(Amount[date  <= "2019-08-12"],na.rm=T),
             w4_20.pymt= sum(Amount[date  <= "2019-08-19"],na.rm=T),
             w5_20.pymt= sum(Amount[date  <= "2019-08-26"],na.rm=T),
             w6_20.pymt= sum(Amount[date  <= "2019-09-02"],na.rm=T),
             w7_20.pymt= sum(Amount[date  <= "2019-09-07"],na.rm=T),                        
             w8_20.pymt= sum(Amount[date  <= "2019-09-09"],na.rm=T),
             w9_20.pymt= sum(Amount[date  <= "2019-09-16"],na.rm=T),
             w10_20.pymt= sum(Amount[date <= "2019-09-23"],na.rm=T),
             w11_20.pymt= sum(Amount[date <= "2019-09-30"],na.rm=T),
             w12_20.pymt= sum(Amount[date <= "2019-10-07"],na.rm=T),
             w13_20.pymt= sum(Amount[date <= "2019-10-14"],na.rm=T),
             w14_20.pymt= sum(Amount[date <= "2019-10-21"],na.rm=T),
             w15_20.pymt= sum(Amount[date <= "2019-11-04"],na.rm=T),
             w16_20.pymt= sum(Amount[date <= "2019-11-11"],na.rm=T),
             w17_20.pymt= sum(Amount[date <= "2019-11-18"],na.rm=T),
             w18_20.pymt= sum(Amount[date <= "2019-11-25"],na.rm=T),
             w19_20.pymt= sum(Amount[date <= "2019-12-02"],na.rm=T),
             w20_20.pymt= sum(Amount[date <= "2019-12-09"],na.rm=T),
             w21_20.pymt= sum(Amount[date <= "2019-12-16"],na.rm=T),
             w22_20.pymt= sum(Amount[date <= "2019-12-23"],na.rm=T),
             w23_20.pymt= sum(Amount[date <= "2019-12-30"],na.rm=T),
             #20AB
             w24_20.pymt= sum(Amount[date <= "2020-01-06"],na.rm=T),
             w25_20.pymt= sum(Amount[date <= "2020-01-13"],na.rm=T),
             w26_20.pymt= sum(Amount[date <= "2020-01-20"],na.rm=T),
             w27_20.pymt= sum(Amount[date <= "2020-01-27"],na.rm=T),
             w28_20.pymt= sum(Amount[date <= "2020-02-03"],na.rm=T),
             w29_20.pymt= sum(Amount[date <= "2020-02-13"],na.rm=T),
             w30_20.pymt= sum(Amount[date <= "2020-02-10"],na.rm=T),
             w31_20.pymt= sum(Amount[date <= "2020-02-24"],na.rm=T),
             w32_20.pymt= sum(Amount[date <= "2020-03-02"],na.rm=T),
             w33_20.pymt= sum(Amount[date <= "2020-03-09"],na.rm=T),
             w34_20.pymt= sum(Amount[date <= "2020-03-16"],na.rm=T),
             w35_20.pymt= sum(Amount[date <= "2020-03-23"],na.rm=T),
             w36_20.pymt= sum(Amount[date <= "2020-03-30"],na.rm=T),
             w37_20.pymt= sum(Amount[date <= "2020-04-06"],na.rm=T),
             w38_20.pymt= sum(Amount[date <= "2020-04-13"],na.rm=T),
             w39_20.pymt= sum(Amount[date <= "2020-04-20"],na.rm=T),
             w40_20.pymt= sum(Amount[date <= "2020-04-27"],na.rm=T),
             w41_20.pymt= sum(Amount[date <= "2020-05-04"],na.rm=T),
             w42_20.pymt= sum(Amount[date <= "2020-05-11"],na.rm=T),
             w43_20.pymt= sum(Amount[date <= "2020-05-18"],na.rm=T),
             w44_20.pymt= sum(Amount[date <= "2020-05-25"],na.rm=T),
             w45_20.pymt= sum(Amount[date <= "2020-05-01"],na.rm=T),
             w46_20.pymt= sum(Amount[date <= "2020-06-08"],na.rm=T),
             w47_20.pymt= sum(Amount[date <= "2020-06-15"],na.rm=T),
             w48_20.pymt= sum(Amount[date <= "2020-06-22"],na.rm=T),
             w49_20.pymt= sum(Amount[date <= "2020-06-29"],na.rm=T),
             w50_20.pymt= sum(Amount[date <= "2020-07-06"],na.rm=T),
             w51_20.pymt= sum(Amount[date <= "2020-07-13"],na.rm=T),
             w52_20.pymt= sum(Amount[date <= "2020-07-20"],na.rm=T))
             

#to check if it works
print(reg_weekly_rep_clt_20)
View(reg_weekly_rep_clt_20)

#Rwanda level
weekly_rep_clt_20 <- v201%>% 
  group_by ()%>%
  summarise( #20A
    w1_20.pymt= sum(Amount[date  <= "2019-07-29"],na.rm=T),
    w2_20.pymt= sum(Amount[date  <= "2019-08-05"],na.rm=T),
    w3_20.pymt= sum(Amount[date  <= "2019-08-12"],na.rm=T),
    w4_20.pymt= sum(Amount[date  <= "2019-08-19"],na.rm=T),
    w5_20.pymt= sum(Amount[date  <= "2019-08-26"],na.rm=T),
    w6_20.pymt= sum(Amount[date  <= "2019-09-02"],na.rm=T),
    w7_20.pymt= sum(Amount[date  <= "2019-09-07"],na.rm=T),                        
    w8_20.pymt= sum(Amount[date  <= "2019-09-09"],na.rm=T),
    w9_20.pymt= sum(Amount[date  <= "2019-09-16"],na.rm=T),
    w10_20.pymt= sum(Amount[date <= "2019-09-23"],na.rm=T),
    w11_20.pymt= sum(Amount[date <= "2019-09-30"],na.rm=T),
    w12_20.pymt= sum(Amount[date <= "2019-10-07"],na.rm=T),
    w13_20.pymt= sum(Amount[date <= "2019-10-14"],na.rm=T),
    w14_20.pymt= sum(Amount[date <= "2019-10-21"],na.rm=T),
    w15_20.pymt= sum(Amount[date <= "2019-11-04"],na.rm=T),
    w16_20.pymt= sum(Amount[date <= "2019-11-11"],na.rm=T),
    w17_20.pymt= sum(Amount[date <= "2019-11-18"],na.rm=T),
    w18_20.pymt= sum(Amount[date <= "2019-11-25"],na.rm=T),
    w19_20.pymt= sum(Amount[date <= "2019-12-02"],na.rm=T),
    w20_20.pymt= sum(Amount[date <= "2019-12-09"],na.rm=T),
    w21_20.pymt= sum(Amount[date <= "2019-12-16"],na.rm=T),
    w22_20.pymt= sum(Amount[date <= "2019-12-23"],na.rm=T),
    w23_20.pymt= sum(Amount[date <= "2019-12-30"],na.rm=T),
    #20AB
    w24_20.pymt= sum(Amount[date <= "2020-01-06"],na.rm=T),
    w25_20.pymt= sum(Amount[date <= "2020-01-13"],na.rm=T),
    w26_20.pymt= sum(Amount[date <= "2020-01-20"],na.rm=T),
    w27_20.pymt= sum(Amount[date <= "2020-01-27"],na.rm=T),
    w28_20.pymt= sum(Amount[date <= "2020-02-03"],na.rm=T),
    w29_20.pymt= sum(Amount[date <= "2020-02-13"],na.rm=T),
    w30_20.pymt= sum(Amount[date <= "2020-02-10"],na.rm=T),
    w31_20.pymt= sum(Amount[date <= "2020-02-24"],na.rm=T),
    w32_20.pymt= sum(Amount[date <= "2020-03-02"],na.rm=T),
    w33_20.pymt= sum(Amount[date <= "2020-03-09"],na.rm=T),
    w34_20.pymt= sum(Amount[date <= "2020-03-16"],na.rm=T),
    w35_20.pymt= sum(Amount[date <= "2020-03-23"],na.rm=T),
    w36_20.pymt= sum(Amount[date <= "2020-03-30"],na.rm=T),
    w37_20.pymt= sum(Amount[date <= "2020-04-06"],na.rm=T),
    w38_20.pymt= sum(Amount[date <= "2020-04-13"],na.rm=T),
    w39_20.pymt= sum(Amount[date <= "2020-04-20"],na.rm=T),
    w40_20.pymt= sum(Amount[date <= "2020-04-27"],na.rm=T),
    w41_20.pymt= sum(Amount[date <= "2020-05-04"],na.rm=T),
    w42_20.pymt= sum(Amount[date <= "2020-05-11"],na.rm=T),
    w43_20.pymt= sum(Amount[date <= "2020-05-18"],na.rm=T),
    w44_20.pymt= sum(Amount[date <= "2020-05-25"],na.rm=T),
    w45_20.pymt= sum(Amount[date <= "2020-05-01"],na.rm=T),
    w46_20.pymt= sum(Amount[date <= "2020-06-08"],na.rm=T),
    w47_20.pymt= sum(Amount[date <= "2020-06-15"],na.rm=T),
    w48_20.pymt= sum(Amount[date <= "2020-06-22"],na.rm=T),
    w49_20.pymt= sum(Amount[date <= "2020-06-29"],na.rm=T),
    w50_20.pymt= sum(Amount[date <= "2020-07-06"],na.rm=T),
    w51_20.pymt= sum(Amount[date <= "2020-07-13"],na.rm=T),
    w52_20.pymt= sum(Amount[date <= "2020-07-20"],na.rm=T))
    

#to check if it works
print(weekly_rep_clt_20)
View(weekly_rep_clt_20)
sum(d20$X2020A_CycleCredit)
sum(d20$TotalCredit)

names(d20)
credit_20 <- d20 %>% group_by (Region.Name)%>%  summarise(
                             credit.20A = sum(X2020A_CycleCredit),
                             credit.20AB = sum(TotalCredit)
)
              
View(credit_20) 


write.table(reg_weekly_rep_clt_20, file = paste(od, "weekly_rep_clt_20_Region_level.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(weekly_rep_clt_20, file = paste(od, "weekly_rep_clt_20.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(credit_20, file = paste(od, "20AB_Credit.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

##-------------------------------- end--------------------------------------------------------------##
# repayment monthly performance on Sites and district level 

# subset monthly payment
Aug_20 <-subset(v201, v201$date <= "2019-08-31")
Aug_20.pymt <- Aug_20%>% group_by(GlobalClientID)%>% summarise(Aug_20.pymt=sum(Amount))

Sept_20 <-subset(v201,v201$date <= "2019-09-30")
Sept_20.pymt <- Sept_20%>% group_by(GlobalClientID)%>% summarise(Sept_20.pymt=sum(Amount))

Octor_20 <-subset(v201,v201$date <= "2019-10-31")
Octor_20.pymt <- Octor_20%>% group_by(GlobalClientID)%>% summarise(Octor_20.pymt=sum(Amount))

Nov_20 <-subset(v201,v201$date <= "2019-11-30")
Nov_20.pymt <- Nov_20%>% group_by(GlobalClientID)%>% summarise(Nov_20.pymt=sum(Amount))

Dec_20 <-subset(v201,v201$date <= "2019-12-31")
Dec_20.pymt <- Dec_20%>% group_by(GlobalClientID)%>% summarise(Dec_20.pymt=sum(Amount))

Jan_20 <-subset(v201,v201$date <= "2020-01-31")
Jan_20.pymt <- Jan_20%>% group_by(GlobalClientID)%>% summarise(Jan_20.pymt=sum(Amount))

Feb_20 <-subset(v201,v201$date <= "2020-02-29")
Feb_20.pymt <-Feb_20%>% group_by(GlobalClientID)%>% summarise(Feb_20.pymt=sum(Amount))

Mar_20 <-subset(v201,v201$date <= "2020-03-31")
Mar_20.pymt <- Mar_20%>% group_by(GlobalClientID)%>% summarise(Mar_20.pymt=sum(Amount))

Apr_20 <-subset(v201,v201$date <= "2020-04-30")
Apr_20.pymt <- Apr_20%>% group_by(GlobalClientID)%>% summarise(Apr_20.pymt=sum(Amount))

May_20 <-subset(v201,v201$date <= "2020-05-31")
May_20.pymt <- May_20%>% group_by(GlobalClientID)%>% summarise(May_20.pymt=sum(Amount))

Jun_20 <-subset(v201,v201$date <= "2020-06-30")
Jun_20.pymt <- Jun_20%>% group_by(GlobalClientID)%>% summarise(Jun_20.pymt=sum(Amount))

Jul_20 <-subset(v201,v201$date <= "2020-07-20")
Jul_20.pymt <- Jul_20%>% group_by(GlobalClientID)%>% summarise(Jul_20.pymt=sum(Amount))

# Merge monthly payment in Sc
d20$Aug_20.pymt <- Aug_20.pymt$Aug_20.pymt[match(d20$GlobalClientID,Aug_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Aug_20.pymt)]))
d20$Aug_20.pymt <- ifelse(is.na(d20$Aug_20.pymt), 0, d20$Aug_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Aug_20.pymt)]))

d20$Sept_20.pymt <- Sept_20.pymt$Sept_20.pymt[match(d20$GlobalClientID,Sept_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Sept_20.pymt)]))
d20$Sept_20.pymt <- ifelse(is.na(d20$Sept_20.pymt), 0, d20$Sept_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Sept_20.pymt)]))

d20$Oct_20.pymt <- Octor_20.pymt$Octor_20.pymt[match(d20$GlobalClientID,Octor_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Oct_20.pymt)]))
d20$Oct_20.pymt <- ifelse(is.na(d20$Oct_20.pymt), 0, d20$Oct_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Oct_20.pymt)]))

d20$Nov_20.pymt <- Nov_20.pymt$Nov_20.pymt[match(d20$GlobalClientID,Nov_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Nov_20.pymt)]))
d20$Nov_20.pymt <- ifelse(is.na(d20$Nov_20.pymt), 0, d20$Nov_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Nov_20.pymt)]))

d20$Dec_20.pymt <- Dec_20.pymt$Dec_20.pymt[match(d20$GlobalClientID,Dec_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Dec_20.pymt)]))
d20$Dec_20.pymt <- ifelse(is.na(d20$Dec_20.pymt), 0, d20$Dec_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Dec_20.pymt)]))

d20$Jan_20.pymt <- Jan_20.pymt$Jan_20.pymt[match(d20$GlobalClientID,Jan_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Jan_20.pymt)]))
d20$Jan_20.pymt <- ifelse(is.na(d20$Jan_20.pymt), 0, d20$Jan_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Jan_20.pymt)]))

d20$Feb_20.pymt <- Feb_20.pymt$Feb_20.pymt[match(d20$GlobalClientID,Feb_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Feb_20.pymt)]))
d20$Feb_20.pymt <- ifelse(is.na(d20$Feb_20.pymt), 0, d20$Feb_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Feb_20.pymt)]))

d20$Mar_20.pymt <- Mar_20.pymt$Mar_20.pymt[match(d20$GlobalClientID,Mar_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Mar_20.pymt)]))
d20$Mar_20.pymt <- ifelse(is.na(d20$Mar_20.pymt), 0, d20$Mar_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Mar_20.pymt)]))

d20$Apr_20.pymt <- Apr_20.pymt$Apr_20.pymt[match(d20$GlobalClientID,Apr_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Apr_20.pymt)]))
d20$Apr_20.pymt <- ifelse(is.na(d20$Apr_20.pymt), 0, d20$Apr_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Apr_20.pymt)]))

d20$May_20.pymt <- May_20.pymt$May_20.pymt[match(d20$GlobalClientID,May_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$May_20.pymt)]))
d20$May_20.pymt <- ifelse(is.na(d20$May_20.pymt), 0, d20$May_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$May_20.pymt)]))

d20$Jun_20.pymt <- Jun_20.pymt$Jun_20.pymt[match(d20$GlobalClientID,Jun_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Jun_20.pymt)]))
d20$Jun_20.pymt <- ifelse(is.na(d20$Jun_20.pymt), 0, d20$Jun_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Jun_20.pymt)]))

d20$Jul_20.pymt <- Jul_20.pymt$Jul_20.pymt[match(d20$GlobalClientID,Jul_20.pymt$GlobalClientID)]
length(unique(d20$GlobalClientID[is.na(d20$Jul_20.pymt)]))
d20$Jul_20.pymt <- ifelse(is.na(d20$Jul_20.pymt), 0, d20$Jul_20.pymt)
length(unique(d20$GlobalClientID[is.na(d20$Jul_20.pymt)]))

#check if it works
names(d20)
sum(d20$X2020A_CycleCredit)
sum(d20$TotalCredit)
sum(d20$Jul_20.pymt)
sum(Jul_20.pymt$Jul_20.pymt)
sum(Jul_20.pymt$Jul_20.pymt) - sum(d20$Jul_20.pymt) # maybe due to inactive clients
#--------------------
100*sum(d20$Aug_20.pymt)/sum(d20$X2020A_CycleCredit) #20.1%
100*sum(d20$Sept_20.pymt)/sum(d20$X2020A_CycleCredit) #27.5%
100*sum(d20$Oct_20.pymt)/sum(d20$X2020A_CycleCredit) # 35.7%
100*sum(d20$Nov_20.pymt)/sum(d20$X2020A_CycleCredit) # 42.6%
100*sum(d20$Dec_20.pymt)/sum(d20$X2020A_CycleCredit) # 54.6%
100*sum(d20$Jan_20.pymt)/sum(d20$TotalCredit) # 39.9%
100*sum(d20$Feb_20.pymt)/sum(d20$TotalCredit) # 45.6%
100*sum(d20$Mar_20.pymt)/sum(d20$TotalCredit) # 51.1%
100*sum(d20$Apr_20.pymt)/sum(d20$TotalCredit) # 55.2%
100*sum(d20$May_20.pymt)/sum(d20$TotalCredit) # 66.5%
100*sum(d20$Jun_20.pymt)/sum(d20$TotalCredit) # 79.8%
100*sum(d20$Jul_20.pymt)/sum(d20$TotalCredit) # 89.8%

# site level
site_pymt_monthly <- d20%>% group_by(Region.Name, DistrictName, SiteName)%>% summarise(
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

View(site_pymt_monthly)

# District level
district_pymt_monthly <- d20%>% group_by(DistrictName)%>% summarise(
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

View(district_pymt_monthly)
#Region level
region_pymt_monthly <- d20%>% group_by(Region.Name)%>% summarise(
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

View(region_pymt_monthly)
# Country level
rw_pymt_monthly <- d20%>% group_by()%>% summarise(
                           Aug_x.repaid = 100*sum(Aug_20.pymt)/sum(X2020A_CycleCredit),
                           Sept_x.repaid= 100*sum(Sept_20.pymt)/sum(X2020A_CycleCredit),
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

View(rw_pymt_monthly)
#--------------------------------------------------------------------------------------------#

write.table(site_pymt_monthly, file = paste(od, "20AB_site_pymt_monthly.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(district_pymt_monthly, file = paste(od, "20AB_District_pymt_monthly.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(region_pymt_monthly, file = paste(od, "20AB_region_pymt_monthly.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(region_pymt_monthly, file = paste(od, "20AB_region_pymt_monthly.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(rw_pymt_monthly, file = paste(od, "20AB_rw_pymt_monthly.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")


