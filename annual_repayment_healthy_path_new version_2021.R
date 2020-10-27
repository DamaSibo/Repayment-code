#B seasons Historical Repayment health Path 
#date: July 09, 2020
#Author:Vedaste
#Editor: Dama

#clear environment
rm(list = ls()); cat("\014")

#Setting data and output directories
dd <- "D:/Drop analysis/Repayment/Healthy Path/data"
wd <- "D:/Drop analysis/Repayment/Healthy Path/data"
od <- "D:/Drop analysis/Repayment/Healthy Path/output"

### 1.calling libraries as in my other scripts.
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)


#PART I: Reading data
#######################################################################################
# 0.loading the SC, VR and FM Zones data for 2018 and Keeping up to current week  vR data

#2017 ----------------------------------------------------------------------------------------

d17 <- read.csv(paste(dd, "SC_2017_10.27.17.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
d17 <- subset(d17, TotalCredit > 0)
nrow(d17) #216,076 total clients

v17 <- read.csv(paste(dd, "VR_2017_10.27.17.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
nrow(v17) #1,714,680 transactions
#2018 ----------------------------------------------------------------------------------------

d18 <- read.csv(paste(dd, "SC_2018_8.6.18.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
d18 <- subset(d18, TotalCredit > 0)
nrow(d18) #264,454 total clients

v18 <- read.csv(paste(dd, "VR_2018_8.6.18.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
nrow(v18) #1,935,165 transactions
#2019 ----------------------------------------------------------------------------------------

d19 <- read.csv(paste(dd, "SC_2019_8.5.19.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
d19 <- subset(d19, TotalCredit > 0)
nrow(d19) #322,269 total clients

v19 <- read.csv(paste(dd, "VR_2019_8.5.19.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")
nrow(v19) #2,509,951 transactions
#--------------------------------------------------------------------------------------------
d20 <- read.csv(paste(dd, "SC_2020_2020.07.08.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
d20 <- subset(d20, TotalCredit > 0)
nrow(d20) #464,526 total clients

v20 <- read.csv(paste(dd, "V_2020_2020.07.07.csv", sep = "/"), header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
nrow(v20) #2,509,951 transactions
########################################################################################
# I. Changing dates format
########################################################################################

#2020 ----------
#To put dates in standard format,
v20$date <- substr(v20$RepaymentDate, 1, 10)
v20$date <- gsub(" ", "", v20$date)
v20$date <- gsub("-", "/", v20$date)
v20$date <- as.Date(v20$date)

#To check if we are in date format
class(v20$date)
summary(v20$date)
#2019 ----------
#To put dates in standard format,
v19$date <- substr(v19$RepaymentDate, 1, 10)
v19$date <- gsub(" ", "", v19$date)
v19$date <- gsub("-", "/", v19$date)
v19$date <- as.Date(v19$date)

#To check if we are in date format
class(v19$date)
summary(v19$date)

#2018 --------
#To put dates in standard format,
v18$date <- substr(v18$RepaymentDate, 1, 10)
v18$date <- gsub(" ", "", v18$date)
v18$date <- gsub("-", "/", v18$date)
v18$date <- as.Date(v18$date)

#To check if we are in date format
class(v18$date)
summary(v18$date)

#2017 --------
#To put dates in standard format,
v17$date <- substr(v17$RepaymentDate, 1, 10)
v17$date <- gsub(" ", "", v17$date)
v17$date <- gsub("-", "/", v17$date)
v17$date <- as.Date(v17$date)

#To check if we are in date format
class(v17$date)
summary(v17$date)

########################################################################################
# III. Subsetting needed columns and payments
########################################################################################

#2020 ------------
myvars <- c("District","GlobalClientID","Amount", "date")
v201 <- v20[myvars]
names(v201)

#We need only payments made by July 27. 
#so, let's use subset
table(v201$date, useNA = "ifany")

v202 <- subset(v201, v201$date < "2019-07-28")

#to check if it works
summary(v202$date)

#2019 ------------
myvars <- c("District","GlobalClientID","Amount", "date")
v191 <- v19[myvars]
names(v191)

#We need only payments made by July 27. 
#so, let's use subset
table(v191$date, useNA = "ifany")

v192 <- subset(v191, v191$date < "2019-07-28")

#to check if it works
summary(v192$date)

#2018 ------------

v181 <- v18[myvars]
names(v181)

#We need only payments made by July 27. 
#so, let's use subset
table(v181$date, useNA = "ifany")
v182 <- subset(v181, v181$date < "2018-07-28")

#to check if it works
summary(v182$date)

#2017 ------------
v171 <- v17[myvars]
names(v171)

#We need only payments made by July 27. 
#so, let's use subset
table(v171$date, useNA = "ifany")

v172 <- subset(v171, v171$date < "2017-07-28")

#to check if it works
summary(v172$date)

#-----------------------------------------------------
#Now, let's merge the Vertical repayment wth SC in order to determine clients who met repayment

#before that, let's put the VR on client level
#removing unnecessary columns first

v193 <- select(v192, GlobalClientID, Amount)
v183 <- select(v182, GlobalClientID, Amount)
v173 <- select(v172, GlobalClientID, Amount)

#to check if it works
names(v193)

v194 <- v193 %>%
  group_by (GlobalClientID)%>%
  summarise (amount.paid = sum(Amount))

v184 <- v183 %>%
  group_by (GlobalClientID)%>%
  summarise (amount.paid = sum(Amount))

v174 <- v173 %>%
  group_by (GlobalClientID)%>%
  summarise (amount.paid = sum(Amount))


#to check if it works
dim(v194)
dim(v184)
dim(v174)

#saving this objects
save(v194, file = paste(wd,"v194.RData", sep ="/"))
save(v184, file = paste(wd,"v184.RData", sep ="/"))
save(v174, file = paste(wd,"v174.RData", sep ="/"))
#load
load(file = paste(wd,"v194.RData", sep ="/"))
load(file = paste(wd,"v184.RData", sep ="/"))
load(file = paste(wd,"v174.RData", sep ="/"))

#then, let's do merge
d191 <- merge(d19, v194, by = "GlobalClientID", all.x = T)
d181 <- merge(d18, v184, by = "GlobalClientID", all.x = T)
d171 <- merge(d17, v174, by = "GlobalClientID", all.x = T)

#to check if it works
sum(d191$amount.paid, na.rm = T)
sum(d181$amount.paid, na.rm = T)
sum(d181$TotalRepaid)
sum(d171$amount.paid, na.rm = T)

#let's remove NAs 
d191$amount.paid <- ifelse(is.na(d191$amount.paid), 0, d191$amount.paid)
d181$amount.paid <- ifelse(is.na(d181$amount.paid), 0, d181$amount.paid)
d171$amount.paid <- ifelse(is.na(d171$amount.paid), 0, d171$amount.paid)

#to check if it works
summary(d181$amount.paid)
summary(d191$amount.paid)

#---------------------------------------------------
#Now, let's check if a client finished repayment by July 27 or not. 
d191$finish.rep <- ifelse(d191$amount.paid >= d191$TotalCredit, 1, 0)
d181$finish.rep <- ifelse(d181$amount.paid >= d181$TotalCredit, 1, 0)
d171$finish.rep <- ifelse(d171$amount.paid >= d171$TotalCredit, 1, 0)

#To check if it works
table(d191$finish.rep, useNA = "ifany")
table(d181$finish.rep, useNA = "ifany")
table(d171$finish.rep, useNA = "ifany")

#Now, let's subset only clients who did finish repayment
d192 <- subset(d191, d191$finish.rep > 0)
d182 <- subset(d181, d181$finish.rep > 0)
d172 <- subset(d171, d171$finish.rep > 0)

#to check if it works
summary(d182$finish.rep)
summary(d192$finish.rep)

###-----------------------------------------
#Then, let's do the same for the Vertical repayment - for each group

#starting with A+B clients
v192$match.clt <- match(v192$GlobalClientID, d192$GlobalClientID, nomatch = 0)
v182$match.clt <- match(v182$GlobalClientID, d182$GlobalClientID, nomatch = 0)
v172$match.clt <- match(v172$GlobalClientID, d172$GlobalClientID, nomatch = 0)

#to check if it works
table(v192$match.clt, useNA = "ifany")
table(v182$match.clt, useNA = "ifany")
table(v172$match.clt, useNA = "ifany")

#subsetting only clients within these groups
v195 <- subset(v192, match.clt > 0)
v185 <- subset(v182, match.clt > 0)
v175 <- subset(v172, match.clt > 0)

#to check if it works
summary(v185$match.clt)
summary(v195$match.clt)
dim(v195)
dim(v185)
dim(v175)

######################################################################################
########################################################################################
# IV. Calculating the repayment trends on weekly basis
########################################################################################
#2019 ---------------------------------------------------------
weekly_rep_clt_19 <- v195%>% 
            group_by ()%>%
            summarise( w1_19.pymt= sum(Amount[date <= "2018-08-03"],na.rm=T),
                       w2_19.pymt= sum(Amount[date <= "2018-08-10"],na.rm=T),
                       w3_19.pymt= sum(Amount[date <= "2018-08-17"],na.rm=T),
                       w4_19.pymt= sum(Amount[date <= "2018-08-24"],na.rm=T),
                       w5_19.pymt= sum(Amount[date <= "2018-08-31"],na.rm=T),
                       w6_19.pymt= sum(Amount[date <= "2018-09-07"],na.rm=T),                        
                       w7_19.pymt= sum(Amount[date <= "2018-09-14"],na.rm=T),
                       w8_19.pymt= sum(Amount[date <= "2018-09-21"],na.rm=T),
                       w9_19.pymt= sum(Amount[date <= "2018-09-28"],na.rm=T),
                       w10_19.pymt= sum(Amount[date <= "2018-10-05"],na.rm=T),
                       w11_19.pymt= sum(Amount[date <= "2018-10-12"],na.rm=T),
                       w12_19.pymt= sum(Amount[date <= "2018-10-19"],na.rm=T),
                       w13_19.pymt= sum(Amount[date <= "2018-10-26"],na.rm=T),
                       w14_19.pymt= sum(Amount[date <= "2018-11-02"],na.rm=T),
                       w15_19.pymt= sum(Amount[date <= "2018-11-09"],na.rm=T),
                       w16_19.pymt= sum(Amount[date <= "2018-11-16"],na.rm=T),
                       w17_19.pymt= sum(Amount[date <= "2018-11-23"],na.rm=T),
                       w18_19.pymt= sum(Amount[date <= "2018-11-30"],na.rm=T),
                       w19_19.pymt= sum(Amount[date <= "2018-12-07"],na.rm=T),
                       w20_19.pymt= sum(Amount[date <= "2018-12-14"],na.rm=T),
                       w21_19.pymt= sum(Amount[date <= "2018-12-21"],na.rm=T),
                       w22_19.pymt= sum(Amount[date <= "2018-12-28"],na.rm=T),
                       w23_19.pymt= sum(Amount[date <= "2019-01-04"],na.rm=T),
                       w24_19.pymt= sum(Amount[date <= "2019-01-11"],na.rm=T),
                       w25_19.pymt= sum(Amount[date <= "2019-01-18"],na.rm=T),
                       w26_19.pymt= sum(Amount[date <= "2019-01-25"],na.rm=T),
                       w27_19.pymt= sum(Amount[date <= "2019-01-01"],na.rm=T),
                       w28_19.pymt= sum(Amount[date <= "2019-02-08"],na.rm=T),
                       w29_19.pymt= sum(Amount[date <= "2019-02-15"],na.rm=T),
                       w30_19.pymt= sum(Amount[date <= "2019-02-22"],na.rm=T),
                       w31_19.pymt= sum(Amount[date <= "2019-02-01"],na.rm=T),
                       w32_19.pymt= sum(Amount[date <= "2019-03-08"],na.rm=T),
                       w33_19.pymt= sum(Amount[date <= "2019-03-15"],na.rm=T),
                       w34_19.pymt= sum(Amount[date <= "2019-03-22"],na.rm=T),
                       w35_19.pymt= sum(Amount[date <= "2019-03-29"],na.rm=T),
                       w36_19.pymt= sum(Amount[date <= "2019-04-05"],na.rm=T),
                       w37_19.pymt= sum(Amount[date <= "2019-04-12"],na.rm=T),
                       w38_19.pymt= sum(Amount[date <= "2019-04-19"],na.rm=T),
                       w39_19.pymt= sum(Amount[date <= "2019-04-26"],na.rm=T),
                       w40_19.pymt= sum(Amount[date <= "2019-05-03"],na.rm=T),
                       w41_19.pymt= sum(Amount[date <= "2019-05-10"],na.rm=T),
                       w42_19.pymt= sum(Amount[date <= "2019-05-17"],na.rm=T),
                       w43_19.pymt= sum(Amount[date <= "2019-05-24"],na.rm=T),
                       w44_19.pymt= sum(Amount[date <= "2019-05-31"],na.rm=T),
                       w45_19.pymt= sum(Amount[date <= "2019-06-07"],na.rm=T),
                       w46_19.pymt= sum(Amount[date <= "2019-06-14"],na.rm=T),
                       w47_19.pymt= sum(Amount[date <= "2019-06-21"],na.rm=T),
                       w48_19.pymt= sum(Amount[date <= "2019-06-28"],na.rm=T),
                       w49_19.pymt= sum(Amount[date <= "2019-07-05"],na.rm=T),
                       w50_19.pymt= sum(Amount[date <= "2019-07-12"],na.rm=T),
                       w51_19.pymt= sum(Amount[date <= "2019-07-19"],na.rm=T),
                       w52_19.pymt= sum(Amount[date <= "2019-07-26"],na.rm=T))

                           
                          
#to check if it works
print(weekly_rep_clt_19)
View(weekly_rep_clt_19)
sum(v195$Amount[v195$date <= "2019-07-26"])
sum(v195$Amount[v195$date <= "2018-08-03"]) 

#2018 -----------------------
weekly_rep_clt_18 <- v185%>% 
  group_by ()%>%
  summarise (w1_18.pymt= sum(Amount[date <= "2017-08-04"],na.rm=T),
             w2_18.pymt= sum(Amount[date <= "2017-08-11"],na.rm=T),
             w3_18.pymt= sum(Amount[date <= "2017-08-28"],na.rm=T),
             w4_18.pymt= sum(Amount[date <= "2017-08-25"],na.rm=T),
             w5_18.pymt= sum(Amount[date <= "2017-09-01"],na.rm=T),
             w6_18.pymt= sum(Amount[date <= "2017-09-08"],na.rm=T),
             w7_18.pymt= sum(Amount[date <= "2017-09-15"],na.rm=T),
             w8_18.pymt= sum(Amount[date <= "2017-09-22"],na.rm=T),
             w9_18.pymt= sum(Amount[date <= "2017-09-29"],na.rm=T),
             w10_18.pymt= sum(Amount[date <= "2017-10-06"],na.rm=T),
             w11_18.pymt= sum(Amount[date <= "2017-10-13"],na.rm=T),
             w12_18.pymt= sum(Amount[date <= "2017-10-20"],na.rm=T),
             w13_18.pymt= sum(Amount[date <= "2017-10-27"],na.rm=T),
             w14_18.pymt= sum(Amount[date <= "2017-11-03"],na.rm=T),
             w15_18.pymt= sum(Amount[date <= "2017-11-10"],na.rm=T),
             w16_18.pymt= sum(Amount[date <= "2017-11-17"],na.rm=T),
             w17_18.pymt= sum(Amount[date <= "2017-11-24"],na.rm=T),
             w18_18.pymt= sum(Amount[date <= "2017-12-01"],na.rm=T),
             w19_18.pymt= sum(Amount[date <= "2017-12-08"],na.rm=T),
             w20_18.pymt= sum(Amount[date <= "2017-12-15"],na.rm=T),
             w21_18.pymt= sum(Amount[date <= "2017-12-22"],na.rm=T),
             w22_18.pymt= sum(Amount[date <= "2017-12-29"],na.rm=T),
             w23_18.pymt= sum(Amount[date <= "2018-01-05"],na.rm=T),
             w24_18.pymt= sum(Amount[date <= "2018-01-12"],na.rm=T),
             w25_18.pymt= sum(Amount[date <= "2018-01-19"],na.rm=T),
             w26_18.pymt= sum(Amount[date <= "2018-01-26"],na.rm=T),
             w27_18.pymt= sum(Amount[date <= "2018-02-02"],na.rm=T),
             w28_18.pymt= sum(Amount[date <= "2018-02-09"],na.rm=T),
             w29_18.pymt= sum(Amount[date <= "2018-02-16"],na.rm=T),
             w30_18.pymt= sum(Amount[date <= "2018-02-23"],na.rm=T),
             w31_18.pymt= sum(Amount[date <= "2018-03-02"],na.rm=T),
             w32_18.pymt= sum(Amount[date <= "2018-03-09"],na.rm=T),
             w33_18.pymt= sum(Amount[date <= "2018-03-16"],na.rm=T),
             w34_18.pymt= sum(Amount[date <= "2018-03-23"],na.rm=T),
             w35_18.pymt= sum(Amount[date <= "2018-03-30"],na.rm=T),
             w36_18.pymt= sum(Amount[date <= "2018-04-06"],na.rm=T),
             w37_18.pymt= sum(Amount[date <= "2018-04-13"],na.rm=T),
             w38_18.pymt= sum(Amount[date <= "2018-04-20"],na.rm=T),
             w39_18.pymt= sum(Amount[date <= "2018-04-27"],na.rm=T),
             w40_18.pymt= sum(Amount[date <= "2018-05-04"],na.rm=T),
             w41_18.pymt= sum(Amount[date <= "2018-05-11"],na.rm=T),
             w42_18.pymt= sum(Amount[date <= "2018-05-18"],na.rm=T),
             w43_18.pymt= sum(Amount[date <= "2018-05-25"],na.rm=T),
             w44_18.pymt= sum(Amount[date <= "2018-06-01"],na.rm=T),
             w45_18.pymt= sum(Amount[date <= "2018-06-08"],na.rm=T),
             w46_18.pymt= sum(Amount[date <= "2018-06-15"],na.rm=T),
             w47_18.pymt= sum(Amount[date <= "2018-06-22"],na.rm=T),
             w48_18.pymt= sum(Amount[date <= "2018-06-29"],na.rm=T),
             w49_18.pymt= sum(Amount[date <= "2018-07-06"],na.rm=T),
             w50_18.pymt= sum(Amount[date <= "2018-07-13"],na.rm=T),
             w51_18.pymt= sum(Amount[date <= "2018-07-20"],na.rm=T),
             w52_18.pymt= sum(Amount[date <= "2018-07-27"],na.rm=T))



                           
#to check if it works
print(weekly_rep_clt_18)
View(weekly_rep_clt_18)
sum(v185$Amount[v185$date <= "2018-07-27"])

#Efficiency works with Zach

#playing with dates
# wks <- seq(as.Date("2017-08-04"), as.Date("2018-07-27"), by ="week")
# 
# sum(v185$Amount[v185$date <= as.Date("2018-07-27")])
# i = 1
# for (d  in wks) {
#   print(paste(paste('w',i,'_18', sep = ''),as.Date(d), ':', 
#               sum(v185$Amount[v185$date <= as.Date(d)]),sep = " "))
#   i = i + 1
# }
# 
# 
# 
# print(wks)
# 
# for (d in wks) {
#   print(as.Date(d))
#   
# }

#2017 --------------------------------------------------------------------------------

weekly_rep_clt_17 <- v175 %>% 
                      group_by () %>%
                      summarise (w1_17.pymt= sum(Amount[date <= "2016-08-05"],na.rm=T),
                            w2_17.pymt= sum(Amount[date <= "2016-08-12"],na.rm=T),
                            w3_17.pymt= sum(Amount[date <= "2016-08-17"],na.rm=T),
                            w4_17.pymt= sum(Amount[date <= "2016-08-26"],na.rm=T),
                            w5_17.pymt= sum(Amount[date <= "2016-09-02"],na.rm=T),
                            w6_17.pymt= sum(Amount[date <= "2016-09-09"],na.rm=T),
                            w7_17.pymt= sum(Amount[date <= "2016-09-16"],na.rm=T),
                            w8_17.pymt= sum(Amount[date <= "2016-09-23"],na.rm=T),
                            w9_17.pymt= sum(Amount[date <= "2016-09-30"],na.rm=T),
                            w10_17.pymt= sum(Amount[date <= "2016-10-07"],na.rm=T),
                            w11_17.pymt= sum(Amount[date <= "2016-10-14"],na.rm=T),
                            w12_17.pymt= sum(Amount[date <= "2016-10-21"],na.rm=T),
                            w13_17.pymt= sum(Amount[date <= "2016-10-28"],na.rm=T),
                            w14_17.pymt= sum(Amount[date <= "2016-11-04"],na.rm=T),
                            w15_17.pymt= sum(Amount[date <= "2016-11-11"],na.rm=T),
                            w16_17.pymt= sum(Amount[date <= "2016-11-18"],na.rm=T),
                            w17_17.pymt= sum(Amount[date <= "2016-11-25"],na.rm=T),
                            w18_17.pymt= sum(Amount[date <= "2016-12-02"],na.rm=T),
                            w19_17.pymt= sum(Amount[date <= "2016-12-09"],na.rm=T),
                            w20_17.pymt= sum(Amount[date <= "2016-12-16"],na.rm=T),
                            w21_17.pymt= sum(Amount[date <= "2016-12-23"],na.rm=T),
                            w22_17.pymt= sum(Amount[date <= "2016-12-30"],na.rm=T),
                            w23_17.pymt= sum(Amount[date <= "2017-01-09"],na.rm=T),
                            w24_17.pymt= sum(Amount[date <= "2017-01-16"],na.rm=T),
                            w25_17.pymt= sum(Amount[date <= "2017-01-23"],na.rm=T),
                            w26_17.pymt= sum(Amount[date <= "2017-01-30"],na.rm=T),
                            w27_17.pymt= sum(Amount[date <= "2017-02-06"],na.rm=T),
                            w28_17.pymt= sum(Amount[date <= "2017-02-13"],na.rm=T),
                            w29_17.pymt= sum(Amount[date <= "2017-02-20"],na.rm=T),
                            w30_17.pymt= sum(Amount[date <= "2017-02-27"],na.rm=T),
                            w31_17.pymt= sum(Amount[date <= "2017-03-03"],na.rm=T),
                            w32_17.pymt= sum(Amount[date <= "2017-03-10"],na.rm=T),
                            w33_17.pymt= sum(Amount[date <= "2017-03-17"],na.rm=T),
                            w34_17.pymt= sum(Amount[date <= "2017-03-24"],na.rm=T),
                            w35_17.pymt= sum(Amount[date <= "2017-03-31"],na.rm=T),
                            w36_17.pymt= sum(Amount[date <= "2017-04-07"],na.rm=T),
                            w37_17.pymt= sum(Amount[date <= "2017-04-14"],na.rm=T),
                            w38_17.pymt= sum(Amount[date <= "2017-04-21"],na.rm=T),
                            w39_17.pymt= sum(Amount[date <= "2017-04-28"],na.rm=T),
                            w40_17.pymt= sum(Amount[date <= "2017-05-05"],na.rm=T),
                            w41_17.pymt= sum(Amount[date <= "2017-05-12"],na.rm=T),
                            w42_17.pymt= sum(Amount[date <= "2017-05-19"],na.rm=T),
                            w43_17.pymt= sum(Amount[date <= "2017-05-26"],na.rm=T),
                            w44_17.pymt= sum(Amount[date <= "2017-06-02"],na.rm=T),
                            w45_17.pymt= sum(Amount[date <= "2017-06-09"],na.rm=T),
                            w46_17.pymt= sum(Amount[date <= "2017-06-16"],na.rm=T),
                            w47_17.pymt= sum(Amount[date <= "2017-06-23"],na.rm=T),
                            w48_17.pymt= sum(Amount[date <= "2017-06-30"],na.rm=T),
                            w49_17.pymt= sum(Amount[date <= "2017-07-07"],na.rm=T),
                            w50_17.pymt= sum(Amount[date <= "2017-07-14"],na.rm=T),
                            w51_17.pymt= sum(Amount[date <= "2017-07-21"],na.rm=T),
                            w52_17.pymt= sum(Amount[date <= "2017-07-28"],na.rm=T))


#to check if it works
print(weekly_rep_clt_17)
View(weekly_rep_clt_17)
sum(v175$Amount[v175$date <= "2016-08-05"])

#Then, creating column showing total credit for each year

credit_dist_19 <- d192 %>% 
  group_by () %>%
  summarise (tot.credit.19 = sum(TotalCredit),
             tot.a.credit.19 = sum(X2019A_CycleCredit))

dim(credit_dist_19)
#to check if it works
print(credit_dist_19)
sum(d192$TotalCredit)

#For 2018
credit_dist_18 <- d182%>% 
                  group_by ()%>%
                  summarise (tot.credit.18 = sum(TotalCredit),
                             tot.a.credit.18 = sum(X2018A_CycleCredit))

#to check if it works
print(credit_dist_18)
sum(d182$TotalCredit)

#For 2017
credit_dist_17 <- d172%>% 
                  group_by ()%>%
                  summarise (tot.credit.17 = sum(TotalCredit),
                             tot.a.credit.17 = sum(X2017A_CycleCredit))

#to check if it works
print(credit_dist_17)
sum(d172$TotalCredit)

#Now, let's merge all columns

weekly_rep_clt <- merge(weekly_rep_clt_19, weekly_rep_clt_18, all = T)
names(weekly_rep_clt_19)
names(weekly_rep_clt_18)
dim(weekly_rep_clt)
weekly_rep_clt1 <- merge(weekly_rep_clt, weekly_rep_clt_17, all = T)

#to check if it works
dim(weekly_rep_clt_18)
dim(weekly_rep_clt_17)
dim(weekly_rep_clt)
dim(weekly_rep_clt1)

#merge with the 
weekly_rep_clt2 <- merge(weekly_rep_clt1, credit_dist_19, all = T)

#to check if it works
dim(weekly_rep_clt2)
weekly_rep_clt2$tot.credit.19 == sum(d192$TotalCredit)

weekly_rep_clt3 <- merge(weekly_rep_clt2, credit_dist_18, all = T)
weekly_rep_clt4 <- merge(weekly_rep_clt3, credit_dist_17, all = T)

#To check if it works 
dim(weekly_rep_clt4)
View(weekly_rep_clt4)
weekly_rep_clt4$tot.credit.19 == sum(d192$TotalCredit)
weekly_rep_clt4$tot.a.credit.19 == sum(d192$X2019A_CycleCredit)
weekly_rep_clt4$tot.credit.18 == sum(d182$TotalCredit)
weekly_rep_clt4$tot.credit.17 == sum(d172$TotalCredit)

(weekly_rep_clt4$w28_17.pymt + 
    weekly_rep_clt4$w28_18.pymt + 
    weekly_rep_clt4$w28_19.pymt)/(weekly_rep_clt4$tot.credit.19 + 
                                    weekly_rep_clt4$tot.credit.18 + 
                                    weekly_rep_clt4$tot.credit.17)

#Now, let's create the total amount paid for each week
head(weekly_rep_clt4)

weekly_rep_clt4$w1.pymt <- weekly_rep_clt4$w1_19.pymt + 
  weekly_rep_clt4$w1_18.pymt +
  weekly_rep_clt4$w1_17.pymt 
  
#to check if it works
weekly_rep_clt4$w1.pymt/(weekly_rep_clt4$tot.credit.19 + 
                           weekly_rep_clt4$tot.credit.18 + 
                           weekly_rep_clt4$tot.credit.17) #3.86%

weekly_rep_clt4$w2.pymt <- weekly_rep_clt4$w2_19.pymt + 
  weekly_rep_clt4$w2_18.pymt +
  weekly_rep_clt4$w2_17.pymt

weekly_rep_clt4$w3.pymt <- weekly_rep_clt4$w3_19.pymt + 
  weekly_rep_clt4$w3_18.pymt +
  weekly_rep_clt4$w3_17.pymt

weekly_rep_clt4$w4.pymt <- weekly_rep_clt4$w4_19.pymt + 
  weekly_rep_clt4$w4_18.pymt +
  weekly_rep_clt4$w4_17.pymt

weekly_rep_clt4$w5.pymt <- weekly_rep_clt4$w5_19.pymt + 
  weekly_rep_clt4$w5_18.pymt +
  weekly_rep_clt4$w5_17.pymt

weekly_rep_clt4$w6.pymt <- weekly_rep_clt4$w6_19.pymt + 
  weekly_rep_clt4$w6_18.pymt +
  weekly_rep_clt4$w6_17.pymt

weekly_rep_clt4$w7.pymt <- weekly_rep_clt4$w7_19.pymt + 
  weekly_rep_clt4$w7_18.pymt +
  weekly_rep_clt4$w7_17.pymt


weekly_rep_clt4$w8.pymt <- weekly_rep_clt4$w8_19.pymt + 
  weekly_rep_clt4$w8_18.pymt +
  weekly_rep_clt4$w8_17.pymt


weekly_rep_clt4$w9.pymt <- weekly_rep_clt4$w9_19.pymt + 
  weekly_rep_clt4$w9_18.pymt +
  weekly_rep_clt4$w9_17.pymt


weekly_rep_clt4$w10.pymt <- weekly_rep_clt4$w10_19.pymt + 
  weekly_rep_clt4$w10_18.pymt +
  weekly_rep_clt4$w10_17.pymt


weekly_rep_clt4$w11.pymt <- weekly_rep_clt4$w11_19.pymt + 
  weekly_rep_clt4$w11_18.pymt +
  weekly_rep_clt4$w11_17.pymt


weekly_rep_clt4$w12.pymt <- weekly_rep_clt4$w12_19.pymt + 
  weekly_rep_clt4$w12_18.pymt +
  weekly_rep_clt4$w12_17.pymt


weekly_rep_clt4$w13.pymt <- weekly_rep_clt4$w13_19.pymt + 
  weekly_rep_clt4$w13_18.pymt +
  weekly_rep_clt4$w13_17.pymt


weekly_rep_clt4$w14.pymt <- weekly_rep_clt4$w14_19.pymt + 
  weekly_rep_clt4$w14_18.pymt +
  weekly_rep_clt4$w14_17.pymt

weekly_rep_clt4$w15.pymt <- weekly_rep_clt4$w15_19.pymt + 
  weekly_rep_clt4$w15_18.pymt +
  weekly_rep_clt4$w15_17.pymt

weekly_rep_clt4$w16.pymt <- weekly_rep_clt4$w16_19.pymt + 
  weekly_rep_clt4$w16_18.pymt +
  weekly_rep_clt4$w16_17.pymt


weekly_rep_clt4$w17.pymt <- weekly_rep_clt4$w17_19.pymt + 
  weekly_rep_clt4$w17_18.pymt +
  weekly_rep_clt4$w17_17.pymt


weekly_rep_clt4$w18.pymt <- weekly_rep_clt4$w18_19.pymt + 
  weekly_rep_clt4$w18_18.pymt +
  weekly_rep_clt4$w18_17.pymt


weekly_rep_clt4$w19.pymt <- weekly_rep_clt4$w19_19.pymt + 
  weekly_rep_clt4$w19_18.pymt +
  weekly_rep_clt4$w19_17.pymt

weekly_rep_clt4$w20.pymt <- weekly_rep_clt4$w20_19.pymt + 
  weekly_rep_clt4$w20_18.pymt +
  weekly_rep_clt4$w20_17.pymt

weekly_rep_clt4$w21.pymt <- weekly_rep_clt4$w21_19.pymt + 
  weekly_rep_clt4$w21_18.pymt +
  weekly_rep_clt4$w21_17.pymt

weekly_rep_clt4$w22.pymt <- weekly_rep_clt4$w22_19.pymt + 
  weekly_rep_clt4$w22_18.pymt +
  weekly_rep_clt4$w22_17.pymt


weekly_rep_clt4$w23.pymt <- weekly_rep_clt4$w23_19.pymt + 
  weekly_rep_clt4$w23_18.pymt +
  weekly_rep_clt4$w23_17.pymt


weekly_rep_clt4$w24.pymt <- weekly_rep_clt4$w24_19.pymt + 
  weekly_rep_clt4$w24_18.pymt +
  weekly_rep_clt4$w24_17.pymt


weekly_rep_clt4$w25.pymt <- weekly_rep_clt4$w25_19.pymt + 
  weekly_rep_clt4$w25_18.pymt +
  weekly_rep_clt4$w25_17.pymt


weekly_rep_clt4$w26.pymt <- weekly_rep_clt4$w26_19.pymt + 
  weekly_rep_clt4$w26_18.pymt +
  weekly_rep_clt4$w26_17.pymt


weekly_rep_clt4$w27.pymt <- weekly_rep_clt4$w27_19.pymt + 
  weekly_rep_clt4$w27_18.pymt +
  weekly_rep_clt4$w27_17.pymt


weekly_rep_clt4$w28.pymt <- weekly_rep_clt4$w28_19.pymt + 
  weekly_rep_clt4$w28_18.pymt +
  weekly_rep_clt4$w28_17.pymt


weekly_rep_clt4$w29.pymt <- weekly_rep_clt4$w29_19.pymt + 
  weekly_rep_clt4$w29_18.pymt +
  weekly_rep_clt4$w29_17.pymt


weekly_rep_clt4$w30.pymt <- weekly_rep_clt4$w30_19.pymt + 
  weekly_rep_clt4$w30_18.pymt +
  weekly_rep_clt4$w30_17.pymt

weekly_rep_clt4$w31.pymt <- weekly_rep_clt4$w31_19.pymt + 
  weekly_rep_clt4$w31_18.pymt +
  weekly_rep_clt4$w31_17.pymt


weekly_rep_clt4$w32.pymt <- weekly_rep_clt4$w32_19.pymt + 
  weekly_rep_clt4$w32_18.pymt +
  weekly_rep_clt4$w32_17.pymt


weekly_rep_clt4$w33.pymt <- weekly_rep_clt4$w33_19.pymt + 
  weekly_rep_clt4$w33_18.pymt +
  weekly_rep_clt4$w33_17.pymt


weekly_rep_clt4$w34.pymt <- weekly_rep_clt4$w34_19.pymt + 
  weekly_rep_clt4$w34_18.pymt +
  weekly_rep_clt4$w34_17.pymt


weekly_rep_clt4$w35.pymt <- weekly_rep_clt4$w35_19.pymt + 
  weekly_rep_clt4$w35_18.pymt +
  weekly_rep_clt4$w35_17.pymt


weekly_rep_clt4$w36.pymt <- weekly_rep_clt4$w36_19.pymt + 
  weekly_rep_clt4$w36_18.pymt +
  weekly_rep_clt4$w36_17.pymt


weekly_rep_clt4$w37.pymt <- weekly_rep_clt4$w37_19.pymt + 
  weekly_rep_clt4$w37_18.pymt +
  weekly_rep_clt4$w37_17.pymt

weekly_rep_clt4$w38.pymt <- weekly_rep_clt4$w38_19.pymt + 
  weekly_rep_clt4$w38_18.pymt +
  weekly_rep_clt4$w38_17.pymt

weekly_rep_clt4$w39.pymt <- weekly_rep_clt4$w39_19.pymt + 
  weekly_rep_clt4$w39_18.pymt +
  weekly_rep_clt4$w39_17.pymt


weekly_rep_clt4$w40.pymt <- weekly_rep_clt4$w40_19.pymt + 
  weekly_rep_clt4$w40_18.pymt +
  weekly_rep_clt4$w40_17.pymt


weekly_rep_clt4$w41.pymt <- weekly_rep_clt4$w41_19.pymt + 
  weekly_rep_clt4$w41_18.pymt +
  weekly_rep_clt4$w41_17.pymt


weekly_rep_clt4$w42.pymt <- weekly_rep_clt4$w42_19.pymt + 
  weekly_rep_clt4$w42_18.pymt +
  weekly_rep_clt4$w42_17.pymt

weekly_rep_clt4$w43.pymt <- weekly_rep_clt4$w43_19.pymt + 
  weekly_rep_clt4$w43_18.pymt +
  weekly_rep_clt4$w43_17.pymt

weekly_rep_clt4$w44.pymt <- weekly_rep_clt4$w44_19.pymt + 
  weekly_rep_clt4$w44_18.pymt +
  weekly_rep_clt4$w44_17.pymt

weekly_rep_clt4$w45.pymt <- weekly_rep_clt4$w45_19.pymt + 
  weekly_rep_clt4$w45_18.pymt +
  weekly_rep_clt4$w45_17.pymt


weekly_rep_clt4$w46.pymt <- weekly_rep_clt4$w46_19.pymt + 
  weekly_rep_clt4$w46_18.pymt +
  weekly_rep_clt4$w46_17.pymt


weekly_rep_clt4$w47.pymt <- weekly_rep_clt4$w47_19.pymt + 
  weekly_rep_clt4$w47_18.pymt +
  weekly_rep_clt4$w47_17.pymt


weekly_rep_clt4$w48.pymt <- weekly_rep_clt4$w48_19.pymt + 
  weekly_rep_clt4$w48_18.pymt +
  weekly_rep_clt4$w48_17.pymt


weekly_rep_clt4$w49.pymt <- weekly_rep_clt4$w49_19.pymt + 
  weekly_rep_clt4$w49_18.pymt +
  weekly_rep_clt4$w49_17.pymt


weekly_rep_clt4$w50.pymt <- weekly_rep_clt4$w50_19.pymt + 
  weekly_rep_clt4$w50_18.pymt +
  weekly_rep_clt4$w50_17.pymt


weekly_rep_clt4$w51.pymt <- weekly_rep_clt4$w51_19.pymt + 
  weekly_rep_clt4$w51_18.pymt +
  weekly_rep_clt4$w51_17.pymt


weekly_rep_clt4$w52.pymt <- weekly_rep_clt4$w52_19.pymt + 
  weekly_rep_clt4$w52_18.pymt +
  weekly_rep_clt4$w52_17.pymt


weekly_rep_clt4$tot.credit <- weekly_rep_clt4$tot.credit.19 + 
                              weekly_rep_clt4$tot.credit.18 + 
                              weekly_rep_clt4$tot.credit.17

weekly_rep_clt4$tot.a.credit <- weekly_rep_clt4$tot.a.credit.19 + 
                              weekly_rep_clt4$tot.a.credit.18 + 
                              weekly_rep_clt4$tot.a.credit.17


weekly_rep_path <- weekly_rep_clt4 %>% 
                   group_by () %>%
                   summarise(w1_rep = 100*sum(w1.pymt)/sum(tot.a.credit),
                             w2_rep = 100*sum(w2.pymt)/sum(tot.a.credit),
                             w3_rep = 100*sum(w3.pymt)/sum(tot.a.credit),
                             w4_rep = 100*sum(w4.pymt)/sum(tot.a.credit),
                             w5_rep = 100*sum(w5.pymt)/sum(tot.a.credit),
                             w6_rep = 100*sum(w6.pymt)/sum(tot.a.credit),
                             w7_rep = 100*sum(w7.pymt)/sum(tot.a.credit),
                             w8_rep = 100*sum(w8.pymt)/sum(tot.a.credit),
                             w9_rep = 100*sum(w9.pymt)/sum(tot.a.credit),
                             w10_rep = 100*sum(w10.pymt)/sum(tot.a.credit),
                             w11_rep = 100*sum(w11.pymt)/sum(tot.a.credit),
                             w12_rep = 100*sum(w12.pymt)/sum(tot.a.credit),
                             w13_rep = 100*sum(w13.pymt)/sum(tot.a.credit),
                             w14_rep = 100*sum(w14.pymt)/sum(tot.a.credit),
                             w15_rep = 100*sum(w15.pymt)/sum(tot.a.credit),
                             w16_rep = 100*sum(w16.pymt)/sum(tot.a.credit),
                             w17_rep = 100*sum(w17.pymt)/sum(tot.a.credit),
                             w18_rep = 100*sum(w18.pymt)/sum(tot.a.credit),
                             w19_rep = 100*sum(w19.pymt)/sum(tot.a.credit),
                             w20_rep = 100*sum(w20.pymt)/sum(tot.a.credit),
                             w21_rep = 100*sum(w21.pymt)/sum(tot.a.credit),
                             w22_rep = 100*sum(w22.pymt)/sum(tot.a.credit),
                             w23_rep = 100*sum(w23.pymt)/sum(tot.a.credit),
                             w24_rep = 100*sum(w24.pymt)/sum(tot.credit),
                             w25_rep = 100*sum(w25.pymt)/sum(tot.credit),
                             w26_rep = 100*sum(w26.pymt)/sum(tot.credit),
                             w27_rep = 100*sum(w27.pymt)/sum(tot.credit),
                             w28_rep = 100*sum(w28.pymt)/sum(tot.credit),
                             w29_rep = 100*sum(w29.pymt)/sum(tot.credit),
                             w30_rep = 100*sum(w30.pymt)/sum(tot.credit),
                             w31_rep = 100*sum(w31.pymt)/sum(tot.credit),
                             w32_rep = 100*sum(w32.pymt)/sum(tot.credit),
                             w33_rep = 100*sum(w33.pymt)/sum(tot.credit),
                             w34_rep = 100*sum(w34.pymt)/sum(tot.credit),
                             w35_rep = 100*sum(w35.pymt)/sum(tot.credit),
                             w36_rep = 100*sum(w36.pymt)/sum(tot.credit),
                             w37_rep = 100*sum(w37.pymt)/sum(tot.credit),
                             w38_rep = 100*sum(w38.pymt)/sum(tot.credit),
                             w39_rep = 100*sum(w39.pymt)/sum(tot.credit),
                             w40_rep = 100*sum(w40.pymt)/sum(tot.credit),
                             w41_rep = 100*sum(w41.pymt)/sum(tot.credit),
                             w42_rep = 100*sum(w42.pymt)/sum(tot.credit),
                             w43_rep = 100*sum(w43.pymt)/sum(tot.credit),
                             w44_rep = 100*sum(w44.pymt)/sum(tot.credit),
                             w45_rep = 100*sum(w45.pymt)/sum(tot.credit),
                             w46_rep = 100*sum(w46.pymt)/sum(tot.credit),
                             w47_rep = 100*sum(w47.pymt)/sum(tot.credit),
                             w48_rep = 100*sum(w48.pymt)/sum(tot.credit),
                             w49_rep = 100*sum(w49.pymt)/sum(tot.credit),
                             w50_rep = 100*sum(w50.pymt)/sum(tot.credit),
                             w51_rep = 100*sum(w51.pymt)/sum(tot.credit),
                             w52_rep = ifelse(100*sum(w52.pymt)/sum(tot.credit) > 100 ,100))

print(weekly_rep_path)
View(weekly_rep_path)

########################################################################################
# Exporting and saving final outputs
########################################################################################

write.table(weekly_rep_path, file = paste(od, "weekly_rep_path_9.24.19.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")


#saving
save(weekly_rep_path, file = paste(wd,"weekly_rep_path.RData", sep ="/"))


#---------------------------------- End ------------------------------------------------
