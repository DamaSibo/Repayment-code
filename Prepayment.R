######## Prepayment Tracker ########
#group_by products 
# totl of ag.pro= Maize + Fertilizers 
#solar= solar products + 
#Chicken == RF 500 per chick
#Trave = travertine
#Rest >> other products 
     #Strong start <<< normal payments plus 5000 RF
     #If your credit is less than 5000 you have to finish your loan

#Calculating total credit
#d21$tot.creditAd <-  d21$X2021A_Maize.kg*950 
#d21$TotalCredit <- d21$TotalCredit + d21$tot.creditAd
#new percentage repaix new
#d21$X..Repaid2 <- round(100*(d21$TotalRepaid_IncludingOverpayments/d21$TotalCredit),1)
#changing the % repaid TO 100
#d21$X..Repaid2 <- ifelse(d21$X..Repaid2 > 100, 100, d21$X..Repaid2)
#clear environment
rm(list = ls()); cat("\014")

#Setting data and output directories
dd <-  "D:/Drop analysis/Repayment/data"
od <-  "D:/Drop analysis/Repayment/output"

# load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)

#loading objects/data/data frame

d21 <- read.csv(paste(dd, "SC_2021_2020.09.15.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")
#Removing trials
colnames(d21)
d21 <- d21 %>% select(-ends_with("Trial.qty"))
#Sum of all products
colnames(d21)
d21$sumpro <- rowSums(d21[,48:75])
#Remove those without products
length(unique(d21$GlobalClientID[d21$sumpro == 0]))
d21 <- subset(d21, sumpro > 0)
#Total credit
sum(d21$TotalCredit)
#Summing the products based on the group
df <- d21 %>% 
  mutate(AG = X2021A_DAP.kg + 
           X2021A_Maize.kg + 
           X2021A_NPK.17.kg + 
           X2021A_Pan.53.kg + 
           X2021A_Pan.691.kg + 
           X2021A_SC.403.kg + 
           X2021A_SC.637.kg + 
           X2021A_UREA.kg + 
           X2021A_WH.403.kg + 
           X2021A_WH.505.kg +
           X2021A_WH.605.kg,
         Veg = X2021A_TUN.qty + 
           X2021A_TOM.qty + 
           X2021A_KAR.qty + 
           X2021A_SHU.qty + X2021A_POV.qty,
         Pics = X2021A_PICS100KG.qty + X2021A_PICS50KG.qty,
         Pico = X2021A_GLP.Pico.qty,
         Chic = X2021A_Inkoko.qty,
         Avoc = X2021A_Avoka.qty,
         Phn = X2021A_Tecno.qty,
         Solar = X2021A_Biolite.SHS.qty + 
           X2021A_GLP.SKP.200.qty + 
           X2021A_DLight.S200.qty + 
           X2021A_Niwa.300XL.qty,
         Trav = X2021A_Travertine.kg)

#subset total credit more than 3000
df1 <- subset(df, TotalCredit >= 3000)
#subset total credit less than 3000
low <- subset(df, TotalCredit < 3000)

#Calculating prepayment conditions without avocado+10 and chicken 
df2 <- df1 %>% mutate(prepayment = if_else (AG >= 100 & Solar >= 1 , "12000",
                                            if_else(AG < 100 & AG >0 & Solar >= 1, "8000", 
                                                    if_else(AG >= 100 & Solar ==0 , "7000", 
                                                            if_else(AG==0 & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 25 & Avoc ==0 & Veg==0 & Pics==0, "2500",
                                                                    if_else(Avoc < 10 & Avoc > 0 & (AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0) , "1000",
                                                                            if_else(Avoc >= 10 & (AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0 & Chic==0), "500",
                                                                                    if_else(Avoc == 0 & AG == 0  & Solar == 0 & Pico == 0 & Phn == 0 & Trav == 0 & Veg==0 & Chic >= 1, "0",
                                                                                            if_else(AG == 0 & Solar >=1 | Pico >2, "5000",
                                                                                                    if_else(AG < 100 & Solar == 0, "3000", "NA"))))))))))
 


#Calculating prepayment conditions with avocado+10 
#Changing the format on prepayment to numeric
df2$prepayment <- as.numeric(df2$prepayment)
# With Avocat +10
df2$prepaymentAV <- ifelse(df2$Avoc >= 10 , df2$prepayment +1000 ,df2$prepayment)
#With Chicken
df2$prepaymentFi <- ifelse(df2$Chic >=1, (df2$prepaymentAV+(df2$Chic*500)) ,df2$prepaymentAV)
#Re_adding the clients with less than 3000 credit
#### Working with Qualifications####
 #Conditions: Non Starters: Clients who have yet to make any payment towards 21A
 #Starters: Clients who have made a payment, but not yet reached their qualification amount
 #Qualifiers (Minimum): Clients who have met the minimum prepayment amount
 #Qualifiers (Full):Clients who have met their prepayment amount in full
 #Strong Starters: Clients who have paid RF 5000 more than their prepayment amount.

#Adding categories for clients above 3000 credit 
df3 <- df2 %>% mutate(Qual=
                        if_else(TotalRepaid_IncludingOverpayments <100, "Non.starter",
                                if_else(prepaymentFi >= 3000 & (TotalRepaid_IncludingOverpayments >= 100 & TotalRepaid_IncludingOverpayments < 3000 & X..Repaid < 100), "Starters",
                                        if_else(TotalRepaid_IncludingOverpayments >= 3000 & TotalRepaid_IncludingOverpayments < prepaymentFi & X..Repaid < 100, "Qual.min",
                                                      if_else(TotalRepaid_IncludingOverpayments >= prepaymentFi & X..Repaid < 100, "Qual.full",
                                                                if_else(prepaymentFi < 3000 & (TotalRepaid_IncludingOverpayments >= 100 & X..Repaid < 100 & TotalRepaid_IncludingOverpayments < prepaymentFi)  , "Starters",
                                                                        if_else(X..Repaid ==100 , "Finishers", "NA")))))))


#Adding categories to clients with bellow 3000 credit
low$prepaymentFi <- low$TotalCredit
sum(low$prepaymentFi)
sum(df3$prepaymentFi)
sum(df3$prepaymentFi)+sum(low$prepaymentFi)
low <- low %>% mutate(Qual=
                        if_else(TotalRepaid_IncludingOverpayments <100, "Non.starter",
                                if_else(TotalRepaid_IncludingOverpayments >= 100 & TotalRepaid_IncludingOverpayments < prepaymentFi & X..Repaid < 100 , "Starters",
                                                if_else(TotalRepaid_IncludingOverpayments >= prepaymentFi & X..Repaid < 100, "Qual.full",
                                                                if_else(X..Repaid ==100 , "Finishers", "NA")))))





table(low$Qual)
#Readding two dataframes
  #Adding empty columns to bind together these two datasets
low$prepayment <- 0
low$prepaymentAV <- 0
low$prepaymentFi <- low$TotalCredit
df4 <- rbind(df3, low)
# Adding a new column of Strong starters
df4$QualF <- ifelse(df4$TotalRepaid_IncludingOverpayments >= df4$prepaymentFi + 5000 , 1, df4$Qual )
table(df4$QualF)

#####Analysis of the site level
sitelevel <- df4 %>%
  group_by(site.id, DistrictName, SiteName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )
###Distrist level
district <- df4 %>%
  group_by(RegionName,DistrictName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )
### Region Level
district <- df4 %>%
  group_by(RegionName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )
############# carried over clients #################
d21c <- subset(df4, X2021A_Enrollment.Fee.adjustment > 0)
table(d21c$QualF)
## Only non starters carried over
carriedno <- subset(d21c,QualF == "Non.starter" & !is.na(ClientPhone))
carriedno <- carriedno %>% select(c("DistrictName" ,"SiteName","LastName" ,"FirstName","OAFID","ClientPhone"))
write_csv(carriedno, "carriedovernonsta.csv")

################# SMS to clients in various sites ####
schedule <- read.csv("sitedelivery2.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "") 
#check sites that have Loaded and delivered
length(unique(schedule$Site.id[schedule$Delivery.Status == "Delivered" | schedule$Delivery.Status == "Loaded"]))
#Adding deliverly status
##Site id
df4$Site.id <- paste(df4$DistrictName, df4$SiteName, sep = "")
df4$Delivery.Status <- schedule$Delivery.Status[match(df4$Site.id, schedule$Site.id)]
#Changing the dates formats
v21$Delivery.Dates <- as.Date(v21$Delivery.Dates, "%Y-%m-%d")
##Adding the schedule also to the transactions
v21$Delivery.Status <- schedule$Delivery.Status[match(v21$site.id, schedule$site.id)]

#Subseting clients with phone numbers and delivered or loaded
delivery <- subset(df4,Delivery.Status == "Delivered" |
                       Delivery.Status == "Loaded")
#Adding phone numbers
delivery$ClientPhone2 <- d21B$ClientPhone[match(delivery$GlobalClientID, d21B$GlobalClientID)]
#Selecting clients who have phone
delivery <- subset(delivery, !is.na(ClientPhone))
colnames(delivery)
delivery <- delivery[,c("RegionName" ,"DistrictName","SiteName","ClientPhone2","prepaymentFi","QualF", "Delivery.Status" )]
#Subseting all the categories
nonstarters <- subset(delivery, QualF == "Non.starter")
starters <- subset(delivery, QualF == "Starters")
qualified.min <- subset(delivery, QualF == "Qual.min")
qualified.full <- subset(delivery, QualF == "Qual.full")
strong.starters <- subset(delivery, QualF == "1" | QualF == "Finishers")
#Writing the CSV
setwd("/Users/jimmymarcel/Documents/One Acre Fund/DATA_FILES/Key Stats/list_for_SMS")
write.csv(nonstarters, "Non.starters.csv")
write.csv(starters, "Starters.csv")
write.csv(qualified.min, "qualified.minim.csv")
write.csv(qualified.full, "qualified.full.csv")
write.csv(strong.starters, "strong.starters.csv")

######### farmer level HP#######
# we exclude finishers and strong starters
df5 <- subset(df4, QualF != "1" & QualF != "Finishers")
#Adding a column of required amount to be strong start
df5$topay <- ifelse(df5$TotalCredit > 5000, (df5$prepaymentFi + 5000), df5$prepaymentFi)
#adding the colum to show the remaining to be paid
df5 <- df5 %>% mutate(Remaining.tpay = topay - TotalRepaid_IncludingOverpayments)
colnames(df5)
df5 <- df5[,c(3,6,9,10,11,19,26,30,88,92)]
df5 <- subset(df5, !is.na(ClientPhone))
write.csv(df5, "data.remaingtopay.csv")
#Farmers with 0 prepayment
length(unique(df4$GlobalClientID[df4$TotalRepaid_IncludingOverpayments == 0]))

####### Site before and after pick up ####
#Subsetting the delivered and loaded sites
delivery <- subset(schedule,Delivery.Status == "Delivered" |
                     Delivery.Status == "Loaded")
##Adding delivery date to the transactions and delivery status
df4$Delivery.Dates<- schedule$Delivery.Dates[match(df4$site.id, schedule$Site.id)]
## Adding delivery status
df4$Delivery.Status <- schedule$Delivery.Status[match(df4$site.id, schedule$Site.id)]
#Before the delivery on August 13the and after delivery(after 13th)
#After delivery
sitedeliverybefore <- df4 %>%
  group_by(site.id, RegionName, DistrictName, SiteName, Delivery.Status) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    starters = length(unique(GlobalClientID)[QualF == "Non.starter"]),
    MinimumQualifiers = length(unique(GlobalClientID)[QualF == "Qual.min"]),
    FullQualifiers = length(unique(GlobalClientID)[QualF == "Qual.full"]),
    Strong.starters = length(unique(GlobalClientID)[QualF == "1"| QualF=="Finishers"])
  )
write.csv(sitedelivery, "Sitedeliveryafter.csv")
write.csv(sitedeliverybefore, "Sitedeliverybefore.csv")

###matching with dama's results
df4$dama <- dama$Prep.required[match(df4$AccountNumber, dama$AccountNumber)]
df4$check <- ifelse(df4$prepaymentFi != df4$dama , 1, 0)
check <- subset(df4, check== "1")
colnames(check)
check <- check[,c(3,6,31,78:93)]
write_csv(check, "Check.csv")
