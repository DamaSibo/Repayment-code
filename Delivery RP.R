# Prepayment performance in delivery
#clear environment
rm(list = ls()); cat("\014")

#Setting data and output directories
dd <-  "D:/Drop analysis/Repayment/data"
od <-  "D:/Drop analysis/Repayment/output"

# load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)

#loading objects/data/data frame
d21 <- read.csv(paste(dd, "SC_2021_2020.08.24.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

v21 <- read.csv(paste(dd, "VR_2021_2020.08.25.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

dsc <- read.csv(paste(dd, "21A Official Delivery Schedule_25_Aug_2020.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

#------------------------------------------------------------------------------------------
# Total clients
d21$total.clients <- ifelse((d21$X2021A_Avoka.qty >0                  
                             |d21$X2021A_Biolite.SHS.qty >0            
                             |d21$X2021A_DAP.kg >0                     
                             |d21$X2021A_DLight.S200.qty >0         
                             |d21$X2021A_GLP.Pico.qty >0         
                             |d21$X2021A_GLP.SKP.200.qty >0            
                             |d21$X2021A_Inkoko.qty >0                 
                             |d21$X2021A_KAR.qty >0                    
                             |d21$X2021A_Maize.kg >0                   
                             |d21$X2021A_Niwa.300XL.qty >0       
                             |d21$X2021A_NPK.17.kg >0                  
                             |d21$X2021A_Pan.53.kg >0                  
                             |d21$X2021A_Pan.691.kg >0                 
                             |d21$X2021A_PICS100KG.qty >0      
                             |d21$X2021A_PICS50KG.qty >0              
                             |d21$X2021A_POV.qty >0                    
                             |d21$X2021A_SC.403.kg >0                  
                             |d21$X2021A_SC.637.kg >0                  
                             |d21$X2021A_SHU.qty >0                    
                             |d21$X2021A_Tecno.qty >0                  
                             |d21$X2021A_TOM.qty >0                    
                             |d21$X2021A_Travertine.kg >0              
                             |d21$X2021A_TUN.qty >0                    
                             |d21$X2021A_UREA.kg >0                    
                             |d21$X2021A_WH.403.kg >0                  
                             |d21$X2021A_WH.505.kg >0                  
                             |d21$X2021A_WH.605.kg >0),1,0)

#check
length(unique(d21$GlobalClientID[d21$total.clients==0]))
table(d21$total.clients, useNA = "ifany")
d21 <- subset(d21, d21$total.clients==1)

#d21 <- select(d21,DistrictName,SiteName,GroupName,LastName,FirstName,OAFID,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,TotalRepaid_IncludingOverpayments,
 #             GlobalClientID)

#To put dates in standard format,
#2020
class(v21$RepaymentDate)
v21$date <- substr(v21$RepaymentDate, 1, 10)
v21$date <- gsub(" ", "", v21$date)
v21$date <- gsub("-", "/", v21$date)
v21$date <- as.Date(v21$date)
class(v21$date)

# Delivery
class(dsc$Delivery.Dates )
dsc$date <- substr(dsc$Delivery.Dates, 1, 10)
dsc$date <- gsub(" ", "", dsc$date)
dsc$date <- gsub("-", "/", dsc$date)
dsc$date <- as.Date(dsc$date)
class(dsc$date)

# Create unique site
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = ".")
v21$site.id <- paste(v21$District, v21$Site, sep = ".")

#check
names(dsc)
show(d21$site.id)
show(dsc$site.id)
show(v21$site.id)

#Now, let see if sites in the distribution schedule all matches sites in SCs and VR
#before checking which sites in SC and VR matches the ones we have in the dist sched.

dsc$site.id <- ifelse(dsc$site.id == "Huye.Mwurire", "Huye.MWURIRE",dsc$site.id)
dsc$site.id <- ifelse(dsc$site.id == "Huye.Rusagara", "Huye.rusagara",dsc$site.id)
dsc$site.id <- ifelse(dsc$site.id == "Karongi.RUBUMBA", "Karongi.Rubumba",dsc$site.id)
dsc$site.id <- ifelse(dsc$site.id == "Nyaruguru.nyamabuye", "Nyaruguru.Nyamabuye",dsc$site.id)
#dsc-sc
test <- merge(data.frame(id = unique((dsc$site.id)), x = 1),
              data.frame(id = unique(d21$site.id), y = 1), by = "id", all = T)

test$dif <- test$x + test$y

test2 <- test[which(is.na(test$dif)), ]
nrow(test2)
test2
length(unique(test2$id[!is.na(test2$x)])) #means, all sites in dist schedule = in SC


#dsc-v
test <- merge(data.frame(id = unique((dsc$site.id)), x = 1),
              data.frame(id = unique(v21$site.id), y = 1), by = "id", all = T)

test$dif <- test$x + test$y

test2 <- test[which(is.na(test$dif)), ]
nrow(test2)
test2
length(unique(test2$id[!is.na(test2$x)])) #means, all sites in dist schedule = in SC
#----------------------------------------------------------------------------------
v21$delivery.status <- dsc$Delivery.Status[match(v21$site.id, dsc$site.id)]
v21$delivery.weeks <- dsc$Delivery.Week[match(v21$site.id, dsc$site.id)]
table(v21$delivery.status, useNA = "ifany")
table(v21$delivery.weeks, useNA = "ifany")

d21$delivery.status <- dsc$Delivery.Status[match(d21$site.id, dsc$site.id)]
d21$delivery.weeks <- dsc$Delivery.Week[match(d21$site.id, dsc$site.id)]
table(d21$delivery.status, useNA = "ifany")
table(d21$delivery.weeks, useNA = "ifany")

# weekly prepayment
# Before
week1 <-subset(v21, v21$date > "2020-07-19" & v21$date <= "2020-07-27")
week1.27.Jul.pymt <- week1%>% group_by(GlobalClientID)%>% summarise(week1.pymt=sum(Amount))

week2 <-subset(v21, v21$date > "2020-07-27" & v21$date <= "2020-08-03")
week2.03.Aug.pymt <- week2%>% group_by(GlobalClientID)%>% summarise(week1.pymt=sum(Amount))

week3 <-subset(v21, v21$date > "2020-08-03" & v21$date <= "2020-08-10")
week3.10.Aug.pymt <- week2%>% group_by(GlobalClientID)%>% summarise(week1.pymt=sum(Amount))

# After
week4 <-subset(v21, v21$date > "2020-08-10" & v21$date <= "2020-08-17")
week4.17.Aug.pymt <- week2%>% group_by(GlobalClientID)%>% summarise(week1.pymt=sum(Amount))

week5 <-subset(v21, v21$date > "2020-08-17" & v21$date <= "2020-08-24")
week5.24.Aug.pymt <- week2%>% group_by(GlobalClientID)%>% summarise(week1.pymt=sum(Amount))
#-----------------------------------------------------------------------------------------------------
v21$weekly <- ifelse( v21$date <= "2020-07-19","Before","")
v21$weekly <- ifelse( v21$date > "2020-07-19" & v21$date <= "2020-07-27","July-27",v21$weekly)
v21$weekly <- ifelse(v21$date > "2020-07-27" & v21$date <= "2020-08-03","Aug-03",v21$weekly)
v21$weekly <- ifelse(v21$date > "2020-08-03" & v21$date <= "2020-08-10","Aug-10",v21$weekly)
v21$weekly <- ifelse(v21$date > "2020-08-10" & v21$date <= "2020-08-17","Aug-17",v21$weekly)
v21$weekly <- ifelse(v21$date > "2020-08-17" & v21$date <= "2020-08-24","Aug-24",v21$weekly)

table(v21$weekly, useNA = "ifany")
names(v21)
table(v21$delivery.status)
v211 <- v21%>% group_by(District,Site, delivery.status, weekly)%>% summarise(
                         tot.made.pymt = length(unique(GlobalClientID)),
                         tot.Amount = sum(Amount))
View(v211)

write.table(v211,file = paste(od, "Delivery trend.CSV", sep = "/"), 
            row.names = FALSE, col.names = TRUE, sep = ",")



