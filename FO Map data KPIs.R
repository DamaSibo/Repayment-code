### FO Map data update


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
sum(d20$TotalRepaid)/sum(d20$TotalCredit)

d21 <- read.csv(paste(dd, "SC_2021_2020.07.29.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
nrow(d21)
length(unique(d21$GlobalClientID[d21$TotalCredit>0]))

#---------------------------------------------------------------------------------------
# add group unique
d20$grp.id <- paste(d20$DistrictName, d20$SiteName, d20$GroupName, sep = ".")
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = ".")

# select necessary column
names(d20)
d20 <- select(d20,grp.id,SeasonName, DistrictName, SiteName, GroupName,LastName, FirstName, Facilitator, OAFID,AccountNumber,GlobalClientID,
              TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,LastRepayment)
#d21 <- select(d21,SeasonName, DistrictName, SiteName, GroupName, grp.id,OAFID,AccountNumber,GlobalClientID,
 #             TotalCredit,TotalRepaid,RemainingCredit,X..Repaid)
# Re-enrolled clients
d20$match <- match(d20$AccountNumber, d21$AccountNumber, nomatch = 0)
#check if it works
names(d20)
nrow(d20)
nrow(d21)
length(unique(d20$AccountNumber[d20$match == 0]))
length(unique(d20$AccountNumber[d20$match > 0]))
length(unique(d20$AccountNumber[d20$match > 0]))/nrow(d20)

#d201 <- subset(d20, d20$match >0)
#nrow(d201)

d20$re.enrolled.21a <- ifelse(d20$match >0 , 1, 0)
table(d20$re.enrolled.21a, useNA = "ifany")

# check
length(unique(d20$AccountNumber[d20$X..Repaid==100]))
length(unique(d20$grp.id))
length(unique(d20$AccountNumber[d20$re.enrolled.21a ==1 & d20$X..Repaid==100]))
#------------------------------------------------------------------------------------------
# Site level
d201 <- d20 %>% group_by(DistrictName,SiteName) %>% summarise(
                        tot.clt = length(unique(AccountNumber)),
                        tot.grp = length(unique(grp.id)),
                        tot.clt.100 = length(unique(AccountNumber[X..Repaid==100])),
                        clt.reenrolled.21a = sum(re.enrolled.21a),
                        clt.reenrolled.100 = length(unique(AccountNumber[re.enrolled.21a ==1 & X..Repaid==100])),
                        x.repaid = (sum(TotalRepaid)/sum(TotalCredit))*100,
                        )



View(d201)
sum(d201$tot.clt)
sum(d201$tot.grp)
sum(d201$tot.clt.100)
sum(d201$clt.reenrolled.21a)
sum(d201$clt.reenrolled.100)

# 21A Clients
names(d21)
d21$A.clts.21 <- ifelse(d21$X2021A_Avoka.qty >0                
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
                         |d21$X2021A_PICS.100KG.qty >0             
                         |d21$X2021A_PICS.50KG.qty >0              
                         |d21$X2021A_POV.qty >0                    
                         |d21$X2021A_SHU.qty >0                    
                         |d21$X2021A_Tecno.qty >0                  
                         |d21$X2021A_TOM.qty >0                    
                         |d21$X2021A_Travertine.kg >0              
                         |d21$X2021A_TUN.qty >0                    
                         |d21$X2021A_UREA.kg >0,1,0)
table(d21$A.clts.21, useNA = "ifany")

d211 <- d21 %>% group_by(DistrictName,SiteName) %>% summarise(
                       tot.clt = sum(A.clts.21))

d21$tot.credit <- d21$X2021A_Avoka.qty* 500 +                
                  d21$X2021A_Biolite.SHS.qty*85000 +             
                  d21$X2021A_DAP.kg* 480 +                      
                  d21$X2021A_DLight.S200.qty* 25000 +            
                  d21$X2021A_GLP.Pico.qty* 9000 +                
                  d21$X2021A_GLP.SKP.200.qty* 25000 +            
                  d21$X2021A_Inkoko.qty* 500 +                  
                  d21$X2021A_KAR.qty* 130 +                    
                  d21$X2021A_Maize.kg* 950 +                    
                  d21$X2021A_Niwa.300XL.qty* 35000 +              
                  d21$X2021A_NPK.17.kg* 603 +                  
                  d21$X2021A_PICS.100KG.qty*  2240 +            
                  d21$X2021A_PICS.50KG.qty* 1800 +              
                  d21$X2021A_POV.qty* 130 +                    
                  d21$X2021A_SHU.qty* 130 +                    
                  d21$X2021A_Tecno.qty* 12000 +                  
                  d21$X2021A_TOM.qty* 130 +                    
                  d21$X2021A_Travertine.kg* 100 +              
                  d21$X2021A_TUN.qty* 130 +                    
                  d21$X2021A_UREA.kg* 447


sum(d21$tot.credit)
sum(d21$X2021A_Enrollment.Fee.adjustment)

d21$total.credit <- d21$tot.credit + d21$X2021A_Enrollment.Fee.adjustment

sum(d21$total.credit)

length(unique(d21$GlobalClientID[d21$total.credit <=10000]))
length(unique(d21$GlobalClientID[d21$total.credit <=8000]))
length(unique(d21$GlobalClientID[d21$total.credit <=3000]))
View(d211)

length(unique(d21$GlobalClientID[d21$A.clts.21 >0]))
sum(d21$total.credit)/length(unique(d21$GlobalClientID[d21$A.clts.21 >0]))


write.table(d211, file = paste(od, "21A Clients enrolled.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d201, file = paste(od, "20AB client List.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

# Group level
## Client enrolled within groups finished 100%
d202 <- d20 %>% group_by(grp.id, DistrictName,SiteName, GroupName) %>% summarise(
                        tot.Credit = sum(TotalCredit),
                        tot.Repaid = sum(TotalRepaid),
                        tot.clt = length(unique(AccountNumber)),
                        tot.clt.100 = length(unique(AccountNumber[X..Repaid==100])),
                        clt.reenrolled.21a = sum(re.enrolled.21a),
                        clt.reenrolled.100 = length(unique(AccountNumber[re.enrolled.21a ==1 & X..Repaid==100])),
                        x.repaid = (sum(TotalRepaid)/sum(TotalCredit))*100,
                        )

# group enrolled in 21a
d202$grp.clt.enrolled.21a <- ifelse(d202$clt.reenrolled.21a >0, 1,0)
table(d202$grp.clt.enrolled.21a, useNA = "ifany")
# group enrolled in 21a but not yet finished 100%
d202$group.not.100.clt.enro <- ifelse(d202$grp.clt.enrolled.21a==1 & d202$x.repaid <100, 1,0)
table(d202$group.not.100.clt.enro, useNA = "ifany")

View(d202)

# back to site level
nrow(d202)
names(d202)
length(unique(d202$grp.id[d202$x.repaid == 100]))
length(unique(d202$grp.id[d202$x.repaid < 100]))
length(unique(d202$grp.id[d202$x.repaid < 100 & d202$clt.reenrolled.21a >0]))

# site id
d202$site.id <- paste(d202$DistrictName, d202$SiteName, sept="")
d203 <- d202 %>% group_by(site.id,DistrictName,SiteName) %>% summarise(
                        tot.clt = sum((tot.clt)),
                        tot.grp = length(unique(grp.id)),
                        tot.clt.100 = sum(tot.clt.100),
                        clt.reenrolled.21a = sum(clt.reenrolled.21a),
                        clt.reenrolled.100 = sum(clt.reenrolled.100),
                        grp.clt.enrolled.21a = sum(grp.clt.enrolled.21a),
                        group.not.100.clt.enro = sum(group.not.100.clt.enro),
                        x.repaid = (sum(tot.Repaid)/sum(tot.Credit))*100)
                        



sum(d203$grp.clt.enrolled.21a)
sum(d203$group.not.100.clt.enro)
View(d203)

# District Level
d204 <- d202 %>% group_by(DistrictName) %>% summarise(
                         tot.clt = sum((tot.clt)),
                         tot.grp = length(unique(grp.id)),
                         tot.clt.100 = sum(tot.clt.100),
                         clt.reenrolled.21a = sum(clt.reenrolled.21a),
                         clt.reenrolled.100 = sum(clt.reenrolled.100),
                         grp.clt.enrolled.21a = sum(grp.clt.enrolled.21a),
                         group.not.100.clt.enro = sum(group.not.100.clt.enro),
                         x.repaid = (sum(tot.Repaid)/sum(tot.Credit))*100)


# country level
d205 <- d202 %>% group_by( ) %>% summarise(
                         tot.clt = sum((tot.clt)),
                         tot.grp = length(unique(grp.id)),
                         tot.clt.100 = sum(tot.clt.100),
                         clt.reenrolled.21a = sum(clt.reenrolled.21a),
                         clt.reenrolled.100 = sum(clt.reenrolled.100),
                         grp.clt.enrolled.21a = sum(grp.clt.enrolled.21a),
                         group.not.100.clt.enro = sum(group.not.100.clt.enro),
                         x.repaid = (sum(tot.Repaid)/sum(tot.Credit))*100)

View(d205)

# Region level
names(d202)

d206 <- d202 %>% group_by(RegionName) %>% summarise(
                            tot.clt = sum((tot.clt)),
                            tot.grp = length(unique(grp.id)),
                            tot.clt.100 = sum(tot.clt.100),
                            clt.reenrolled.21a = sum(clt.reenrolled.21a),
                            clt.reenrolled.100 = sum(clt.reenrolled.100),
                            grp.clt.enrolled.21a = sum(grp.clt.enrolled.21a),
                            group.not.100.clt.enro = sum(group.not.100.clt.enro),
                            x.repaid = (sum(tot.Repaid)/sum(tot.Credit))*100)

# exprt data
write.table(d203, file = paste(od, "FO map data 7 July 2020.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d204, file = paste(od, "FO map data District level 7 July 2020.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d205, file = paste(od, "FO map data Country level 7 July 2020.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d206, file = paste(od, "FO map data Region level 7 July 2020.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d206, file = paste(od, "FO map data Region level 7 July 2020.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(d201, file = paste(od, "Client enrolled in 21a.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(d20, file = paste(od, "20AB Clients List.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")

#-----------------------------------END------------------------------------------------



