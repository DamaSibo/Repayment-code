###########################################
# 2020A repayment KPI uPDATE              #      
# Date:2020-  June 1-7June                #
# Author:Jacques                          #
###########################################


#clear environment
rm(list = ls()); cat("\014")
#Setting data and output directories

dd <- "C:/Users/One Acre Fund/Documents/Jacques/Repayment Update/2021/July 13/Data"
od <- "C:/Users/One Acre Fund/Documents/Jacques/Repayment Update/2021/July 13/Outdata"


# load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)

#loading objects/data/data frame

##############################################################################

d21A <- read.csv(paste(dd, "SC_2021_07_15.csv.",sep = "/"), header = TRUE,
                 stringsAsFactors = FALSE, na.strings = "")

names(d21A)

# site id
d21A$Site.id <-paste(d21A$DistrictName, d21A$SiteName, sep = ".")

names(d21A)
# group id
d21A$Group.id <-paste(d21A$DistrictName, 
                      d21A$SiteName,d21A$GroupName, sep = ".")
length(unique(d21A$Group.id))

# 21A Clients

d21A$A.clts.21 <- ifelse(d21A$X2021A_Avoka.qty >0                
                         |d21A$X2021A_Biolite.SHS.qty >0            
                         |d21A$X2021A_DAP.kg >0                     
                         |d21A$X2021A_DLight.S200.qty >0            
                         |d21A$X2021A_GLP.Pico.qty >0               
                         |d21A$X2021A_GLP.SKP.200.qty >0            
                         |d21A$X2021A_Inkoko.qty >0                 
                         |d21A$X2021A_KAR.qty >0                    
                         |d21A$X2021A_Maize.kg >0                   
                         |d21A$X2021A_Niwa.300XL.qty >0             
                         |d21A$X2021A_NPK.17.kg >0                  
                         |d21A$X2021A_PICS.100KG.qty >0             
                         |d21A$X2021A_PICS.50KG.qty >0              
                         |d21A$X2021A_POV.qty >0                    
                         |d21A$X2021A_SHU.qty >0                    
                         |d21A$X2021A_Tecno.qty >0                  
                         |d21A$X2021A_TOM.qty >0                    
                         |d21A$X2021A_Travertine.kg >0              
                         |d21A$X2021A_TUN.qty >0                    
                         |d21A$X2021A_UREA.kg >0,1,0)
sum(d21A$A.clts.21)
# SUBSETING THE CLTS WITH CREDIT

d21A <- subset(d21A, A.clts.21== 1)
# new clients
d21A$new.clts <- ifelse(d21A$NewMember=="True",1,0)
sum(d21A$new.clts)

# Old clts
d21A$old.clts <- ifelse(d21A$NewMember=="False",1,0)
sum(d21A$old.clts)

# Calculating Ag quantity  

d21A$ag.quantity<-d21A$X2021A_DAP.kg +                     
                  d21A$X2021A_Maize.kg +             
                  d21A$X2021A_NPK.17.kg +                     
                  d21A$X2021A_UREA.kg 

sum(d21A$ag.quantity)

# solar clients
d21A$solars <- d21A$X2021A_Biolite.SHS.qty+                      
               d21A$X2021A_DLight.S200.qty+               
               d21A$X2021A_GLP.SKP.200.qty 
sum(d21A$solars)

# veggies clients

d21A$veggies <- (d21A$X2021A_KAR.qty+ 
                 d21A$X2021A_POV.qty+
                 d21A$X2021A_SHU.qty+
                 d21A$X2021A_TUN.qty+
                 d21A$X2021A_TOM.qty)
sum(d21A$veggies)

# PICO AND TECHNO CLIENTS

d21A$picotec <-ifelse(d21A$X2021A_GLP.Pico.qty>0 | 
                      d21A$X2021A_Tecno.qty>0,1,0)
sum(d21A$picotec)

# clients with 2 PICO OR TECNO

d21A$pico2 <- ifelse(d21A$X2021A_GLP.Pico.qty>1,1,0)
sum(d21A$pico2)
d21A$tec2 <-ifelse(d21A$X2021A_Tecno.qty >1,1,0)
sum(d21A$tec2)

# Total credit
d21A$tot.credit <- d21A$TotalCredit +
                   (d21A$X2021A_DAP.kg*480)+
                   (d21A$X2021A_NPK.17.kg*603)+
                   (d21A$X2021A_UREA.kg*447)

sum(d21A$tot.credit)
#Prepayment

d21A$Ag.pic.Prep.less100 <- ifelse(d21A$ag.quantity<100 & d21A$picotec ==1, 3000,0)
d21A$Ag.pic.Prep.grt100 <- ifelse(d21A$ag.quantity>= 100 & d21A$picotec==1, 7000,0)
d21A$Ag.solar.Prep.less100 <- ifelse(d21A$ag.quantity<100 & d21A$solars>0, 8000,0)
d21A$Ag.solar.Prep.grt100 <-ifelse(d21A$ag.quantity>= 100 & d21A$solars== 1, 12000,0)
d21A$solars.prep <- ifelse(d21A$ag.quantity==0 & d21A$picotec==0 & d21A$solars >0,5000,0)
d21A$veggies.Prep <- ifelse(d21A$ag.quantity==0 & d21A$picotec==0 & d21A$solars==0 & d21A$veggies >=40, 3000,0)
d21A$chick.prep <- ifelse(d21A$X2021A_Inkoko.qty>0, (d21A$X2021A_Inkoko.qty *500),0)
d21A$credit.less.3000 <- ifelse(d21A$A.clts.21==1 & d21A$tot.credit<3000, d21A$tot.credit,0)

#######-----------
d21A$tot.prep <- ifelse(d21A$A.clts.21==1 & d21A$ag.quantity<100 & d21A$picotec ==1, 3000,0)
d21A$tot.prep <- ifelse(d21A$ag.quantity>= 100 & d21A$picotec==1, 7000,d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$A.clts.21==1 & d21A$ag.quantity<100 & d21A$solars>0, 8000,d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$ag.quantity>= 100 & d21A$solars>0, 12000,d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$ag.quantity==0 & d21A$picotec==0 & d21A$solars >0,5000,d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$ag.quantity==0 & d21A$picotec==0 & d21A$solars==0 & d21A$veggies >=40, 3000,d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$ag.quantity==0 & d21A$picotec==0 & d21A$solars==0 & d21A$veggies <40 &
                        d21A$X2021A_Inkoko.qty>0, (d21A$X2021A_Inkoko.qty *500),d21A$tot.prep)
d21A$tot.prep <- ifelse(d21A$A.clts.21==1 & d21A$tot.credit<3000, d21A$tot.credit,d21A$tot.prep)

sum(d21A$tot.prep)

# Total prep
d21A$Tot.Prepayment <- (d21A$Ag.pic.Prep.less100 +
                        d21A$Ag.pic.Prep.grt100 +
                        d21A$Ag.solar.Prep.less100 +
                        d21A$Ag.solar.Prep.grt100 +
                        d21A$chick.prep +
                        d21A$chick.prep +
                        d21A$solars.prep+
                        d21A$credit.less.3000)
#check
sum(d21A$Tot.Prepayment)

summary(d21A$Tot.Prepayment)
length(unique(d21A$GlobalClientID[d21A$Tot.Prepayment==0]))
length(unique(d21A$GlobalClientID[d21A$tot.credit==0]))
length(unique(d21A$GlobalClientID[d21A$TotalCredit==0 & d21A$A.clts.21==0]))
length(unique(d21A$GlobalClientID[d21A$tot.credit==0 & d21A$A.clts.21==0]))
sum(d21A$TotalCredit)
sum(d21A$TotalRepaid_IncludingOverpayments)

# clients paid almost 3k
names(d21A)
d21A$paid3k <- ifelse(d21A$A.clts.21==1 & d21A$TotalRepaid_IncludingOverpayments >= 3000,1,0)
sum(d21A$paid3k)

# qualifiers

d21A$Qualifiers <- ifelse(d21A$A.clts.21 ==1 &
                          d21A$TotalRepaid_IncludingOverpayments >= d21A$tot.prep,1,0)
sum(d21A$Qualifiers)


# clts who did any payment
d21A$clts.paid.any <- ifelse(d21A$A.clts.21==1 & d21A$TotalRepaid_IncludingOverpayments>0,1,0)
sum(d21A$clts.paid.any)
table(d21A$clts.paid)

#Qualifiers on site level

EN21 <- d21A %>%
  group_by(Site.id, DistrictName, RegionName, SiteName)%>%
  summarise(A.clts.21 = sum(A.clts.21),
            new.clts= sum(new.clts),
            old.clts= sum(old.clts),
            Groups = length(unique(Group.id)),
            TotalCredit= sum(TotalCredit),
            TotalRepaid= sum(TotalRepaid_IncludingOverpayments),
            clts.paid.any= sum(clts.paid.any),
            paid3k= sum(paid3k),
            Qualifiers= sum(Qualifiers))
View(EN21) 
nrow(EN21)
sum(EN21$A.clts.21)
sum(EN21$Qualifiers)

# CHECK IF THE Qualifier is not > total clients
d21A$check.qual <- ifelse(d21A$Qualifiers > d21A$A.clts.21,1,0)
View(d21A$check.qual)

# exporting the data to be used in the KPIs

write.table(EN21, file = paste(od, "21 Prepayment 15B .csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")



