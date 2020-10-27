###########################################
# 2021A prepayment database               #      
# Date: July 22, 2019                     #
# Author=Dama                             #
# Database Qc: Paci, Jimmy               #
##########################################


#clear environment
rm(list = ls()); cat("\014")

#Setting data and output directories
dd <-  "D:/Drop analysis/Repayment/data"
od <-  "D:/Drop analysis/Repayment/output"

# load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)

#loading objects/data/data frame

d21 <- read.csv(paste(dd, "SC_2021_2020.09.10.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")
#--------------------------------------------------------------------------------
#checking the object
nrow(d21)
names(d21)
length(d21$GlobalClientID[d21$TotalCredit>0])
length(d21$GlobalClientID[d21$TotalCredit==0])
sum(d21$X2021A_Maize.kg)
length(d21$GlobalClientID[d21$X2021A_Maize.kg>0])
length(d21$GlobalClientID[d21$TotalCredit<0])
sum(d21$TotalCredit)
sum(d21$X2021A_CycleCredit)
sum(d21$X2021A_Enrollment.Fee.adjustment)
length(unique(d21$GlobalClientID[d21$X2021A_Enrollment.Fee.adjustment>0]))

# create total credit
#d21$add.credit <-  d21$X2021A_Maize.kg* 950

#sum(d21$add.credit)
sum(d21$TotalCredit)
d21$total.credit <- d21$TotalCredit
sum(d21$total.credit)
sum(d21$TotalRepaid_IncludingOverpayments)
sum(d21$TotalRepaid_IncludingOverpayments)/sum(d21$total.credit)*100
length(unique(d21$GlobalClientID[d21$total.credit<=8000]))
length(unique(d21$GlobalClientID[d21$total.credit<=5000]))
length(unique(d21$GlobalClientID[d21$total.credit<=3000]))
#determining clients ordered in 21a
colnames(d21)
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


length(unique(d21$GlobalClientID[d21$total.clients==0]))
table(d21$total.clients, useNA = "ifany")
d21 <- subset(d21, d21$total.clients==1)
#----------------
d21$ag.clients <- ifelse(d21$X2021A_UREA.kg >0
                         |d21$X2021A_DAP.kg >0
                         |d21$X2021A_NPK.17.kg >0
                         |d21$X2021A_Maize.kg > 0
                         |d21$X2021A_Pan.53.kg >0
                         |d21$X2021A_Pan.691.kg >0
                         |d21$X2021A_WH.403.kg >0
                         |d21$X2021A_WH.505.kg>0
                         |d21$X2021A_WH.605.kg >0
                         |d21$X2021A_SC.403.kg >0
                         |d21$X2021A_SC.637.kg >0,1,0)

table(d21$ag.clients, useNA = "ifany")
# Calculate clients with veg
d21$veg.clients <-ifelse(d21$X2021A_TUN.qty >0
                         |d21$X2021A_TOM.qty >0
                         |d21$X2021A_SHU.qty >0
                         |d21$X2021A_POV.qty >0
                         |d21$X2021A_KAR.qty >0,1,0)
table(d21$veg.clients, useNA = "ifany")

#calculating Clients with travertine
d21$travertine.clients <- ifelse(d21$X2021A_Travertine.kg >0,1,0)
table(d21$travertine.clients, useNA = "ifany")

# calculating clients with PICS
d21$pics.clients <- ifelse(d21$X2021A_PICS100KG.qty > 0
                           |d21$X2021A_PICS50KG.qty >0, 1, 0)
table(d21$pics.clients, useNA = "ifany")

#calculating Clients with chicken
d21$chicken.clients <- ifelse(d21$X2021A_Inkoko.qty >0,1,0)

table(d21$chicken.clients, useNA = "ifany")

#calculating clients with avoka
d21$avoka.clients <- ifelse(d21$X2021A_Avoka.qty > 0, 1, 0)
table(d21$avoka.clients, useNA = "ifany")

#calculating solar clients
d21$Solar.client <- ifelse(d21$X2021A_Biolite.SHS.qty >0                    
                           |d21$X2021A_DLight.S200.qty >0
                           |d21$X2021A_GLP.SKP.200.qty >0
                           |d21$X2021A_Niwa.300XL.qty >0,1,0)

table(d21$Solar.client, useNA = "ifany")

#calculating pico and phone clients number
d21$pico.techno <- ifelse(d21$X2021A_GLP.Pico.qty >0
                          |d21$X2021A_GLP.Pico.Trial.qty >0
                          |d21$X2021A_Tecno.qty >0, 1,0)

table(d21$pico.techno, useNA = "ifany")

# Calculating travertine only clients
d21$trv.only.clients <- ifelse(d21$travertine.clients == 1
                               &(d21$ag.clients == 0
                                 & d21$pico.techno==0
                                 & d21$Solar.client == 0
                                 & d21$chicken.clients == 0
                                 & d21$avoka.clients ==0
                                 & d21$veg.clients == 0
                                 & d21$pics.clients == 0),1,0)


table(d21$trv.only.clients, useNA = "ifany")
# calculate veg only clients
d21$veg.only.client <-ifelse(d21$veg.clients == 1
                             & (d21$ag.clients == 0
                                & d21$Solar.client == 0
                                & d21$pico.techno==0
                                & d21$chicken.clients == 0
                                & d21$avoka.clients ==0
                                & d21$travertine.clients == 0
                                & d21$pics.clients == 0),1,0)
table(d21$veg.only.client, useNA = "ifany")

# calculate PICS only clients
d21$pics.only.client <- ifelse(d21$pics.clients == 1
                               &(d21$ag.clients == 0
                                 & d21$Solar.client == 0
                                 & d21$chicken.clients == 0
                                 & d21$pico.techno==0
                                 & d21$avoka.clients ==0
                                 & d21$travertine.clients == 0
                                 & d21$veg.clients == 0),1,0)

table(d21$pics.only.client, useNA = "ifany")

# calculate Pico&phone only clients
d21$pico.techno.only <- ifelse(d21$pico.techno == 1
                               & (d21$ag.clients == 0
                                  & d21$Solar.client == 0
                                  & d21$chicken.clients == 0
                                  & d21$pics.clients==0
                                  & d21$avoka.clients ==0
                                  & d21$travertine.clients == 0
                                  & d21$veg.clients == 0),1,0)
table(d21$pico.techno.only, useNA = "ifany")

# calculate avoka only clients
d21$avoka.only <- ifelse(d21$avoka.clients == 1
                         & (d21$ag.clients == 0
                            & d21$Solar.client == 0
                            & d21$chicken.clients == 0
                            & d21$pics.clients==0
                            & d21$pico.techno ==0
                            & d21$travertine.clients == 0
                            & d21$veg.clients == 0),1,0)

table(d21$avoka.only, useNA = "ifany")
# calculate solar only clients
d21$solar.only <- ifelse(d21$Solar.client == 1
                         & (d21$ag.clients == 0
                            & d21$pico.techno == 0
                            & d21$chicken.clients == 0
                            & d21$pics.clients==0
                            & d21$avoka.clients ==0
                            & d21$travertine.clients == 0
                            & d21$veg.clients == 0),1,0)
table(d21$pico.techno.only, useNA = "ifany")
# calculate tecno only clients
d21$techno.only <- ifelse(d21$X2021A_Tecno.qty>0
                          & (d21$ag.clients == 0
                             & d21$Solar.client == 0
                             & d21$chicken.clients == 0
                             & d21$pics.clients==0
                             & d21$avoka.clients ==0
                             & d21$travertine.clients == 0
                             & d21$veg.clients == 0
                             & d21$X2021A_GLP.Pico.qty==0),1,0)

table(d21$techno.only, useNA = "ifany")

# calculate 2pico only clients
d21$pico.2.only <- ifelse(d21$X2021A_GLP.Pico.qty==2
                          &(d21$ag.clients == 0
                            & d21$Solar.client == 0
                            & d21$chicken.clients == 0
                            & d21$pics.clients==0
                            & d21$avoka.clients ==0
                            & d21$travertine.clients == 0
                            & d21$veg.clients == 0
                            & d21$X2021A_Tecno.qty==0),1,0)

table(d21$pico.2.only, useNA = "ifany")

# ag+pico+phone clients
table(d21$ag.clients, useNA = "ifany")
table(d21$pico.techno, useNA = "ifany")
d21$ag.pico.techno.client <- ifelse(d21$ag.clients >0 
                                    |d21$pico.techno > 0,1,0)
table(d21$ag.pico.techno.client, useNA = "ifany")
##########################################################################
# Calculating Ag quantity
d21$ag.qty <- d21$X2021A_DAP.kg+
  d21$X2021A_NPK.17.kg+
  d21$X2021A_UREA.kg+
  d21$X2021A_Maize.kg+
  d21$X2021A_WH.403.kg+
  d21$X2021A_WH.505.kg+
  d21$X2021A_WH.605.kg+
  d21$X2021A_SC.403.kg+
  d21$X2021A_SC.637.kg+
  d21$X2021A_Pan.53.kg+
  d21$X2021A_Pan.691.kg


summary(d21$ag.qty) 
sum(d21$ag.qty)
# Calculating veg quantity
d21$veg.qty <- d21$X2021A_TUN.qty+
  d21$X2021A_TOM.qty+
  d21$X2021A_SHU.qty+
  d21$X2021A_POV.qty+
  d21$X2021A_KAR.qty
summary(d21$veg.qty) 
sum(d21$veg.qty)

# ag and veg quantity
d21$ag.veg.client <- d21$ag.qty + d21$veg.qty
summary(d21$ag.veg.client) 

# solar quantity
d21$solar.qty <- d21$X2021A_Biolite.SHS.qty +                   
  d21$X2021A_DLight.S200.qty+
  d21$X2021A_GLP.SKP.200.qty +
  d21$X2021A_Niwa.300XL.qty
summary(d21$solar.qty)
sum(d21$solar.qty)

#pics quantity for each client
d21$pics.qty <- d21$X2021A_PICS100KG.qty + d21$X2021A_PICS50KG.qty
sum((d21$pics.qty))

###################################################################################
#Determining Clients with more than 100kg ag
d21$clients_with_more_ag <- ifelse(d21$ag.qty >=100,1,0)
table(d21$clients_with_more_ag, useNA = "ifany")

#Prepayment calculation
####################################################################################################################

# Calculating Ag prepayment (3,000 F)
d21$ag.prep <- ifelse (d21$ag.pico.techno.client == 1 & d21$ag.qty >=100, 7000,0)
d21$ag.prep <- ifelse (d21$ag.pico.techno.client == 1 & d21$ag.qty <100, 3000,d21$ag.prep)

#+avoka
length(unique(d21$GlobalClientID[d21$X2021A_Avoka.qty>0]))
length(unique(d21$GlobalClientID[d21$X2021A_Avoka.qty>9]))
length(unique(d21$GlobalClientID[d21$X2021A_Avoka.qty<=9 &d21$X2021A_Avoka.qty >0]))
length(unique(d21$GlobalClientID[d21$ag.qty<100 &d21$ag.pico.techno.client == 1
                                 &d21$X2021A_Avoka.qty <=9 &d21$X2021A_Avoka.qty >0]))

d21$ag.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$ag.qty >=100 & d21$X2021A_Avoka.qty >9 
                      & d21$avoka.only==0,8000, d21$ag.prep)

d21$ag.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$ag.qty <100 & d21$X2021A_Avoka.qty <=9 
                      &d21$X2021A_Avoka.qty >0 & d21$avoka.only==0,4000, d21$ag.prep)

table(d21$ag.prep,useNA = "ifany")
sum(d21$ag.prep)

# Calculating solar prepayment (5,000 F)
d21$solar.prep <- ifelse(d21$Solar.client ==1 | d21$pico.techno.only==1, 5000,0)
d21$solar.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$Solar.client==1
                         & d21$ag.qty >=100,12000,d21$solar.prep)                           
d21$solar.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$Solar.client==1
                         & d21$ag.qty <100,8000,d21$solar.prep)

#+avoka
d21$solar.prep <- ifelse((d21$solar.only==1 | d21$pico.techno.only==1) & d21$X2021A_Avoka.qty >9 
                         &d21$X2021A_Avoka.qty >0 & d21$avoka.only==0, 6000,d21$solar.prep)
d21$solar.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$Solar.client==1 & d21$ag.qty >=100
                         & d21$X2021A_Avoka.qty >9 &d21$X2021A_Avoka.qty >0&
                           d21$avoka.only==0,13000,d21$solar.prep)
d21$solar.prep <- ifelse(d21$ag.pico.techno.client == 1 & d21$Solar.client==1 
                         & d21$ag.qty <100 & d21$X2021A_Avoka.qty >9 &d21$X2021A_Avoka.qty >0
                         & d21$avoka.only==0,9000,d21$solar.prep)

#check
length(unique(d21$GlobalClientID[d21$X2021A_Enrollment.Fee.adjustment>0 & d21$Solar.client==1]))#4,223

length(unique(d21$GlobalClientID[d21$X2021A_Avoka.qty>9 & d21$avoka.only==0 & (d21$solar.only==1
                                                                               |d21$pico.techno.only==1)])) # 0

table(d21$solar.prep,useNA = "ifany")
sum(d21$solar.prep)

# calculate techno & 2 pico only prepayment
table(d21$X2021A_GLP.Pico.qty, useNA = "ifany")
table(d21$X2021A_Tecno.qty, useNA = "ifany")
table(d21$techno.only, useNA = "ifany")
d21$techno.2pico.prep <- ifelse(d21$techno.only==1 | d21$pico.2.only==1 , 5000,0)

table(d21$techno.2pico.prep,useNA = "ifany")
sum(d21$techno.2pico.prep)

# Calculating Chicken prepayment (500 F per chicken)
d21$chichens.prep <- ifelse(d21$chicken.clients == 1, d21$X2021A_Inkoko.qty*500,0)

table(d21$chichens.prep,useNA = "ifany")
sum(d21$chichens.prep)

# Calculating avoka only prepayment (Order 3-9 trees F1000 and >9 trees F1500)
d21$avoka.prep <- ifelse(d21$avoka.only == 1 & d21$X2021A_Avoka.qty >9,1500,0 )
d21$avoka.prep <- ifelse(d21$avoka.only == 1 & d21$X2021A_Avoka.qty <=9,1000,d21$avoka.prep)
table(d21$avoka.prep,useNA = "ifany")
sum(d21$avoka.prep)

# claculating trv prepayment
min(d21$X2021A_Travertine.kg[d21$X2021A_Travertine.kg>0])
table(d21$trv.only.clients)
d21$trv.prep <- ifelse(d21$trv.only.clients==1 & d21$X2021A_Travertine.kg<=25, 2500,0)
d21$trv.prep <- ifelse(d21$trv.only.clients==1 &d21$X2021A_Travertine.kg>25, 3000,d21$trv.prep)

table(d21$trv.prep,useNA = "ifany")
sum(d21$trv.prep)

# Ag + Solar prepayment
d21$ag.sol.prep <- ifelse(d21$solar.prep > d21$ag.prep,d21$solar.prep,d21$ag.prep)
table(d21$ag.sol.prep,useNA = "ifany")

# Full prepayment
d21$Full.Prep <- d21$ag.sol.prep + 
  d21$chichens.prep +
  d21$techno.2pico.prep +
  d21$avoka.prep +
  d21$trv.prep

table(d21$Full.Prep,useNA = "ifany")
sum(d21$Full.Prep)
summary(d21$Full.Prep)


# calculate veg prepayment
d21$veg.prep <- ifelse(d21$veg.qty >0 & d21$total.credit >3000 & d21$Full.Prep == 0, 3000,0)
d21$veg.prep <- ifelse((d21$veg.qty >0 & d21$total.credit <=3000
                        & d21$Full.Prep == 0), d21$total.credit,d21$veg.prep )

table(d21$veg.prep,useNA = "ifany")
summary(d21$veg.prep)

# claculating pics prepayment
d21$pics.prep <- ifelse(d21$pics.qty >0 & d21$total.credit >3000 & d21$veg.prep == 0 
                        & d21$Full.Prep == 0,3000,0)
d21$pics.prep <- ifelse(d21$pics.qty >0 & d21$total.credit <=3000 & d21$veg.prep == 0
                        & d21$Full.Prep == 0,d21$total.credit,d21$pics.prep )

table(d21$pics.prep, useNA = "ifany")

d21$other.product.prep <- d21$veg.prep + d21$pics.prep
summary(d21$other.product.prep)

d21$total.prep <- d21$Full.Prep +d21$other.product.prep
table(d21$total.prep, useNA = "ifany")
summary(d21$total.prep)
sum(d21$total.prep)
sum(d21$total.prep)/sum(d21$total.credit)

d21a <- subset(d21, d21$total.prep==0)

d21$Prep.required <- ifelse(d21$total.prep==0 & d21$total.credit>3000,3000,d21$total.prep)
d21$Prep.required <- ifelse(d21$total.prep==0 & d21$total.credit<=3000,d21$total.credit,d21$Prep.required)
table(d21$Prep.required, useNA = "ifany")
summary(d21$Prep.required)
sum(d21$Prep.required)
sum(d21$Prep.required)/sum(d21$total.credit)
#-----------------------------------------------------------------------------------------------
#Calculate Min prepayment
names(d21)
#Adding a column of minimum prepayment required
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client==1 & d21$Solar.client ==1,3000,d21$Prep.required)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$Solar.client == 0, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$chicken.clients == 1, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$chicken.clients == 0, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$travertine.clients == 1, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$travertine.clients == 0, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$veg.clients == 1, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$veg.clients == 0, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$pics.clients == 1, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$pics.clients == 0, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$avoka.clients == 1, 3000,d21$Min_Prepay)
d21$Min_Prepay <- ifelse(d21$ag.pico.techno.client == 1 & d21$avoka.clients == 0, 3000,d21$Min_Prepay)

table(d21$Min_Prepay, useNA = "ifany")
sum(d21$Min_Prepay)
sum(d21$Prep.required)
sum(d21$Prep.required)-sum(d21$Min_Prepay)
sum(d21$Min_Prepay)/sum(d21$total.credit)
#-----------------------------------------------------------------------------------------------
# select necessary clumns
names(d21)
sum(d21$TotalCredit)
sum(d21$total.credit)
sum(d21$TotalRepaid)
sum(d21$TotalRepaid_IncludingOverpayments)
sum(d21$TotalRepaid_IncludingOverpayments) - sum(d21$TotalRepaid)
100*sum(d21$TotalRepaid_IncludingOverpayments)/sum(d21$total.credit)
100*sum(d21$TotalRepaid_IncludingOverpayments)/sum(d21$TotalCredit)
# select necessary column
names(d21)
nrow(d21)
d211 <- select(d21,RegionName,DistrictName,SiteName,delivery.status,delivery.weeks,GroupName,LastName,FirstName,OAFID,NewMember,Facilitator,ClientPhone,AccountNumber,GlobalClientID,total.credit, TotalRepaid,
               X..Repaid,X2021A_Enrollment.Fee.adjustment, TotalRepaid_IncludingOverpayments,ag.clients,Solar.client, travertine.clients,pics.clients,chicken.clients,                        
               trv.only.clients,veg.only.client,pics.only.client, avoka.only,pico.techno,pico.techno.only,                             
               ag.qty,X2021A_Inkoko.qty,veg.qty,solar.qty,X2021A_Travertine.kg,pics.qty,X2021A_Avoka.qty,                                        
               clients_with_more_ag,ag.prep,solar.prep,chichens.prep,avoka.prep,veg.prep, other.product.prep,ag.sol.prep,Min_Prepay,Prep.required)

names(d211)
nrow(d211)
#---------------------------------------------------------------------------------------------
# Prepayment tracker Database
#calculating remaining on full prepayment
d211$rem.to.prep <- ifelse(d211$Prep.required >= d211$TotalRepaid_IncludingOverpayments,d211$Prep.required - d211$TotalRepaid_IncludingOverpayments, 0) 
d211$rem.to.prep <- ifelse(d211$TotalRepaid_IncludingOverpayments==0, d211$Prep.required,d211$rem.to.prep)

length(unique( d211$GlobalClientID[d211$rem.to.prep == 0]))
length(unique(d211$GlobalClientID[d211$TotalRepaid_IncludingOverpayments>=d211$Prep.required]))
length(unique(d211$GlobalClientID[d211$rem.to.prep > d21$Prep.required]))
sum(d211$rem.to.prep)
summary(d211$rem.to.prep)

# Strong Amount
d211$strong.amount <- d211$Prep.required+5000
sum(d211$strong.amount)  

d21pr.tracker <- select(d211,RegionName,DistrictName, SiteName,GroupName,LastName,FirstName,Facilitator,AccountNumber,X2021A_Enrollment.Fee.adjustment,
                        Solar.client, X2021A_Inkoko.qty,X2021A_Avoka.qty, Prep.required,TotalRepaid_IncludingOverpayments,strong.amount,Min_Prepay,)


write.table(d21pr.tracker,file = paste(od, "21A prepayment tracker database.csv", sep = "/"), 
            row.names = FALSE, col.names = TRUE, sep = ",")

#-----------------------------------------------------------------------------------------------#
#Strong Start Categories
table(d211$total.credit)
d211$x.repaid <- ifelse(d211$total.credit>0,100*(d211$TotalRepaid_IncludingOverpayments/d211$total.credit),0)
d211$x.repaid <- ifelse(d211$x.repaid>100,100,d211$x.repaid)
length(unique(d211$GlobalClientID[d211$X..Repaid==100]))
length(unique(d211$GlobalClientID[d211$x.repaid==100]))

d211$strong.starter <- ifelse( d211$TotalRepaid_IncludingOverpayments >= (d211$Prep.required+5000), 1,0)
d211$Full.Qualifier <- ifelse(d211$strong.starter==0 & d211$TotalRepaid_IncludingOverpayments >= d211$Prep.required & d211$x.repaid <100,1,0)
d211$Min.Qualifier <- ifelse(d211$strong.starter==0 &d211$Full.Qualifier==0 &d211$TotalRepaid_IncludingOverpayments>=3000 &
                               d211$TotalRepaid_IncludingOverpayments < d211$Prep.required & d211$x.repaid <100 ,1,0)
d211$Starter <- ifelse(d211$strong.starter==0 & d211$Full.Qualifier==0 & d211$Min.Qualifier==0 & d211$TotalRepaid_IncludingOverpayments >=100
                       & d211$TotalRepaid_IncludingOverpayments <3000 & d211$x.repaid <100, 1,0)

d211$non.starters <- ifelse(d211$TotalRepaid_IncludingOverpayments <100,1,0)
d211$finishers <- ifelse(d211$strong.starter==0 & d211$x.repaid == 100,1,0)


table(d211$finishers)
table(d211$strong.starter)
length(unique(d211$GlobalClientID[d211$x.repaid==100 & d211$strong.starter==1]))
length(unique(d211$GlobalClientID[d211$finishers==1 | d211$strong.starter==1]))
table(d211$Full.Qualifier, useNA = "ifany")
table(d211$Min.Qualifier)
table(d211$Starter, useNA = "ifany")
table(d211$non.starters, useNA = "ifany")

# check
d211$tot <-d211$non.starters+d211$Starter+d211$Min.Qualifier+d211$Full.Qualifier+d211$strong.starter +d211$finishers
table(d211$tot, useNA = "ifany")
sum(d211$tot)
nrow(d211)

#---------------------------------------------------------------------------------------------
# Put on site level
d212 <- d211 %>%group_by(DistrictName, SiteName) %>% summarise(
  tot.client = length(unique(GlobalClientID)) ,
  tot.credit = sum(total.credit),
  tot.repaid = sum(TotalRepaid_IncludingOverpayments),
  Prep.required = sum(Prep.required),
  Min.prepay = sum(Min_Prepay),
  Non.Starters = sum(non.starters),
  Starters = sum(Starter),
  Min.Qualifier= sum(Min.Qualifier),
  Full.Qualifier = sum(Full.Qualifier),
  strong.starter= sum(strong.starter),
  finishers = sum(finishers),
  Strong.finisher = sum(strong.starter)+sum(finishers))

View(d212)


#excel export
write.table(d212,file = paste(od, "Strong start category_client_08_Sept.csv", sep = "/"), 
            row.names = FALSE, col.names = TRUE, sep = ",")
