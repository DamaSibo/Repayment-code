### Debt forgiviness approch
#Creating a big dataframe with different criterion
   # Vlookup in most concerning


library(dplyr)
#Adding 10% for each %repaid
datatrack2 <- datatrack %>% mutate(added10 = X..Repaid+10)

deptforg<- datatrack2 %>%
  group_by(grp.id) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[RemainingCredit == 0])),
    tot.most = length(unique(GlobalClientID[status == "most concerning"])),
    tot.enro = length(unique(GlobalClientID[enrolled > 0])),
    tot.cr = sum(RemainingCredit),
    tot.bellow65 = length(unique(GlobalClientID[X..Repaid < 65])),
    below100 = length(unique(GlobalClientID[added10 <= 100])),
    below95 = length(unique(GlobalClientID[added10 <= 95])),
    below90 = length(unique(GlobalClientID[added10<= 90])),
    below85 = length(unique(GlobalClientID[added10<= 85])),
    below80 = length(unique(GlobalClientID[added10<= 80])),
    below75 = length(unique(GlobalClientID[added10<= 75])),
    below70 = length(unique(GlobalClientID[added10<= 70])),
    below65 = length(unique(GlobalClientID[added10<= 65])),
    below60 = length(unique(GlobalClientID[added10<= 60])),
    below55 = length(unique(GlobalClientID[added10<= 55])),
    below50 = length(unique(GlobalClientID[added10<= 50])),
    below45 = length(unique(GlobalClientID[added10<= 45])),
    below40 = length(unique(GlobalClientID[added10<= 40])),
    below35 = length(unique(GlobalClientID[added10 <= 35]))
  )

#Adding a column of who need to finish
deptforg <- deptforg %>% mutate(needtofinish = tot.cl- tot.fin )
#Groups with 1 or more client in the "most concerning" bucket:
length(unique(deptforg$grp.id[deptforg$tot.most >= 1]))
#Groups with 1 or more client below 65%
length(unique(deptforg$grp.id[deptforg$tot.bellow65 >= 1]))

#Finishers in groups with 1 or more "most concerning" client
  ##groups with 1+ most concerning
groupswithmost <- subset(deptforg, tot.most >= 1)
sum(groupswithmost$tot.fin)

#Groups with at least 1 finisher
length(unique(deptforg$grp.id[deptforg$tot.fin >= 1]))
#Groups finished
length(unique(deptforg$grp.id[deptforg$tot.fin == deptforg$tot.cl]))
#Groups with only 1 finishers
length(unique(deptforg$grp.id[deptforg$needtofinish == 1]))

#### Parcentge paid #####
### clients below threshold 
length(unique(df$GlobalClientID[df$X..Repaid <= 85]))
#Total Banned



#### Banned and enrolled
getwd()
length(unique(df$GlobalClientID[df$X..Repaid <= 25 & df$enrolled >0]))
write.csv(deptforg, "dept forgiveness.csv")

#Calculating the total number of farmers banned after 10%
sum(deptforg$tot.cl[deptforg$below100 !=0])
#Clients bellow the threshold
sum(deptforg$below35)
#Clients in Groups where 1 or more client is below thrshold (after 10% Investment added)
sum(deptforg$tot.cl[deptforg$below35 !=0 &
                      deptforg$below35 >= 1])

#enrolled bellow threshold
sum(deptforg$below100[deptforg$below100 !=0 &
                      deptforg$tot.enro !=0])

#Enrolled clients in Groups where 1 or more client is below thrshold (after 10% Investment added)
sum(deptforg$tot.cl[deptforg$below100 !=0 &
                      deptforg$below100 >= 1 &
                      deptforg$tot.enro !=0])






#5. Enrolled Clients above this threshold but below 100%
length(unique(datatrack2$GlobalClientID[datatrack2$enrolled >=1 & datatrack2$added10 < 100 & 
                                          datatrack2$added10 >= 35 ]))

#5. b. Average Amount Owed by these clients
mean(amountav$RemainingCredit[amountav$added10 < 100 &
                                amountav$added10 >= 35])
117285840/11576
#6. credit rolled over
sum(deptforg$tot.cr[deptforg$below100 !=0 &
                      deptforg$below35 >= 1])

#Groups with only 1 nonfinishers
length(deptforg$tot.cl[deptforg$need == 1])

sum(df$TotalRepaid)/sum(df$TotalCredit)

#Credit rollover





##############################################
 #additing 4% to each farmer
datatrack3 <- datatrack %>% mutate(added14 = X..Repaid + 14)

deptforg2<- datatrack3 %>%
  group_by(grp.id) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    tot.fin = length(unique(GlobalClientID[datatrack$RemainingCredit == 100])),
    tot.most = length(unique(GlobalClientID[status == "most concerning"])),
    tot.enro = length(unique(GlobalClientID[enrolled > 0])),
    rem.cr = sum(RemainingCredit),
    tot.bellow65 = length(unique(GlobalClientID[X..Repaid < 65])),
    below85 = length(unique(GlobalClientID[addi.4 <= 85])),
    below80 = length(unique(GlobalClientID[addi.4<= 80])),
    below75 = length(unique(GlobalClientID[addi.4<= 75])),
    below70 = length(unique(GlobalClientID[addi.4<= 70])),
    below65 = length(unique(GlobalClientID[addi.4<= 65])),
    below60 = length(unique(GlobalClientID[addi.4<= 60])),
    below55 = length(unique(GlobalClientID[addi.4<= 55])),
    below50 = length(unique(GlobalClientID[addi.4<= 50])),
    below45 = length(unique(GlobalClientID[addi.4<= 45])),
    below40 = length(unique(GlobalClientID[addi.4<= 40])),
    below35 = length(unique(GlobalClientID[addi.4<= 35])),
    below30 = length(unique(GlobalClientID[addi.4<= 30])),
    below25 = length(unique(GlobalClientID[addi.4<= 25]))
  )
deptforg2 <- deptforg2 %>% mutate(need = tot.cl-tot.fin )
#Calculating the total number of farmers banned after 10%
sum(deptforg2$tot.cl[deptforg2$below25 !=0 ])
#enrolled
sum(deptforg2$tot.cl[deptforg2$below25 !=0 &
                      deptforg2$tot.enro !=0])

#Groups with only 1 nonfinishers
length(deptforg2$tot.cl[deptforg2$need == 1])
    
#groups with only 1 nonfinishers
nonfinishersg <- df %>%
  group_by(grp.id) %>%
  summarise(
    tot.cl = length(GlobalClientID),
    tot.fin = length(GlobalClientID[X..Repaid == 100]),
    rem.cr = sum(RemainingCredit)
  )

#Finished not enrolled
notenrolled <- subset(df, X..Repaid == 100 & enrolled == 0)
#Enrolled but not finishes
ennotfinish <- subset(df, X..Repaid < 100 & enrolled != 0)
length(unique(ennotfinish$GlobalClientID[ennotfinish$X..Repaid < 90 & ennotfinish$X..Repaid >= 75]))
getwd()
#Adding who need to pay and the amount to a
nonfinishersg <- nonfinishersg %>% mutate(need = tot.cl-tot.fin )
nonfinishersg1 <- subset(nonfinishersg,need == 1)
write.csv(nonfinishersg, "nonfinishers_groups.csv")

  

##### week of 13 July ###### add 18.5 to evebody's credit and see

