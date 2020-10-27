#Group Completion Analysis
#Date: 11 Junes, 2020
#owner: Dama,


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
d20 <- read.csv(paste(dd, "SC_Light_20200611.csv", sep = "/"), header = TRUE,  
                    stringsAsFactors = FALSE, na.strings = "")
d20 <- subset(d20, d20$TotalCredit >0)
nrow(d20)
sum(d20$TotalRepaid)/sum(d20$TotalCredit)
#----------------------------------------------------------------------------
# put data at the group level
d201 <- d20 % 

