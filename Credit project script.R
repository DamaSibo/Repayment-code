# install.packages("plyr", dependencies = TRUE)

lapply(libs, require, character.only = T)
# read data
d21_pre_pre <- read.csv(paste(dd, "SC_2021_2020.08.24.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

d21_pre_post <- read.csv(paste(dd, "SC_2021_2020.10.15.csv",sep = "/"), header = TRUE,
                 stringsAsFactors = FALSE, na.strings = "")

site.fin <- read.csv(paste(dd, "Site DE finished.csv",sep = "/"), header = TRUE,
                     stringsAsFactors = FALSE, na.strings = "")
#-----------------------------------------------------------------------------------
names(d21_pre)
names(site.fin)
d21_pre$site.id <- paste(d21_pre$DistrictName, d21_pre$SiteName, sep=".")
d21_pre_post$site.id <- paste(d21_pre_post$DistrictName, d21_pre_post$SiteName, sep=".")
site.fin$site.id <- paste(site.fin$DistrictName, site.fin$X..of.Sites, sep = ".")

# match 
d21_pre$match <- site.fin$X..of.Sites[match(d21_pre$site.id, site.fin$site.id)]
d21_pre_post$match <- site.fin$X..of.Sites[match(d21_pre_post$site.id, site.fin$site.id)]

length(unique(d21_pre$GlobalClientID[!is.na(d21_pre$match)]))
d21_pre$match <- ifelse(is.na(d21_pre$match),"No","Yes")
d21_pre_post$match <- ifelse(is.na(d21_pre_post$match),"No","Yes")
table(d21_pre$match, useNA = "ifany")
table(d21_pre_post$match, useNA = "ifany")

d202 <- d21_pre%>% group_by(match)%>% summarise(
  total.credit = sum(TotalCredit[X2021A_Enrollment.Fee.adjustment==0]),
  total.credit.roll = sum(TotalCredit[X2021A_Enrollment.Fee.adjustment>0]),
  total.credit.)

View(d202)

d203 <- d21_pre_post%>% group_by(match)%>% summarise(
  total.credit = sum(TotalCredit[X2021A_Enrollment.Fee.adjustment==0]),
  total.credit.roll = sum(TotalCredit[X2021A_Enrollment.Fee.adjustment>0]))

View(d203)

