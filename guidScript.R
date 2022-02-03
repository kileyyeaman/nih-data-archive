install.packages('lubridate')
install.packages('dplyr')
update.packages('rcpp')
update.packages('lubridate')

library('lubridate')
library('dplyr')

guidDat <- read.csv('/Users/kileyyeaman/Desktop/ALERTStudyConsent-GUIDInfo_DATA_2022-01-12_1928.csv')
View(guidDat)

guidDat <- dplyr::rename(guidDat,
                           ID = id, FIRSTNAME = child_fname, MIDDLENAME = child_mname,
                           LASTNAME = child_lname, COB = child_pob, SEX = child_sex,
                           SUBJECTHASMIDDLENAME = middle_name)

guidDat <- subset(guidDat, select = -c(visit_date))

guidDat$SEX <- as.factor(guidDat$SEX)
levels(guidDat$SEX) <- c("F", "M")

guidDat$SUBJECTHASMIDDLENAME <- as.factor(guidDat$SUBJECTHASMIDDLENAME)
levels(guidDat$SUBJECTHASMIDDLENAME) <- c("NO", "YES")

guidDat$child_dob <- as.Date(guidDat$child_dob, format = "%m/%d/%y")
guidDat$YOB <- year(ymd(guidDat$child_dob))

guidDat$child_dob <- as.Date(guidDat$child_dob, format = "%m/%d/%y")
guidDat$MOB <- month(ymd(guidDat$child_dob))

guidDat$child_dob <- as.Date(guidDat$child_dob, format = "%m/%d/%y")
guidDat$DOB <- day(ymd(guidDat$child_dob))

guidDat <- subset(guidDat, select = -c(child_dob))

guidDat <- guidDat[,c(1,2,3,4,9,10,8,5,6,7)]

write.csv(guidDat, '/Users/kileyyeaman/Desktop/ALERTStudyConsent-GUIDInfo.csv')
