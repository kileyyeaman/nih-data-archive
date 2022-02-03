# Loading dplyr package to rename variables
#  If this does not work on your computer, you need to install dplyr by running 
# install.packages('dplyr') in the console. Then, rerun library(dplyr).
library(dplyr)
library(lubridate)
library(tidyr)

####################################################################
# IMPORT CSV FILES
####################################################################
# Loading REDCap datasets (data entry, parent, teen, teachers)
# To run most up to date dataset, update file paths below. 
# Make sure the file path has a FORWARD slash ("/") NOT a BACK slash ("\")
# Although back slash is default to windows, forward slash in used in R
# due to other commands using forward slash.

subjectDat <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/NDAR Subject Info.csv')
guidDat <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTStudyConsent-GUIDInfo--results-20220113T005741375Z.csv')
ndar_subject01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTDataEntry-NDARNdarsubject01Nda_DATA_2022-01-14_1319.csv')
ndar_subject01_dem <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTParentSurveys-NDARNdarsubject01Gen_DATA_2022-01-14_1324.csv')
scared01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARScared01_DATA_2022-01-13_2006.csv')
dass01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTParentSurveys-NDARDass01_DATA_2022-01-13_2038.csv')
spsrqc01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTParentSurveys-NDARSpsrq01_DATA_2022-01-13_1422.csv')
barty01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/alert_bart_summary_01-14-2022_0820.csv')
ari01_a <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARAri01_DATA_2022-01-13_1756.csv')
ari01_p <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTParentSurveys-NDARAri01_DATA_2022-01-13_1757.csv')
sitbi01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTDataEntry-NDARSitbi01_DATA_2022-01-14_1421.csv')
erc01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTParentSurveys-NDARErc01_DATA_2022-01-13_1609.csv')
inq01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARInq01_DATA_2022-01-13_1620.csv')
cerq01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARCerq01_DATA_2022-01-13_1704.csv')
ngses01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARNgses01_DATA_2022-01-13_1727.csv')
rpas01 <- read.csv('/Users/kileyyeaman/Desktop/NDAR/Raw Data/ALERTAdolescentSurve-NDARRpas01_DATA_2022-01-13_1735.csv')


####################################################################
# TIDY GUID/SUBJECT DATA
####################################################################
# Convert variables in guidDat 
guidDat <- dplyr::rename(guidDat, 
                         src_subject_id = RECORD_ID, 
                         subjectkey = GUID)
guidDat <- subset(guidDat, select = -c(MESSAGE))

subjectDat <- dplyr::rename(subjectDat,
                            src_subject_id = ID,
                            sex = SEX,
                            interview_date = visit_date)
participantDat <- merge(guidDat, subjectDat, by = "src_subject_id")

# Calculating age (in months) at visit 
participantDat$child_dob <- as.Date(participantDat$child_dob, "%m/%d/%y")
participantDat$interview_date <- as.Date(participantDat$interview_date, "%m/%d/%y")
participantDat$interview_age <- interval(participantDat$child_dob, 
                                         participantDat$interview_date) %/% months(1)    
participantDat$interview_date<- as.character(participantDat$interview_date)
participantDat <- subset(participantDat, select = -c(child_dob))

participantDat <- participantDat[,c(2,1,3,5,4)]


#Subset, remove ineligible/withdrawn participants
participantDat <- subset(participantDat, 
                         src_subject_id != 5004 & src_subject_id != 5101 &
                         src_subject_id != 5116 & src_subject_id != 5119 & 
                         src_subject_id != 5141 & src_subject_id != 5142 &
                         src_subject_id != 5151 & src_subject_id != 5154 &
                         src_subject_id != 5163 & src_subject_id != 5164 &
                         src_subject_id != 5168)

####################################################################
# SCARED
####################################################################
# Renaming variables
scared01 <- dplyr::rename(scared01,
                        src_subject_id = record_id, 
                        timept = redcap_event_name, 
                        scared_1 = scar1, scared_2 = scar2, scared_3 = scar3, 
                        scared_4 = scar4, scared_5 = scar5, scared_6 = scar6,
                        scared_7 = scar7, scared_8 = scar8, scared_9 = scar9, 
                        scared_10 = scar10, scared_11 = scar11, scared_12 = scar12, 
                        scared_13 = scar13, scared_14 = scar14, scared_15 = scar15,
                        scared_16 = scar16, scared_17 = scar17, scared_18 = scar18,
                        scared_19 = scar19, scared_20 = scar20, scared_21 = scar21, 
                        scared_22 = scar22, scared_23 = scar23, scared_24 = scar24,
                        scared_25 = scar25, scared_26 = scar26, scared_27 = scar27,
                        scared_28 = scar28, scared_29 = scar29, scared_30 = scar30, 
                        scared_31 = scar31, scared_32 = scar32, scared_33 = scar33,
                        scared_34 = scar34, scared_35 = scar35, scared_36 = scar36,
                        scared_37 = scar37, scared_38 = scar38, scared_39 = scar39, 
                        scared_40 = scar40, scared_41 = scar41)

# Assigning version number, respondent, and timepoint
scared01$scared_version <- "Child"
scared01$respondent <- "Child"
scared01$timept <- "1"

# Merging data frames
scared01 <- merge(participantDat, scared01, by = "src_subject_id")

# Calculating total score
cbind(names(scared01)) 
scared01_cbind <- cbind(scared01[,7:47])
scared01$scared_total <- rowSums(scared01_cbind)

# Calculating PD score
pd_cbind <- cbind(scared01$scared_1, scared01$scared_6, 
                        scared01$scared_9, scared01$scared12,
                        scared01$scared_15, scared01$scared_18, 
                        scared01$scared_19, scared01$scared_22,
                        scared01$scared_24, scared01$scared_27,
                        scared01$scared_30, scared01$scared_34,
                        scared01$scared_38)
scared01$scared_pd_score <- rowSums(pd_cbind) 

# Calculating GAD score
gad_cbind <- cbind(scared01$scared_5, scared01$scared_7, 
                  scared01$scared_14, scared01$scared_21,
                  scared01$scared_23, scared01$scared_28, 
                  scared01$scared_33, scared01$scared_35,
                  scared01$scared_37)
scared01$scared_gad_score <- rowSums(gad_cbind)    

# Calculating SAD score
sad_cbind <- cbind(scared01$scared_4, scared01$scared_8, 
                   scared01$scared_13, scared01$scared_16,
                   scared01$scared_20, scared01$scared_25, 
                   scared01$scared_29, scared01$scared_31)
scared01$scared_sad_score <- rowSums(sad_cbind)   

# Calculating SOCAD score
socad_cbind <- cbind(scared01$scared_3, scared01$scared_10, 
                     scared01$scared_26, scared01$scared_32,
                     scared01$scared_39, scared01$scared_40, 
                     scared01$scared_41)
scared01$scared_socad_score <- rowSums(socad_cbind)   

# Calculating SSA score
ssa_cbind  <- cbind(scared01$scared_2, scared01$scared_11, 
                    scared01$scared_17, scared01$scared_36)
scared01$scared_ssa_score <- rowSums(ssa_cbind)   



####################################################################
# DASS
####################################################################
# Renaming variables
dass01 <- dplyr::rename(dass01,
                        src_subject_id = record_id,
                        visnum = redcap_event_name,
                        dass_1 = das1, dass_2 = das2, dass_3 = das3,
                        dass_4 = das4, dass_5 = das5, dass_6 = das6,
                        dass_7 = das7, dass_8 = das8, dass_9 = das9,
                        dass_10 = das10, dass_11 = das11, dass_12 = das12,
                        dass_13 = das13, dass_14 = das14, dass_15 = das15,
                        dass_16 = das16, dass_17 = das17, dass_18 = das18,
                        dass_19 = das19, dass_20 = das20, dass_21 = das21)

dass01 <- merge(participantDat, dass01, by = "src_subject_id")

stress_cbind <- cbind(dass01$dass_1, dass01$dass_6, dass01$dass_8, 
                      dass01$dass_11, dass01$dass_12, dass01$dass_14,
                      dass01$dass_18)
dass01$dass_stress_sc <- rowSums(stress_cbind)

anxiety_cbind <- cbind(dass01$dass_2, dass01$dass_4, dass01$dass_7,
                       dass01$dass_9, dass01$dass_15, dass01$dass_19,
                       dass01$dass_20)
dass01$dass_anx_sc <- rowSums(anxiety_cbind)

depress_cbind <- cbind(dass01$dass_3, dass01$dass_5, dass01$dass_10,
                        dass01$dass_13, dass01$dass_16, dass01$dass_17,
                        dass01$dass_21)

dass01$dass_depr_sc <- rowSums(depress_cbind)

dass01$visnum <- "1"

####################################################################
# NDAR_SUBJECT01 / NDAR_GENOMICS01
####################################################################
ndar_subject01$ksa1_c[ndar_subject01$ksa1_c == "1"] <- "ADHD (child report)"
ndar_subject01$ksa1_p[ndar_subject01$ksa1_p == "1"] <- "ADHD (parent report)"
ndar_subject01$ksa1_c[ndar_subject01$ksa1_c == "0"] <- NA
ndar_subject01$ksa1_p[ndar_subject01$ksa1_p == "0"] <- NA


ndar_subject01$ksa2_p[ndar_subject01$ksa2_p == "1"] <- "Oppositional Defiant Disorder (parent report)"
ndar_subject01$ksa2_p[ndar_subject01$ksa2_p == "0"] <- NA


ndar_subject01$ksa3_c[ndar_subject01$ksa3_c == "1"] <- "Conduct Disorder (child report)"
ndar_subject01$ksa3_p[ndar_subject01$ksa3_p == "1"] <-  "Conduct Disorder (parent report)"
ndar_subject01$ksa3_c[ndar_subject01$ksa3_c == "0"] <- NA
ndar_subject01$ksa3_p[ndar_subject01$ksa3_p == "0"] <- NA


ndar_subject01$ksa4_c[ndar_subject01$ksa4_c == "1"] <- "Panic Disorder (child report)"
ndar_subject01$ksa4_p[ndar_subject01$ksa4_p == "1"] <- "Panic Disorder (parent report)"
ndar_subject01$ksa4_c[ndar_subject01$ksa4_c == "0"] <- NA
ndar_subject01$ksa4_p[ndar_subject01$ksa4_p == "0"] <- NA


ndar_subject01$ksa5_c[ndar_subject01$ksa5_c == "1"] <- "Social Anxiety (child report)"
ndar_subject01$ksa5_p[ndar_subject01$ksa5_p == "1"] <- "Social Anxiety (parent report)"
ndar_subject01$ksa5_c[ndar_subject01$ksa5_c == "0"] <- NA
ndar_subject01$ksa5_p[ndar_subject01$ksa5_p == "0"] <- NA


ndar_subject01$ksa6_c[ndar_subject01$ksa6_c == "1"] <- "Generalized Anxiety (child report)"
ndar_subject01$ksa6_p[ndar_subject01$ksa6_p == "1"] <- "Generalized Anxiety (parent report)"
ndar_subject01$ksa6_c[ndar_subject01$ksa6_c == "0"] <- NA
ndar_subject01$ksa6_p[ndar_subject01$ksa6_p == "0"] <- NA


ndar_subject01$ksa7_c[ndar_subject01$ksa7_c == "1"] <- "Post-Traumatic Stress (child report)"
ndar_subject01$ksa7_p[ndar_subject01$ksa7_p == "1"] <- "Post-Traumatic Stress (parent report)"
ndar_subject01$ksa7_c[ndar_subject01$ksa7_c == "0"] <- NA
ndar_subject01$ksa7_p[ndar_subject01$ksa7_p == "0"] <- NA


ndar_subject01$ksa8_c[ndar_subject01$ksa8_c == "1"] <- "Major Depressive Disorder (child report))"
ndar_subject01$ksa8_p[ndar_subject01$ksa8_p == "1"] <- "Major Depressive Disorder (parent report)"
ndar_subject01$ksa8_c[ndar_subject01$ksa8_c == "0"] <- NA
ndar_subject01$ksa8_p[ndar_subject01$ksa8_p == "0"] <- NA


ndar_subject01$ksa9_c[ndar_subject01$ksa9_c == "1"] <- "Dysthymia (child report)"
ndar_subject01$ksa9_p[ndar_subject01$ksa9_p == "1"] <- "Dysthymia (parent report)"
ndar_subject01$ksa9_c[ndar_subject01$ksa9_c == "0"] <- NA
ndar_subject01$ksa9_p[ndar_subject01$ksa9_p == "0"] <- NA


ndar_subject01$ksa10_c[ndar_subject01$ksa10_c == "1"] <- "Mania/Hypomania (child report)"
ndar_subject01$ksa10_p[ndar_subject01$ksa10_p == "1"] <- "Mania/Hypomania (parent report)"
ndar_subject01$ksa10_c[ndar_subject01$ksa10_c == "0"] <- NA
ndar_subject01$ksa10_p[ndar_subject01$ksa10_p == "0"] <- NA


ndar_subject01$ksa11_p[ndar_subject01$ksa11_p == "1"] <- "Autism Spectrum Disorder (parent report)"
ndar_subject01$ksa11_p[ndar_subject01$ksa11_p == "0"] <- NA


ksads_dx <- cbind(ndar_subject01[,3:21])
ksads_dx <- apply(ksads_dx, 1, 
             function(x) paste(x[!is.na(x)], collapse = ", "))

ndar_subject01$phenotype <- ksads_dx
ndar_subject01$record_id <- gsub('.{3}$', '', ndar_subject01$record_id)
ndar_subject01 <- dplyr::rename(ndar_subject01, 
                                          src_subject_id = record_id)
ndar_subject01 <- subset(ndar_subject01, 
                         select = c(src_subject_id, phenotype))
ndar_subject01 <- merge(participantDat, ndar_subject01, by = "src_subject_id")

ndar_subject01$twins_study <- "No"
ndar_subject01$sibling_study <- "No"
ndar_subject01$family_study <- "No"
ndar_subject01$sample_taken <- "No"

genomics_subject02 <- ndar_subject01
ndar_subject01_dem$dem86_p___1[ndar_subject01_dem$dem86_p___1 == 1] <- "American Indian/Alaskan Native"
ndar_subject01_dem$dem86_p___2[ndar_subject01_dem$dem86_p___2 == 1] <- "Asian"
ndar_subject01_dem$dem86_p___3[ndar_subject01_dem$dem86_p___3 == 1] <- "Native Hawaiian or Other Pacific Islander"
ndar_subject01_dem$dem86_p___4[ndar_subject01_dem$dem86_p___4 == 1] <- "Black or African American"
ndar_subject01_dem$dem86_p___5[ndar_subject01_dem$dem86_p___5 == 1] <- "White"


ndar_subject01_dem$dem86_p___1[ndar_subject01_dem$dem86_p___1 == 0] <- NA
ndar_subject01_dem$dem86_p___2[ndar_subject01_dem$dem86_p___2 == 0] <- NA
ndar_subject01_dem$dem86_p___3[ndar_subject01_dem$dem86_p___3 == 0] <- NA
ndar_subject01_dem$dem86_p___4[ndar_subject01_dem$dem86_p___4 == 0] <- NA
ndar_subject01_dem$dem86_p___5[ndar_subject01_dem$dem86_p___5 == 0] <- NA


ndar_subject01_dem <- dplyr::rename(ndar_subject01_dem,
                                    src_subject_id = record_id)
ndar_subject01_dem <- merge(ndar_subject01, ndar_subject01_dem, 
                             by = "src_subject_id")

ndar_subject01_dem[,13:17]
race  <- cbind(ndar_subject01_dem$dem86_p___1, ndar_subject01_dem$dem86_p___2, ndar_subject01_dem$dem86_p___3,
               ndar_subject01_dem$dem86_p___4, ndar_subject01_dem$dem86_p___5) 
ndar_subject01$race <- apply(race, 1, 
              function(x) paste(x[!is.na(x)], collapse = ", "))

genomics_subject02$race <- ndar_subject01$race 
ndar_subject01$reg_mother_edu <- ndar_subject01_dem$dem80_p
ndar_subject01$reg_father_edu <- ndar_subject01_dem$dem81_p
ndar_subject01$employ_status <- ndar_subject01_dem$dem13_p
ndar_subject01$grade <- ndar_subject01_dem$dem82_p
ndar_subject01$phenotype_description <- ndar_subject01$phenotype

# Changing variable codes to align with NDAR
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 2] <- NA
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 3] <- 16
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 4] <- 17
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 5] <- 19
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 6] <- 20
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 7] <- 21
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 8] <- 22
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 9] <- 23
ndar_subject01$reg_mother_edu[ndar_subject01$reg_mother_edu == 10] <- 24



ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 2] <- NA
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 3] <- 16
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 4] <- 17
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 5] <- 19
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 6] <- 20
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 7] <- 21
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 8] <- 22
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 9] <- 23
ndar_subject01$reg_father_edu[ndar_subject01$reg_father_edu == 10] <- 24

ndar_subject01$grade[ndar_subject01$grade == 1] <- "3rd"
ndar_subject01$grade[ndar_subject01$grade == 2] <- "4th"
ndar_subject01$grade[ndar_subject01$grade == 3] <- "5th"
ndar_subject01$grade[ndar_subject01$grade == 4] <- "6th"
ndar_subject01$grade[ndar_subject01$grade == 5] <- "7th"
ndar_subject01$grade[ndar_subject01$grade == 6] <- "8th"
ndar_subject01$grade[ndar_subject01$grade == 7] <- "9th"
ndar_subject01$grade[ndar_subject01$grade == 8] <- "10th"
ndar_subject01$grade[ndar_subject01$grade == 9] <- "11th"
ndar_subject01$grade[ndar_subject01$grade == 10] <- "12th"

ndar_subject01$phenotype[ndar_subject01$phenotype == ""] <- "NA"
ndar_subject01$phenotype_description[ndar_subject01$phenotype_description == ""] <- "NA"
genomics_subject02$phenotype[genomics_subject02$phenotype == ""] <- "NA"


####################################################################
# SPSRQC
####################################################################
spsrqc01 <- dplyr::rename(spsrqc01,
                          src_subject_id = record_id,
                          spsrc_c6 = sps1, spsrc_c7 = sps2, 
                          spsrc_c8 = sps3,
                          spsrc_c10 = sps4, spsrc_c11 = sps5, 
                          spsrc_c13 = sps6, spsrc_c14 = sps7, 
                          spsrc_c16 = sps8, spsrc_c18 = sps9,
                          spsrc_c19 = sps10, spsrc_c20 = sps11, 
                          spsrc_c21 = sps12, spsrc_c22 = sps13,
                          spsrc_c23 = sps14, spsrc_c24 = sps15,
                          spsrc_c25 = sps16, spsrc_c26 = sps17,
                          spsrc_c27 = sps18, spsrc_c28 = sps19,
                          spsrc_c29 = sps20, spsrc_c30 = sps21,
                          spsrc_c31 = sps22, spsrc_c32 = sps23, 
                          spsrc_c34 = sps24, spsrc_c35 = sps25,
                          spsrc_c37 = sps26, spsrc_c39 = sps27,
                          spsrc_c41 = sps28, spsrc_c43 = sps29,
                          spsrc_c44 = sps30, spsrc_c45 = sps31,
                          spsrc_c46 = sps32, spsrc_c47 = sps33)

spsrqc01 <- subset(spsrqc01, select = -c(redcap_event_name))
spsrqc01 <- merge(participantDat, spsrqc01, by = "src_subject_id")

####################################################################
# BART-Y
####################################################################
barty01 <- dplyr::rename(barty01, 
                         src_subject_id = subject,
                         adjusted_avg = mean_pumps,
                         bartsdadjustedpumps = sd_pumps,
                         bart_totalpointsession = total_collected)
barty01 <- merge(participantDat, barty01, by = "src_subject_id")
barty01 <- subset(barty01, select = -c(sum_burst, postexp))

####################################################################
# ARI
####################################################################
ari01_a <- dplyr::rename(ari01_a, 
                         src_subject_id = record_id, 
                         timept = redcap_event_name,
                         ari_c_1 = ari1_c, ari_c_2 = ari2_c, ari_c_3 = ari3_c,
                         ari_c_4 = ari4_c, ari_c_5 = ari5_c, ari_c_6 = ari6_c,
                         ari_c_7 = ari7_c)

ari01_p <- dplyr::rename(ari01_p, 
                         src_subject_id = record_id, 
                         timept = redcap_event_name,
                         ari_c_1 = ari1_p, ari_c_2 = ari2_p, ari_c_3 = ari3_p,
                         ari_c_4 = ari4_p, ari_c_5 = ari5_p, ari_c_6 = ari6_p,
                         ari_c_7 = ari7_p)

ari01_a$timept <- 1
ari01_p$timept <- 1
ari01_a$respondent <- "Child"
ari01_p$respondent <- "Parent"

ari01_a <- merge(participantDat, ari01_a, by = "src_subject_id")
ari01_p <- merge(participantDat, ari01_p, by = "src_subject_id")

ari01 <- merge(ari01_a, ari01_p, by = "src_subject_id")
ari01 <- pivot_longer(ari01, 
                      cols = c("respondent.x", "respondent.y"),
                      values_to = "respondent")
colnames(ari01) <- gsub('.x','', colnames(ari01))
ari01 <- ari01[,-c(14:26)]
ari01 <- dplyr::rename(ari01, sex = s)

ari_cbind<- cbind(ari01$ari_c_1, ari01$ari_c_2, ari01$ari_c_3,
                  ari01$ari_c_4, ari01$ari_c_5, ari01$ari_c_6)

ari01$ari_c_1 <- as.integer(ari01$ari_c_1)
ari01$ari_c_2 <- as.integer(ari01$ari_c_2)
ari01$ari_c_3 <- as.integer(ari01$ari_c_3)
ari01$ari_c_4 <- as.integer(ari01$ari_c_4)
ari01$ari_c_5 <- as.integer(ari01$ari_c_5)
ari01$ari_c_6 <- as.integer(ari01$ari_c_6)
ari01$ari_c_7 <- as.integer(ari01$ari_c_7)



ari01$ari_averagescore <- rowMeans(ari_cbind, na.rm=T)
ari01$ari_totalscore <- rowSums(ari_cbind, na.rm = T)

####################################################################
# SITBI
####################################################################
sitbi01$record_id <- gsub('.{3}$', '', sitbi01$record_id)

sitbi01 <- dplyr::rename(sitbi01, 
                         src_subject_id = record_id,
                         visit = redcap_event_name,
                         t_sitbi_si_1 = sit1, suic_idea_age_first = sit4, 
                         suic_idea_intensity_wp = sit8, t_sitbi_si_7 = sit9,
                         suic_idea_future = sit10, suic_plan_age_first = sit12, 
                         t_sitbi_sp_16 = sit22, suic_plan_future = sit23, 
                         t_sitbi_asa_18 = sit24, t_sitbi_asa_19 = sit25,
                         suic_atmpt_interrupt_lifetime = sit26, t_sitbi_asa_20a = sit26a,
                         t_sitbi_asa_20a = sit26a, t_sitbi_asa_20b = sit26b, 
                         t_sitbi_asa_20c = sit26c,  suic_atmpt_ever = sit27, 
                         suic_atmpt_lifetime = sit28, t_sitbi_sa_28a = sit28a, 
                         t_sitbi_sa_28b = sit28b, suic_atmpt_lethal_injury = sit28c,
                         t_sitbi_sa_29 = sit29, suic_atmpt_future = sit30,
                         t_sitbi_nssi_47 = sit39, nonsuic_selfinj_future = sit41,
                         t_sibti_ir_52 = sit42, t_sibti_ir_53 = sit43, 
                         t_sibti_ir_98 = sit44, t_sibti_ir_99a = sit45,
                         t_sibti_ir_100 = sit45b, t_sibti_ir_102 = sit46, 
                         t_sibti_ir_103 = sit47, t_sibti_ir_105 = sit47b)
sitbi01 <- merge(participantDat, sitbi01, by = "src_subject_id")
sitbi01$visit <- "1"

sitbi01 <- subset(sitbi01, select = -c(suic_idea_age_first))
cbind(names(sitbi01)) 

sitbi <- sitbi01[,c(9, 12, 14:15, 20, 25, 30, 32:34)]
sitFun <- function(x){
  as.numeric(x)+1
}

sitbi01[,c(9, 12,  14:15, 20, 25, 30, 32:34)] <- lapply(sitbi, sitFun)

####################################################################
# ERC
####################################################################
erc01 <- dplyr::rename(erc01, 
                       src_subject_id = record_id,
                       timept = redcap_event_name,
                       erc5 = erc_5)

erc01$timept <- "1"
erc01["respondent"] <- "Parent"
erc01 <- merge(participantDat, erc01, by = "src_subject_id")

####################################################################
# INQ
####################################################################
inq01 <- dplyr::rename(inq01, 
                     src_subject_id = record_id, 
                     visit = redcap_event_name,
                     inq15_1 = inq1, inq15_2 = inq2, inq15_3 = inq3, 
                     inq15_4 = inq4, inq15_5 = inq5, inq15_6 = inq6, 
                     inq15_7 = inq7, inq15_8 = inq8, inq15_9 = inq9, 
                     inq15_10 = inq10, inq15_11 = inq11, inq15_12 = inq12, 
                     inq15_13 = inq13, inq15_14 = inq14, inq15_15 = inq15)
inq01 <- merge(participantDat, inq01, by = "src_subject_id")
inq01$visit <- "1"

inq <- inq01[,7:21]
fun <- function(x){
  as.numeric(x)-1
}

inq01[,7:21] <- lapply(inq, fun)

inq01[is.na(inq01)] <- "" 

####################################################################
# CERQ
####################################################################
cerq01 <- dplyr::rename(cerq01, 
                      src_subject_id = record_id, 
                      cepq_1 = cer1, cepq_2 = cer2, cepq_3 =  cer3, 
                      cepq_4 = cer4, cepq_5 = cer5, cepq_6 = cer6, 
                      cepq_7 = cer7, cepq_8 = cer8, cepq_9 = cer9,
                      cepq_10 = cer10, cepq_11 = cer11, cepq_12 = cer12,
                      cepq_13 = cer13, cepq_14 = cer14, cepq_15 = cer15,
                      cepq_16 = cer16, cepq_17 = cer17, cepq_18 = cer18)

cerq01 <- subset(cerq01, select = -c(redcap_event_name))
cerq01 <- merge(participantDat, cerq01)

####################################################################
# GSE
####################################################################
ngses01 <- dplyr::rename(ngses01, 
                     src_subject_id = record_id, 
                     timept = redcap_event_name, 
                     gse_01 = gse1, gse_02 = gse2, gse_03 = gse3,
                     gse_04 = gse4, gse_05 = gse5, gse_06 = gse6,
                     gse_07 = gse7, gse_08 = gse8, gse_09 = gse9,
                     gse_10 = gse10)
ngses01 <- merge(participantDat, ngses01, by = "src_subject_id")
ngses01$timept <- "1"

####################################################################
# RPA
####################################################################
rpas01 <- dplyr::rename(rpas01, 
                     src_subject_id = record_id, 
                     timept = redcap_event_name, 
                     rpac1 = rpa1_a, rpac3 = rpa2_a, rpac15 = rpa3_a, 
                     rpac4 = rpa4_a, rpac13 = rpa5_a, rpac9 = rpa6_a,
                     rpac8  = rpa7_a, rpac16 = rpa8_a, rpac14 = rpa9_a,
                     rpac12 = rpa10_a, rpac7 = rpa11_a, rsq_q16 = rpa12_a,
                     rpac11 = rpa13_a, rpac17 = rpa14_a, rpac6 = rpa15_a, 
                     rpac2 = rpa16_a, rpac5 = rpa17_a)
rpas01 <- merge(participantDat, rpas01, by = "src_subject_id")
rpas01$timept <- "1"
is.na(rpas01) <- ""
View(rpas01)
rpas01[is.na(rpas01)] <- ""
####################################################################
# EXPORT CSV FILES
####################################################################
write.csv(scared01, '/Users/kileyyeaman/Desktop/NDAR/Clean/scared01.csv', row.names = F)
write.csv(dass01, '/Users/kileyyeaman/Desktop/NDAR/Clean/dass01.csv', row.names = F)
write.csv(genomics_subject02, '/Users/kileyyeaman/Desktop/NDAR/Clean/genomics_subject02.csv', row.names = F)
write.csv(ndar_subject01, '/Users/kileyyeaman/Desktop/NDAR/Clean/ndar_subject01.csv', row.names = F)
write.csv(barty01, '/Users/kileyyeaman/Desktop/NDAR/Clean/barty01.csv', row.names = F)
write.csv(erc01, '/Users/kileyyeaman/Desktop/NDAR/Clean/erc01.csv', row.names = F)
write.csv(sitbi01, '/Users/kileyyeaman/Desktop/NDAR/Clean/sitbi01.csv', row.names = F)
write.csv(inq01, '/Users/kileyyeaman/Desktop/NDAR/Clean/inq01.csv', row.names = F)
write.csv(cerq01, '/Users/kileyyeaman/Desktop/NDAR/Clean/cerq01.csv', row.names = F)
write.csv(ngses01, '/Users/kileyyeaman/Desktop/NDAR/Clean/ngses01.csv', row.names = F)
write.csv(rpas01, '/Users/kileyyeaman/Desktop/NDAR/Clean/rpas01.csv', row.names = F)
write.csv(ari01, '/Users/kileyyeaman/Desktop/NDAR/Clean/ari01.csv', row.names = F)


