library(checkmate)
library(data.table)
library(tidyverse)
library(data.table)

datensatz <- readRDS("data/prakt_clean_data")
data.table(main_data)

##CFR
#Komplett Deutschland
sdi_total <- main_data$seven_day_inz #7-T-Inzidenz
CFR_total <- sum(main_data$seven_day_death_inz)/sum(sdi_total) #CFR 
CFR_total_pre_okt <- sum(main_data$seven_day_death_inz[-c(274:395)])/sum(sdi_total[-c(274:395)])

sdi_jul_total <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz
sdi_aug_total <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz
sdi_sep_total <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz

sdi_jul_total_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz
sdi_aug_total_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz
sdi_sep_total_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz

CFR_jul_total <- sum(sdi_jul_total_death)/sum(sdi_jul_total)
CFR_aug_total <- sum(sdi_aug_total_death)/sum(sdi_aug_total)
CFR_sep_total <- sum(sdi_sep_total_death)/sum(sdi_sep_total)

CFR_jul_sep_total <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz)/
  sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz)

#Altersgruppe 0-14 Jahre
inhabitants_A00_A14 = 11477737

sdi_A00_A14 <- main_data$seven_day_inz_A00_A14
sdi_A00_A14_death <- main_data$seven_day_death_inz_A00_A14
CFR_A00_A14 <- sum(sdi_A00_A14_death)/sum(sdi_A00_A14)

sdi_jul_A00_A14 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A00_A14
sdi_aug_A00_A14 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A00_A14
sdi_sep_A00_A14 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A00_A14

sdi_jul_A00_A14_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A00_A14
sdi_aug_A00_A14_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A00_A14
sdi_sep_A00_A14_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A00_A14

CFR_jul_A00_A14 <- sum(sdi_jul_A00_A14_death)/sum(sdi_jul_A00_A14)
CFR_aug_A00_A14 <- sum(sdi_aug_A00_A14_death)/sum(sdi_aug_A00_A14)
CFR_sep_A00_A14 <- sum(sdi_sep_A00_A14_death)/sum(sdi_sep_A00_A14)

CFR_jul_sep_A00_A14 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A00_A14)/
  sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A00_A14)

#Altersgruppe 15-34 Jahre
 inhabitants_A15_A34 = 18921292

 sdi_A15_A34 <- main_data$seven_day_inz_A15_A34
 sdi_A15_A34_death <- main_data$seven_day_death_inz_A15_A34
 CFR_A15_A34 <- sum(sdi_A15_A34_death)/sum(sdi_A15_A34)
 
 sdi_jul_A15_A34 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A15_A34
 sdi_aug_A15_A34 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A15_A34
 sdi_sep_A15_A34 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A15_A34
 
 sdi_jul_A15_A34_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A15_A34
 sdi_aug_A15_A34_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A15_A34
 sdi_sep_A15_A34_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A15_A34
 
 CFR_jul_A15_A34 <- sum(sdi_jul_A15_A34_death)/sum(sdi_jul_A15_A34)
 CFR_aug_A15_A34 <- sum(sdi_aug_A15_A34_death)/sum(sdi_aug_A15_A34)
 CFR_sep_A15_A34 <- sum(sdi_sep_A15_A34_death)/sum(sdi_sep_A15_A34) 

 
 CFR_jul_sep_A15_A34 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A15_A34)/
   sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A15_A34)
 
#Altersgruppe 35-59 Jahre
 inhabitants_A35_A59 = 27600978
 
 sdi_A35_A59 <- main_data$seven_day_inz_A35_A59
 sdi_A35_A59_death <- main_data$seven_day_death_inz_A35_A59
 CFR_A35_A59 <- sum(sdi_A35_A59_death)/sum(sdi_A35_A59)
 
 sdi_jul_A35_A59 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A35_A59
 sdi_aug_A35_A59 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A35_A59
 sdi_sep_A35_A59 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A35_A59
 
 sdi_jul_A35_A59_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A35_A59
 sdi_aug_A35_A59_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A35_A59
 sdi_sep_A35_A59_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A35_A59
 
 CFR_jul_A35_A59 <- sum(sdi_jul_A35_A59_death)/sum(sdi_jul_A35_A59)
 CFR_aug_A35_A59 <- sum(sdi_aug_A35_A59_death)/sum(sdi_aug_A35_A59)
 CFR_sep_A35_A59 <- sum(sdi_sep_A35_A59_death)/sum(sdi_sep_A35_A59) 
 
 CFR_jul_sep_A35_A59 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A35_A59)/
   sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A35_A59)
 
#Altersgruppe 60-79 Jahre
 inhabitants_A60_A79 = 18153339
 
 sdi_A60_A79 <- main_data$seven_day_inz_A60_A79
 sdi_A60_A79_death <- main_data$seven_day_death_inz_A60_A79
 CFR_A60_A79 <- sum(sdi_A60_A79_death)/sum(sdi_A60_A79)
 
 sdi_jul_A60_A79 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A60_A79
 sdi_aug_A60_A79 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A60_A79
 sdi_sep_A60_A79 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A60_A79
 
 sdi_jul_A60_A79_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A60_A79
 sdi_aug_A60_A79_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A60_A79
 sdi_sep_A60_A79_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A60_A79
 
 CFR_jul_A60_A79 <- sum(sdi_jul_A60_A79_death)/sum(sdi_jul_A60_A79)
 CFR_aug_A60_A79 <- sum(sdi_aug_A60_A79_death)/sum(sdi_aug_A60_A79)
 CFR_sep_A60_A79 <- sum(sdi_sep_A60_A79_death)/sum(sdi_sep_A60_A79) 
 
 CFR_jul_sep_A60_A79 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A60_A79)/
   sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A60_A79)
 
#Altersgruppe 80+ Jahre
 inhabitants_A80 = 5936434
 
 sdi_A80 <- main_data$seven_day_inz_A80
 sdi_A80_death <- main_data$seven_day_death_inz_A80
 CFR_A80 <- sum(sdi_A80_death)/sum(sdi_A80)
 
 sdi_jul_A80 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A80
 sdi_aug_A80 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A80
 sdi_sep_A80 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A80
 
 sdi_jul_A80_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A80
 sdi_aug_A80_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A80
 sdi_sep_A80_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A80
 
 CFR_jul_A80 <- sum(sdi_jul_A80_death)/sum(sdi_jul_A80)
 CFR_aug_A80 <- sum(sdi_aug_A80_death)/sum(sdi_aug_A80)
 CFR_sep_A80 <- sum(sdi_sep_A80_death)/sum(sdi_sep_A80) 
 
 CFR_jul_sep_A80 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A80)/
   sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A80)
 
##Rückrechnung von CFR auf Inzidenz ab Oktober
 
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz)/CFR_jul_sep_total
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A00_A14)/CFR_jul_sep_A00_A14
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A15_A34)/CFR_jul_sep_A15_A34
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A35_A59)/CFR_jul_sep_A35_A59
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A60_A79)/CFR_jul_sep_A60_A79
 (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A80)/CFR_jul_sep_A80
  main_data["2020-12-31" == rep_date_divi]$seven_day_death_inz
#CFR_jul_total <- sum(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_death_cases)/
#  sum(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_cases)

#CFR_aug_total <- sum(main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$total_death_cases)/
#  sum(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_cases)                      

#CFR_sep_total <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$total_death_cases)/
#  sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$total_cases)


#CFR_jul_total_sdi_2 <- sum(seven_day_inz(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_death_cases))/
#  sum(seven_day_inz(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_cases))

#CFR_aug_total_sdi_2 <- sum(seven_day_inz(main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$total_death_cases))/
#  sum(seven_day_inz(main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_cases))                       

#CFR_sep_total_sdi_2 <- sum(seven_day_inz(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$total_death_cases))/
#  sum(seven_day_inz(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$total_cases))



#CFR_total_sdi_3 <- sum(seven_day_inz(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_death_cases))/
#  sum(seven_day_inz(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$total_cases))

