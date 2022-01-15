library(checkmate)
library(data.table)
library(tidyverse)

main_data <- readRDS("data/main_data")
source("examples_of_code/example_aggregating.R")
data.table(main_data)

##CFR
#Komplett Deutschland


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

##Falls noetig
#sdi_7_jul_A00_A14_death <- main_data["2020-08-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A00_A14
#sdi_7_aug_A00_A14_death <- main_data["2020-09-08" > rep_date_divi & rep_date_divi >= "2020-08-08"]$seven_day_death_inz_A00_A14
#sdi_7_sep_A00_A14_death <- main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-09-08"]$seven_day_death_inz_A00_A14

#CFR_7_jul_A00_A14 <- sum(sdi_7_jul_A00_A14_death)/sum(sdi_jul_A00_A14)
#CFR_7_aug_A00_A14 <- sum(sdi_7_aug_A00_A14_death)/sum(sdi_aug_A00_A14)
#CFR_7_sep_A00_A14 <- sum(sdi_7_sep_A00_A14_death)/sum(sdi_sep_A00_A14)

CFR_7_jul_sep_A00_A14 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A00_A14)/
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
 
 CFR_7_jul_sep_A15_A34 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A15_A34)/
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
 
 CFR_7_jul_sep_A35_A59 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A35_A59)/
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
 
 CFR_7_jul_sep_A60_A79 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A60_A79)/
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
 
 CFR_7_jul_sep_A80 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A80)/
    sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A80)
 
 #Altersgruppe 15-80+ Jahre
 inhabitants_A15_A80 = 70612043
 
 sdi_A15_A80 <- main_data$seven_day_inz_A15_A80
 sdi_A15_A80_death <- main_data$seven_day_death_inz_A15_A80
 CFR_A15_A80 <- sum(sdi_A15_A80_death)/sum(sdi_A15_A80)
 
 sdi_jul_A15_A80 <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A15_A80
 sdi_aug_A15_A80 <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_inz_A15_A80
 sdi_sep_A15_A80 <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_inz_A15_A80
 
 sdi_jul_A15_A80_death <- main_data["2020-08-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A15_A80
 sdi_aug_A15_A80_death <- main_data["2020-09-01" > rep_date_divi & rep_date_divi >= "2020-08-01"]$seven_day_death_inz_A15_A80
 sdi_sep_A15_A80_death <- main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-09-01"]$seven_day_death_inz_A15_A80
 
 CFR_jul_A15_A80 <- sum(sdi_jul_A15_A80_death)/sum(sdi_jul_A15_A80)
 CFR_aug_A15_A80 <- sum(sdi_aug_A15_A80_death)/sum(sdi_aug_A15_A80)
 CFR_sep_A15_A80 <- sum(sdi_sep_A15_A80_death)/sum(sdi_sep_A15_A80)
 
 CFR_jul_sep_A15_A80 <- sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_death_inz_A15_A80)/
    sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A15_A80)
 
 CFR_7_jul_sep_A15_A80 <- sum(main_data["2020-10-08" > rep_date_divi & rep_date_divi >= "2020-07-08"]$seven_day_death_inz_A15_A80)/
    sum(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"]$seven_day_inz_A15_A80)
 
##Rueckrechnung von CFR auf Inzidenz ab Oktober
 #Original
 kor_inz_A00_A14 <- (main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A00_A14)/CFR_jul_sep_A00_A14
 kor_inz_A15_A34 <-(main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A15_A34)/CFR_jul_sep_A15_A34
 kor_inz_A35_A59 <-(main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A35_A59)/CFR_jul_sep_A35_A59
 kor_inz_A60_A79 <-(main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A60_A79)/CFR_jul_sep_A60_A79
 kor_inz_A80 <-(main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A80)/CFR_jul_sep_A80
 kor_inz_A15_A80 <-(main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A15_A80)/CFR_jul_sep_A15_A80
 
 
##Rueckrechnung von CFR auf Inzidenz ab Oktober, mit 7-Tages-Lag
 kor_7_inz_A00_A14 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A00_A14)/CFR_7_jul_sep_A00_A14
 kor_7_inz_A15_A34 <-(main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A15_A34)/CFR_7_jul_sep_A15_A34
 kor_7_inz_A35_A59 <-(main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A35_A59)/CFR_7_jul_sep_A35_A59
 kor_7_inz_A60_A79 <-(main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A60_A79)/CFR_7_jul_sep_A60_A79
 kor_7_inz_A80 <-(main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A80)/CFR_7_jul_sep_A80
 kor_7_inz_A15_A80 <-(main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_death_inz_A15_A80)/CFR_7_jul_sep_A15_A80
  
 ##Zeite Schätzung der Gesamtinzidenz
 ##Gesamtbevoelkerung_genau = 82089780
 
 seven_day_inz_A00_A14_okt_dez <- main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_inz_A00_A14
 
 seven_day_inz_7_A00_A14_okt_dez <- main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-08"]$seven_day_inz_A00_A14
 
 
 kor_inz_total <- ((seven_day_inz_A00_A14_okt_dez*(11477737/82089780))+(kor_inz_A15_A34*(18921292/82089780))+
                           (kor_inz_A35_A59*(27600978/82089780))+(kor_inz_A60_A79*(18153339/82089780))+(kor_inz_A80*(5936434/82089780)))
 
 kor_inz_total_zwei_15_80 <- ((kor_inz_A15_A34*(18921292/70612043))+(kor_inz_A35_A59*(27600978/70612043))+
                                 (kor_inz_A60_A79*(18153339/70612043))+(kor_inz_A80*(5936434/70612043)))
 
 
 kor_7_inz_total <- ((seven_day_inz_7_A00_A14_okt_dez*(11477737/82089780))+(kor_7_inz_A15_A34*(18921292/82089780))+
                        (kor_7_inz_A35_A59*(27600978/82089780))+(kor_7_inz_A60_A79*(18153339/82089780))+(kor_7_inz_A80*(5936434/82089780)))
 
 kor_7_inz_total_zwei_15_80 <- ((kor_7_inz_A15_A34*(18921292/70612043))+(kor_7_inz_A35_A59*(27600978/70612043))+
                                (kor_7_inz_A60_A79*(18153339/70612043))+(kor_7_inz_A80*(5936434/70612043)))
 
 
 #main_data["2020-12-31" == rep_date_divi]$seven_day_death_inz
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

##plots aufgeschlüsselt nach Altersgruppen
 
 data_okt_dez <- main_data["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]
 
 data_okt_dez$kor_inz_A00_A14 <- kor_inz_A00_A14
 data_okt_dez$kor_inz_A15_A34 <- kor_inz_A15_A34
 data_okt_dez$kor_inz_A35_A59 <- kor_inz_A35_A59
 data_okt_dez$kor_inz_A60_A79 <- kor_inz_A60_A79
 data_okt_dez$kor_inz_A80 <- kor_inz_A80
 data_okt_dez$kor_inz_total <- kor_inz_total
 
 data_okt_dez$kor_7_inz_A00_A14 <- kor_7_inz_A00_A14
 data_okt_dez$kor_7_inz_A15_A34 <- kor_7_inz_A15_A34
 data_okt_dez$kor_7_inz_A35_A59 <- kor_7_inz_A35_A59
 data_okt_dez$kor_7_inz_A60_A79 <- kor_7_inz_A60_A79
 data_okt_dez$kor_7_inz_A80 <- kor_7_inz_A80
 data_okt_dez$kor_7_inz_total <- kor_7_inz_total
 
 farben4 <- c("gemeldete" = "#000000", "korrigierte" = "#E31A1C", "lagkorrigiert" = "#E31A1C")
 dt_korr_7_inz_15_34 <- copy(data_okt_dez[, c("rep_date_divi", "seven_day_inz_A15_A34", "kor_inz_A15_A34", "kor_7_inz_A15_A34")])
 setnames(dt_korr_7_inz_15_34, c("seven_day_inz_A15_A34", "kor_inz_A15_A34", "kor_7_inz_A15_A34"), c("gemeldete", "korrigierte", "lagkorrigiert"))
 
 dt_korr_7_inz_15_34[, time := dt_korr_7_inz_15_34$rep_date_divi]
 dt_korr_7_inz_15_34 <- dt_korr_7_inz_15_34[, -1]
 dt_korr_7_inz_15_34_melt <- melt(dt_korr_7_inz_15_34, id.vars = "time", value.name = "inz")
 plot_korr_7_inz_15_34 <- dt_korr_7_inz_15_34_melt %>% 
   ggplot(aes(x = time, y = inz, color = variable, group=variable)) +
   geom_line() +
   labs(x = "", y = "7-Tages-Inzidenz") +
   scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
   scale_color_manual(values = farben4, name = "Art der Inzidenz") +
   theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
         axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
   scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
                date_labels = "%d. %b %Y")
 plot_korr_7_inz_15_34
 ggsave("Plots/korrigierte Inzidenz mit Lag 15-34.png", plot = plot_korr_7_inz_15_34, width = 20, height = 10, units = c("cm"))
 
 
 dt_korr_7_inz_35_59 <- copy(data_okt_dez[, c("rep_date_divi", "seven_day_inz_A35_A59", "kor_inz_A35_A59", "kor_7_inz_A35_A59")])
 setnames(dt_korr_7_inz_35_59, c("seven_day_inz_A35_A59", "kor_inz_A35_A59", "kor_7_inz_A35_A59"), c("gemeldete", "korrigierte", "lagkorrigiert"))
 
 dt_korr_7_inz_35_59[, time := dt_korr_7_inz_35_59$rep_date_divi]
 dt_korr_7_inz_35_59 <- dt_korr_7_inz_35_59[, -1]
 dt_korr_7_inz_35_59_melt <- melt(dt_korr_7_inz_35_59, id.vars = "time", value.name = "inz")
 plot_korr_7_inz_35_59 <- dt_korr_7_inz_35_59_melt %>% 
   ggplot(aes(x = time, y = inz, color = variable)) +
   geom_line() +
   labs(x = "", y = "7-Tages-Inzidenz") +
   scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
   scale_color_manual(values = farben4, name = "Art der Inzidenz") +
   theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
         axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
   scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
                date_labels = "%d. %b %Y")
 plot_korr_7_inz_35_59
 ggsave("Plots/korrigierte Inzidenz mit Lag 35-59.png", plot = plot_korr_7_inz_35_59, width = 20, height = 10, units = c("cm"))
 
 
 
 dt_korr_7_inz_60_79 <- copy(data_okt_dez[, c("rep_date_divi", "seven_day_inz_A60_A79", "kor_inz_A60_A79", "kor_7_inz_A60_A79")])
 setnames(dt_korr_7_inz_60_79, c("seven_day_inz_A60_A79", "kor_inz_A60_A79", "kor_7_inz_A60_A79"), c("gemeldete", "korrigierte", "lagkorrigiert"))
 
 dt_korr_7_inz_60_79[, time := dt_korr_7_inz_60_79$rep_date_divi]
 dt_korr_7_inz_60_79 <- dt_korr_7_inz_60_79[, -1]
 dt_korr_7_inz_60_79_melt <- melt(dt_korr_7_inz_60_79, id.vars = "time", value.name = "inz")
 plot_korr_7_inz_60_79 <- dt_korr_7_inz_60_79_melt %>% 
   ggplot(aes(x = time, y = inz, color = variable)) +
   geom_line() +
   labs(x = "", y = "7-Tages-Inzidenz") +
   scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
   scale_color_manual(values = farben4, name = "Art der Inzidenz") +
   theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
         axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
   scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
                date_labels = "%d. %b %Y")
 plot_korr_7_inz_60_79
 ggsave("Plots/korrigierte Inzidenz mit Lag 60-79.png", plot = plot_korr_7_inz_60_79, width = 20, height = 10, units = c("cm"))
 
 
 dt_korr_7_inz_80 <- copy(data_okt_dez[, c("rep_date_divi", "seven_day_inz_A80", "kor_inz_A80", "kor_7_inz_A80")])
 setnames(dt_korr_7_inz_80, c("seven_day_inz_A80", "kor_inz_A80", "kor_7_inz_A80"), c("gemeldete", "korrigierte", "lagkorrigiert"))
 
 dt_korr_7_inz_80[, time := dt_korr_7_inz_80$rep_date_divi]
 dt_korr_7_inz_80 <- dt_korr_7_inz_80[, -1]
 dt_korr_7_inz_80_melt <- melt(dt_korr_7_inz_80, id.vars = "time", value.name = "inz")
 plot_korr_7_inz_80 <- dt_korr_7_inz_80_melt %>% 
   ggplot(aes(x = time, y = inz, color = variable)) +
   geom_line() +
   labs(x = "", y = "7-Tages-Inzidenz") +
   scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
   scale_color_manual(values = farben4, name = "Art der Inzidenz") +
   theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
         axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
   scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
                date_labels = "%d. %b %Y")
 plot_korr_7_inz_80
 ggsave("Plots/korrigierte Inzidenz mit Lag über 80.png", plot = plot_korr_7_inz_80, width = 20, height = 10, units = c("cm"))
 
 
 ##plots aller Altersgruppen mit 7 Tage Lag, Unterschätzung
 data_okt_dez$unterschaetzung_total_7 <- data_okt_dez$kor_7_inz_total/data_okt_dez$seven_day_inz
 data_okt_dez$unterschaetzung_A15_A34_7 <- data_okt_dez$kor_7_inz_A15_A34/data_okt_dez$seven_day_inz_A15_A34
 data_okt_dez$unterschaetzung_A35_A59_7 <- data_okt_dez$kor_7_inz_A35_A59/data_okt_dez$seven_day_inz_A35_A59
 data_okt_dez$unterschaetzung_A60_A79_7 <- data_okt_dez$kor_7_inz_A60_A79/data_okt_dez$seven_day_inz_A60_A79
 data_okt_dez$unterschaetzung_A80_7 <- data_okt_dez$kor_7_inz_A80/data_okt_dez$seven_day_inz_A80
 
 
 
 dt_unterschaetzung_7 <- data_okt_dez[, c("rep_date_divi", "unterschaetzung_total_7", "unterschaetzung_A15_A34_7", "unterschaetzung_A35_A59_7", 
                                          "unterschaetzung_A60_A79_7","unterschaetzung_A80_7")]
 setnames(dt_unterschaetzung_7, c("unterschaetzung_total_7", "unterschaetzung_A15_A34_7", "unterschaetzung_A35_A59_7", "unterschaetzung_A60_A79_7",
                                  "unterschaetzung_A80_7"), c("Gesamt", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "Über 79 Jahre"))
 dt_unterschaetzung_7[, time := dt_unterschaetzung_7$rep_date_divi]
 dt_unterschaetzung_7 <- dt_unterschaetzung_7[, -1]
 dt_unterschaetzung_melt_7 <- melt(dt_unterschaetzung_7, id.vars = "time", value.name = "inz")
 
 farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
              "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")
 
 plot_unterschaetzung_7 <- dt_unterschaetzung_melt_7 %>% 
    ggplot(aes(x = time, y = inz, color = variable)) +
    geom_line() +
    labs(x = "", y = "Faktor der Unterschätzung") +
    scale_y_continuous(labels = scales::comma, limits = c(0, 3), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) +
    scale_color_manual(values = farben3, name = "Altersgruppe") +
    geom_hline(yintercept = 1, color = "#808080") + 
    theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
    scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
                 date_labels = "%d. %b %Y") + theme_bw()
 plot_unterschaetzung_7
 ggsave("Plots/Unterschätzung Inzidenz mit Lag.png", plot = plot_unterschaetzung_7, width = 20, height = 10, units = c("cm"))