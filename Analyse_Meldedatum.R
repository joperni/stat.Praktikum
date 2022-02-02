library(plyr)
main_data <- read.csv("~/Downloads/Covid19.csv")
main_data$Meldedatum = as.Date(main_data$Meldedatum)
main_data <- subset(main_data, Meldedatum <= "2021-01-31")

main_age<- ddply(main_data,.(Meldedatum, Altersgruppe),numcolwise(sum))
main_age$Altersgruppe = as.factor(main_age$Altersgruppe)

# Todesfälle und Fälle Ü80
main_age_U79 <- subset(main_age, Altersgruppe == "A80+")

deaths_U79_7d <- main_age_U79[,c("Meldedatum", "AnzahlTodesfall")]
deaths_U79_7d$AnzahlTodesfall = seven_day_inz(deaths_U79_7d$AnzahlTodesfall, inhabitants = 5936434)
ggplot(data=deaths_U79_7d, aes(x= Meldedatum, y=AnzahlTodesfall)) + geom_line()

cases_U79_7d <- main_age_U79[,c("Meldedatum", "AnzahlFall")]
cases_U79_7d$AnzahlFall = seven_day_inz(cases_U79_7d$AnzahlFall, inhabitants = 5936434)
ggplot(data=cases_U79_7d, aes(x= Meldedatum, y=AnzahlFall)) + geom_line()

# Todesfälle 60-79
main_age_6079 <- subset(main_age, Altersgruppe == "A60-A79")

deaths_6079_7d <- main_age_6079[,c("Meldedatum", "AnzahlTodesfall")]
deaths_6079_7d$AnzahlTodesfall = seven_day_inz(deaths_6079_7d$AnzahlTodesfall, inhabitants = 18153339)
ggplot(data=deaths_6079_7d, aes(x= Meldedatum, y=AnzahlTodesfall)) + geom_line()

# Todesfälle und Fälle insgesamt
main<- ddply(main_data,.(Meldedatum),numcolwise(sum))

deaths_7d <- main[,c("Meldedatum", "AnzahlTodesfall")]
deaths_7d$AnzahlTodesfall = seven_day_inz(deaths_7d$AnzahlTodesfall)
ggplot(data=deaths_7d, aes(x= Meldedatum, y=AnzahlTodesfall)) + geom_line()

cases_7d <- main[,c("Meldedatum", "AnzahlFall")]
cases_7d$AnzahlFall = seven_day_inz(cases_7d$AnzahlFall)
ggplot(data=cases_7d, aes(x= Meldedatum, y=AnzahlFall)) + geom_line()

## Bei der Inzidenz scheint das Bild zu stimmen, nur bei den Toddesfällen nicht
