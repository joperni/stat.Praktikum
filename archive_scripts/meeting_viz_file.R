source("Praktikum.R")

# Vom 01.01.2020 bis zum Ende des Beobachtungszeitraum
ccf(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_inz],
    main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_death_inz], main = "01.01. - Ende Vergleichszeitraum")
# Im vgl ungeklaettet
ccf(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-01-01",
              total_cases],
    main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-01-01",
              total_death_cases], main = "01.01. - Ende Vergleichszeitraum (ungeklättet)")

# Vom 01.01.2020 bis zum Start unseres Beobachtungszeitraums
ccf(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_inz],
    main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_death_inz], main = "01.01. - Anfang Vergleichszeitraum")

# Vergleichszeitraum
ccf(main_data["2020-09-30" > rep_date_divi & rep_date_divi >= "2020-06-01",
              seven_day_inz],
    main_data["2020-09-30" > rep_date_divi & rep_date_divi >= "2020-06-01",
              seven_day_death_inz], main = "Vergleichszeitraum")

# Vom 01.01.2020 bis zum Start des Vergleichszeitraums
ccf(main_data["2020-06-01" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_inz],
    main_data["2020-06-01" > rep_date_divi & rep_date_divi >= "2020-01-01",
              seven_day_death_inz], main = "01.01. - Anfang Vergleichszeitraum")

# unseres Beobachtungszeitraums
ccf(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25",
              seven_day_inz],
    main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25",
              seven_day_death_inz], main = "Beobachtungszeitraum")
