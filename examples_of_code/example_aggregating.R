library(data.table)
main_data <- readRDS("data/main_data")
setDT(main_data)
cnames <- colnames(main_data)

main_data[, female_cases := sum(.SD),
                 .SDcols = cnames[grepl("W", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
main_data[, male_cases := sum(.SD),
          .SDcols = cnames[grepl("M", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]


main_data[, male_cases := sum(.SD),
          .SDcols = cnames[grepl("M", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A00_A04_cases := sum(.SD),
          .SDcols = cnames[grepl("A00-A04", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A05_A14_cases := sum(.SD),
          .SDcols = cnames[grepl("A05-A14", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A15_A34_cases := sum(.SD),
          .SDcols = cnames[grepl("A15-A34", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A35_A59_cases := sum(.SD),
          .SDcols = cnames[grepl("A35-A59", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A60_A79_cases := sum(.SD),
          .SDcols = cnames[grepl("A60-A79", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A80_cases := sum(.SD),
          .SDcols = cnames[grepl("A80+", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

main_data[, A00_A14_cases := sum(.SD),
          .SDcols = cnames[(grepl("A00-A04", cnames)|grepl("A05-A14", cnames)) & grepl("number_cases", cnames)], by = rep_date_divi]


main_data[, A00_A04_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A00-A04", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A05_A14_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A05-A14", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A15_A34_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A15-A34", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A35_A59_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A35-A59", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A60_A79_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A60-A79", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A80_death_cases := sum(.SD),
          .SDcols = cnames[grepl("A80+", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data[, A00_A14_death_cases := sum(.SD),
          .SDcols = cnames[(grepl("A00-A04", cnames)|grepl("A05-A14", cnames)) & grepl("number_case_death", cnames)], by = rep_date_divi]

main_data$seven_day_inz <- seven_day_inz(main_data$total_cases)
main_data$seven_day_death_inz <- seven_day_inz(main_data$total_death_cases)

main_data$seven_day_inz_A00_A14 <- seven_day_inz(main_data$A00_A14_cases, 11477737)
main_data$seven_day_death_inz_A00_A14 <- seven_day_inz(main_data$A00_A14_death_cases, 11477737)

main_data$seven_day_inz_A15_A34 <- seven_day_inz(main_data$A15_A34_cases, 18921292)
main_data$seven_day_death_inz_A15_A34 <- seven_day_inz(main_data$A15_A34_death_cases, 18921292)

main_data$seven_day_inz_A35_A59 <- seven_day_inz(main_data$A35_A59_cases, 27600978)
main_data$seven_day_death_inz_A35_A59 <- seven_day_inz(main_data$A35_A59_death_cases, 27600978)

main_data$seven_day_inz_A60_A79 <- seven_day_inz(main_data$A60_A79_cases, 18153339)
main_data$seven_day_death_inz_A60_A79 <- seven_day_inz(main_data$A60_A79_death_cases, 18153339)

main_data$seven_day_inz_A80 <- seven_day_inz(main_data$A80_cases, 5936434)
main_data$seven_day_death_inz_A80 <- seven_day_inz(main_data$A80_death_cases, 5936434)

