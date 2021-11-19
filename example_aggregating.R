library(data.table)
main_data <- readRDS("data/main_data")
setDT(main_data)
cnames <- colnames(main_data)
main_data[, female_cases := sum(.SD),
                 .SDcols = cnames[grepl("W", cnames) & grepl("number_cases", cnames)], by = rep_date]
main_data[, male_cases := sum(.SD),
          .SDcols = cnames[grepl("M", cnames) & grepl("number_cases", cnames)], by = rep_date]
