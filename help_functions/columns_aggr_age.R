library(data.table)
source("help_functions/seven_day_inzidenz.R")
main_data <- readRDS("data/main_data")
data <- copy(main_data)
setDT(main_data)
age_groups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
# for the first to age-groups (we want to aggregate them)
data <- data[, paste0("total_", age_groups[1], age_groups[2]) := sum(.SD), 
             .SDcols = colnames(main_data)[(grepl(age_groups[1],
                                                  colnames(main_data)) |
                                              grepl(age_groups[2], colnames(main_data))) &
                                             grepl("number_cases", colnames(main_data))],
             by = rep_date_divi]
# for all the other age_groups
for (i in 3:6) {
  data <- data[, paste0("total_", age_groups[i]) := sum(.SD), 
                            .SDcols = colnames(main_data)[grepl(age_groups[i], colnames(main_data))&
                                                          grepl("number_cases", colnames(main_data))],
                            by = rep_date_divi]
}

colnames(data)[96:100] <- c("total_00_14", "total_15_34", "total_35_59", "total_60_79", "total_80plus")
