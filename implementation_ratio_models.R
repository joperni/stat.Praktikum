library(segmented)
library(ggplot2)
library(checkmate)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(cowplot)
library(grid)
source("examples_of_code/example_aggregating.R")
source("help_functions/model_help_functions.R")
setDT(main_data)

#  Choose our "beobachtungszeitraum"
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-10-01"])

set.seed(1352674267)

age <- c("overall", "15_34", "35_59", "60_79", "80")

sdi_cases_colnames <- c("seven_day_inz", "seven_day_inz_A15_A34",
                        "seven_day_inz_A35_A59", "seven_day_inz_A60_A79", "seven_day_inz_A80")
sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A15_A34",
                         "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")
data[, paste("ratio", age, sep = "_") := 
       data[, .SD, .SDcols = sdi_deaths_colnames] / data[, .SD, .SDcols = sdi_cases_colnames]]

formulas_ratio <- paste(paste("ratio", age, sep = "_"), " ~ rep_date_divi")

# change data type of the date, because otherwise predicting doesnt work
data$rep_date_divi <- as.numeric(data$rep_date_divi)

dt_models_ratio <- data.table(formulas = formulas_ratio)
# add a lm that is needed for the segmented/selgmented function
dt_models_ratio[, base_model := lapply(formulas, function(x) lm(data = data, x))]

# using selg_function from "help_functions/model_help_functions.R" to create a bic for each number of breakpoints
# error messages are no problem. They get prduced and catched by the segmented::selgmented function
dt_models_ratio[, model_bic := lapply(base_model, selg_function)
          # choosing the model with the sequential lowest BIC for the number of breakpoints
][, model_bic_seq := Map(seq_bic_model, base_model, model_bic)
  # making a confint matrix with the predicted y for the estimated breakpoints
][, confint := lapply(model_bic_seq, function(model) {
  conf_matrix <- confint(model)
  # bind the conf_matrix with a prediction for the estimated breakpoints
  new_matrix <- cbind(conf_matrix,
                      y = predict(model, newdata = data.frame(rep_date_divi = conf_matrix[, 1]),
                                  type = "response"))})
]

residual_plot <- function(model) {
  plot(model$residuals, predict(model))
}
# most models seem resonable (15-34 not, but that was expectable)
lapply(dt_models_ratio$model_bic_seq, residual_plot)
