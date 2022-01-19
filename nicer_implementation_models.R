library(segmented)
library(ggplot2)
library(checkmate)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(cowplot)
library(grid)
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
source("help_functions/model_help_functions.R")
setDT(main_data)
# filter data
# data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-10-01"])

set.seed(1352674267)

sdi_cases_colnames <- c("seven_day_inz", "seven_day_inz_A15_A34",
                        "seven_day_inz_A35_A59", "seven_day_inz_A60_A79", "seven_day_inz_A80")
sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A15_A34",
                         "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")
# using formulas for modelfitting
formulas <- c(paste(sdi_cases_colnames, " ~ rep_date_divi"),
              paste(sdi_deaths_colnames, " + 1*exp(-5) ~ rep_date_divi"),
              "cases_covid_divi ~ rep_date_divi")

data$rep_date_divi <- as.numeric(data$rep_date_divi)

dt_models <- data.table(formulas = formulas)
dt_models[, base_model := lapply(formulas, function(x) glm(data = data, x, family = Gamma(link = "log")))]
dt_models[, model_bic := lapply(base_model, selg_function)][
  , model_bic_seq := Map(seq_bic_model, base_model, model_bic)][
    , confint_model := lapply(model_bic_seq, confint)][
      , confint := Map(function(model) {
        conf_matrix <- confint(model)
        new_matrix <- cbind(conf_matrix,
                            y = predict(model, newdata = data.frame(rep_date_divi = conf_matrix[, 1]),
                                        type = "response"))
      }, model_bic_seq)]



# # help data table for the plots -----------------------------------------

# At first: Making data table with breakpoints, for all used gamma models

# how much should each label be repeatet
rep_times_age <- c(0, cumsum(vapply(dt_models[, confint], nrow, numeric(1))))
# char_vec with age groups
age_groups <- c("Gesamt","15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre", "Gesamt",
                "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre", "Gesamt")
variables <- c("inz", "deaths", "beds")
rep_times_var <- c(0, sum(rep_times_age[6]), sum(rep_times_age[11]), rep_times_age[12])
dt_breakpoints <- as.data.table(Reduce(rbind, dt_models[, confint]))
# fastest way in dt
# add an age column
for (i in seq_along(age_groups)) {
  set(dt_breakpoints,
      seq(rep_times_age[i] + 1, rep_times_age[1 + i]), "age_variable", age_groups[i])
}
# add a column with indicates the variable we used for our model
for (i in seq_along(variables)) {
  set(dt_breakpoints,
      seq(rep_times_var[i] + 1, rep_times_var[1 + i]), "variable", variables[i])
}

setnames(dt_breakpoints, c("time", "lowerCI", "upperCI", "sdi", "variable", "origin"))
dt_breakpoints[, c("time", "lowerCI", "upperCI") := lapply(.SD, as.Date, format = "%d. %b %Y", origin = lubridate::origin),
               .SDcols = c("time", "lowerCI", "upperCI")]



# Transforming data table for easier plotting for cases
#
# add fitted values to a data_frame
dt_cases_fitted_vals <- 
  dt_models[grepl("seven_day_inz", formulas), lapply(model_bic_seq, function(model) model$fitted.values)]
# add time column
dt_cases_fitted_vals[, time := as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin)]
setnames(dt_cases_fitted_vals, c("V1", "V2", "V3", "V4",
                                 "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_cases <- melt(dt_cases_fitted_vals, id.vars = "time", value.name = "sdi")


# for deaths now
dt_deaths_fitted_vals <-
  dt_models[grepl("seven_day_death_inz", formulas), lapply(model_bic_seq, function(model) model$fitted.values)]
dt_deaths_fitted_vals[, time := as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin)]
setnames(dt_deaths_fitted_vals, c("V1", "V2", "V3", "V4",
                                  "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
fitted_vals_melt_deaths <- melt(dt_deaths_fitted_vals, id.vars = "time", value.name = "sdi")
# for hospitalisierung
dt_hosp_y_fitted <- data.table(time = as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin),
                               fitted = dt_models[grepl("cases_covid_divi", formulas),
                                                  model_bic_seq[[1]]$fitted.values],
                               timeseries = dt_models[grepl("cases_covid_divi", formulas), model_bic_seq[[1]]$y])

# with timeseries ---------------------------------------------------------

# for cases
dt_cases_y_fitted <- data.table(time = as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin),
                                fitted = dt_models[1, model_bic_seq[[1]]$fitted.values],
                                timeseries = dt_models[1, model_bic_seq[[1]]$y])
setnames(dt_cases_y_fitted, c("fitted", "timeseries"), c("geschaetzt", "gemeldet"))
dt_cases_y_fitted_melt <- melt(dt_cases_y_fitted, id.vars = "time", value.name = "values")

# for deaths
dt_deaths_y_fitted <- data.table(time = as.Date(data$rep_date_divi, format = "%d. %b %Y",
                                                origin = lubridate::origin),
                                 fitted = dt_models[6, model_bic_seq[[1]]$fitted.values],
                                 timeseries = dt_models[6, model_bic_seq[[1]]$y])
setnames(dt_deaths_y_fitted, c("fitted", "timeseries"), c("geschaetzt", "gemeldet"))
dt_deaths_y_fitted_melt <- melt(dt_deaths_y_fitted, id.vars = "time", value.name = "values")

# for hosp
setnames(dt_hosp_y_fitted, c("fitted", "timeseries"), c("geschaetzt", "gemeldet"))
dt_hosp_y_fitted_melt <- melt(dt_hosp_y_fitted, id.vars = "time", value.name = "values")


# growth_rates ------------------------------------------------------------

growth_rate <- function(model) {
  mod_coef <- model$coefficients
  exp(cumsum(mod_coef[seq(2, length(model$coefficients) / 2 + 1)]))
}
