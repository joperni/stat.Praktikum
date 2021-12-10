library(segmented)
library(data.table)
library(ggplot2)
library(checkmate)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(cowplot)
library(grid)
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
source("help_functions/modell_help_functions.R")
setDT(main_data)
# filter data
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])

set.seed(1352674267)

sdi_cases_colnames <- c("seven_day_inz", "seven_day_inz_A00_A14", "seven_day_inz_A15_A34",
                        "seven_day_inz_A35_A59", "seven_day_inz_A60_A79", "seven_day_inz_A80")
sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34",
                         "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")
# using formulas for modelfitting
formulas <- c(paste(sdi_cases_colnames, " ~ rep_date_divi"),
              paste("log(", sdi_cases_colnames, " + .1 * 10^-8) ~ rep_date_divi"),
              paste(sdi_deaths_colnames, " + 0.0001 ~ rep_date_divi"),
              paste("log(", sdi_deaths_colnames, " + .1 * 10^-8) ~ rep_date_divi"),
              "cases_covid_divi ~ rep_date_divi", "log(seven_day_hosp_inz + .1 * 10^-8) ~ rep_date_divi")

dt_models <- data.table(formula = formulas[!grepl("log", formulas)])
# dt_models[grepl("log", formula), base_model := lapply(formula,
#                                                  function(x) glm(data = data, x))]
dt_models[!grepl("log", formula), base_model := lapply(formula,
                                                  function(x) glm(data = data, x,  family = Gamma(link = "log")))]
dt_models[, model_bic := lapply(base_model, selg_function)][
  , model_bic_seq := Map(seq_bic_modell, base_model, model_bic)][
    , plot_model := Map(function_plot, base_model, model_bic, model_bic_seq, formulas[!grepl("log", formulas)])]



# # help data table for the plots -----------------------------------------

# At first: Making data table with breakpoints, for all used gamma models
dt_breakpoints <- 
  dt_models[!grepl("log", formula), .(time = as.Date(model_bic_seq[[1]]$psi[, 2], origin = "1970-01-01"),
                                      model = model_bic_seq), by = formula]
dt_breakpoints$sdi <- c(19, 43, 142, 151, 149, 0, 32, 192, 224, 179,
                        22, 70, 117, 152, 172, 177, 169, 230, 90, 100,
                        10, 100,
                        # now deaths
                        9, 11, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.3, 0.2, 3, 16.5, 20, 28,
                        # for hosp
                        700, 2750, 3500, 4100)
dt_breakpoints[, `:=`(formula = NULL, model = NULL)]
dt_breakpoints$variable <- c(rep("overall", 5), rep("", 1), rep("15-34 Jahre", 4), rep("35-59 Jahre", 8),
                             rep("60-79 Jahre", 2), rep("ueber 80 Jahre", 2),
                             # deaths
                             rep("overall", 2), rep("", 5), rep("15-34 Jahre", 2), rep("35-59 Jahre", 3),
                             rep("60-79 Jahre", 1), rep("ueber 80 Jahre", 3), rep("overall", 4))

# Transforming data table for easier plotting for cases
#
# add fitted values to a data_frame
dt_cases_fitted_vals <- 
  dt_models[c(1, 3:6), lapply(model_bic_seq, function(model) model$fitted.values)]
# add time column
dt_cases_fitted_vals[, time := data$rep_date_divi]
setnames(dt_cases_fitted_vals, c("V1", "V2", "V3", "V4",
                                 "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_cases <- melt(dt_cases_fitted_vals, id.vars = "time", value.name = "sdi")

# for deaths now
dt_deaths_fitted_vals <- dt_models[c(7, 9:12), lapply(model_bic_seq, function(model) model$fitted.values)]
dt_deaths_fitted_vals[, time := data$rep_date_divi]
setnames(dt_deaths_fitted_vals, c("V1", "V2", "V3", "V4",
                                  "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
fitted_vals_melt_deaths <- melt(dt_deaths_fitted_vals, id.vars = "time", value.name = "sdi")
# for hospitalisierung
dt_hosp_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[13, model_bic_seq[[1]]$fitted.values],
                               timeseries = dt_models[13, model_bic_seq[[1]]$y])

# with timeseries ---------------------------------------------------------

# for cases
dt_cases_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[1, model_bic_seq[[1]]$fitted.values],
                                timeseries = dt_models[1, model_bic_seq[[1]]$y])
setnames(dt_cases_y_fitted, c("fitted", "timeseries"), c("geschaetzt", "gemeldet"))
dt_cases_y_fitted_melt <- melt(dt_cases_y_fitted, id.vars = "time", value.name = "values")

# for deaths
dt_deaths_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[7, model_bic_seq[[1]]$fitted.values],
                                 timeseries = dt_models[7, model_bic_seq[[1]]$y])
setnames(dt_deaths_y_fitted, c("fitted", "timeseries"), c("geschaetztes", "gemeldetes"))
dt_deaths_y_fitted_melt <- melt(dt_deaths_y_fitted, id.vars = "time", value.name = "values")

# for hosp
setnames(dt_hosp_y_fitted, c("fitted", "timeseries"), c("geschaetztes", "gemeldetes"))
dt_hosp_y_fitted_melt <- melt(dt_hosp_y_fitted, id.vars = "time", value.name = "values")