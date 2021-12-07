library(segmented)
library(data.table)
library(ggplot2)
library(checkmate)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
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
              "seven_day_hosp_inz ~ rep_date_divi", "log(seven_day_hosp_inz + .1 * 10^-8) ~ rep_date_divi")

dt_models <- data.table(formula = formulas)
dt_models[grepl("log", formula), base_model := lapply(formula,
                                                 function(x) glm(data = data, x))]
dt_models[!grepl("log", formula), base_model := lapply(formula, 
                                                  function(x) glm(data = data, x,  family = Gamma(link = "log")))]
dt_models[, model_bic := lapply(base_model, selg_function)][
  , model_bic_seq := Map(seq_bic_modell, base_model, model_bic)][
    , plot_model := Map(function_plot, base_model, model_bic, model_bic_seq, formulas)]

# dt_models[, `:=`(aic_base_model = lapply(base_model, AIC), aic_model_bic = lapply(model_bic, AIC),
#                  aic_model_bic_seq = lapply(model_bic_seq, AIC), bic_base_model = lapply(base_model, BIC),
#                  bic_model_bic = lapply(model_bic, BIC), bic_model_bic_seq = lapply(model_bic_seq, BIC))]

# help data table for the plot. It contains the fitted values for each gamma model
dt_cases_fitted_vals <- 
  dt_models[1:6, lapply(model_bic_seq, function(model) model$fitted.values)]
colnames(dt_cases_fitted_vals) <- c(sdi_cases_colnames)
dt_cases_fitted_vals[, time := data$rep_date_divi]
fitted_vals_melt_cases <- melt(dt_cases_fitted_vals, id.vars = "time", value.name = "sdi")

cases_breakpoints <- fitted_vals_melt_cases %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400)) +
  scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))

# for deaths now
dt_deaths_fitted_vals <- dt_models[13:18, lapply(model_bic_seq, function(model) model$fitted.values)]
colnames(dt_deaths_fitted_vals) <- c(sdi_deaths_colnames)
dt_deaths_fitted_vals[, time := data$rep_date_divi]
fitted_vals_melt_deaths <- melt(dt_deaths_fitted_vals, id.vars = "time", value.name = "sdi")

deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tage-Inzidenz Tode") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))

# for hosp now
dt_hosp_fitted_vals <- data.table(time = data$rep_date_divi, fitted = dt_models[25, model_bic_seq[[1]]$fitted.values])

hosp_breakpoints <- dt_hosp_fitted_vals %>% 
  ggplot(aes(x = time, y = fitted)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50)) +
  scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
ggsave("Plots/breakpoints_hosp.png", plot = hosp_breakpoints, width = 20, height = 10, units = c("cm"))


