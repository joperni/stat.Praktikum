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
  plot(predict(model), model$residuals)
}
# most models seem resonable (15-34 not, but that was expectable)
lapply(dt_models_ratio$model_bic_seq, residual_plot)


# # help data table for the plots -----------------------------------------

# At first: Making data table with breakpoints, for all used gamma models

# how much should each label be repeated
rep_times_age_ratio <- c(0, cumsum(vapply(dt_models_ratio[, confint], nrow, numeric(1))))
# char_vec with age groups
age_groups_ratio <- c("Gesamt","15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre")
dt_bp_ratio <- as.data.table(Reduce(rbind, dt_models_ratio[, confint]))
# fastest way in dt
# add an age column
for (i in seq_along(age_groups_ratio)) {
  set(dt_bp_ratio,
      seq(rep_times_age_ratio[i] + 1, rep_times_age_ratio[1 + i]), "age_variable", age_groups_ratio[i])
}

setnames(dt_bp_ratio, c("time", "lowerCI", "upperCI", "sdi", "variable"))
dt_bp_ratio[, c("time", "lowerCI", "upperCI") := lapply(.SD, as.Date, format = "%d. %b %Y",
                                                                 origin = lubridate::origin),
                     .SDcols = c("time", "lowerCI", "upperCI")]



# Transforming data table for easier plotting for cases
#
# add fitted values to a data_frame
dt_ratio_fitted_vals <- 
  dt_models_ratio[, lapply(model_bic_seq , function(model) model$fitted.values)]
# add time column
dt_ratio_fitted_vals[, time := as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin)]
setnames(dt_ratio_fitted_vals, c("V1", "V2", "V3", "V4",
                                 "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_ratio <- melt(dt_ratio_fitted_vals, id.vars = "time", value.name = "sdi")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")

# Age_group_plot ratio ----------------------------------------------------------
dt_bp_ratio$variable[dt_bp_ratio$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_ratio$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_ratio$variable)[5] = c("Über 79 Jahre")
ratio_breakpoints <- ggplot(fitted_vals_melt_ratio,
                            aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.25), seq(0, 0.25, 0.05)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_bp_ratio[, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_bp_ratio, aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi)) +
  labs(x = "", y = "Geschätzer Anteil der Verstorbenen an den Infizierten")


# just Timeseries and overall-group ---------------------------------------

# for cases
dt_ratio_y_fitted <- data.table(time = as.Date(data$rep_date_divi, format = "%d. %b %Y", origin = lubridate::origin),
                                fitted = dt_models_ratio[1, model_bic_seq[[1]]$fitted.values],
                                timeseries = data[, ratio_overall])
setnames(dt_ratio_y_fitted, c("fitted", "timeseries"), c("geschaetzt", "gemeldet"))
dt_ratio_y_fitted_melt <- melt(dt_ratio_y_fitted, id.vars = "time", value.name = "values")

farben4 <- c("geschätzt" = "darkorange", "gemeldet" = "#000000")

levels(dt_ratio_y_fitted_melt$variable)[1] = c("geschätzt")
ratio_timeseries <- dt_ratio_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "Geschätzer Anteil der Verstorbenen an den Infizierten") +
  scale_color_manual(values = farben4, name = "", labels = names(farben4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 0.05)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 19, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_bp_ratio[variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4[[1]]) +
  geom_segment(data = dt_bp_ratio[variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4[[1]])
