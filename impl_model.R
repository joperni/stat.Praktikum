library(segmented)
library(data.table)
library(ggplot2)
library(checkmate)
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
source("help_functions/modell_help_functions.R")
setDT(main_data)
#
#
set.seed(1352674267)
# assumption: Gamma Distribution for Cases ------------------------------------------


hist(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_inz)



sdi_cases_colnames <- c("seven_day_inz", "seven_day_inz_A00_A14", "seven_day_inz_A15_A34",
                        "seven_day_inz_A35_A59", "seven_day_inz_A60_A79", "seven_day_inz_A80")

# filter data
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])

# Gamma because smoothed (through 7-day Inzidenz) cases, arent "Zähldaten" &
# log_normalverteilung doesnt fit very well (see playground_breakpoints.R)
# making a list of models for all age groups
list_gamma_sdi_modells <- lapply(paste(sdi_cases_colnames, " ~ rep_date_divi"), 
                             function(formula) fit_gamma <- glm(data = data, formula, family = Gamma(link = "log")))

# fit the segmented model on all models with no specified breakpoints
list_seg_sdi_gamma <- lapply(list_gamma_modells, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_sdi_gamma <- lapply(list_gamma_modells, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_seq_sdi <- Map(seq_bic_modell, list_gamma_modells, list_selg_gamma)

# plot for all models
plots_sdi <- Map(function_plot, list_seg_gamma, list_selg_gamma, list_bic_seq, sdi_cases_colnames)
#
#

# to compare we use a log lm  -------------------------------------------------

# hist(log(seven_day_inz(data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$total_cases)))
# 
# fit_lm <- lm(data = data, log(seven_day_inz(x_cases) + .1 * 10^(-5)) ~ rep_date_divi)
# 
# # no set breakpoints
# seg_lm <- segmented(fit_lm, psi = list(x = NA, z = .3), 
#                     control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7, it.max = 50, K = 12, display = TRUE))
# 
# # probably is 6 better against overfitting
# selg_lm <- selgmented(fit_lm, type = "bic", Kmax = 12)
# 
# 
# ggplot(data = data, aes(x = rep_date_divi, y =  log(seven_day_inz(x_cases) + .1 * 10^(-5)))) +
#   geom_point() +
#   geom_line(aes(y = seg_lm$fitted.values, color = "fitted")) +
#   geom_line(aes(y = selg_lm$fitted.values, color = "fitted_selg"))
# 
# modells <- rbindlist(lapply(list(seg_gamma, selg_gamma, seg_lm, selg_lm), inference_glm_seg))
# list(modells = modells, plot_gamma = plot_gamma, plot_lm = plot_lm)


# Gamma assumption: Death Cases -------------------------------------------------------------

hist(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A80)

sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34",
                        "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")

# filter data
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])

# Gamma because smoothed (through 7-day Inzidenz) cases, arent "Zähldaten" &
# log_normalverteilung doesnt fit very well (see playground_breakpoints.R)
# making a list of models for all age groups
list_gamma_death_modells <- lapply(paste(sdi_deaths_colnames, " + 0.0001 ~ rep_date_divi"), 
                             function(formula) fit_gamma <- glm(data = data, formula, family = Gamma(link = "log")))

# fit the segmented model on all models with no specified breakpoints
list_seg_death_gamma <- lapply(list_gamma_death_modells, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_death_gamma <- lapply(list_gamma_death_modells, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_seq_death <- Map(seq_bic_modell, list_gamma_death_modells, list_selg_gamma)

# plot for all models
plots_death <- Map(function_plot, list_seg_death_gamma, list_selg_death_gamma, list_bic_seq_death, sdi_deaths_colnames)

# Gamma assumption: Death Cases -------------------------------------------------------------

hist(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A80)

sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34",
                         "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")

# filter data
data <- copy(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])

# Gamma because smoothed (through 7-day Inzidenz) cases, arent "Zähldaten" &
# log_normalverteilung doesnt fit very well (see playground_breakpoints.R)
# making a list of models for all age groups
list_gamma_death_modells <- lapply(paste(sdi_deaths_colnames, " + 0.0001 ~ rep_date_divi"), 
                                   function(formula) fit_gamma <- glm(data = data, formula, family = Gamma(link = "log")))

# fit the segmented model on all models with no specified breakpoints
list_seg_death_gamma <- lapply(list_gamma_death_modells, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_death_gamma <- lapply(list_gamma_death_modells, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_seq_death <- Map(seq_bic_modell, list_gamma_death_modells, list_selg_gamma)

# plot for all models
plots_death <- Map(function_plot, list_seg_death_gamma, list_selg_death_gamma, list_bic_seq_death, sdi_deaths_colnames)


