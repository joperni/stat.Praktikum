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
list_gamma_sdi_models <- lapply(paste(sdi_cases_colnames, " ~ rep_date_divi"), 
                             function(formula) fit_gamma <- glm(data = data, formula, family = Gamma(link = "log")))

# fit the segmented model on all models with no specified breakpoints
list_seg_sdi_gamma <- lapply(list_gamma_sdi_models, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_sdi_gamma <- lapply(list_gamma_sdi_models, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_seq_sdi <- Map(seq_bic_modell, list_gamma_sdi_models, list_selg_sdi_gamma)

# plot for all models
plots_sdi <- Map(function_plot, list_seg_sdi_gamma, list_selg_sdi_gamma, list_bic_seq_sdi, sdi_cases_colnames)
#
#

# to compare we use a log lm  -------------------------------------------------

hist(log(seven_day_inz(data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$total_cases)))
# 
list_lm_sdi_models <- lapply(paste("log(", sdi_cases_colnames, " + .1*10^-8) ~ rep_date_divi"), 
                                function(formula) glm(data = data, formula))
# 
# fit the segmented model on all models with no specified breakpoints
list_seg_lm_sdi <- lapply(list_lm_sdi_models, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_lm_sdi <- lapply(list_lm_sdi_models, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_lm_sdi <- Map(seq_bic_modell, list_lm_sdi_models, list_selg_lm_sdi)

# plot for all models
plots_lm_sdi <- Map(function_plot, list_seg_lm_sdi, list_selg_lm_sdi, list_bic_lm_sdi, sdi_cases_colnames)



# Gamma assumption: Death Cases -------------------------------------------------------------

hist(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A80)

sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34",
                        "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")


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
list_bic_seq_death <- Map(seq_bic_modell, list_gamma_death_modells, list_selg_death_gamma)

# mostly TRUE
log(list_bic_seq_death[[1]]$fitted.values) == model.matrix(list_bic_seq_death[[1]]) %*% list_bic_seq_death[[1]]$coefficients

# plot for all models
plots_death <- Map(function_plot, list_seg_death_gamma, list_selg_death_gamma, list_bic_seq_death, sdi_deaths_colnames)


# to compare we use loglm for deathcases ----------------------------------

sdi_deaths_colnames <- c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34",
                         "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80")


list_lm_death_modells <- lapply(paste("log(", sdi_deaths_colnames, " + .1 * 10^-7) ~ rep_date_divi"), 
                                   function(formula) fit_lm <- glm(data = data, formula))

# fit the segmented model on all models with no specified breakpoints
list_seg_death_lm <- lapply(list_lm_death_modells, seg_function)

# fit the selgmented models on all models with the BIC criteria
list_selg_death_lm <- lapply(list_lm_death_modells, selg_function)

# fit the segmented models on all models with the sequentiell BIC criteria from selgmented
list_bic_seq_death_lm <- Map(seq_bic_modell, list_lm_death_modells, list_selg_death_lm)

# plot for all models
plots_death <- Map(function_plot, list_seg_death_lm, list_selg_death_lm, list_bic_seq_death_lm, sdi_deaths_colnames)

# Gamma assumption: Hospitalisierung -------------------------------------------------------------

hist(main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_hosp_inz)

gamma_hosp_model <- glm(data = data, seven_day_hosp_inz ~ rep_date_divi, family = Gamma(link = "log"))
seg_host_gamma <- seg_function(gamma_hosp_model)
selg_host_gamma <- selg_function(gamma_hosp_model)
bic_seq_hosp_gamma <- seq_bic_modell(gamma_hosp_model, selg_host_gamma)
plot_hosp_gamma <- function_plot(seg_host_gamma, selg_host_gamma, bic_seq_hosp_gamma, "Hospitalisierungsrate")


lm_hosp_model <- glm(data = data, log(seven_day_hosp_inz + 0.0001) ~ rep_date_divi)
seg_host_lm <- seg_function(lm_hosp_model)
selg_host_lm <- selg_function(lm_hosp_model)
bic_seq_hosp_lm <- seq_bic_modell(lm_hosp_model, selg_host_lm)
plot_hosp_lm <- function_plot(seg_host_lm, selg_host_lm, bic_seq_hosp_lm, "Hospitalisierungsrate")
