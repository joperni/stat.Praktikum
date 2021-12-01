library(segmented)
library(data.table)
library(ggplot2)
library(checkmate)
source("help_functions/seven_day_inzidenz.R")
source("help_functions/columns_aggr_age.R")
setDT(data)
#
#

# assumption: Gamma Distribution ------------------------------------------

# the hist shows: the data ist bimodal and linkssteil
hist(seven_day_inz(data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$total_cases))
#
# assumption: log-Gaussian ------------------------------------------------
# Data not really normaly distributed, but more than before
hist(log(seven_day_inz(data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$total_cases)))


## help function
inference_glm_seg <- function(modell) {
  slopes <- slope(modell) 
  conf_int <- confint(modell)
  list(modell = list(modell), slopes = list(slopes), conf_int = list(conf_int),
       BIC = BIC(modell))
}

age_groups_N <- c(83240000, 11477737, 18921292, 27600978, 18153339, 5936434)
names(age_groups_N) <- c("total_cases", colnames(data)[96:100])

# filter data
data <- copy(data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"])

# Gamma because smoothed (through 7-day Inzidenz) cases, arent "Zähldaten" &
# log_normalverteilung doesnt fit very well (see playground_breakpoints.R)
list_gamma_modells <- lapply(paste("seven_day_inz(", names(age_groups_N), ",", age_groups_N ,") ~ rep_date_divi"), 
                             function(formula) fit_gamma <- glm(data = data, formula, family = Gamma(link = "log")))

seg_function <- function(modell) {
  segmented(modell, psi = NA, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7,
                                                       it.max = 100, K = 12, display = FALSE))}
list_seg_gamma <- lapply(list_gamma_modells, seg_function)

# probably is 6 better against overfitting
selg_function <- function(modell) selgmented(modell, type = "bic", Kmax = 12, msg = TRUE)

list_selg_gamma <- lapply(list_gamma_modells, selg_function)

# with list "list_selg_gamma[[1]]$selection.psi" sequentiell selection is possible
seq_bic_modell <- function(modell, modell_selg) {
  # Are the BICs of the breakpoints higher than before
  logical_seq <- modell_selg$selection.psi[-12] > modell_selg$selection.psi[-1]
  # picks first break point that is the last one which is smaller than befor
  n_breakpoints <- which(!logical_seq)[1] - 1
  segmented(modell, npsi = n_breakpoints, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7,
                                                                 it.max = 100, K = 12, display = TRUE))
}

list_bic_seq <- Map(seq_bic_modell, list_gamma_modells, list_selg_gamma)


function_plot <- function(modell_1, modell_2, modell_3, title) {
  plot_gamma <- ggplot(data, aes(modell_1$y, x = rep_date_divi)) +
    geom_point() +
    geom_line(aes(y = modell_1$fitted.values, color = "fitted")) +
    geom_line(aes(y = modell_2$fitted.values, color = "fitted_bic")) +
    geom_line(aes(y = modell_3$fitted.values, color = "fitted_seq_bic")) +
    labs(title = title)
}

plots <- Map(function_plot, list_seg_gamma, list_selg_gamma, list_bic_seq, names(age_groups_N))
#
#

# to compare we use a lm  -------------------------------------------------

fit_lm <- lm(data = data, log(seven_day_inz(x_cases) + .1 * 10^(-5)) ~ rep_date_divi)

# no set breakpoints
seg_lm <- segmented(fit_lm, psi = list(x = NA, z = .3), 
                    control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7, it.max = 50, K = 12, display = TRUE))

# probably is 6 better against overfitting
selg_lm <- selgmented(fit_lm, type = "bic", Kmax = 12)


ggplot(data = data, aes(x = rep_date_divi, y =  log(seven_day_inz(x_cases) + .1 * 10^(-5)))) +
  geom_point() +
  geom_line(aes(y = seg_lm$fitted.values, color = "fitted")) +
  geom_line(aes(y = selg_lm$fitted.values, color = "fitted_selg"))

modells <- rbindlist(lapply(list(seg_gamma, selg_gamma, seg_lm, selg_lm), inference_glm_seg))
list(modells = modells, plot_gamma = plot_gamma, plot_lm = plot_lm)
