# data --------------------------------------------------------------------
library(segmented)
library(data.table)
library(ggplot2)
source("help_functions/seven_day_inzidenz.R")
main_data <- readRDS("data/main_data")
setDT(main_data)
data <- copy(main_data["2020-10-01" > rep_date_divi & rep_date_divi >= "2020-07-01"])
cnames <- colnames(data)
age_groups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
data <- data[, x_cases := sum(.SD),
             .SDcols = cnames[grepl(age_groups[6], cnames) & grepl("number_cases", cnames)],
             by = rep_date_divi][!is.na(x_cases)]


### helpfunction
inference_glm_seg <- function(model) {
  slopes <- slope(model)
  conf_int <- confint(model)
  list(model = list(model), slopes = list(slopes), conf_int = list(conf_int),
       BIC = BIC(model))
}

fit_lm <- lm(data = data, log(seven_day_inz(x_cases) + .1 * 10^(-5)) ~ rep_date_divi)
fit_gamma <- glm(data = data, seven_day_inz(x_cases) ~ rep_date_divi, family = Gamma(link = "log"))


# lm with breakpoints -----------------------------------------------------

seg_lm <- segmented(fit_lm, psi = list(x = NA, z = .3), 
    control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7, it.max = 50, K = 5, display = TRUE))
plot_lm <- ggplot(data = data, aes(x = rep_date_divi, y =  log(seven_day_inz(x_cases) + .1 * 10^(-5)))) +
  geom_point() +
  geom_line(aes(y = seg_lm$fitted.values, color = "fitted"))


# gamma with breakpoints --------------------------------------------------

seg_gamma <- segmented(fit_gamma, psi = NA, 
                    control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7, it.max = 100, K = 6, display = TRUE))
seg_gamma_update <- segmented(fit_gamma, psi = seg_gamma$psi[, 2], control = seg.control(n.boot = 200, display = TRUE))

# probably is 6 better against overfitting
selg_gamma <- selgmented(fit_gamma, type = "bic", Kmax = 12)

plot_gamma <- ggplot(data = data, aes(x = rep_date_divi, y = seven_day_inz(x_cases))) +
  geom_point() +
  geom_line(aes(y = seg_gamma$fitted.values, color = "fitted")) +
  geom_line(aes(y = seg_gamma_update$fitted.values, color = "fitted_updated")) +
  geom_line(aes(y = selg_gamma$fitted.values, color = "fitted_selg"))

inference_models <- lapply(list(seg_gamma, seg_gamma_update, selg_gamma), inference_glm_seg)

