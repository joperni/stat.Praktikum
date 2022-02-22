## help function
inference_glm_seg <- function(model) {
  slopes <- slope(model) 
  conf_int <- confint(model)
  list(model = list(model), slopes = list(slopes), conf_int = list(conf_int),
       BIC = BIC(model))
}

# seg_function <- function(model) {
#   segmented(model, psi = NA, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7,
#                                                     it.max = 400, K = 9, display = FALSE))}

# probably is 6 better against overfitting
selg_function <- function(model) selgmented(model, type = "bic", Kmax = 9, msg = FALSE)

# with list "list_selg_gamma[[1]]$selection.psi" sequential selection is possible
seq_bic_model <- function(model, model_selg) {
  # Are the BICs of the breakpoints higher than before?
  logical_seq <- model_selg$selection.psi[-9] > model_selg$selection.psi[-1]
  # A potential model is a NA if the model can not be fit with the data for the specific number of breakpoints
  # removing NAs
  logical_seq[is.na(logical_seq)] <- FALSE
  # picks first numer of breakpoints that is the last one which has a smaller BIC than the number before
  # If the BIC of all number of breakpoints are smaller than before, we choose kmax (9)
  n_breakpoints <- ifelse(all(logical_seq), 9, which(!logical_seq)[1] - 1)
  segmented(model, npsi = n_breakpoints, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-9,
                                                                it.max = 400, K = 9, display = FALSE))
}

# function_plot <- function(model_1, model_2, model_3, title) {
#   plot_gamma <- ggplot(data, aes(model_1$y, x = rep_date_divi)) +
#     geom_point() +
#     geom_line(aes(y = model_1$fitted.values, color = "fitted")) +
#     geom_line(aes(y = model_2$fitted.values, color = "fitted_bic")) +
#     geom_line(aes(y = model_3$fitted.values, color = "fitted_seq_bic")) +
#     labs(title = title)
# }

# This part is for the growth rates exp(beta) for the models
growth_rate <- function(model) {
  mod_coef <- model$coefficients
  exp(cumsum(mod_coef[seq(2, length(model$coefficients) / 2 + 1)]))
}