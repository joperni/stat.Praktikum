## help function
inference_glm_seg <- function(model) {
  slopes <- slope(model) 
  conf_int <- confint(model)
  list(model = list(model), slopes = list(slopes), conf_int = list(conf_int),
       BIC = BIC(model))
}

seg_function <- function(model) {
  segmented(model, psi = NA, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7,
                                                    it.max = 400, K = 9, display = FALSE))}

# probably is 6 better against overfitting
selg_function <- function(model) selgmented(model, type = "bic", Kmax = 9, msg = FALSE)

# with list "list_selg_gamma[[1]]$selection.psi" sequentiell selection is possible
seq_bic_model <- function(model, model_selg) {
  # Are the BICs of the breakpoints higher than before?
  logical_seq <- model_selg$selection.psi[-9] > model_selg$selection.psi[-1]
  # roving NAs
  logical_seq[is.na(logical_seq)] <- FALSE
  # picks first break point that is the last one which is smaller than before
  n_breakpoints <- ifelse(all(logical_seq), 9, which(!logical_seq)[1] - 1)
  segmented(model, npsi = n_breakpoints, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-9,
                                                                it.max = 400, K = 9, display = FALSE))
}

function_plot <- function(model_1, model_2, model_3, title) {
  plot_gamma <- ggplot(data, aes(model_1$y, x = rep_date_divi)) +
    geom_point() +
    geom_line(aes(y = model_1$fitted.values, color = "fitted")) +
    geom_line(aes(y = model_2$fitted.values, color = "fitted_bic")) +
    geom_line(aes(y = model_3$fitted.values, color = "fitted_seq_bic")) +
    labs(title = title)
}