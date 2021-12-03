## help function
inference_glm_seg <- function(modell) {
  slopes <- slope(modell) 
  conf_int <- confint(modell)
  list(modell = list(modell), slopes = list(slopes), conf_int = list(conf_int),
       BIC = BIC(modell))
}

seg_function <- function(modell) {
  segmented(modell, psi = NA, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-7,
                                                    it.max = 200, K = 9, display = FALSE))}

# probably is 6 better against overfitting
selg_function <- function(modell) selgmented(modell, type = "bic", Kmax = 9, msg = TRUE)

# with list "list_selg_gamma[[1]]$selection.psi" sequentiell selection is possible
seq_bic_modell <- function(modell, modell_selg) {
  # Are the BICs of the breakpoints higher than before?
  logical_seq <- modell_selg$selection.psi[-9] > modell_selg$selection.psi[-1]
  # roving NAs
  logical_seq[is.na(logical_seq)] <- FALSE
  # picks first break point that is the last one which is smaller than before
  n_breakpoints <- ifelse(all(logical_seq), 9, which(!logical_seq)[1])
  segmented(modell, npsi = n_breakpoints, control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-9,
                                                                it.max = 200, K = 9, display = TRUE))
}

function_plot <- function(modell_1, modell_2, modell_3, title) {
  plot_gamma <- ggplot(data, aes(modell_1$y, x = rep_date_divi)) +
    geom_point() +
    geom_line(aes(y = modell_1$fitted.values, color = "fitted")) +
    geom_line(aes(y = modell_2$fitted.values, color = "fitted_bic")) +
    geom_line(aes(y = modell_3$fitted.values, color = "fitted_seq_bic")) +
    labs(title = title)
}