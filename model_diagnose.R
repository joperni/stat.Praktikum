source("nicer_implementation_models.R")
library(ggplot2)
library(checkmate)
library(ggpubr)

#
# making a function, to diagnose models like the plot.glm function for...
# input: segmented objects, fitted with "family = Gamma(link = "log")"
seg_diagnose <- function(model) {
  assert_class(model, "segmented")
  # TODO check for gamma distribution
  df <- data.frame(res = model$residuals, fitted = model$fitted.values,
                   res_standard = rstandard(model),
                   theo_quants = qgamma(ppoints(length(model$residuals)),
                                        # taking just the estimation of the shape, not the se
                                        shape = MASS::gamma.shape(model)[[1]]),
                   leverage = hatvalues(model))
  
  # residuals vs predicted values
  res_pred_plot <- ggplot(df, aes(fitted, res)) +
    geom_point() +
    xlab("gefittete Werte") +
    ylab("Residuen") +
    geom_hline(yintercept = 0, linetype = "dotted")
  
  # gamma qqplot
  # TODO this doesnt make that much sense. The distribution must fit *between* the breakpoints
  # would add breakpoints to this plot would be like shooting with canons on sparrows 
  qq_gamma_plot <- ggplot(df, aes(theo_quants, sort(res_standard))) +
    geom_point() +
    xlab("theoretische Gammaquantile") +
    ylab("standatisierte Residuen")
  
  # root(standatized residuals) vs fitted values
  scale_loc_plot <- ggplot(df, aes(fitted, sqrt(abs(res_standard)))) +
    geom_point() +
    xlab("gefittete Werte") +
    ylab("wurzel(|standatisierte Residuen|)")
  
  # leverage vs standatized residuals
  lev_res_plot <- ggplot(df, aes(leverage, res_standard)) +
    geom_point() +
    xlab("leverage") +
    ylab("standatisierte Residuen")
  # output
  ggarrange(plotlist = list(res_pred_plot, qq_gamma_plot, scale_loc_plot, lev_res_plot))
}
#
list_model_diagnose <- lapply(dt_models[, model_bic_seq], seg_diagnose)

