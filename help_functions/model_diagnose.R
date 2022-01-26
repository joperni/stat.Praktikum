library(ggplot2)
library(checkmate)
library(ggpubr)

# source: https://stackoverflow.com/questions/62345433/how-to-center-axes-in-ggplot2
symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}

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
                   leverage = hatvalues(model),
                   cooks_d <- cooks.distance(model))
  
  # residuals vs predicted values
  res_pred_plot <- ggplot(df, aes(fitted, res)) +
    geom_point() +
    xlab("gefittete Werte") +
    ylab("Residuen") +
    scale_y_continuous(limits = symmetric_limits) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", size = 1),
          panel.grid = element_line(colour = "gray57", size = 0.2),
          axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
          axis.text   = element_text(colour = "black")) +
    theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$fitted)))
  
  # # gamma qqplot
  # # TODO this doesnt make that much sense. The distribution must fit *between* the breakpoints
  # # would add breakpoints to this plot would be like shooting with canons on sparrows 
  # qq_gamma_plot <- ggplot(df, aes(theo_quants, sort(res_standard))) +
  #   geom_point() +
  #   xlab("theoretische Gammaquantile") +
  #   ylab("standatisierte Residuen")
  # 
  # # root(standatized residuals) vs fitted values
  # scale_loc_plot <- ggplot(df, aes(fitted, sqrt(abs(res_standard)))) +
  #   geom_point() +
  #   xlab("gefittete Werte") +
  #   ylab("wurzel(|standatisierte Residuen|)")
  
  # leverage vs standatized residuals
  lev_res_plot <- ggplot(df, aes(leverage, res_standard, color = cooks_d)) +
    geom_point() +
    xlab("Leverage") +
    ylab("standatisierte Residuen") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() +
    scale_color_continuous(name = "Cooks Distance") +
    scale_y_continuous(limits = symmetric_limits) +
    theme(panel.border = element_rect(colour = "black", size = 1),
          panel.grid = element_line(colour = "gray57", size = 0.2),
          axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
          axis.text   = element_text(colour = "black")) +
    theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$leverage)))
  # output
  alinged_diagn <- align_plots(res_pred_plot,lev_res_plot, align = "hv", axis = "tblr")
  ggarrange(plotlist = list(ggdraw(alinged_diagn[[1]]), ggdraw(alinged_diagn[[2]])))
}
