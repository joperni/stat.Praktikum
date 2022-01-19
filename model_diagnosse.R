source("nicer_implementation_models.R")

bsp_diagnose <- plot(glm(data = iris, Petal.Length ~ Petal.Width, family = Gamma(link = "log")))
#
# Residuals vs Predicted Values

#
# Normal Q-Q-Plot

#
# wurzel(Std. Pearson residual) vs Pred Values

#
# Std. Pearson resid. vs Leverage
