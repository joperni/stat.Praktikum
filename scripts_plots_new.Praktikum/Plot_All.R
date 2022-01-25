source("scripts_plots_new.Praktikum/plots_data_overview.R")
source("Praktikum.R")
source("scripts_plots_new.Praktikum/models_plots.R")
aligned <- align_plots(ppseven_day_alter,
                       ppbetten,
                       ppdeaths,
                       plot_korr_7_inz_15_34,
                       plot_korr_7_inz_35_59,
                       plot_korr_7_inz_60_79,
                       plot_korr_7_inz_80,
                       plot_korr_7_inz_g,
                       plot_unterschätzung,
                       plot_unterschaetzung_7,
                       cases_timeseries,
                       hosp_timeseries,
                       deaths_timeseries,
                       cases_breakpoints,
                       deaths_breakpoints,
                       log_deaths_breakpoints,
                       beta_comparison,
                       align = "hv",
                       axis = "tblr")

ggsave("Plots/7-Tage-Inzidenz nach Alter.png", plot = ggdraw(aligned[[1]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/intensivbettenbelegung.png", plot = ggdraw(aligned[[2]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/Todesfälle nach Alter.png", plot = ggdraw(aligned[[3]]), width = 22, height = 10, units = c("cm"))

ggsave("Plots/korrigierte Inzidenz mit Lag 15-34.png", plot = ggdraw(aligned[[4]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/korrigierte Inzidenz mit Lag 35-59.png", plot = ggdraw(aligned[[5]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/korrigierte Inzidenz mit Lag 60-79.png", plot = ggdraw(aligned[[6]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/korrigierte Inzidenz mit Lag über 80.png", plot = ggdraw(aligned[[7]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/korrigierte Inzidenz mit Lag gesamt.png", plot = ggdraw(aligned[[8]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/Unterschätzung Inzidenz.png", plot = ggdraw(aligned[[9]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/Unterschätzung Inzidenz mit Lag.png", plot = ggdraw(aligned[[10]]), width = 22, height = 10, units = c("cm"))

ggsave("Plots/timeseries_model_cases.png", plot = ggdraw(aligned[[11]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_hosp.png", plot = ggdraw(aligned[[12]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_deaths.png", plot = ggdraw(aligned[[13]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_cases.png", plot = ggdraw(aligned[[14]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_deaths.png", plot = ggdraw(aligned[[15]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/log_breakpoints_deaths.png", plot = ggdraw(aligned[[16]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/beta_comparison.png", plot = ggdraw(aligned[[17]]), width = 22, height = 10, units = c("cm"))