source("implementation_models.R")
# error messages are no problem. They get produced and catched by the segmented::selgmented function
source("Praktikum.R")


# data from "Praktikum.R"
dt_kor_inz <- copy(data_okt_dez[, .(rep_date = as.numeric(rep_date_divi), .SD),
                                .SDcols = colnames(data_okt_dez)[grepl("kor_inz", colnames(data_okt_dez))]][
                                  , .SD.kor_inz_A00_A14 := NULL])
# is dependent from the sequence of the columns in "Praktikum.R"
setcolorder(dt_kor_inz, c(1, 6, 2:4))
formulas_kor <- paste(colnames(dt_kor_inz)[-1], "+ 1 * exp(-5) ~ rep_date")

dt_kor_models <- data.table(formulas = formulas_kor[-2])

dt_kor_models[, seg_fixed_bp := lapply(seq(1, 5)[-2], function(x) {
  # first fit the base model
  segmented(glm(formulas_kor[x], dt_kor_inz, family = Gamma(link = "log")),
            # n breakpoints from the fitted models for the not corrected inzidenz
            npsi = length(dt_models[x, model_bic_seq[[1]]$psi[, 2]]),
            # (it.max = 0) to avoid new bps
            control = seg.control(fix.npsi = FALSE, n.boot = 0, tol = 1e-9,
                                  it.max = 800, display = FALSE))
  })
# making a confint matrix with the predicted y for the estimated breakpoints
][, confint := lapply(seg_fixed_bp, function(model) {
  conf_matrix <- confint(model)
  # bind the conf_matrix with a prediction for the estimated breakpoints
  new_matrix <- cbind(conf_matrix,
                      y = predict(model, newdata = data.frame(rep_date = conf_matrix[, 1]),
                                  type = "response"))})
]

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")


# # help data table for the plots -----------------------------------------

# At first: Making data table with breakpoints, for all used gamma models

# # how much should each label be repeated
rep_times_age_kor <- c(0, cumsum(vapply(dt_kor_models[, confint], nrow, numeric(1))))
# char_vec with age groups
age_groups_kor <- c("Gesamt", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre")
dt_bp_cor <- as.data.table(Reduce(rbind, dt_kor_models[, confint]))
# fastest way in dt
# add an age column
for (i in seq_along(age_groups_kor)) {
  set(dt_bp_cor,
     seq(rep_times_age_kor[i] + 1, rep_times_age_kor[1 + i]), "age_variable", age_groups_kor[i])
}

#
setnames(dt_bp_cor, c("time", "lowerCI", "upperCI", "sdi", "variable"))
dt_bp_cor[, c("time", "lowerCI", "upperCI") := lapply(.SD, as.Date, format = "%d. %b %Y",
                                                      origin = lubridate::origin),
          .SDcols = c("time", "lowerCI", "upperCI")]



# Transforming data table for easier plotting for cases
#
# add fitted values to a data_frame
dt_kor_fitted_vals <- 
  dt_kor_models[, lapply(seg_fixed_bp, function(model) model$fitted.values)]
# add time column
dt_kor_fitted_vals[, time := as.Date(dt_kor_inz$rep_date, format = "%d. %b %Y", origin = lubridate::origin)]
setnames(dt_kor_fitted_vals, c("V1", "V2", "V3", "V4"), c("overall", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_kor <- melt(dt_kor_fitted_vals, id.vars = "time", value.name = "sdi")
# adding confints manually
bp_manually <- data.frame(x = as.Date(c("2020-10-01", "2020-12-10")),
                          xend = as.Date(c("2020-12-23", "2020-12-23")),
                          y = c(296.23610, 397.76134), variable = "Gesamt")

# Age_group_plot ----------------------------------------------------------
dt_bp_cor$variable[dt_bp_cor$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_kor$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_kor$variable)[4] = c("Über 79 Jahre")
kor_model_breakpoints <- ggplot(fitted_vals_melt_kor[variable == "Gesamt"],
                                aes(x = time, y = sdi#, color = variable
                                )) +
  geom_line(color = "darkorange") +
  geom_line(data = dt_kor_inz[, .(time = as.Date(rep_date, format = "%d. %b %Y", origin = lubridate::origin),
                                  sdi = .SD.kor_inz_total#, variable = "Gesamt"
  )]) +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 450), breaks = seq(0, 450, 50)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  geom_point(data = dt_bp_cor[variable == "Gesamt", .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_bp_cor[variable == "Gesamt"], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y", limits = as.Date(c("2020-10-01", "2020-12-23"))) +
  geom_segment(data = bp_manually, aes(x = x, y = y, xend = xend, yend = y)) +
  geom_segment(data = bp_manually, aes(x = x, y = y, xend = xend, yend = y))
ggsave("Plots/kor_model.png", kor_model_breakpoints, width = 22, height = 10, units = c("cm"))

kor_model_age <- ggplot(fitted_vals_melt_kor,
                                aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 700), breaks = seq(0, 700, 100)) +
  scale_color_manual(values = farben3[-2], name = "Altersgruppe", labels = names(farben3)[-2]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y", limits = as.Date(c("2020-10-01", "2020-12-23"))) +
geom_point(data = dt_bp_cor[, .(sdi, time, variable)], shape = 18, size = 3) +
geom_segment(data = dt_bp_cor, aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi)) +
geom_segment(data = bp_manually, aes(x = x, y = y, xend = xend, yend = y))

ggsave("Plots/kor_model_age.png", kor_model_age, width = 22, height = 10, units = c("cm"))


# grid_plot ---------------------------------------------------------------

dt_breakpoints$variable[dt_breakpoints$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_cases$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_cases$variable)[5] = c("Über 79 Jahre")
cases_for_grid_kor <- fitted_vals_melt_cases %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme_bw() +
  # deleting 
  theme(axis.title = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  #theme(plot.margin = unit(c(-0.5, 1.5, 3, 0), "cm")) +
  annotate("text", label = "Inzidenz", x = as.Date(c("2020-10-01")),
           y = 370, size = 4.5, colour = "black", hjust = 0) +
  labs(y = "7-Tages-Inzidenz je 100.000 Einw.") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 2.2) +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text   = element_text(colour = "black")) +
  geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))



kor_for_grid <- ggplot(fitted_vals_melt_kor,
                       aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "geschätzte korrigierte 7-Tages-Inzidenz je 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 700), breaks = seq(0, 700, 150)) +
  scale_color_manual(values = farben3[-2], name = "Altersgruppe", labels = names(farben3)[-2]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text = element_text(colour = "black")) +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y", limits = as.Date(c("2020-10-01", "2020-12-23"))) +
  geom_point(data = dt_bp_cor[, .(sdi, time, variable)], shape = 18, size = 2.2) +
  geom_segment(data = dt_bp_cor, aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi)) +
  annotate("text", label = "korrigierte Inzidenz", x = as.Date(c("2020-10-01")),
           y = 630, size = 4.5, colour = "black", hjust = 0) +
  geom_segment(data = bp_manually, aes(x = x, y = y, xend = xend, yend = y))

# legend for the plot
legend <- get_legend(
  # create some space to the left of the legend
  cases_for_grid_kor + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# # creates y Axis for the hole plot
# y_axis <- textGrob("Inzidenz", 
#                      gp = gpar(fontface="bold", fontsize = 15), rot = 90)

p_grid <- plot_grid(cases_for_grid_kor +
                      # deleting legend manuall to add it later, for the grid plot
                      theme(legend.position = "none"),
                    NULL,
                    kor_for_grid,
                    # labels need to be adjusted
                    # one col for the three plots, to adjust them among each other
                    ncol = 1,
                    # adjusting the positions of the labels
                    label_x = .08, label_y = .97, hjust = 0, scale = 1, 
                    align = "v",
                    rel_heights = c(7, -0.22, 7))

# Arranges all together
grid_plot_kor <- grid.arrange(arrangeGrob(p_grid, right = legend))
ggsave("Plots/grid_plot_korr.png", plot = grid_plot_kor, width = 22, height = 10, units = c("cm"))
