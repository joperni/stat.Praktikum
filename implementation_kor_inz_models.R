source("implementation_models.R")
source("Praktikum.R")


# data from "Praktikum.R"
dt_kor_inz <- copy(data_okt_dez[, .(rep_date = as.numeric(rep_date_divi), .SD),
                                .SDcols = colnames(data_okt_dez)[grepl("kor_inz", colnames(data_okt_dez))]][
                                  , .SD.kor_inz_A00_A14 := NULL])
# is dependent from the sequence of the columns in "Praktikum.R"
setcolorder(dt_kor_inz, c(1, 6, 2:4))
formulas_kor <- paste(colnames(dt_kor_inz)[-1], "+ 1 * exp(5) ~ rep_date")

dt_kor_models <- data.table(formulas = formulas_kor)

dt_kor_models[, seg_fixed_bp := lapply(1:5, function(x) {
  # first fit the base model
  segmented(glm(formulas_kor[1], dt_kor_inz, family = Gamma(link = "log")),
            # psi from the fitted models for the not corrected inzidenz
            psi = dt_models[x, model_bic_seq[[1]]$psi[, 2]],
            # (it.max = 0) to avoid new bps
            control = seg.control(it.max = 0))
  })]

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")


# # help data table for the plots -----------------------------------------

# At first: Making data table with breakpoints, for all used gamma models

# # how much should each label be repeated
# rep_times_age_kor <- c(0, cumsum(vapply(dt_kor_models[, seg_fixed_bp], function(x) length(x$psi), numeric(1))))
# # char_vec with age groups
# age_groups_kor <- c("Gesamt","15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre")
# dt_breakpoints_kor <- lapply(dt_kor_models[, seg_fixed_bp], function(x) {
#   unname(x$psi)
#   })
# # fastest way in dt
# # add an age column
# for (i in seq_along(age_groups_kor)) {
#   set(dt_breakpoints_kor,
#       seq(rep_times_age_kor[i] + 1, rep_times_age_kor[1 + i]), "age_variable", age_groups_kor[i])
# }

# dt_breakpoints_kor[, lapply(dt_kor_models[, seg_fixed_bp]), function(model)  {
#   y = predict(model, newdata = dt_breakpoints_kor[, .(rep_date_divi = bp)],
#                                  type = "response")})]
# 
# setnames(dt_breakpoints, c("time", "lowerCI", "upperCI", "sdi", "variable", "origin"))
# dt_breakpoints[, c("time", "lowerCI", "upperCI") := lapply(.SD, as.Date, format = "%d. %b %Y", origin = lubridate::origin),
#                .SDcols = c("time", "lowerCI", "upperCI")]



# Transforming data table for easier plotting for cases
#
# add fitted values to a data_frame
dt_kor_fitted_vals <- 
  dt_kor_models[, lapply(seg_fixed_bp, function(model) model$fitted.values)]
# add time column
dt_kor_fitted_vals[, time := as.Date(dt_kor_inz$rep_date, format = "%d. %b %Y", origin = lubridate::origin)]
setnames(dt_kor_fitted_vals, c("V1", "V2", "V3", "V4",
                                 "V5"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "ueber 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_kor <- melt(dt_kor_fitted_vals, id.vars = "time", value.name = "sdi")

# Age_group_plot ----------------------------------------------------------
# dt_breakpoints$variable[dt_breakpoints$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_kor$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_kor$variable)[5] = c("Über 79 Jahre")
kor_model_breakpoints <- ggplot(fitted_vals_melt_kor,
                            aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600), breaks = seq(0, 600, 50)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y")# +
  # geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  # geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))

ggsave("Plots/kor_model.png", kor_model_breakpoints, width = 22, height = 10, units = c("cm"))
