source("implementation_models.R")
source("Praktikum.R")
set.seed(23354634)

# data from "Praktikum.R"
dt_kor_inz <- copy(data_okt_dez[, .(rep_date = as.numeric(rep_date_divi), .SD),
                                .SDcols = colnames(data_okt_dez)[grepl("kor_inz", colnames(data_okt_dez))]][
                                  , .SD.kor_inz_A00_A14 := NULL])

# using usually y + exp(-3) in the formula
formula <- ".SD.kor_inz_A35_A59 ~ rep_date"

glm_obj <- glm(formula, dt_kor_inz, family = Gamma(link = "log"))

# dt_models[3, model_bic_seq[[1]]$psi[, 2]] ist 7

seg_obj <- segmented(glm_obj, npsi = 7, control = seg.control(fix.npsi = TRUE))

df_for_plot <- data.frame(time = dt_kor_inz$rep_date, y = glm_obj$y, fitted = seg_obj$fitted.values)


ggplot(df_for_plot, aes(x = time, y = y)) +
  geom_line() +
  geom_line(aes(y = fitted))
