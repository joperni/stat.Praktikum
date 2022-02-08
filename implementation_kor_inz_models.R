source("implementation_models.R")
source("Praktikum.R")

dt_kor_inz <- copy(data_okt_dez[, .(date = as.numeric(rep_date_divi), .SD),
                                .SDcols = colnames(data_okt_dez)[grepl("kor_inz", colnames(data_okt_dez))]][
                                  , .SD.kor_inz_A00_A14 := NULL])
formulas_kor <- paste(colnames(dt_kor_inz)[-1], "+ 1 * exp(5) ~ date")

dt_kor_models <- data.table(formulas = formulas_kor)

dt_kor_models[, base_model := lapply(formulas, glm, dt_kor_inz, family = Gamma(link = "log"))]

dt_kor_models[, seg_fixed_bp := lapply(1:5, function(x) {
  segmented(dt_kor_models[x, base_model][[1]], fixed.psi = list(dt_models[x, model_bic_seq[[1]]$psi[, 2]]))
  })]
              

segmented(dt_kor_models[1, base_model][[1]], fixed.psi = dt_models[1, model_bic_seq[[1]]$psi[, 2]])

seg_debug <- segmented(glm(formulas_kor[1], dt_kor_inz, family = Gamma(link = "log")),
                       fixed.psi = dt_models[2, model_bic_seq[[1]]$psi[, 2]], control = seg.control(it.max = 30))
