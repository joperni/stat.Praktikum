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
