library(data.table)
library(checkmate)


# rki_main ----------------------------------------------------------------
rki_main <- read.csv("data/RKI_COVID19.csv")
setDT(rki_main)
colnames(rki_main)[1] <- "IdBundesland"
rki_main_names <- c("IdBundesland", "Bundesland", "Landkreis", "Altersgruppe",
                    "Geschlecht", "AnzahlFall", "AnzahlTodesfall", "ObjectId",
                    "Meldedatum", "Datenstand", "IdLandkreis", "NeuerFall","NeuerTodesfall",
                    "Refdatum", "NeuGenesen", "AnzahlGenesen", "IstErkrankungsbeginn", "Altersgruppe2")
# Set sample for speedpurposes
set.seed(23456)
# sample nothing, but it stays so it can easily be changed
rki_sample <- rki_main[sample(.N, .N * 1)]

# unique age-classes 0-4, 5-14, 15-34, 35-59, 60-79, 80+, unbekannt
# Altersgruppe2: Always "Nicht ?bermittelt"
unique(rki_sample[, c("Altersgruppe", "Altersgruppe2")])

# vector for changing bundeslaender-labels later
vec_state <- unique(rki_sample[, Bundesland])
names(vec_state) <- c("HE", "HA", "NI", "NW", "BW", "BE", "BY", "SN", "SH",
                      "HB", "RP", "TH", "MV", "SL", "ST", "BB")


cleaning_rki <- function(DT = rki_sample) {
  # makes sure the function doesnt change the input object
  data <- copy(DT)
  # function is just for rki_main_data
  assertDataFrame(data)
  assertSubset(colnames(data), rki_main_names)
  data[data == "unbekannt"] <- NA
  # change data types
  # to date
  data[, `:=`(Refdatum = as.Date(Refdatum), Meldedatum = as.Date(Meldedatum),
              # to factor
              Altersgruppe = as.factor(Altersgruppe), Geschlecht = as.factor(Geschlecht),
              # to boolean
              IstErkrankungsbeginn = IstErkrankungsbeginn == 0)]
  # "Datenstand" not interesting ( we just want data before 2021 anyway)
  # "Altersgruppe2" not useful at all
  # "IdBundesland" & "Landkreis" equivalent to "Bundesland" & "IdLandkreis"
  data[, `:=`(Datenstand = NULL, Altersgruppe2 = NULL, IdBundesland = NULL,
              Landkreis = NULL)]
  # Filterng Data
  data_autumn <- data[Refdatum < "2021-01-31"]
  # cleaner colnames
  colnames(data_autumn) <- c("state", "age_grouped", "gender", "number_cases",
                             "number_case_death", "object_id", "rep_date", "id_substate", "new_case",
                             "new_case_death", "ref_date", "new_recover", "number_recover", "is_onset_illnes")
  # reordering cols
  setcolorder(data_autumn, c("object_id", "state", "id_substate", "age_grouped",
                             "gender", "number_cases", "number_case_death", "number_recover", "new_case",
                             "new_case_death", "new_recover", "is_onset_illnes", "rep_date", "ref_date"))
  setorder(data_autumn, ref_date)
  
  # iterating through states -> replacing state-names by handier ones
  # (https://www.datenportal.bmbf.de/portal/de/G122.html)
  lapply(1:18, function(i) data_autumn[state == vec_state[i], state := names(vec_state)[i]])
  
  # Change Type of the new_columns
  # Their format is complicated for the daily reports
  # -> Changing new_case_death to TRUE/FALSE and deleting
  # - new_case (always zero)
  # - new_recover (opposite of new_case_death)
  data_autumn[, `:=`(new_case = NULL, new_recover = NULL,
                     new_case_death = new_case_death == 0)][]
}
clean_rki <- cleaning_rki()

clean_casted_rki <- dcast(clean_rki,
                          ref_date ~ gender + age_grouped + is_onset_illnes,
                          value.var = c("number_cases", "number_case_death"),
                          fun.aggregate = sum)
cnames_rki <- colnames(clean_casted_rki)
clean_casted_rki[, total_cases := sum(.SD),
                 .SDcols = cnames_rki[grepl("number_cases", cnames_rki)], by = ref_date]
clean_casted_rki[, total_death_cases := sum(.SD),
                 .SDcols = cnames_rki[grepl("number_case_death", cnames_rki)], by = ref_date]


# divi_main ---------------------------------------------------------------

divi_main <- read.csv("data/divi_covid.csv")
setDT(divi_main)
divi_main[, `:=`(date = as.Date(date), bundesland = NULL)]
colnames(divi_main) <- c("rep_date", "id_substate", "number_locations", "number_rep_areas", 
                         "cases_covid", "cases_covid_invasive", "beds_free", "beds_occupied", 
                         "beds_occupied_just_adults", "beds_free_just_adults")
clean_divi <- divi_main[rep_date < "2021-01-31"]
cnames_divi <- colnames(clean_divi)
prakt_clean_data <- merge(clean_divi, clean_rki, by = "rep_date", all = TRUE)
setcolorder(prakt_clean_data, c("rep_date", "object_id", "state", "id_substate", "age_grouped",
                                "gender", "number_cases", "number_case_death", "number_recover",
                                "new_case_death", "is_onset_illnes", "ref_date", "number_locations",
                                "number_rep_areas", "cases_covid", "cases_covid_invasive", "beds_free",
                                "beds_occupied", "beds_free_just_adults", "beds_occupied_just_adults"))


clean_divi_aggr <- clean_divi[, lapply(.SD, sum), .SDcols = cnames_divi[-c(1, 2)], by = rep_date]
prakt_aggr_data <- merge(clean_divi_aggr, clean_casted_rki, by.x = "rep_date", by.y = "ref_date", all = TRUE)

saveRDS(prakt_clean_data, "data/prakt_clean_data")
saveRDS(prakt_aggr_data, "data/main_data")




# continue_with_cleaning_main ---------------------------------------------

main_data <- readRDS("data/main_data")
# better colnames
colnames(main_data)[1:9] <- paste(colnames(main_data)[1:9], "divi", sep = "_")
seven_day_inz <- function(x, inhabitants = 83240000) {
  assert_numeric(x)
  inz <- numeric(0)
  x_looping <- c(NA, NA, NA, x, NA, NA, NA)
  vapply(seq_along(x) + 3, function(current_day) {
    seven_day_seq <- seq(current_day - 3, current_day + 3)
    sum(x_looping[seven_day_seq], na.rm = TRUE) * 100000/inhabitants
  }, numeric(1))
}






