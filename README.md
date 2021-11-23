# stat.Praktikum
erstes Repo für statistisches Praktikum

# Structure of main data
  - One row for each ref_date of rki_data (all divi-type-data is NA if there is no matching divi_rep_date)
    - rep_date_divi is ref_date_rki
  - columns that ends with "_divi" are fom divi_data
  - all other columns are from rki data
  - The name of the column explains the demographic group
    - number_cases (Covid_Fallzahlen), number_case_deaths (Covid_Todeszahlen)
    - _NA, _W, _M Geschlecht
    - _NA, _A... Alter
    - _TRUE, _FALSE Ist Erkrankungsbeginn bekannt?
  - total_cases, total_death_cases contains the aggregated number of CovidFallzahlen/CovidTodeszahlen
  - An Example of aggregations over columns can be found in the code_examples folder
  - A function that transforms data to the seven_day_inzidewnz can be found in the help_functions folder
