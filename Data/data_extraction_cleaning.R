library(tidyverse)
library(janitor)

### HPSA DATA----------------------------------------------------

hpsas <- read_csv("all-primary-care-hpsas.csv") |>
  clean_names()

hpsas_df <- hpsas |>
  select(
    hpsa_name, metropolitan_indicator, designation_type,  
    hpsa_status, hpsa_score,hpsa_designation_population, 
    u_s_mexico_border_county_indicator,
    rural_status, hpsa_population_type, county_equivalent_name
  )

hpsas_df <- hpsas_df |>
  drop_na() |>
  rename("counties" = "county_equivalent_name")|>
  select(-counties)


hpsa <- hpsas_df |>
  mutate(
    hpsa_population_type = 
      fct_collapse(hpsa_population_type,
                   migrant_population = c("Low Income Population HPSA", 
                                           "Low Income Migrant Farmworker Population HPSA",
                                           "Low Income Migrant Seasonal Worker Population HPSA",
                                           "Low Income Homeless Migrant Farmworker Population HPSA",
                                           "Low Income Homeless Population HPSA",
                                           "Migrant Seasonal Worker Population HPSA",
                                           "Low Income Population HPSA"),
                  native_american = c("Native American Population HPSA",
                                      "Medicaid Eligible Population HPSA",
                                      "Other Population HPSA",
                                      "Geographic Population")
    ),
    rural_status = fct_collapse(rural_status,
                              non_rural = "Non-Rural",
                              unknown = c("Not Applicable", "Unknown"),
                              partially_rural = "Partially Rural",
                              rural = "Rural")
  )

hpsa <- hpsa |>
  distinct()

write_csv(hpsa, "hpsa.csv")

### census data ---------------------------------------------------
censusdf <- read_csv("2020Census.csv") |>
  clean_names() |>
  filter(str_detect(geography, "County")==TRUE) |>
  select(geography, total_population) |>
  slice(-1)

census_df <- censusdf |>
  mutate(
    counties = str_remove_all(geography, "County"),
    counties = str_to_title(counties),
    counties = str_trim(counties, side = "right") 
  ) |> select(-geography) |>
  relocate(counties, before = total_population) |>
  rename("total_population" = "before")


census_hpsas <- left_join(hpsas_df, census_df, by = "counties")

### Add two more variables from census_pov_data
census_pov <- read_csv("census_pov_medHHinc_2016.csv") |>
  filter(state == "CA") |>
  slice(-1) |>
  mutate(
    counties = str_remove_all(Name, "County"),
    counties = str_trim(counties),
    med_hh = as.numeric(str_remove_all(med_hh, ",")),
    per_pov = as.numeric(per_pov)
  ) |>
  select(per_pov,med_hh, counties)

complet_df <- census_hpsas |>
  left_join(census_pov, by = "counties")
write_csv(complet_df, "data2.csv")


