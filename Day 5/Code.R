library(tidyverse)
library(readxl)

seeds_backup <- read_excel("Data.xlsx", col_names = FALSE, n_max = 1) |> 
        pull("...1")

seed_soil_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                               skip = 3, n_max = 15) |> 
        pull("...1")

soil_fertilizer_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                     skip = 20, n_max = 12) |> 
        pull("...1")

fertilizer_water_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                      skip = 34, n_max = 38) |> 
        pull("...1")

water_light_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                 skip = 74, n_max = 24) |> 
        pull("...1")

light_temperature_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                       skip = 100, n_max = 29) |> 
        pull("...1")

temperature_humidity_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                          skip = 131, n_max = 31) |> 
        pull("...1")

humidity_location_backup <- read_excel("Data.xlsx", col_names = FALSE, 
                                       skip = 164, n_max = 24) |> 
        pull("...1")

seeds <- seeds_backup |> 
        str_sub(start = 8) |> 
        str_split(" ") |> 
        unlist()

create_db <- function(x) {
        x |>
                tibble() |>
                mutate(Dest = as.numeric(str_split(x, " ", simplify = TRUE)[,1]),
                       Source = as.numeric(str_split(x, " ", simplify = TRUE)[,2]),
                       Length = as.numeric(str_split(x, " ", simplify = TRUE)[,3]),
                       Max = Source + Length - 1,
                       Operation = Dest - Source) |>
                select(Dest, Source, Length, Max, Operation)
}

seed_soil <- create_db(seed_soil_backup)
soil_fertilizer <- create_db(soil_fertilizer_backup)
fertilizer_water <- create_db(fertilizer_water_backup)
water_light <- create_db(water_light_backup)
light_temperature <- create_db(light_temperature_backup)
temperature_humidity <- create_db(temperature_humidity_backup)
humidity_location <- create_db(humidity_location_backup)

input <- seeds[1]
output <- input

for (i in 1:length(seed_soil$Source)) {
        if (input => seed_soil$Source[i] & input <= seed_soil$Max[i]) {
                output <- input + seed_soil$Operation[i]
                break
        }
}
