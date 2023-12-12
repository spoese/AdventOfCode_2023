library(tidyverse)
library(readxl)
library(intervals)

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
        unlist() |> 
        as.numeric()

create_db <- function(x) {
        x |>
                tibble() |>
                mutate(Dest = as.numeric(str_split(x, " ", simplify = TRUE)[,1]),
                       Source = as.numeric(str_split(x, " ", simplify = TRUE)[,2]),
                       Length = as.numeric(str_split(x, " ", simplify = TRUE)[,3]),
                       Max_Source = Source + Length - 1,
                       Max_Dest = Dest + Length - 1,
                       Operation = Dest - Source) |>
                select(Dest, Source, Length, Max_Dest, Max_Source, Operation)
}

map_list <- map(list(seed_soil_backup, soil_fertilizer_backup,
                     fertilizer_water_backup, water_light_backup,
                     light_temperature_backup, temperature_humidity_backup,
                     humidity_location_backup), create_db)

# test_seeds <- c(79,14,55,13)
# test_seed <- c("50 98 2", "52 50 48")
# test_soil <- c("0 15 37", "37 52 2", "39 0 15")
# test_fertilizer <- c("49 53 8", "0 11 42", "42 0 7", "57 7 4")
# test_water <- c("88 18 7", "18 25 70")
# test_light <- c("45 77 23", "81 45 19", "68 64 13")
# test_temperature <- c("0 69 1", "1 0 69")
# test_humidity <- c("60 56 37", "56 93 4")
# test_maps <- map(list(test_seed, test_soil, test_fertilizer,
#                       test_water, test_light, test_temperature,
#                       test_humidity), create_db)

#Part 1

# test_input <- test_seeds[4]
# for (j in 1:7) {
#         for (i in 1:length(test_maps[[j]]$Source)) {
#                 if (test_input >= test_maps[[j]]$Source[i] & test_input <= test_maps[[j]]$Max[i]) {
#                         test_input <- test_input + test_maps[[j]]$Operation[i]
#                         break
#                 }
#         }
# }
# test_input


get_location <- function(input) {
        for (j in 1:7) {
                for (i in 1:length(map_list[[j]]$Source)) {
                        if (input >= map_list[[j]]$Source[i] & 
                            input <= map_list[[j]]$Max_Source[i]) {
                                input <- input + map_list[[j]]$Operation[i]
                                break
                        }
                }
        }
        input
}


map_dbl(seeds, get_location) |> 
        min()
#Part 2

seeds_bounds <- tibble(Starts = seeds[seq(1, 19, by = 2)],
                       Lengths = seeds[seq(2, 20, by = 2)]) |>
        mutate(Ends = Starts + Lengths - 1)

ss_function <- function(seed, num) {
        mapping <- map_list[[num]] |> 
                filter(seed >= Source,
                       seed <= Max_Source)
        if (dim(mapping)[1] == 0) {
                seed
        } else {
                seed + mapping$Operation
        }
}

outputs <- NULL
for (j in 1:10) {
        candidates <- tibble(lower = seeds_bounds$Starts[j], 
                             upper = seeds_bounds$Ends[j])
        for (i in 1:7) {
                all_bounds <- NULL
                for (k in 1:dim(candidates)[1]) {
                        new_ubs <- map_list[[i]] |> 
                                filter(Max_Source > candidates$lower[k],
                                       Max_Source < candidates$upper[k]) |> 
                                arrange(Max_Source) |> 
                                pull(Max_Source) 
                        new_lbs <- map_list[[i]] |> 
                                filter(Source > candidates$lower[k],
                                       Source < candidates$upper[k]) |> 
                                arrange(Source) |> 
                                pull(Source) 
                        new_ubs <- c(new_ubs, new_lbs-1) |> 
                                unique()
                        new_lbs <- c(new_lbs, new_ubs+1) |> 
                                unique()
                        all_bounds <- c(all_bounds,
                                        candidates$lower[k], 
                                        candidates$upper[k],
                                        new_ubs, new_lbs) |> 
                                unique() |> 
                                sort()
                        # new_candidates <- tibble(lower = all_bounds[seq(1, length(all_bounds)-1, by = 2)],
                        #                         upper = all_bounds[seq(2, length(all_bounds), by = 2)])
                }
                candidates_vec <- map2(all_bounds, i, ss_function) |> 
                        unlist()
                candidates <- tibble(lower = candidates_vec[seq(1, length(candidates_vec)-1, by = 2)],
                                     upper = candidates_vec[seq(2, length(candidates_vec), by = 2)])
        }
        outputs <- c(outputs, candidates_vec)
}
outputs |> min()