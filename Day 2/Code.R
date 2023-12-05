library(tidyverse)

games_backup <- readxl::read_excel("Day2.xlsx", col_names = FALSE) |> 
        rename("Games" = "...1")

#Part 1

get_games <- function(x) {
        trials <- gsub(".+: ", "", x) |> 
                str_split("; ") |> 
                unlist()
        tibble(Trial = 1:length(trials),
               Results = trials) |> 
                mutate(Green = as.integer(str_extract(Results, "\\d+(?= green)")),
                       Red = as.integer(str_extract(Results, "\\d+(?= red)")),
                       Blue = as.integer(str_extract(Results, "\\d+(?= blue)")),
                       ID = 1) |> 
                replace_na(list(Green = 0, Red = 0, Blue = 0)) |> 
                pivot_longer(Green:Blue, names_to = "Color",values_to = "N") |> 
                pivot_wider(id_cols = ID,
                            names_from = c(Color, Trial),
                            values_from = N) |> 
                select(-ID)
}

test <- tibble(Games = c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                         "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                         "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                         "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                         "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

games <- games_backup |> 
        rowwise() |> 
        mutate(ID = str_extract(Games, "Game \\d+") |> 
                       str_sub(start = 6),
               get_games(Games)) |> 
        pivot_longer(cols = c(-Games,-ID), names_to = "Color_Trial", values_to = "N") |> 
        rowwise() |> 
        mutate(Color = str_split(Color_Trial, "_")[[1]][1],
               Trial = str_split(Color_Trial, "_")[[1]][2]) |> 
        select(-Games, -Color_Trial) |> 
        pivot_wider(id_cols = c(ID, Trial),
                    names_from = Color,
                    values_from = N) 

possible <- games |> 
        summarize(Max_Green = max(Green, na.rm = TRUE),
                  Max_Red = max(Red, na.rm = TRUE),
                  Max_Blue = max(Blue, na.rm = TRUE),
                  .by = ID) |> 
        mutate(Possible = Max_Green <= 13 &
                                 Max_Red <= 12 &
                                 Max_Blue <= 14,
               ID = as.integer(ID)) |> 
        filter(Possible)

sum(possible$ID)        
