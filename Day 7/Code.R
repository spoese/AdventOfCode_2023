library(tidyverse)

data_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        rename("Data" = "...1") |> 
        rowwise() |> 
        mutate(Hand = str_split(Data, " ", simplify = TRUE)[1],
               Bid = str_split(Data, " ", simplify = TRUE)[2]) |> 
        select(-Data)

data <- data_backup |> 
        bind_cols(str_split(data_backup$Hand, "", simplify = TRUE)) |> 
        pivot_longer(cols = c(-Hand, -Bid)) |> 
        mutate(value = factor(value, levels = c("2", "3", "4", "5",
                                                   "6", "7", "8", "9",
                                                   "T", "J", "Q", "K", "A"))) |> 
        arrange(desc(value)) |> 
        pivot_wider(id_cols = c(Hand, Bid),
                    names_from = value,
                    names_prefix = "Num",
                    values_from = name,
                    values_fn = length) |> 
        rowwise() |> 
        mutate(Max = max(c_across(starts_with("Num")), na.rm = TRUE),
               Twos = sum(c_across(starts_with("Num")) == 2, na.rm = TRUE)) |> 
        mutate(Type = factor(case_when(
                Max == 5 ~ "Five",
                Max == 4 ~ "Four",
                Max == 3 & Twos == 1 ~ "Full",
                Max == 3 ~ "Three",
                Twos == 2 ~ "TwoPair",
                Twos == 1 ~ "Pair",
                TRUE ~ "None"
        ), levels = c("None", "Pair", "TwoPair", "Three", "Full", "Four", "Five"))) |> 
        select(Hand, Bid, Type)

data |> 
        bind_cols(str_split(data_backup$Hand, ""Hand))
        arrange(Type)
