library(tidyverse)

data_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        rename("Data" = "...1") |> 
        rowwise() |> 
        mutate(Hand = str_split(Data, " ", simplify = TRUE)[1],
               Bid = str_split(Data, " ", simplify = TRUE)[2]) |> 
        select(-Data)

test <- tibble(Hand = c("32T3K", "T55J5", "KK677", "KTJJT", "QQQJA"),
               Bid = c(765, 684, 28, 220, 483))

#Part 1

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
        ungroup() |> 
        mutate(Type = factor(case_when(
                Max == 5 ~ "Five",
                Max == 4 ~ "Four",
                Max == 3 & Twos == 1 ~ "Full",
                Max == 3 ~ "Three",
                Twos == 2 ~ "TwoPair",
                Twos == 1 ~ "Pair",
                TRUE ~ "None"
        ), levels = c("None", "Pair", "TwoPair", "Three", "Full", "Four", "Five"))) |> 
        select(Hand, Bid, Type) |> 
        separate_wider_position(Hand, c(Card1 = 1, Card2 = 1, Card3 = 1,
                                        Card4 = 1, Card5 = 1),
                                cols_remove = FALSE) |> 
        mutate(across(Card1:Card5,
                      ~ factor(.x,
                               levels = c("2", "3", "4", "5",
                                          "6", "7", "8", "9",
                                          "T", "J", "Q", "K", "A")))) |> 
        arrange(desc(Type), desc(Card1), desc(Card2), desc(Card3), desc(Card4),
                desc(Card5))

data |> 
        bind_cols(rank = 1000:1) |> 
        mutate(value = as.integer(Bid)*rank) |> 
        pull(value) |> 
        sum()

#Part 2

joker_data <- data_backup |> 
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
        ungroup() |> 
        mutate(Type = factor(case_when(
                Max == 5 | Max == 4 & NumJ %in% c(1,4) | Max == 3 & NumJ == 2 | NumJ == 3 & Twos == 1 ~ "Five",
                Max == 4 & is.na(NumJ) | Max == 3 & NumJ %in% c(1,3) & Twos == 0 | NumJ == 2 & Twos == 2 ~ "Four",
                Max == 3 & Twos == 1 & is.na(NumJ) | Twos == 2 & NumJ == 1 ~ "Full",
                Max == 3 & is.na(NumJ) | Max == 2 & Twos == 1 & NumJ %in% 1:2 ~ "Three",
                Twos == 2 & is.na(NumJ) ~ "TwoPair",
                Twos == 1 & is.na(NumJ) | Max == 1 & NumJ == 1 ~ "Pair",
                TRUE ~ "None"
        ), levels = c("None", "Pair", "TwoPair", "Three", "Full", "Four", "Five"))) |> 
        select(Hand, Bid, Type) |> 
        separate_wider_position(Hand, c(Card1 = 1, Card2 = 1, Card3 = 1,
                                        Card4 = 1, Card5 = 1),
                                cols_remove = FALSE) |> 
        mutate(across(Card1:Card5,
                      ~ factor(.x,
                               levels = c("J", "2", "3", "4", "5",
                                          "6", "7", "8", "9",
                                          "T", "Q", "K", "A")))) |> 
        arrange(desc(Type), desc(Card1), desc(Card2), desc(Card3), desc(Card4),
                desc(Card5))

joker_data |> 
        bind_cols(rank = 1000:1) |> 
        mutate(value = as.integer(Bid)*rank) |> 
        pull(value) |> 
        sum()
