library(tidyverse)

# test_backup <- c("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
#                  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
#                  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
#                  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
#                  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
#                  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

cards_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        pull("...1")
cards_backup

#Part 1

get_numbers <- function(x) {
        tibble(numbers = as.integer(str_sub(cards_backup, start = x, end = x+1)))
}

# tibble(id = as.integer(str_sub(test_backup, start = 6, end = 6)),
#        winners = list_cbind(map(seq(9, 21, by = 3), get_numbers)),
#        options = list_cbind(map(seq(26, 47, by = 3), get_numbers))) |> 
#         rowwise() |> 
#         mutate(Num_Winners = sum(winners %in% options)) |> 
#         select(id, Num_Winners) |> 
#         mutate(Points = trunc(2^(Num_Winners-1)))


tibble(id = as.integer(str_sub(cards_backup, start = 6, end = 8)),
               winners = list_cbind(map(seq(11, 38, by = 3), get_numbers)),
               options = list_cbind(map(seq(43, 115, by = 3), get_numbers))) |> 
        rowwise() |> 
        mutate(Num_Winners = sum(winners %in% options)) |> 
        select(id, Num_Winners) |> 
        mutate(Points = trunc(2^(Num_Winners-1))) |> 
        pull(Points) |> 
        sum()

#Part 2

