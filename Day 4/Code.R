library(tidyverse)

cards_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        pull("...1")
cards_backup

get_numbers <- function(x) {
        as.integer(str_sub(cards_backup, start = x, end = x+1))
}

tibble(id = as.integer(str_sub(cards_backup, start = 6, end = 8)),
       winners = map(seq(11, 38, by = 3), get_numbers))

get_numbers(11)
map(seq(11, 38, by = 3), get_numbers)
