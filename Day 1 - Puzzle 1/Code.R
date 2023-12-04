library(tidyverse)
library(stringi)

lines <- readxl::read_excel("Data1-1.xlsx",col_names = FALSE) |> 
        rename("Codes" = "...1")

#Part 1

part1 <- lines |> 
        mutate(First = str_extract(Codes, "[:Digit:]"),
               Last = str_extract(stri_reverse(Codes), "[:Digit:]"),
               Total = as.integer(paste0(First,Last)))

sum(part1$Total)

#Part 2

test <- tibble(Codes = c("two1nine", "eightwothree", "abcone2threexyz",
                         "xtwone3four", "4nineeightseven2", "zoneight234",
                         "7pqrstsixteen")) |> 
        mutate(First = str_extract(Codes, "[:Digit:]"),
               Last = str_extract(stri_reverse(Codes), "[:Digit:]"),
               Total = as.integer(paste0(First,Last)))

check_digit <- function(x) {
        for (i in 1:(str_length(x)-2)){
                y <- str_sub(x, start = i)
                x <- case_when(
                        str_sub(y, end = 3) == "one" ~ paste0(str_sub(x, end = i-1),
                                                              "  1",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 3) == "two" ~ paste0(str_sub(x, end = i-1),
                                                              "  2",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 5) == "three" ~ paste0(str_sub(x, end = i-1),
                                                                "    3",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 4) == "four" ~ paste0(str_sub(x, end = i-1),
                                                               "   4",
                                                               str_sub(y, start = 5)),
                        str_sub(y, end = 4) == "five" ~ paste0(str_sub(x, end = i-1),
                                                               "   5",
                                                               str_sub(y, start = 5)),
                        str_sub(y, end = 3) == "six" ~ paste0(str_sub(x, end = i-1),
                                                              "  6",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 5) == "seven" ~ paste0(str_sub(x, end = i-1),
                                                                "    7",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 5) == "eight" ~ paste0(str_sub(x, end = i-1),
                                                                "    8",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 4) == "nine" ~ paste0(str_sub(x, end = i-1),
                                                               "   9",
                                                               str_sub(y, start = 5)),
                        TRUE ~ paste0(str_sub(x, end = i-1),y)
                )
        }
        str_replace_all(x, " ", "")
}

rev_check_digit <- function(x) {
        for (i in 1:(str_length(x)-2)){
                y <- str_sub(x, start = i)
                x <- case_when(
                        str_sub(y, end = 3) == "eno" ~ paste0(str_sub(x, end = i-1),
                                                              "  1",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 3) == "owt" ~ paste0(str_sub(x, end = i-1),
                                                              "  2",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 5) == "eerht" ~ paste0(str_sub(x, end = i-1),
                                                                "    3",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 4) == "ruof" ~ paste0(str_sub(x, end = i-1),
                                                               "   4",
                                                               str_sub(y, start = 5)),
                        str_sub(y, end = 4) == "evif" ~ paste0(str_sub(x, end = i-1),
                                                               "   5",
                                                               str_sub(y, start = 5)),
                        str_sub(y, end = 3) == "xis" ~ paste0(str_sub(x, end = i-1),
                                                              "  6",
                                                              str_sub(y, start = 4)),
                        str_sub(y, end = 5) == "neves" ~ paste0(str_sub(x, end = i-1),
                                                                "    7",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 5) == "thgie" ~ paste0(str_sub(x, end = i-1),
                                                                "    8",
                                                                str_sub(y, start = 6)),
                        str_sub(y, end = 4) == "enin" ~ paste0(str_sub(x, end = i-1),
                                                               "   9",
                                                               str_sub(y, start = 5)),
                        TRUE ~ paste0(str_sub(x, end = i-1),y)
                )
        }
        str_replace_all(x, " ", "")
}

test |>
        rowwise() |> 
        mutate(Mod_Codes = check_digit(Codes),
               Reverse_Mod_Codes = rev_check_digit(stri_reverse(Codes)),
               Mod_First = str_extract(Mod_Codes, "[:Digit:]"),
               Mod_Last = str_extract(Reverse_Mod_Codes, "[:Digit:]"),
               Mod_Total = as.integer(paste0(Mod_First,Mod_Last)))

part2 <- lines |> 
        rowwise() |> 
        mutate(Mod_Codes = check_digit(Codes),
               Reverse_Mod_Codes = rev_check_digit(stri_reverse(Codes)),
               Mod_First = str_extract(Mod_Codes, "[:Digit:]"),
               Mod_Last = str_extract(Reverse_Mod_Codes, "[:Digit:]"),
               Mod_Total = as.integer(paste0(Mod_First,Mod_Last)))

sum(part2$Mod_Total)
