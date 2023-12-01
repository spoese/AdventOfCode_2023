library(tidyverse)
library(stringi)

lines <- readxl::read_excel("Data1-1.xlsx",col_names = FALSE) |> 
        rename("Codes" = "...1")

part1 <- lines |> 
        mutate(First = str_extract(Codes, "[:Digit:]"),
               Last = str_extract(stri_reverse(Codes), "[:Digit:]"),
               Total = as.integer(paste0(First,Last)))

sum(part1$Total)
