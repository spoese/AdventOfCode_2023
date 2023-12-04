library(tidyverse)

games <- readxl::read_excel("Day2.xlsx", col_names = FALSE) |> 
        rename("Games" = "...1")