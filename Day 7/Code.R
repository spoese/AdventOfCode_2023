library(tidyverse)

data_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        rename("Data" = "...1") |> 
        rowwise() |> 
        mutate(Hand = str_split(Data, " ", simplify = TRUE)[1],
               Bid = str_split(Data, " ", simplify = TRUE)[2])

