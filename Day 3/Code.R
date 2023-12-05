library(tidyverse)

engine_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        rename("Data" = "...1")

paste0(engine_backup[1:140,]) |> 
        str_split("") |> 
        table()
x <- 5

get_digits <- function(row) {
        selection_0 <- NULL
        selection_2 <- NULL
        if (row > 1) {
                selection_0 <- engine_backup[row-1,]
        }
        selection_1 <- engine_backup[row,]
        str_extract(selection_1, "")
        if (row < 140) {
                selection_2 <- engine_backup[row+1,]
        }
}

get_digits(1)