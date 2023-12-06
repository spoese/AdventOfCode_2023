library(tidyverse)

engine_backup <- readxl::read_excel("Data.xlsx", col_names = FALSE) |> 
        rename("Data" = "...1") |> 
        pull(Data)

test_backup <- c("467..114..",
                 "...*......",
                 "..35..633.",
                 "......#...",
                 "617*......",
                 ".....+.58.",
                 "..592.....",
                 "......755.",
                 "...$.*....",
                 ".664.598..")


local_backup <- engine_backup

#Part 1

part_numbers <- NULL

for (row in 2:139) {
        symbol_locs <- unlist(gregexpr('[^0-9.]', local_backup[row]))
        
       
        if (symbol_locs[1] != -1) {
                for (i in symbol_locs) {
                        #Start with in-row observations
                        prev_num <- str_sub(local_backup[row], end = i-1) |> 
                                str_extract("\\d+$")
                        if (!is.na(prev_num)) {
                                part_numbers <- c(part_numbers, prev_num)
                                prev_length <- str_length(prev_num)
                                local_backup[row] <- paste0(str_sub(local_backup[row], end = i-prev_length-1),
                                                            paste(rep(".", prev_length), collapse = ""),
                                                            str_sub(local_backup[row], start = i))
                        }
                        next_num <- str_sub(local_backup[row], start = i+1) |> 
                                str_extract("^\\d+")
                        if (!is.na(next_num)) {
                                part_numbers <- c(part_numbers, next_num)
                                next_length <- str_length(next_num)
                                local_backup[row] <- paste0(str_sub(local_backup[row], end = i),
                                                            paste(rep(".", next_length), collapse = ""),
                                                            str_sub(local_backup[row], start = i+next_length+1))
                        }
                        
                        #Move to previous row observations
                        for (j in 1:5) {
                                test <- str_sub(local_backup[row-1], start = i+2-j, end = i+4-j)
                                if (grepl("[0-9]{3}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row-1] <- paste0(str_sub(local_backup[row-1], end = i+1-j),
                                                                      paste(rep(".", 3), collapse = ""),
                                                                      str_sub(local_backup[row-1], start = i+5-j))
                                }
                        }
                        for (k in 1:4) {
                                test <- str_sub(local_backup[row-1], start = i+2-k, end = i+3-k)
                                if (grepl("[0-9]{2}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row-1] <- paste0(str_sub(local_backup[row-1], end = i+1-k),
                                                                      paste(rep(".", 2), collapse = ""),
                                                                      str_sub(local_backup[row-1], start = i+4-k))
                                }
                        }
                        for (l in 1:3) {
                                test <- str_sub(local_backup[row-1], start = i+2-l, end = i+2-l)
                                if (grepl("[0-9]{1}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row-1] <- paste0(str_sub(local_backup[row-1], end = i+1-l),
                                                                      paste(rep(".", 1), collapse = ""),
                                                                      str_sub(local_backup[row-1], start = i+3-l))
                                }
                        }
                        
                        #Move to next row observations
                        for (j in 1:5) {
                                test <- str_sub(local_backup[row+1], start = i+2-j, end = i+4-j)
                                if (grepl("[0-9]{3}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row+1] <- paste0(str_sub(local_backup[row+1], end = i+1-j),
                                                                      paste(rep(".", 3), collapse = ""),
                                                                      str_sub(local_backup[row+1], start = i+5-j))
                                }
                        }
                        for (k in 1:4) {
                                test <- str_sub(local_backup[row+1], start = i+2-k, end = i+3-k)
                                if (grepl("[0-9]{2}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row+1] <- paste0(str_sub(local_backup[row+1], end = i+1-k),
                                                                      paste(rep(".", 2), collapse = ""),
                                                                      str_sub(local_backup[row+1], start = i+4-k))
                                }
                        }
                        for (l in 1:3) {
                                test <- str_sub(local_backup[row+1], start = i+2-l, end = i+2-l)
                                if (grepl("[0-9]{1}", test)) {
                                        part_numbers <- c(part_numbers, test)
                                        local_backup[row+1] <- paste0(str_sub(local_backup[row+1], end = i+1-l),
                                                                      paste(rep(".", 1), collapse = ""),
                                                                      str_sub(local_backup[row+1], start = i+3-l))
                                }
                        }
                }
        }
}

part_numbers |> 
        unlist() |> 
        as.integer() |> 
        sum()
