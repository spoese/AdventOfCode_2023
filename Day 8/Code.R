library(tidyverse)

instructions_backup <- readxl::read_excel("Data.xlsx", n_max = 1, col_names = FALSE) |> 
        pull(1)

maps_backup <- readxl::read_excel("Data.xlsx", skip = 2, col_names = FALSE)

#Part 1

instructions <- instructions_backup |> 
        str_split("") |> 
        unlist()

maps <- maps_backup |> 
        mutate(start = str_sub(...1, end = 3),
               left = str_sub(...1, start = 8, end = 10),
               right = str_sub(...1, start = 13, end = 15)) |> 
        select(-...1)

next_stop <- function(s, direction) {
        if (direction == "L") {
                maps |> 
                        filter(start == s) |> 
                        pull(left)
        } else if (direction == "R") {
                maps |> 
                        filter(start == s) |> 
                        pull(right)
        }
}

#Naive brute-force
# location <- maps$start[1]
# iter <- 0
# done <- FALSE
# while (!done){
#         for (s in instructions) {
#                 iter <- iter + 1
#                 location <- next_stop(location, s)
#                 if (location == "ZZZ") {
#                         print(iter)
#                         done <- TRUE
#                         break
#                 }
#                 if (iter %% 1000000 == 0) {
#                         print(paste0("On iteration ",iter," the location was ",location))
#                 }
#         }
# }

#Cheaper hopefully

get_end <- function(x, names_out) {
        temp <- x
        iter <- 0
        found <- FALSE
        for (i in instructions) {
                iter <- iter + 1
                temp <- next_stop(temp, i)
                if (temp == "ZZZ") {
                        found <- TRUE
                        found_iter <- iter
                }
        } 
        if (found) {
                output <- tibble(Col1 = temp, Col2 = found_iter)
        } else {
                output <- tibble(Col1 = temp, Col2 = NA)
        }
        names(output) <- names_out
        output
}

# check_end <- function(x) {
#         temp <- x
#         iter <- 0
#         found <- FALSE
#         for (i in instructions) {
#                 iter <- iter + 1
#                 temp <- next_stop(temp, i)
#                 if (temp == "ZZZ") {
#                         found <- TRUE
#                         break
#                 }
#         } 
#         if (found) {
#                 iter
#         } else {
#                 NA
#         }
# }

map_pointers <- maps |> 
        rowwise() |> 
        mutate(get_end(start, c("end_location", "zzz_location"))) |> 
        ungroup()

next_skip <- function(s) {
                map_pointers |> 
                        filter(start == s) |> 
                        pull(end_location)
}

found <- FALSE
start <- "AAA"
iter <- 0
locations <- vector()
while (!found) {
        iter <- iter + 1
        start <- next_skip(start)
        if (start == "ZZZ") {
                found <- TRUE
        } else {
                locations <- c(locations, start)
        }
}
iter * length(instructions)
