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

location <- maps$start[1]
iter <- 0
done <- FALSE
while (!done){
        for (s in instructions) {
                iter <- iter + 1
                location <- next_stop(location, s)
                if (location == "JHJ") {
                        print(iter)
                        done <- TRUE
                        break
                }
        }
        if (iter > 10000) {
                print("Taking too long")
                done <- TRUE
        }
}
