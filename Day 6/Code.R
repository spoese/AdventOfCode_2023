library(tidyverse)

data_backup <- tibble(Time = c(53, 91, 67, 68),
                      Distance = c(250, 1330, 1081, 1025))

#Part 1

ways <- NULL
for (i in 1:4){
        temp <- tibble(Hold = 0:data_backup$Time[i]) |> 
                mutate(Distance = Hold*(data_backup$Time[i]-Hold)) |> 
                filter(Distance > data_backup$Distance[i]) |> 
                pull(Hold) |> 
                length()
        ways <- c(ways, temp)
}
prod(ways)
