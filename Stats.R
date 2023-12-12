stats <- tibble(Day = 1:11,
                PartB = c(205876, 173146, 114224, 113115, 68811, 87649, 68476,
                          60891, 61120, 36451, 41710),
                PartAOnly = c(65363, 7901, 16994, 15101, 26628, 1418, 6027,
                              12082, 862, 14165, 1944)) |> 
        rowwise() |> 
        mutate(PartA = sum(PartB,PartAOnly)) |> 
        bind_cols(PrevA = lag(stats$PartA, order_by = stats$Day),
                  PrevB = lag(stats$PartB, order_by = stats$Day)) |> 
        select(Day, PartA, PartB, PrevA, PrevB)

PartA_model <- lm(PartA ~ Day + PrevA, data = stats)

PartB_model <- lm(PartB ~ PartA, data = stats)

parta_comp <- tibble(Day = 1:11,
                     Original = stats$PartA,
                     Predictions = predict(PartA_model, 
                                           stats |> 
                                                   select(Day, PrevA))) |> 
        mutate(Diff = (Original - Predictions)/Predictions) |> 
        arrange(Diff)
parta_comp

partb_comp <- tibble(Day = 1:11,
                     Original = stats$PartB,
                     Predictions = predict(PartB_model, 
                                           parta_comp |> 
                                                   arrange(Day) |> 
                                                   select(Predictions) |> 
                                                   rename("PartA" = "Predictions"))) |> 
        mutate(Diff = (Original - Predictions)/Predictions) |> 
        arrange(Diff)
partb_comp
