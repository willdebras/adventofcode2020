#Day 1

input <- read.delim("Day 1/input_day1.txt", col.names = "input", header = F)

combinations <- t(utils::combn(input$input, 2))

comb2020 <- combinations[,1] + combinations[,2]

matrix_index <- match(2020, comb2020)

combinations[matrix_index,1] * combinations[matrix_index,2]
