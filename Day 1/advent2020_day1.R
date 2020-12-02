#Day 1

input <- read.delim("Day 1/input_day1.txt", col.names = "input", header = F)

combinations <- t(utils::combn(input$input, 2)) # creates two column matrix of unique combinations

comb2020 <- combinations[,1] + combinations[,2] # matrix calculation to equal 2020 

matrix_index <- match(2020, comb2020) # match matrix index of 2020

combinations[matrix_index,1] * combinations[matrix_index,2] #multiplies values for final answer

#Day 1 - part 2


combinations <- t(utils::combn(input$input, 3)) # creates three column matrix of unique combinations

comb2020 <- combinations[,1] + combinations[,2] + combinations[,3] # matrix calculation to equal 2020 

matrix_index <- match(2020, comb2020) # match matrix index of 2020

combinations[matrix_index,1] * combinations[matrix_index,2] * combinations[matrix_index, 3] #multiplies values for final answer
