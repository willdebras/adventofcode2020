# day 3

library(dplyr)

input <- read.delim("Day 3/input_day3.txt", sep = "\n", col.names = "text", header = F)

input_matrix <- strsplit(input$text, "") %>% as.data.frame() %>% t()


smashin_trees <- function(df, moveright, movedown) {
  
  
  down_pos <- seq(from = 1, to = nrow(df), by = movedown)
  right_pos <- seq(1, by = moveright, length.out = nrow(df)/movedown) %% 31
  right_pos[right_pos==0] <-31
  
  positions <- mapply(function(x,y) df[x,][y], down_pos, right_pos)
  return(length(which(positions == "#")))
  
  
  
}
smashin_trees(input_matrix, 3, 1)


vec <- c(

  smashin_trees(input_matrix, 1, 1),
  smashin_trees(input_matrix, 3, 1),
  smashin_trees(input_matrix, 5, 1),
  smashin_trees(input_matrix, 7, 1),
  smashin_trees(input_matrix, 1, 2)

)

prod(vec)


