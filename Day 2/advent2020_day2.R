# day 2


input <- read.csv("Day 2/input_day2.csv", col.names = "input", header = F)


password_qual <- function(x) {
  
  minmax <- stringr::str_split_fixed(x, "[^0-9]", n = 3)
  min <- as.numeric(minmax[1])
  max <- as.numeric(minmax[2])
  
  qualresp <- stringr::str_split(minmax[3], ":")
  
  qual <- qualresp[[1]][1]
  resp <- qualresp[[1]][2]
  
  count <- stringr::str_count(qualresp[[1]][2], qualresp[[1]][1])
  
  return(ifelse(count>=min&count<=max, TRUE, FALSE))
  
}

quals <- lapply(input$input, password_qual)
length(quals[quals==TRUE])
