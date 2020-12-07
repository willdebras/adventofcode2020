input <- read.delim("Day 5/input_day5.txt", col.names = "input", header = F)

calc_seatid <- function(x) {
 
  
  sub1 <- gsub("F|L", "0",  x) 
  sub2 <- gsub("B|R", "1",  sub1)
  
  
  row <- strtoi(substr(sub2, 1, 7), base = 2)
  column <- strtoi(substr(sub2, 8, 10), base = 2)
  
  seatid <- row*8 + column
  
  return(seatid)
}


seatids <- lapply(input$input, calc_seatid)

max(unlist(seatids))


# part 2

seat_range <- min(unlist(seatids)):max(unlist(seatids))

seat_diff <- setdiff((seat_range), seatids)

seat_diff
