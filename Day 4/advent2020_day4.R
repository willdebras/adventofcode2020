# day 4 

input <- data.table::fread("Day 4/input_day4.txt", header = F, sep = "\n")

# collapse all this garbage so we get one line per passport

input_string <- paste(unlist(input), collapse =" ")
input_df <- strsplit(input_string, split = "  ")
param <- data.table::data.table(string = unlist(strsplit(input_string, split = "  ")))

# I started out string detecting from individual lines, but decided to rip off some code from Adam Austin instead to get these into individual key value dfs

fields    <- strsplit(param$string, " |\n") # splits the fields per passport
kv_list   <- lapply(fields, strsplit, split = ":")  # splits key from value
kv_matrix <- lapply(kv_list, function(x) do.call(rbind, x)) # gives us listed pairs in a matrix
named_lists <- lapply(kv_matrix, function(x) setNames(as.list(x[, 2]), x[, 1])) # applies names to list
df_list     <- lapply(named_lists, data.frame, stringsAsFactors = FALSE) # turns those names into col names in a df



valid <- function(df) {
  
  if (ncol(df)==8) {
    
    return(TRUE)
    
  }
  
  else if (ncol(df)==7 & !any(colnames(df) %in% "cid")) {
    
    return(TRUE)
    
  }
  
  else return(FALSE)
  
}

valids <- lapply(df_list, valid)

length(valids[valids==TRUE])

# Part 2 

# fill in missing data so we can do data validation

all_fields <- Reduce(unique, lapply(df_list, names))
complete_df_list <- lapply(df_list, complete_data, all_fields = all_fields)


valid_2 <- function(df) {
  
  
}
