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

# List of dataframes of valid passsports so far

df_list2 <- df_list[valids==TRUE]


valid2 <- function(df) {
  
  
  val_byr <- as.numeric(df[,"byr"])>=1920 & as.numeric(df[,"byr"]) <= 2002
  val_iyr <- as.numeric(df[,"iyr"])>=2010 & as.numeric(df[,"iyr"]) <= 2020
  val_eyr <- as.numeric(df[,"eyr"])>=2020 & as.numeric(df[,"eyr"]) <= 2030
  val_hcl <- grepl("^\\#[0-9a-f]{6}$", df[,"hcl"])
  val_ecl <- df[,"ecl"] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val_pid <- grepl("^[0-9]{9}$", df[,"pid"])
  
  val_hgt <- ifelse(grepl("cm", df[,"hgt"]), # the if for cm
                    as.numeric(substr(df[,"hgt"], 1, nchar(df[,"hgt"])-2))>=150 & as.numeric(substr(df[,"hgt"], 1, nchar(df[,"hgt"])-2))<=193, # the logical for cm
                    ifelse(grepl("in", df[,"hgt"]), # the if for inches
                           as.numeric(substr(df[,"hgt"], 1, nchar(df[,"hgt"])-2))>=59 & as.numeric(substr(df[,"hgt"], 1, nchar(df[,"hgt"])-2))<=76, # the logical for inches
                           FALSE
                    )
  )
  
  valid_all_fields <- all(val_byr, val_iyr, val_eyr, val_hcl, val_ecl, val_pid, val_hgt)

  return(valid_all_fields)
  
}


valids2 <- lapply(df_list2, valid2)

length(valids2[valids2==TRUE])

