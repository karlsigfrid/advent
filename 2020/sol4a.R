

data <- readLines("input4a.txt")
d2 <- strsplit(data, " ")
d2 <- c(d2, list(character(0)))

all_list <- list()
all_these <- c()
for (this in d2){
  if(length(this) == 0){
    all_list <- c(all_list, list(all_these))
    all_these <- c()
  }else{
    these <- gsub("^(.+):.+", "\\1", this)
    all_these <- c(all_these, these)
  }
}

needed <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

complete <- sapply(all_list, function(q) all(needed %in% q))
sum(complete)