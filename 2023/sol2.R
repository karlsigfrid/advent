is_this_possible <- function(thisgame){
  these_max <- sapply(mycolors, function(q){
    this <- gsub(":|;", ",", thisgame) |> strsplit(",") |> unlist()
    this <- this[grepl(q, this)]
    gsub("\\D", "", this) |> as.numeric() |> max()
  })
  is_possible <- all(these_max <= allowed_max)
  list(these_max, is_possible)
}

data <- readLines("input2.txt")
mycolors <- c("blue", "red", "green")
allowed_max <- c(14, 12, 13)

## Part 1
output <- lapply(data, is_this_possible)
possible <- sapply(output, function(q) q[[2]])
which(possible) |> sum()

## Part 2
power <- sapply(output, function(q) prod(q[[1]])) 
sum(power)
