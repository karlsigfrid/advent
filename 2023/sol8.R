# Clean data
data <- readLines("input8.txt")
lr <- data[1] |> strsplit("") |> unlist()
data <- tail(data, -2)

map <- strsplit(data, " = ")
listnames <- sapply(map, function(q) q[1])
map <- lapply(map, function(q){
  q2 <- gsub("[()]", "", q[2]) |>
    strsplit(", ") |> unlist()
  list(L=q2[1], R=q2[2])
} )
names(map) <- listnames
map

# Part 1

count_steps <- function(current="AAA", end="ZZZ"){
  count <- 0
  repeat{
    for(direction in lr){
      count <- count + 1
      current <- map[[current]][[direction]]
      if(grepl(end, current)) return (list(count, current))
    }
  }
}
count_steps()

# Part 2

starts <- listnames[grepl("..A", listnames)]
cycles <- lapply(starts, count_steps, end="..Z") |> sapply(function(q) q[[1]])
nc <- length(cycles)
denoms <- lapply(cycles, function(q){
  which(q %% (1:q) == 0)
}) |> unlist() |> table()

common <- denoms[denoms==nc][2] |> names() |> as.numeric()
c2 <- cycles
c2[-1] <- c2[-1] / common
prod(c2) |> format(digits=22)


