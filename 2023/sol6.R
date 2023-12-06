# Clean data
data <- readLines("input6.txt")
data <- gsub("^.+: +", "", data)
data <- strsplit(data, " +") |> lapply(as.numeric) |>
  as.data.frame() |> as.matrix() |> t()

# Part 1 

dist <- function(p) (0:p) * (p - (0:p)) # Speed * traveltime

apply(data, 2, function(q){
  mydist <- dist(q[1])
  sum(mydist > q[2])
}) |> prod()

# Part 2

mytime <- paste0(data[1, ], collapse="") |> as.numeric()
record_distance <- paste0(data[2, ], collapse="") |> as.numeric()
dist_v <- dist(mytime)
sum(dist_v > record_distance)

# Alternative, differentiate p * (t - p) = record
# to get the shortest and longest presses to tie the record

highest_equal <- mytime/2 + sqrt(mytime^2/4 - record_distance)
lowest_equal <- mytime/2 - sqrt(mytime^2/4 - record_distance)

highest_equal - lowest_equal - 1
