v <- readLines("input_2021_1.txt") |> as.numeric()
sum(diff(v) > 0)

n <- length(v)
v2 <- sapply(1:(n-2), function(q) sum(v[q:(q+2)]))

sum(diff(mid2) > 0)
