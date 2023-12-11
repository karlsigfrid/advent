# Clean data
data <- readLines("input11.txt") |>
  strsplit("") |> data.frame() |> t()
rownames(data) <- NULL

## Part 1

## Expand
m <- data

rowlist <- apply(m, 1, function(q){
  if(any(q == "#")) return (q)
  return(rbind(q, q))
})
m <- do.call(rbind, rowlist)

collist <- apply(m, 2, function(q){
  if(any(q == "#")) return (q)
  return(cbind(q, q))
})
m <- do.call(cbind, collist)

## Calc dist

pos <- which(m == "#", arr.ind =  T)

dist <- apply(pos, 1, function(q) (t(pos) - q) |> abs() |> sum())

sum(dist) / 2


## Part 2

m2 <- matrix(1, ncol=ncol(data), nrow=nrow(data))
m2[data == "#"] <- 0

m2 <- apply(m2, 1, function(q){
  if(any(q == 0)) return (q)
  return(rep(2, length(q)))
}) |> t()

m2 <- apply(m2, 2, function(q){
  if(any(q == 0)) return (q)
  return(rep(2, length(q)))
})

one_dist <- function(a, b, n=10^6){
  nx <- sum(m2[a[1], a[2]:b[2]] == 2)
  ny <- sum(m2[a[1]:b[1], a[2]] == 2)
  totx <- abs(b[2] - a[2]) + nx * (n - 1)
  toty <- abs(b[1] - a[1]) + ny * (n - 1)
  return(totx + toty)
}

pos <- which(m2 == 0, arr.ind =  T) |>
  apply(1, paste0, collapse=",")
pos2 <- expand.grid(pos, pos) |> as.matrix()
pos2 <- pos2[pos2[, 1] < pos2[, 2], ]

apply(pos2, 1, function(q){
  a <- strsplit(q[1], ",") |> unlist() |> as.numeric()
  b <- strsplit(q[2], ",") |> unlist() |> as.numeric()
  one_dist(a=a, b=b)
}) |> sum()
