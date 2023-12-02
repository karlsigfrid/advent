# Prep data
v <- readLines("data/20b.txt") |> as.numeric()
n <- length(v)
n3 <- 2 * n 
v2 <- 1:n

for(i in 1:n){
#  if (sign(v[i]) == 1) v3 <- c(rbind(NA, v2))
#  if (sign(v[i]) == -1) v3 <- c(rbind(v2, NA))
  v3 <- c(rbind(v2, NA))
  where <- which(v3 == i)
  step <- v[i] * 2 + sign(v[i])
  whereto <- (where + step) %% n3
  if(whereto == 0) whereto <- 2 * n
  if(whereto != where){
    v3[whereto] <- i
    v3[where] <- NA
  }
  v2 <- na.omit(v3)
#  print(v[v2])
}

v4 <- v[v2]

index <- sapply(c(1000, 2000, 3000), function(q) (which(v4 == 0) + q) %% n)
sum(v4[index])
