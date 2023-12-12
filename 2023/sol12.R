data <- readLines("input12_test.txt") |> strsplit(" ")
data <- lapply(data, function(q){
  correct <- strsplit(q[2], ",")[[1]] |> as.numeric()
  v <- gsub("#", "1", q[1])
  v <- gsub("[.]", "0", v)
  v <- gsub("[?]", "2", v)
  v <- strsplit(v, "")[[1]] |> as.numeric()
  which2 <- which(v == 2)
  n1 <- sum(correct) - sum(v == 1)
  n0 <- sum(v == 2) - n1
  v[v == 2] <- 0
  list(correct=correct, v=v, which2=which2, n0=n0, n1=n1)
})

## Part 1

myperm <- function(n0, n1, correct){
  print("myperm")
  ucorrect <- unique(correct)
  n <- n0 + n1
  m <- c()
  for(i in 1:n){
    m <- rbind(cbind(m, 0), cbind(m, 1))
    paste0()
  }
  m <- m[rowSums(m) == n1, ]
  m
}

check_v <- function(v, correct){
  print("check_v")
  mygroups <- paste0(v, collapse="") |> strsplit("0+") |> unlist()
  n <- nchar(mygroups)
  n <- n[n != 0]
  length(n) == length(correct) && all(n == correct)
}

all_sums <- c()
for (i in 1:length(data)){
  print(i)
  m <- myperm(data[[i]]$n0, data[[i]]$n1) |> rbind()
  ncorrect <- apply(m, 1, function(q){
    thisperm <- data[[i]]$v
    thisperm[data[[i]]$which2] <- q
    check_v(v=thisperm, correct=data[[i]]$correct)
  }) |> sum()
  all_sums <- c(all_sums, ncorrect)
}

all_sums |> sum()