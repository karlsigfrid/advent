data <- readLines("input12.txt") |> strsplit(" ")
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

myperm <- function(n0, n1){
  n <- n0 + n1
  m <- c()
  for(i in 1:n){
    m <- rbind(cbind(m, 0), cbind(m, 1))
    remove1 <- rowSums(m) > n1
    remove0 <- rowSums(1 - m) > n0
    m <- m[!(remove1 | remove0), , drop=F]
  }
  m <- m[rowSums(m) == n1, ]
  m
}

check_v <- function(v, correct){
  #print("check_v")
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

#21, 7716

## Part 2

data <- readLines("input12_test.txt") |> strsplit(" ")
data <- lapply(data, function(q){
  q1 <- paste0(rep(q[1], 5), collapse="?")
  q2 <- paste0(rep(q[2], 5), collapse=",")
  c(q1, q2)
})
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
