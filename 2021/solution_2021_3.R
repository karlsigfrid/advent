more1 <- function(v){
  sumv <- sum(v == 1)
  sum0 <- sum(v == 0)
  if (sumv == sum0) return (NA)
  sumv > sum0
}

to_decimal <- function(x){
  n <- length(x)
  multi <- 2^((n-1):0)
  sum(multi * x)
}

m2 <- "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" %>% strsplit("\n") %>% unlist %>% strsplit("") %>%
  lapply(as.numeric) %>%
  do.call(rbind, .)


m1 <- readLines("input_2021_3.txt") %>% strsplit("") %>%
  lapply(as.numeric) %>%
  do.call(rbind, .)

n <- nrow(m1)

the_gamma <- (apply(m1, 2, sum) > (n/2)) %>% as.numeric
the_eps <- 1 - the_gamma
to_decimal(the_gamma) * to_decimal(the_eps)



### Part 2

ox <- scrub <- m1
pos <- 1
while(nrow(ox) > 1){
  if (is.na(more1(ox[, pos])) | more1(ox[, pos])){
    ox <- ox[ox[, pos] == 1, , drop=F]
  }
  else{
    ox <- ox[ox[, pos] == 0, , drop=F]
  }
  pos <- pos + 1
}

pos <- 1
while(nrow(scrub) > 1){
  if (is.na(more1(scrub[, pos])) | more1(scrub[, pos])){
    scrub <- scrub[scrub[, pos] == 0, , drop=F]
  }
  else{
    scrub <- scrub[scrub[, pos] == 1, , drop=F]
  }
  pos <- pos + 1
}

c(ox)
c(scrub)
to_decimal(c(ox)) * to_decimal(c(scrub))
