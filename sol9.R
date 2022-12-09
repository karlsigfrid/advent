#h, t = c(x, y)
move_t <- function(h, t){ 
  ht_diff <- h - t
  abs_diff <- abs(ht_diff)
  the_move <- c(0, 0)
  if(max(abs_diff > 1) & min(abs_diff) > 0) the_move <- c(1, 1)
  if(abs_diff[1] > 1 & abs_diff[2] == 0) the_move <- c(1, 0)
  if(abs_diff[1] == 0 & abs_diff[2] > 1) the_move <- c(0, 1)
  the_move <- the_move * sign(ht_diff)
  the_move
}

s <- readLines("data/9b.txt") |> strsplit(split=" ")
dir_list <- list(R=c(1, 0), L=c(-1, 0), U=c(0, 1), D=c(0, -1))

## Part 1
h <- c(0, 0)
t <- c(0, 0)
all_t <- c(0, 0)

for (this in s){
  for(i in 1:as.numeric(this[2])){
    h <- h + dir_list[[this[1]]]
    t <- t + move_t(h=h, t=t)
    all_t <- rbind(all_t, t)
  }
}
all_t |> unique() |> nrow()


## Part 2
ht <- matrix(0, ncol=2, nrow=10)
all_t9 <- c(0, 0)

for (this in s){
  for(i in 1:as.numeric(this[2])){
    ht[1, ] <- ht[1, ] + dir_list[[this[1]]]
    for (j in 1:9){
      ht[j+1, ] <- ht[j+1, ] + move_t(h=ht[j, ], t=ht[j+1, ])
    }
    all_t9 <- rbind(all_t9, ht[10, ])
  }
}
all_t9 |> unique() |> nrow()