count_adjecent <- function(v, m){
  dirs <- matrix(c(1,0,0,-1,0,0,0,1,0,0,-1,0,0,0,1,0,0,-1), ncol=3, byrow=T)
  adj_v <- apply(dirs, 1, function(q){
    this_v <- v + q
    any(m[, 1] == this_v[1] & m[, 2] == this_v[2] & m[, 3] == this_v[3])
  })
  sum(adj_v)
} 
  


#Prepare data
s <- readLines("data/18b.txt") |>
  strsplit(",") |>
  lapply(as.integer)
m <- do.call(rbind, s)

# Part 1
num_adjecent <- apply(m, 1, count_adjecent, m=m)
6 * nrow(m) - sum(num_adjecent)
