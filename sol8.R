check_visibility <- function(m, the_row, the_col){
  row_n <- nrow(m)
  col_n <- ncol(m)
  surround <- list(m[1:(the_row - 1), the_col],
                   m[(the_row + 1):row_n, the_col],
                   m[the_row, 1:(the_col - 1)],
                   m[the_row, (the_col + 1):col_n])
  sapply(surround, function(q) all(m[the_row, the_col] > q)) |> any()
}

count_direction <- function(m, the_row, the_col, d){
  edges <- c(1, nrow(m), 1, ncol(m))

  if(d == 1) v <- c(-1, 0) #going up
  if(d == 2) v <- c(1, 0)  #down
  if(d == 3) v <- c(0, -1) #left
  if(d == 4) v <- c(0, 1)  #right
  
  n_visible <- 0
  steps <- 1
  repeat{
    n_visible <- n_visible + 1
    if(m[the_row + steps * v[1], the_col + steps * v[2]] >= m[the_row, the_col]|
       (the_row + steps * v[1] == edges[d] & d %in% c(1, 2)) |
        (the_col + steps * v[2] == edges[d] & d %in% c(3, 4))){
      break
    }
    steps <- steps + 1
  }
    n_visible
}


# Read data, transform data
data <- readLines("data/8b.txt") |>
  strsplit(split="") |>
  lapply(as.integer)
m <- do.call(rbind, data)

## Part 1

# Count inner visible trees
vis_inner <- 0
for(the_row in 2:(nrow(m)-1)){
  for(the_col in 2:(ncol(m)-1)){
    vis_inner <- vis_inner + check_visibility(m, the_row, the_col)
  }
}
# Count edges
vis_edge <- 2 * nrow(m) + 2 * ncol(m) - 4

vis_inner + vis_edge

## Part 2
vis_scores <- c()
for(the_row in 2:(nrow(m)-1)){
  for(the_col in 2:(ncol(m)-1)){
    vis_scores <-
      c(vis_scores,
        sapply(1:4, function(q) count_direction(m, the_row, the_col, d=q)) |>
          prod())
  }
}
max(vis_scores)
