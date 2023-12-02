oneseat <- function(v){
  v1 <- v[1:7]
  v2 <- v[8:10]

  all_rows <- 1:128
  for (vi in v1){
    nrows <- length(all_rows)
    if(vi == "F") all_rows <- all_rows[1:(nrows/2)]
    if(vi == "B") all_rows <- all_rows[(nrows/2 + 1):nrows]
  }
  this_row <- all_rows - 1

  all_cols <- 1:8
  for (vi in v2){
    ncols <- length(all_cols)
    if(vi == "L") all_cols <- all_cols[1:(ncols/2)]
    if(vi == "R") all_cols <- all_cols[(ncols/2 + 1):ncols]
  }
  this_col <- all_cols - 1
  return(c(this_row, this_col))
}

data <- readLines("input5a.txt")
v <- strsplit(data, "")
df <- sapply(v, oneseat) |> t() |> data.frame()
id <- apply(df, 1, function(q) q[1] * 8 + q[2])
max(id)

mygrid <- expand.grid(0:127, 0:7) |> data.frame()
mygrid$id <- mygrid$Var1 * 8 + mygrid$Var2

mygrid$myseat <-
  !(mygrid$id %in% id) & (mygrid$id - 1) %in% id & (mygrid$id + 1) %in% id 

mygrid$id[which(mygrid$myseat)]
