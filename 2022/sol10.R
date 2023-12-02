
# Prepare the data

the_indices <- c(20, 60, 100, 140, 180, 220)
x <- readLines("data/10b.txt") |>
  strsplit(split=" ") |>
  unlist() |>
  (function(.) gsub("addx|noop", "0", .))() |>
  as.numeric() |>
  (function(.) c(1, .))() |>
  cumsum()


# Part 1

sum(x[the_indices] * the_indices)


# Part 2

df <- data.frame(x=x[-241], col=rep(0:39, 6))
df$draw <- sapply(1:240, function(q){
  as.numeric(df$col[q] %in% (df$x[q] + c(-1, 0, 1))) + 1
})

image_m <- matrix(c(".", "#")[df$draw], nrow=6, byrow=T)
for (i in 1:6) print(paste(image_m[i, ], collapse=""))