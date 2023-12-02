
numtrees <- function(xstep, ystep){
  x <- seq(1, 1+xstep*(nr-1), by=xstep)
  y <- seq(1, nr, by=ystep) 

  pos <- c()
  for (i in 1:nr){
    pos <- c(pos, m[y[i], x[i]])
  }
  print(table(pos))
}


data <- readLines("input3a.txt")
nr <- length(data)
needed_width <- nr * 7 + 1
width <- nchar(data[1])
nrep <- ceiling(needed_width / width)

m <- sapply(data, function(q){
  s2 <- strsplit(q, "")[[1]]
  s2 <- rep(s2, nrep)
  s2
}) |> t()

xes <- c(1,3,5,7,1)
ys <- c(1,1,1,1,2)
treess <- c()
for (j in 1:5){
  treess <- c(treess, numtrees(xes[j], ys[j])[1])
}

prod(treess)