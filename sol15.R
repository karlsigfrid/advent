
# Prepare the data
s <- readLines("data/15b.txt")
s2 <- gsub("^.+?x=(\\d+|-\\d+).+?y=(\\d+|-\\d+).+?x=(\\d+|-\\d+).+?y=(\\d+|-\\d+)",
     "\\1,\\2,\\3,\\4", s)
df <- read.table(text=s2, sep=",")
colnames(df) <- c("x1", "y1", "x2", "y2")
y <- 2*10^6

#Part 1

impossible <- c()
for (i in 1:nrow(df)){
  d <- df[i, , drop=F]
  dist_beacon <- with(d, abs(x2 - x1) + abs(y2 - y1))
  dist_y <- abs(y - d$y1)
  extra <- dist_beacon - dist_y
  if(extra >= 0){
    interval <- c(d$x1 - extra, d$x1 + extra)
    impossible <- c(impossible, interval[1]:interval[2])
  }
}

impossible <- unique(impossible)
beacons <- df$x2[df$y2 == y]
impossible <- impossible[!(impossible %in% beacons)]
length(impossible)
