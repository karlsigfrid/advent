elf_to_dec <- function(v){
  fives <- rev(5^(0:(length(v)-1)))
  t(v) %*% fives
}

# Prepare the data
s <- readLines("data/25b.txt") |>
  strsplit("")

v <- lapply(s, function(q){
  q2 <- gsub("-", "-1", q)
  q3 <- gsub("=", "-2", q2)
  as.numeric(q3)
  })

res <- c()
for (this in v) res <- c(res, elf_to_dec(this))

resn <- sum(res)
rest <- format(resn, digits=22)

#Dec to elf
resv <- strsplit(rest, "")[[1]] |> as.numeric()
n <- length(resv)

#Find the number of digits in elf
ndig <- 0
the_max <- 0
while (the_max < resn){
  ndig <- ndig + 1
  the_max <- sum(2 * (5^(0:(ndig-1))))
}

# Translate to elf, digit by digit
elf_scale <- (-2):2
elf <- rep(2, ndig)
this_d <- 1

for(this_d in 1:ndig){
  prev_dig <- elf[1:(this_d - 1)]
  if(this_d == 1) prev_dig <- c()
  trail_dig <- elf[(this_d + 1):ndig]
  if(this_d == ndig) trail_dig <- c()
  candidates <- sapply(elf_scale, function(q){
    vtry <- c(prev_dig, q, trail_dig)
    elf_to_dec(vtry)
  })
  next_n <- min(which(candidates >= resn))
  elf[this_d] <- elf_scale[next_n]
}

#Translate the negative numbers
elf2 <- as.character(elf)
elf2 <- gsub("-2", "=", elf2)
elf2 <- gsub("-1", "-", elf2)
elf2 <- paste(elf2, collapse="")
elf2
