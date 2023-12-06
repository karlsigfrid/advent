#Clean data
data <- readLines("input5.txt")
seeds <- data[1] |> strsplit(" ") |> unlist() |> tail(-1) |> as.numeric()
data <- tail(data, -2)
data <- data |> strsplit(" ")

m <- c()
current <- ""
for(this in data){
  if(length(this) == 0) next
  if(grepl("\\D", this[1])){
    current <- this[1]
    next
  }
  thisrow <- c(this, current)
  m <- rbind(m, thisrow) 
}
df <- data.frame(m)
for(i in 1:3) df[, i] <- as.numeric(df[, i])
colnames(df) <- c("dest", "source", "length", "step")
v_steps <- unique(df$step)

## Part 1

onestep <- function(input, d){
  output <- input
  for(i in 1:nrow(d)){
    if(input >= d$source[i] & input < d$source[i] + d$length[i]){
      larger <- input - d$source[i]
      output <- d$dest[i] + larger
      return(output)
    }
  }
  return(output)
}

oneseed <- function(theseed){
  current <- theseed
  for(step in v_steps){
    current <- onestep(input=current, d=df[df$step == step, ])
  }
  return(current)
}

final <- sapply(seeds, oneseed)
min(final)

## Part 2

onestep2 <- function(input, d){
  output <- input
  for(i in 1:nrow(d)){
    if(min(input) > (d$source[i] + d$length[i]-1)|
       (max(input) < d$source[i])) next
    which_fit <- which(input >= d$source[i] &
                         input < d$source[i] + d$length[i])
    larger <- input[which_fit] - d$source[i]
    output[which_fit] <- d$dest[i] + larger
  }
  return(output)
}

oneseed2 <- function(theseed){
  current <- theseed
  for(step in v_steps){
    print(step)
    current <- onestep2(input=current, d=df[df$step == step, ])
  }
  return(current)
}

final2 <- c()
for(i in seq(1, length(seeds), by=2)){
  print(i)
  these_seeds <- seeds[i]:(seeds[i]+seeds[i+1]-1)
  final2 <- c(final2, oneseed2(these_seeds))
}
min(final2)

#46294175

