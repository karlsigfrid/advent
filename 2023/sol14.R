# Load and clean data

data <- readLines("input14.txt")
data <- gsub("[.]", "0", data)
data <- gsub("[#]", "1", data)
data <- gsub("O", "2", data)
data <- data |> strsplit("") |> lapply(as.numeric) |>
  data.frame() |> t()
data <- rbind(1, data)
rownames(data) <- NULL

# Part 1

d_old <- 0
d_new <- data

while(any(d_new != d_old)){
  d_old <- d_new
  rolling_index <- which(d_new == 2)
  above_ground <- d_new[rolling_index - 1]
  move_index <- rolling_index[above_ground == 0]
  d_new[move_index - 1] <- 2
  d_new[move_index] <- 0
}

rowSums(d_new == 2) %*% (nrow(d_new):1)


# Part 2

oneroll <- function(d_new, direction="N"){
  directions <- list(N=-1, S=1, E=nrow(d_new), W=-nrow(d_new))
  d_old <- 0
  while(any(d_new != d_old)){
    d_old <- d_new
    rolling_index <- which(d_new == 2)
    ahead_ground <- d_new[rolling_index + directions[[direction]]]
    move_index <- rolling_index[ahead_ground == 0]
    d_new[move_index + directions[[direction]]] <- 2
    d_new[move_index] <- 0
  }
  d_new
}

data2 <- rbind(data, 1)
data2 <- cbind(1, data2, 1)

d <- data2
d_list <- list()
repeat{
  print(length(d_list))
  d <- oneroll(d, direction="N")
  d <- oneroll(d, direction="W")
  d <- oneroll(d, direction="S")
  d <- oneroll(d, direction="E")
  isequal <- sapply(d_list, function(q) all(d == q))
  if(any(isequal)){
    firstrep <- which(isequal)
    cycle <- length(d_list) + 1 - firstrep 
    break
  }
  d_list <- c(d_list, list(d))
}

myoffset <- 10^9 %% cycle
i <- 0
while(myoffset + i * cycle < firstrep) i <- i + 1
d_final <- d_list[[myoffset + i * cycle]] 

d_final <- d_final[1:(nrow(d_final)-1), ]
rowSums(d_final == 2) %*% (nrow(d_final):1)
