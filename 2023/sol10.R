# Clean data
data <- readLines("input10_test2.txt") |>
  strsplit("") |> data.frame() |> t()
#which(data == "S", arr.ind=T)
#data[18:20, 104:106]
# We can start down in both the example and with the real data.


# Part 1

v <- c("|NNSS", "-EEWW", "LSEWN", "JENSW", "7ESNW", "FNEWS")

li <- lapply(v, function(q){
  item <- list()
  item[[substr(q, 2, 2)]] = substr(q, 3, 3)
  item[[substr(q, 4, 4)]] = substr(q, 5, 5)
  item
})

names(li) <- sapply(v, function(q) substr(q, 1, 1))
dir_all <- list(S=c(1, 0), N=c(-1, 0), E=c(0, 1), W=c(0, -1))

this_pos <- which(data == "S", arr.ind=T) |> c()
all_pos <- this_pos
this_pos <- this_pos + dir_all[["S"]]
all_pos <- rbind(all_pos, this_pos)
dir_prev <- "S"
repeat{
  this_pipe <- data[this_pos[1], this_pos[2]]
  if(this_pipe == "S") break
  dir_next <- li[[this_pipe]][[dir_prev]]
  this_pos <- this_pos + dir_all[[dir_next]]
  all_pos <- rbind(all_pos, this_pos)
  dir_prev <- dir_next
}

ceiling((nrow(all_pos) - 2) / 2)
