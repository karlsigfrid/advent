# Clean data
data <- readLines("input16.txt") |>
  strsplit("") |> data.frame() |> t()
rownames(data) <- NULL
data <- cbind("#", data, "#")
data <- rbind("#", data, "#")
nr <- nrow(data)
nc <- ncol(data)

li <- list()
li[["."]] <- list(E="E", W="W", S="S", N="N")
li[["/"]] <- list(E="N", W="S", S="W", N="E")
li[["\\"]] <- list(E="S", W="N", S="E", N="W")
li[["|"]] <- list(E=c("S", "N"), W=c("S", "N"), S="S", N="N")
li[["-"]] <- list(S=c("E", "W"), N=c("E", "W"), E="E", W="W")

dir_all <- list(S=c(1, 0), N=c(-1, 0), E=c(0, 1), W=c(0, -1))
dir_to_layer <- list(E=1, W=2, S=3, N=4)

one_start <- function(this_row, this_col, dir_prev){
  this_pos <- c(this_row, this_col)
  all_pos <- array(data=0, dim=c(nr, nc, 4))
  the_queue <- list()

  repeat{
    this_pipe <- data[this_pos[1], this_pos[2]]
    if(this_pipe =="#" ||
       all_pos[this_pos[1], this_pos[2], dir_to_layer[[dir_prev]]] == 1){
      if(length(the_queue) == 0) break
      this_pos <- the_queue[[1]]$pos
      dir_prev <- the_queue[[1]]$dir
      the_queue <- the_queue[-1]
      next
    }
    all_pos[this_pos[1], this_pos[2], dir_to_layer[[dir_prev]]] <- 1
    dir_next <- li[[this_pipe]][[dir_prev]]
    if(length(dir_next) == 2){
      the_queue <- c(the_queue,
                 list(list(pos=this_pos + dir_all[[dir_next[1]]],
                           dir=dir_next[1])))
      dir_next <- dir_next[[2]]
    }
    this_pos <- this_pos + dir_all[[dir_next]]
    dir_prev <- dir_next
  }

  sumpos <- all_pos[,,1] +all_pos[,,2]+all_pos[,,3]+all_pos[,,4]
  sum(sumpos > 0)
}

#Part 1

one_start(this_row = 2, this_col = 2, dir_prev = "E")

#Part 2

allrows <- 2:(nr-1)
allcols <- 2:(nc-1)

all_starts <- rbind(cbind(allrows, 2, "E"),
                    cbind(allrows, nc-1, "W"),
                    cbind(2, allcols, "S"),
                    cbind(nr-1, allcols, "N")) |>
  as.data.frame()
all_starts[, 1] <- as.numeric(all_starts[, 1])
all_starts[, 2] <- as.numeric(all_starts[, 2])
colnames(all_starts) <- c("row", "col", "dir")

res <- sapply(1:nrow(all_starts), function(q){
  print(all_starts[q, ])
  one_start(this_row = all_starts[q, 1],
            this_col = all_starts[q, 2],
            dir_prev = all_starts[q, 3])
  })

max(res)