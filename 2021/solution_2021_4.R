library(dplyr)

#Make the draw vector and the game boards
s <- readLines("input_2021_4b.txt")
s

draws <- s[1] %>% strsplit(",") %>% unlist %>% as.numeric

ms <- tail(s, -2) %>% strsplit(" ") %>%
  lapply(function(q) as.numeric(q[nchar(q) > 0])) %>%
  do.call(rbind, .)

boards <- lapply(seq(1, nrow(ms), by=5), function(q) ms[q:(q+4), ])

# Part 1

for(i in 5:length(draws)){
  for (this_board in boards){
    tf_board <- (this_board %in% draws[1:i]) %>%
      matrix(ncol=5)
    if (any(c(rowSums(tf_board), colSums(tf_board)) == 5)){
      unmarked <- sum(this_board[!tf_board])
      print(unmarked * draws[i])
      stop()
    }
  }
}


# Part 2

winning_boards <- c()
for(i in 5:length(draws)){
  for (boardnum in 1:length(boards)){
    if(boardnum %in% winning_boards) next
    tf_board <- (boards[[boardnum]] %in% draws[1:i]) %>%
      matrix(ncol=5)
    if (any(c(rowSums(tf_board), colSums(tf_board)) == 5)){
      winning_boards <- c(winning_boards, boardnum)
      if(all(1:length(boards) %in% winning_boards)){
        unmarked <- sum(boards[[boardnum]][!tf_board])
        print(unmarked * draws[i])
        stop()
      }
    }
  }
}

