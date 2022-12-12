
good_neighbors <- function(this_row_col, s , t, down=FALSE){
  let_v <- c(1, 1:26, 26)
  names(let_v) <- c("S", letters, "E")
  this_letter <- s[this_row_col[1], this_row_col[2]]
  neighbors <-  this_row_col +
    matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), nrow=2)
  neighbors <- data.frame(row=neighbors[1, ], col= neighbors[2, ])
  neighbors <- neighbors[neighbors$row > 0 &
                           neighbors$row <= nrow(s) &
                           neighbors$col > 0 &
                           neighbors$col <= ncol(s), ]
  neighbors$is_na <- apply(neighbors, 1, function(q) as.integer(is.na(t[q["row"], q["col"]])))
  neighbors$letter <- apply(neighbors, 1, function(q) (s[q["row"], q["col"]]))
  neighbors$expand <- apply(neighbors, 1, function(q){
    ifelse(unlist(q["is_na"]) == 1 &
             (let_v[unlist(q["letter"])] - let_v[this_letter]) <= 1, 1, 0)
  })
  if(down){
    neighbors$expand <- apply(neighbors, 1, function(q){
    ifelse(unlist(q["is_na"]) == 1 &
             (let_v[unlist(q["letter"])] - let_v[this_letter]) >= -1, 1, 0)
  })
  }
  else{
    neighbors$expand <- apply(neighbors, 1, function(q){
      ifelse(unlist(q["is_na"]) == 1 &
               (let_v[unlist(q["letter"])] - let_v[this_letter]) <= 1, 1, 0)
    })
  }
  neighbors[neighbors$expand == 1, c("row", "col"), drop=F]
}

all_good_neighbors <- function(s, t, the_cycle, down=FALSE){
  last_loc <- which(t == the_cycle - 1, arr.ind = TRUE)
  new_locs <- c()
  for (i in 1:nrow(last_loc)){
    new_loc <- good_neighbors(this_row_col = last_loc[i, ], s=s, t=t, down=down)
    new_locs <- rbind(new_locs, new_loc)
  }
  new_locs <- unique(new_locs)
  new_locs
}

update_t <- function(new_locs, t, the_cycle){
  for(i in 1:nrow(new_locs)){
    t[new_locs$row[i], new_locs$col[i]] <- the_cycle
  }
  t
}

#Initialize

s <- readLines("data/12b.txt") |>
  strsplit(split="") |>
  (function(.) do.call(rbind, .))()


## Part 1
row_col_start <- which(s == "S", arr.ind=TRUE)
row_col_finish <- which(s == "E", arr.ind=TRUE)

t <- matrix(NA, nrow=nrow(s), ncol=ncol(s))
t[row_col_start] <- 0

the_cycle <- 1

#while(is.na(t[row_col_finish[1], row_col_finish[2]])){
while(any(s == "E" & is.na(t))){
  new_locs <- all_good_neighbors(s=s, t=t, the_cycle=the_cycle)
  t <- update_t(new_locs, t, the_cycle)
  the_cycle <- the_cycle + 1
}
print(the_cycle - 1)


## Part 2

row_col_end <- which(s == "a", arr.ind=TRUE)
row_col_start <- which(s == "E", arr.ind=TRUE)

t <- matrix(NA, nrow=nrow(s), ncol=ncol(s))
t[row_col_start] <- 0

the_cycle <- 1

repeat{
  new_locs <- all_good_neighbors(s=s, t=t, the_cycle=the_cycle, down=TRUE)
  t <- update_t(new_locs, t, the_cycle)
  if(any(s == "a" & !is.na(t))) break
  the_cycle <- the_cycle + 1
}
the_cycle