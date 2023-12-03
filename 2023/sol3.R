# Part 1

make_diglist <- function(data_m){
  diglist <- list()
  for(thisrow in 1:nr){
    for(thiscol in 1:nc){
      if(grepl("\\d", data_m[thisrow, thiscol]) &&
         grepl("\\D", data_m[thisrow, thiscol-1])){
        remaining <- paste0(data_m[thisrow, thiscol:nc], collapse="")
        digval <- gsub("(\\d+).*$", "\\1", remaining) |> as.numeric()
        digcols <- thiscol:(thiscol+nchar(digval)-1)
        this_dig <- list(row=thisrow, col=digcols, value=digval)
        diglist <- c(diglist, list(this_dig))
      }
    }
  }
  diglist
}

adjecent_symbol <- function(diglist, data_m){
  for (i in 1:length(diglist)){
    this <- diglist[[i]]
    symbols1 <- data_m[this$row, c(head(this$col, 1)-1, tail(this$col, 1)+1)]
    symbols2 <- data_m[c(this$row-1, this$row+1),
                     (head(this$col, 1)-1):(tail(this$col, 1)+1)]
    allsymbols <- c(symbols1, symbols2)
    diglist[[i]]$symbol <- !all(grepl("[.0-9]", allsymbols))
  }
  diglist
}

data <- readLines("input3.txt")
data_m <- data |> strsplit("") |> data.frame() |> t() |> as.matrix()
rownames(data_m) <- NULL
data_m <- cbind(".", data_m, ".")
data_m <- rbind(".", data_m, ".")
nr <- nrow(data_m)
nc <- ncol(data_m)
diglist <- make_diglist(data_m)
diglist <- adjecent_symbol(diglist, data_m)

usable_values <- sapply(diglist, function(q){
  ifelse(q$symbol, q$value, 0)
})
sum(usable_values)

#Part 2

find_stars <- function(data_m){
  stars <- c()
  for(thisrow in 1:nr){
    for(thiscol in 1:nc){
      if(grepl("[*]", data_m[thisrow, thiscol])){
        stars <- rbind(stars, c(thisrow, thiscol))
      }
    }
  }
  stars
}

find_gears <- function(stars, diglist){
  all_gears <- c()
  for(i in 1:nrow(stars)){
    this <- stars[i, ]
    adjecent_digs <- c()
    for (digs in diglist){
      if (this[1] %in% (digs$row-1):(digs$row+1) &&
          this[2] %in% (head(digs$col,1)-1):(tail(digs$col,1)+1)){
        adjecent_digs <- c(adjecent_digs, digs$value)
      }
    }
    if(length(adjecent_digs) == 2) all_gears <-
      c(all_gears, prod(adjecent_digs))
  }
  all_gears
}

stars <- find_stars(data_m)
all_gears <- find_gears(stars, diglist)
sum(all_gears)
