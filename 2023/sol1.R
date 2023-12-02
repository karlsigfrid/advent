data <- readLines("input1.txt")

## Part 1
values <- sapply(data, function(q){
  digs <- gsub("\\D", "", q) |> strsplit("") |> unlist()
  first <- digs[1]
  last <- digs[length(digs)]
  c(first, last) |> paste0(collapse="") |> as.numeric()
})
sum(values)

## Part2
mydigits <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

data2 <- sapply(data, function(q){
  newq <- q
  pos <- list()
  for(i in 1:9) pos[[i]] <- gregexpr(mydigits[i], newq) |> unlist()
  for(i in 1:9){
    if(pos[[i]][1] != -1){
      for (this in pos[[i]]){
        substr(newq, this, this) <- as.character(i)
      }
    }
  }
  newq
})

values <- sapply(data2, function(q){
  digs <- gsub("\\D", "", q) |> strsplit("") |> unlist()
  first <- digs[1]
  last <- digs[length(digs)]
  c(first, last) |> paste0(collapse="") |> as.numeric()
})
sum(values)
