# Clean data
data <- readLines("input9.txt")
list_orig <- strsplit(data, " ") |> lapply(as.numeric)

## Part 1

list_diff <- lapply(list_orig, function(q){
  v <- q
  li <- list(v)
  while(any(v != 0)){
    v <- diff(v)
    li <- c(li, list(v))
  }
  return(li)
})

list_append <- lapply(list_diff, function(q){
  li <- q
  nv <- length(li)
  append_this <- 0
  for(i in nv:1){
    li[[i]] <- c(li[[i]], tail(li[[i]], 1) + append_this)
    append_this <- tail(li[[i]], 1)
  }
  li
})

v_last <- sapply(list_append, function(q) tail(q[[1]], 1))
sum(v_last)


## Part 2

list_append_back <- lapply(list_diff, function(q){
  li <- q
  nv <- length(li)
  append_this <- 0
  for(i in nv:1){
    li[[i]] <- c(head(li[[i]], 1) - append_this, li[[i]])
    append_this <- head(li[[i]], 1)
  }
  li
})

v_first <- sapply(list_append_back, function(q) head(q[[1]], 1))
sum(v_first)
