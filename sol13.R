transform_input <- function(s){
  s <- gsub("\\[", "list(", s)
  s <- gsub("\\]", ")", s)
  eval(parse(text=s))
}

first <- function(a, b){
  na <- length(a)
  nb <- length(b)
  
  #If one or both are empty
  if (na == 0 & nb == 0) return(0)
  if (na == 0 & nb > 0) return (1)
  if (na > 0 & nb == 0) return (2)
  
  #If one element is a list and the other not
  for (i in 1:min(na, nb)){
    if (class(a[[i]]) == "list" & class(b[[i]]) != "list"){
      b[[i]] <- list(b[[i]])
    }
    if (class(a[[i]]) != "list" & class(b[[i]]) == "list"){
      a[[i]] <- list(a[[i]])
    }

    #Compare to lists
    if (class(a[[i]]) == "list"){
      res <- first(a=a[[i]], b=b[[i]])
      if(res %in% 1:2) return (res)
    }
    else{
    if (a[[i]] < b[[i]]) return(1)
    if (a[[i]] > b[[i]]) return(2)
    }
  }
  if (na < nb) return (1)
  if (na > nb) return (2)
  return(0)
}
  

# Part 1

s <- readLines("data/13b.txt")
a <- lapply(seq(1, length(s), by=3), function(q) transform_input(s[q]))
b <- lapply(seq(2, length(s), by=3), function(q) transform_input(s[q]))

res <- sapply(1:length(a), function(q) first(a[[q]], b[[q]]))
which(res == 1) |> sum()


# Part 2

ab <- c(a, b, list(list(list(2))), list(list(list(6))))

num_first <- c()
for (i in 1:length(ab)){
  res <- sapply(1:length(ab), function(q) first(ab[[i]], ab[[q]]))
  num_first <- c(num_first, sum(res == 1))
}

ordered_ab <- ab[order(num_first, decreasing = T)]

for (i in 1:length(ordered_ab)){
  if(identical(ordered_ab[[i]], list(list(2)))) index2 <- i
  if(identical(ordered_ab[[i]], list(list(6)))) index6 <- i
}

index2 * index6
