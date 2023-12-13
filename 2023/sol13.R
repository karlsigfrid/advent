# Load and clean data

data <- readLines("input13.txt")
data <- gsub("#", "1", data)
data <- gsub("[.]", "0", data)
li <- list()
m <- c()
for(d in data){
  if (d == ""){
    li <- c(li, list(m))
    m <- c()
    next
  }
  thisrow <- strsplit(d, "")[[1]] |> as.numeric()
  m <- rbind(m, thisrow)
}

# Part 1

is_reflection_point_v <- function(index, m){
  nc <- ncol(m)
  col1 <- index
  col2 <- index + 1
  repeat{
    if (!all(m[, col1] == m[, col2])) return (FALSE)
    if (col1 == 1 | col2 == nc) return(TRUE)
    col1 <- col1 - 1
    col2 <- col2 + 1
  }
}

is_reflection_point_h <- function(index, m){
  nr <- nrow(m)
  row1 <- index
  row2 <- index + 1
  repeat{
    if (!all(m[row1, ] == m[row2, ])) return (FALSE)
    if (row1 == 1 | row2 == nr) return(TRUE)
    row1 <- row1 - 1
    row2 <- row2 + 1
  }
}

one_matrix <- function(m, old_point=list(0, "x")){
  for (i in 1:(ncol(m)-1)){
    if(is_reflection_point_v(index=i, m=m) &&
       !(i == old_point[[1]] & old_point[[2]] == "v")){
      return(data.frame(index=i, direction="v"))
    }
  }
  for (i in 1:(nrow(m)-1)){
    if(is_reflection_point_h(index=i, m=m) &&
       !(i == old_point[[1]] & old_point[[2]] == "h")){
      return(data.frame(index=i, direction="h"))
    }
  }
  return(NA)
}

df <- c()
for (item in li) df <- rbind(df, one_matrix(item))
df$mult <- ifelse(df$direction == "h", 100, 1)
df$index %*% df$mult

# Part 2

all_smudges <- function(i){
  item <- li[[i]]
  for(thisrow in 1:nrow(item)){
    for(thiscol in 1:ncol(item)){
      m <- item
      m[thisrow, thiscol] <- 1 - m[thisrow, thiscol]
      this_smudge <- one_matrix(m=m, old_point=list(df[i,1], df[i,2]))
      if (!is.na(this_smudge)[1]) return(this_smudge)
    }
  }
}
  
df2 <- c()
for (i in 1:length(li)) df2 <- rbind(df2, all_smudges(i))
df2$mult <- ifelse(df2$direction == "h", 100, 1)
df2$index %*% df2$mult
