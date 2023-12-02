one_round <- function(df){
  suppressMessages(attach(df))
  for(i in 1:nrow(df)){
    if(!is.na(tot[i])) next
    m1tot <- tot[m0 == m1[i]]
    m2tot <- tot[m0 == m2[i]]
    if(is.na(m1tot) | is.na(m2tot)) next
    df$tot[i] <- eval(parse(text=paste("m1tot", op[i], "m2tot")))
  }
  detach(df)
  df
}

#Prepare the data
library(dplyr)

init_df <- function(){
  df <- readLines("data/21b.txt") %>%
  gsub(":", "", .) %>%
  strsplit(" ") %>%
  lapply(function(q){
    if(length(q) == 2) return(c(q[1], NA, NA, NA, q[2]))
    c(q, NA)
  }) %>%
  do.call(rbind, .) %>%
  data.frame %>%
  rename(m0=X1, m1=X2, m2=X4, op=X3,  tot=X5) %>%
  mutate(tot=as.numeric(tot))
  df
}

# Part 1
df <- init_df()
root_index <- which(df$m0 == "root")
while(is.na(df$tot[root_index])){
  df <- one_round(df=df)
}

format(df$tot[root_index], digits=22)

f_humn <- function(h){
  df <- init_df()
  df$tot[humn_index] <- h
  root_index <- which(df$m0 == "root")
  while(is.na(df$tot[root_index])){
    df <- one_round(df=df)
  }
  m1_index <- which(df$m0 == df$m1[root_index])
  m2_index <- which(df$m0 == df$m2[root_index])
  abs(df$tot[m1_index] - df$tot[m2_index])
}

#f_humn2 <- function(h) sapply(h, f_humn)

# Part 2
f_humn(h=500)
humn_index <- which(df$m0 == "humn")
optim(par=df$tot[humn_index], fn=f_humn, gr=NULL)

optimize(f=f_humn, interval=c(0, 10^6))


