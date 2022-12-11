# Prepare data
library(dplyr)

calc_m <- function(m, mult_all=NULL){
  newvals <- c()
  to <- c()
  for(old in m$items){
    newval <- eval(parse(text=m$oper))
    if(is.null(mult_all)) newval <- newval %/% 3
    if(!is.null(mult_all)) newval <- newval %% mult_all
    which_to <- ifelse(newval %% m$divis == 0, m$to[1], m$to[2])
    newvals <- c(newvals, newval)
    to <- c(to, which_to)
  }
  list(newvals=newvals, to=to, from=m$me)
}

update_m <- function(o, m_list){
  n <- length(o$newvals)
  if(n == 0) return(m_list)
  for(i in 1:n) m_list[[o$to[i]]][["items"]] <-
      c(m_list[[o$to[i]]][["items"]], o$newvals[i])
  m_list[[o$from]][["inspect"]] <- m_list[[o$from]][["inspect"]] + n
  m_list[[o$from]][["items"]] <- c()
  m_list
}

# Prepare data
s <- readLines("data/11b.txt") %>%
  gsub(".+: (.*)", "\\1", .) %>%
  gsub(".+(=|by|monkey) (.*)", "\\2", .)
m_list <- list()
n_m <- (length(s) + 1) / 7
start_r <- seq(2, length(s), by=7)
for (i in 1:n_m){
  m_list[[i]] <- list()
  m_list[[i]][["me"]] <- i
  m_list[[i]][["items"]] <- s[start_r[i]] %>% strsplit(split=", ") %>%
    unlist %>% as.numeric
  m_list[[i]][["oper"]] <- s[start_r[i]+1]
  m_list[[i]][["divis"]] <- s[start_r[i]+2] %>% as.numeric
  m_list[[i]][["to"]] <- c(s[start_r[i]+3], s[start_r[i]+4]) %>%
    as.numeric %>% "+"(1)
  m_list[[i]][["inspect"]] <- 0
}
mult_all <- sapply(m_list, function(q) q$divis) %>% prod

## Part 1

for(j in 1:20){
  for(i in 1:length(m_list)){
    o <- calc_m(m_list[[i]])
    m_list <- update_m(o=o, m_list=m_list)
  }
}

sapply(m_list, function(q) q$inspect) %>%
  sort(decreasing=T) %>% .[1:2] %>% prod


## Part 2 (rerun: prepare data)

for(j in 1:10000){
  for(i in 1:length(m_list)){
    o <- calc_m(m_list[[i]], mult_all=mult_all)
    m_list <- update_m(o=o, m_list=m_list)
  }
}

sapply(m_list, function(q) q$inspect) %>%
  sort(decreasing=T) %>% .[1:2] %>% prod
