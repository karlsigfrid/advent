# Load and clean data

ascii <- read.csv("ascii-table.txt", sep="", header=F)
colnames(ascii) <- c("char", "dec", "hex")
li <- list()
for(i in 1:nrow(ascii)) li[[ascii$char[i]]] <- ascii$dec[i]

#data <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
data <- readLines("input15.txt")

d_list <- data |> strsplit(",") |> unlist() |> strsplit("")
res <- sapply(d_list, function(q){
  current <- 0
  for(this in q){
    current <- current + li[[this]]
    current <- current * 17
    current <- current %% 256
  }
  current
})
sum(res)


# Part 2

gethash <- function(s){
  v <- (strsplit(s, "=|-")[[1]][1]) |> strsplit("") |> unlist()
  current <- 0
  for(this in v){
    current <- current + li[[this]]
    current <- current * 17
    current <- current %% 256
  }
  current
}

data2 <- strsplit(data, ",")[[1]]

li2 <- lapply(1:256, function(q) list(label=NULL, lens=NULL))
for(this in data2){
  thebox <- gethash(this)
  thelabel <- gsub("(=|-).*", "", this)
  if(grepl("-", this)){
    currentslot <- which(li2[[thebox+1]]$label == thelabel) 
    if(length(currentslot) != 0){
      li2[[thebox+1]]$label <- li2[[thebox+1]]$label[-currentslot]
      li2[[thebox+1]]$lens <- li2[[thebox+1]]$lens[-currentslot]
    }
  }
  if(grepl("=", this)){
    thelens <- gsub(".+=", "", this) |> as.numeric()
    currentslot <- which(li2[[thebox+1]]$label == thelabel) 
    if(length(currentslot) != 0){
      li2[[thebox+1]]$lens[currentslot] <- thelens
    }
    if(length(currentslot) == 0){
      li2[[thebox+1]]$label <- c(li2[[thebox+1]]$label, thelabel)
      li2[[thebox+1]]$lens <- c(li2[[thebox+1]]$lens, thelens)
    }
  }
}

slot_lens_score <- sapply(li2, function(q){
  n <- length(q$label)
  if(n == 0) return(0)
  q$lens %*% (1:n)
})

slot_lens_score %*% (1:length(slot_lens_score))