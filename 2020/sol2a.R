data <- readLines("input2a.txt")

data <- strsplit(data, split=" ")

data2 <- lapply(data, function(q){
  lowhigh <- strsplit(q[1], "-")[[1]]
  letter <- gsub(":", "", q[2])
  s <- q[3]
  c(lowhigh[1], lowhigh[2], letter, s)
  })

data2 <- as.data.frame(data2) |> t() |> data.frame()

data2$correct <- apply(data2, 1, function(q){
  num_let <- gregexpr(q[3], q[4])[[1]] |> length()
  tf <- num_let >= as.numeric(q[1]) & num_let <= as.numeric(q[2])
  tf
})

table(data2$correct)

data2$correct2 <- apply(data2, 1, function(q){
  first <- substr(q[4], as.numeric(q[1]), as.numeric(q[1]))
  second <- substr(q[4], as.numeric(q[2]), as.numeric(q[2]))
  thesum <- (first == q[3]) + (second == q[3])
  thesum == 1
})

table(data2$correct2)
