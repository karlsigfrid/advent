# Clean data
data <- readLines("input7.txt")
data <- strsplit(data, " ")
df <- data %>% as.data.frame() |> t() |> data.frame()
colnames(df) <- c("hand", "winning")
df$winning <- df$winning |> as.numeric()
df$hand <- gsub("T", "a", df$hand)
df$hand <- gsub("J", "b", df$hand)
df$hand <- gsub("Q", "c", df$hand)
df$hand <- gsub("K", "d", df$hand)
df$hand <- gsub("A", "e", df$hand)
rownames(df) <- NULL
list_hands <- strsplit(df$hand, "")

## Part 1

types <- c("11111", "1112", "122", "113", "23", "14", "5")

df$type <- sapply(list_hands, function(q){
  this_type <- q |> table() |> sort() |> paste0(collapse="")
  which(types == this_type)
})

df$high <- NA
for(thistype in 1:7){
  thisindex <- which(df$type == thistype)
  if(length(thisindex) == 0) next
  sublist <- list_hands[thisindex]
  m <- sublist |> data.frame() |>t()
  thisorder <- order(m[, 1], m[, 2], m[, 3], m[, 4], m[, 5])
  thisrank <- sapply(1:length(thisorder), function(q) which(thisorder == q))
  df$high[thisindex] <- thisrank
}

df <- df[order(df$type, df$high), ]
sum(df$winning * 1:nrow(df))
