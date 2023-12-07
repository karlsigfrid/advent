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
df2 <- df
list_hands <- strsplit(df$hand, "")

## Part 1

card_ranking <- function(list_hands){
  m <- list_hands |> data.frame() |> t()
  thisorder <- order(m[, 1], m[, 2], m[, 3], m[, 4], m[, 5])
  high <- sapply(1:length(thisorder), function(q) which(thisorder == q))
  return(high)
}

types <- c("11111", "1112", "122", "113", "23", "14", "5")

df$type <- sapply(list_hands, function(q){
  this_type <- q |> table() |> sort() |> paste0(collapse="")
  which(types == this_type)
})

df$high <- card_ranking(list_hands=list_hands)
df <- df[order(df$type, df$high), ]
sum(df$winning * 1:nrow(df))

## Part 2

df <- df2
list_hands1 <- list_hands
list_hands2 <- list_hands

list_hands2 <- lapply(list_hands2, function(q) gsub("b", "1", q))

for (i in 1:nrow(df)){
  best_card <- list_hands1[[i]][list_hands1[[i]] != "b"] |>
  table() |> sort(decreasing = T) |> names() |> head(1)
  if(length(best_card) == 0) best_card <- "e"
  list_hands1[[i]][list_hands1[[i]] == "b"] <- best_card
}

df$type <- sapply(list_hands1, function(q){
  this_type <- q |> table() |> sort() |> paste0(collapse="")
  which(types == this_type)
})

df$high <- card_ranking(list_hands=list_hands2)
df <- df[order(df$type, df$high), ]
sum(df$winning * 1:nrow(df))
