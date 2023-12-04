data <- readLines("input4.txt")
data <- gsub("^.+: +", "", data)
data <- gsub(" *[|] *", "|", data)
data <- trimws(data)
data <- gsub(" +", " ", data)
list1 <- strsplit(data, "[|]")

list2 <- lapply(list1, function(q){
  q2 <- strsplit(q, " ")
  q2 <- lapply(q2, as.numeric)
  names(q2) <- c("winner", "me")
  q2
})

# Part 1

num_wins <- sapply(list2, function(q) sum(q$me %in% q$winner))
wins <- num_wins[num_wins > 0]
sum(2^(wins - 1))

# Part 2

num_cards <- rep(1, length(num_wins))

for(i in 1:length(num_wins)){
  these_cards <- num_cards[i]
  these_winnings <- num_wins[i]
  if(these_winnings == 0) next
  add_to <- (i + 1):(i + these_winnings)
  num_cards[add_to] <- num_cards[add_to] + these_cards
}

sum(num_cards)