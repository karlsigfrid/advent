library(dplyr)

v <- read.csv("input_2021_2.txt", sep=" ", header=FALSE)
colnames(v) <- c("direc", "value")


#v <- data.frame(direc=c("forward", "down", "forward", "up", "down", "forward"),
#               value=c(5,5,8,3,8,2))


the_sums <- v %>% group_by(direc) %>%
  summarize(sum=sum(value))

head(the_sums)
(the_sums$sum[1] - the_sums$sum[3]) * the_sums$sum[2]


# Part 2

horiz <- 0
depth <- 0
aim <- 0

for (i in 1:nrow(v)){
  if(v$direc[i] == "forward"){
    horiz <- horiz + v$value[i]
    depth <- depth + v$value[i] * aim
    }
  else if (v$direc[i] == "down"){
    aim <- aim + v$value[i]
    }
  else if (v$direc[i] == "up"){ 
    aim <- aim - v$value[i]
  }
}
horiz
depth
horiz * depth