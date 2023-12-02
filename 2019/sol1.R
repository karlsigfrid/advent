data <- readLines("input1.txt") |> as.numeric()

#part 1
data1 <- floor(data/3) - 2
sum(data1)

#part 2

fuel_sums <- c()
for (datai in data){
  this_fuel <- datai
  all_fuel_this <- c()
  repeat{
    this_fuel <- floor(this_fuel / 3) - 2
    if(this_fuel <= 0){
      break
    } else{
      all_fuel_this <- c(all_fuel_this, this_fuel)
    } 
  }
  fuel_sums <- c(fuel_sums, sum(all_fuel_this))
}
sum(fuel_sums)
  


