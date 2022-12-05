a <- strtoi(rfp('2019','1'))
fuel <- a %/% 3L - 2L # Fuel needed to transport each module
total <- sum(fuel) # Total mass of fuel
total # Part 1 (3297866): Fuel needed to transport all modules
while(length(fuel)>0L) {
  fuel <- fuel[fuel > 8L] %/% 3L - 2L # Fuel needed to transport previous fuel
  total <- total + sum(fuel) # Add to total
}
total # Part 2 (4943923): Fuel needed to transport all modules and fuel
