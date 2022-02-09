a <- strtoi(rfp('2020','1'))

goal <- 2020L

all <- \() {
  g2 <- goal/2
  sup <- a>g2
  d <- a[!sup] # Possible values under goal / 2
  d2 <- a[sup] # Possible values above goal / 2
  gd2 <- goal-d2 # New goal to obtain
  n1 <- d[d%in%gd2] # If number == new goal, sum is possible
  
  comb_inf <- t(Rfast::comb_n(d, 2)) # Possibles combinations for values under goal/2
  comb_inf_sums <- comb_inf[,1] + comb_inf[,2] # Possibles sums
  under <- comb_inf_sums[comb_inf_sums < g2] # Possibles sums under half goal
  s1 <- under[under%in%gd2] # Which sum is present in difference
  n1n2 <- comb_inf[s1==comb_inf_sums] # Find originals numbers
  c(n1*(goal-n1), prod(n1n2, goal-s1)) # Return product of the 2 numbers that add up to goal, and the same for 3 numbers
}
all()
