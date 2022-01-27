library(data.table)
a <- scan(fp(2021,1),what=0L,quiet=T)
sum(shift(a,-1L,fill=0L)>a) # 20µs # Difference between current and previous, fill with 0
sum(shift(a,-3L,fill=0L)>a) # 20µs # Difference between n and n-3 
