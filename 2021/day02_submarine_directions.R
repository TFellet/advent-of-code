library(data.table)
a <- scan(fp(2021,2), sep=' ', what = list('a', 0L),quiet=T)
sum(a[[2]]*(a[[1]]=='down')-a[[2]]*(a[[1]]=='up'))*sum(a[[2]]*(a[[1]]=='forward')) # Part 1
ai<-cumsum(a[[2]]*(a[[1]]=='down')-a[[2]]*(a[[1]]=='up'))
fw<-a[[1]]=='forward'
sum(ai[fw]*a[[2]][fw])*sum(a[[2]][fw]) # Part 2
