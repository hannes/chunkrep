#options(chunkrep.len=1000L)#, 
options(chunkrep.debug=T)
a <- 1:10^8
b <- chunkrep::wrap(a)
.Internal(inspect(b))

length(b)
head(b)
tail(b)

str(complete.cases(b))
str(complete.cases(b))

identical(a, b)
