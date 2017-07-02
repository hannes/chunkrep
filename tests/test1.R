a <- 1:10^8
b <- chunkrep::wrap(a)
.Internal(inspect(b))

length(b)
head(b)
tail(b)

str(complete.cases(b))
