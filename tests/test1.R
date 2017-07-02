a <- 1:10^9
b <- chunkrep::wrap(a)
.Internal(inspect(b))

length(b)
head(b)
tail(b)

complete.cases(b)
