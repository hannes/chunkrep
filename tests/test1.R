a <- 1:10^8
b <- chunkrep::wrap(a)
.Internal(inspect(b))

length(b)
head(b)
tail(b)

dd <- complete.cases(b)
