options(chunkrep.len=100L)
options(chunkrep.debug=T)


for (i in 1:3){
	print(i)
	a <- 1:10^7
	a1 <- 10^7:1
	b <- chunkrep::wrap(a)
	b1 <- chunkrep::wrap(a1)

	.Internal(inspect(b))
	.Internal(inspect(b1))

	length(b)
	head(b)
	tail(b)

	str(complete.cases(b))
	str(complete.cases(b))

	str(complete.cases(b1))
	str(complete.cases(b1))

	identical(a, b)
	identical(a1, b1)

	b <- NULL
	b1 <- NULL
}
