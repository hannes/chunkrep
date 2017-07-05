options(chunkrep.len=100L)
#options(chunkrep.debug=T)
stopifnot(identical(chunkrep::treesize(), 0L))

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

	stopifnot(chunkrep::treesize() > 0)

	str(complete.cases(b1))
	str(complete.cases(b1))


	stopifnot(identical(a, b))
	stopifnot(identical(a1, b1))

	b <- NULL
	b1 <- NULL
}
gc()

stopifnot(identical(chunkrep::treesize(), 0L))
