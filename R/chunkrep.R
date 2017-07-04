wrap <- function(x) {
	options(chunkrep.len=as.integer(getOption("chunkrep.len", 1000)))
	.Call(chunkrep_wrap, x)
}
