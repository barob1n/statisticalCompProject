list_args <- Vectorize( function(a,b) c( as.list(a), as.list(b)), SIMPLIFY = FALSE)

make_args_mtx <- function( alist ) {
  Reduce(function(x,y) outer(x,y, list_args), alist)
}

multi.outer <- function(f, ...) {
  args <- make_args_mtx(list(...))
  apply(args, 1:length(dim(args)), function(a) do.call(f, a[[1]] ) )
}

fun <- function(a,b,c) paste(a,b,c)

ans <- multi.outer(fun, LETTERS[1:2], c(3,4,5), letters[6:7])

print(ans)