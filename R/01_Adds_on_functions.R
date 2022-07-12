# Trace of a matrix
tr <- function(M){ sum(diag(M))}

# True empirical variance (!= estimation variance)
var <- function(x, y = NULL, na.rm = TRUE, use){
  if (missing(use)) 
    use <- if (na.rm) 
      "na.or.complete"
  else "everything"
  tmp <- stats:::var(x=x,y=y,use=use)
  N <- NROW(x)
  tmp*(N-1)/N
}  

# True empirical standard deviation
f.sd <- function(x,na.rm=T){
  sqrt(f.var(x,na.rm=na.rm))
}




