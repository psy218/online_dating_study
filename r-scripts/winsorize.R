winsorize <- function(x, q = 0.05, upperonly) {
  extrema <- quantile(x, c(q, 1-q), na.rm = T)	
  
  if (upperonly == T) {
    x[x>extrema[2]] <- extrema[2]   
    
  } else {
    x[x<extrema[1]] <- extrema[1] 
    x[x>extrema[2]] <- extrema[2] 
  }
  return(x)
}
