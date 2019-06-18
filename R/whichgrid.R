whichgrid <- function(num = NULL,
                       rule = NULL){
  # browser()

  grid <- rep(NA, length(num))
  for(i in 1:length(num)){
    temp <- num[i]
    a <- temp-rule[,1]
    b <- rule[,2] - temp

    idx <- which(a > 0 & b > 0)
    if(length(idx) == 0) {idx = NA}
    grid[i] <- idx
  }
  return(grid)
}