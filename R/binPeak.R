binPeak <- function(peak,
                    rule){
  setGeneric(name = "whichGrid",
             def = function(mz, rule){
               which(mz - rule[,1] >= 0 &
                       mz - rule[,2] < 0)[1]
             })
  peak <- peak[order(peak[,1]),,drop = FALSE]
  new.peak <- matrix(0, ncol = 2, nrow = nrow(rule))
  new.peak[,1] <- 1:nrow(new.peak) 
  colnames(new.peak) <- c("index", "intensity")
  temp.idx <- sapply(peak[,1], function(mz){
    whichGrid(mz = mz, rule = rule)
  })
  
  peak <- data.frame(temp.idx, peak, stringsAsFactors = FALSE)
  
  temp <- plyr::ddply(.data = peak, .variables = plyr::.(temp.idx), 
                      .fun = plyr::summarize,
              intensity = sum(X2))
  
  
  new.peak[,2][match(temp[,1], new.peak[,1])] <- temp[,2]
  
  return(new.peak)
}

