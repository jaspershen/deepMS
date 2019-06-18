trans <- function(data, mz.range, mz.pixel, rt.pixel) {
  #
  # Args:
  #   data: mzXML format data
  #   mz.range: mz range
  #   mz.pixel: mz partion
  #   rt.pixel: rt partion
  #
  # Returns:
  #   New data matrix
  data1 <- mzR::openMSfile(data)
  data2 <- mzR::peaks(data1)
  rm(data1)
  # Build the rule of segment
  mz1 <- mz.range[1]
  mz2 <- mz.range[2]
  a <- seq(mz1, mz2, length.out = mz.pixel + 1)
  b <- a[1]
  for (i in 2:(length(a) - 1)) {
    b = c(b, a[i], a[i])
  }
  b = c(b, a[length(a)])
  # c is the rule of mz pixel
  c <- matrix(b, ncol = 2, byrow = TRUE)
  rm(a, b)
  # New data table for sample
  data.new <- matrix(0, ncol = length(data2), nrow = mz.pixel)

  #-----------------------------------------------------------------------------
  for (i in 1:length(data2)) {
    if (i %% 100 == 0){
      cat(paste(i, ""))
    }
    
    temp.data <- data2[[i]]
    temp.idx <- whichgrid(num = temp.data[, 1], rule = c)

    for (j in 1:length(temp.idx)){
      data.new[temp.idx[j], i] <- sum(temp.data[j, 2], data.new[temp.idx[j], i])
    }
    rm(temp.data, temp.idx)
  }
  #-----------------------------------------------------------------------------
  return(data.new)
}
