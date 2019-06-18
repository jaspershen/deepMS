trans <- function(data,#mzXML format data
                  mz.range = c(50,1200),#mz range
                  mz.pixel = 100,#mz partion
                  rt.pixel = 100#rt partion
                  ){
  data1 <- mzR::openMSfile(data)
  data2 <- mzR::peaks(data1)
  rm(list="data1")
  ## build the rule of segment
  mz1 <- mz.range[1]
  mz2 <- mz.range[2]
  a <- seq(mz1, mz2, length.out = mz.pixel+1)
  b <- sort(c(a[1],a[length(a)],rep(a[-c(1,length(a))],2)))
  ##c is the rule of mz pixel
  c <- matrix(b, ncol = 2, byrow = TRUE)
  remove(list=c("a","b"))
  ###new data table for sample
  data.new <- matrix(0, ncol = length(data2), nrow = mz.pixel)

  #-----------------------------------------------------------------------------
  for (i in 1:length(data2)) {
    if(i %% 100 == 0){
      cat(paste(i, ""))
    }

  temp.data <- data2[[i]]
  temp.idx <- whichgrid(num = temp.data[,1], rule = c)

  for (j in 1:length(temp.idx)){
    data.new[temp.idx[j],i] <- sum(temp.data[j,2], data.new[temp.idx[j],i])
  }
  rm(list = c("temp.data","temp.idx"))
  }
  #-----------------------------------------------------------------------------
  return(data.new)
}



