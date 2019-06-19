map <- function(data, mz.range, mz.pixel, rt.pixel) {
  #
  # Args:
  #   data: mzXML format data that will be displayed as heat map
  #
  # Returns:
  #   New data matrix that is used in generating heatmap
  data1 <- mzR::openMSfile(data)
  data2 <- mzR::peaks(data1)
  rm(data1)
  numrows <- 0
  for(i in 1:length(data2)) {
    for(j in 1:dim(data2[[i]])[1]) {
      numrows = max(numrows, data2[[i]][j, 1])
    }
  }
  
  cat(paste("numrows is", numrows))
  plot <- matrix(0, ncol = length(data2)+1, nrow = numrows+1)
  for(i in 1:length(data2)) {
    mat <- data2[[i]]
    for(j in 1:dim(mat)[1]) {
      plot[mat[j, 1], i] <- mat[j, 2] ** (1/10)
      # Or use this
      #plot[mat[j, 1], i] <- log10(mat[j, 2])
    }
  }
  pheatmap::pheatmap(plot, cluster_cols = FALSE, cluster_rows = FALSE)
  return(plot)
}

