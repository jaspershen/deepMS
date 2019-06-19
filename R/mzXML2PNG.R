map <- function(data) {
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
  # cat(paste("numrows is", numrows))
  plot <- matrix(0, ncol = length(data2)+1, nrow = numrows+1)
  for(i in 1:length(data2)) {
    mat <- data2[[i]]
    for(j in 1:dim(mat)[1]) {
      plot[mat[j, 1], i] <- mat[j, 2] ^ (1/12)
      # Or use this
      #plot[mat[j, 1], i] <- log10(mat[j, 2])
    }
  }
  return(plot)
}

createPNG <- function(first, last) {
  for(i in first:last) {
    input = paste(i, ".mzXML", sep = "")
    png(filename = paste(i, ".png", sep = ""), width = 256, height = 256)
    pheatmap::pheatmap(map(input), cluster_cols = FALSE, cluster_rows = FALSE, legend = FALSE)
    dev.off()
  }
}

convert <- function(wd = "c:/New Folder",
                    first = 1,
                    last = 10) {
  
  setwd(wd)
  createPNG(first, last)
}