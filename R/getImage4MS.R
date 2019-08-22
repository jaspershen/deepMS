data <- list.files(path = ".",
                   pattern = '\\.(mz[X]{0,1}ML|cdf)',
                   recursive = TRUE)


# setGeneric(name = "getImage4MS", 
#            def = function(
#              data,
#              path = ".",
#              threads = 3,
             mz.range = c(50, 1200)#mz range
             rt.range = c(60, 900)
#              mz.pixel = 256,#mz partion
#              rt.pixel = 256#rt partion
#            ){
             data.name <- stringr::str_replace(string = data, 
                                               pattern = ".mzXML", 
                                               replacement = "")
             data1 <- mzR::openMSfile(data)
             peaks <- ProtGenerics::peaks(object = data1)
             # rm(list = c("data1"))
             data2 <- MSnbase::readMSData(files = data, 
                                          msLevel. = 1, 
                                          mode = "onDisk", 
                                          verbose = TRUE)
             rt <- ProtGenerics::rtime(object = data2)
             # rm(list = c("data2"))
              
             remain.idx <- which(rt > rt.range[1] & rt < rt.range[2])
             peaks <- peaks[remain.idx]
             rt <- rt[remain.idx]
             
             ##log10 intensity
             peaks <- lapply(peaks, function(x){
               x[,2] <- log(x[,2] + 1, 10)
               x
             })
             
             library(magrittr)
             lapply(peaks, function(x){
               nrow(x)
             }) %>% 
               unlist() %>% 
               sum()
              
             sxtTools::ms2Plot(spectrum1 = peaks[[1000]])
             
             
             ##a demo
             peaks2 <- peaks[sort(sample(1:length(peaks), 100))]
             
             peaks2 <- 
               mapply(function(x, y){
               cbind(x, y)
             },
             x = peaks2,
             y = 1:length(peaks2))
             
             peaks2 <- do.call(rbind, peaks2)
             
             colnames(peaks2) <- c("mz", "intensity", "rt")
             
             library(ggplot2)
             peaks2 <- as.data.frame(peaks2)
             
             ggplot(peaks2, aes(x = rt, y = mz, colour = intensity)) +
               geom_bin2d() +
               scale_fill_gradient(low = "blue", high = "red") + 
               theme_bw()
             
             ggplot(peaks2, aes(x = rt, y = mz, colour = intensity)) +
               geom_point() +
               scale_colour_gradient(low = "blue", high = "red") +
               theme_bw()
             
             library(tidyverse)
             data.new <- 
               as.data.frame(data.new)
             data.new2 <- 
               data.new
             
             colnames(data.new2) <-
               paste('RT', 1:ncol(data.new2), sep = "")
             
             rownames(data.new2) <-
               paste('mz', 1:nrow(data.new2), sep = "")
             
             data.new2 <- 
               data.new %>% 
               rownames_to_column()
             
             data.new2 <- 
             data.new2 %>% 
               gather(key = "rt", value = "value", V1:V100)
             
             
             ##get the mz range
             temp <- unique(unlist(lapply(peaks, function(x){
               range(x[,1])
             })))
             mz.range2 <- range(temp)
             rm(list = c("temp"))
             
             mz.range[1] <- ifelse(mz.range[1] > mz.range2[1], mz.range[1], mz.range2[1])
             mz.range[2] <- ifelse(mz.range[2] < mz.range2[2], mz.range[2], mz.range2[2])
             
             ## build the rule of segment
             mz1 <- mz.range[1]
             mz2 <- mz.range[2]
             
             a <- seq(mz1, mz2, length.out = mz.pixel + 1)
             b <- a[-1]
             a <- a[-length(a)]
             rule <- data.frame(a, b, stringsAsFactors = FALSE)
             rule[1,1] <- rule[1,1] - 0.000001
             rule[nrow(rule),2] <- rule[nrow(rule),2] + 0.000001
             
             ##c is the rule of mz pixel
             remove(list = c("a","b"))
             ###new data table for sample
             data.new <- matrix(0, ncol = length(rt), 
                                nrow = mz.pixel)

             new.peaks <- pbapply::pblapply(seq_along(rt), function(idx){
               binPeak(peak = peaks[[idx]], rule = rule)
             })
             new.peaks <- lapply(new.peaks, function(x){
               x[, -1, drop = FALSE]
             })
             new.peaks <- do.call(cbind, new.peaks)
             colnames(new.peaks) <- rt
             
             ###RT seperation
             ##get the rt range
             rt.range <- range(as.numeric(colnames(new.peaks)))
             ## build the rule of segment
             rt1 <- rt.range[1]
             rt2 <- rt.range[2]
             
             a <- seq(rt1, rt2, length.out = rt.pixel + 1)
             b <- a[-1]
             a <- a[-length(a)]
             rule <- data.frame(a, b, stringsAsFactors = FALSE)
             rule[1,1] <- rule[1,1] - 0.000001
             rule[nrow(rule),2] <- rule[nrow(rule),2] + 0.000001
             
             ##c is the rule of rt pixel
             remove(list = c("a","b"))
             
             temp.idx <- sapply(as.numeric(colnames(new.peaks)), function(rt){
               whichGrid(mz = rt, rule = rule)
             })
             
             unique.idx <- 1:rt.pixel
           
             return.peak <- lapply(unique.idx, function(x){
               idx <- which(temp.idx == x)
               if(length(idx) == 0){
                 return(rep(0, nrow(new.peaks)))
               }
               
               temp.peak <- new.peaks[,idx,drop = FALSE]
               temp.peak <- apply(temp.peak, 1, sum)
               temp.peak
             })
             
           
             return.peak <- do.call(cbind, return.peak)  
             
             png(filename = file.path(path, paste(data.name, ".png", sep = "")),
                 width = rt.pixel, height = mz.pixel)
             
             par(mar = c(0,0,0,0))
               plot <- pheatmap::pheatmap(mat = return.peak,  
                                        # color = colorRampPalette(c("white", "red"))(10000),
                                        cluster_cols = FALSE, 
                                        cluster_rows = FALSE, 
                                  border_color = NA, legend = FALSE)
               plot
             dev.off()
           # })






