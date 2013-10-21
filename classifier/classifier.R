library('png')
library('waveslim')
library('functional')
library('kknn')

path.to.data <- '../Data/rotated/Set2'

LoadData <- function(path, size=65536) {
#   data <- matrix(nrow=0, ncol=size)
  x <- ""
  data <- list()
  labels <- c()
  file.names <- dir(path=path)
  for(i in seq(1:length(file.names))) {
    path.to.img <- paste(path, file.names[i],sep="/")
#     features.vector <- as.vector(readPNG(path.to.img))
#     data <- rbind(data, features.vector)
    data[[i]] <- readPNG(path.to.img)
    labels[i] <- sub("[0-9].png", x, file.names[i])
  }
  list(data, labels)
}

Normalize <- function(data) {
  means <- apply(data, 2, mean)
  maxs <- apply(data, 2, max)
  sds <- apply(data, 2, sd)
  for(i in 1:dim(data)[1]) {
    for(j in 1:dim(data)[2]) {
      data[i,j] <- (data[i,j]-means[j])/sds[j]
    }
  }
#   data[!is.finite(data)] <- 0
#   data <- data[,apply(data, 2, Compose(is.finite, all))]
  data
}

ExtractFeatures <- function(img, wave.name, level) {
  img.dwt <- dwt.2d(img, wave.name, level)
  range <- 3*level+1
  features <- 1:range
  for(i in 1:range) {
    features[i] <- sd(as.vector(img.dwt[[i]]))
  }
  features
}

ConvertDataToFeatures <- function(data, wave.name, level) {
  featured.data <- list()
  for(i in 1:length(data)) {
    featured.data[[i]] <- ExtractFeatures(data[[i]], wave.name, level) 
  }
  featured.data
}

PrepareDataFrame <- function(featured.data, labels) {
  data <- data.frame()
  for(i in 1:length(featured.data)) {
    data <- rbind(data, featured.data[[i]])
  }
# Normalization   
  data <- Normalize(data)
  data <- cbind(data, labels)
# Shuffle data   
  data <- data[sample.int(nrow(data)),] 
  n <- dim(data)[2]
  colnames(data)[n] <- "Labels"
  data
}

DoKNN <- function(data, dist=2, kSupremum=100, folds.count=5) {
  m <- dim(data)[1]
  n <- dim(data)[2]-1
  
  step <- as.integer(m/folds.count)
  
  errorRates <- matrix(0, nrow=kSupremum, ncol=folds.count)
  
  for(j in seq(1,m,by=step)) {
    
    fold.index <- as.integer(j/step)+1
    
    if( j+step >= m ) {
      test.data <- data[j:m,]
      train.data <- data[1:(j-1),]
    }
    else if( j == 1 ) {
      test.data <- data[1:(j+step-1),]
      train.data <- data[(j+step):m,]
    }
    else {
      test.data <- data[j:(j+step-1),]
      train.data <- rbind(data[1:(j-1),],data[(j+step):m,])
    }
    
    for(i in 1:kSupremum) {
      kknn.result <- kknn(Labels~., train.data, test.data, distance=dist, k=i)
      summary(kknn.result)
      fit <- fitted(kknn.result)
      errorRate <- sum(as.integer(fit != test.data$Labels))/nrow(test.data)
      errorRates[i,fold.index] <- errorRate
    } 
  }
  print(errorRates)
  apply(errorRates, 1, mean)
}

both.data.labels <- LoadData(path.to.data)
data <- both.data.labels[[1]]
labels <- both.data.labels[[2]]
featured.data <- ConvertDataToFeatures(data, 'haar', 2)
data <- PrepareDataFrame(featured.data, labels)
euclid <- DoKNN(data, kSupremum=15)
plot(1:15, euclid, main="Error rate with respect to neighbours count", xlab="Neighbours count", ylab="Error rate", pch=15, col="green", ylim=c(0,1))