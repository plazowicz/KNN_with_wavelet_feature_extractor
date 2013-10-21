library('png')
library('waveslim')
library('functional')

path.to.data <- '../Data/rotated/Set1'

LoadData <- function(path, size=65536) {
#   data <- matrix(nrow=0, ncol=size)
  data <- list()
  labels <- list()
  file.names <- dir(path=path)
  for(i in seq(1:length(file.names))) {
    path.to.img <- paste(path, file.names[i],sep="/")
#     features.vector <- as.vector(readPNG(path.to.img))
#     data <- rbind(data, features.vector)
    data[[i]] <- readPNG(path.to.img)
    labels[[i]] <- sub("[0-9].png", x, file.names[i])
  }
  list(data, labels)
}

Normalize <- function(data) {
  means <- apply(data, 2, mean)
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  for(i in 1:dim(data)[1]) {
    for(j in 1:dim(data)[2]) {
      data[i,j] <- (data[i,j]-means[j])/(maxs[j]-mins[j])
    }
  }
#   data[!is.finite(data)] <- 0
  data <- data[,apply(data, 2, Compose(is.finite, all))]
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

both.data.labels <- LoadData(path.to.data)
data <- both.data.labels[[1]]
labels <- both.data.labels[[2]]
featured.data <- ConvertDataToFeatures(data, 'haar', 2)

