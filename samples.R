download.file("http://www.greenmountaindiapers.com/skin/common_files/modules/Socialize/images/twitter.png",
              destfile = "twitter.png")
library(randomForest)
library(dslabs)
mnist <- read_mnist()

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

image(matrix(rev(mnist$train$images[3,]), 28, 28))

library(imager)

xy <- load.image("COVID-19.png")
head(as.data.frame(xy))
thmb <- resize(xy,round(width(xy)/10),round(height(xy)/10))
plot(xy,main="Thumbnail")

test <- resize(xy,round(width(xy)/10),round(height(xy)/10))
plot(xy > .5,main="Thumbnail")

test2 <- as.array(xy)

resizePixels = function(im, w, h) {
  pixels = as.vector(im)
  # initial width/height
  w1 = nrow(im)
  h1 = ncol(im)
  # target width/height
  w2 = w
  h2 = h
  # Create empty vector
  temp = vector('numeric', w2*h2)
  # Compute ratios
  x_ratio = w1/w2
  y_ratio = h1/h2
  # Do resizing
  for (i in 0:(h2-1)) {
    for (j in 0:(w2-1)) {
      px = floor(j*x_ratio)
      py = floor(i*y_ratio)
      temp[(i*w2)+j] = pixels[(py*w1)+px]
    }
  }
  
  m = matrix(temp, h2, w2)
  return(m)
}
thmb <- resizePixels(xy, 28, 28)
plot(thmb, main="Thumbnail")
image(array(thmb), 28, 28)
