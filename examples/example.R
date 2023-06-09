image(1:28, 1:28, matrix(mnist$test$images[16,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")


x_r = U %*% y
x_r_t = t(x_r)
dim(x_r_t)

for(i in 1:150){
image(1:28, 1:28, matrix(x_r_t[i,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
}

mnist = dslabs::read_mnist("/home/zl22291@bristol.ac.uk/Documents/Projects/onlinePCA/data")
mnist_7 = mnist$test$images[mnist$test$labels == 7, ]
nrow(mnist_7)

mnist_7_opca = onlinePCA::algorithm1(x = mnist_7, k = 5, eps = 1)

xr = (mnist_7_opca[[1]])
U = (mnist_7_opca[[2]])

mnist_opca = onlinePCA::algorithm1(x = mnist$train$images[1:500,], k = 5, eps = 0.35)
mnist_opca = onlinePCA::algorithm1(x = mnist$train$images[1:500,], k = 5, eps = 10)
xr = (mnist_opca[[1]])
U = (mnist_opca[[2]])

x_r_t = t(xr)
dim(x_r_t)

###################################################
library(animation)
library(magick)

png(file="examples/images/example%02d.png", width=200, height=200)
for (i in 1:nrow(x_r_t)){
  image(1:28, 1:28, matrix(x_r_t[i,], nrow=28)[ , 28:1], 
        col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
}
dev.off()












