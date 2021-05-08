library(MASS)

X_train <- read.csv('trainInput.csv', header = F)
y_train <- read.csv('trainOutput.csv', header = F)
X_test <- read.csv('testInput.csv', header = F)
y_test <- read.csv('testOutput.csv', header = F)
all_training <- t(rbind(X_train, y_train))
all_testing <- t(rbind(X_test, y_test))

image_list = list()
for (i in 0:9){
  image_list[[toString(i)]] <- all_training[all_training[, ncol(all_testing)] == i, -257]
}

# If you want to display the image, you need to rotate it
two = matrix(image_list[['2']][97,], nrow = 16)
image(two[, 16:1])

# we can see from the plot that the first 50 decompose values explain the almost 80% of information of the data
SigValDec = svd(image_list[['8']])
plot(cumsum(SigValDec$d) / sum(SigValDec$d), type = 'l', ylab = 'importance')

r_sig_vec <- list()
for (i in 0:9) {
  r_sig_vec[[ toString(i) ]] <- svd(image_list[[ toString(i) ]], nv = 50)$v
}

# the dimension of the right singular vector is 256 * 50
print( dim(r_sig_vec[['0']]))

# For example
b <- ginv(r_sig_vec[['0']]) %*% matrix(image_list[['0']][12,], ncol = 1)
zero_for_zero <- norm(matrix(image_list[['0']][12,], ncol = 1) - (r_sig_vec[['0']] %*% b), '2')

b <- ginv(r_sig_vec[['0']]) %*% matrix(image_list[['3']][12,], ncol = 1)
zero_for_three <- norm(matrix(image_list[['3']][12,], ncol = 1) - (r_sig_vec[['0']] %*% b), '2')

print(zero_for_zero)
print(zero_for_three)

Error_tables <- sapply(1: rowNum, function(inx) {
  testImg <- matrix(all_testing[inx, 1:256], ncol = 1)
  sapply(0:9, function(digit) {
    b <- ginv(r_sig_vec[[ toString(digit) ]]) %*% testImg
    error <- norm(testImg - (r_sig_vec[[ toString(digit) ]] %*% b), '2')
    return(error)
  })
})


dim(Error_tables)

inx <- apply(Error_tables, 2, which.min)
prediction <- c('0':'9')[inx]

# confusion matrix for prediction and test data
table(prediction , unlist(all_testing[, 257]))


