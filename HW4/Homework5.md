Homework 5
================
Franck Brych
3/1/2023

``` r
mcycle = mcycle

y <- mcycle$accel
x <- matrix(mcycle$times, length(mcycle$times), 1)

plot(x, y, xlab="Time (ms)", ylab="Acceleration (g)")
```

![](Homework5_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# split data into a training and validation set

``` r
set.seed(24000)
# 75% of dataset length is equal to 100
train_index = sample(1:nrow(mcycle), 100, replace = F) 
train = mcycle[train_index,]
validation = mcycle[-train_index,]
ytrain <- train$accel
xtrain <- matrix(train$times, length(train$times), 1)
ytest = validation$accel
xtest = matrix(validation$times, length(validation$times),1)

plot(xtrain,ytrain)
```

![](Homework5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
plot(xtest,ytest)
```

![](Homework5_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

# functions

``` r
## Epanechnikov kernel function
## x  - n x p matrix of training inputs
## x0 - 1 x p input where to make prediction
## lambda - bandwidth (neighborhood size)
kernel_epanechnikov <- function(x, x0, lambda=1) {
  d <- function(t)
    ifelse(t <= 1, 3/4*(1-t^2), 0)
  z <- t(t(x) - x0)
  d(sqrt(rowSums(z*z))/lambda)
}

## k-NN kernel function
## x  - n x p matrix of training inputs
## x0 - 1 x p input where to make prediction
## k  - number of nearest neighbors
kernel_k_nearest_neighbors <- function(x, x0, k=1) {
  ## compute distance betwen each x and x0
  z <- t(t(x) - x0)
  d <- sqrt(rowSums(z*z))

  ## initialize kernel weights to zero
  w <- rep(0, length(d))
  
  ## set weight to 1 for k nearest neighbors
  w[order(d)[1:k]] <- 1
  
  return(w)
}

## Make predictions using the NW method
## y  - n x 1 vector of training outputs
## x  - n x p matrix of training inputs
## x0 - m x p matrix where to make predictions
## kern  - kernel function to use
## ... - arguments to pass to kernel function
nadaraya_watson <- function(y, x, x0, kern, ...) {
  k <- t(apply(x0, 1, function(x0_) {
    k_ <- kern(x, x0_, ...)
    k_/sum(k_)
  }))
  yhat <- drop(k %*% y)
  attr(yhat, 'k') <- k
  return(yhat)
}

## Helper function to view kernel (smoother) matrix
matrix_image <- function(x) {
  rot <- function(x) t(apply(x, 2, rev))
  cls <- rev(gray.colors(20, end=1))
  image(rot(x), col=cls, axes=FALSE)
  xlb <- pretty(1:ncol(x))
  xat <- (xlb-0.5)/ncol(x)
  ylb <- pretty(1:nrow(x))
  yat <- (ylb-0.5)/nrow(x)
  axis(3, at=xat, labels=xlb)
  axis(2, at=yat, labels=ylb)
  mtext('Rows', 2, 3)
  mtext('Columns', 3, 3)
}

## Compute effective df using NW method
## y  - n x 1 vector of training outputs
## x  - n x p matrix of training inputs
## kern  - kernel function to use
## ... - arguments to pass to kernel function
effective_df <- function(y, x, kern, ...) {
  y_hat <- nadaraya_watson(y, x, x,
    kern=kern, ...)
  sum(diag(attr(y_hat, 'k')))
}

## loss function
## y    - train/test y
## yhat - predictions at train/test x
loss_squared_error <- function(y, yhat)
  (y - yhat)^2

## test/train error
## y    - train/test y
## yhat - predictions at train/test x
## loss - loss function
error <- function(y, yhat, loss=loss_squared_error)
  mean(loss(y, yhat))

## AIC
## y    - training y
## yhat - predictions at training x
## d    - effective degrees of freedom
aic <- function(y, yhat, d)
  error(y, yhat) + 2/length(y)*d

## BIC
## y    - training y
## yhat - predictions at training x
## d    - effective degrees of freedom
bic <- function(y, yhat, d)
  error(y, yhat) + log(length(y))/length(y)*d


## make predictions using NW method at training inputs
y_hat <- nadaraya_watson(ytrain, xtrain, xtrain,
  kernel_epanechnikov, lambda=5)

## view kernel (smoother) matrix
matrix_image(attr(y_hat, 'k'))
```

![](Homework5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
## compute effective degrees of freedom
edf <- effective_df(ytrain, xtrain, kernel_epanechnikov, lambda=5)
aic(ytrain, y_hat, edf)
```

    ## [1] 720.4719

``` r
bic(ytrain, y_hat, edf)
```

    ## [1] 720.6974

``` r
## create a grid of inputs 
x_plot <- matrix(seq(min(xtrain),max(xtrain),length.out=100),100,1)

## make predictions using NW method at each of grid points
y_hat_plot <- nadaraya_watson(y, x, x_plot,
  kernel_epanechnikov, lambda=5)

## plot predictions
plot(xtrain, ytrain, xlab="Time (ms)", ylab="Acceleration (g)")
lines(x_plot, y_hat_plot, col="#882255", lwd=2) 
```

![](Homework5_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

# Running Nadaraya-Watson method with the k-NN kernel function

With the squared-error loss function, compute and plot the training
error, AIC, BIC, and validation error (using the validation data) as
functions of the tuning parameter.

``` r
aic_results = rep(NA, 20)
bic_results = rep(NA, 20)
train_error = rep(NA, 20)
validation_error = rep(NA,20)
for (i in 1:20){
   ## make predictions using NW method at training inputs
   y_hat <- nadaraya_watson(ytrain, xtrain, xtrain,
     kern=kernel_k_nearest_neighbors, k=i)
   y_hat_validation = nadaraya_watson(ytrain, xtrain, xtest,
     kern=kernel_k_nearest_neighbors, k=i)
   edf <- effective_df(ytrain, xtrain, 
     kern=kernel_k_nearest_neighbors, k=i)
   
   aic_results[i] <- aic(ytrain, y_hat, edf)
   bic_results[i] <- bic(ytrain, y_hat, edf)
   train_error[i] = error(ytrain, y_hat)
   validation_error[i] = error(ytest, y_hat_validation)
   
}
plot(1:20, validation_error, type = 'l', col = 'purple', ylab = 'error')
lines(1:20, aic_results, type = 'l', col = 'red')
lines(1:20, bic_results,type = 'l', col = 'blue')
lines(1:20, train_error, type = 'l', col = 'green')
legend("topright", legend=c("Validation error", "AIC", "BIC", "Training Error"),
       col=c("purple", "red", "blue", "green"), lty=1)
```

![](Homework5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# 5-fold cross-validation

``` r
inc_flds  <- createFolds(y, k=5)

sapply(inc_flds, length)  ## not all the same length
```

    ## Fold1 Fold2 Fold3 Fold4 Fold5 
    ##    27    26    27    26    27

``` r
cv <- function(kNN = 10, flds=inc_flds) {
  cverr <- rep(NA, length(flds))
  for(tst_idx in 1:length(flds)) { ## for each fold
    
    ## get training and testing data
    inc_trn <- mcycle[-flds[[tst_idx]],]
    inc_tst <- mcycle[ flds[[tst_idx]],]
    ## fit kNN model to training data
    knn_fit <- knnreg(accel ~ times,
                      k=kNN, data=inc_trn)
  
    pre_tst <- predict(knn_fit, inc_tst)
    cverr[tst_idx] <- mean((inc_tst$accel - pre_tst)^2)
    }
  return(cverr)
}

## Compute 5-fold CV for kNN = 1:20
cverrs <- sapply(1:20, cv)
print(cverrs) ## rows are k-folds (1:5), cols are kNN (1:20)
```

    ##           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
    ## [1,] 1104.0066 963.2811 829.8950 595.4610 602.9297 644.1396 730.1586 728.8818
    ## [2,]  686.9211 579.6152 490.4085 527.7196 545.4924 577.2764 549.9656 498.4572
    ## [3,] 1125.3605 703.0117 609.3731 639.1960 504.0455 541.1075 571.0490 636.8820
    ## [4,] 1171.0640 964.8424 786.8098 777.3214 721.1757 749.9423 686.3931 718.4300
    ## [5,]  867.9997 669.2978 727.9205 623.8304 487.5875 450.2543 420.8501 415.6207
    ##          [,9]    [,10]    [,11]    [,12]    [,13]    [,14]    [,15]    [,16]
    ## [1,] 759.3206 736.9746 683.2371 657.6138 664.0747 648.9650 680.5860 668.0187
    ## [2,] 552.1994 554.8777 559.5091 592.5426 571.5380 591.9512 605.4025 595.0136
    ## [3,] 652.9986 684.5233 657.3048 645.1741 670.2805 683.3256 738.8691 750.1166
    ## [4,] 720.3484 751.0692 734.2500 746.1768 753.7234 754.5138 812.7621 809.1996
    ## [5,] 423.4028 441.2661 444.2845 441.3349 473.8083 457.6137 461.8079 514.3904
    ##         [,17]    [,18]    [,19]    [,20]
    ## [1,] 695.0868 674.0900 646.5045 648.6001
    ## [2,] 618.3160 601.5832 611.9161 652.3766
    ## [3,] 759.0053 774.3326 813.3968 875.7761
    ## [4,] 763.3864 784.5076 812.6157 846.2298
    ## [5,] 525.0236 517.2738 518.9184 512.8850

``` r
cverrs_mean <- apply(cverrs, 2, mean)
cverrs_sd   <- apply(cverrs, 2, sd)
```

# Plot CV-estimated test error

``` r
## Plot the results of 5-fold CV for kNN = 1:20
plot(x=1:20, y=cverrs_mean, 
     ylim=range(cverrs),
     xlab="'k' in kNN", ylab="CV Estimate of Test Error")
segments(x0=1:20, x1=1:20,
         y0=cverrs_mean-cverrs_sd,
         y1=cverrs_mean+cverrs_sd)
best_idx <- which.min(cverrs_mean)
points(x=best_idx, y=cverrs_mean[best_idx], pch=20)
abline(h=cverrs_mean[best_idx] + cverrs_sd[best_idx], lty=3)
```

![](Homework5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Interpretation of the figures:

Our objective is to select the K value that is largest but within one
standard error of the minimum error. In this case, the minimum error is
at k = 4, with the dotted line representing the highest point in the
standard error. k = 20 is the largest K value that still falls beneath
this dotted line.
