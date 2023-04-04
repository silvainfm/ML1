Homework 6
================
Franck Brych
4/4/2023

# Load the data

``` r
vo = read.csv(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.train"))
vo = vo[,2:ncol(vo)]

# Convert response variable to factor variable for RF classification
vo$y = as.factor(vo$y)
```

# Fit the random forest or gradient boosted model to the “vowel.train” data using all of the 11 features using the default values of the tuning parameters.

``` r
fit = randomForest(y~., data = vo)
print(fit)
```

    ## 
    ## Call:
    ##  randomForest(formula = y ~ ., data = vo) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 3
    ## 
    ##         OOB estimate of  error rate: 3.22%
    ## Confusion matrix:
    ##     1  2  3  4  5  6  7  8  9 10 11 class.error
    ## 1  48  0  0  0  0  0  0  0  0  0  0  0.00000000
    ## 2   1 47  0  0  0  0  0  0  0  0  0  0.02083333
    ## 3   0  0 48  0  0  0  0  0  0  0  0  0.00000000
    ## 4   0  0  0 47  0  1  0  0  0  0  0  0.02083333
    ## 5   0  0  0  0 46  1  0  0  0  0  1  0.04166667
    ## 6   0  0  0  0  0 43  0  0  0  0  5  0.10416667
    ## 7   0  0  0  0  2  0 44  2  0  0  0  0.08333333
    ## 8   0  0  0  0  0  0  0 48  0  0  0  0.00000000
    ## 9   0  0  0  0  0  0  1  1 46  0  0  0.04166667
    ## 10  0  0  0  0  0  0  0  0  1 47  0  0.02083333
    ## 11  0  0  0  0  0  1  0  0  0  0 47  0.02083333

``` r
plot(fit)
```

![](Homework6_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Use 5-fold CV to tune the number of variables randomly sampled as candidates at each split if using random forest, or the ensemble size if using gradient boosting

``` r
set.seed(24000)

# min node sizes and sampled variables
sampled_variables =  c(3,4,5)
node_size = c(1,5,10,20,40,80)

# cross-validation grid search - 5 folds.
control = trainControl(method = 'cv', number = 5, search = 'grid')

tunegrid = expand.grid(mtry = sampled_variables, min.node.size = node_size, splitrule = 'gini')
grid_fits = train(y~., data = vo, metric = 'Accuracy', method = 'ranger',tuneGrid=tunegrid, trControl=control)

# results
grid_fits
```

    ## Random Forest 
    ## 
    ## 528 samples
    ##  10 predictor
    ##  11 classes: '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 423, 424, 419, 423, 423 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  min.node.size  Accuracy   Kappa    
    ##   3      1             0.9621111  0.9583102
    ##   3      5             0.9545620  0.9500043
    ##   3     10             0.9489176  0.9437927
    ##   3     20             0.8936727  0.8830045
    ##   3     40             0.7539053  0.7292498
    ##   3     80             0.6359730  0.5996159
    ##   4      1             0.9508739  0.9459464
    ##   4      5             0.9414200  0.9355424
    ##   4     10             0.9337826  0.9271457
    ##   4     20             0.8860720  0.8746268
    ##   4     40             0.7421388  0.7162802
    ##   4     80             0.6189300  0.5808589
    ##   5      1             0.9451929  0.9396985
    ##   5      5             0.9337460  0.9271035
    ##   5     10             0.9261270  0.9187301
    ##   5     20             0.8654142  0.8519104
    ##   5     40             0.7213961  0.6934946
    ##   5     80             0.6058613  0.5664905
    ## 
    ## Tuning parameter 'splitrule' was held constant at a value of gini
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were mtry = 3, splitrule = gini
    ##  and min.node.size = 1.

# With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the “vowel.test” data.

``` r
set.seed(24000)

# build best model using parameters found earlier
best_fit = randomForest(y~., data = vo, mtry = 3, nodesize = 1)

# load in data
t = read.csv(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.test"))
t = t[,2:ncol(t)]
t$y = as.factor(t$y)

# predict using best fit
p = predict(best_fit, newdata = t)

# confusion matrix
confusionMatrix(t$y, p)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2  3  4  5  6  7  8  9 10 11
    ##         1  35  6  1  0  0  0  0  0  0  0  0
    ##         2   0 23 15  0  0  0  0  0  4  0  0
    ##         3   0  3 29  2  0  6  0  0  0  0  2
    ##         4   0  0  3 30  0  9  0  0  0  0  0
    ##         5   0  0  0  3 17 16  6  0  0  0  0
    ##         6   0  0  0  0  9 23  0  0  0  0 10
    ##         7   0  0  1  0  8  2 28  0  2  1  0
    ##         8   0  0  0  0  0  0  7 31  4  0  0
    ##         9   0  0  0  0  0  0  5  7 21  3  6
    ##         10  3 14  1  0  0  0  0  0  1 23  0
    ##         11  0  1  1  1  0  4  4  0 13  0 18
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.6017          
    ##                  95% CI : (0.5555, 0.6467)
    ##     No Information Rate : 0.1299          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.5619          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
    ## Sensitivity           0.92105  0.48936  0.56863  0.83333  0.50000  0.38333
    ## Specificity           0.98349  0.95422  0.96837  0.97183  0.94159  0.95274
    ## Pos Pred Value        0.83333  0.54762  0.69048  0.71429  0.40476  0.54762
    ## Neg Pred Value        0.99286  0.94286  0.94762  0.98571  0.95952  0.91190
    ## Prevalence            0.08225  0.10173  0.11039  0.07792  0.07359  0.12987
    ## Detection Rate        0.07576  0.04978  0.06277  0.06494  0.03680  0.04978
    ## Detection Prevalence  0.09091  0.09091  0.09091  0.09091  0.09091  0.09091
    ## Balanced Accuracy     0.95227  0.72179  0.76850  0.90258  0.72079  0.66803
    ##                      Class: 7 Class: 8 Class: 9 Class: 10 Class: 11
    ## Sensitivity           0.56000  0.81579  0.46667   0.85185   0.50000
    ## Specificity           0.96602  0.97406  0.94964   0.95632   0.94366
    ## Pos Pred Value        0.66667  0.73810  0.50000   0.54762   0.42857
    ## Neg Pred Value        0.94762  0.98333  0.94286   0.99048   0.95714
    ## Prevalence            0.10823  0.08225  0.09740   0.05844   0.07792
    ## Detection Rate        0.06061  0.06710  0.04545   0.04978   0.03896
    ## Detection Prevalence  0.09091  0.09091  0.09091   0.09091   0.09091
    ## Balanced Accuracy     0.76301  0.89492  0.70815   0.90409   0.72183
