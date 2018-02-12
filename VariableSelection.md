Supervised variable selection methods
================
Mateusz Dolinski

-   [Introduction](#introduction)
-   [Discriminant analysis](#discriminant-analysis)
-   [Logistic regression](#logistic-regression)
-   [Summary](#summary)

Introduction
------------

The aim of this analysis is to compare various methods of variable selection. We will model Wisconsin Diagnostic Breast Cancer Data. The data set consists of 569 cases, of which 212 were diagnosed as malignant and 357 diagnosed as benign. The original dataset along with detailed description is available under the following URL: <https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)>

Before we move to modelling, let us download the data:

``` r
#Load data
url_link <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
DATA <- read.table(file = url_link,
                   header = F, 
                   sep = ',')
names <- c('radius', 'texture', 'peri', 'area', 
           'smooth', 'comp', 'scav', 'ncav', 'symt', 'fracd')
metrics <- c('mv', 'ev', 'sd')

#Add names and get rid of ID
column_names <- c('ID', 'diagnosis', 
                  apply(X = expand.grid(metrics, names),
                        MARGIN = 1, 
                        FUN = function(x) {return(paste(x[2], x[1], sep = '_'))}))
colnames(DATA) <- column_names
DATA <- DATA %>%
  select(-ID)

#Create scaled dataset for regressions
DATAREG <- DATA %>%
  mutate(diagnosis = as.numeric(diagnosis) - 1) %>%
  scale(center = T, scale = T) %>%
  data.frame()
```

Discriminant analysis
---------------------

Let us perform default LDA and set model computed on all variables as a benchmark result:

``` r
#Function which predicts 1CV accuracy on LDA analysis based on given formula
#@param formula is a formula used in discriminant analysis
performLDA <- function(formula){
  #Estimate model
  lda_model <- lda(formula, 
                   data = DATA, 
                   CV = T)
  #Compute accuracy
  lda_predict <- lda_model$class
  confusion_matrix <- table(lda_predict, 
                            DATA$diagnosis)
  sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

formula <- as.formula('diagnosis ~ .')
sprintf('The accuracy metric for benchmark LDA model is following: %f', 
        performLDA(formula))
```

    ## [1] "The accuracy metric for benchmark LDA model is following: 0.957821"

The outcome looks surprisingly good. Let us try to improve it, even slightly. First, let us apply regression variable selection methods to select the most important subset of exogenous variables.

First considered method is LASSO regression:

``` r
#Function which for given data set performs lasso variable selection
#@param DATASET is the given data set
findCoefLasso <- function(DATASET){
  #Estimate lasso coefficients 
  cv_lasso <- cv.glmnet(x = as.matrix(DATASET[, -1]),
                        y = DATASET[, 1], 
                        alpha = 1, 
                        nfolds = 10)
  
  #Find selected variables
  coef <- coef(cv_lasso)
  coef_ind <- which(as.matrix(coef) != 0)[-1]
  selected_variables <- colnames(DATA)[coef_ind]
  
  #Perform discriminant analysis on selected variables
  formula <- as.formula(paste('diagnosis ~ ', 
                              paste(selected_variables, 
                                    collapse = ' + ')))
}
```

Next consider stepwise variable selection methods. The model argument in the following function will be useful later:

``` r
#Function which for given data set performs stepwise variable selection
#@param DATASET is the given data set
#@param direcion: 'forward' or 'backward' selection
#@param model: 'glm' or 'lda' model
findCoefStep <- function(DATASET, direction, model){
  #assert that direction and model are ok
  stopifnot(direction %in% c('forward', 'backward') &&
              model %in% c('glm', 'lda'))
  
  #Prepare models for step function
  if (model == 'lda'){
    null <- lm(diagnosis ~ 1, 
               data = DATASET)
    full <- lm(diagnosis ~ ., 
               data = DATASET)
  } else {
    null <- glm(diagnosis ~ 1, 
                data = DATASET, 
                family = binomial(link = 'logit'))
    full <- glm(diagnosis ~ ., 
                data = DATASET, 
                family = binomial(link = 'logit'))
  }
  
  if (direction == 'forward'){
    #Estimate stepwise forward and backward regressions
    forward <- step(null, 
                    scope = list(lower = null, upper = full), 
                    direction = 'forward', trace = F)
    
    #Find selected variables
    variables_forw <- names(coef(forward))[-1]
    
    #Create formula
    as.formula(paste('diagnosis ~ ', 
                     paste(variables_forw, 
                           collapse = ' + ')))
  } else {
    #Estimate stepwise forward and backward regressions
    backward <- step(full, 
                     scope = list(lower = null, upper = full), 
                     direction = 'backward', trace = F)
    
    #Find selected variables
    variables_back <- names(coef(backward))[-1]
    
    #Create formula
    as.formula(paste('diagnosis ~ ', 
                     paste(variables_back, 
                           collapse = ' + ')))
  }
}
```

Time to calculate error based on models estimated after applying variable selection. We will use Leave One Out CV:

``` r
#Wrapper function to perform K fold CV for variable selection 
#@param K is the number of folds
#@param findFormula is the function to obtain formula to be used in further analysis
#@param DATASET is the data set
#@param DATASETREG is the scaled dataset for variable selection purposes
#@param method is either 'lda' or 'glm' - model evaluated
performKCV <- function(K, findFormula, DATASET, DATASETREG, method, ...){
  #assert that method is ok
  stopifnot(method %in% c('lda', 'glm'))
  
  #Tweak target variable if necessary
  if (method == 'glm'){
    DATASETREG[, 1] <- as.numeric(DATASET[, 1]) - 1
  }
  
  #Create folds for this CV
  folds <- rep(c(1:K), length.out = nrow(DATASET))
  
  #Number of correct classifications
  correct <- 0
  
  #Iterate over observations
  for (i in 1:K){
    
    #Create samples for this iteration
    SAMPLE <- DATASET[-which(folds == i), ]
    check <- DATASET[which(folds == i), ]
    SAMPLEREG <- DATASETREG[-which(folds == i), ]
    
    if (method == 'lda'){
      #Perform LDA
      formula <- findFormula(SAMPLEREG, ...)
      lda_model <- lda(formula, SAMPLE)
      
      #Update # of correct classifications
      correct_pred <- sum(as.character(predict(lda_model, check)$class) == check[, 1])
      correct <- correct + correct_pred
    } else {
      #Perform GLM
      formula <- findFormula(SAMPLEREG, ...)
      glm_model <- glm(formula, 
                       SAMPLE, 
                       family = binomial(link = 'logit'))
      
      #Update # of correct classifications
      prediction <- ifelse(predict(glm_model, data.frame(check), type = 'response') >= 0.5,
                           1,
                           0)
      correct_pred <- sum(prediction  == as.numeric(check[, 1]) - 1)
      correct <- correct + correct_pred
    }
  }
  correct / length(folds)
}
```

Time to use the implemented framework:

``` r
#Calculate errors of the lda models with various variable selection methods:
sprintf('The results for LDA on reduced model chosen by lasso are following: %f',
        performKCV(10, findCoefLasso, DATA, DATAREG, 'lda'))
```

    ## [1] "The results for LDA on reduced model chosen by lasso are following: 0.961336"

``` r
sprintf('The results for LDA on reduced model chosen by forward are following: %f',
        performKCV(10, findCoefStep, DATA, DATAREG, 'lda', 'forward', 'lda'))
```

    ## [1] "The results for LDA on reduced model chosen by forward are following: 0.961336"

``` r
sprintf('The results for LDA on reduced model chosen by backward are following: %f',
        performKCV(10, findCoefStep, DATA, DATAREG, 'lda', 'backward', 'lda'))
```

    ## [1] "The results for LDA on reduced model chosen by backward are following: 0.961336"

Using variable selection not only can we can beat the original score but we also reduce the number of variables used which is very helpful in recognition of true causes of diagnosis of the type of breast cancer tumor.

Logistic regression
===================

Lastly, let us try estimate error on traditional logistic regression with setpwise variable selection:

``` r
#Calculate errors of the glms with various variable selection methods:
sprintf('The results for GLM on reduced model chosen by forward are following: %f',
        performKCV(10, findCoefStep, DATA, DATAREG, 'glm', 'forward', 'glm'))
```

    ## [1] "The results for GLM on reduced model chosen by forward are following: 0.959578"

``` r
sprintf('The results for GLM on reduced model chosen by backward are following: %f',
        performKCV(10, findCoefStep, DATA, DATAREG, 'glm', 'backward', 'glm'))
```

    ## [1] "The results for GLM on reduced model chosen by backward are following: 0.954306"

Logistic regression has trouble with convergence as well as behaves comparably to the benchmark model.

Summary
=======

Not only does LDA perform better but logistic regression has trouble with convergence (warnings were suppressed). Therefore I would suggest discriminant analysis with variable selection as the best model out of considered approaches.
