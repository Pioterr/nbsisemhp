#' @title R6 class for Naive Bayes classifier Object
#'
#' @description
#' Create an object of class NaiveBayesClassifier.
#' The Naives Bayes Classifier is a classifier based on applying Bayes' theorem with strong(naive) independance
#' assumption between the features
#'
#' @import doParallel
#' @import foreach
#' @import parallel
#' @import plotly
#' @importFrom discretization topdown
#'
#' @export
#'
#' @example
#' #Import data
#' data(mtcars)
#' #Get target and feature
#' y <-
#' X <-
#'
#' # Initialize object
#' obj <- naivebayes_classifier$new()
#'
#' #Train
#' obj$fit(X,y)
#'
#' #Predict
#' y_pred <- obj$predict(X)
#'
#'
#' @details
#' Additional details...
#'
#' @references
#' references
#'
#'
#'
#'
naivebayes_classifier <- R6::R6Class(classname = "NaiveBayesClassifier",
                                 private = list(
                                   #log prediction probability
                                   log_prob_f = NULL,
                                   #prediction probability
                                   prob_f = NULL,
                                   #name of numerical features
                                   num = NULL,
                                   #name of non-numerical features
                                   non_num = NULL,
                                   #Number of feature
                                   num_feat = NULL,
                                   #different class label
                                   Fclass_name = NULL,
                                   #Feature name
                                   Ffeat_name = NULL,
                                   #Calculated prior probability
                                   Fclass_app = NULL,
                                   #Training data set dimension
                                   Fdim_train = NULL,
                                   #smoothing parameter
                                   Falpha = NULL,
                                   #number of feature seen during train
                                   Fn_feature_count = NULL,
                                   #table of feature probability log
                                   sample_weight = NULL,
                                   #interval discretionary
                                   intervalle = NULL,
                                   #numerical columns not to discretize
                                   Fcol_to_keep = NULL,
                                   #Boolean, if true proceed discretisation
                                   Fdicret = TRUE,
                                   #Has been fitted ?
                                   flag_fit = NULL,
                                   ###separate the dataframe into two dataframes based on the column type
                                   ###the keep argument allows you to indicate whether certain columns should not be discretized
                                   ###the keep_all argument allows you not discretize all the columns if TRUE
                                   separerColonnes = function(dataframe, keep = private$Fcol_to_keep) {
                                     ###stores variables not to be discretized in a private variable
                                     if(!is.null(keep)){
                                       private$Fcol_to_keep <- keep
                                     }

                                     colonnes_numeriques <- numeric()
                                     colonnes_non_numeriques <- character()

                                     count_non_categorical <- 0

                                     ###for each columns of the dataframe, if the column is numeric or non factor and not in the list of columns not to discretise
                                     ###will stock it in the "colonnes_numeriques" variable
                                     ###the other columns will be assign to the "colonnes_non_numerique" variable.
                                     for (colonne in names(dataframe)) {
                                       if (is.numeric(dataframe[[colonne]]) & (!is.factor(dataframe[[colonne]])) & !(colonne %in% keep)) {
                                         colonnes_numeriques <- c(colonnes_numeriques, colonne)
                                         count_non_categorical <- count_non_categorical + 1
                                       } else {
                                         colonnes_non_numeriques <- c(colonnes_non_numeriques, colonne)
                                       }
                                     }
                                     ### If non there is non numerical columns to discretise
                                     ### Warning message and split the data
                                     if(count_non_categorical > 0){
                                       text <- cat("\nNon-categorical variables detected. These variables have been discretized : \n",paste(colonnes_numeriques,collapse = ", "), "\n")
                                       message(text,"\n")

                                       #Column factorization before next operation
                                       dataframe[colonnes_non_numeriques] <- lapply(dataframe[colonnes_non_numeriques],as.factor)

                                       ###transform the two variable (list) to a dataframe and un a list
                                       resultats <- list(numeriques = dataframe[colonnes_numeriques],
                                                         non_numeriques = dataframe[colonnes_non_numeriques])

                                       #warning("Non-categorical variables detected. These variables have been discretized. \n")
                                     }else {
                                       resultats <- dataframe[colonnes_non_numeriques]
                                       private$Fdicret <- FALSE
                                     }


                                     #Column factorization before next operation
                                     dataframe[colonnes_non_numeriques] <- lapply(dataframe[colonnes_non_numeriques],as.factor)

                                     ###transform the two variable (list) to a dataframe and un a list
                                     resultats <- list(numeriques = dataframe[colonnes_numeriques],
                                                       non_numeriques = dataframe[colonnes_non_numeriques])

                                     return(resultats)
                                   },

                                   ###assign the intervalls of discretisation calculate with the topdown method to a private variable
                                   get_inter = function(X_sep,y, discretisation = private$Fdicret){
                                     if(isTRUE(discretisation)){
                                       ###get the intervalls using top down method of the first variable of the list X_sep which is the numerical variables
                                       split <- discretization::topdown(cbind(X_sep[[1]],as.factor(y)), method = 1)

                                       ###replace the first and last split point by -Inf and Inf respectively.
                                       ###necessary because the lowest value of the train data could be superior of the lowest value of the test data
                                       for(i in 1:length(split)){
                                         split[[i]][1] <- -Inf
                                         split[[i]][length(split[[i]])] <- Inf
                                         }
                                     }
                                     private$intervalle <- split
                                   },
                                   ###discretise the numeric data using the intervalls calculate with "get_inter()"
                                   ###return a dataframe with discretised value
                                   discretize = function(X_sep, intervalle = private$intervalle, discretisation = private$Fdicret){
                                     if(isTRUE(discretisation)){
                                       ###numerical variables
                                       df <- X_sep[[1]]

                                       ###split the data using the intervalls
                                       for(i in 1:ncol(df)){
                                         df[[i]] <- cut(df[[i]], breaks = intervalle[[i]], labels = FALSE, include.lowest = TRUE, right = TRUE)
                                       }

                                       ###create a dataframe with the discretised numerical variables and the non numerical variables
                                       X <- cbind(df, X_sep[[2]])

                                     } else {
                                       X <- X_sep
                                     }
                                     return(as.data.frame(X))
                                   }
                                 ),
                                 public = list(
                                   #' @description
                                   #' Creates a new instance of this [R6][R6::R6Class] class.
                                   #'
                                   #' @param alpha (`float`)\cr
                                   #' Smoothing parameters. Default is 1.
                                   #'
                                   #'
                                   initialize = function(alpha = 1){
                                     private$Falpha <- alpha
                                     private$flag_fit <- FALSE

                                     #Check package presence
                                     if (!require(discretization)){
                                       install.packages("discretization")
                                     }

                                     if (!require(doParallel)){
                                       install.packages("doParallel")
                                     }

                                     if (!require(foreach)){
                                       install.packages("foreach")
                                     }


                                   },
                                   #' @description
                                   #' Split the data in train and test
                                   #'
                                   #' @param X (`array`)\cr
                                   #' 2D array with feature as column name
                                   #'
                                   #' @param y (`list()`)\cr
                                   #' List of class to evaluate
                                   #'
                                   #' @param test_size (`float`)\cr
                                   #' Size of the test sample
                                   #' Default = 0.3
                                   #'
                                   #' @return
                                   #' Return a fitted object of classe NaiveBayesClassifier
                                   stratified_split = function(X, y, test_size = 0.3) {
                                     ### Bind X and y in one dataframe
                                     data <- cbind.data.frame(X, y)

                                     ### Separate the indices of each class
                                     indices_by_class <- lapply(unique(y), function(class_label) {
                                       which(y == class_label)
                                     })

                                     ### Initialize training and testing indices
                                     train_indices <- test_indices <- numeric(0)

                                     ### For each class, split the indices into training and testing sets
                                     for (class_indices in indices_by_class) {
                                       n_class <- length(class_indices)
                                       n_test_class <- round(n_class * test_size)

                                       ### Select random indices for test set of this class
                                       test_indices_class <- sample(class_indices, n_test_class)

                                       ### Add training and testing indices for this class
                                       train_indices <- c(train_indices, setdiff(class_indices, test_indices_class))
                                       test_indices <- c(test_indices, test_indices_class)
                                     }

                                     ### Create the training set by excluding observations from the test set
                                     train_data <- data[train_indices, ]

                                     ### Create the test set by selecting only observations from the test set
                                     test_data <- data[test_indices, ]

                                     ### Return training and test sets for x and y
                                     return(list(
                                       X_train = train_data[, -ncol(train_data)],  # Exclure la dernière colonne (variable à prédire)
                                       y_train = train_data[, ncol(train_data)],
                                       X_test = test_data[, -ncol(test_data)],
                                       y_test = test_data[, ncol(test_data)]
                                     ))
                                   },
                                   #' @description
                                   #' Fit the object to a training data set
                                   #'
                                   #' @param X (`array`)\cr
                                   #' 2D array with feature as column name
                                   #'
                                   #' @param y (`list()`)\cr
                                   #' List of class to evaluate
                                   #'
                                   #' @param col_to_keep (`list()`) columns not to discretize
                                   #' Default = NULL
                                   #'
                                   #' @return
                                   #' Return a fitted object of classe NaiveBayesClassifier
                                   fit = function(X,y, alpha = private$Falpha, col_to_keep = NULL, uni_prior_prob = FALSE){
                                     #dataframe check
                                     df_ok <- is.data.frame(X)
                                     if (!df_ok) {stop("This is not a dataframe \n")}

                                     #vector check
                                     y_test <- dim(X)[1] == length(y)
                                     if (!y_test) {stop("y length different from X row \n")}

                                     if (!is.logical(uni_prior_prob)) {stop("uni_prior_prob must be logical \n")}


                                     #removing Na from training set
                                     if (any(is.na(X)) || any(is.na(y))) {
                                       warning("NAs present in at least X or y. row with NAs have been removed. \n")
                                       # row with missing value
                                       na_rows <- complete.cases(X, y)

                                       # drop rows with NA
                                       X <- X[na_rows, , drop = FALSE]
                                       y <- y[na_rows, drop = FALSE]
                                     }

                                     # Discretisation

                                     if (sum( sapply(X,is.numeric))){
                                       X_sep <- private$separerColonnes(X, col_to_keep)
                                       private$get_inter(X_sep, y)
                                       X <- private$discretize(X_sep)
                                     }


                                     y <- as.factor(y)

                                     private$Fclass_name <- levels(y)
                                     # Prior prob check
                                     if(uni_prior_prob){
                                       private$Fclass_app <- prop.table(table(private$Fclass_name))
                                     }else{
                                       private$Fclass_app <- prop.table(table(y))
                                     }

                                     #Get training information
                                     private$Ffeat_name <- colnames(X)
                                     private$Fdim_train <- dim(X)

                                     #Initialisation
                                     vec_log <- NULL
                                     vec_n_feat <- NULL
                                     flag_zero <- FALSE

                                     #posterior table calculation
                                     for (i in colnames(X)){
                                       tab_n_feat <- table(X[,i],y)
                                       vec_n_feat[[i]] <- tab_n_feat #number of occurrence without smoothing parameters

                                       if(sum(tab_n_feat == 0)){
                                         flag_zero <- TRUE
                                       }


                                       tab_smooth <- tab_n_feat + alpha
                                       tab_prob <- prop.table(tab_smooth,margin = 2) #Probability

                                       tab_log <- log(tab_prob) #take the log
                                       vec_log[[i]] <- tab_log
                                     }

                                     #Assignate value to object param
                                     private$Fn_feature_count <- vec_n_feat
                                     private$sample_weight <- vec_log
                                     private$Falpha <- alpha
                                     private$flag_fit <- TRUE

                                     if(flag_zero & !alpha){
                                       warning("Zero value found\nConsider laplace smoothing\n")
                                     }


                                     #Return object as a result for method chaining
                                     invisible(self)
                                   },
                                   fit_parallel = function(X,y, alpha = 1, ncores = 4, col_to_keep = NULL, uni_prior_prob = FALSE){
                                     if(!require(doParallel) & !require(foreach)){
                                       stop("library parallel, doParallel or foreach is not installed")
                                     }

                                     #dataframe check
                                     df_ok <- is.data.frame(X)
                                     if (!df_ok) {stop("This is not a dataframe \n")}

                                     #vector check
                                     y_test <- dim(X)[1] == length(y)
                                     if (!y_test) {stop("y length different from X row \n")}

                                     if (!is.logical(uni_prior_prob)) {stop("uni_prior_prob must be logical \n")}

                                     #No zero cores
                                     zero_test <- ncores > 0
                                     if(!zero_test){stop("ncores must be >0 \n")}



                                     #removing Na from training set
                                     if (any(is.na(X)) || any(is.na(y))) {
                                       warning("NAs present in at least X or y. row with NAs have been removed. \n")
                                       # row with missing value
                                       na_rows <- complete.cases(X, y)

                                       # drop rows with NA
                                       X <- X[na_rows, , drop = FALSE]
                                       y <- y[na_rows, drop = FALSE]
                                     }

                                     # Discretisation
                                     if (sum( sapply(X,is.numeric))){
                                       X_sep <- private$separerColonnes(X, col_to_keep)
                                       private$get_inter(X_sep, y)
                                       X <- private$discretize(X_sep)
                                     }
                                     y <- as.factor(y)

                                     private$Fclass_name <- levels(y)
                                     # Prior prob check
                                     if(uni_prior_prob){
                                       private$Fclass_app <- prop.table(table(private$Fclass_name))
                                     }else{
                                       private$Fclass_app <- prop.table(table(y))
                                     }
                                     #Get training information
                                     private$Ffeat_name <- colnames(X)
                                     private$Fdim_train <- dim(X)
                                     private$Falpha <- alpha

                                     # Initialize cluster
                                     doParallel::registerDoParallel(ncores)
                                     # Split Data
                                     blocs <- split.default(X,names(X))
                                     #init list
                                     liste_final <- list()
                                     vec_final <- NULL

                                     # res inter
                                     result <- foreach::foreach(b=blocs,.export =c("self") ,.combine =  ,.inorder = F) %dopar% {
                                       tab_1 <- b[1]
                                       tab <- table(cbind(tab_1,y)) + private$Falpha

                                       #return(tab)
                                       return(log(prop.table(tab,margin = 2)))
                                     }

                                     # Stop cluster
                                     doParallel::stopImplicitCluster()

                                     names(result) <- names(blocs)



                                     private$sample_weight <- result
                                     private$flag_fit <- TRUE
                                     private$Falpha <- alpha

                                     invisible(self)
                                   },
                                   #' @description
                                   #' Predict target values based whole data set.
                                   #'
                                   #' @param X (`array`)\cr
                                   #' 2D array of shape (n_row,n_feature) with feature as column name
                                   #'
                                   #' @return Return a list of class prediction
                                   predict = function(X){

                                     #Call to predict proba
                                     log_table <- self$predict_proba_joint_log(X)

                                     pred <- private$Fclass_name[apply(log_table,1,which.max)]

                                     return(pred)
                                   },
                                   #' @description
                                   #' Predict target values based whole data set. Use predict_on_one function
                                   #'
                                   #' @param X (`array`)\cr
                                   #' 2D array of shape (n_row,n_feature) with feature as column name
                                   #'
                                   #' @return Return a list of class prediction
                                   predict_parallel = function(X, ncores = 4){
                                     #Is function fitted
                                     if(!private$flag_fit){
                                       stop("\nMust fit function first \n")
                                     }
                                     #dataframe check
                                     df_ok <- is.data.frame(X)
                                     if (!df_ok) {stop("This is not a dataframe \n")}


                                     #No zero cores
                                     zero_test <- ncores > 0
                                     if(!zero_test){stop("ncores must be >0 \n")}

                                     # Fonction de prediction en parallele
                                     if(!require(parallel) & !require(doParallel) & !require(foreach)){
                                       stop("\nlibrary parallel, doParallel or foreach is not installed\n")
                                     }else{
                                       #Number of row
                                       n <- nrow(X)
                                       part <- n/ncores
                                       # Initialize cluster
                                       doParallel::registerDoParallel(ncores)
                                       # Split Data
                                       blocs <- split(X, ceiling(1:nrow(X)/part))
                                       # res inter
                                       y_pred <- foreach::foreach(b=blocs,.export = c("self"),.combine = c ,.inorder = F) %dopar% {return(self$predict(b))}

                                       # Stop cluster
                                       doParallel::stopImplicitCluster()


                                       return(y_pred)
                                     }
                                   },
                                   predict_proba_joint_log = function(X){
                                     #return the log joint
                                     #Is function fitted
                                     if(!private$flag_fit){
                                       stop("Must fit function first \n")
                                     }

                                     #dataframe check
                                     df_ok <- is.data.frame(X)
                                     if (!df_ok) {stop("This is not a dataframe \n")}


                                     p_prob <- private$Fclass_app


                                     flag <- FALSE
                                     #NA Check
                                     if (any(is.na(X))) {
                                       X <- X[, colSums(is.na(X)) == 0]
                                       flag <- TRUE
                                       warning("\nNAs present in at least one variable of X. Those variables wont be used\n")
                                     }
                                     if (sum(X == '')) {
                                       X <- X[, colSums(X == '') == 0]
                                       flag <- TRUE
                                       warning("\nempty string present in at least one variable of X. Those variables wont be used\n")
                                     }

                                     if(flag){
                                       warning("Feature without NA or empty string : \n", paste(colnames(X), collapse = ", "),"\n")
                                     }

                                     #Check if numeric column
                                     if (sum( sapply(X,is.numeric))){

                                       X_sep <- private$separerColonnes(X, private$Fcol_to_keep)
                                       X <- private$discretize(X_sep)
                                     }

                                     # Fonction qui predit et qui retourne un vecteur y de shape (n_row,1)
                                     log_prob_f <- matrix(log(p_prob), nrow = nrow(X),ncol = length(private$Fclass_name), byrow = T)
                                     feat_not_used <- c()

                                     for (var in colnames(X)){
                                       cond <- var %in% private$Ffeat_name
                                       if(cond){
                                         tab_feature <- private$sample_weight[[var]]
                                         X_var_feat <- as.character(X[[var]])

                                         # test if feature has been seen during fit

                                         feat_uni <- unique(X_var_feat)
                                         table_uni <- rownames(tab_feature)
                                         n_value_in_table <- sum(sapply(unique(feat_uni),FUN = function(x){x %in% table_uni}))
                                         test <- n_value_in_table == length(feat_uni)
                                         if(!test){
                                           warning("\n feature value in ",var," not seen during fit, it has been ignored in the log calculation \n")
                                           feat_not_used <- append(feat_not_used, var)
                                           next
                                         }
                                         log_prob_f <- log_prob_f + tab_feature[X_var_feat,]
                                       }else{
                                         warning(cat(var," not seen during fit"))
                                       }
                                     }
                                     if(length(feat_not_used) > 0){
                                       warning("\n Those features have not been used for prediction :\n", paste(feat_not_used, collapse = ", ") ,"\n")
                                     }

                                     rownames(log_prob_f) <- 1:nrow(log_prob_f)
                                     names(dimnames(log_prob_f)) <- c("","class")


                                     return(log_prob_f)
                                   },
                                   predict_proba_log = function(X){
                                     joint_table <- self$predict_proba_joint_log(X)
                                     normalized <- joint_table - log(rowSums(exp(joint_table)))
                                     return(normalized)
                                   }
                                   ,
                                   predict_proba = function(X){
                                     prob_table <- exp(self$predict_proba_log(X))
                                     return(round(prob_table,3))
                                   }
                                   ,
                                   #' @description
                                   #' Use to compare test value with predict value
                                   #'
                                   #' @param X (`array`)\cr
                                   #' 2D array of shape (n_row,n_feature) with feature as column name
                                   #'
                                   #' @param y (`list()`)\cr
                                   #' List of test y to evaluate the model prediction.
                                   #'
                                   #' @param pred_f (`array`) change to perform a self prediction, or to use result from previous prediction.
                                   #' possible value [`TRUE`, `FALSE`]
                                   #' if TRUE, X need to be a 2D array of shape (n_row,n_feature) with feature as column name
                                   #' if FALSE, X need to be a list of prediction
                                   #'
                                   #' @param as ((`str()`)) change to get the confusion matrix, or different indicator.Default is 'table' for a confusion matrix.
                                   #' possible value ['table','accuracy']
                                   #'
                                   #' @return Return result between y test and self$predict(X)
                                   score= function(X,y, as = 'table', pred_f= TRUE){
                                     #Is function fitted
                                     if(!private$flag_fit){
                                       stop("Must fit function first \n")
                                     }
                                     #as factor
                                     y <- as.factor(y)

                                     if(isTRUE(pred_f)){
                                       #predict value
                                       prediction <- as.factor(self$predict(X))

                                       test <- length(levels(prediction)) == length(levels(y))
                                       if(!test){
                                         stop("\nDifferent levels in prediction and observed value \n")
                                       }

                                       tab <- table(y,prediction, dnn = c("real","predict"))
                                       if(as == 'accuracy'){
                                         accuracy <- sum(diag(tab))/sum(tab)
                                         return(accuracy)
                                       }else{
                                         return(tab)
                                       }
                                     } else if(!isTRUE(pred_f)){

                                       X <- as.factor(X)
                                       test <- length(levels(X)) == length(levels(y))
                                       if(!test){
                                         stop("\nDifferent levels in prediction and observed value \n")
                                       }

                                       tab <- table(y,X, dnn = c("real","predict"))
                                       if(as == 'accuracy'){
                                         accuracy <- sum(diag(tab))/sum(tab)
                                         return(accuracy)
                                       }else{
                                         return(tab)
                                       }
                                     }


                                   },
                                   #' @description
                                   #' Plot a bar plot for each variable of explanatory variables.
                                   #' For each class of a variable, n bars are plot (n = number of y class).
                                   #'
                                   plot = function() {
                                     if(!require(plotly)){
                                       install.packages("plotly")
                                       #stop("plotly is not installed")
                                     }

                                     if(is.null(private$Fn_feature_count)){stop("object must be fitted with the fit method")}

                                     count <- private$Fn_feature_count
                                     print(count)
                                     plot_list <- list()
                                     for(i in 1:length(count)){
                                       df <- as.data.frame(count[[i]])
                                       n_plot <- plotly::plot_ly(df, x = ~Var1, y = ~Freq, type = "bar", color = ~y, colors = "Set1") %>%
                                         layout(title = "Bar Plot",
                                                xaxis = list(title = "Class"),
                                                yaxis = list(title = "Freq"))
                                       plot_list <- append(plot_list, list(n_plot))
                                     }
                                     dimrow <- length(count) / 2
                                     subplot(plot_list, nrows = dimrow) %>%
                                       layout(showlegend = FALSE)
                                   },
                                   #' @description
                                   #' Print function for the naive bayes classifier object.
                                   #' Print parameters, number of feature and number of classe
                                   #'
                                   print = function() {
                                     cat("Naive Bayes Classifier:\n")
                                     cat("Smoothing parameter (alpha): ", private$Falpha, "\n")
                                     cat("Dimension of training data set : ",private$Fdim_train[1]," rows",private$Fdim_train[2]," columns\n" )
                                     cat("Number of classes: ", length(private$Fclass_name), "\n")
                                     cat("Class names: ", paste(private$Fclass_name, collapse = ", "), "\n")
                                     cat("Number of features: ", private$Fdim_train[2], "\n")
                                     cat("Features names: ", paste(private$Ffeat_name, collapse = ", "), "\n")
                                   },
                                   summary = function() {
                                     cat("Summary of Naive Bayes Classifier:\n")
                                     cat("Smoothing parameter (alpha): ", private$Falpha, "\n")
                                     cat("Dimension of training data set : ",private$Fdim_train[1]," rows",private$Fdim_train[2]," columns\n" )
                                     cat("Number of classes: ", length(private$Fclass_name), "\n")
                                     cat("Class names: ", paste(private$Fclass_name, collapse = ", "), "\n")
                                     cat("prior probabilities: ", private$Fclass_app, "\n")
                                     cat("Number of features: ", private$Fdim_train[2], "\n")
                                     cat("Features names: ", paste(private$Ffeat_name, collapse = ", "), "\n")
                                     cat("Non-numerical features names: ", paste(private$non_num, collapse = ", "), "\n")
                                     cat("Numerical features names: ", paste(private$num, collapse = ", "), "\n")
                                     cat("discretization intervals for numerical features:\n")
                                     print(private$intervalle)
                                   }),
                                 active = list(
                                   #' @field alpha (`float`)\cr
                                   #' Object smoothing parameters alpha used for actual fit.
                                   #' Alpha as impact on the calculated probability, you need to change it before a fit. Read-only
                                   alpha = function() return(private$Falpha),
                                   #' @field logsweight (`list()`)\cr
                                   #' Return list of posterior probability table. Read-only
                                   logsweight = function() return(private$sample_weight),
                                   #' @field class_name (`list()`)\cr
                                   #' Return list of target names. Read-only
                                   class_name = function() return(private$Fclass_name),
                                   #' @field class_app (`list()`)\cr
                                   #' Return target prior probability. Read-only
                                   class_app = function() return(private$Fclass_app),
                                   #' @field inter (`list()`)\cr
                                   #' Return target prior probability. Read-only
                                   inter = function() return(private$intervalle),
                                   #' @field inter (`list()`)\cr
                                   #' Return target prior probability. Read-only
                                   feature_name = function() return(private$Ffeat_name),
                                   #' @field inter (`list()`)\cr
                                   #' Return target prior probability. Read-only
                                   n_feature_count = function() return(private$Fn_feature_count)
                                 )
)
