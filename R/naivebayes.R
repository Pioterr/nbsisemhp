#Import library
library(R6)
library(stats)

#doc
library(roxygen2)
library(devtools)

#calcul aprallel
library(parallel)
library(doParallel)
library(foreach)

#Discretisation
library(discretization)

#Test efficacité
library(profvis)


#' @title R6 class for Naive Bayes classifier Object
#'
#' @description 
#' Create an object of class NaiveBayesClassifier.
#' The Naives Bayes Classifier is a classifier based on applying Bayes' theorem with strong(naive) independance
#' assumption between the features
#' 
#' @importFrom stats mean
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


naivebayes_classifier <- R6Class(classname = "NaiveBayesClassifier",
                                 private = list(
                                   #Number of feature
                                   num_feat = NULL,
                                   #type of class
                                   Fclass_name = NULL,
                                   #Feature name
                                   Ffeat_name = NULL,
                                   #proba d'appartenance
                                   Fclass_app = NULL,
                                   #Training data set dimension
                                   Fdim_train = NULL,
                                   #smoothing param
                                   Falpha = NULL,
                                   #probability
                                   sample_weight = NULL,
                                   #interval discretionary 
                                   intervalle = NULL,
                                   #numerical columns not to discretize
                                   Fcol_to_keep = NULL,
                                   #separe le dataframe en deux dataframe en fonction du type des colonnes
                                   separerColonnes = function(dataframe, keep = private$Fcol_to_keep) {
                                     if(!is.null(keep)){
                                       private$Fcol_to_keep <- keep
                                     }
                                     colonnes_numeriques <- numeric()
                                     colonnes_non_numeriques <- character()
                                     
                                     for (colonne in names(dataframe)) {
                                       if (is.numeric(dataframe[[colonne]]) & (!is.factor(dataframe[[colonne]])) & !(colonne %in% keep)) {
                                         colonnes_numeriques <- c(colonnes_numeriques, colonne)
                                         warning("Non-categorical variables detected. These variables have been discretized.")
                                         #
                                       } else {
                                         colonnes_non_numeriques <- c(colonnes_non_numeriques, colonne)
                                         #
                                       }
                                     }
                                     
                                     resultats <- list(numeriques = dataframe[colonnes_numeriques],
                                                       non_numeriques = dataframe[as.factor(colonnes_non_numeriques)])
                                     
                                     return(resultats)
                                   },
                                   
                                   #donne les intervalle pour la dis avec la method topdown
                                   get_inter = function(X_sep,y){
                                     split <- topdown(cbind(X_sep[[1]],as.factor(y)), method = 1)
                                     for(i in 1:length(split)){
                                       split[[i]][1] <- 0
                                       split[[i]][length(split[[i]])] <- Inf
                                     }
                                     private$intervalle <- split
                                   },
                                   #discretise les donnees numerique du dataframe d entree
                                   discretize = function(X_sep, intervalle = private$intervalle){
                                     df <- X_sep[[1]]
                                     for(i in 1:ncol(df)){
                                       df[[i]] <- cut(df[[i]], breaks = intervalle[[i]], labels = FALSE, include.lowest = TRUE, right = TRUE)
                                     }
                                     
                                     
                                     X <- cbind(df, X_sep[[2]])
                                     
                                     
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
                                     # Combinez X et y dans un seul ensemble de données
                                     data <- cbind.data.frame(X, y)
                                     
                                     # Séparer les indices de chaque classe
                                     indices_by_class <- lapply(unique(y), function(class_label) {
                                       which(y == class_label)
                                     })
                                     
                                     # Initialiser les indices d'entraînement et de test
                                     train_indices <- test_indices <- numeric(0)
                                     
                                     # Pour chaque classe, diviser les indices en ensembles d'entraînement et de test
                                     for (class_indices in indices_by_class) {
                                       n_class <- length(class_indices)
                                       n_test_class <- round(n_class * test_size)
                                       
                                       # Sélectionner des indices aléatoires pour l'ensemble de test de cette classe
                                       test_indices_class <- sample(class_indices, n_test_class)
                                       
                                       # Ajouter les indices d'entraînement et de test pour cette classe
                                       train_indices <- c(train_indices, setdiff(class_indices, test_indices_class))
                                       test_indices <- c(test_indices, test_indices_class)
                                     }
                                     
                                     # Créer l'ensemble d'entraînement en excluant les observations de l'ensemble de test
                                     train_data <- data[train_indices, ]
                                     
                                     # Créer l'ensemble de test en sélectionnant uniquement les observations de l'ensemble de test
                                     test_data <- data[test_indices, ]
                                     
                                     # Retourner les ensembles d'entraînement et de test pour X et y
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
                                   fit = function(X,y, alpha = private$Falpha, col_to_keep = NULL){
                                     
                                     if (any(is.na(X)) || any(is.na(y))) {
                                       warning("NAs present in at least X or y. NAs have been removed.")
                                       # Vérifier les valeurs manquantes dans X et y
                                       na_rows <- complete.cases(X, y)
                                       
                                       # Filtrer X et y pour ne conserver que les lignes sans NA
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
                                     # Table de proba basique
                                     private$Fclass_name <- levels(y)
                                     private$Fclass_app <- prop.table(table(y))
                                     #on récupère les noms & les infos
                                     
                                     private$Ffeat_name <- colnames(X)
                                     private$Fdim_train <- dim(X)
                                     #Initialisation
                                     vec_final = NULL
                                     #Table de proba posterieur
                                     for (i in colnames(X)){
                                       table_i <- table(X[,i],y) + alpha
                                       table_log <- log(prop.table(table_i,margin = 2))
                                       vec_final[[i]] <- table_log
                                     }
                                     private$sample_weight <- vec_final
                                     #Return object a result for method chaining
                                     invisible(self)
                                   },
                                   #' @description
                                   #' Predict target values based whole data set.
                                   #'
                                   #' @param X (`array`)\cr  
                                   #' 2D array of shape (n_row,n_feature) with feature as column name
                                   #' 
                                   #' @return Return a list of class prediction
                                   predict_v2 = function(X){
                                     if (any(is.na(X)) || any(is.na(y))) {
                                       stop("NAs present in at least one variable of X")
                                       
                                     }
                                     
                                     if (sum( sapply(X,is.numeric))){
                                       
                                       X_sep <- private$separerColonnes(X, private$Fcol_to_keep)
                                       X <- private$discretize(X_sep)
                                     }
                                     # Fonction qui predit et qui retourne un vecteur y de shape (n_row,1)
                                     log_prob_f <- matrix(log(private$Fclass_app), nrow = nrow(X),ncol = length(private$Fclass_name), byrow = T)
                                     
                                     for (var in colnames(X)){
                                       tab_feature <- private$sample_weight[[var]]
                                       X_var_feat <- X[[var]]
                                       log_prob_f <- log_prob_f + tab_feature[X_var_feat,]
                                     }
                                     pred <- private$Fclass_name[apply(log_prob_f,1,which.max)]
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
                                     # Fonction de prediction en parallele
                                     #Number of row
                                     n <- nrow(X)
                                     # Initialise cluster
                                     clust <- parallel::makeCluster(ncores)
                                     # Split Data
                                     blocs <- split(X, 1 + (1:n) %% ncores)
                                     # res inter
                                     res_inter <- parSapply
                                     
                                     # Fonction qui predit et qui retourne un vecteur y de shape (n_row,1)
                                     y_pred <- c()
                                     y_pred <- apply(INDEX = 1:nrow(X), margin=, X = X, FUN = self$predict_on_one)
                                     return(y_pred)
                                   },
                                   predict_proba = function(){
                                     #return prediction probability
                                   },
                                   #' @description
                                   #' Use to compare test value with predict value
                                   #'
                                   #' @param X (`array`)\cr  
                                   #' 2D array of shape (n_row,n_feature) with feature as column name
                                   #' 
                                   #' @param y (`list()`)\cr  
                                   #' List of test y to evaluate the model prediction.
                                   #' 
                                   #' @param self (`array`) change to perform a self prediction, or to use result from previous prediction.
                                   #' possible value [`TRUE`, `FALSE`]
                                   #' if TRUE, X need to be a 2D array of shape (n_row,n_feature) with feature as column name
                                   #' if FALSE, X need to be a list of prediction
                                   #' 
                                   #' @param as ((`str()`)) change to get the confusion matrix, or different indicator.Default is 'table' for a confusion matrix.
                                   #' possible value ['table','accuracy']
                                   #' 
                                   #' @return Return result between y test and self$predict(X)
                                   score= function(X,y, as = 'table', self= TRUE){
                                     if(isTRUE(self)){
                                       tab <- table(y,self$predict_v2(X), dnn = c("real","predict"))
                                       if(as == 'accuracy'){
                                         accuracy <- sum(diag(tab))/sum(tab)
                                         return(accuracy)
                                       }else{
                                         
                                         return(tab)
                                       }
                                     } else if(!isTRUE(self)){
                                       tab <- table(y,X, dnn = c("real","predict"))
                                       print(class(tab))
                                       if(as == 'accuracy'){
                                         accuracy <- sum(diag(tab))/sum(tab)
                                         return(accuracy)
                                       }else{
                                         return(tab)
                                       }
                                     }
                                     ## Attention, il faut avoir fit la fonction avant, flag à faire
                                     
                                     
                                   },
                                   test = function(X,y){
                                     X_sep <- private$separerColonnes(X)
                                     private$get_inter(X_sep, y)
                                     X <- private$discretize(X_sep)
                                     return(X)
                                   }
                                   ,
                                   #' @description
                                   #' Print function for the naive bayes classifier object.
                                   #' Print parameters, number of feature and number of classe
                                   #'
                                   print = function(){
                                     cat("Training Dataset \n")
                                     cat("Value of target : ",private$Fclass_name,"\n")
                                     cat("Number of trained feature : ",private$Fdim_train[2]," and ", private$Ffeat_name,"\n")
                                     cat("Dimension of training data set : ",private$Fdim_train[1]," rows\n" )
                                   }
                                 ),
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
                                   inter = function() return(private$intervalle)
                                 )
)

