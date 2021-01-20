
#' @title  Building the machine learning model for product classification
#'
#' @description This function provides a trained machine learning model to classify products into coicop groups. In addition, the function returns the characteristics of the model and figures describing the learning process.
#' @param data_train Training data set for the model. This set must contain all the columns defined by the \code{indicators} parameter and the \code{coicop} column (with matched coicop groups to all products). If the \code{key_words} vector is non-empty, the set should also contain a \code{description} column. Ideally, the indicators should be of the numerical type. If the indicator is not of the numerical type, it will be converted to this type.
#' @param data_test A test set that is used to validate the machine learning model. This set should have the same structure as the training set, but it is not obligatory. If the test set is not specified by the user then the test set is drawn from the training set (see \code{p} parameter).
#' @param indicators A vector of column names to be considered in building a machine learning model.
#' @param key_words A vector of keywords or phrases that will be recognized in the \code{description} column. For each such keyword and or phrase, a new binary variable (column) will be created and included in the machine model training process.
#' @param sensitivity A logical parameter that indicates whether lowercase or uppercase letters are to be distinguished when the \code{key_words} vector is not empty.
#' @param p A parameter related to creating the testing set, if it has not been specified by the user. The test set is then created on the basis of a coicop-balanced subsample of the training set. The size of this subsample is 100p percents of the training set size.
#' @param w A parameter for determining the measure of choosing the optimal machine learning model. For each combination of parameters specified in the \code{grid} list, the error rate of the trained model is calculated on the basis of the error on the training set (error_L=1-accuracy_L) and the error on the testing set (error_T=1-accuracy_T). Final error rate is estimated as: \code{w accuracy_L + (1-w) accuracy_T}.
#' @param rounds The maximum number of iterations during the training stage.
#' @param grid The list of vectors of parameters which are taken into consideration during the \code{Extreme Gradient Boosting training}. The default value of this list is as follows: \code{grid=list(eta=c(0.05,0.1,0.2),max_depth=c(6),min_child_weight=c(1),max_delta_step=c(0),subsample=c(1),gamma=c(0),lambda=c(1),alpha=c(0)}. The complete list of parameters for the used \code{Tree Booster} is available online \href{ https://xgboost.readthedocs.io/en/latest/parameter.html }{here}.
#' @rdname model_classification
#' @return In general, this function provides a trained machine learning model to classify products into coicop groups. In addition, the function returns the characteristics of the model and figures describing the learning process. The machine learning process is based on the \code{XGBoost} algorithm (from the \code{XGBoost} package) which is an implementation of gradient boosted decision trees designed for speed and performance. The function takes into account each combination of model parameters (specified by the \code{grid} list) and provides, inter alia, an optimally trained model (a model that minimizes the error rate calculated on the basis of a fixed value of the \code{w} parameter). After all, the function returns a list of the following objects: \code{model} - the optimally trained model; \code{best_parameters} - a set of parameters of the optimal model;  \code{indicators} - a vector of all indicators used; \code{key_words} - a vector of all key words and phrases used; \code{coicops} - a dataframe with categorized COICOPs; \code{sensitivity} - a value of the used 'sensitivity' parameter; \code{figure_training} - a plot of the error levels calculated for the training set and the tetsing set during the learning process of the returned model (error = 1 - accuracy); \code{figure_importance} - a plot of the relative importance of the used indicators. 
#' @references
#' {Tianqi Chen and Carlos Guestrin (2016). \emph{XGBoost: A Scalable Tree Boosting System}. 22nd SIGKDD Conference on Knowledge Discovery and Data Mining.}
#' 
#' @examples 
#' \donttest{my.grid=list(eta=c(0.01,0.02,0.05),subsample=c(0.5))}
#' \donttest{data_train<-dplyr::filter(dataCOICOP,dataCOICOP$time<=as.Date("2020-08-01"))}
#' \donttest{data_test<-dplyr::filter(dataCOICOP,dataCOICOP$time>as.Date("2020-08-01"))}
#' \donttest{ML<-model_classification(data_train,data_test,grid=my.grid,
#' indicators=c("prodID","unit","description"),key_words=c("milk"),rounds=50)}
#' \donttest{ML$best_parameters}
#' \donttest{ML$indicators}
#' \donttest{ML$figure_training} 
#' \donttest{ML$figure_importance}
#' @export

model_classification <-
  function(data_train = data.frame(),
  data_test = data.frame(),
  indicators = c(),
  key_words = c(),
  sensitivity = TRUE,
  p = 0.9,
  w = 0.2,
  rounds = 200,
  grid = list())
  {
  #default value of 'grid'
  grid_default <-
  list(
  eta = c(0.05, 0.1, 0.2),
  max_depth = c(6),
  min_child_weight = c(1),
  max_delta_step = c(0),
  subsample = c(1),
  gamma = c (0),
  lambda = c(1),
  alpha = c(0)
  )
  ng_ok <- names(grid_default)
  ng <- names(grid)
  if (length(ng) > 0) {
  for (i in 1:length(ng)) {
  if (ng[i] %in% ng_ok)
  grid_default[[ng[i]]] <- grid[[ng[i]]]
  else
  stop(paste("The 'grid' list does not include the parameter: ", ng[i]))
  }
  }
  grid <- grid_default
  #checking parameters
  if (nrow(data_train) == 0)
  stop("There is no data_train among parameters or the data_train set is empty!")
  if ((length(indicators) + length(key_words)) == 0)
  stop ("The model cannot be built due to the lack of indicators")
  cn <- colnames(data_train)
  if (!("coicop" %in% cn))
  stop("A column 'coicop' is missing")
  if (("description" %in% indicators) | (length(key_words) >= 1)) {
  if (!(("description" %in% cn)))
  stop("A column 'description' is missing!")
  }
  description <-
  coicop <- iterations <- value <- error <- Feature <- Importance <-
  NULL
  if (nrow(data_test) == 0) {
  #Creating data_train and data_test
  trainIndex <-
  caret::createDataPartition(data_train$coicop, p = p, list = FALSE)
  data_test <- data_train[-trainIndex, ]
  data_train <- data_train[trainIndex, ]
  } else
  {
  cn <- colnames(data_test)
  if (!("coicop" %in% cn))
  stop("A column 'coicop' is missing")
  if (("description" %in% indicators) | (length(key_words) >= 1)) {
  if (!(("description" %in% cn)))
  stop("A column 'description' is missing!")
  }
  }
  #lower/uppar cases
  if (sensitivity == TRUE) {
  if (("description" %in% indicators) | (length(key_words) >= 1))
  {
  data_train$description <- tolower(data_train$description)
  data_test$description <- tolower(data_test$description)
  }
  if (length(key_words) >= 1)
  key_words <- tolower(key_words)
  
  }
  
  #data_train reduction
  data_train <-
  dplyr::select(data_train, indicators, description, coicop)
  data_train <- dplyr::distinct(data_train)
  
  #data_test reduction
  data_test <- dplyr::select(data_test, indicators, description, coicop)
  data_test <- dplyr::distinct(data_test)
  
  #coicops
  #Let us remember the oryginal names of coicops from data_train
  coicop_oryg <- unique(data_train$coicop)
  coicop_num <- unique(as.numeric(as.factor(data_train$coicop)))
  coicops <- data.frame(coicop_oryg, coicop_num)
  
  #data_train preparation
  Y <- as.numeric(as.factor(data_train$coicop)) - 1
  N <- length(unique(data_train$coicop))
  data_train$coicop <- NULL
  if (length(key_words) >= 1)
  for (i in 1:length(key_words)) {
  var <-
  stringr::str_detect(data_train$description, pattern = key_words[i])
  data_train[, key_words[i]] <-
  as.numeric(var)
  }
  
  if (!("description" %in% indicators))
  data_train$description <- NULL
  cols_train <- colnames(data_train)
  for (i in 1:length(cols_train))
  if (is.numeric(data_train[, cols_train[i]]) == 0)
  data_train[, cols_train[i]] <-
  as.numeric(as.factor(data_train[, cols_train[i]]))
  X <- data.matrix(data_train)
  
  #data_test preparation
  Y_test <- as.numeric(as.factor(data_test$coicop)) - 1
  data_test$coicop <- NULL
  if (length(key_words) >= 1)
  for (i in 1:length(key_words)) {
  var <-
  stringr::str_detect(data_test$description, pattern = key_words[i])
  data_test[, key_words[i]] <-
  as.numeric(var)
  }
  
  if (!("description" %in% indicators))
  data_test$description <- NULL
  cols_test <- colnames(data_test)
  for (i in 1:length(cols_test))
  if (is.numeric(data_test[, cols_test[i]]) == 0)
  data_test[, cols_test[i]] <-
  as.numeric(as.factor(data_test[, cols_test[i]]))
  X_test <- data.matrix(data_test)
  
  #training and testing sets
  train.mat <- xgboost::xgb.DMatrix(data = X, label = Y)
  test.mat <- xgboost::xgb.DMatrix(data = X_test, label = Y_test)
  
  models <-
  list() #list of created models (best for the given set of parameters)
  errors <- c() #vector of accuracy measures
  parameter_list <- list()
  
  for (p1 in 1:length(grid$eta))
  for (p2 in 1:length(grid$max_depth))
  for (p3 in 1:length(grid$min_child_weight))
  for (p4 in 1:length(grid$max_delta_step))
  for (p5 in 1:length(grid$subsample))
  for (p6 in 1:length(grid$gamma))
  for (p7 in 1:length(grid$lambda))
  for (p8 in 1:length(grid$alpha))
  {
  #treining
  param <- list(
  objective = "multi:softmax",
  num_class = N,
  booster = "gbtree",
  eval_metric = "merror",
  eta = grid$eta[p1],
  max_depth = grid$max_depth[p2],
  min_child_weight = grid$min_child_weight[p3],
  max_delta_step = grid$max_delta_step[p4],
  subsample = grid$subsample[p5],
  gamma = grid$gamma[p6],
  lambda = grid$lambda[p7],
  apha = grid$alpha[p8]
  )
  
  #fitting the model
  xgb.fit <- xgboost::xgb.train(
  params = param,
  data = train.mat,
  nrounds = rounds,
  early_stopping_rounds = 50,
  watchlist = list(train = train.mat, test = test.mat)
  )
  #best iteration
  train_error <-
  xgb.fit$evaluation_log[xgb.fit$best_iteration, ]$train_merror
  test_error <-
  xgb.fit$evaluation_log[xgb.fit$best_iteration, ]$test_merror
  accuracy <- w * train_error + (1 - w) * test_error
  
  #buiulding a list of models and a vector of their errors
  models[[length(models) + 1]] <- xgb.fit
  errors <- c(errors, accuracy)
  parameter_list[[length(parameter_list) + 1]] <-
  list(
  p = p,
  w = w,
  rounds = rounds,
  eta = grid$eta[p1],
  max_depth = p2,
  min_child_weight = grid$min_child_weight[p3],
  max_delta_step = grid$max_delta_step[p4],
  subsample = grid$subsample[p5],
  gamma = grid$gamma[p6],
  lambda = grid$lambda[p7],
  alpha = grid$alpha[p8]
  )
  }
  
  #taking the best of best models :)
  best <- min(which(errors == min(errors)))
  best_parameters <- unlist(parameter_list[[best]])
  xgb.fit <- models[[best]]
  
  #figure for comparison of errors in iterations in training and testing sets
  iter <- xgb.fit$evaluation_log$iter
  data_train_error <- xgb.fit$evaluation_log$train_merror
  data_test_error <- xgb.fit$evaluation_log$test_merror
  df_errors <- data.frame(iter, data_train_error, data_test_error)
  df_errors <- reshape::melt(df_errors, id.var = 'iter')
  colnames(df_errors) <- c("iterations", "error", "value")
  figure_training <-
  ggplot2::ggplot(df_errors, ggplot2::aes(x = iterations, y = value, col =
  error)) + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
  ggplot2::geom_point(size = 0.9) + ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::labs(y = "error value")
  
  #importance of factors
  impMatrix <- data.frame(xgboost::xgb.importance(model = xgb.fit))
  df_imp <-
  data.frame(Feature = impMatrix$Feature, Importance = impMatrix$Gain)
  figure_importance <-
  ggplot2::ggplot(data = df_imp, ggplot2::aes(x = Feature, y = Importance)) +
  ggplot2::coord_flip() +
  ggplot2::geom_bar(stat = "identity",
  fill = "grey",
  color = "black") + ggplot2::labs(x = "Relative importance")
  
  return (
  list(
  model = xgb.fit,
  best_parameters = best_parameters,
  indicators = indicators,
  key_words = key_words,
  coicops = coicops,
  sensitivity = sensitivity,
  figure_training = figure_training,
  figure_importance = figure_importance
  )
  )
  }
  

#' @title  Predicting product COICOP levels via the machine learning model
#'
#' @description This function predicts product COICOP levels via the selected machine learning model.
#' @param model A list of 8 elements which identify the previously built machine learning model (the list is obtained via the \code{model_classification} function).
#' @param data A data set for the model (products with their characteristics). This data set must contain all the columns which were used in the built model. 
#' @rdname data_classifying
#' @return This function provides the indicated data set with an additional column, i.e. \code{coicop_predicted}, which is obtained by using the selected machine learing model.  
#' @examples 
#' #Building the model
#' \donttest{my.grid=list(eta=c(0.01,0.02,0.05),subsample=c(0.5))}
#' \donttest{data_train<-dplyr::filter(dataCOICOP,dataCOICOP$time<=as.Date("2020-08-01"))}
#' \donttest{data_test<-dplyr::filter(dataCOICOP,dataCOICOP$time>as.Date("2020-08-01"))}
#' \donttest{ML<-model_classification(data_train,data_test,grid=my.grid,
#' indicators=c("prodID","unit","description"),key_words=c("milk"),rounds=50)}
#' #Data classification
#' \donttest{data_classifying(ML, data_test)}
#' @export

data_classifying <- function (model = list(), data)
{
description <- NULL
if (nrow(data) == 0)
stop("The dataset is empty")
if (!(length(model) == 8))
stop ("The model appears to be incomplete")
#taking model parameters
model_used <- model$model
indicators <- model$indicators
key_words <- model$key_words
coicops <- model$coicops
sensitivity <- model$sensitivity

#preparing data into the right form
data_new <- dplyr::select(data, indicators, description)
#lower/uppar cases

if (sensitivity == TRUE) {
if (("description" %in% indicators) | (length(key_words) >= 1))
data_new$description <- tolower(data_new$description)

if (length(key_words) >= 1)
key_words <- tolower(key_words)

}
if (length(key_words) >= 1)
for (i in 1:length(key_words))
{
var <-
stringr::str_detect(data_new$description,  pattern = key_words[i])
data_new[, key_words[i]] <- as.numeric(var)
}
if (!("description" %in% indicators))
data_new$description <- NULL
cols_new <- colnames(data_new)
for (i in 1:length(cols_new))
if (is.numeric(data_new[, cols_new[i]]) == 0)
data_new[, cols_new[i]] <-
as.numeric(as.factor(data_new[, cols_new[i]]))
data_new <- data.matrix(data_new)
predictions <- stats::predict(model_used, data_new) + 1
predictions_new <- c()
for (i in 1:length(predictions)) {
no <- which(coicops$coicop_num == predictions[i])
predictions_new <-
c(predictions_new, as.vector(coicops$coicop_oryg)[no])
}

data$coicop_predicted <- as.factor(predictions_new)
return (data)
}


#' @title  Saving the machine learning model on the disk
#'
#' @description This function saves a list of machine learning model elements on the disk, i.e. the resulting 8 files are written.
#' @param model A list of 8 elements which identify the previously built machine learning model (the list is obtained via the \code{model_classification} function).
#' @param dir The name of the directory where the selected model should be saved. The directory with all necessary files will be created in the working directory.
#' @rdname save_model
#' @return This function saves a list of ML model elements on the disk, i.e. the resulting 8 files are written into the new directory specified by \code{dir}. The list should be obtained previously using the \code{model_classification} function. After saving the model, it can be loaded at any time by using the \code{load_model} function.  
#' @examples 
#' #Building the model
#' \donttest{my.grid=list(eta=c(0.01,0.02,0.05),subsample=c(0.5))}
#' \donttest{data_train<-dplyr::filter(dataCOICOP,dataCOICOP$time<=as.Date("2020-08-01"))}
#' \donttest{data_test<-dplyr::filter(dataCOICOP,dataCOICOP$time>as.Date("2020-08-01"))}
#' \donttest{ML<-model_classification(data_train,data_test,grid=my.grid,
#' indicators=c("prodID","unit","description"),key_words=c("milk"),rounds=50)}
#' #Saving the model
#' \donttest{save_model(ML, dir="My_model")}
#' @export

save_model <- function (model = list(), dir = "ML_model")
{
if (!(length(model) == 8))
stop ("The model appears to be incomplete")
#taking model parameters
model_used <- model$model
best_parameters <- model$best_parameters
indicators <- model$indicators
key_words <- model$key_words
coicops <- model$coicops
sensitivity <- model$sensitivity
figure_training <- model$figure_training
figure_importance <- model$figure_importance

#saving
dir.create(dir, showWarnings = FALSE)
path = paste(dir, "/", sep = "")
xgboost::xgb.save(model_used, paste(path, "my.model", sep = ""))
saveRDS(best_parameters, paste(path, "best_parameters.RDS", sep = ""))
saveRDS(indicators, paste(path, "indicators.RDS", sep = ""))
saveRDS(key_words, paste(path, "key_words.RDS", sep = ""))
saveRDS(coicops, paste(path, "coicops.RDS", sep = ""))
saveRDS(sensitivity, paste(path, "sensitivity.RDS", sep = ""))
saveRDS(figure_training, paste(path, "figure_training.RDS", sep = ""))
saveRDS(figure_importance,
paste(path, "figure_importance.RDS", sep = ""))
}

#' @title  Loading the machine learning model from the disk
#'
#' @description This function loads a list of machine learning model elements from the disk, i.e. the needed 8 files are read.
#' @param dir The name of the directory from which the machine learning model is to be loaded. The directory must be in the working directory.
#' @rdname load_model
#' @return This function loads a list of ML model elements from the disk, i.e. the needed 8 files are read from the directory selected by \code{dir}. After loading the model it can be used for product classification by using \code{data_classifying} function.  
#' @examples 
#' #Building the model
#' \donttest{my.grid=list(eta=c(0.01,0.02,0.05),subsample=c(0.5))}
#' \donttest{data_train<-dplyr::filter(dataCOICOP,dataCOICOP$time<=as.Date("2020-08-01"))}
#' \donttest{data_test<-dplyr::filter(dataCOICOP,dataCOICOP$time>as.Date("2020-08-01"))}
#' \donttest{ML<-model_classification(data_train,data_test,grid=my.grid,
#' indicators=c("prodID","unit","description"),key_words=c("milk"),rounds=50)}
#' #Saving the model
#' \donttest{save_model(ML, dir="My_model")}
#' #Loading the model
#' \donttest{ML_fromPC<-load_model("My_model")}
#' #COICOP predicting
#' \donttest{data_classifying(ML_fromPC, data_test)}
#' @export

#' @export
load_model <- function (dir = "ML_model")
{
path = paste(dir, "/", sep = "")
#loading model parameters
model_used <- xgboost::xgb.load(paste(path, "my.model", sep = ""))
best_parameters <- readRDS(paste(path, "best_parameters.RDS", sep = ""))
indicators <- readRDS(paste(path, "indicators.RDS", sep = ""))
key_words <- readRDS(paste(path, "key_words.RDS", sep = ""))
coicops <- readRDS(paste(path, "coicops.RDS", sep = ""))
sensitivity <- readRDS(paste(path, "sensitivity.RDS", sep = ""))
figure_training <- readRDS(paste(path, "figure_training.RDS", sep = ""))
figure_importance <-
readRDS(paste(path, "figure_importance.RDS", sep = ""))
return (
list(
model = model_used,
best_parameters = best_parameters,
indicators = indicators,
key_words = key_words,
coicops = coicops,
sensitivity = sensitivity,
figure_training = figure_training,
figure_importance = figure_importance
)
)
}



