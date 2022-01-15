library(keras)
library(lime)
library(tidyquant)
library(tidyr)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(readr)
library(ggplot2)
library(forcats)

churn_data_raw <- read_csv("data/IBM_Customer_churn.csv")

head(churn_data_raw)

# Remove unnecessary data
## Add Target Variable (i.e Churn to start) 
churn_data_tbl <- churn_data_raw %>%
    drop_na() %>%
    select(Churn, everything())

# Split test/training sets
library(caTools)
set.seed(100)
sample <- sample.split(churn_data_tbl$Churn, SplitRatio = 0.8) 
train_tbl_with_ids <- subset(churn_data_tbl, sample == T ) 
test_tbl_with_ids <- subset(churn_data_tbl, sample == F ) 

## Remove ID  ##

train <- select(train_tbl_with_ids, -customerID)
test <- select(test_tbl_with_ids, -customerID)

## ---- EDA ----- =##

## Bins of 12 months - detect more or less suspectiple to churn 
##ggplot(train, aes(tenure)) + geom_histogram(binwidth = 12, color = 'black', fill = 'white') 

##ggplot(train, aes(TotalCharges)) + geom_histogram(binwidth = 12, color = 'black', fill = 'white') 
## Noticed a large amount between 0 and 1000 ##


##ggplot(train, aes(log(TotalCharges))) + geom_histogram(color = 'black', fill = 'white') 
## Better distribution ## 

## ------------------------------------------##

# Determine if log transformation improves correlation 
# between TotalCharges and Churn
train %>%
    select(Churn, TotalCharges) %>%
    mutate(
        Churn = Churn %>% as.factor() %>% as.numeric(),
        LogTotalCharges = log(TotalCharges)
    ) %>%
    correlate() %>%
    focus(Churn) %>%
    fashion()

## Should improve accuracy of ANN ##

## Dummy Data ##
##Using new package ##
# Create recipe

##step_discretize() with the option = list(cuts = 6) to
##cut the continuous variable for “tenure” (number of years as a customer) to group customers into cohorts.
## Segmentation 

rec_obj <- recipe(Churn ~ ., data = train) %>%
    step_discretize(tenure, options = list(cuts = 6)) %>%
    step_log(TotalCharges) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train)

## Use recipe to bake the ML algorithim and get it ready##
x_train_tbl <- bake(rec_obj, new_data = train) %>% select(-Churn)
x_test_tbl  <- bake(rec_obj, new_data = train) %>% select(-Churn)

## Store actual variables for ANN Model 
y_train_vec <- ifelse(pull(train, Churn) == 'Yes', 1, 0)
y_test_vec  <- ifelse(pull(test, Churn) == 'Yes', 1, 0)

# Building the Artificial Neural Network


## -- Apply the layers ##
## -- 3 layer MLP ##


## layer dense = hidden layers (2) 
## apply 16 units as the nodes and select uniform and relu activation 
## use the no . columns for input shape (35) 
## ncol(x_train_tbl)
# Dropout to prevent over-fitting
##layer dropout removes weights below threshold to control over-fitting 
##rate = 0.10 to remove weights below 10%
# Dropout to prevent overfitting
##the activation = "sigmoid" (common for binary classification).##
##keras::install_keras()
library(keras)

model_keras <- keras_model_sequential()
model_keras %>% 
    layer_dense(
        units= 16, 
        kernel_initializer = "uniform", 
        activation = "relu", 
        input_shape = ncol(x_train_tbl)) %>% 
    layer_dropout(rate = 0.1) %>%
    layer_dense(
        units              = 16, 
        kernel_initializer = "uniform", 
        activation         = "relu") %>% 
    layer_dropout(rate = 0.1) %>%
    layer_dense(
        units              = 1, 
        kernel_initializer = "uniform", 
        activation         = "sigmoid") %>% 
    compile(
        optimizer = 'adam',
        loss      = 'binary_crossentropy',
        metrics   = 'accuracy'
    )

# Fit the keras model to the training data
history <- fit(
    object           = model_keras, 
    x                = as.matrix(x_train_tbl), 
    y                = y_train_vec,
    batch_size       = 50, 
    epochs           = 35,
    validation_split = 0.30,
    verbose = 0
)

# save the model
save_model_hdf5(model_keras, 'model/customer_churn.hdf5')

plot(history) +
    theme_tq() +
    scale_color_tq() +
    scale_fill_tq() +
    labs(title = "Deep Learning Training Results")

# Predicted Class Probability

yhat_keras_prob_vec  <- model_keras %>% predict(as.matrix(x_test_tbl))%>%
    as.vector()

# Predicted Class 

yhat_keras_class_vec <- model_keras %>% predict(as.matrix(x_test_tbl))
yhat_keras_class_vec <- ifelse(yhat_keras_class_vec > 0.5, 1, 0)
 
yhat_keras_class_vec <- as.factor(as.vector(yhat_keras_class_vec)) 

# Format test data and predictions for yardstick metrics
truthyvec <- as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0")

est <- estimateclassvec[1:1407]
estimateclassvec <- yhat_keras_class_vec 

estimates_keras_tbl <- tibble(
    truth      = truthyvec,
    estimate   = estimateclassvec,
    class_prob = yhat_keras_prob_vec
)


estimates_keras_tbl

estimates_keras_tbl %>% conf_mat(truth, estimate)

estimates_keras_tbl %>% metrics(truth, estimate)

estimates_keras_tbl %>% roc_auc(truth, class_prob)

options(yardstick.event_first = FALSE)
# Precision
tibble(
    precision = estimates_keras_tbl %>% precision(truth, estimate),
    recall    = estimates_keras_tbl %>% recall(truth, estimate)
)

# F1-Statistic
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)

# Setup lime::model_type() function for keras
model_type.keras.engine.sequential.Sequential <- function(x, ...) {
    "classification"
}

# Setup lime::predict_model() function for keras
predict_model.keras.engine.sequential.Sequential <- function(x, newdata, type, ...) {
    pred <- predict(object = x, x = as.matrix(newdata))
    data.frame(Yes = pred, No = 1 - pred)
}

# Test our predict_model() function
predictions <- predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
    tibble::as_tibble()

test_tbl_with_ids$churn_prob <- predictions$Yes

# Run lime() on training set
explainer <- lime::lime(
    x              = x_train_tbl, 
    model          = model_keras, 
    bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
    x_test_tbl[1,], 
    explainer    = explainer, 
    n_labels     = 1, 
    n_features   = 4,
    kernel_width = 0.5)

plot_features(explanation) +
    labs(title = "LIME Feature Importance Visualization",
         subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

plot_explanations(explanation) +
    labs(title = "LIME Feature Importance Heatmap",
         subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

save(list = ls(), file = 'data/customer_churn.RData')
