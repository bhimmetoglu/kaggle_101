# Scripts for Santander Customer Satisfaction Competition

## Algorithms used
1. XGBoost
2. Nnet
3. RandomForest
4. Support Vector Machines
5. Blended and Stacked models

## Short description of scripts
* clean_process.R: This script removes constant and duplicate columns. Then, some of the features are deskewed.
* boost_fulldat.R: XGBoost is trained on the full data using 5-fold cross-validation (CV). The determined parameters are then used to fit on all the training data and predictions on the test are performed. This results in a high score.
* nnet_fulldat.R: Training of neural network predictors. Neural networks (with one hidden layer) does not outperform XGBoost.
* rf_fulldat.R: RandomForest predictors are trained. A stratified data set is necessary for Random Forests (i.e. data sets where the observations corressponding to each type of responses are balanced). 
* svm_fulldat.R: Support Vector Mahcine (SVM) predictors are trained. SVMs also need stratified data sets. They do not perform very well.

### Stacking 
Model stacking is applied based on the models trained on the *full data*. In the *stacking* folder, we build the stacking model, and attempt to train the level 2 model. Then, in *stacking.tst0*, we stack up all the models using the full train data and then use the level2 model to predict on test data. We also considered the possibility of using simple logistic regression as the level 2 model, which indeed gave good results. 

In the stacked models, we did not incorporate SVM, since its performance was poor.  
