# nbsisemhp

## Introduction

This project is part of our cursus in University. 
The objectif was to create a R package to implement the Naive Bayes classifier under R6 class. We also had to create a shiny web app to show our package possibilities.
The Naive bayes classifier are probabilistic classifier that apply bayes’ theorem and add a strong independence assumption between features.
There is different type of Naïve bayes classifier, depending on the type of input data.


This package contains different methods:
* naivebayes_classifier$new() as constructor
* naivebayes_classifier$stratified_split() split the data in a stratified manner
*	naivebayes_classifier$fit():
    *	This method is used to train the model. It takes every type of data as entry and will process them. (There is a parallel version of it)
*	naivebayes_classifier$predict():
    *	This method is used for prediction and return the class prediction. (There is a parallel version of it)
    *	Several other prediction methods are available depending on what you want as output.
        * naivebayes_classifier$predict_proba_joint_log()
        * naivebayes_classifier$predict_proba_log()
        * naivebayes_classifier$predict_proba()
*	naivebayes_classifier$score() that create confusion matrix, and can give accuracy score.
*	naivebayes_classifier$plot() is used to plot feature density.
*	naivebayes_classifier$print()
*	naivebayes_classifier$summary()


## Package Installation

You can install this package directly from github
```
library(devtools)
```
```
install_github("Pioterr/nbsisemhp")
```
Once the package is downloaded with his dependencies, you can load it.
```
library(nbsisemhp)
```
Once the package is loaded, you can start to use it.

---
You can access the documentation with the command
```
?naivebayes_classifier
```



## Rshiny Tutorial:

We've created an RShiny application to use our newly created package. 

The application is disponible on shinyapp.io ([lien](https://pmh-naivebayesclassifier.shinyapps.io/appr/)).
And on github ([lien](https://github.com/HugoA-A/RShiny_nbsisemhp.git))
### Interface and componant 

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/7e67c0d3-1c2f-45f5-94a8-35433ecc12ef)

Once you have launched the application, you will see the name of the application in the top left-hand corner. 
Just to the right you will see the pages available:
- Loading data : The default page when the application is launched. Allows you to load .csv or .xlsx data, select the target variable and view the loaded table.
- Data exploration : Page where you can generate statistics on the data you have previously loaded.
- Fit: Page where you can separate the data into train and test, select the explanatory variables, train the model and plot the distribution of y for each class of X.
- Predict : Page where you can use the previously trained model, export the results, display the accuracy and confusion matrix and calaculate the probability of belonging to each class for each observation.

We will now go into more detail on each of these pages.

### Detailed presentation

To demonstrate the application's functionality, we're going to use the iris commposer dataset with 150 observations and 5 variables.

#### Loading data

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/7900c82e-721d-4837-b9e2-48dd51228922)


The "Browse..." button lets you select a file (only .csv and .xlsx files are accepted). Once selected, the file is loaded directly and a drop-down list appears.
The drop-down list (Choose your target variable) allows you to choose your target variable. In the case of the iris dataset, this is the 'Species' variable.
The "Validate" button allows you to validate your choice and will separate your dataset into two subsets. One for your explanatory variables and one for your target variable.
The "View data" button allows you to display the first n rows of the sub-datasets created and thus check that everything is in order. 
The n number of rows can be modified in "Number of rows".

#### Data exploration

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/a12a7b80-e1af-440a-8b00-f0b911c081e6)


By clicking on generate statistics a table will appear with different descriptive statistics for your explanatory variables.

#### Fit

The fit page has two tabs.

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/d65eb8e7-24d6-429f-945c-456c40f60a05)

For the first tab : Fit
The first element of the "select test set size" page allows you to choose the size of your test data set and therefore train.
By clicking on "Split data" your two previously created data sets will be split into train and test sets (4 tables) in order to stratify them and the size selected earlier.
You will then be able to select your explanatory variables.
The "train model" button will train the model by taking the selected X and y of the train dataset.

<img width="959" alt="image" src="https://github.com/Pioterr/nbsisemhp/assets/145919293/56d4ff63-99b5-432c-bf0c-c121649e7215">

For the second tab: Plot
By clicking on "Show plot", plots will appear showing the distribution of classes of y in each variable of X and for each of their classes. The legend is accessible by passing the mouse over the plot.

#### Predict

The predict page has three tabs.

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/a176fae9-ff5c-4964-8ecc-be60b3184492)

The first, "Prediction", allows you to predict the y's with the X's in the test data set. 
The results will be displayed automatically.
You can also export the results in .csv format.

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/da8adb2b-f579-4d4a-a197-6c7acbaf892d)

The second page, "Measure", allows you to calculate the accuracy score and the confusion matrix using the predictions and the y values from the test dataset.

<img width="947" alt="image" src="https://github.com/Pioterr/nbsisemhp/assets/145919293/6728d2bc-1274-4131-874c-d5715b1839af">

The third page, "Probability", allows you to calculate the probabilties of belonging to each class for each observation. This is calculated usin the test dataset generated before.

