# nbsisemhp

## Rshiny Tuorial:

We've created an RShiny application to use our newly created package. 
The application is disponible on shinyapp.io ([lien](https://pmh-naivebayesclassifier.shinyapps.io/appr/)).
And on github ([lien](https://github.com/HugoA-A/RShiny_nbsisemhp.git))
### Interface and componant 

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/7e67c0d3-1c2f-45f5-94a8-35433ecc12ef)

Once you have launched the application, you will see the name of the application in the top left-hand corner. 
Just to the right you will see the pages available:
- Loading data : The default page when the application is launched. Allows you to load .csv or .xlsx data, select the target variable and view the loaded table.
- Data exploration : Page where you can generate statistics on the data you have previously loaded.
- Fit: Page where you can separate the data into train and test, select the explanatory variables and train the model.
- Predict : Page where you can use the previously trained model, export the results, display the accuracy and confusion matrix.

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

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/d65eb8e7-24d6-429f-945c-456c40f60a05)


The first element of the "select test set size" page allows you to choose the size of your test data set and therefore train.
By clicking on "Split data" your two previously created data sets will be split into train and test sets (4 tables) in order to stratify them and the size selected earlier.
You will then be able to select your explanatory variables.
The "train model" button will train the model by taking the selected X and y of the train dataset.

#### Predict

The predict page has two sound tabs.

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/a176fae9-ff5c-4964-8ecc-be60b3184492)

The first, "Prediction", allows you to predict the y's with the X's in the test data set. 
The results will be displayed automatically.
You can also export the results in .csv format.

![image](https://github.com/Pioterr/Sise_NaiveBayes/assets/145919293/da8adb2b-f579-4d4a-a197-6c7acbaf892d)

The second page, "Measure", allows you to calculate the accuracy score and the confusion matrix using the predictions and the y values from the test dataset.
