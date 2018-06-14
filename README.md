# Application-basis-classification

Time to have some fun with ***supervised machine learning classification and mortgage data!***

Here's the situation. I have gained access to some mortgage data for England and Wales, that looks at the purchases of residential buildings using a mortgage. In particular there is a variable `income_basis`. This tells us whether the mortgage was taken out by one person *(sole application)* or by more than one applicant *(joint applicantion)*. It's a really great indicator of housing affordability, and one I'd like to explore further in the data analysis. 

The data are monthly, going from 01-2013 to 12-2017, however for unknown reasons, this `income_basis` variable took a a sabbatical and is missing for all months in 2016. It returns for 2017. 

The dataset contains many other features, such as postcode, gross income, purchase price, loan amount, initial interest rate etc... And after tidying the dataset there are ~800,000 records in total. 

**In this project I attempt to create a model to predict the income basis (sole or joint application), to be applied to the 2016 data. We will use Python (wih Jupyter Notebooks) and SciKit-Learn to do this**


*PLEASE NOTE: DATA FOR THIS PROJECT ARE NOT AVAILABLE TO SHARE DUE TO MY AGREEMENT WITH THE DATA SOURCE.*




