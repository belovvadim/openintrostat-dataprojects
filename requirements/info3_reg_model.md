# Course 3 Project: Linear Regression and Modeling

Congratulations on getting a job as a data scientist at Paramount Pictures! 

Your boss has just acquired data about how much audiences and critics like movies as well as numerous other variables about the movies. This dataset is provided below, and it includes information from [Rotten Tomatoes](https://www.rottentomatoes.com/) and [IMDB](https://www.imdb.com/) for a random sample of movies.

She is interested in learning what attributes make a movie popular. She is also interested in learning something new about movies. She wants you team to figure it all out.

As part of this project you will complete exploratory data analysis (EDA), modeling, and prediction. 

All analysis must be completed using the R programming language via RStudio, and your write up must be an R Markdown document. To help you get started we provide a template Rmd file below (see Rmd template in the Required files section below). Download this file, and fill in each section.

## Instructions

Your project will consist of 6 parts:

- **Data:** (3 points) Describe how the observations in the sample are collected, and the implications of this data collection method on the scope of inference (generalizability / causality).

- **Research question:** (3 points) Come up with a research question that you want to answer using these data and a multiple linear regression model. You should phrase your research question in a way that matches up with the scope of inference your dataset allows for. You are welcomed to create new variables based on existing ones. Along with your research question include a brief discussion (1-2 sentences) as to why this question is of interest to you and/or your audience.

- **EDA:** (10 points) Perform exploratory data analysis (EDA) that addresses the research question you outlined above. Your EDA should contain numerical summaries and visualizations. Your EDA does not have to touch on every single variable that you are considering for your model, but what you choose to present should be a deliberate (not haphazard) selection. This might mean you initially create a lot more visualizations and summary statistics than what you finally choose to include in your paper. Each R output and plot should be accompanied by a brief interpretation.

- **Modeling:** (20 points) Develop a multiple linear regression model to predict a numerical variable in the dataset. The response variable and the explanatory variables can be existing variables in the dataset, or new variables you create based on existing variables.

- **Prediction:** (5 points) Pick a movie from 2016 (a new movie that is not in the sample) and do a prediction for this movie using your the model you developed and the `predict` function in R. Also quantify the uncertainty around this prediction using an appropriate interval.

- **Conclusion:** (3 points) A brief summary of your findings from the previous sections without repeating your statements from earlier as well as a discussion of what you have learned about the data and your research question. You should also discuss any shortcomings of your current study (either due to data collection or methodology) and include ideas for possible future research.

In addition to these parts, there are also 6 points allocated to format, overall organization, and readability of your project. Total points add up to 50 points. See the assessment rubric for more details on how your peers will evaluate your work.

### More information on the data

The data set is comprised of 651 randomly sampled movies produced and released before 2016.

Some of these variables are only there for informational purposes and do not make any sense to include in a statistical analysis. It is up to you to decide which variables are meaningful and which should be omitted. For example information in the the `actor1` through `actor5` variables was used to determine whether the movie casts an actor or actress who won a best actor or actress Oscar.

You might also choose to omit certain observations or restructure some of the variables to make them suitable for answering your research questions.

When you are fitting a model you should also be careful about collinearity, as some of these variables may be dependent on each other.

Source: Rotten Tomatoes and IMDB APIs.

### More information on model selection

You may choose to use any of the model selection techniques presented in this course, however you should justify your choice. Note that there are many other model selection techniques that are beyond the scope of this course, and those should not be used in this project.

Regardless of whether you are doing forward selection or backward elimination, you should decide on a set of variables that will be considered for the model. These do not have to include all of the variables in the dataset. In fact, some variables might be completely inappropriate to consider (such as URL of the movie) or clearly not informative (such as the actor variables with so many levels). You should first go through the dataset and identify the variable you want to consider, and provide a justification for including those (or excluding the others).
