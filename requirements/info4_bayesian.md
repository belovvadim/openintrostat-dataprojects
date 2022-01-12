# Course 4 Project: Bayesian Statistics

Congratulations on getting a job as a data scientist at Paramount Pictures! 

Your boss has just acquired data about how much audiences and critics like movies as well as numerous other variables about the movies. This dataset is provided below, and it includes information from [Rotten Tomatoes](https://www.rottentomatoes.com/) and [IMDB](https://www.imdb.com/) for a random sample of movies.

Your boss is interested in learning what attributes make a movie popular. She is also interested in learning something new about movies. She wants your team to figure it all out.

As part of this project you will complete exploratory data analysis (EDA), modeling, and prediction.

The specific modeling task you need to complete is as follows: Develop a Bayesian regression model to predict audience_score from the following explanatory variables. Note that some of these variables are in the original dataset provided, and others are new variables you will need to construct in the data manipulation section using the mutate function in dplyr:

- feature_film: *"yes" if title_type is Feature Film, "no" otherwise*

- drama: *"yes" if genre is Drama, "no" otherwise*

- runtime

- mpaa_rating_R: *"yes" if mpaa_rating is R, "no" otherwise*

- thtr_rel_year

- oscar_season: *"yes" if movie is released in November, October, or December (based on thtr_rel_month), "no" otherwise*

- summer_season: *"yes" if movie is released in May, June, July, or August (based on thtr_rel_month), "no" otherwise*

- imdb_rating

- imdb_num_votes

- critics_score

- best_pic_nom

- best_pic_win

- best_actor_win

- best_actress_win

- best_dir_win

- top200_box

All analysis must be completed using the R programming language via RStudio, and your write-up must be an R Markdown document. To help you get started we provide a template Rmd file below (see Rmd template in the Required files section below). Download this file, and fill in each section.

## Instructions

Your project will consist of 6 parts:

1. **Data:** (2 points) Describe how the observations in the sample are collected, and the implications of this data collection method on the scope of inference (generalizability / causality).

2. **Data manipulation:** (10 points) Create new variables using the mutate function in the dplyr package following these guidelines:

    * Create new variable based on `title_type`: New variable should be called `feature_film` with levels yes (movies that are feature films) and no (2 pt)

    * Create new variable based on `genre`: New variable should be called `drama` with levels yes (movies that are dramas) and no (2 pt)

    * Create new variable based on `mpaa_rating`: New variable should be called `mpaa_rating_R` with levels yes (movies that are R rated) and no (2 pt)

    * Create two new variables based on `thtr_rel_month`: 

        - New variable called `oscar_season` with levels yes (if movie is released in November, October, or December) and no (2 pt)

        - New variable called `summer_season` with levels yes (if movie is released in May, June, July, or August) and no (2 pt)

3. **EDA:** (9 points) Perform exploratory data analysis (EDA) of the relationship between audience_score and the new variables constructed in the previous part. Your EDA should contain numerical summaries and visualizations. This might mean you initially create a lot more visualizations and summary statistics than what you finally choose to include in your paper. Each R output and plot should be accompanied by a brief interpretation.

4. **Modeling:** (15 points) Develop a Bayesian regression model to predict `audience_score` from the following explanatory variables. Note that some of these variables are in the original dataset provided, and others are new variables you constructed earlier:`feature_film`,` drama`, `runtime`, `mpaa_rating_R`, `thtr_rel_year`, `oscar_season`, `summer_season`, `imdb_rating`, `imdb_num_votes`, `critics_score`, `best_pic_nom`, `best_pic_win`, `best_actor_win`, `best_actress_win`, `best_dir_win`, `top200_box`. Complete Bayesian model selection and report the final model. Also perform model diagnostics and interpret coefficients of your final model in context of the data.

5. **Prediction:** (5 points) Pick a movie from 2016 (a new movie that is not in the sample) and do a prediction for this movie using your the model you developed and the `predict` function in R.

6. **Conclusion:** (3 points) A brief summary of your findings from the previous sections without
 repeating your statements from earlier as well as a discussion of what you have learned about the data and your research question. You should also discuss any shortcomings of your current study (either due to data 
collection or methodology) and include ideas for possible future research.

In addition to these parts, there are also 6 points allocated to format, overall organization, and readability of your project. Total points add up to 50 points. See the [assessment rubric](https://statswithr.github.io/projects/04_bayesian/bayesian_project_rubric.Rmd) for more details on how your peers will evaluate your work.

### More information on the data

The data set is comprised of 651 randomly sampled movies produced and released before 2016.

Some of these variables are only there for informational purposes and do not make any sense to include in a statistical analysis. It is up to you to decide which variables are meaningful and which should be omitted. For example information in the the `actor1` through `actor5` variables was used to determine whether the movie casts an actor or actress who won a best actor or actress Oscar.

You might also choose to omit certain observations or restructure some of the variables to make them suitable for answering your research questions.

When you are fitting a model you should also be careful about collinearity, as some of these variables may be dependent on each other.

Source: Rotten Tomatoes and IMDB APIs.

### More information on model selection

You may choose to use any of the Bayesian model selection techniques presented in this course, however you should justify your choice. Note that there are many other model selection techniques that are beyond the scope of this course, and those should not be used in this project.

Note that you have a very specific task on hand: predict audience_score based on the explanatory variables listed above. Also note that you first need to create some of these explanatory variables based on existing variables in the dataset.
