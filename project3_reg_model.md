Prediction of IMDB Movie Scores (Not Using RT)
================
Vadim Belov

-   [Setup](#setup)
    -   [Task](#task)
    -   [Load packages](#load-packages)
    -   [Load data](#load-data)
-   [Part 1: Data](#part-1-data)
-   [Part 2: Research Question](#part-2-research-question)
-   [Part 3: Exploratory Data
    Analysis](#part-3-exploratory-data-analysis)
    -   [Target Variable(s)](#target-variables)
    -   [Missing Values](#missing-values)
    -   [Numerical Predictors](#numerical-predictors)
    -   [Categorical Predictors](#categorical-predictors)
    -   [Non-Linear Effects](#non-linear-effects)
-   [Part 4: Modeling](#part-4-modeling)
    -   [1st Strategy: Keep Factors
        (“Nominal”)](#1st-strategy-keep-factors-nominal)
    -   [2nd Strategy: Expand Factor Levels
        (“Effective”)](#2nd-strategy-expand-factor-levels-effective)
    -   [Model Interpretation and
        Diagnostics](#model-interpretation-and-diagnostics)
-   [Part 5: Prediction](#part-5-prediction)
-   [Part 6: Conclusion](#part-6-conclusion)

## Setup

Some code chunks are hidden by default (in the HTML version), for easier
reading. You can freely unfold them at will, by clicking on the “code”
tab.

### Task

Describe movies dataset and perform EDA. Develop a multiple linear
regression model to predict a numerical variable, that describes movie’s
popularity. Do a prediction for previously unseen data (cf. [project
requirements](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info3_reg_model.md)).

### Load packages

``` r
library(ggplot2)    # graphics
library(gridExtra)  # multiple side-by-side ggplot's
library(GGally)     # pairwise correlation plots
library(dplyr)      # data manipulation
library(tidyr)      # data cleaning and representation change
library(forcats)    # modify factor levels
library(e1071)      # compute central moments (skewness and kurtosis)
library(leaps)      # model selection tools
library(knitr)      # report generation
library(kableExtra) # customized tables
```

### Load data

``` r
load("data/movies.Rdata")
```

------------------------------------------------------------------------

## Part 1: Data

The data set is comprised of 651 randomly sampled movies, produced and
released before 2016. The information on the movies has been gathered,
in particular, from [Rotten Tomatoes](https://www.rottentomatoes.com/)
and [IMDB](https://www.imdb.com/).

**Generalizability:** Random sampling implies that the results of
analysis, derived from these data, can be generalized to the population
of interest (movies released before 2016).

**Causality**: Since no random assignment has been employed, this study
is purely *observational* in nature and does not guarantee causal
inferences. Strictly speaking, one can only identify the possible
association between variables (but not necessarily the causal
dependencies), using variation in the collected data samples.

There are not enough details on the data collection to judge about the
possible sources of biases. The quality and fullness of the database is
crucial here, as only the movies which have a record are available for
the study.

------------------------------------------------------------------------

## Part 2: Research Question

We are interested in learning what attributes make a movie popular. In
particular, **which features are most useful for predicting success of
the movie, for instance, judged by the audience reception?** *(That is,
we want to predict the movies score, based on its attributes; but we
would also like to find a reasonably “small” models, which are good at
explaining the relationship between the response and the predictors.)*
This can present an interest as for regular movie-goers, but also serve
as useful information and important incentive for studio executives, or
people in the industry. Lets peek at our variables:

``` r
names(movies)
```

    ##  [1] "title"            "title_type"       "genre"            "runtime"         
    ##  [5] "mpaa_rating"      "studio"           "thtr_rel_year"    "thtr_rel_month"  
    ##  [9] "thtr_rel_day"     "dvd_rel_year"     "dvd_rel_month"    "dvd_rel_day"     
    ## [13] "imdb_rating"      "imdb_num_votes"   "critics_rating"   "critics_score"   
    ## [17] "audience_rating"  "audience_score"   "best_pic_nom"     "best_pic_win"    
    ## [21] "best_actor_win"   "best_actress_win" "best_dir_win"     "top200_box"      
    ## [25] "director"         "actor1"           "actor2"           "actor3"          
    ## [29] "actor4"           "actor5"           "imdb_url"         "rt_url"

Unfortunately, there is no information in the dataset about box office
or video sales (except only the binary `top200_box` variable), which
would be of prime importance to guide financial, marketing, or artistic
decisions. Instead, as a measure of the movie’s success we can use its
scoring on film review aggregator websites, represented by 3 numerical
(`imdb_rating`, `audience_score`, `critics_score`) and 2 categorical
(`audience_rating`, `critics_rating`) variables. We may either choose
one of them as our target response variable, or create an aggregated
one.

------------------------------------------------------------------------

## Part 3: Exploratory Data Analysis

### Target Variable(s)

First, let us inspect how various measures of the movie’s success relate
to each other. The summary statistics for the candidate variables are as
follows:

``` r
movie_scores <- movies %>% 
  select(imdb_rating, audience_score, critics_score, audience_rating, critics_rating)
summary(movie_scores)
```

    ##   imdb_rating    audience_score  critics_score    audience_rating
    ##  Min.   :1.900   Min.   :11.00   Min.   :  1.00   Spilled:275    
    ##  1st Qu.:5.900   1st Qu.:46.00   1st Qu.: 33.00   Upright:376    
    ##  Median :6.600   Median :65.00   Median : 61.00                  
    ##  Mean   :6.493   Mean   :62.36   Mean   : 57.69                  
    ##  3rd Qu.:7.300   3rd Qu.:80.00   3rd Qu.: 83.00                  
    ##  Max.   :9.000   Max.   :97.00   Max.   :100.00                  
    ##          critics_rating
    ##  Certified Fresh:135   
    ##  Fresh          :209   
    ##  Rotten         :307   
    ##                        
    ##                        
    ## 

``` r
# Draw pairwise associations
ggpairs(
  movie_scores, 
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1))
) +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    axis.text = element_text(size = 8)
  ) +
  labs(title = "Comparison of Ratings at IMDB and RT")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/pairwise-scores-1.png" width="90%" style="display: block; margin: auto;" />

For completeness, we have also included categorical RT ratings in a
pairwise plot. From the side-by-side boxplots, the audience rating seems
to be just the threshold on the audience score, and similarly the
“Rotten” rating from critics. However critics’ scores for “Fresh” and
“Certified Fresh” categories overlap quite a bit. In any case, as a
target variable, the classification by discrete ratings with few levels
(e.g., using logistic regression) seems to be more restrictive than the
estimate for a numeric score, that takes continuous range of values. In
principle, one could use discrete RT ratings as quite reliable
predictors of a movie score. However, it would be awkward to make a
“prediction” of one form of rating based on the other. *We, therefore,
choose to challenge ourselves and omit those discrete variables.*

One can see the high correlation 0.865 between IMDB and RT’s audience
scores, as well as slightly lower (but still quite strong 0.765 and
0.704) correlations with critics’ score. In addition, we can ask how
much do various scores differ across movies. For instance, calculate the
sample proportion of movies, for which two audience scores (properly
normalized) differ, say, by 20%:

``` r
# To be able to compare, first normalize scores so that they lie in the same range 
normalize <- function(x) ( x - min(x) ) / ( max(x) - min(x) )
normalized_scores <- select_if(movie_scores, is.numeric) %>% 
  mutate( across(everything(), normalize) )
sum(abs(normalized_scores$imdb_rating - normalized_scores$audience_score) > 0.2) / nrow(movie_scores)
```

    ## [1] 0.1658986

These results are reassuring, since we can assume that movies are rated
more or less coherently by different sources. From the diagonal plots,
the distributions of `audience_score` and `crtics_score` are strongly
left-skewed with two pronounced “humps” (bi-modal). **We choose
`imdb_rating` as our response variable**, since its distribution appears
to be closer to the normal “bell-shaped” curve. Though, it still
demonstrates a noticeable left skew:

``` r
( skew <- e1071::skewness(movies$imdb_rating) )
```

    ## [1] -0.9012715

We perform some visual inspection to see how strongly IMDB scores
deviate from the normal distribution.

``` r
histogram_imdb <- ggplot(movies, aes(x = imdb_rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(movies$imdb_rating), sd = sd(movies$imdb_rating))
  ) + 
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  scale_x_continuous(breaks = seq(2, 9, by = 0.5), expand = c(0.02, 0.02)) + 
  theme(panel.grid.major.x = element_blank(), legend.position = c(0.2, 0.9)) +
  annotate("text", x = 3.3, y = 0.315, label = paste("Skewness:", round(skew, 3))) +
  labs(title = "Histogram", x = "IMDB rating", y = "Probability Density")
qqnorm_imdb <- ggplot(movies, aes(sample = imdb_rating)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
grid.arrange(histogram_imdb, qqnorm_imdb, ncol = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/IMDB-skew-1.png" width="90%" style="display: block; margin: auto;" />
From the quantile-quantile plot, the data in our sample does not satisfy
the normality assumption. However, we can rectify this and **derive the
new score `imdb_norm`**, by performing a form of log-transformation on
our data.

``` r
transform_response <- function(x, fixed_range = TRUE) {
  max_x <- ifelse(fixed_range, 9, max(x))   # sample range is sensitive to outliers
  return( log10( max_x + 1 - x ) )
}
get_imdb <- function(y) 10 - 10 ^ y   # inverse transformation to IMDB scale [1,10]
ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + 
  stat_function(fun = get_imdb) + 
  labs(title = "Transformation of Response Variable", x = "New Scale", y = "IMDB Scale")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/imdb-transformation-1.png" width="90%" style="display: block; margin: auto;" />

We can derive the original IMDB rating from the predicted `imdb_norm`
score, using the inverse formula:  

![\\mathtt{imdb\\\_rating} = 10 - 10^\\mathtt{imdb\\\_norm}](https://latex.codecogs.com/png.latex?%5Cmathtt%7Bimdb%5C_rating%7D%20%3D%2010%20-%2010%5E%5Cmathtt%7Bimdb%5C_norm%7D "\mathtt{imdb\_rating} = 10 - 10^\mathtt{imdb\_norm}")

Given possible values of the IMDB rating
![\[1,10\]](https://latex.codecogs.com/png.latex?%5B1%2C10%5D "[1,10]"),
the new score varies in the range
![(-\\infty, 1\]](https://latex.codecogs.com/png.latex?%28-%5Cinfty%2C%201%5D "(-\infty, 1]")
(unbounded from below). One could have adjusted it to lie between 0 and
1, at the expense of slightly less intuitive expression and data being
less “normal-like”. There is little gain though, since theoretically
best rating of 10 (corresponding to arbitrarily small `imdb_norm`) is
practically unattainable anyway. Indeed, the № 1 movie on the
[list](https://www.imdb.com/search/title/?groups=top_100&sort=user_rating,desc&view=simple)
is rated 9.3, which corresponds to `imdb_norm = -0.155`.

Lets check that the new scores are indeed normally distributed.

``` r
imdb_norm <- transform_response(movies$imdb_rating)
histogram_imdb <- ggplot(as.data.frame(imdb_norm), aes(x = imdb_norm)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(imdb_norm), sd = sd(imdb_norm))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate(
    "text", x = 0.15, y = 2.7,
    label = paste("Skewness:", round(skewness(imdb_norm), digits = 3))
  ) +
  theme(legend.position = c(0.2, 0.9)) +
  labs(title = "Histogram", x = "Transformed IMDB Rating", y = "Probability Density")
qqnorm_imdb <- ggplot(as.data.frame(imdb_norm), aes(sample = imdb_norm)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
grid.arrange(histogram_imdb, qqnorm_imdb, ncol = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/IMDB-normal-1.png" width="90%" style="display: block; margin: auto;" />

The data appears almost symmetric now, and, judging by the Q-Q plot, it
largely follows the normal distribution. Even though for a large sample
size (n \> 200), statistical tests can detect even small but meaningless
departures from normality, we can nevertheless perform the [Shapiro-Wilk
test](https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test), for
instance:

``` r
shapiro.test(imdb_norm)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  imdb_norm
    ## W = 0.9957, p-value = 0.06999

Using the concordance significance level
![\\alpha = 0.05](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%200.05 "\alpha = 0.05"),
the high p-value corroborates the null-hypothesis, namely, that the
population is normally distributed. Compare this to the results of the
test, performed on the original un-transformed data:

``` r
shapiro.test(movies$imdb_rating)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  movies$imdb_rating
    ## W = 0.95476, p-value = 2.98e-13

We construct the design matrix to include only relevant target variable
`imdb_norm` and all potentially useful predictors, that will be
considered for the model. Therefore, we can safely omit dummy variables
(like `title` or URL) and those clearly useless for prediction (such as
actor or director), that cannot be generalized to the population.

``` r
# During analysis, I have identified a couple of genre mis-classifications (there are probably more)
movies[movies$title == "Bats", "genre"] <- "Horror"
movies[movies$title == "Battlefield Earth", "genre"] <- "Science Fiction & Fantasy"
movies[movies$title == "Epic Movie", "genre"] <- "Comedy"
movies[movies$title == "Vampire Hunter D: Bloodlust", "genre"] <- "Animation"
movies[movies$title == "Viva Knievel!", "genre"] <- "Action & Adventure"
design <- movies %>% 
  select(-c(title, studio, imdb_rating, critics_rating:audience_score, director:rt_url)) %>% 
  mutate(imdb_norm = imdb_norm, .before = 1)   # target variable on the first position
```

``` r
glimpse(design)
```

    ## Rows: 651
    ## Columns: 18
    ## $ imdb_norm        <dbl> 0.6532125, 0.4313638, 0.3802112, 0.4471580, 0.6901961~
    ## $ title_type       <fct> Feature Film, Feature Film, Feature Film, Feature Fil~
    ## $ genre            <fct> Drama, Drama, Comedy, Drama, Horror, Documentary, Dra~
    ## $ runtime          <dbl> 80, 101, 84, 139, 90, 78, 142, 93, 88, 119, 127, 108,~
    ## $ mpaa_rating      <fct> R, PG-13, R, PG, R, Unrated, PG-13, R, Unrated, Unrat~
    ## $ thtr_rel_year    <dbl> 2013, 2001, 1996, 1993, 2004, 2009, 1986, 1996, 2012,~
    ## $ thtr_rel_month   <dbl> 4, 3, 8, 10, 9, 1, 1, 11, 9, 3, 6, 12, 1, 9, 6, 8, 3,~
    ## $ thtr_rel_day     <dbl> 19, 14, 21, 1, 10, 15, 1, 8, 7, 2, 19, 18, 4, 23, 20,~
    ## $ dvd_rel_year     <dbl> 2013, 2001, 2001, 2001, 2005, 2010, 2003, 2004, 2013,~
    ## $ dvd_rel_month    <dbl> 7, 8, 8, 11, 4, 4, 2, 3, 1, 8, 5, 9, 7, 2, 3, 12, 8, ~
    ## $ dvd_rel_day      <dbl> 30, 28, 21, 6, 19, 20, 18, 2, 21, 14, 1, 23, 9, 13, 2~
    ## $ imdb_num_votes   <int> 899, 12285, 22381, 35096, 2386, 333, 5016, 2272, 880,~
    ## $ best_pic_nom     <fct> no, no, no, no, no, no, no, no, no, no, no, no, no, n~
    ## $ best_pic_win     <fct> no, no, no, no, no, no, no, no, no, no, no, no, no, n~
    ## $ best_actor_win   <fct> no, no, no, yes, no, no, no, yes, no, no, yes, no, ye~
    ## $ best_actress_win <fct> no, no, no, no, no, no, no, no, no, no, no, no, yes, ~
    ## $ best_dir_win     <fct> no, no, no, yes, no, no, no, no, no, no, no, no, no, ~
    ## $ top200_box       <fct> no, no, no, no, no, no, no, no, no, no, yes, no, no, ~

### Missing Values

Lets take care of missing values that we may have in our data.

``` r
kable(design[!complete.cases(design), colSums(is.na(design)) > 0]) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
runtime
</th>
<th style="text-align:right;">
dvd_rel_year
</th>
<th style="text-align:right;">
dvd_rel_month
</th>
<th style="text-align:right;">
dvd_rel_day
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
132
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
92
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

There can be no movie without runtime:

``` r
filter(movies, is.na(runtime)) %>% kable() %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
title_type
</th>
<th style="text-align:left;">
genre
</th>
<th style="text-align:right;">
runtime
</th>
<th style="text-align:left;">
mpaa_rating
</th>
<th style="text-align:left;">
studio
</th>
<th style="text-align:right;">
thtr_rel_year
</th>
<th style="text-align:right;">
thtr_rel_month
</th>
<th style="text-align:right;">
thtr_rel_day
</th>
<th style="text-align:right;">
dvd_rel_year
</th>
<th style="text-align:right;">
dvd_rel_month
</th>
<th style="text-align:right;">
dvd_rel_day
</th>
<th style="text-align:right;">
imdb_rating
</th>
<th style="text-align:right;">
imdb_num_votes
</th>
<th style="text-align:left;">
critics_rating
</th>
<th style="text-align:right;">
critics_score
</th>
<th style="text-align:left;">
audience_rating
</th>
<th style="text-align:right;">
audience_score
</th>
<th style="text-align:left;">
best_pic_nom
</th>
<th style="text-align:left;">
best_pic_win
</th>
<th style="text-align:left;">
best_actor_win
</th>
<th style="text-align:left;">
best_actress_win
</th>
<th style="text-align:left;">
best_dir_win
</th>
<th style="text-align:left;">
top200_box
</th>
<th style="text-align:left;">
director
</th>
<th style="text-align:left;">
actor1
</th>
<th style="text-align:left;">
actor2
</th>
<th style="text-align:left;">
actor3
</th>
<th style="text-align:left;">
actor4
</th>
<th style="text-align:left;">
actor5
</th>
<th style="text-align:left;">
imdb_url
</th>
<th style="text-align:left;">
rt_url
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
The End of America
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Unrated
</td>
<td style="text-align:left;">
Indipix
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
7.5
</td>
<td style="text-align:right;">
739
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
Anne Sundberg
</td>
<td style="text-align:left;">
Naomi Wolf
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1294790/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/end_of_america/
</td>
</tr>
</tbody>
</table>

</div>

In fact, we can manually correct this erroneous data entry

``` r
movies[movies$title == "The End of America", "runtime"] <- 74
design[is.na(design$runtime), "runtime"] <- 74
```

On the other hand, there should be more than enough movies without dvd
release. In principle, we could omit these data points as well, but
there is a better way to handle dvd release date information, as will
become clear shortly.

### Numerical Predictors

Lets check pairwise associations of numeric predictors with response and
among each other (including also dates of release in theater and on
dvd).

``` r
ggpairs(
  data = select_if(design, is.numeric), 
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1))
) +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  labs(title = "Pairwise Correlations of Numerical Variables")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/pairwise-numeric-1.png" width="90%" style="display: block; margin: auto;" />

From the first row, the moderate (and significant) correlation of
response with `runtime` and `imdb_num_votes` suggests these variables as
potentially important (numerical) predictors. Their relationship with
response will look more linear (and less stretched out) on the
respective scatterplots, if we correct for very high right skewness of
the data, seen on the corresponding diagonal plots. But first I identify
particularly extreme `runtime` lengths:

``` r
movies %>% 
  filter( ! between(runtime, 60, 210) ) %>% 
  select(title, title_type, runtime, imdb_num_votes, imdb_rating, audience_score, critics_score) %>% 
  arrange(imdb_rating) %>% 
  kable() %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
title_type
</th>
<th style="text-align:right;">
runtime
</th>
<th style="text-align:right;">
imdb_num_votes
</th>
<th style="text-align:right;">
imdb_rating
</th>
<th style="text-align:right;">
audience_score
</th>
<th style="text-align:right;">
critics_score
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Sea Monsters: A Prehistoric Adventure
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:right;">
7.0
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
100
</td>
</tr>
<tr>
<td style="text-align:left;">
Africa: The Serengeti
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
535
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
100
</td>
</tr>
<tr>
<td style="text-align:left;">
Hotel Terminus: The Life and Times of Klaus Barbie
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
267
</td>
<td style="text-align:right;">
872
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
100
</td>
</tr>
</tbody>
</table>

I choose to remove 2 very short documentaries as clear outliers and do
not consider titles shorter than, say, 60 min. (The 3rd long one turns
out to be not so far off on the transformed distribution.) After passing
to the inverse of a runtime, that we call `quickness = 1 / runtime`, the
data appears more symmetric.

``` r
design <- filter( design, runtime > 60 ) %>%
  mutate(quickness = 1 / runtime, .keep = "unused")
inverse_runtime <- ggplot(movies, aes(x = runtime)) + 
  stat_function( fun = function(x) 1 / x ) + 
  labs(title = "Variable Transformation", x = "Runtime of a Movie", y = "Quickness = 1 / Runtime")

# Plot the transformed distribution
quickness_hist <- ggplot(design, aes(x = quickness)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), adjust = 1, size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(design$quickness), sd = sd(design$quickness))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) +
  theme(legend.position = c(0.15, 0.9)) +
  annotate(
    "text", x = 0.005, y = 215,
    label = paste("Skewness:", round(skewness(design$quickness), digits = 3))
  ) +
  labs(title = "Histogram", x = "Movie's Quickness", y = "Probability Density")
quickness_qqnorm <- ggplot(design, aes(sample = quickness)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")

grid.arrange( inverse_runtime, quickness_hist, quickness_qqnorm, layout_matrix = rbind(c(1), c(2,3)) )
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/runtime-transformation-1.png" width="90%" style="display: block; margin: auto;" />

For `imdb_num_votes` we perform simple log-transformation. From the
following plots, the new distribution is symmetric but somewhat
under-dispersed with thinner tails (negative excess kurtosis, Q-Q plot
displays an S-shaped curve).

``` r
design <- mutate(design, 
  log_votes = log(imdb_num_votes), .keep = "unused"
)
# Plot the distribution of the transformed variable
logvotes_hist <- ggplot(design, aes(x = log_votes)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), adjust = 1, size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(design$log_votes), sd = sd(design$log_votes))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) +
  theme(legend.position = c(0.15, 0.9)) +
  annotate(
    "text", x = 6, y = 0.2,
    label = paste("Kurtosis:", round(kurtosis(design$log_votes), digits = 3))
  ) +
  labs(title = "Histogram", x = "Logarithm of Number of Votes on IMDB", y = "Probability Density")
logvotes_qqnorm <- ggplot(design, aes(sample = log_votes)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
grid.arrange(logvotes_hist, logvotes_qqnorm, ncol = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/log_votes-distribution-1.png" width="90%" style="display: block; margin: auto;" />

Next, the highest degree of correlation is between years of release in
theater and on dvd, which is a potential source of collinearity in our
model. However, instead of simply dropping `dvd_rel_year`, we observe
the two clusters on their respective scatterplot, only one of which is
responsible for correlation. We therefore choose to create the binary
variable, specifying the cluster to which movie belongs:

``` r
design <- mutate(design,
  dvd_lag = as.factor( ifelse( dvd_rel_year - thtr_rel_year > 4 | is.na(dvd_rel_year), "Yes", "No" ) )
) %>% 
  select(-c(dvd_rel_year:dvd_rel_day))
```

The chosen value of a “lag” between release in theaters and on dvd leads
to the maximum of correlation in category with “No” lag (straight line
on the scatterplot), and minimizes it in the second diffuse “Yes”
cluster. After accounting for `dvd_rel_year`, using almost perfect
correlation with `thtr_rel_year` in the first category, we omit dvd
release date from consideration altogether. (We also partially keep
information about `dvd_rel_year` in moderate negative correlation with
`imdb_num_votes`, which is maximized for “Yes”, and almost 0 for “No”
`dvd_lag`: “The earlier such movie was released on dvd, the larger
number of votes it has accumulated, on average.”) There is no reason to
suspect month or day of dvd release can be any reliable predictors of a
movie score. On the other hand, we still keep the full theatrical
release date, since movie premieres are known for its seasonality (some
periods are better than others in terms of box office). If movie does
not have release on dvd (yet), we formally allocate it to the “Yes”
category.

It is possible to show that two peaks in number of votes and runtime
distributions nicely correspond to two categories of `dvd_lag`, thus
proving its usefulness. We can also see the correlations between
transformed variables and within groups.

``` r
ggpairs(mapping = aes(col = dvd_lag, alpha = 0.3),
  data = design %>% 
    select(imdb_norm, quickness, log_votes, thtr_rel_year, dvd_lag), 
  columns = 1:4,
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1))
) +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  labs(title = "Pairwise Correlations of Numerical Variables")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />

### Categorical Predictors

Compute summary statistics:

``` r
summary( select_if(design, is.factor) )
```

    ##         title_type                 genre      mpaa_rating  best_pic_nom
    ##  Documentary : 53   Drama             :303   G      : 18   no :627     
    ##  Feature Film:591   Comedy            : 88   NC-17  :  2   yes: 22     
    ##  TV Movie    :  5   Action & Adventure: 63   PG     :118               
    ##                     Mystery & Suspense: 59   PG-13  :133               
    ##                     Documentary       : 50   R      :329               
    ##                     Horror            : 24   Unrated: 49               
    ##                     (Other)           : 62                             
    ##  best_pic_win best_actor_win best_actress_win best_dir_win top200_box dvd_lag  
    ##  no :642      no :556        no :577          no :606      no :634    No :400  
    ##  yes:  7      yes: 93        yes: 72          yes: 43      yes: 15    Yes:249  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ## 

We can also plot our numeric response against each of the individual
categorical predictors, using side-by-side boxplots.

``` r
design %>% 
  select_if(is.factor) %>% 
  cbind(imdb = design$imdb_norm) %>% 
  pivot_longer(1:9, names_to = "predictor", values_to = "category") %>% 
  ggplot(aes(x = imdb, y = category)) +
  geom_boxplot() +
  facet_wrap( ~ predictor, ncol = 3, scales = "free") +
  labs(x = "IMDB Rating", y = NULL, title = "Categorical Predictors for Movie Score") +
  theme(axis.text = element_text(size = 6))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/categorical-predictors-1.png" width="90%" style="display: block; margin: auto;" />

We can see that whether or not one of the main actors/actresses in the
movie ever won an Oscar has very little or no relevance for the movie
score on its own. Since these variables are similar in nature, we can
lump them together for simplicity into `awards_team`. On the other hand,
the nomination or winning the best picture Oscar appear to be quite
reliable predictors (although sample is imbalanced), but they overlap
almost fully, with only one misclassification (“won, but no
nomination”):

``` r
filter(movies, best_pic_win == "yes" & best_pic_nom == "no") %>%
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
title_type
</th>
<th style="text-align:left;">
genre
</th>
<th style="text-align:right;">
runtime
</th>
<th style="text-align:left;">
mpaa_rating
</th>
<th style="text-align:left;">
studio
</th>
<th style="text-align:right;">
thtr_rel_year
</th>
<th style="text-align:right;">
thtr_rel_month
</th>
<th style="text-align:right;">
thtr_rel_day
</th>
<th style="text-align:right;">
dvd_rel_year
</th>
<th style="text-align:right;">
dvd_rel_month
</th>
<th style="text-align:right;">
dvd_rel_day
</th>
<th style="text-align:right;">
imdb_rating
</th>
<th style="text-align:right;">
imdb_num_votes
</th>
<th style="text-align:left;">
critics_rating
</th>
<th style="text-align:right;">
critics_score
</th>
<th style="text-align:left;">
audience_rating
</th>
<th style="text-align:right;">
audience_score
</th>
<th style="text-align:left;">
best_pic_nom
</th>
<th style="text-align:left;">
best_pic_win
</th>
<th style="text-align:left;">
best_actor_win
</th>
<th style="text-align:left;">
best_actress_win
</th>
<th style="text-align:left;">
best_dir_win
</th>
<th style="text-align:left;">
top200_box
</th>
<th style="text-align:left;">
director
</th>
<th style="text-align:left;">
actor1
</th>
<th style="text-align:left;">
actor2
</th>
<th style="text-align:left;">
actor3
</th>
<th style="text-align:left;">
actor4
</th>
<th style="text-align:left;">
actor5
</th>
<th style="text-align:left;">
imdb_url
</th>
<th style="text-align:left;">
rt_url
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
The Hurt Locker
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:right;">
131
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:left;">
Summit Entertainment
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
2010
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
318019
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
Kathryn Bigelow
</td>
<td style="text-align:left;">
Jeremy Renner
</td>
<td style="text-align:left;">
Anthony Mackie
</td>
<td style="text-align:left;">
Brian Geraghty
</td>
<td style="text-align:left;">
Guy Pearce
</td>
<td style="text-align:left;">
Ralph Fiennes
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0887912/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/hurt_locker/
</td>
</tr>
</tbody>
</table>

</div>

Note that some categories contain very few data points, such as rating
NC-17 (plus “TV Movie” title type has huge variance of scores, as
result), so we can merge some factor levels together:

``` r
design <- mutate(design, 
  title_type = fct_collapse(title_type, `Feature or TV` = c("Feature Film", "TV Movie")),
  mpaa_rating = fct_collapse(mpaa_rating, `R or NC-17` = c("R", "NC-17")),
  awards_team = as.factor( ifelse(
    best_actor_win == "yes" | best_actress_win == "yes" | best_dir_win == "yes", "Yes", "No") ),
  awards_pic = as.factor( ifelse(best_pic_nom == "yes" | best_pic_win == "yes", "Yes", "No") ), 
  .keep = "unused"
)
```

### Non-Linear Effects

One of conditions to fit linear regression model is that predictors are
linearly associated with response variable. However, we can see
non-linear trend on the `imdb_norm vs log_votes` scatterplot for both
higher and lower number of votes. Part of it can be explained by
conditioning on movie type:

``` r
ggplot( design, aes(y = imdb_norm, x = log_votes, col = title_type)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(title = "Non-linear Dependence", x = "Log of Votes", y = "IMDB Score")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

The trend is approximately linear for two types until the number of
votes approaches about trillion, after which the new regime takes its
turn. If one does not wish to include non-linear effects, for the sake
of interpretability, one could fit two different slopes (linear
coefficients in a multiple regression). In the absence of any natural
grouping, analogous to `title_type` in the lower region, one has to
introduce an artificial threshold on number of votes. This lead to the
piecewise linear approximation, equivalent to a first order spline with
one knot:

![y = \\beta_0 + \\beta_1 x +\\beta_2 (x - k)\_+ + ..., \\qquad \\mbox{where} \\quad (x - k)\_+ = \\begin{cases}
        x - k, & \\text{for } x > k \\\\
        0, & \\text{for } x \< k
        \\end{cases} ](https://latex.codecogs.com/png.latex?y%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%20x%20%2B%5Cbeta_2%20%28x%20-%20k%29_%2B%20%2B%20...%2C%20%5Cqquad%20%5Cmbox%7Bwhere%7D%20%5Cquad%20%28x%20-%20k%29_%2B%20%3D%20%5Cbegin%7Bcases%7D%0A%20%20%20%20%20%20%20%20x%20-%20k%2C%20%26%20%5Ctext%7Bfor%20%7D%20x%20%3E%20k%20%5C%5C%0A%20%20%20%20%20%20%20%200%2C%20%26%20%5Ctext%7Bfor%20%7D%20x%20%3C%20k%0A%20%20%20%20%20%20%20%20%5Cend%7Bcases%7D%20 "y = \beta_0 + \beta_1 x +\beta_2 (x - k)_+ + ..., \qquad \mbox{where} \quad (x - k)_+ = \begin{cases}
        x - k, & \text{for } x > k \\
        0, & \text{for } x < k
        \end{cases} ")

I used truncated power series parameterization, which is easier to
interpret than B-splines. On the plot we see, that it largely captures
the curved trend in the upper `log_votes` range. However, there remains
some outlying observations, especially, for higher `imdb_norm` (lower
`imdb_rating`) values.

``` r
piecewise <- function(x, thresh = 11.33) {
  x_diff <- (x - thresh) * ifelse(x > thresh, 1, 0)
  return( cbind(x, x_diff) )
}
piecewise_fit <- lm(imdb_norm ~ title_type + piecewise(log_votes), design)
summary(piecewise_fit)
```

    ## 
    ## Call:
    ## lm(formula = imdb_norm ~ title_type + piecewise(log_votes), data = design)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.26492 -0.06710  0.00016  0.06253  0.37412 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.488141   0.030206  16.160  < 2e-16 ***
    ## title_typeFeature or TV     0.240641   0.016763  14.356  < 2e-16 ***
    ## piecewise(log_votes)x      -0.017356   0.003519  -4.933 1.03e-06 ***
    ## piecewise(log_votes)x_diff -0.104241   0.013429  -7.762 3.28e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1056 on 645 degrees of freedom
    ## Multiple R-squared:  0.3655, Adjusted R-squared:  0.3626 
    ## F-statistic: 123.9 on 3 and 645 DF,  p-value: < 2.2e-16

``` r
# Plot the resulting fitted curves and residuals
piecewise_pred <- ggplot(
  data =  design %>% cbind(predict(piecewise_fit, design, interval = "confidence")), 
  aes(y = imdb_norm, x = log_votes, col = title_type, legend = "top")
) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", se = TRUE, linetype = "longdash", alpha = 0.2) +
  geom_line(aes(x = log_votes, y = fit), linetype = "solid", size = 1.5) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = title_type), linetype = "dotted", alpha = 0.25) +
  geom_vline(xintercept = 11.33, linetype = "dashed") +
  labs(title = "Piecewise-linear Approximation", x = "Log of Votes", y = "IMDB Score")
piecewise_resid <- ggplot(
  design %>% mutate(resid = piecewise_fit$residuals), aes(x = log_votes, y = resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Log of Votes on IMDB", y = "Residuals", title = "Partial Piecewise Fit: Residuals")
piecewise_qqnorm <- ggplot(design %>% mutate(resid = piecewise_fit$residuals), aes(sample = resid)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
grid.arrange( piecewise_pred, piecewise_resid, piecewise_qqnorm, layout_matrix = rbind(c(1), c(2,3)) )
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/piecewise-diagnostics-1.png" width="90%" style="display: block; margin: auto;" />

In principle, one could also fit two different lines in a lower region,
by means of an interaction term `title_type * log_votes`. This would
make our approximation align even closer to `loess` curve visually, but
does not lead to any significant improvement in predictive power. One
could further account for non-linearity by introducing polynomial
features, or higher order splines, which would make our model even more
flexible but less interpretable. One has to decide whether a possible
gain in accuracy worth the effort.

**A short digression** (can skip to Part 4): [Should one choose “raw”
monomials `I(x^n)` or orthogonal
polynomials?](https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal)
Orthogonalization has the effect of removing unwarranted collinearity
among different order components. The cost is the loss in
interpretability, since coefficients in orthogonal polynomials represent
completely different quantities from that of their “raw” counterpart.
However, when trying to get the same quantities from both regressions,
the estimates and standard errors will be identical.

[The key difference is the
following](https://stats.stackexchange.com/questions/258307/raw-or-orthogonal-polynomial-regression):
using orthogonal polynomials allows you to isolate the contribution of
each term to explaining variance in the outcome (e.g., as measured by
the squared semipartial correlation). This has the advantage that you
can see whether a certain order in the polynomial significantly improves
the regression over the lower orders. In particular, in the absence of
correlations between orthogonal components the lower order coefficients
(and their significance) won’t change when we add higher order
coefficients (or they stay roughly the same, *if everything else held
fixed*).

*About possible interpretation:* If one really wanted to, by completing
the square (cube, etc), the polynomial could be re-written as a product

![\\beta_1 X_1 + \\beta_2 X_1^2 + ... + \\beta_d X_1^d = \\beta_0 + \\beta_d \\prod\_{j=1}^d (X_1 - c_j)](https://latex.codecogs.com/png.latex?%5Cbeta_1%20X_1%20%2B%20%5Cbeta_2%20X_1%5E2%20%2B%20...%20%2B%20%5Cbeta_d%20X_1%5Ed%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_d%20%5Cprod_%7Bj%3D1%7D%5Ed%20%28X_1%20-%20c_j%29 "\beta_1 X_1 + \beta_2 X_1^2 + ... + \beta_d X_1^d = \beta_0 + \beta_d \prod_{j=1}^d (X_1 - c_j)")

One could then say that
![\\beta_d](https://latex.codecogs.com/png.latex?%5Cbeta_d "\beta_d")
was the change in the response for a one-unit change in a latter
product, etc. But the sampling distributions of the roots
![c_j](https://latex.codecogs.com/png.latex?c_j "c_j"), as functions of
![\\beta_i](https://latex.codecogs.com/png.latex?%5Cbeta_i "\beta_i"),
would itself be very tricky, as well as the meaning of such new feature
(so this is often a considerable amount of work for little gain).

For the above mentioned reasons, I choose to consider orthogonal
`poly()`. Compare, for instance, two partial models, fitting polynomials
of degree 3 and 4, respectively:

``` r
poly3_fit <- lm(imdb_norm ~ title_type + poly(log_votes, 3), design)
summary(poly3_fit)
```

    ## 
    ## Call:
    ## lm(formula = imdb_norm ~ title_type + poly(log_votes, 3), data = design)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.24959 -0.06753  0.00036  0.06308  0.39106 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              0.30173    0.01725  17.488  < 2e-16 ***
    ## title_typeFeature or TV  0.24330    0.01823  13.343  < 2e-16 ***
    ## poly(log_votes, 3)1     -1.41758    0.11704 -12.112  < 2e-16 ***
    ## poly(log_votes, 3)2     -0.64970    0.11462  -5.669 2.17e-08 ***
    ## poly(log_votes, 3)3     -0.49222    0.10858  -4.533 6.92e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1059 on 644 degrees of freedom
    ## Multiple R-squared:  0.363,  Adjusted R-squared:  0.3591 
    ## F-statistic: 91.75 on 4 and 644 DF,  p-value: < 2.2e-16

``` r
poly4_fit <- lm(imdb_norm ~ title_type + poly(log_votes, 4), design)
summary(poly4_fit)
```

    ## 
    ## Call:
    ## lm(formula = imdb_norm ~ title_type + poly(log_votes, 4), data = design)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.26128 -0.06874  0.00011  0.06104  0.38598 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              0.30228    0.01720  17.574  < 2e-16 ***
    ## title_typeFeature or TV  0.24271    0.01818  13.352  < 2e-16 ***
    ## poly(log_votes, 4)1     -1.41597    0.11666 -12.137  < 2e-16 ***
    ## poly(log_votes, 4)2     -0.65112    0.11425  -5.699 1.84e-08 ***
    ## poly(log_votes, 4)3     -0.49145    0.10823  -4.541 6.69e-06 ***
    ## poly(log_votes, 4)4     -0.23925    0.10560  -2.266   0.0238 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1056 on 643 degrees of freedom
    ## Multiple R-squared:  0.3681, Adjusted R-squared:  0.3631 
    ## F-statistic:  74.9 on 5 and 643 DF,  p-value: < 2.2e-16

``` r
anova(poly4_fit, piecewise_fit)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: imdb_norm ~ title_type + poly(log_votes, 4)
    ## Model 2: imdb_norm ~ title_type + piecewise(log_votes)
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    643 7.1684                           
    ## 2    645 7.1973 -2 -0.028933 1.2976 0.2739

The 4th order term leads to significant improvement over the 3rd order
polynomial fit, but no big difference with the piecewise linear
approximation. Since “the power of logarithm of number of votes” is
already hard enough to interpret, we prefer to stick with more tractable
piecewise linear model. I choose to encode the additional truncated
power term separately as a new feature in our data:

``` r
design <- mutate(design,
  votes_boost = (log_votes - 11.33) * ifelse(log_votes > 11.33, 1, 0)
)
```

------------------------------------------------------------------------

## Part 4: Modeling

Let us find the variables that constitute the best predictive model of
the movie’s IMDB rating, using multiple linear regression. According to
this task, we are interested in model’s accuracy in the first place (and
not the inference for various predictors per se, based on p-values). To
find small and interpretable models, we would use selection criterion
that explicitly penalize larger models, such as an **adjusted
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2")** (other common
choices are AIC, BIC,
![C_p](https://latex.codecogs.com/png.latex?C_p "C_p")). This has an
advantage of not depending on the pre-set arbitrary significance level
for predictor coefficients.

Different selection approaches generally give similar but not identical
models. In principle, the best subset selection method compares models
with various combinations of
![p](https://latex.codecogs.com/png.latex?p "p") features to find the
one which is *best*, according to some criteria. In particular, if one
considers *all* possible
![\\binom{p}{k}](https://latex.codecogs.com/png.latex?%5Cbinom%7Bp%7D%7Bk%7D "\binom{p}{k}")
models with exactly ![k](https://latex.codecogs.com/png.latex?k "k")
predictors, it would require to fit
![2^p](https://latex.codecogs.com/png.latex?2%5Ep "2^p") models in
total, and quickly becomes too computationally expensive as the number
of features ![p](https://latex.codecogs.com/png.latex?p "p") grows.
Instead, we choose to perform the **backward stepwise selection**, which
explores a far more restricted set of models, searching only through
![1+p(p+1)/2](https://latex.codecogs.com/png.latex?1%2Bp%28p%2B1%29%2F2 "1+p(p+1)/2")
possible combinations of features.

In backwards selection approach, one starts with the full linear model,
containing *all* available predictors, and then consequently removes
them, in contrast to alternative forward selection strategy, where we
add features one by one. Hopefully, this might help us avoid drawbacks
of “suppressor effects”, when some of the variables are only significant
when another predictor is held constant.

Note that one can proceed in two fundamentally different ways, according
to how one address discrete predictors. First, one can treat such
variables “nominally”, and thus compare only models that differ by
presence of factor as a whole (even though technically all levels are
realized through indicator variables). On the other hand, each factor
with ![l](https://latex.codecogs.com/png.latex?l "l") levels is encoded
via ![l-1](https://latex.codecogs.com/png.latex?l-1 "l-1") indicator
variables (with respect to single reference level), thus contributing
“effective” degrees of freedom, that can be fitted separately. It is
curious to compare two strategies and the resulting best fit models.

### 1st Strategy: Keep Factors (“Nominal”)

At each step
![k=p,p-1,...,1](https://latex.codecogs.com/png.latex?k%3Dp%2Cp-1%2C...%2C1 "k=p,p-1,...,1")
of backwards selection method one chooses best among
![k-1](https://latex.codecogs.com/png.latex?k-1 "k-1") models with one
of predictors removed, that minimizes residual sum of squared error (or
maximizes ![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2")). The
automated algorithms like `fastbw()` (from `rms` package) or `step()`
(from `stats` package), terminate when none of subset models leads to
improvement in the AIC goodness-of-fit measure. This stopping rule can
result in complicated models for high number of parameters, while less
verbose models can turn out just about as good. For this reason, I
implement the search for best model “from scratch”, with the aid of some
helper functions.

``` r
# Given a vector of predictors x, return matrix, where each column is a parse model with 1 predictor removed 
get_subsets <- function(x) {
  k <- length(x)
  x_times <- rep(x, times = k)
  x_each <- rep(x, each = k)
  X <- matrix(x_times[x_times != x_each], nrow = k - 1)
  return(X)
}
# For each subset of predictors, fit corresponding models and find the best one
get_best <- function(X, y, data, measure = "adj.r.squared", max = TRUE) {
  rhs <- apply(X, 2, paste, collapse = "+")
  formulas <- paste(y, rhs, sep = "~ ")
  models <- lapply(formulas, function(f) lm(f, data = data))
  scores <- sapply(models, function(m) summary(m)[[measure]])
  if ( max ) {
    idx <- which.max(scores)
    return( list(score = scores[idx], variables = X[ , idx]) )
  } else {
    idx <- which.min(scores)
    return( list(score = scores[idx], variables = X[ , idx]) )
  }
}
```

So far we considered only additive models, where the association between
a predictor ![X_j](https://latex.codecogs.com/png.latex?X_j "X_j") and
the response does not depend on the values of other predictors. I have
only commented that the interaction between `title_type` and `log_votes`
does not result in significant improvement. However, without an
explanatory theory for which effects might be important for movie’s
popularity, **lets include also all possible 2-way interactions
![X_i\\times X_j](https://latex.codecogs.com/png.latex?X_i%5Ctimes%20X_j "X_i\times X_j")
in the full model**.

``` r
# Take 2 vectors of features and return their pairwise unique interaction products
interact <- function(left, right, .sep = ":") {
  filter_right <- right[ !(right %in% left) ]
  M1 <- sapply( filter_right, function(x) paste(left, x, sep = .sep) )
  interactions1 <- as.vector( unlist(M1) )
  overlap <- right[ right %in% left ]
  if ( length(overlap) == 0 ) {
    return( interactions1)
  } else {
    filter_left <- left[ !(left %in% right) ]
    M2 <- sapply( filter_left, function(x) paste(x, overlap, sep = .sep) )
    M3 <- sapply( overlap, function(x) paste(x, overlap, sep = .sep) )
    interactions2 <- as.vector( unlist(M2) )
    interactions3 <- as.vector( unlist(M3[lower.tri(M3)]) )
    return( c(interactions1, interactions2, interactions3) )
  }
}
```

``` r
# Avoid categorical-categorical interaction terms
cats <- names(select_if(design, is.factor))
nums <- names(select_if(design, is.numeric))[-1]
interactions <- interact(nums, c(nums, cats))
all_predictors <- c(cats, nums, interactions)
# Check for linear dependencies in the full model (technically, each factor level is a separate column)
rhs_p <- paste(all_predictors, collapse = " + ")
expand_factors <- model.matrix(formula(paste("~ imdb_norm +", rhs_p)), data = design)
( full_model_lindeps <- plm::detect.lindep(expand_factors, suppressPrint = TRUE) )
```

    ##                                votes_boost 
    ##                                         27 
    ##        title_typeFeature or TV:votes_boost 
    ##                                         33 
    ## genreArt House & International:votes_boost 
    ##                                         85 
    ##               genreDocumentary:votes_boost 
    ##                                         87 
    ##             mpaa_ratingUnrated:votes_boost 
    ##                                        117

From an interpretation standpoint, one should follow **hierarchy rules**
– that is, any time a “higher-order” term is in a model, the related
“lower-order” terms (main effect) have to be included as well, even if
sometimes they are mathematically irrelevant (statistically
non-significant). In terms of coefficient estimates, the interpretation
of the interaction is considered “constant” in this case, while the
effect of the main predictors is rather “conditional”:
![y = (\\beta_i + \\beta\_{ij} X_j) X_i + ...](https://latex.codecogs.com/png.latex?y%20%3D%20%28%5Cbeta_i%20%2B%20%5Cbeta_%7Bij%7D%20X_j%29%20X_i%20%2B%20... "y = (\beta_i + \beta_{ij} X_j) X_i + ...").
[The model should also retain its form under scale
transformations](http://www.talkstats.com/threads/doing-correct-backwards-elimination-with-interaction-terms.59314/)
![X_i + c](https://latex.codecogs.com/png.latex?X_i%20%2B%20c "X_i + c").

``` r
# Create a wrapper function to consider only subsets, that obey hierarchy rules for selection
obey_hierarchy <- function(x, x_subset) {
  pred <- x[! x %in% x_subset]
  return( ! pred %in% unlist(strsplit(x_subset, ":")) )
}
get_subsets_hierarchical <- function(x) {
  X <- get_subsets(x)
  return( X[ , apply(X, 2, obey_hierarchy, x = x), drop = FALSE] )
}
```

The results for each step
![k](https://latex.codecogs.com/png.latex?k "k") do not depend on the
choice of measure, since various model selection criteria (such as
![R^2\_{adj}](https://latex.codecogs.com/png.latex?R%5E2_%7Badj%7D "R^2_{adj}"),
![C_p](https://latex.codecogs.com/png.latex?C_p "C_p"), BIC, or AIC,
that are all modifications of RSS) differ only in how models of
*different* sizes are compared. The intuition behind the *adjusted
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2")* is that once
all of the correct predictors have been included in the model, adding
additional noise variables does not lead to additional gain in accuracy
of model’s predictions. Unlike the regular
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") statistic,
![R^2\_{adj}](https://latex.codecogs.com/png.latex?R%5E2_%7Badj%7D "R^2_{adj}")
puts a penalty for the inclusion of unnecessary variables in the model.
(We also remove found linear dependencies, created by interactions
between `votes_boost` and certain factor levels.)

``` r
# Start with the full model, containing all ("nominal") predictors
all_predictors <- all_predictors[! grepl("votes_boost:(title_type|genre|mpaa_rating)", all_predictors)]
# Iterate over different size models, keeping the score of the found best one and predictors as dummies
p_nom <- length(all_predictors)
X_nom <- matrix(NA, nrow = p_nom, ncol = p_nom + 1)
colnames(X_nom) <- c("adjr2", all_predictors)
X <- as.matrix(all_predictors, ncol = 1)
for (size in seq(from = p_nom, to = 1, by = -1)) {
  best_model <- get_best(X, y = "imdb_norm", data = design)
  best_subset <- all_predictors %in% best_model$variables
  X_nom[size, ] <- c(best_model$score, as.numeric(best_subset))
  X <- get_subsets_hierarchical(best_model$variables)
}
# Print a quick glimpse of an output table
kable(as.data.frame(X_nom)) %>% kable_styling("striped") %>% scroll_box(height = "400px", width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
adjr2
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
votes_boost
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:title_type
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:genre
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
votes_boost:top200_box
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
votes_boost:dvd_lag
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
votes_boost:awards_team
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
votes_boost:awards_pic
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:thtr_rel_month
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:thtr_rel_day
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:quickness
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:log_votes
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year:votes_boost
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:thtr_rel_day
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:quickness
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:log_votes
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_month:votes_boost
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:quickness
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:log_votes
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_day:votes_boost
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:log_votes
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
quickness:votes_boost
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
log_votes:votes_boost
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.2437625
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.4366178
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.4663458
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5082450
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5353894
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5534722
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5569125
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5634217
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5646854
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5740271
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5803531
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5817563
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5866456
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5860105
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5889638
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5929787
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5975434
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5968613
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5998011
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.5999661
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6012627
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6027002
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6038687
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6034710
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6042621
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6051116
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6059121
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6070402
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6081464
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6098383
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6109009
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6117018
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6124372
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6133309
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6136122
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6142550
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6150829
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6148134
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6143761
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6147172
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6147431
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6149265
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6150534
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6151839
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6152053
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6151800
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6150268
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6147984
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6146157
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6145404
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6143814
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6138444
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6135195
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6129065
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6126085
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6122467
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6117698
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6112414
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6107112
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6100921
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6093781
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6086758
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6079416
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6072029
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6064390
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6056695
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6048953
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

We can visually compare the accuracy of models of different size, by
drawing the curve for calculated
![R^2\_{adj}](https://latex.codecogs.com/png.latex?R%5E2_%7Badj%7D "R^2_{adj}")
score.

``` r
# Create re-usable helper function for basic curve
plot_score <- function(vec) {
  ggplot(
    data = data.frame(k = 1:length(vec), score = vec),
    aes(x = k, y = score)
  ) +
    geom_line(col = "deepskyblue", size = 1) +
    geom_point(col = "deepskyblue", size = 3) + 
    labs(
      x = "Number of Variables", 
      title = "Backwards Stepwise Model Selection"
    )
}
# Customize plot for a given vector of values
v  <- X_nom[, "adjr2"]
points <- c(pars = 6, nom = 13, max = which.max(v))
plot_score(vec = v) +
  geom_point(
    data = data.frame(idx = points, value = v[points]) -> v_points,
    aes(x = idx, y = value),
    size = 5, stroke = 1, shape = 1, col = "red"
  ) + 
  geom_label(
    data = v_points,
    aes(x = idx, y = value, label = format(value, digits = 3)),
    nudge_y = 0.02, nudge_x = - 1, alpha = 0.5
  ) +
  ylab(expression(R[adj]^2))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

The best model in terms of accuracy contains 45 predictors (“nominal”
degrees of freedom), and therefore very hard to interpret. Since its
score differs very slightly from the plateau value, it is also hard to
justify why should one prefer this particular model over the others that
are almost as good. On the other hand, the blue curve also demonstrates
a pronounced “elbow”, after which each additional value leads to little
or no improvement in predictive power. Consider first the “parsimonious”
model of size 6:

``` r
# Get the corresponding predictors
subset_pars <- X_nom[points[["pars"]], ]
( predictors_pars <- names(subset_pars)[subset_pars == 1]  ) 
```

    ## [1] "title_type"    "genre"         "mpaa_rating"   "thtr_rel_year"
    ## [5] "log_votes"     "votes_boost"

This economic model makes the most use of its few variables, before any
interactions turn in. It therefore allows us to answer the question
“which features are most useful for prediction”. We prefer a compromise
and get some boost in accuracy, by considering another “elbow” model in
the range \[7, 17\], for instance, the relative peak at number of
predictors 13:

``` r
# Get the corresponding predictors
subset_nom <- X_nom[points[["nom"]], ]
( predictors_nom <- names(subset_nom)[subset_nom == 1]  ) 
```

    ##  [1] "title_type"          "genre"               "mpaa_rating"        
    ##  [4] "dvd_lag"             "awards_pic"          "thtr_rel_year"      
    ##  [7] "quickness"           "log_votes"           "votes_boost"        
    ## [10] "thtr_rel_year:genre" "quickness:genre"     "log_votes:genre"    
    ## [13] "log_votes:dvd_lag"

Refit the latter “nominal” model for further examination and prediction:

``` r
rhs_nom <- paste(predictors_nom, collapse = " + ")
model_nom <- lm(paste("imdb_norm ~ ", rhs_nom), data = design)
summary(model_nom)
```

    ## 
    ## Call:
    ## lm(formula = paste("imdb_norm ~ ", rhs_nom), data = design)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.227987 -0.052848 -0.000621  0.048951  0.293050 
    ## 
    ## Coefficients:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                  -8.813e+00  2.261e+00  -3.898
    ## title_typeFeature or TV                       1.598e-01  3.707e-02   4.312
    ## genreAnimation                               -7.838e+00  9.621e+00  -0.815
    ## genreArt House & International                2.683e+00  4.754e+00   0.564
    ## genreComedy                                  -2.844e+00  2.858e+00  -0.995
    ## genreDocumentary                              5.810e+00  3.199e+00   1.816
    ## genreDrama                                    3.415e+00  2.259e+00   1.511
    ## genreHorror                                   5.849e-01  3.686e+00   0.159
    ## genreMusical & Performing Arts                5.803e+00  5.199e+00   1.116
    ## genreMystery & Suspense                       3.590e+00  2.852e+00   1.259
    ## genreOther                                    6.057e+00  4.942e+00   1.226
    ## genreScience Fiction & Fantasy               -2.638e+01  9.438e+00  -2.795
    ## mpaa_ratingR or NC-17                         2.046e-02  2.553e-02   0.801
    ## mpaa_ratingPG                                 3.003e-02  2.557e-02   1.174
    ## mpaa_ratingPG-13                              5.260e-02  2.654e-02   1.982
    ## mpaa_ratingUnrated                           -2.762e-02  3.016e-02  -0.916
    ## dvd_lagYes                                    1.688e-01  5.310e-02   3.180
    ## awards_picYes                                -6.059e-02  2.037e-02  -2.975
    ## thtr_rel_year                                 4.859e-03  1.162e-03   4.183
    ## quickness                                    -1.445e+01  8.322e+00  -1.736
    ## log_votes                                    -3.234e-02  9.298e-03  -3.478
    ## votes_boost                                  -8.057e-02  1.250e-02  -6.444
    ## genreAnimation:thtr_rel_year                  3.719e-03  4.618e-03   0.805
    ## genreArt House & International:thtr_rel_year -1.680e-03  2.407e-03  -0.698
    ## genreComedy:thtr_rel_year                     1.337e-03  1.463e-03   0.914
    ## genreDocumentary:thtr_rel_year               -3.169e-03  1.618e-03  -1.958
    ## genreDrama:thtr_rel_year                     -1.948e-03  1.160e-03  -1.680
    ## genreHorror:thtr_rel_year                    -5.014e-04  1.905e-03  -0.263
    ## genreMusical & Performing Arts:thtr_rel_year -3.008e-03  2.641e-03  -1.139
    ## genreMystery & Suspense:thtr_rel_year        -2.027e-03  1.471e-03  -1.378
    ## genreOther:thtr_rel_year                     -3.185e-03  2.493e-03  -1.277
    ## genreScience Fiction & Fantasy:thtr_rel_year  1.446e-02  4.981e-03   2.902
    ## genreAnimation:quickness                      3.201e+01  2.714e+01   1.179
    ## genreArt House & International:quickness      3.780e+01  2.210e+01   1.710
    ## genreComedy:quickness                         2.607e+01  1.187e+01   2.196
    ## genreDocumentary:quickness                    2.221e+01  1.048e+01   2.121
    ## genreDrama:quickness                          2.155e+01  9.193e+00   2.344
    ## genreHorror:quickness                         3.235e+01  2.270e+01   1.425
    ## genreMusical & Performing Arts:quickness     -1.676e+00  2.276e+01  -0.074
    ## genreMystery & Suspense:quickness             2.289e+01  1.347e+01   1.699
    ## genreOther:quickness                          1.724e+01  1.893e+01   0.910
    ## genreScience Fiction & Fantasy:quickness     -7.108e+01  2.637e+01  -2.695
    ## genreAnimation:log_votes                      5.899e-04  3.307e-02   0.018
    ## genreArt House & International:log_votes      1.825e-02  3.485e-02   0.524
    ## genreComedy:log_votes                        -1.115e-02  1.112e-02  -1.003
    ## genreDocumentary:log_votes                    1.689e-02  1.305e-02   1.295
    ## genreDrama:log_votes                          1.632e-02  8.924e-03   1.828
    ## genreHorror:log_votes                         8.388e-03  2.062e-02   0.407
    ## genreMusical & Performing Arts:log_votes      3.057e-03  1.756e-02   0.174
    ## genreMystery & Suspense:log_votes             1.694e-02  1.232e-02   1.375
    ## genreOther:log_votes                          5.488e-03  1.624e-02   0.338
    ## genreScience Fiction & Fantasy:log_votes     -1.640e-01  4.279e-02  -3.833
    ## dvd_lagYes:log_votes                         -1.540e-02  5.423e-03  -2.839
    ##                                              Pr(>|t|)    
    ## (Intercept)                                  0.000108 ***
    ## title_typeFeature or TV                      1.90e-05 ***
    ## genreAnimation                               0.415554    
    ## genreArt House & International               0.572707    
    ## genreComedy                                  0.320083    
    ## genreDocumentary                             0.069805 .  
    ## genreDrama                                   0.131199    
    ## genreHorror                                  0.873979    
    ## genreMusical & Performing Arts               0.264843    
    ## genreMystery & Suspense                      0.208585    
    ## genreOther                                   0.220835    
    ## genreScience Fiction & Fantasy               0.005358 ** 
    ## mpaa_ratingR or NC-17                        0.423245    
    ## mpaa_ratingPG                                0.240769    
    ## mpaa_ratingPG-13                             0.047967 *  
    ## mpaa_ratingUnrated                           0.360235    
    ## dvd_lagYes                                   0.001551 ** 
    ## awards_picYes                                0.003052 ** 
    ## thtr_rel_year                                3.32e-05 ***
    ## quickness                                    0.083079 .  
    ## log_votes                                    0.000541 ***
    ## votes_boost                                  2.40e-10 ***
    ## genreAnimation:thtr_rel_year                 0.421021    
    ## genreArt House & International:thtr_rel_year 0.485492    
    ## genreComedy:thtr_rel_year                    0.360946    
    ## genreDocumentary:thtr_rel_year               0.050696 .  
    ## genreDrama:thtr_rel_year                     0.093564 .  
    ## genreHorror:thtr_rel_year                    0.792532    
    ## genreMusical & Performing Arts:thtr_rel_year 0.255061    
    ## genreMystery & Suspense:thtr_rel_year        0.168856    
    ## genreOther:thtr_rel_year                     0.201981    
    ## genreScience Fiction & Fantasy:thtr_rel_year 0.003844 ** 
    ## genreAnimation:quickness                     0.238700    
    ## genreArt House & International:quickness     0.087709 .  
    ## genreComedy:quickness                        0.028459 *  
    ## genreDocumentary:quickness                   0.034372 *  
    ## genreDrama:quickness                         0.019380 *  
    ## genreHorror:quickness                        0.154729    
    ## genreMusical & Performing Arts:quickness     0.941334    
    ## genreMystery & Suspense:quickness            0.089895 .  
    ## genreOther:quickness                         0.362947    
    ## genreScience Fiction & Fantasy:quickness     0.007237 ** 
    ## genreAnimation:log_votes                     0.985774    
    ## genreArt House & International:log_votes     0.600758    
    ## genreComedy:log_votes                        0.316236    
    ## genreDocumentary:log_votes                   0.195983    
    ## genreDrama:log_votes                         0.067982 .  
    ## genreHorror:log_votes                        0.684369    
    ## genreMusical & Performing Arts:log_votes     0.861855    
    ## genreMystery & Suspense:log_votes            0.169702    
    ## genreOther:log_votes                         0.735539    
    ## genreScience Fiction & Fantasy:log_votes     0.000140 ***
    ## dvd_lagYes:log_votes                         0.004675 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08506 on 596 degrees of freedom
    ## Multiple R-squared:  0.6198, Adjusted R-squared:  0.5866 
    ## F-statistic: 18.69 on 52 and 596 DF,  p-value: < 2.2e-16

Some of predictor coefficients are not statistically significant.
However, this is expected, since we selected among “nominal” variables,
not distinguishing between individual factor levels.

### 2nd Strategy: Expand Factor Levels (“Effective”)

Each factor with ![l](https://latex.codecogs.com/png.latex?l "l")
levels, encoded as indicator variable, contributes
![l-1](https://latex.codecogs.com/png.latex?l-1 "l-1") degrees of
freedom (1 level is kept as reference). In the second approach, one
treats those “effective” variables as separate predictors.

``` r
full_model_matrix <- expand_factors[ , -full_model_lindeps[-1]]
full_model_data <- as.data.frame(full_model_matrix)[ , -1]
full_model_fit <- lm("imdb_norm ~ .", full_model_data)
```

``` r
# Effective number of parameters, excluding intercept
( p_eff <- full_model_fit$rank - 1 )
```

    ## [1] 150

In this case, we can automate the process of model selection, using
`regsubsets()` from the `leaps` library, which has a similar syntax to
`lm()`. It is laborious to print out the full summary, which returns
separate best models of all sizes up to `nvmax` in the output (similar
to our constructed earlier `X_nom` matrix). Lets concentrate instead on
the
![R^2\_{adj}](https://latex.codecogs.com/png.latex?R%5E2_%7Badj%7D "R^2_{adj}")
score.

``` r
X_eff <- regsubsets(
  formula(full_model_fit), full_model_data, nvmax = p_eff, method = "backward"
)
w <- summary(X_eff)$adjr2
points <- c(eff = 16, max = which.max(w))
plot_score(vec = w) +
  geom_point(
    data = data.frame(idx = points, value = w[points]) -> w_points,
    aes(x = idx, y = value),
    size = 5, stroke = 1, shape = 1, col = "red"
  ) + 
  geom_label(
    data = w_points,
    aes(x = idx, y = value, label = format(value, digits = 3)),
    nudge_y = 0.03, nudge_x = -3, alpha = 0.5
  ) +
  ylab(expression(R[adj]^2))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-22-1.png" width="90%" style="display: block; margin: auto;" />

We can also display the selected variables for the best model with a
given number of predictors, using built-in `plot()` command of
`regsubsets` object. Models are ranked vertically according to the
chosen measure, and contributions of different variables are represented
with black rectangles. (I have silently modified the definition of
`plot.regsubsets()` to be able to change label settings for a slightly
more readable output.)

``` r
plot(
  X_eff, 
  scale = "adjr2", 
  main = "Backwards Stepwise Selection: Predictors",
  ylab = expression(R[adj]^2),
  cex.axis = 0.4
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/backwards-variables-1.png" width="100%" style="display: block; margin: auto;" />

Compared to the first approach, the blue curve stabilizes at a slightly
higher value, and it is harder to discern the particular “parsimonious”
model using the “elbow” method. Consider, for instance, the “effective”
model with 16 variables:

``` r
k_eff <- points[["eff"]]
predictors_eff <- names(coef(X_eff, k_eff))[-1]
rhs_eff <- paste(predictors_eff, collapse = " + ")
model_eff <- lm(paste("imdb_norm ~", rhs_eff), full_model_data)
summary(model_eff)
```

    ## 
    ## Call:
    ## lm(formula = paste("imdb_norm ~", rhs_eff), data = full_model_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.266647 -0.058236  0.002285  0.051757  0.293123 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error t value
    ## (Intercept)                                     7.669e-01  4.515e-02  16.985
    ## genreDocumentary                               -1.690e-01  3.102e-02  -5.448
    ## `genreMusical & Performing Arts`               -1.648e-01  2.788e-02  -5.912
    ## `mpaa_ratingR or NC-17`                         6.130e-02  1.392e-02   4.404
    ## mpaa_ratingPG                                   6.714e-02  1.514e-02   4.434
    ## quickness                                      -6.545e+02  1.198e+02  -5.461
    ## `title_typeFeature or TV:log_votes`             1.640e-02  3.736e-03   4.390
    ## `genreArt House & International:thtr_rel_year` -5.599e-05  1.236e-05  -4.531
    ## `genreDrama:thtr_rel_year`                     -4.544e-05  4.292e-06 -10.585
    ## `genreMystery & Suspense:thtr_rel_year`        -2.648e-05  6.726e-06  -3.937
    ## `mpaa_ratingPG-13:log_votes`                    9.494e-03  1.484e-03   6.396
    ## `dvd_lagYes:quickness`                          1.180e+01  3.314e+00   3.561
    ## `dvd_lagYes:log_votes`                         -1.053e-02  3.128e-03  -3.368
    ## `awards_picYes:thtr_rel_year`                  -3.333e-05  1.008e-05  -3.307
    ## `thtr_rel_year:quickness`                       3.281e-01  5.961e-02   5.505
    ## `thtr_rel_year:log_votes`                      -2.014e-05  2.504e-06  -8.045
    ## `log_votes:votes_boost`                        -6.750e-03  8.900e-04  -7.584
    ##                                                Pr(>|t|)    
    ## (Intercept)                                     < 2e-16 ***
    ## genreDocumentary                               7.31e-08 ***
    ## `genreMusical & Performing Arts`               5.53e-09 ***
    ## `mpaa_ratingR or NC-17`                        1.25e-05 ***
    ## mpaa_ratingPG                                  1.09e-05 ***
    ## quickness                                      6.81e-08 ***
    ## `title_typeFeature or TV:log_votes`            1.33e-05 ***
    ## `genreArt House & International:thtr_rel_year` 7.02e-06 ***
    ## `genreDrama:thtr_rel_year`                      < 2e-16 ***
    ## `genreMystery & Suspense:thtr_rel_year`        9.18e-05 ***
    ## `mpaa_ratingPG-13:log_votes`                   3.11e-10 ***
    ## `dvd_lagYes:quickness`                         0.000397 ***
    ## `dvd_lagYes:log_votes`                         0.000804 ***
    ## `awards_picYes:thtr_rel_year`                  0.000995 ***
    ## `thtr_rel_year:quickness`                      5.38e-08 ***
    ## `thtr_rel_year:log_votes`                      4.27e-15 ***
    ## `log_votes:votes_boost`                        1.20e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08746 on 632 degrees of freedom
    ## Multiple R-squared:  0.5738, Adjusted R-squared:  0.563 
    ## F-statistic: 53.18 on 16 and 632 DF,  p-value: < 2.2e-16

Notice that, compared to the first “nominal” approach, all predictors
turn out to be statistically significant with small p-values. For
example, the negligibly small individual correlation of `thtr_rel_year`
with response may become quite noticeable, if conditioned on other
variables (e.g., such as `genre` or `dvd_lag`). Apart from treating
factor levels differently, the second selection approach does not obey
hierarchy and it is not clear how to interpret the resulting
`model_eff`. Therefore, for stick to the previously found `model_nom`
and inspect it further.

### Model Interpretation and Diagnostics

The selected model has the following form:

![\\widehat{\\mathtt{imdb\\\_norm}} = \\beta_0 + \\pmb{\\beta}\_1^T \\pmb{I}\_{\\mathtt{genre}}^{\\scriptsize\\text{ref: Documentary}} + \\pmb{\\beta}\_5^T \\pmb{I}\_{\\mathtt{mpaa\\\_rating}}^{\\scriptsize\\text{ref: G}} +\\beta_6^{} I\_{\\mathtt{title\\\_type}}^{\\scriptsize\\text{Feature or TV}} + \\beta\_{11}^{} I\_{\\mathtt{awards\\\_pic}}^{\\scriptsize\\text{Yes}} + \\beta\_{12}^{} I\_{\\mathtt{dvd\\\_lag}}^{\\scriptsize\\text{Yes}} +  \\\\
(\\beta_4^{} + \\pmb{\\beta}\_7^T \\pmb{I}\_{\\mathtt{genre}}^{\\scriptsize\\text{ref: Documentary}} ) \\times \\mathtt{thtr\\\_rel\\\_year} + (\\beta_9^{} + \\pmb{\\beta}\_{10}^T \\pmb{I}\_{\\mathtt{genre}}^{\\scriptsize\\text{ref: Documentary}} ) \\times \\mathtt{quickness} + \\\\ 
(\\beta_3^{} + \\pmb{\\beta}\_8^T \\pmb{I}\_{\\mathtt{genre}}^{\\scriptsize\\text{ref: Documentary}} + \\beta\_{13}^{} I\_{\\mathtt{dvd\\\_lag}}^{\\scriptsize\\text{Yes}}) \\times \\mathtt{log\\\_votes} + \\beta_2^{} \\times (\\mathtt{log\\\_votes} - 11.33)\_+ ](https://latex.codecogs.com/png.latex?%5Cwidehat%7B%5Cmathtt%7Bimdb%5C_norm%7D%7D%20%3D%20%5Cbeta_0%20%2B%20%5Cpmb%7B%5Cbeta%7D_1%5ET%20%5Cpmb%7BI%7D_%7B%5Cmathtt%7Bgenre%7D%7D%5E%7B%5Cscriptsize%5Ctext%7Bref%3A%20Documentary%7D%7D%20%2B%20%5Cpmb%7B%5Cbeta%7D_5%5ET%20%5Cpmb%7BI%7D_%7B%5Cmathtt%7Bmpaa%5C_rating%7D%7D%5E%7B%5Cscriptsize%5Ctext%7Bref%3A%20G%7D%7D%20%2B%5Cbeta_6%5E%7B%7D%20I_%7B%5Cmathtt%7Btitle%5C_type%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BFeature%20or%20TV%7D%7D%20%2B%20%5Cbeta_%7B11%7D%5E%7B%7D%20I_%7B%5Cmathtt%7Bawards%5C_pic%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BYes%7D%7D%20%2B%20%5Cbeta_%7B12%7D%5E%7B%7D%20I_%7B%5Cmathtt%7Bdvd%5C_lag%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BYes%7D%7D%20%2B%20%20%5C%5C%0A%28%5Cbeta_4%5E%7B%7D%20%2B%20%5Cpmb%7B%5Cbeta%7D_7%5ET%20%5Cpmb%7BI%7D_%7B%5Cmathtt%7Bgenre%7D%7D%5E%7B%5Cscriptsize%5Ctext%7Bref%3A%20Documentary%7D%7D%20%29%20%5Ctimes%20%5Cmathtt%7Bthtr%5C_rel%5C_year%7D%20%2B%20%28%5Cbeta_9%5E%7B%7D%20%2B%20%5Cpmb%7B%5Cbeta%7D_%7B10%7D%5ET%20%5Cpmb%7BI%7D_%7B%5Cmathtt%7Bgenre%7D%7D%5E%7B%5Cscriptsize%5Ctext%7Bref%3A%20Documentary%7D%7D%20%29%20%5Ctimes%20%5Cmathtt%7Bquickness%7D%20%2B%20%5C%5C%20%0A%28%5Cbeta_3%5E%7B%7D%20%2B%20%5Cpmb%7B%5Cbeta%7D_8%5ET%20%5Cpmb%7BI%7D_%7B%5Cmathtt%7Bgenre%7D%7D%5E%7B%5Cscriptsize%5Ctext%7Bref%3A%20Documentary%7D%7D%20%2B%20%5Cbeta_%7B13%7D%5E%7B%7D%20I_%7B%5Cmathtt%7Bdvd%5C_lag%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BYes%7D%7D%29%20%5Ctimes%20%5Cmathtt%7Blog%5C_votes%7D%20%2B%20%5Cbeta_2%5E%7B%7D%20%5Ctimes%20%28%5Cmathtt%7Blog%5C_votes%7D%20-%2011.33%29_%2B%20 "\widehat{\mathtt{imdb\_norm}} = \beta_0 + \pmb{\beta}_1^T \pmb{I}_{\mathtt{genre}}^{\scriptsize\text{ref: Documentary}} + \pmb{\beta}_5^T \pmb{I}_{\mathtt{mpaa\_rating}}^{\scriptsize\text{ref: G}} +\beta_6^{} I_{\mathtt{title\_type}}^{\scriptsize\text{Feature or TV}} + \beta_{11}^{} I_{\mathtt{awards\_pic}}^{\scriptsize\text{Yes}} + \beta_{12}^{} I_{\mathtt{dvd\_lag}}^{\scriptsize\text{Yes}} +  \\
(\beta_4^{} + \pmb{\beta}_7^T \pmb{I}_{\mathtt{genre}}^{\scriptsize\text{ref: Documentary}} ) \times \mathtt{thtr\_rel\_year} + (\beta_9^{} + \pmb{\beta}_{10}^T \pmb{I}_{\mathtt{genre}}^{\scriptsize\text{ref: Documentary}} ) \times \mathtt{quickness} + \\ 
(\beta_3^{} + \pmb{\beta}_8^T \pmb{I}_{\mathtt{genre}}^{\scriptsize\text{ref: Documentary}} + \beta_{13}^{} I_{\mathtt{dvd\_lag}}^{\scriptsize\text{Yes}}) \times \mathtt{log\_votes} + \beta_2^{} \times (\mathtt{log\_votes} - 11.33)_+ ")

Here the notation like
![I\_{\\mathtt{title\\\_type}}^{\\scriptsize\\text{Feature or TV}}](https://latex.codecogs.com/png.latex?I_%7B%5Cmathtt%7Btitle%5C_type%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BFeature%20or%20TV%7D%7D "I_{\mathtt{title\_type}}^{\scriptsize\text{Feature or TV}}")
denotes indicator variable, taking the value 1 for specified level of a
binary predictor. The bold font denotes the vector of such indicator
variables, with respect to specified reference level. The interpretation
of the first line, thus, is that it gives average IMDB score
`imdb_norm`, whose value w.r.t. baseline
![\\beta_0](https://latex.codecogs.com/png.latex?%5Cbeta_0 "\beta_0")
changes for each category by the corresponding coefficients
![\\beta_i](https://latex.codecogs.com/png.latex?%5Cbeta_i "\beta_i"),
provided below:

``` r
kable(model_nom$coefficients) %>% kable_styling("striped") %>% scroll_box(height = "400px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
x
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:right;">
-8.8134000
</td>
</tr>
<tr>
<td style="text-align:left;">
title_typeFeature or TV
</td>
<td style="text-align:right;">
0.1598500
</td>
</tr>
<tr>
<td style="text-align:left;">
genreAnimation
</td>
<td style="text-align:right;">
-7.8382020
</td>
</tr>
<tr>
<td style="text-align:left;">
genreArt House & International
</td>
<td style="text-align:right;">
2.6830868
</td>
</tr>
<tr>
<td style="text-align:left;">
genreComedy
</td>
<td style="text-align:right;">
-2.8436431
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDocumentary
</td>
<td style="text-align:right;">
5.8101353
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDrama
</td>
<td style="text-align:right;">
3.4147140
</td>
</tr>
<tr>
<td style="text-align:left;">
genreHorror
</td>
<td style="text-align:right;">
0.5849411
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMusical & Performing Arts
</td>
<td style="text-align:right;">
5.8026147
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMystery & Suspense
</td>
<td style="text-align:right;">
3.5900507
</td>
</tr>
<tr>
<td style="text-align:left;">
genreOther
</td>
<td style="text-align:right;">
6.0573538
</td>
</tr>
<tr>
<td style="text-align:left;">
genreScience Fiction & Fantasy
</td>
<td style="text-align:right;">
-26.3793537
</td>
</tr>
<tr>
<td style="text-align:left;">
mpaa_ratingR or NC-17
</td>
<td style="text-align:right;">
0.0204625
</td>
</tr>
<tr>
<td style="text-align:left;">
mpaa_ratingPG
</td>
<td style="text-align:right;">
0.0300290
</td>
</tr>
<tr>
<td style="text-align:left;">
mpaa_ratingPG-13
</td>
<td style="text-align:right;">
0.0526017
</td>
</tr>
<tr>
<td style="text-align:left;">
mpaa_ratingUnrated
</td>
<td style="text-align:right;">
-0.0276175
</td>
</tr>
<tr>
<td style="text-align:left;">
dvd_lagYes
</td>
<td style="text-align:right;">
0.1688346
</td>
</tr>
<tr>
<td style="text-align:left;">
awards_picYes
</td>
<td style="text-align:right;">
-0.0605906
</td>
</tr>
<tr>
<td style="text-align:left;">
thtr_rel_year
</td>
<td style="text-align:right;">
0.0048590
</td>
</tr>
<tr>
<td style="text-align:left;">
quickness
</td>
<td style="text-align:right;">
-14.4475239
</td>
</tr>
<tr>
<td style="text-align:left;">
log_votes
</td>
<td style="text-align:right;">
-0.0323427
</td>
</tr>
<tr>
<td style="text-align:left;">
votes_boost
</td>
<td style="text-align:right;">
-0.0805682
</td>
</tr>
<tr>
<td style="text-align:left;">
genreAnimation:thtr_rel_year
</td>
<td style="text-align:right;">
0.0037185
</td>
</tr>
<tr>
<td style="text-align:left;">
genreArt House & International:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0016801
</td>
</tr>
<tr>
<td style="text-align:left;">
genreComedy:thtr_rel_year
</td>
<td style="text-align:right;">
0.0013371
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDocumentary:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0031686
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDrama:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0019482
</td>
</tr>
<tr>
<td style="text-align:left;">
genreHorror:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0005014
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMusical & Performing Arts:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0030083
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMystery & Suspense:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0020266
</td>
</tr>
<tr>
<td style="text-align:left;">
genreOther:thtr_rel_year
</td>
<td style="text-align:right;">
-0.0031850
</td>
</tr>
<tr>
<td style="text-align:left;">
genreScience Fiction & Fantasy:thtr_rel_year
</td>
<td style="text-align:right;">
0.0144560
</td>
</tr>
<tr>
<td style="text-align:left;">
genreAnimation:quickness
</td>
<td style="text-align:right;">
32.0139495
</td>
</tr>
<tr>
<td style="text-align:left;">
genreArt House & International:quickness
</td>
<td style="text-align:right;">
37.8040842
</td>
</tr>
<tr>
<td style="text-align:left;">
genreComedy:quickness
</td>
<td style="text-align:right;">
26.0731137
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDocumentary:quickness
</td>
<td style="text-align:right;">
22.2141732
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDrama:quickness
</td>
<td style="text-align:right;">
21.5519849
</td>
</tr>
<tr>
<td style="text-align:left;">
genreHorror:quickness
</td>
<td style="text-align:right;">
32.3505248
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMusical & Performing Arts:quickness
</td>
<td style="text-align:right;">
-1.6759425
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMystery & Suspense:quickness
</td>
<td style="text-align:right;">
22.8850544
</td>
</tr>
<tr>
<td style="text-align:left;">
genreOther:quickness
</td>
<td style="text-align:right;">
17.2388548
</td>
</tr>
<tr>
<td style="text-align:left;">
genreScience Fiction & Fantasy:quickness
</td>
<td style="text-align:right;">
-71.0800298
</td>
</tr>
<tr>
<td style="text-align:left;">
genreAnimation:log_votes
</td>
<td style="text-align:right;">
0.0005899
</td>
</tr>
<tr>
<td style="text-align:left;">
genreArt House & International:log_votes
</td>
<td style="text-align:right;">
0.0182494
</td>
</tr>
<tr>
<td style="text-align:left;">
genreComedy:log_votes
</td>
<td style="text-align:right;">
-0.0111524
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDocumentary:log_votes
</td>
<td style="text-align:right;">
0.0168901
</td>
</tr>
<tr>
<td style="text-align:left;">
genreDrama:log_votes
</td>
<td style="text-align:right;">
0.0163169
</td>
</tr>
<tr>
<td style="text-align:left;">
genreHorror:log_votes
</td>
<td style="text-align:right;">
0.0083880
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMusical & Performing Arts:log_votes
</td>
<td style="text-align:right;">
0.0030572
</td>
</tr>
<tr>
<td style="text-align:left;">
genreMystery & Suspense:log_votes
</td>
<td style="text-align:right;">
0.0169374
</td>
</tr>
<tr>
<td style="text-align:left;">
genreOther:log_votes
</td>
<td style="text-align:right;">
0.0054877
</td>
</tr>
<tr>
<td style="text-align:left;">
genreScience Fiction & Fantasy:log_votes
</td>
<td style="text-align:right;">
-0.1640014
</td>
</tr>
<tr>
<td style="text-align:left;">
dvd_lagYes:log_votes
</td>
<td style="text-align:right;">
-0.0153970
</td>
</tr>
</tbody>
</table>

</div>

Recall that the predicted score is related to the original IMDB rating
\[1,10\] as follows:

![\\mathtt{imdb\\\_rating} = 10 \* \\left( 1 - \\frac{10^\\mathtt{imdb\\\_norm}}{10} \\right)](https://latex.codecogs.com/png.latex?%5Cmathtt%7Bimdb%5C_rating%7D%20%3D%2010%20%2A%20%5Cleft%28%201%20-%20%5Cfrac%7B10%5E%5Cmathtt%7Bimdb%5C_norm%7D%7D%7B10%7D%20%5Cright%29 "\mathtt{imdb\_rating} = 10 * \left( 1 - \frac{10^\mathtt{imdb\_norm}}{10} \right)")

Thus, `imdb_norm` can be interpreted as an exponent, giving the
“fractional relative decrease” of the movie rating with respect to the
maximum available value of 10. Given that
![\\log xy = \\log x + \\log y](https://latex.codecogs.com/png.latex?%5Clog%20xy%20%3D%20%5Clog%20x%20%2B%20%5Clog%20y "\log xy = \log x + \log y"),
for an additional increase in numeric predictor
![X_j](https://latex.codecogs.com/png.latex?X_j "X_j") by 1 unit, all
else being equal, the decrease in the movie rating (w.r.t. 10) is
multiplied by
![10^{\\beta_j}](https://latex.codecogs.com/png.latex?10%5E%7B%5Cbeta_j%7D "10^{\beta_j}").
The corresponding coefficient
![\\beta_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j") is
modified according to the levels of categorical predictors, as given by
the above formula.

In case of the 3rd line, the 1 unit increase in
`log_votes = log10(imdb_num_votes)` actually corresponds to 10 times
increase in `imdb_num_votes`, resulting in the *elastic* multiplicative
change
![10^{\\beta}](https://latex.codecogs.com/png.latex?10%5E%7B%5Cbeta%7D "10^{\beta}")
of `imdb_rating`, all else being fixed. The final truncated term
![(\\mathtt{log\\\_votes}-11.3)\_+ = I\_{\\mathtt{imdb\\\_num\\\_votes}> 10^{11.3}}^{\\scriptsize\\text{Yes}} \\times \\log\_{10}(\\mathtt{imdb\\\_num\\\_votes}/10^{11.3})](https://latex.codecogs.com/png.latex?%28%5Cmathtt%7Blog%5C_votes%7D-11.3%29_%2B%20%3D%20I_%7B%5Cmathtt%7Bimdb%5C_num%5C_votes%7D%3E%2010%5E%7B11.3%7D%7D%5E%7B%5Cscriptsize%5Ctext%7BYes%7D%7D%20%5Ctimes%20%5Clog_%7B10%7D%28%5Cmathtt%7Bimdb%5C_num%5C_votes%7D%2F10%5E%7B11.3%7D%29 "(\mathtt{log\_votes}-11.3)_+ = I_{\mathtt{imdb\_num\_votes}> 10^{11.3}}^{\scriptsize\text{Yes}} \times \log_{10}(\mathtt{imdb\_num\_votes}/10^{11.3})")
is effectively changes both the average value (intercept) and
coefficient of change (slope) for very large number of votes.

The application of multiple linear regression methods generally depend
on the set of conditions, that we can verify, using following
[diagnositic
plots](https://www.qualtrics.com/support/stats-iq/analyses/regression-guides/interpreting-residual-plots-improve-regression/):

``` r
model <- model_nom
resid_hist <- ggplot(model, aes(x = .resid)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(model$residuals), sd = sd(model$residuals))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate(
    "text", x = -0.2, y = 4.2,
    label = paste("Kurtosis:", round(kurtosis(model$residuals), digits = 3))
  ) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "Residuals", y = "Probability Density")
resid_qqnorm <- ggplot(model, aes(sample = .resid)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot for Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
resid_fitted <- ggplot(model, aes(x = .fitted, y = abs(.resid))) +
  geom_jitter(shape = 16, size = 2, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Absolute Values of Residuals")
resid_order <- ggplot(model, aes(x = 1:length(.resid), y = .resid)) +
  geom_point(shape = 21, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Independence", x = "Order of Collection", y = "Residuals")
grid.arrange(resid_hist, resid_qqnorm, resid_fitted, resid_order, ncol = 2, nrow = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/model-diagnostics-1.png" width="90%" style="display: block; margin: auto;" />

1.  The *residuals are nearly normal* around 0. They are slightly
    over-dispersed with thick tails (positive excess kurtosys, flipped
    S-shape on QQ-plot), but deviations from normality are negligibly
    small (especially for large data sets).
2.  The *variability of the residuals is approximately constant*
    (homoscedasticity), without obvious trend, as can be seen from the
    absolute values of residuals, plotted against fitted values of IMDB
    score. The slightly higher variance at the relatively high values of
    `imdb_norm` (corresponding to larger `imdb_rating`) is mainly due to
    several outlying predictions. We will return to this issue shortly.
3.  The apparent randomness of residuals w.r.t. order of collection
    implies that they are *independent*, coming from a random sample.
4.  Each variable should be *linearly related* to the outcome. We
    already verified linearity assumption, when discussed
    `imdb_norm vs. log_votes` scatterplot and features design. To check
    linear relationship with other numeric predictors, we plot residuals
    against each of them.

``` r
resid_quickness <- ggplot(broom::augment(model), aes(x = design$quickness, y = .resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Quickness = 1 / Runtime", y = NULL)
resid_year <- ggplot(broom::augment(model), aes(x = design$thtr_rel_year, y = .resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Year of Release in Theater", y = NULL)
resid_month <- ggplot(broom::augment(model), aes(x = design$thtr_rel_month, y = .resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Month of Release in Theater", y = NULL)
resid_day <- ggplot(broom::augment(model), aes(x = design$thtr_rel_day, y = .resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Day of Release in Theater", y = NULL)
grid.arrange(
  resid_quickness, resid_year, resid_month, resid_day, ncol = 2,
  top = grid::textGrob(
    "Linearity Assumption: Residuals vs. Predictors", gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-25-1.png" width="90%" style="display: block; margin: auto;" />
Finally, in order to visually assess the accuracy of predictions, we
plot the actual IMDB scores against fitted values (with diagonal and
local regression lines on top, to see any patterns).

``` r
imdb_predicted <- ggplot(broom::augment(model), aes(x = .fitted, y = imdb_norm)) + 
  geom_point(aes(color = imdb_norm - .fitted < 0.17 | imdb_norm < 0.5), shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  theme(legend.position = "top") + 
  labs(x = "Fitted Values", y = "IMDB Score", title = "Actual Values vs. Predicted")
imdb_predicted
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project3_reg_model_files/figure-gfm/unnamed-chunk-26-1.png" width="90%" style="display: block; margin: auto;" />

We clearly see linear trend, with the normal scatter spreading at
distance 0.2 about the null. The color denotes outlying observations
with largest residuals in the lower region of `imdb_rating` (see
discussion in Conclusion).

------------------------------------------------------------------------

## Part 5: Prediction

The good practice is to split data into “training” and “test” subsets,
and to use the second one for validation of the accuracy of predictions
on the data, not used for the model fitting. Instead, we used adjusted
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") measure as a
means to compensate possible over-fitting (another way is
cross-validation). Lets check how our model performs on some
out-of-sample observations. We pick a movie from 2016 (and a handful of
others not in the original dataset), with information gathered from
[Rotten Tomatoes](https://www.rottentomatoes.com/) and
[IMDB](https://www.imdb.com/):

``` r
kable(new_data) %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
title_type
</th>
<th style="text-align:left;">
genre
</th>
<th style="text-align:right;">
runtime
</th>
<th style="text-align:left;">
mpaa_rating
</th>
<th style="text-align:right;">
thtr_rel_year
</th>
<th style="text-align:left;">
dvd_lag
</th>
<th style="text-align:right;">
imdb_rating
</th>
<th style="text-align:right;">
imdb_num_votes
</th>
<th style="text-align:left;">
critics_rating
</th>
<th style="text-align:right;">
critics_score
</th>
<th style="text-align:left;">
audience_rating
</th>
<th style="text-align:right;">
audience_score
</th>
<th style="text-align:left;">
awards_pic
</th>
<th style="text-align:left;">
top200_box
</th>
<th style="text-align:left;">
imdb_url
</th>
<th style="text-align:left;">
rt_url
</th>
<th style="text-align:right;">
imdb_norm
</th>
<th style="text-align:right;">
log_votes
</th>
<th style="text-align:right;">
quickness
</th>
<th style="text-align:right;">
votes_boost
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
The Conjuring 2
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Horror
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:left;">
R or NC-17
</td>
<td style="text-align:right;">
2016
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
247809
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt3065204/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/the_conjuring_2>
</td>
<td style="text-align:right;">
0.4313638
</td>
<td style="text-align:right;">
12.420414
</td>
<td style="text-align:right;">
0.0074627
</td>
<td style="text-align:right;">
1.0904136
</td>
</tr>
<tr>
<td style="text-align:left;">
Pleasantville
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
1998
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
7.5
</td>
<td style="text-align:right;">
124997
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0120789/?ref_=ttspec_ql>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/pleasantville>
</td>
<td style="text-align:right;">
0.3979400
</td>
<td style="text-align:right;">
11.736045
</td>
<td style="text-align:right;">
0.0080645
</td>
<td style="text-align:right;">
0.4060450
</td>
</tr>
<tr>
<td style="text-align:left;">
Forrest Gump
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:right;">
142
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
1994
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1904201
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0109830/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/forrest_gump>
</td>
<td style="text-align:right;">
0.0791812
</td>
<td style="text-align:right;">
14.459573
</td>
<td style="text-align:right;">
0.0070423
</td>
<td style="text-align:right;">
3.1295731
</td>
</tr>
<tr>
<td style="text-align:left;">
Ferris Bueller’s Day Off
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
1986
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
335380
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
92
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0091042/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/ferris_buellers_day_off>
</td>
<td style="text-align:right;">
0.3424227
</td>
<td style="text-align:right;">
12.723019
</td>
<td style="text-align:right;">
0.0097087
</td>
<td style="text-align:right;">
1.3930195
</td>
</tr>
<tr>
<td style="text-align:left;">
Crank
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:left;">
R or NC-17
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
241402
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0479884/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/crank>
</td>
<td style="text-align:right;">
0.4913617
</td>
<td style="text-align:right;">
12.394219
</td>
<td style="text-align:right;">
0.0120482
</td>
<td style="text-align:right;">
1.0642189
</td>
</tr>
<tr>
<td style="text-align:left;">
Prince of Darkness
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Horror
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:left;">
R or NC-17
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
40033
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0093777/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/prince_of_darkness>
</td>
<td style="text-align:right;">
0.5185139
</td>
<td style="text-align:right;">
10.597459
</td>
<td style="text-align:right;">
0.0098039
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Religulous
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:left;">
R or NC-17
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
58537
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0815241/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/religulous>
</td>
<td style="text-align:right;">
0.3802112
</td>
<td style="text-align:right;">
10.977414
</td>
<td style="text-align:right;">
0.0099010
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Metal: A Headbanger’s Journey
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:left;">
R or NC-17
</td>
<td style="text-align:right;">
2005
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
11950
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0478209/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/metal_a_headbangers_journey>
</td>
<td style="text-align:right;">
0.2787536
</td>
<td style="text-align:right;">
9.388487
</td>
<td style="text-align:right;">
0.0104167
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Catwoman
</td>
<td style="text-align:left;">
Feature or TV
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2004
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
3.4
</td>
<td style="text-align:right;">
114846
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
<https://www.imdb.com/title/tt0327554/?ref_=adv_li_tt>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/catwoman>
</td>
<td style="text-align:right;">
0.8195439
</td>
<td style="text-align:right;">
11.651347
</td>
<td style="text-align:right;">
0.0096154
</td>
<td style="text-align:right;">
0.3213474
</td>
</tr>
</tbody>
</table>

</div>

Using developed model, perform a prediction of `imdb_norm` and then
back-transform it to original scale, for ease of interpretation.

``` r
predictions <- as.data.frame(
  predict( model_nom, newdata = new_data, interval = "prediction" )
) %>% transmute( 
  imdb_fit = get_imdb(fit), imdb_lwr = get_imdb(upr), imdb_upr = get_imdb(lwr)
) %>% round(1)
new_data %>% select(title, imdb_rating) %>% 
  cbind( predictions) %>% mutate(
    `lwr < actual < upr` = imdb_lwr < imdb_rating & imdb_rating < imdb_upr
  ) %>% kable() %>% kable_styling("striped") 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:right;">
imdb_rating
</th>
<th style="text-align:right;">
imdb_fit
</th>
<th style="text-align:right;">
imdb_lwr
</th>
<th style="text-align:right;">
imdb_upr
</th>
<th style="text-align:left;">
lwr \< actual \< upr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
The Conjuring 2
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
4.9
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Pleasantville
</td>
<td style="text-align:right;">
7.5
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Forrest Gump
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Ferris Bueller’s Day Off
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
7.1
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Crank
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
7.2
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Prince of Darkness
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
6.5
</td>
<td style="text-align:right;">
4.8
</td>
<td style="text-align:right;">
7.7
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Religulous
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
7.9
</td>
<td style="text-align:right;">
6.8
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Metal: A Headbanger’s Journey
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
Catwoman
</td>
<td style="text-align:right;">
3.4
</td>
<td style="text-align:right;">
6.1
</td>
<td style="text-align:right;">
4.2
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
</tbody>
</table>

Although new data is not randomly sampled, I tried different movies in
terms of score, genre, time period, etc. The last columns display lower
and upper boundaries of prediction interval, as well whether it managed
to capture the actual IMDB score in each case.

In general, the accuracy of
![\\hat{Y}](https://latex.codecogs.com/png.latex?%5Chat%7BY%7D "\hat{Y}")
as a prediction for
![Y=f(X)+\\epsilon](https://latex.codecogs.com/png.latex?Y%3Df%28X%29%2B%5Cepsilon "Y=f(X)+\epsilon")
depends on some *reducible* error (associated to imperfect estimate
![\\hat{f}](https://latex.codecogs.com/png.latex?%5Chat%7Bf%7D "\hat{f}")),
as well as *irreducible* random error
![\\epsilon](https://latex.codecogs.com/png.latex?%5Cepsilon "\epsilon"),
that does not depend on
![X](https://latex.codecogs.com/png.latex?X "X"). The first type of
uncertainty, surrounding average values over a large number of movies in
our case (e.g., variability of coefficient estimates
![\\hat{\\beta}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cbeta%7D "\hat{\beta}")
in linear regression), can be quantified by *confidence interval*. For a
given confidence level
![\\alpha = 0.95](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%200.95 "\alpha = 0.95"),
95% of similarly constructed intervals for a large number of data
samples will contain the true value of
![f(X)](https://latex.codecogs.com/png.latex?f%28X%29 "f(X)").

On the other hand, a *prediction interval* can be used to quantify how
much an individual point will differ from the population regression
plane, that is, the uncertainty surrounding IMDB score for a
*particular* movie in our case. We interpret this to mean that 95 % of
intervals of this form will contain the true value of `imdb_rating` for
a given city. The prediction interval is always wider.

It seems our model performs not so bad, but there is a plenty room for
improvement (especially, for some [“notoriously bad”
movies](https://www.imdb.com/search/title/?groups=bottom_250&view=simple&sort=user_rating,asc&ref_=adv_prv)).

------------------------------------------------------------------------

## Part 6: Conclusion

In this project, I have attempted to find a reasonably “small” model,
which is good at explaining the relationship between movies attributes
and its rating on IMDB, that also have small errors and, thus, is good
for predicting the latter. Moreover, I have chosen not to use another
ratings/scores on Rotten Tomatoes as potential predictors (since they,
essentially, represent the same information as response). Using backward
feature elimination strategy and
![R\_{adj}^2](https://latex.codecogs.com/png.latex?R_%7Badj%7D%5E2 "R_{adj}^2")
as model selection criterion, I have ranked predictors by their impact
on prediction accuracy. The most important attributes turned out to be
`genre`, `log_votes`, `thtr_rel_year`, `mpaa_rating`, `title_type`,
followed by interactions `thtr_rel_year:genre`, `log_votes:genre`,
`quickness`, `quickness:genre`, `awards_pic`, `dvd_lag` and
`dvd_lag:log_votes`. The latter categorical predictors specify, if the
movie was nominated or won an Oscar, as well as whether its release on
dvd appeared shortly, or it was in fact “re-released” from the past
(e.g., when the technology became available).

Note that the resulting model is effectively non-linear, fitting
log-transformed response and `imdb_num_votes` as well as inverse
`1/runtime = quickness`. I implemented the model search strategy, that
only eliminates categorical predictors as a whole (not individual factor
levels) and also respects the hierarchy rules (i.e., not to eliminate
predictors, if their interactions are still in the model). The resulting
economic model explains about 62% of variation in response
(![R^2=0.619](https://latex.codecogs.com/png.latex?R%5E2%3D0.619 "R^2=0.619"),
and penalized
![R\_{adj}^2=0.586](https://latex.codecogs.com/png.latex?R_%7Badj%7D%5E2%3D0.586 "R_{adj}^2=0.586")).
This is contrasted with the automated search using `leaps` package, that
does not satisfy the above conditions, and leads to less tractable
results, with only small boost in accuracy.

The selected model somewhat underperforms in the region of really low
scores. Lets look closer at the movies, that constitute most problematic
observations for our model, highlighted in red on the “Actual
vs. Predicted” plot above:

``` r
pred <- predict(model_nom, design)
filter(movies, runtime > 60)[design$imdb_norm - pred > 0.17 & design$imdb_norm > 0.5, ] %>% 
  select(-c(runtime, studio, thtr_rel_month:dvd_rel_day, best_pic_nom:actor5)) %>% 
  kable(digits = 3) %>% kable_styling("striped") %>% scroll_box(width = "100%", height = "400px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
title
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
title_type
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
genre
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
mpaa_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
thtr_rel_year
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
imdb_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
imdb_num_votes
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
critics_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
critics_score
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
audience_rating
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
audience_score
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
imdb_url
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
rt_url
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rhinestone
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:left;">
PG
</td>
<td style="text-align:right;">
1984
</td>
<td style="text-align:right;">
3.6
</td>
<td style="text-align:right;">
5544
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0088001/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/rhinestone/
</td>
</tr>
<tr>
<td style="text-align:left;">
Dragon Wars
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
3.6
</td>
<td style="text-align:right;">
21268
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0372873/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/dragon_wars/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Room
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:right;">
3.5
</td>
<td style="text-align:right;">
27769
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0368226/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/the_room_1998/
</td>
</tr>
<tr>
<td style="text-align:left;">
Jack and Jill
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:left;">
PG
</td>
<td style="text-align:right;">
2011
</td>
<td style="text-align:right;">
3.4
</td>
<td style="text-align:right;">
57933
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0810913/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/jack_and_jill_2011/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Mod Squad
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1999
</td>
<td style="text-align:right;">
4.1
</td>
<td style="text-align:right;">
7658
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0120757/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/mod_squad/
</td>
</tr>
<tr>
<td style="text-align:left;">
Madea Goes to Jail
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:right;">
4.1
</td>
<td style="text-align:right;">
9424
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1142800/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/madea-goes-to-jail/
</td>
</tr>
<tr>
<td style="text-align:left;">
Jade
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1995
</td>
<td style="text-align:right;">
5.1
</td>
<td style="text-align:right;">
8999
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0113451/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/jade/
</td>
</tr>
<tr>
<td style="text-align:left;">
Liz & Dick
</td>
<td style="text-align:left;">
TV Movie
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
Unrated
</td>
<td style="text-align:right;">
2012
</td>
<td style="text-align:right;">
4.1
</td>
<td style="text-align:right;">
3505
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt2375255/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/liz_and_dick/
</td>
</tr>
<tr>
<td style="text-align:left;">
Epic Movie
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
2.3
</td>
<td style="text-align:right;">
87652
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0799949/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/epic_movie/
</td>
</tr>
<tr>
<td style="text-align:left;">
Porky’s Revenge
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Art House & International
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1985
</td>
<td style="text-align:right;">
4.6
</td>
<td style="text-align:right;">
5863
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0089826/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/porkys_revenge/
</td>
</tr>
<tr>
<td style="text-align:left;">
Disaster Movie
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Comedy
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:right;">
1.9
</td>
<td style="text-align:right;">
73219
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1213644/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/disaster_movie/
</td>
</tr>
<tr>
<td style="text-align:left;">
Meteor
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
PG
</td>
<td style="text-align:right;">
1979
</td>
<td style="text-align:right;">
4.9
</td>
<td style="text-align:right;">
5136
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0079550/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1013772-meteor/
</td>
</tr>
<tr>
<td style="text-align:left;">
Town & Country
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2000
</td>
<td style="text-align:right;">
4.3
</td>
<td style="text-align:right;">
4072
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0141907/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/town_and_country/
</td>
</tr>
<tr>
<td style="text-align:left;">
Doogal
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Animation
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:right;">
2.8
</td>
<td style="text-align:right;">
3790
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0763304/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1156283-doogal/
</td>
</tr>
<tr>
<td style="text-align:left;">
Body of Evidence
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Mystery & Suspense
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
4.3
</td>
<td style="text-align:right;">
11125
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0106453/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/body_of_evidence/
</td>
</tr>
</tbody>
</table>

</div>

By inspecting metadata, we can observe the commonality that all of the
movies have “Rotten” critics rating. Had we included discrete
`critics_rating` and/or `audience_rating` as predictors, this would
reduce the spread (improving
![R\_{adj}^2](https://latex.codecogs.com/png.latex?R_%7Badj%7D%5E2 "R_{adj}^2")
by about 0.1-0.15), but does not get rid of the largest residuals
completely. Instead, we may also notice on the corresponding webpages of
examples from the above, that they are universally recognized as *worst
movies* - the large majority of them received various parody awards like
“Razzie”, “The Stinkers Bad Movie Awards”, or “Golden Schmoes Awards”.
In analogy to already existing `best_pic_nom`, `best_pic_win` in the
original sample, it could be interesting to collect also such data about
“worst_pic” awards, and use it for prediction.
