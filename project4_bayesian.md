Bayesian Model Averaging for Movie Scores
================
Vadim Belov

-   [Setup](#setup)
    -   [Task](#task)
    -   [Load packages](#load-packages)
    -   [Load data](#load-data)
-   [Part 1: Data](#part-1-data)
-   [Part 2: Data manipulation](#part-2-data-manipulation)
    -   [Missing Values](#missing-values)
-   [Part 3: Exploratory data
    analysis](#part-3-exploratory-data-analysis)
    -   [Numerical Predictors](#numerical-predictors)
    -   [Linear Relationship and Variable
        Transformations](#linear-relationship-and-variable-transformations)
    -   [Categorical Predictors](#categorical-predictors)
-   [Part 4: Modeling](#part-4-modeling)
    -   [Full Model Specification and
        Posteriors](#full-model-specification-and-posteriors)
    -   [Model Uncertainty and
        Averaging](#model-uncertainty-and-averaging)
    -   [Model Interpretation and
        Diagnostics](#model-interpretation-and-diagnostics)
-   [Part 5: Prediction](#part-5-prediction)
-   [Part 6: Conclusion](#part-6-conclusion)

## Setup

Some code chunks are hidden by default in the HTML version (mostly for
plotting), but you can easily unfold them, by clicking on the “code”
tab.

### Task

Transform variables according to guidelines, describe dataset and
perform EDA. Develop a Bayesian regression model to predict
`audience_score` from the selected variables. Make a prediction for a
previously unseen data (cf. [project
<<<<<<< HEAD
requirements](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info4_bayesian.md)).
=======
requirements](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info4_bayesian)).
>>>>>>> e52aa905ef579c3e829c745f73239c19e671d57c

### Load packages

``` r
library(ggplot2)    # graphics
library(gridExtra)  # multiple side-by-side ggplot's
library(GGally)     # pairwise correlation plots
library(dplyr)      # data manipulation
library(tidyr)      # data cleaning and representation change
library(BAS)        # Bayesian Variable Selection and Model Averaging
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

In general, we are interested in learning what attributes make a movie
popular. **The specific task** is as follows: Develop a Bayesian
regression model to predict `audience_score` from certain explanatory
variables.

------------------------------------------------------------------------

## Part 2: Data manipulation

We construct the design matrix to include only relevant target variable
and all potentially useful predictors, that will be considered for the
model. Therefore, we can safely omit dummy variables (like title or URL)
and those clearly useless for prediction (such as actor or director),
that cannot be generalized to the population.

| variable           | description                                                                           |
|:-------------------|:--------------------------------------------------------------------------------------|
| `feature_film`     | “yes” if title_type is Feature Film, “no” otherwise                                   |
| `drama`            | “yes” if genre is Drama, “no” otherwise                                               |
| `runtime`          | Runtime of movie (in minutes)                                                         |
| `mpaa_rating_R`    | “yes” if mpaa_rating is R, “no” otherwise                                             |
| `thtr_rel_year`    | Year the movie is released in theaters                                                |
| `oscar_season`     | “yes” if movie is released in November, October, or December, “no” otherwise          |
| `summer_season`    | “yes” if movie is released in May, June, July, or August, “no” otherwise              |
| `imdb_rating`      | Rating on IMDB, between \[1,10\]                                                      |
| `imdb_num_votes`   | Number of votes on IMDB                                                               |
| `critics_score`    | Critics score on Rotten Tomatoes                                                      |
| `best_pic_nom`     | Whether or not the movie was nominated for a best picture Oscar (no, yes)             |
| `best_pic_win`     | Whether or not the movie won a best picture Oscar (no, yes)                           |
| `best_actor_win`   | Whether or not one of the main actors in the movie ever won an Oscar (no, yes)        |
| `best_actress_win` | Whether or not one of the main actresses in the movie ever won an Oscar (no, yes)     |
| `best_dir_win`     | Whether or not the director of the movie ever won an Oscar (no, yes)                  |
| `top200_box`       | Whether or not the movie is in the Top 200 Box Office list on BoxOfficeMojo (no, yes) |

We have to turn some of the original categorical predictors with
multiple levels (`title_type`, `genre`, `mpaa_rating`, and also
`thtr_rel_month`) into simple binary variables:

``` r
design <- select(movies,
  audience_score, critics_score, imdb_rating, imdb_num_votes, runtime, thtr_rel_year, thtr_rel_month,
  title_type, genre, mpaa_rating, best_pic_nom:top200_box
) %>% mutate(
  feature_film = as.factor( ifelse(title_type == "Feature Film", "yes", "no") ),
  drama = as.factor( ifelse(genre == "Drama", "yes", "no") ),
  mpaa_rating_R = as.factor( ifelse(mpaa_rating == "R", "yes", "no") ),
  oscar_season = as.factor( ifelse(thtr_rel_month %in% 10:12, "yes", "no") ),
  summer_season = as.factor( ifelse(thtr_rel_month %in% 5:8, "yes", "no") ),
  .keep = "unused"
)
glimpse(design)
```

    ## Rows: 651
    ## Columns: 17
    ## $ audience_score   <dbl> 73, 81, 91, 76, 27, 86, 76, 47, 89, 66, 75, 46, 89, 5~
    ## $ critics_score    <dbl> 45, 96, 91, 80, 33, 91, 57, 17, 90, 83, 89, 67, 80, 2~
    ## $ imdb_rating      <dbl> 5.5, 7.3, 7.6, 7.2, 5.1, 7.8, 7.2, 5.5, 7.5, 6.6, 6.8~
    ## $ imdb_num_votes   <int> 899, 12285, 22381, 35096, 2386, 333, 5016, 2272, 880,~
    ## $ runtime          <dbl> 80, 101, 84, 139, 90, 78, 142, 93, 88, 119, 127, 108,~
    ## $ thtr_rel_year    <dbl> 2013, 2001, 1996, 1993, 2004, 2009, 1986, 1996, 2012,~
    ## $ best_pic_nom     <fct> no, no, no, no, no, no, no, no, no, no, no, no, no, n~
    ## $ best_pic_win     <fct> no, no, no, no, no, no, no, no, no, no, no, no, no, n~
    ## $ best_actor_win   <fct> no, no, no, yes, no, no, no, yes, no, no, yes, no, ye~
    ## $ best_actress_win <fct> no, no, no, no, no, no, no, no, no, no, no, no, yes, ~
    ## $ best_dir_win     <fct> no, no, no, yes, no, no, no, no, no, no, no, no, no, ~
    ## $ top200_box       <fct> no, no, no, no, no, no, no, no, no, no, yes, no, no, ~
    ## $ feature_film     <fct> yes, yes, yes, yes, yes, no, yes, yes, no, yes, yes, ~
    ## $ drama            <fct> yes, yes, no, yes, no, no, yes, yes, no, yes, no, yes~
    ## $ mpaa_rating_R    <fct> yes, no, yes, no, yes, no, no, yes, no, no, no, no, y~
    ## $ oscar_season     <fct> no, no, no, yes, no, no, no, yes, no, no, no, yes, no~
    ## $ summer_season    <fct> no, no, yes, no, no, no, no, no, no, no, yes, no, no,~

### Missing Values

Lets take care of missing values that we may have in our data.

``` r
design %>% tibble::rownames_to_column() %>% 
  filter( ! complete.cases(.)) %>% 
  select( rowname | which(colSums(is.na(.)) > 0) ) %>% kable() %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
rowname
</th>
<th style="text-align:right;">
runtime
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
334
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

There can be no movie without runtime. In fact, we can look up
information on the corresponding webpage and manually correct this
erroneous data entry.

``` r
design[334, "runtime"] = movies[334, "runtime"] <- 74
```

------------------------------------------------------------------------

## Part 3: Exploratory data analysis

Lets explore the relationship between `audience_score` and predictor
variables constructed in the previous part.

### Numerical Predictors

Check first the pairwise associations of numeric predictors with
response and among each other.

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

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/pairwise-numeric-1.png" width="90%" style="display: block; margin: auto;" />

The correlations are high between `imdb_rating`, `audience_score` and
`critics_score`. This is expected, since audience scores at IMDB and RT,
basically, represent similar information (movie’s popularity). However,
the individual correlations do not provide much information for
inference, since what we are interested in are *conditional
(in)dependencies* between variables (addressed by regression).

Upon inspection, the IMDB Rating totally dominates predictions of the
linear model, *whatever other variables are also included*. This can be
seen on the following plots, comparing `imdb_rating` as a sole predictor
with the full model fit:

``` r
model_imdb <- lm(audience_score ~ imdb_rating, data = design)
model_full <- lm(audience_score ~ ., data = design)
predicted_imdb <- ggplot(broom::augment(model_imdb), aes(x = .fitted, y = audience_score)) + 
  geom_point(shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Fitted Values", y = "Audience Score", title = "IMDB Rating Fit")
predicted_full <- ggplot(broom::augment(model_full), aes(x = .fitted, y = audience_score)) + 
  geom_point(shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Fitted Values", y = "Audience Score", title = "Full Model Fit")
imdb_full <- ggplot(data.frame(imdb_fit = model_imdb$fitted, full_fit = model_full$fitted),
                    aes(x = imdb_fit, y = full_fit)) + 
  geom_point(shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  annotate(
    "text", x = 44, y = 70,
    label = paste("Corr:", round(cor(model_imdb$fitted, model_full$fitted), digits = 3))
  ) +
  labs(x = "IMDB Fit", y = "Full Model Fit", title = "Full Model vs. IMDB Prediction")
grid.arrange(predicted_imdb, predicted_full, imdb_full, layout_matrix = rbind(c(1,2), c(3)) )
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/IMDB-vs-Full-1.png" width="90%" style="display: block; margin: auto;" />

The scatter of residuals around the null (diagonal line on the first two
plots) displays some non-constant variance and a clear trend, thus,
violating conditions of the linear regression. While the distribution of
`audiance_score` has a pronounced 2nd hump around 50, the shape of
`imdb_rating` is more-or-less unimodal, what partially can explain an
underestimate in that region. It turns out that the fit will be much
better (and let me add, “more interesting”), if one removes
`imdb_rating` from predictors.

``` r
design <- select(design, -imdb_rating)
```

This is in accord with the above said about the role of this redundant
variable as essentially encoding the same type of data as response (our
target of prediction). I think, it would be a little bit awkward to
“predict” the movie’s score, based on the score on the other website
(sampled from the same population, which is the “audience”). On the
other hand, I keep the `critics_score` as predictor, since it is not
uncommon for critics to rate movies differently than the audience, based
on other qualities or review criteria. That is to say, assuming these
scores are derived from the different population of “critics”, we can
ask how well they represent the audience opinion (e.g., we can see the
similar bi-modal shaped distribution).

### Linear Relationship and Variable Transformations

One of prerequisites to fit linear regression model is that predictors
are linearly associated with response variable, at least approximately.
However, some non-linear relationships are clearly seen on scatterplots,
that involve `imdb_num_votes` (and to some extent `runtime`). By
applying variable transformations (the common choices are `sqrt`, `log`,
`1/x`, etc.), one can make the association appear more linear. (Often
times this also correct the skewness of the variable’s distribution,
making it more “normal-like”, but this is not the goal per se.)

I find it most useful to apply log-transform to `imdb_num_votes` and a
reciprocal `1/runtime`. With the latter transform, however, the two
lowest `runtime` values become distinct outliers of the new more
symmetrical distribution. To verify this, we can perform the Rosner
statistical test
(![H_0: \\text{value is not an outlier}](https://latex.codecogs.com/png.latex?H_0%3A%20%5Ctext%7Bvalue%20is%20not%20an%20outlier%7D "H_0: \text{value is not an outlier}"),
with default significance level
![\\alpha = 0.05](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%200.05 "\alpha = 0.05")
and the number of suspect outliers
![k=3](https://latex.codecogs.com/png.latex?k%3D3 "k=3")):

``` r
EnvStats::rosnerTest( 1 / movies$runtime )$all.stats
```

    ##   i      Mean.i        SD.i       Value Obs.Num    R.i+1 lambda.i+1 Outlier
    ## 1 0 0.009745953 0.001746193 0.025641026     131 9.102699   3.931959    TRUE
    ## 2 1 0.009721499 0.001632175 0.025000000     574 9.360825   3.931563    TRUE
    ## 3 2 0.009697958 0.001518985 0.003745318     233 3.918828   3.931167   FALSE

``` r
movies %>% tibble::rownames_to_column() %>% 
  filter( ! between(runtime, 60, 210) ) %>% 
  arrange(runtime) %>% 
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
rowname
</th>
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
131
</td>
<td style="text-align:left;">
Africa: The Serengeti
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:left;">
Unrated
</td>
<td style="text-align:left;">
Houston Museum of Natural Scie
</td>
<td style="text-align:right;">
1994
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1998
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
7.3
</td>
<td style="text-align:right;">
535
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
74
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
George Casey
</td>
<td style="text-align:left;">
James Earl Jones
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
<http://www.imdb.com/title/tt0109049/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/imax_africa_the_serengeti/
</td>
</tr>
<tr>
<td style="text-align:left;">
574
</td>
<td style="text-align:left;">
Sea Monsters: A Prehistoric Adventure
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:left;">
National Geographic
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
7.0
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
68
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
Sean MacLeod Phillips
</td>
<td style="text-align:left;">
Liev Schreiber
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
<http://www.imdb.com/title/tt1027743/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/sea_monsters_a\_prehistoric_adventure/
</td>
</tr>
<tr>
<td style="text-align:left;">
233
</td>
<td style="text-align:left;">
Hotel Terminus: The Life and Times of Klaus Barbie
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:right;">
267
</td>
<td style="text-align:left;">
Unrated
</td>
<td style="text-align:left;">
SnagFilms
</td>
<td style="text-align:right;">
1988
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2010
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
872
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
73
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
Marcel Ophuls
</td>
<td style="text-align:left;">
Claude Lanzmann
</td>
<td style="text-align:left;">
Jeanne Moreau
</td>
<td style="text-align:left;">
Johannes Schneider-Merck
</td>
<td style="text-align:left;">
Klaus Barbie
</td>
<td style="text-align:left;">
Marcel Cruat
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0095341/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/hotel_terminus_the_life_and_times_of_klaus_barbie/
</td>
</tr>
</tbody>
</table>

</div>

I choose to remove 2 very short documentaries and do not consider titles
that last less than, say, 60 min. The corresponding distributions are
then nearly symmetrical (and close to normality, as seen from the
quantile-quantile plots below).

``` r
design <- filter(design, runtime > 60) %>% 
  mutate(
    log_votes = log10(imdb_num_votes),
    `1/runtime` = 1 / runtime,
    .keep = "unused"
  )
```

``` r
scatter_votes <- ggplot(design, aes(x = log_votes, y = audience_score, col = feature_film)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  theme(legend.position = "top") +
  labs(title = "Linear Dependence (?)", x = "Logarithm of Number of Votes on IMDB", y = "Audience Score")
histogram_votes <- ggplot(design, aes(x = log_votes)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(design$log_votes), sd = sd(design$log_votes))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate(
    "text", x = mean(design$log_votes), y = 0.02,
    label = paste("Skewness:", round(e1071::skewness(design$log_votes), digits = 3))
  ) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "Logarithm of Number of Votes on IMDB", y = "Probability Density")
qqnorm_votes <- ggplot(design, aes(sample = log_votes)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot: Log of Votes", x = "Theoretical Quantiles", y = "Sample Quantiles")
scatter_runtime <- ggplot(design, aes(x = `1/runtime`, y = audience_score, col = feature_film)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  theme(legend.position = "top") +
  labs(title = "Linear Dependence (?)", x = "1 / Runtime", y = "Audience Score")
histogram_runtime <- ggplot(design, aes(x = `1/runtime`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(design$`1/runtime`), sd = sd(design$`1/runtime`))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate(
    "text", x = mean(design$`1/runtime`), y = 10,
    label = paste("Skewness:", round(e1071::skewness(design$`1/runtime`), digits = 3))
  ) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "1 / Runtime", y = "Probability Density")
qqnorm_runtime <- ggplot(design, aes(sample = `1/runtime`)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot: 1 / Runtime", x = "Theoretical Quantiles", y = "Sample Quantiles")
grid.arrange(scatter_votes, histogram_votes, qqnorm_votes, 
             scatter_runtime, histogram_runtime, qqnorm_runtime, 
             ncol = 3)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/scores-distribution-1.png" width="90%" style="display: block; margin: auto;" />

The scatterplots against the response `audience_score` are much less
“wiggly” now, although still displaying some curved trend. Part of the
curvature in the lower range of `log_votes` can be accounted for by
conditioning on movie type, but for higher values the curve bends
upwards. Note that `feature_film` == “no” category consists mostly of
Documentaries, with only 5 TV Movies. Given the mutual characteristics
of the above types, it could be more natural to aggregate “TV Movie” and
“Feature Film” `title_type` together (rather than suggested
`feature_film`).

### Categorical Predictors

Compute summary statistics of respective counts:

``` r
summary( select_if(design, is.factor) )
```

    ##  best_pic_nom best_pic_win best_actor_win best_actress_win best_dir_win
    ##  no :627      no :642      no :556        no :577          no :606     
    ##  yes: 22      yes:  7      yes: 93        yes: 72          yes: 43     
    ##  top200_box feature_film drama     mpaa_rating_R oscar_season summer_season
    ##  no :634    no : 58      no :344   no :320       no :459      no :441      
    ##  yes: 15    yes:591      yes:305   yes:329       yes:190      yes:208

Note that some classes are imbalanced. Also `best_pic_nom` is likely to
include `best_pic_win` as subset, with only 1 exception (erroneously,
“won without nomination”):

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

We can also plot our numeric response against each of the individual
categorical predictors, using side-by-side boxplots.

``` r
design %>% 
  select_if(is.factor) %>% 
  cbind(score = design$audience_score) %>% 
  pivot_longer(1:11, names_to = "predictor", values_to = "category") %>% 
  ggplot(aes(x = score, y = category)) +
  geom_boxplot() +
  facet_wrap( ~ predictor, ncol = 4, scales = "free") +
  labs(x = "Audience Score", y = NULL, title = "Categorical Predictors for Movie Score") +
  theme(axis.text = element_text(size = 6))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/categorical-predictors-1.png" width="90%" style="display: block; margin: auto;" />

From the median values and ranges overlap, we can suspect
`feature_film`, `best_pic_nom` and possibly `top200_box` as potentially
reliable predictors (without any additional information).

------------------------------------------------------------------------

## Part 4: Modeling

### Full Model Specification and Posteriors

For Bayesian model choice, we start with the full model, which includes
all the predictors. Using `BAS` package, we will fit the multiple linear
regression of the form:

![y_i = \\beta_0 + \\pmb{\\beta}^T(\\mathbf{x}\_i - \\bar{\\mathbf{x}}) + \\epsilon_i](https://latex.codecogs.com/png.latex?y_i%20%3D%20%5Cbeta_0%20%2B%20%5Cpmb%7B%5Cbeta%7D%5ET%28%5Cmathbf%7Bx%7D_i%20-%20%5Cbar%7B%5Cmathbf%7Bx%7D%7D%29%20%2B%20%5Cepsilon_i "y_i = \beta_0 + \pmb{\beta}^T(\mathbf{x}_i - \bar{\mathbf{x}}) + \epsilon_i")

Here boldface corresponds to each predictor and its coefficients
![\\beta_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j"),
index ![i](https://latex.codecogs.com/png.latex?i "i") denotes
observations, and
![\\bar{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cbar%7B%5Cmathbf%7Bx%7D%7D "\bar{\mathbf{x}}")
is a vector of sample means. The estimate of the constant coefficient
![\\hat{\\beta}\_0 = \\bar{\\mathbf{y}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cbeta%7D_0%20%3D%20%5Cbar%7B%5Cmathbf%7By%7D%7D "\hat{\beta}_0 = \bar{\mathbf{y}}")
is, thus, the sample mean of the response variable. One assumes that
![\\epsilon_i](https://latex.codecogs.com/png.latex?%5Cepsilon_i "\epsilon_i")
is independent, and identically distributed with the Normal distribution
![\\epsilon_i\\stackrel{\\mathrm{i.i.d.}}{\\sim}\\mathrm{Normal}(0,\\sigma^2)](https://latex.codecogs.com/png.latex?%5Cepsilon_i%5Cstackrel%7B%5Cmathrm%7Bi.i.d.%7D%7D%7B%5Csim%7D%5Cmathrm%7BNormal%7D%280%2C%5Csigma%5E2%29 "\epsilon_i\stackrel{\mathrm{i.i.d.}}{\sim}\mathrm{Normal}(0,\sigma^2)").

For Bayesian inference, we need to specify a prior distributions for all
the coefficients
![\\beta_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j")
(and other hyper-parameters), that reflect our uncertainty about the
importance of variables. This elicitation can be quite involved,
especially in our case, when we do not have enough prior information
about movies, apart from the data sample itself. Therefore, we are going
to adopt some vague non-informative prior, e.g., *reference prior*,
which is a limiting case of the multivariate Normal-Gamma prior
distribution.

``` r
full.lm <- bas.lm(audience_score ~ ., data = design, 
                  # as n gets large, log-likelihood is approximated by BIC, for nearly flat reference prior
                  prior = "BIC",
                  # include all variables of the full model, having prior model probability exactly one
                  modelprior = Bernoulli(1), include.always = ~ ., n.models = 1)
full.coef <- coef(full.lm)
```

For a single model fit, one updates the coefficients based on the data,
resulting in *posterior distributions* for each of them (plus
intercept):

``` r
par(mfrow = c(4, 4), col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey")
plot(full.coef, ask = FALSE)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" style="display: block; margin: auto;" />

We see that several 95% credible intervals of coefficients (coinciding
with OLS confidence intervals, when using reference prior) contain zero,
suggesting that a sparse model without those variables may be more
accurate.

``` r
round(confint(full.coef, parm = 2:full.coef$n.vars), 3)
```

    ##                         2.5%   97.5%    beta
    ## critics_score          0.372   0.460   0.416
    ## thtr_rel_year         -0.294  -0.080  -0.187
    ## best_pic_nomyes        0.410  14.363   7.386
    ## best_pic_winyes      -16.213   8.122  -4.045
    ## best_actor_winyes     -5.442   0.884  -2.279
    ## best_actress_winyes   -5.899   1.089  -2.405
    ## best_dir_winyes       -6.556   2.708  -1.924
    ## top200_boxyes         -8.745   5.863  -1.441
    ## feature_filmyes      -22.543 -12.712 -17.627
    ## dramayes               1.595   6.333   3.964
    ## mpaa_rating_Ryes      -2.402   1.936  -0.233
    ## oscar_seasonyes       -3.614   1.718  -0.948
    ## summer_seasonyes      -4.114   0.979  -1.567
    ## log_votes              5.613   9.436   7.525
    ## `1/runtime`         -759.958 876.666  58.354
    ## attr(,"Probability")
    ## [1] 0.95
    ## attr(,"class")
    ## [1] "confint.bas"

Overall, for ![p=16](https://latex.codecogs.com/png.latex?p%3D16 "p=16")
variables we have
![2^p=65536](https://latex.codecogs.com/png.latex?2%5Ep%3D65536 "2^p=65536")
possible subset models to consider.

### Model Uncertainty and Averaging

Going one step higher, our uncertainty about model choice can be
represented using probability distribution over all possible models
![M_j](https://latex.codecogs.com/png.latex?M_j "M_j"), where each
probability
![p(M_j)](https://latex.codecogs.com/png.latex?p%28M_j%29 "p(M_j)")
provides measure of how likely is the model. We can then make inference
and compute weighted averages of quantities of interest, using updated
posterior probabilities:

![\\hat{Y}^\\ast = \\sum\_{j=1}^{2^p}\\hat{Y}^\\ast_j \\, p(M_j\|\\mathrm{data}), \\qquad\\qquad p(\\Delta\|\\mathrm{data}) = \\sum\_{j=1}^{2^p}p(\\Delta\|M_j, \\mathrm{data}) \\, p(M_j\|\\mathrm{data}), \\qquad\\qquad \\mbox{etc.}](https://latex.codecogs.com/png.latex?%5Chat%7BY%7D%5E%5Cast%20%3D%20%5Csum_%7Bj%3D1%7D%5E%7B2%5Ep%7D%5Chat%7BY%7D%5E%5Cast_j%20%5C%2C%20p%28M_j%7C%5Cmathrm%7Bdata%7D%29%2C%20%5Cqquad%5Cqquad%20p%28%5CDelta%7C%5Cmathrm%7Bdata%7D%29%20%3D%20%5Csum_%7Bj%3D1%7D%5E%7B2%5Ep%7Dp%28%5CDelta%7CM_j%2C%20%5Cmathrm%7Bdata%7D%29%20%5C%2C%20p%28M_j%7C%5Cmathrm%7Bdata%7D%29%2C%20%5Cqquad%5Cqquad%20%5Cmbox%7Betc.%7D "\hat{Y}^\ast = \sum_{j=1}^{2^p}\hat{Y}^\ast_j \, p(M_j|\mathrm{data}), \qquad\qquad p(\Delta|\mathrm{data}) = \sum_{j=1}^{2^p}p(\Delta|M_j, \mathrm{data}) \, p(M_j|\mathrm{data}), \qquad\qquad \mbox{etc.}")

This Bayesian Model Averaging (BMA) represents the full posterior
uncertainty after seeing the data. For instance, using uniform prior,
that treats all models as equally likely (and reference prior on
coefficients), the 4 most likely models are as follows:

``` r
movies.BIC <- bas.lm(audience_score ~ ., data = design,
                     prior = "BIC", modelprior = uniform(), 
                     method = "MCMC", MCMC.iterations = 10 ^ 7)
round(summary(movies.BIC, n.models = 4), 3)
```

    ##                     P(B != 0 | Y)   model 1   model 2   model 3   model 4
    ## Intercept                   1.000     1.000     1.000     1.000     1.000
    ## critics_score               1.000     1.000     1.000     1.000     1.000
<<<<<<< HEAD
    ## thtr_rel_year               0.871     1.000     0.000     1.000     1.000
    ## best_pic_nomyes             0.140     0.000     0.000     1.000     0.000
    ## best_pic_winyes             0.042     0.000     0.000     0.000     0.000
    ## best_actor_winyes           0.091     0.000     0.000     0.000     1.000
    ## best_actress_winyes         0.086     0.000     0.000     0.000     0.000
    ## best_dir_winyes             0.066     0.000     0.000     0.000     0.000
    ## top200_boxyes               0.042     0.000     0.000     0.000     0.000
    ## feature_filmyes             1.000     1.000     1.000     1.000     1.000
    ## dramayes                    0.897     1.000     1.000     1.000     1.000
    ## mpaa_rating_Ryes            0.039     0.000     0.000     0.000     0.000
=======
    ## thtr_rel_year               0.870     1.000     0.000     1.000     1.000
    ## best_pic_nomyes             0.140     0.000     0.000     1.000     0.000
    ## best_pic_winyes             0.041     0.000     0.000     0.000     0.000
    ## best_actor_winyes           0.089     0.000     0.000     0.000     1.000
    ## best_actress_winyes         0.086     0.000     0.000     0.000     0.000
    ## best_dir_winyes             0.065     0.000     0.000     0.000     0.000
    ## top200_boxyes               0.042     0.000     0.000     0.000     0.000
    ## feature_filmyes             1.000     1.000     1.000     1.000     1.000
    ## dramayes                    0.897     1.000     1.000     1.000     1.000
    ## mpaa_rating_Ryes            0.038     0.000     0.000     0.000     0.000
>>>>>>> e52aa905ef579c3e829c745f73239c19e671d57c
    ## oscar_seasonyes             0.039     0.000     0.000     0.000     0.000
    ## summer_seasonyes            0.065     0.000     0.000     0.000     0.000
    ## log_votes                   1.000     1.000     1.000     1.000     1.000
    ## `1/runtime`                 0.044     0.000     0.000     0.000     0.000
    ## BF                             NA     1.000     0.133     0.132     0.102
<<<<<<< HEAD
    ## PostProbs                      NA     0.404     0.054     0.054     0.042
=======
    ## PostProbs                      NA     0.407     0.055     0.054     0.041
>>>>>>> e52aa905ef579c3e829c745f73239c19e671d57c
    ## R2                             NA     0.552     0.545     0.554     0.554
    ## dim                            NA     6.000     5.000     7.000     7.000
    ## logmarg                        NA -3811.616 -3813.631 -3813.644 -3813.897

The second column contains posterior inclusion probabilities
![p(\\beta_j\\neq0)](https://latex.codecogs.com/png.latex?p%28%5Cbeta_j%5Cneq0%29 "p(\beta_j\neq0)"),
that represent importance of different predictors, as can be seen on the
following graph:

``` r
plot(movies.BIC, which = 4, ask = FALSE, caption = "", sub.caption = "", 
     col.in = "blue", col.ex = "darkgrey", lwd = 3, cex = 0.55, 
     main = "Importance of Coefficients Under BMA")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" style="display: block; margin: auto;" />

PIPs turn out to be rather similar for the whole range of common
coefficient priors (BIC, g-prior, JZS, hyper-g-n, EB-local), except for
AIC, that invariably results in much higher estimates.

``` r
# Find posterior inclusion probabilities for a range of priors (with adjusted hyper-parameters)
pips <- mapply(
  FUN = function(p, a) {
    model <- bas.lm(audience_score ~ ., data = design, prior = p, alpha = a, modelprior = uniform())
    return( model$probne0 )
  },
  p = list(BIC = "BIC", g = "g-prior",  ZS = "JZS", HG = "hyper-g-n", EB = "EB-local", AIC = "AIC"), 
  a = list(NULL, nrow(design), NULL, 3, nrow(design), NULL)
)
rownames(pips) <- movies.BIC$namesx
# Plot the calculated pips
as.data.frame(pips) %>% 
  tibble::rownames_to_column(var = "predictor") %>% 
  pivot_longer(2:7, names_to = "prior", values_to = "pip") %>% 
  ggplot(aes(x = reorder(prior, pip), y = pip)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  facet_wrap( ~ factor(predictor, levels = movies.BIC$namesx), ncol = 4, scales = "free") +
  labs(x = "Priors", y = NULL, title = "Posterior Inclusion Probabilities Using Different Priors")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

**I choose to continue with the BIC prior** as a fairly conservative
approach. Using model `PostProbs` from the summary above to calculate
Bayes factors, one can compare relative plausibility of different
models. The resulting model space and content is visualized graphically,
where each color corresponds to the log posterior odds (over the null
model):

``` r
image(movies.BIC, cex.axis = 0.8, rotate = FALSE)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" style="display: block; margin: auto;" />

Since we have different models to choose from under the Bayesian
framework, we need to first specify which particular model we use to
obtain the prediction. Under the BMA scheme, we have a hierarchical
model, composed of many simpler models as building blocks. The posterior
predictive mean by using the weighted average is the best w.r.t. the
squared error loss, but the BMA model itself is not easy to interpret.

How to select a single model from the posterior distribution and use it
for future inference? The common choices are the Highest Probability
Model (optimal w.r.t. 0-1 loss), Median Probability Model (pips \> 0.5),
and the Best Predictive Model (whose predictions are closest to BMA). In
our case, all three options result in a single best model (under the BIC
prior), for instance:

``` r
coef.HPM <- coef(movies.BIC, estimator = "HPM")
round(confint(coef.HPM), 3)
```

    ##                        2.5%   97.5%    beta
    ## Intercept            61.288  63.384  62.336
    ## critics_score         0.374   0.459   0.417
    ## thtr_rel_year        -0.275  -0.067  -0.171
    ## best_pic_nomyes       0.000   0.000   0.000
    ## best_pic_winyes       0.000   0.000   0.000
    ## best_actor_winyes     0.000   0.000   0.000
    ## best_actress_winyes   0.000   0.000   0.000
    ## best_dir_winyes       0.000   0.000   0.000
    ## top200_boxyes         0.000   0.000   0.000
    ## feature_filmyes     -22.723 -13.025 -17.874
    ## dramayes              1.582   6.113   3.848
    ## mpaa_rating_Ryes      0.000   0.000   0.000
    ## oscar_seasonyes       0.000   0.000   0.000
    ## summer_seasonyes      0.000   0.000   0.000
    ## log_votes             5.440   8.930   7.185
    ## `1/runtime`           0.000   0.000   0.000
    ## attr(,"Probability")
    ## [1] 0.95
    ## attr(,"class")
    ## [1] "confint.bas"

The posterior probability of the model (against the prior
![1/2^p](https://latex.codecogs.com/png.latex?1%2F2%5Ep "1/2^p")):

``` r
hpm <- which.max(movies.BIC$postprobs)
round(movies.BIC$postprobs[[hpm]], 3)
```

<<<<<<< HEAD
    ## [1] 0.405
=======
    ## [1] 0.407
>>>>>>> e52aa905ef579c3e829c745f73239c19e671d57c

### Model Interpretation and Diagnostics

Recall that the `BAS` fits the linear model with all predictors centered
at their sample average values:

``` r
summarize(design,
  avg_feature_film.yes = mean(feature_film == "yes"),
  avg_drama.yes = mean(drama == "yes"),
  avg_critics_score = mean(critics_score),
  avg_thtr_rel_year = mean(thtr_rel_year),
  avg_log_votes = mean(log_votes)
) %>% mutate(
  `10^avg_log_votes` = 10 ^ avg_log_votes
) %>% kable(digits = 2) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
avg_feature_film.yes
</th>
<th style="text-align:right;">
avg_drama.yes
</th>
<th style="text-align:right;">
avg_critics_score
</th>
<th style="text-align:right;">
avg_thtr_rel_year
</th>
<th style="text-align:right;">
avg_log_votes
</th>
<th style="text-align:right;">
10^avg_log_votes
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
57.56
</td>
<td style="text-align:right;">
1997.93
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
16568.83
</td>
</tr>
</tbody>
</table>

The selected model then has the following form:

![\\widehat{\\mathtt{audience\\\_score}} =  62.3 - 17.9 \\times (\\mathtt{feature\\\_film}\_\\mathrm{yes}^{} - 0.91) + 3.8 \\times (\\mathtt{drama}\_\\mathrm{yes}^{} - 0.47) + \\\\  0.42 \\times (\\mathtt{critics\\\_score} - 57.6) - 0.17 \\times (\\mathtt{thtr\\\_rel\\\_year} - 1998) + 7.2 \\times \\log\_{10} \\left(\\frac{\\mathtt{imdb\\\_num\\\_votes}}{16568}\\right)](https://latex.codecogs.com/png.latex?%5Cwidehat%7B%5Cmathtt%7Baudience%5C_score%7D%7D%20%3D%20%2062.3%20-%2017.9%20%5Ctimes%20%28%5Cmathtt%7Bfeature%5C_film%7D_%5Cmathrm%7Byes%7D%5E%7B%7D%20-%200.91%29%20%2B%203.8%20%5Ctimes%20%28%5Cmathtt%7Bdrama%7D_%5Cmathrm%7Byes%7D%5E%7B%7D%20-%200.47%29%20%2B%20%5C%5C%20%200.42%20%5Ctimes%20%28%5Cmathtt%7Bcritics%5C_score%7D%20-%2057.6%29%20-%200.17%20%5Ctimes%20%28%5Cmathtt%7Bthtr%5C_rel%5C_year%7D%20-%201998%29%20%2B%207.2%20%5Ctimes%20%5Clog_%7B10%7D%20%5Cleft%28%5Cfrac%7B%5Cmathtt%7Bimdb%5C_num%5C_votes%7D%7D%7B16568%7D%5Cright%29 "\widehat{\mathtt{audience\_score}} =  62.3 - 17.9 \times (\mathtt{feature\_film}_\mathrm{yes}^{} - 0.91) + 3.8 \times (\mathtt{drama}_\mathrm{yes}^{} - 0.47) + \\  0.42 \times (\mathtt{critics\_score} - 57.6) - 0.17 \times (\mathtt{thtr\_rel\_year} - 1998) + 7.2 \times \log_{10} \left(\frac{\mathtt{imdb\_num\_votes}}{16568}\right)")

The 95% credible intervals of coefficient estimates coincide with the
frequentist confidence intervals, when using reference prior (BIC
approach). The only difference is the **interpretation**. For example,
based on the data, we believe that there is a 95% chance that the
audience score on Rotter Tomatoes will increase *on average* by 3.7 up
to 4.6 for every additional 10 points of critics’ score, or that it will
be higher from 0.7 to 2.7 percent points for every 10 years passed since
release in theaters, *all else being equal*. Further, it is 95% likely
that the average movie score will be higher by 5.4 up to 8.9 points for
every *10 times* increase in number of votes (using the property
![\\log(xy)=\\log(x) + \\log(y)](https://latex.codecogs.com/png.latex?%5Clog%28xy%29%3D%5Clog%28x%29%20%2B%20%5Clog%28y%29 "\log(xy)=\log(x) + \log(y)")),
if everything else is held fixed. Finally, we expect with the
probability of 95% that the average movie score will be about 17.9 lower
(from 13 to 22.7) if it is a Feature Film, and approximately 3.8 points
higher (between 1.6 and 6.1) if it is Drama, respectively.

Before we perform the necessary **diagnostics** of our model, lets first
check that the MCMC sampler method for BMA has properly converged.

``` r
par(mfrow = c(1,2))
diagnostics(movies.BIC, type = c("pip", "model"), col = "blue", pch = 16, cex = 1.5)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" style="display: block; margin: auto;" />

The application of multiple linear regression methods generally depends
on the set of conditions, that we can verify by analyzing residuals. To
construct diagnostic plots, lets first augment our data matrix with
predictions of the HPM model and corresponding residuals. (The usually
helpful function `broom::augment()` is not available for `bas` object.)

``` r
movies.HPM <- predict(movies.BIC, estimator = "HPM")
movies.BMA <- predict(movies.BIC, estimator = "BMA")
design_aug <- mutate(design,
  bma.fit = movies.BMA$fit,  # for comparison with HPM
  hpm.fit = movies.HPM$fit,
  hpm.resid = audience_score - hpm.fit
)
```

``` r
resid_hist <- ggplot(design_aug, aes(x = hpm.resid)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(
    aes(col = "Normal"), size = 1,
    fun = dnorm, args = list(mean = mean(design_aug$hpm.resid), sd = sd(design_aug$hpm.resid))
  ) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate(
    "text", x = mean(design_aug$hpm.resid), y = 0.001,
    label = paste("Kurtosis:", round(e1071::kurtosis(design_aug$hpm.resid), digits = 3))
  ) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "Residuals", y = "Probability Density")
resid_qqnorm <- ggplot(design_aug, aes(sample = hpm.resid)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot for Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
resid_fitted <- ggplot(design_aug, aes(x = hpm.fit, y = abs(hpm.resid))) +
  geom_point(shape = 16, size = 2, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Absolute Values of Residuals")
resid_order <- ggplot(design_aug, aes(x = 1:length(hpm.resid), y = hpm.resid)) +
  geom_point(shape = 21, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Independence", x = "Order of Collection", y = "Residuals")
grid.arrange(resid_hist, resid_qqnorm, resid_fitted, resid_order, ncol = 2, nrow = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-23-1.png" width="90%" style="display: block; margin: auto;" />

1.  The *residuals are nearly normal* around 0. They are slightly
    over-dispersed with thick tails (positive excess kurtosis,
    corresponding to flipped S-shape on the QQ-plot), but deviations
    from normality are negligibly small (especially for large data sets
    like ours,
    ![n=649](https://latex.codecogs.com/png.latex?n%3D649 "n=649")).
2.  The *variability of the residuals is somewhat problematic*
    (heteroscedasticity), as can be seen from the absolute values of
    residuals, plotted against fitted values. The scatter is a little
    bit higher for lower values, predicted by the model.We will return
    to this issue shortly.
3.  The apparent randomness of residuals w.r.t. order of collection
    implies that they are *independent*, coming from a random sample.
4.  Each variable should be *linearly related* to the outcome, as we
    already discussed in the EDA section on the variable transformation.
    We can additionally check linearity assumption, by plotting model
    residuals against each numeric predictor (even `runtime`, for
    completeness).

``` r
resid_critics <- ggplot(design_aug, aes(x = critics_score, y = hpm.resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Critics Score", y = NULL)
resid_year <- ggplot(design_aug, aes(x = thtr_rel_year, y = hpm.resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Year of Release in Theater", y = NULL)
resid_votes <- ggplot(design_aug, aes(x = log_votes, y = hpm.resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Logarithm of Number of Votes on IMDB", y = NULL)
resid_runtime <- ggplot(design_aug, aes(x =`1/runtime`, y = hpm.resid)) + 
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "1 / Runtime", y = NULL)
grid.arrange(
  resid_critics, resid_votes, resid_year, resid_runtime, ncol = 2,
  top = grid::textGrob(
    "Linearity Assumption: Residuals vs. Predictors", gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-24-1.png" width="90%" style="display: block; margin: auto;" />

Finally, in order to visually assess the accuracy of predictions, we
plot the actual IMDB scores against fitted values (with diagonal and
local regression lines on top, to see any patterns). One can
additionally compare predictions of our best model (HPM = MPM = BPM) and
hierarchical BMA model to see that they are in perfect agreement.

``` r
actual_predict <- ggplot(design_aug, aes(x = hpm.fit, y = audience_score)) + 
  geom_point(aes(color = abs(audience_score - hpm.fit) < 25), shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  theme(legend.position = "top") + 
  labs(x = "HPM Fitted Values", y = "Audience Score", title = "Actual vs. Predicted")
BMA_HPM <- ggplot(design_aug, aes(x = hpm.fit, y = bma.fit)) + 
  geom_point(shape = 16, size = 2, alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess") +
  annotate(
    "text", x = 55, y = 75,
    label = paste("Corr:", round(cor(design_aug$bma.fit, design_aug$hpm.fit), digits = 3))
  ) +
  labs(x = "HPM Fit", y = "BMA Fit", title = "BMA vs. HPM Prediction")
grid.arrange(actual_predict, BMA_HPM, ncol = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project4_bayesian_files/figure-gfm/unnamed-chunk-25-1.png" width="90%" style="display: block; margin: auto;" />

------------------------------------------------------------------------

## Part 5: Prediction

Usually, the good practice is to split data into “training” and “test”
subsets, and to use the second one for validation of the accuracy of
predictions on the data, not used for the model fitting. Instead, the
computation of BIC imposes some penalty on the number of variables to
compensate for possible over-fitting (another way is cross-validation).
Lets check how our model performs on some out-of-sample observations. We
pick a movie from 2016 (and a handful of others not in the original
dataset), with information gathered from [Rotten
Tomatoes](https://www.rottentomatoes.com/) and
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
<th style="text-align:right;">
thtr_rel_month
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
best_dir_win
</th>
<th style="text-align:left;">
best_actor_win
</th>
<th style="text-align:left;">
best_actress_win
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
log_votes
</th>
<th style="text-align:right;">
1/runtime
</th>
<th style="text-align:left;">
feature_film
</th>
<th style="text-align:left;">
drama
</th>
<th style="text-align:left;">
mpaa_rating_R
</th>
<th style="text-align:left;">
oscar_season
</th>
<th style="text-align:left;">
summer_season
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
The Conjuring 2
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Horror
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2016
</td>
<td style="text-align:right;">
6
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
<https://www.imdb.com/title/tt3065204/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/the_conjuring_2>
</td>
<td style="text-align:right;">
5.394117
</td>
<td style="text-align:right;">
0.0074627
</td>
<td style="text-align:left;">
yes
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
yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Pleasantville
</td>
<td style="text-align:left;">
Feature Film
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
<td style="text-align:right;">
10
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
<https://www.imdb.com/title/tt0120789/?ref_=ttspec_ql>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/pleasantville>
</td>
<td style="text-align:right;">
5.096900
</td>
<td style="text-align:right;">
0.0080645
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
</tr>
<tr>
<td style="text-align:left;">
Forrest Gump
</td>
<td style="text-align:left;">
Feature Film
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
<td style="text-align:right;">
7
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
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
no
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
6.279713
</td>
<td style="text-align:right;">
0.0070423
</td>
<td style="text-align:left;">
yes
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
</tr>
<tr>
<td style="text-align:left;">
Ferris Bueller’s Day Off
</td>
<td style="text-align:left;">
Feature Film
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
<td style="text-align:right;">
6
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
<https://www.imdb.com/title/tt0091042/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/ferris_buellers_day_off>
</td>
<td style="text-align:right;">
5.525537
</td>
<td style="text-align:right;">
0.0097087
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
no
</td>
<td style="text-align:left;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Crank
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:right;">
9
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
<https://www.imdb.com/title/tt0479884/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/crank>
</td>
<td style="text-align:right;">
5.382741
</td>
<td style="text-align:right;">
0.0120482
</td>
<td style="text-align:left;">
yes
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
</tr>
<tr>
<td style="text-align:left;">
Prince of Darkness
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Horror
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
10
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
yes
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
4.602418
</td>
<td style="text-align:right;">
0.0098039
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
no
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
R
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:right;">
10
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
<https://www.imdb.com/title/tt0815241/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/religulous>
</td>
<td style="text-align:right;">
4.767430
</td>
<td style="text-align:right;">
0.0099010
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
yes
</td>
<td style="text-align:left;">
no
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
R
</td>
<td style="text-align:right;">
2005
</td>
<td style="text-align:right;">
6
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
<https://www.imdb.com/title/tt0478209/?ref_=nv_sr_srsg_0>
</td>
<td style="text-align:left;">
<https://www.rottentomatoes.com/m/metal_a_headbangers_journey>
</td>
<td style="text-align:right;">
4.077368
</td>
<td style="text-align:right;">
0.0104167
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
yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Catwoman
</td>
<td style="text-align:left;">
Feature Film
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
<td style="text-align:right;">
7
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
yes
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
5.060116
</td>
<td style="text-align:right;">
0.0096154
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
no
</td>
<td style="text-align:left;">
yes
</td>
</tr>
</tbody>
</table>

</div>

Using developed model, perform a prediction of `audience_score` with the
corresponding intervals.

``` r
pred.HPM <- predict(movies.BIC, newdata = new_data, estimator = "HPM",
                    se.fit = TRUE, nsim = 10000)
# Show the combined results, comparing actual and predicted values
as.data.frame( cbind( confint(pred.HPM, parm = "pred") ) ) %>% round(0) %>% 
  mutate(
    title = new_data$title,
    audience_score = new_data$audience_score,
    .before = 1
  ) %>% mutate(
    `lwr < actual < upr` =  `2.5%` < audience_score & audience_score < `97.5%`
  ) %>%
  kable() %>% kable_styling("striped") 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:right;">
audience_score
</th>
<th style="text-align:right;">
2.5%
</th>
<th style="text-align:right;">
97.5%
</th>
<th style="text-align:right;">
pred
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
81
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
74
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
79
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
77
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
95
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
84
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
92
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
80
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
71
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
67
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
60
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
64
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
78
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
84
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
91
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
115
</td>
<td style="text-align:right;">
88
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
18
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
</tbody>
</table>

Although new data is not randomly sampled, I tried different movies in
terms of score, genre, time period, etc. The last columns display lower
and upper boundaries of prediction interval, as well whether it managed
to capture the true value of `audience_score` in each case. *What is
Bayesian prediction interval for actual movie scores, and how does it
differ from credible intervals, encountered earlier?*

Assuming that the data is generated from the population distribution:

![\\text{score}\_i \\sim \\mathrm{Normal}(\\mu_i,\\sigma)](https://latex.codecogs.com/png.latex?%5Ctext%7Bscore%7D_i%20%5Csim%20%5Cmathrm%7BNormal%7D%28%5Cmu_i%2C%5Csigma%29 "\text{score}_i \sim \mathrm{Normal}(\mu_i,\sigma)")

where ![\\mu_i](https://latex.codecogs.com/png.latex?%5Cmu_i "\mu_i") is
given by our linear regression formula, there are two types of
uncertainties. The first one, surrounding average values over a large
number of movies, is related to our ignorance about the exact details of
the model. For the linear regression, using certain set of predictor
variables, it is embodied in posterior probability distribution of the
coefficients
![\\beta_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j"),
quantifying our knowledge of their “true” values (e.g., using credible
intervals).

However, the Gaussian model of scores expects observed heights to be
distributed around
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu"), not right on
top of it, with the spread governed by
![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\sigma"). For
every unique combination of predictors’ values
![x](https://latex.codecogs.com/png.latex?x "x") of interest (such as
certain `thtr_rel_year`, `critics_score`, etc.), one samples from the
Normal distribution with the correct values of
![\\mu_i(x), \\sigma](https://latex.codecogs.com/png.latex?%5Cmu_i%28x%29%2C%20%5Csigma "\mu_i(x), \sigma"),
which are themselves sampled from the posterior. The prediction interval
incorporates both the posterior uncertainty of the model, *as well as*
variability related to the data generating process. It thus quantifies
how much an individual point will differ from the population regression
plane, and it is always wider than the credible interval.

It seems our model performs not so bad, but the wide intervals signify
about large uncertainty of our very simple model. There is a plenty room
for improvement!

------------------------------------------------------------------------

## Part 6: Conclusion

We have performed the Bayesian regression, fitting a linear model with
the given set of features to predict the audience score of the movies on
Rotten Tomatoes. However, I have forcefully excluded IMDB movie ratings
from consideration, since they represent redundant information with the
response, and thus un-interesting for the prediction task. It also turns
out that the model works better without `imdb_rating`.

I have performed the model selection, using `BAS` package, where models
can be ranked and weighted according to posterior probability
distribution using data, that is, in an essentially Bayesian way. After
trying several different priors (vague, non-informative) for linear
regression coefficients, the single best model is found under the flat
reference prior (BIC), according to Highest Probability (optimal w.r.t.
![L_0](https://latex.codecogs.com/png.latex?L_0 "L_0")), Median
Probability (all
![p(\\beta_j) > 0.5](https://latex.codecogs.com/png.latex?p%28%5Cbeta_j%29%20%3E%200.5 "p(\beta_j) > 0.5"))
and Best Predictive Model criteria. I have verified that it indeed gives
the same predictions as the hierarchical Bayesian Model Averaging scheme
(optimal w.r.t. ![L_2](https://latex.codecogs.com/png.latex?L_2 "L_2")).

Unfortunately, the model is likely too much simplistic and requires
further improvements. This can be seen in the somewhat larger
variability of model predictions in the middle and lower range of fitted
values. It is always good to look at the problematic observations
(highlighted in red on the `Actual vs. Predicted` scatterplot), where
the model does not perform well. There are underestimates:

``` r
# Underestimates
filter(movies, runtime > 60)[design_aug$audience_score - design_aug$hpm.fit > 25, ] %>% 
  arrange(audience_score) %>% 
  select(-c(runtime, studio, thtr_rel_month:dvd_rel_day, best_pic_nom:actor5)) %>% 
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%", height = "400px")
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
Woo
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
1998
</td>
<td style="text-align:right;">
3.8
</td>
<td style="text-align:right;">
1510
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0120531/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/woo/
</td>
</tr>
<tr>
<td style="text-align:left;">
Dirty Love
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
2005
</td>
<td style="text-align:right;">
3.5
</td>
<td style="text-align:right;">
6054
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0327643/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/dirty_love/
</td>
</tr>
<tr>
<td style="text-align:left;">
A Night at the Roxbury
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
1998
</td>
<td style="text-align:right;">
6.2
</td>
<td style="text-align:right;">
47343
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0120770/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/night_at_the_roxbury/
</td>
</tr>
<tr>
<td style="text-align:left;">
Imagining Argentina
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
2004
</td>
<td style="text-align:right;">
6.3
</td>
<td style="text-align:right;">
2732
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0314197/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/imagining_argentina/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Jazz Singer
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Musical & Performing Arts
</td>
<td style="text-align:left;">
PG
</td>
<td style="text-align:right;">
1980
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
2897
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0080948/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1010979-jazz_singer/
</td>
</tr>
<tr>
<td style="text-align:left;">
See No Evil, Hear No Evil
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1989
</td>
<td style="text-align:right;">
6.8
</td>
<td style="text-align:right;">
34802
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0098282/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/see_no_evil_hear_no_evil/
</td>
</tr>
<tr>
<td style="text-align:left;">
Filly Brown
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
2013
</td>
<td style="text-align:right;">
5.5
</td>
<td style="text-align:right;">
899
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1869425/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/filly_brown_2012/
</td>
</tr>
<tr>
<td style="text-align:left;">
Waist Deep
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
2006
</td>
<td style="text-align:right;">
5.9
</td>
<td style="text-align:right;">
8059
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0456020/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/waist_deep/
</td>
</tr>
<tr>
<td style="text-align:left;">
One Night With the King
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
2006
</td>
<td style="text-align:right;">
6.3
</td>
<td style="text-align:right;">
5704
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0430431/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/one_night_with_the_king/
</td>
</tr>
<tr>
<td style="text-align:left;">
Modigliani
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
2004
</td>
<td style="text-align:right;">
7.4
</td>
<td style="text-align:right;">
8030
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0367188/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/modigliani/
</td>
</tr>
<tr>
<td style="text-align:left;">
Half Baked
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
1998
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
46794
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
81
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0120693/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/half_baked/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Ritz
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
1976
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
1663
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0075144/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/ritz/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Football Factory
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Documentary
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2004
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
23201
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0385705/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1153907-football_factory/
</td>
</tr>
<tr>
<td style="text-align:left;">
Rise of the Footsoldier
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
15806
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0901507/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/rise-of-the-footsoldier/
</td>
</tr>
<tr>
<td style="text-align:left;">
Same Time, Next Year
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
1978
</td>
<td style="text-align:right;">
7.2
</td>
<td style="text-align:right;">
4375
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0078199/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/same_time_next_year/
</td>
</tr>
<tr>
<td style="text-align:left;">
Closet Land
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
1991
</td>
<td style="text-align:right;">
7.2
</td>
<td style="text-align:right;">
2098
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0101597/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/closet_land/
</td>
</tr>
<tr>
<td style="text-align:left;">
Politiki Kouzina (A Touch of Spice)
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Drama
</td>
<td style="text-align:left;">
Unrated
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
9216
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0378897/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/politiki-kouzina-a-touch-of-spice/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Wood
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
6.9
</td>
<td style="text-align:right;">
6336
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
92
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0161100/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/wood/
</td>
</tr>
<tr>
<td style="text-align:left;">
The Five Heartbeats
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Musical & Performing Arts
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1991
</td>
<td style="text-align:right;">
7.5
</td>
<td style="text-align:right;">
2551
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
Upright
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0101891/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/five_heartbeats/
</td>
</tr>
</tbody>
</table>

</div>

One can see that almost all the movies are voted “Upright” by the
audience, but have “Rotten” rating from critics. The possible
explanation is that our model “relies too much” on `critics_score` here.
One can also view movies, that have been grossly overestimated:

``` r
# Overestimates
filter(movies, runtime > 60)[design_aug$hpm.fit - design_aug$audience_score > 25, ] %>% 
  arrange(desc(audience_score)) %>% 
  select(-c(runtime, studio, thtr_rel_month:dvd_rel_day, best_pic_nom:actor5)) %>% 
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%", height = "400px")
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
Rambling Rose
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
1991
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
3866
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0102753/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/rambling_rose/
</td>
</tr>
<tr>
<td style="text-align:left;">
Young Adult
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
2011
</td>
<td style="text-align:right;">
6.3
</td>
<td style="text-align:right;">
63219
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1625346/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/young_adult/
</td>
</tr>
<tr>
<td style="text-align:left;">
Death Line (Raw Meat)
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Horror
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
2433
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0068458/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/death_line/
</td>
</tr>
<tr>
<td style="text-align:left;">
Mr. & Mrs. Bridge
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
1990
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
2282
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0100200/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/mr_and_mrs_bridge/
</td>
</tr>
<tr>
<td style="text-align:left;">
Someone to Watch over Me
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
1987
</td>
<td style="text-align:right;">
6.1
</td>
<td style="text-align:right;">
7076
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0094008/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/someone_to_watch_over_me/
</td>
</tr>
<tr>
<td style="text-align:left;">
Spring Breakers
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
2013
</td>
<td style="text-align:right;">
5.3
</td>
<td style="text-align:right;">
104457
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt2101441/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/spring_breakers_2013/
</td>
</tr>
<tr>
<td style="text-align:left;">
Jaws 2
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:left;">
PG
</td>
<td style="text-align:right;">
1978
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
48718
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0077766/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/jaws_2/
</td>
</tr>
<tr>
<td style="text-align:left;">
Kiss of Death
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
5.9
</td>
<td style="text-align:right;">
13980
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0113552/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1060720-kiss_of_death/
</td>
</tr>
<tr>
<td style="text-align:left;">
Rampart
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
2012
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:right;">
19115
</td>
<td style="text-align:left;">
Certified Fresh
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt1640548/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/rampart_2010/
</td>
</tr>
<tr>
<td style="text-align:left;">
Simone
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
2002
</td>
<td style="text-align:right;">
6.1
</td>
<td style="text-align:right;">
51070
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0258153/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/simone_2002/
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweet Liberty
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
1986
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
2181
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0092035/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/sweet_liberty/
</td>
</tr>
<tr>
<td style="text-align:left;">
Bounce
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
2000
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
17133
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0186894/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/bounce/
</td>
</tr>
<tr>
<td style="text-align:left;">
Night and the City
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
1992
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:right;">
3673
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0105001/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1040890-night_and_the_city/
</td>
</tr>
<tr>
<td style="text-align:left;">
Volcano
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
1997
</td>
<td style="text-align:right;">
5.4
</td>
<td style="text-align:right;">
56329
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0120461/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1076267-volcano/
</td>
</tr>
<tr>
<td style="text-align:left;">
Hulk
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:right;">
5.7
</td>
<td style="text-align:right;">
204042
</td>
<td style="text-align:left;">
Fresh
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0286716/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/hulk/
</td>
</tr>
<tr>
<td style="text-align:left;">
Death Defying Acts
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
2008
</td>
<td style="text-align:right;">
5.9
</td>
<td style="text-align:right;">
8345
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0472071/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/death_defying_acts/
</td>
</tr>
<tr>
<td style="text-align:left;">
Attack of the 50 Foot Woman
</td>
<td style="text-align:left;">
TV Movie
</td>
<td style="text-align:left;">
Other
</td>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
1993
</td>
<td style="text-align:right;">
3.8
</td>
<td style="text-align:right;">
2289
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0106317/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/1050445-attack_of_the_50_foot_woman/
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
Bats
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
3.6
</td>
<td style="text-align:right;">
8229
</td>
<td style="text-align:left;">
Rotten
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Spilled
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0200469/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/bats/
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
Battlefield Earth
</td>
<td style="text-align:left;">
Feature Film
</td>
<td style="text-align:left;">
Action & Adventure
</td>
<td style="text-align:left;">
PG-13
</td>
<td style="text-align:right;">
2000
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
64119
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
11
</td>
<td style="text-align:left;">
<http://www.imdb.com/title/tt0185183/>
</td>
<td style="text-align:left;">
//www.rottentomatoes.com/m/battlefield_earth/
</td>
</tr>
</tbody>
</table>

</div>

I do not see any clear pattern in these examples. In any case, a good
idea could be to search for some other good predictor variables to help
us to categorize movies in a meaningful way. This could require
collecting more data about the movies.
