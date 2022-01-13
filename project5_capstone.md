Predicting House Prices in Ames, Iowa
================
Vadim Belov

-   [Setup](#setup)
    -   [Background](#background)
    -   [Training Data and relevant
        packages](#training-data-and-relevant-packages)
-   [Part 1 Exploratory Data Analysis
    (EDA)](#part-1-exploratory-data-analysis-eda)
    -   [Data Preparation and Missing
        Values](#data-preparation-and-missing-values)
    -   [Categorical Predictors](#categorical-predictors)
    -   [Numerical Variables](#numerical-variables)
-   [Part 2 Development and assessment of an initial model, following a
    semi-guided process of
    analysis](#part-2-development-and-assessment-of-an-initial-model-following-a-semi-guided-process-of-analysis)
    -   [Section 2.1 An Initial Model](#section-21-an-initial-model)
    -   [Section 2.2 Model Selection](#section-22-model-selection)
    -   [Section 2.3 Initial Model
        Residuals](#section-23-initial-model-residuals)
    -   [Section 2.4 Initial Model RMSE](#section-24-initial-model-rmse)
    -   [Section 2.5 Overfitting](#section-25-overfitting)
-   [Part 3 Development of a Final
    Model](#part-3-development-of-a-final-model)
    -   [Section 3.1 Final Model](#section-31-final-model)
    -   [Section 3.2 Transformation](#section-32-transformation)
    -   [Section 3.3 Variable
        Interaction](#section-33-variable-interaction)
    -   [Section 3.4 Variable Selection](#section-34-variable-selection)
    -   [Section 3.5 Model Testing](#section-35-model-testing)
-   [Part 4 Final Model Assessment](#part-4-final-model-assessment)
    -   [Section 4.1 Final Model
        Residual](#section-41-final-model-residual)
    -   [Section 4.2 Final Model RMSE](#section-42-final-model-rmse)
    -   [Section 4.3 Final Model
        Evaluation](#section-43-final-model-evaluation)
    -   [Section 4.4 Final Model
        Validation](#section-44-final-model-validation)
-   [Part 5 Conclusion](#part-5-conclusion)

# Setup

## Background

As a statistical consultant working for a real estate investment firm,
your task is to develop a model to predict the selling price of a given
home in Ames, Iowa. Your employer hopes to use this information to help
assess whether the asking price of a house is higher or lower than the
true value of the house. If the home is undervalued, it may be a good
investment for the firm.

Go through all the stages of the realistic data analysis project,
following step-by-step guidelines. Answer specific questions with
appropriate methods to draw correct conclusions from the data. Complete
the analysis by building the predictive linear model and evaluating in
on out-of-sample observations. See the [project
description](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info5_capstone.md)
for more details.

## Training Data and relevant packages

In order to better assess the quality of the model you will produce, the
data have been randomly divided into three separate pieces: a training
data set, a testing data set, and a validation data set. For now we will
load the training data set, the others will be loaded and used later.

``` r
load("data/ames_train.Rdata")
```

Use the code block below to load any necessary packages

``` r
library(ggplot2)    # graphics
library(gridExtra)  # multiple side-by-side ggplot's
library(dotwhisker) # whiscker plots
library(dplyr)      # data manipulation
library(tidyr)      # data cleaning and representation change
library(forcats)    # for manipulating factors
library(BAS)        # Bayesian Variable Selection and Model Averaging
library(boot)       # Bootstrap Resampling
library(TOSTER)     # equivalence testing
library(knitr)      # report generation
library(kableExtra) # customized tables
```

NOTE: Some code chunks are hidden by default (e.g., for fancier plots),
but you can easily unfold them, by clicking on the “code” tab.

# Part 1 Exploratory Data Analysis (EDA)

When you first get your data, it’s very tempting to immediately begin
fitting models and assessing how they perform. However, before you begin
modeling, it’s absolutely essential to explore the structure of the data
and the relationships between the variables in the data set.

Do a detailed EDA of the ames_train data set, to learn about the
structure of the data and the relationships between the variables in the
data set (refer to Introduction to Probability and Data, Week 2, for a
reminder about EDA if needed). Your EDA should involve creating and
reviewing many plots/graphs and considering the patterns and
relationships you see.

After you have explored completely, submit the three graphs/plots that
you found most informative during your EDA process, and briefly explain
what you learned from each (why you found each informative).

------------------------------------------------------------------------

### Data Preparation and Missing Values

Lets create our design matrix by trimming the original data set, that
is, consecutively getting rid of the least useful or redundant
variables, leaving only potentially relevant predictors. For instance,
the identifier `PID` is a dummy variable, not generalizable to the
population of interest, and hence should be omitted. Before we proceed
further, I first recode nominal categorical feature `MS.SubClass` as
factor (see the
[codebook](http://jse.amstat.org/v19n3/decock/DataDocumentation.txt) for
explanation of level values), as well as some other integer variables,
that are discrete ordinals with few levels. I also convert NA values of
factors into explicit levels “No”, where they clearly denote the
“absence” category. Furthermore, the “actual” missing values of factors
appear as empty category names, that I replace with NA, for ease of
analysis. I apply the log-transform to numerical variables with the
meaning of “footage” (like “area”, but also “Lot.Frontage” linear feet).

``` r
footage <- c("area", "X1st.Flr.SF", "X2nd.Flr.SF", "Low.Qual.Fin.SF", "Lot.Area", "Lot.Frontage",
                    "Total.Bsmt.SF", "BsmtFin.SF.1", "BsmtFin.SF.2", "Bsmt.Unf.SF", 
                    "Open.Porch.SF", "Enclosed.Porch", "Screen.Porch", "X3Ssn.Porch",
                    "Garage.Area", "Mas.Vnr.Area", "Wood.Deck.SF", "Pool.Area")
small_ordinals <- c("Garage.Cars", "Fireplaces", "Bedroom.AbvGr", "Kitchen.AbvGr", 
                    "Full.Bath", "Bsmt.Full.Bath", "Half.Bath", "Bsmt.Half.Bath")
prepare_data <- function(
  data, normal_sale = FALSE, log_transform = footage, to_factor = "MS.SubClass", remove = "PID"
) {
  if (normal_sale) data <- data %>% filter(Sale.Condition == "Normal") %>% select(-Sale.Condition)
  transformed <- data %>% mutate(
    # NA -> "No"
    across(where(is.factor), function(x) fct_explicit_na(x, na_level = "No")),
    # "No" as reference level
    across(where(is.factor), function(x) if ("No" %in% levels(x)) relevel(x, ref = "No") else x),
    # MS.SubClass and small ordinals -> convert to factors
    across(all_of(to_factor), as.factor),
    # "" -> NA, explicitly drop unused levels (with no entries)
    across(where(is.factor), function(x) fct_drop(na_if(x, y = ""))),
    # log-transform area variables
    across(all_of(footage), function(x) log(x + 1), .names = "log_{.col}"), .keep = "unused"
  ) %>% 
    select(-all_of(remove))
}
d0 <- prepare_data(ames_train, to_factor = c("MS.SubClass", small_ordinals))
```

Right away, I choose to drop features, that either have highly
imbalanced classes (for factors), or too few non-zero values (for
integers) to be considered useful for prediction. This can be seen in
the summary below.

``` r
imbalanced <- c("Utilities", "Street", "Pool.QC", "Heating", "Roof.Matl", "Misc.Feature", "Bsmt.Half.Bath",
                "Misc.Val", "log_Pool.Area", "log_Low.Qual.Fin.SF", "log_X3Ssn.Porch")
d <- select(d0, -all_of(imbalanced))
# Display frequency counts
d0 %>% select(all_of(imbalanced)) %>% 
  mutate(across(where(is.numeric), function(x) as.factor(ifelse(x > 0, "> 0", "0")))) %>% summary()
```

    ##   Utilities     Street    Pool.QC  Heating      Roof.Matl   Misc.Feature
    ##  AllPub:1000   Grvl:  3   No:997   GasA:988   CompShg:984   No  :971    
    ##                Pave:997   Ex:  1   GasW:  8   Metal  :  1   Gar2:  2    
    ##                           Fa:  1   Grav:  2   Tar&Grv: 11   Othr:  1    
    ##                           Gd:  1   OthW:  1   WdShake:  2   Shed: 25    
    ##                                    Wall:  1   WdShngl:  2   TenC:  1    
    ##  Bsmt.Half.Bath Misc.Val  log_Pool.Area log_Low.Qual.Fin.SF log_X3Ssn.Porch
    ##  0   :940       > 0: 29   > 0:  3       > 0: 12             > 0: 14        
    ##  1   : 57       0  :971   0  :997       0  :988             0  :986        
    ##  2   :  2                                                                  
    ##  NA's:  1                                                                  
    ## 

In addition to the above, one can perform the ANOVA F-test for
dependence to detect any significant difference in average price between
individual predictor levels of categorical variables. (The assumption of
nearly normal distribution is satisfied by log-transformation of the
price.) As the sample size increases, the (“unexplained”) within-group
variability about the means will diminish but the (“explained”)
variation between group means will not, resulting in a higher
![F = \\frac{\\mathrm{explained\\ variance}}{\\mathrm{unexplained\\ variance}}](https://latex.codecogs.com/png.latex?F%20%3D%20%5Cfrac%7B%5Cmathrm%7Bexplained%5C%20variance%7D%7D%7B%5Cmathrm%7Bunexplained%5C%20variance%7D%7D "F = \frac{\mathrm{explained\ variance}}{\mathrm{unexplained\ variance}}")
(small p-values) and the rejection of the null hypothesis
![H_0](https://latex.codecogs.com/png.latex?H_0 "H_0"): “no difference”.
Hence, for our rather large sample (n = 1000), the following relatively
high p-values are in agreement with the assumed independence, justifying
our decision to discard those variables as not being reliable predictors
of average price.

``` r
d0 %>% summarize(
  across(
    c(all_of(imbalanced), -Utilities), function(x) {
      model <- lm(log(price) ~ x, subset = if (is.numeric(x)) x > 0)  # null-values are different
      p_value <- summary(aov(model))[[1]][["Pr(>F)"]][1]
    }
  )
) %>% kable(digits = 3) %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
Street
</th>
<th style="text-align:right;">
Pool.QC
</th>
<th style="text-align:right;">
Heating
</th>
<th style="text-align:right;">
Roof.Matl
</th>
<th style="text-align:right;">
Misc.Feature
</th>
<th style="text-align:right;">
Bsmt.Half.Bath
</th>
<th style="text-align:right;">
Misc.Val
</th>
<th style="text-align:right;">
log_Pool.Area
</th>
<th style="text-align:right;">
log_Low.Qual.Fin.SF
</th>
<th style="text-align:right;">
log_X3Ssn.Porch
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.062
</td>
<td style="text-align:right;">
0.008
</td>
<td style="text-align:right;">
0.448
</td>
<td style="text-align:right;">
0.255
</td>
<td style="text-align:right;">
0.229
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.737
</td>
<td style="text-align:right;">
0.181
</td>
<td style="text-align:right;">
0.207
</td>
<td style="text-align:right;">
0.501
</td>
</tr>
</tbody>
</table>

</div>

Lets now count the **missing values**:

``` r
na_counts <- colSums(is.na(d))
sort(na_counts[na_counts > 0], decreasing = TRUE)
```

    ##  log_Lot.Frontage     Garage.Yr.Blt      Mas.Vnr.Type  log_Mas.Vnr.Area 
    ##               167                48                 7                 7 
    ##     Bsmt.Exposure     Garage.Finish         Bsmt.Qual         Bsmt.Cond 
    ##                 2                 2                 1                 1 
    ##    BsmtFin.Type.1    BsmtFin.Type.2    Bsmt.Full.Bath       Garage.Cars 
    ##                 1                 1                 1                 1 
    ##       Garage.Qual       Garage.Cond log_Total.Bsmt.SF  log_BsmtFin.SF.1 
    ##                 1                 1                 1                 1 
    ##  log_BsmtFin.SF.2   log_Bsmt.Unf.SF   log_Garage.Area 
    ##                 1                 1                 1

With only one observation without any information about basement, and
one with unknown garage details, plus 2 entries with missing either
`Bsmt.Exposure` or `Garage.Finish`, I choose to omit these 4 incomplete
data entries from our rather large dataset. The following example of a
pairwise scatterplots helps us to justify dropping the variables,
containing many NA’s, by revealing their redundancy due to high
correlation among predictors. (This strategy can be applied more
generally to other variables as well.).

``` r
# Show redundancy of Garage.Yr.Blt and Lot.Frontage due to collinearity
d_garage <- filter(ames_train, !is.na(Garage.Yr.Blt))
garage_yr <- ggplot(d_garage, aes(y = Garage.Yr.Blt, x = Year.Built)) +
  geom_point(shape = 16, size = 2, alpha = 0.5) + 
  annotate(
    "text", x = 1980, y = 1920, size = 5,
    label = paste("Corr:", round(cor(d_garage$Garage.Yr.Blt, d_garage$Year.Built), digits = 3))
  )
d_lot <- filter(ames_train, !is.na(Lot.Frontage))
lot_area <- ggplot(d_lot, aes(x = log(Lot.Area), y = log(Lot.Frontage))) +
  geom_point(aes(shape = Lot.Shape, col = Lot.Shape), size = 2, alpha = 0.6) + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 3, 15)) +
  annotate(
    "text", x = 11, y = 3.5, size = 5,
    label = paste("Corr:", round(cor(log(d_lot$Lot.Area), log(d_lot$Lot.Frontage)), digits = 3))
  )
grid.arrange(
  garage_yr, lot_area, ncol = 2,
  top = grid::textGrob(
    "Collinearity", gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" style="display: block; margin: auto;" />

-   The `Garage.Yr.Blt` is missing for houses without the garage.
    Moreover, for majority of observations, the garage was built more or
    less simultaneously with the house itself, leading to a very high
    correlation coefficient
    (![R = 0.872](https://latex.codecogs.com/png.latex?R%20%3D%200.872 "R = 0.872")).
    Since it is implausible, that the `Garage.Yr.Blt` may contribute to
    price considerably (it is rather the garage area or its quality),
    and in order to avoid collinearity or redundancy of predictors, I
    choose to omit this variable altogether.

-   For the same reasons, I prefer to drop the `Lot.Frontage` with a lot
    of NA’s from potential predictors of price, since after
    log-transformation it appears to be a linear function of the total
    `log(Lot.Area)` (with
    ![R = 0.752](https://latex.codecogs.com/png.latex?R%20%3D%200.752 "R = 0.752"),
    which raises to
    ![R = 0.883](https://latex.codecogs.com/png.latex?R%20%3D%200.883 "R = 0.883")
    for the “Regular” `Lot.Shape`).

-   Lastly, I impute the 7 missing values of `Mas.Vnr.Area`,
    `Mas.Vnr.Type` to be 0 and “None”, respectively, which are by far
    the most common observations in the sample. (Note that
    `sum(d$Mas.Vnr.Area > 0 & d$Mas.Vnr.Type == "None") = 2`)

``` r
handle_na <- function(
  data, filter_rows = c("Bsmt.Exposure", "Garage.Finish"), omit_cols = c("Garage.Yr.Blt", "log_Lot.Frontage")
) {
  transformed <- data %>% filter( ! if_any(all_of(filter_rows), is.na)) %>% 
    select(-all_of(omit_cols)) %>% 
    mutate(
      log_Mas.Vnr.Area = replace_na(log_Mas.Vnr.Area, as.integer(0)),
      Mas.Vnr.Type = replace_na(Mas.Vnr.Type, "None")
    )
}
d <- handle_na(d)
summary(select(d, log_Mas.Vnr.Area, Mas.Vnr.Type))
```

    ##  log_Mas.Vnr.Area  Mas.Vnr.Type
    ##  Min.   :0.000    BrkCmn :  8  
    ##  1st Qu.:0.000    BrkFace:316  
    ##  Median :0.000    None   :597  
    ##  Mean   :2.099    Stone  : 75  
    ##  3rd Qu.:5.067                 
    ##  Max.   :7.163

### Categorical Predictors

A very useful way to visually inspect the possible dependence between
our numeric response and discrete predictors is a side-by-side boxplots.
In this way we can conveniently summarize and compare distributions of
price for each level of every categorical variable.

``` r
d %>% select(where(is.factor), price) %>% 
  pivot_longer(1:45, names_to = "predictor", values_to = "category") %>% 
  ggplot(aes(x = log(price), y = category)) +
  geom_boxplot() +
  facet_wrap( ~ predictor, ncol = 5, scales = "free") +
  labs(y = NULL, title = "Categorical Predictors for Price") +
  theme(axis.text = element_text(size = 6))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/categorical-predictors-1.png" width="90%" style="display: block; margin: auto;" />

-   Some variables show visually less variation in price between
    classes, and so are first candidates for exclusion.

-   Also, from the 2 versions of the same variable, I leave only the 1st
    one (with more pronounced variation).

-   When Quality (`.Qual`) and Condition (`.Cond`) variables are
    present, similar in nature, I usually prefer the first type over the
    second one (with more populated classes).

``` r
# Variation explained by each each factor (on average)
adj.R2 <- sapply(
  colnames(select_if(d, is.factor)),
  function(x) summary(lm(formula(paste("log(price) ~", x)), data = d))$adj.r.squared
)
round(sort(adj.R2, decreasing = TRUE), 3)
```

    ##   Neighborhood    Garage.Cars      Bsmt.Qual     Exter.Qual   Kitchen.Qual 
    ##          0.596          0.503          0.490          0.473          0.462 
    ##  Garage.Finish     Foundation      Full.Bath   Fireplace.Qu    Garage.Type 
    ##          0.357          0.335          0.333          0.321          0.312 
    ##    MS.SubClass     Fireplaces BsmtFin.Type.1     Heating.QC      MS.Zoning 
    ##          0.312          0.275          0.269          0.268          0.210 
    ##   Exterior.1st   Exterior.2nd   Mas.Vnr.Type    Garage.Cond  Bsmt.Exposure 
    ##          0.188          0.184          0.168          0.155          0.139 
    ##    Paved.Drive    Central.Air    Garage.Qual Sale.Condition      Sale.Type 
    ##          0.139          0.138          0.137          0.131          0.128 
    ## Bsmt.Full.Bath      Half.Bath      Lot.Shape     Electrical    House.Style 
    ##          0.100          0.097          0.090          0.086          0.075 
    ##      Bsmt.Cond     Exter.Cond     Roof.Style  Bedroom.AbvGr    Condition.1 
    ##          0.063          0.056          0.055          0.050          0.040 
    ##          Fence BsmtFin.Type.2      Bldg.Type   Land.Contour     Lot.Config 
    ##          0.033          0.030          0.028          0.026          0.022 
    ##     Functional          Alley    Condition.2  Kitchen.AbvGr     Land.Slope 
    ##          0.020          0.020          0.012          0.008          0.006

Additionally, we can take some hints about variable importance from the
amount of variation in response, explained by each categorical
predictor. Since some of the factors have multiple levels, one uses the
adjusted ![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2"), which
penalizes additional degrees of freedom.

### Numerical Variables

There are plenty of area variables (continuous) in the original dataset,
that we can divide into respective groups (together with the associated
discrete predictors).

| meaning  | qualitative                                                  | footage                       |
|:---------|:-------------------------------------------------------------|:------------------------------|
| Storeys  | `House.Style`                                                | `area` (Gr Liv Area) \> 0     |
|          | `MS.SubClass`                                                | `X1st.Flr.SF` \> 0            |
|          |                                                              | `X2nd.Flr.SF`                 |
|          |                                                              | `Low.Qual.Fin.SF` (removed)   |
| Lot      | `Lot.Shape`                                                  | `Lot.Area` \> 0               |
|          | `Lot.Config`                                                 | `Lot.Frontage` \> 0 (removed) |
| Basement | `Bsmt.Qual`                                                  | `Total.Bsmt.SF`               |
|          | `Bsmt.Cond`                                                  | `Bsmt.Unf.SF`                 |
|          | `BsmtFin.Type.1`                                             | `BsmtFin.SF.1`                |
|          | `BsmtFin.Type.2`                                             | `BsmtFin.SF.2`                |
| Garage   | `Garage.Type`, `Garage.Finish`, `Garage.Qual`, `Garage.Cond` | `Garage.Area`                 |
| Masonry  | `Mas.Vnr.Type`                                               | `Mas.Vnr.Area`                |
| Pool     | `Pool.QC` (removed)                                          | `Pool.Area` (removed)         |
| Porch    |                                                              | `Open.Porch.SF`               |
|          |                                                              | `Enclosed.Porch`              |
|          |                                                              | `X3Ssn.Porch` (removed)       |
|          |                                                              | `Screen.Porch`                |
| Other    |                                                              | `Wood.Deck.SF`                |

A lot of these variables have *structural* zeroes, that are constant (in
contrast to 0, appearing due to sampling variability). They represent
the absence of a given quantity and correspond to the “No” level of the
respective qualitative features. Fitting the linear model to such
predictor of mixed type (with the discrete mass concentrated at 0) will
affect the slope parameter
![\\hat{\\beta}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cbeta%7D "\hat{\beta}")
for the continuous distribution, as the null values pull the line away
from the main cloud of points. Therefore, when modelling, it is
reasonable to disentangle the continuous and discrete parts, for
instance, using an indicator variable to treat them separately. Some of
the respective qualitative fatures effectively play this role,
containing “No” category.

(Note that splitting the values of independent variable
![X](https://latex.codecogs.com/png.latex?X "X") generally reduces the
overall variability within each of subsets, and is usually not
recommended. However, it is justified in this case, as I argued above,
since the values are generated in two different ways. Hence, we
encounter a trade-off between the more accurate prediction and a lower
precision of the estimates, according to the formula
![\\mathrm{SE}(\\hat{\\beta}) = \\sigma^2/\\mathrm{var}(X)](https://latex.codecogs.com/png.latex?%5Cmathrm%7BSE%7D%28%5Chat%7B%5Cbeta%7D%29%20%3D%20%5Csigma%5E2%2F%5Cmathrm%7Bvar%7D%28X%29 "\mathrm{SE}(\hat{\beta}) = \sigma^2/\mathrm{var}(X)").)

The validity of a multiple regression method is based on the
approximately **linear association** of each (continuous) predictor with
response. This can be verified using the following scatterplots, where
the discrete zero values are ignored and the provided correlation
coefficient ![R](https://latex.codecogs.com/png.latex?R "R") helps to
assess the strength of association in each case.

``` r
d %>% select_if(is.numeric) %>% 
  relocate(price, .after = last_col()) %>% 
  pivot_longer(cols = 1:21, names_to = "predictor", values_to = "predictor_value") %>% 
  filter(predictor_value > 0) %>% 
  # Compute correlation with response
  group_by(predictor) %>% 
  mutate(R = round(cor(predictor_value, log(price)), 2)) %>% 
  # Draw scatterplots side-by-side
  ggplot(aes(x = predictor_value, y = log(price))) +
  geom_point(shape = 16, size = 1, alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess") +
  coord_flip() + 
  facet_wrap( ~ predictor, ncol = 7, scales = "free") + 
  geom_text(mapping = aes(x = -Inf, y = -Inf, label = R), hjust   = -0.3, vjust   = -12) +
  labs(x = NULL, title = "Logarithm of Price vs. Numerical Predictors") +
  theme(axis.text = element_text(size = 6))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" style="display: block; margin: auto;" />

Although in a multiple linear regression one is ultimately interested in
*conditional* dependencies, the individual correlation coefficients can
still give us some hint as to which predictors might be important:

-   The month and year the house was sold (`Mo.Sold`, `Yr.Sold`) are
    virtually irrelevant for the price.

-   The strongest relationship is with the `Overall.Qual`. The
    dependence on `Overall.Cond` demonstrates non-linear effects, to the
    extent that it flips the sign of
    ![R](https://latex.codecogs.com/png.latex?R "R") (this measure of
    linear association, thus, is not applicable here). As will become
    clear later, this is due to the excess of *new* houses in the medium
    condition, driving the price up. Hence, the inclusion of the age of
    the house should help (and, possibly, an interaction term).

-   Although the dependence of price on `Year.Built` appears to be
    slightly less linear than that on `Year.Remod.Add`, this is largely
    due to the values of the latter being effectively truncated at 1950
    for the vast majority of old houses, as can be checked on their
    respective scatterplot. For the most part, the remodel date is the
    “same as construction date if no remodeling or additions”, so I
    consider its effect as a “minor correction”.

-   Porch Area variables (`Open.Porch.SF`, `Enclosed.Porch`,
    `Screen.Porch`) show very weak association with price, considering
    the former is non-zero. (Remember that the latter condition should
    be stipulated for other area variables as well; see the above
    discussion.)

-   The `Total.Bsmt.SF` appears to be the single most reliable predictor
    among all Basement Area variables.

-   The situation with the above grade (ground) living area is a little
    bit trickier. First, the discrete ordinal `TotRms.AbvGrd` is
    linearly related with total `area`
    (![R = 0.811](https://latex.codecogs.com/png.latex?R%20%3D%200.811 "R = 0.811")).
    Among the two variables, I prefer the latter one (with the
    continuous range of values), since its correlation with price is
    higher.

``` r
# Regression coefficients of Flr.SF on area (intercepts are close to 0)
ames_train %>% mutate(second_floor = as.factor(ifelse(X2nd.Flr.SF > 0, "yes", "no"))) %>% 
  group_by(second_floor) %>% 
  summarize(across(c(X1st.Flr.SF, X2nd.Flr.SF), function(x) coef(lm(x ~ area))[2], .names = "{.col}_beta")) %>% 
  kable(digits = 2) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
second_floor
</th>
<th style="text-align:right;">
X1st.Flr.SF_beta
</th>
<th style="text-align:right;">
X2nd.Flr.SF_beta
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.45
</td>
</tr>
</tbody>
</table>

-   The total `area` is composed almost exclusively of square footage on
    one and/or two floors combined `I(X1st.Flr.SF + X2nd.Flr.SF)` (with
    correlation
    ![R = 0.995](https://latex.codecogs.com/png.latex?R%20%3D%200.995 "R = 0.995")).
    The individual floor SF either both roughly equal the half of the
    total `area` (for a “2Story” building), or approximately
    `X1st.Flr.SF = area` (for a “1Story” building), as can be seen by
    regressing them on `area` within each building type. Therefore,
    knowing the exact values of each store SF is not beneficial, once we
    are given the total area and the number of floors (e.g., contained
    in the type of dwelling `MS.SubClass`, or `House.style`), as also
    verified by the very strong correlation coefficients.

``` r
ames_train %>% group_by(House.Style) %>% summarize(
  cor(log(X1st.Flr.SF), log(area)),
  cor(log(X2nd.Flr.SF), log(area))
) %>% kable(digits = 2) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
House.Style
</th>
<th style="text-align:right;">
cor(log(X1st.Flr.SF), log(area))
</th>
<th style="text-align:right;">
cor(log(X2nd.Flr.SF), log(area))
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1.5Fin
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
NaN
</td>
</tr>
<tr>
<td style="text-align:left;">
1.5Unf
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
NaN
</td>
</tr>
<tr>
<td style="text-align:left;">
1Story
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
NaN
</td>
</tr>
<tr>
<td style="text-align:left;">
2.5Unf
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.92
</td>
</tr>
<tr>
<td style="text-align:left;">
2Story
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.84
</td>
</tr>
<tr>
<td style="text-align:left;">
SFoyer
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
NaN
</td>
</tr>
<tr>
<td style="text-align:left;">
SLvl
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
NaN
</td>
</tr>
</tbody>
</table>

General Note: any feature that does not appear to be important on its
own, can still bring in some predictive value if taken in combination
with other variables (when all else is being held fixed). It is
important then to ensure that one does not “fit the noise” in the data
(that is, to minimize overfitting the training sample).

------------------------------------------------------------------------

# Part 2 Development and assessment of an initial model, following a semi-guided process of analysis

## Section 2.1 An Initial Model

In building a model, it is often useful to start by creating a simple,
intuitive initial model based on the results of the exploratory data
analysis. (Note: The goal at this stage is **not** to identify the
“best” possible model but rather to choose a reasonable and
understandable starting point. Later you will expand and revise this
model to create your final model.

Based on your EDA, select *at most* 10 predictor variables from
“ames_train” and create a linear model for `price` (or a transformed
version of price) using those variables. Provide the *R code* and the
*summary output table* for your model, a *brief justification* for the
variables you have chosen, and a *brief discussion* of the model results
in context (focused on the variables that appear to be important
predictors and how they relate to sales price).

------------------------------------------------------------------------

During the EDA, we have excluded the vast majority of numerical (mostly
area) variables, as well as some highly imbalanced categorical
predictors. For the initial model, lets choose about 5 numerical and 5
factor variables from those that appear to be most closely associated
with response, such as area outside and inside the house (both above and
below grade), as well as its age. The other important predictors are the
qualty of the overall material and finish of the house, of the exterior
and of the basement (despite the name `Bsmt.Qual` actually measures the
height), as well as the overall condition of the house. Finally, number
of fireplaces and garage size in car capacity seem to be good indicators
of price. (Note the following linear dependencies that I found
`Fireplaces ~ Fireplace.Qu`, `BsmtFin.Type.1 ~ Bsmt.Qual`,
proportionality `Garage.Cars ~ Garage.Area`).

From the preliminary analysis, I decide to only model housing prices
under normal sale conditions, because houses with non-normal selling
conditions exhibit atypical behavior and can disproportionately
influence the model (as suggested in Capstone Quiz II). Since both
`ames_test` and `ames_validation` data sets contain only “Normal” sales,
there is a *sampling bias* (leading to test RMSE \< train RMSE). There
is no point in modelling the dependence on `Sale.Condition` (e.g., using
interaction terms), since its accuracy for other types of sales cannot
be independently verified.

``` r
initial <- c("log_area", "log_Lot.Area", "log_Total.Bsmt.SF", "Year.Built",
             "Overall.Cond", "Overall.Qual", "Exter.Qual", "Garage.Cars", "Fireplaces", "BsmtFin.Type.1")
d_norm <- ames_train %>% prepare_data(
  normal_sale = TRUE, to_factor = "MS.SubClass", remove = c("PID", imbalanced)
) %>% handle_na()
d_initial <- select(d_norm, c(price, all_of(initial)))
initial.lm <- lm(log(price) ~ ., data = d_initial)
summary(initial.lm)
```

    ## 
    ## Call:
    ## lm(formula = log(price) ~ ., data = d_initial)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.82757 -0.05981  0.00176  0.06466  0.50420 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.753586   0.444869   3.942 8.78e-05 ***
    ## log_area           0.371515   0.016919  21.959  < 2e-16 ***
    ## log_Lot.Area       0.108027   0.008442  12.797  < 2e-16 ***
    ## log_Total.Bsmt.SF  0.122475   0.013547   9.041  < 2e-16 ***
    ## Year.Built         0.002920   0.000204  14.310  < 2e-16 ***
    ## Overall.Cond       0.055396   0.004013  13.805  < 2e-16 ***
    ## Overall.Qual       0.069714   0.005226  13.340  < 2e-16 ***
    ## Exter.QualFa      -0.289491   0.050835  -5.695 1.73e-08 ***
    ## Exter.QualGd      -0.151415   0.026853  -5.639 2.37e-08 ***
    ## Exter.QualTA      -0.216616   0.029677  -7.299 6.90e-13 ***
    ## Garage.Cars        0.039324   0.007130   5.515 4.69e-08 ***
    ## Fireplaces         0.044638   0.006949   6.423 2.27e-10 ***
    ## BsmtFin.Type.1ALQ -0.625709   0.096543  -6.481 1.58e-10 ***
    ## BsmtFin.Type.1BLQ -0.645928   0.097336  -6.636 5.88e-11 ***
    ## BsmtFin.Type.1GLQ -0.599913   0.096519  -6.216 8.16e-10 ***
    ## BsmtFin.Type.1LwQ -0.699677   0.096474  -7.252 9.53e-13 ***
    ## BsmtFin.Type.1Rec -0.651782   0.096591  -6.748 2.85e-11 ***
    ## BsmtFin.Type.1Unf -0.697881   0.094143  -7.413 3.10e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.108 on 814 degrees of freedom
    ## Multiple R-squared:  0.9218, Adjusted R-squared:  0.9201 
    ## F-statistic: 564.3 on 17 and 814 DF,  p-value: < 2.2e-16

``` r
# Visually display coefficients with 95 CI, using dot-whiskers plot
dwplot(initial.lm) + geom_vline(xintercept = 0, lty = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/fit_model-1.png" width="90%" style="display: block; margin: auto;" />

*Discussion*: All chosen variables appear to be important predictors,
with significant coefficients (low p-values, for at least one level of
categorical predictors). The numerical predictors are all positively
related to price. For instance, for each additional point in
`Overall.Qual`, one expects the price to be higher by
![\\exp(0.07) = 1.07](https://latex.codecogs.com/png.latex?%5Cexp%280.07%29%20%3D%201.07 "\exp(0.07) = 1.07"),
that is to raise by about 7% on average, all else being equal.
Similarly, for 10% increase in above grade living `area`, the predicted
price is expected to raise by roughly
![(1.1^{0.0372} - 1) \* 100 = 3.6\\%](https://latex.codecogs.com/png.latex?%281.1%5E%7B0.0372%7D%20-%201%29%20%2A%20100%20%3D%203.6%5C%25 "(1.1^{0.0372} - 1) * 100 = 3.6\%").
Analogously, the newer houses are more valuable, etc.

The coefficients for categorical predictors show the relative change in
average `log(price)` with respect to reference level, all else being
held fixed. Thus, any additional fireplace or car in the garage
consecutively increases the average expected price by the respective
amount. The model predicts lower price on average for any quality of the
material on the exterior, below the “Excellent”, and so on.

------------------------------------------------------------------------

## Section 2.2 Model Selection

Now either using `BAS` or another stepwise selection procedure choose
the “best” model you can, using your initial model as your starting
point. Try at least two different model selection methods and compare
their results. Do they both arrive at the same model or do they
disagree? What do you think this means?

------------------------------------------------------------------------

Apart from the prior uncertainty, surrounding coefficients
![\\beta_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j") of
each predictor, our uncertainty about model choice can be represented
using probability distribution
![p(M_k)](https://latex.codecogs.com/png.latex?p%28M_k%29 "p(M_k)") over
all possible models. The Bayesian Model Averaging (implemented in the
`BAS` package) then allows to assess the full posterior uncertainty
after seeing the data.

``` r
# To help with the BAS plotting, if `force heredity = TRUE` is chosen
# create named vector of subset indexes, skipping individual levels
idx_per_lvl <- function(d, interactions = NULL, intercept = FALSE) {
  if ( ! is.null(interactions) ) {
    for (two_way in interactions) {
      terms <- strsplit(two_way, ":")[[1]]
      left <- terms[1]; right <- terms[2]
      if ( is.numeric(d[[left]]) ) d[ , two_way] <- d[ , right] else d[ , two_way] <- d[ , left]
    }
  }
  variables <- cumsum(
    sapply(rename(d, Intercept = price), function(x) if (is.numeric(x)) 1 else length(levels(x)) - 1)
  )
  if ( ! intercept ) variables <- variables[-1]
  return ( variables )
}
```

``` r
initial.bas <- bas.lm(
  formula = log(price) ~ ., data = d_initial,
  # as n gets large, log-likelihood is approximated by BIC, for nearly flat reference prior on coef-s
  prior = "BIC", 
  # all models are equally likely
  modelprior = uniform(), 
  # force all levels of a factor to be included together
  force.heredity = TRUE
)
# Display prior inclusion probabilities (PIP), without an intercept
variables <- idx_per_lvl(d_initial)
plot(initial.bas, which = 4, ask = FALSE, caption = "", sub.caption = "", 
     col.in = "blue", col.ex = "darkgrey", lwd = 3, cex = 0.7,
     main = "Importance of Coefficients Under BMA",
     # show only one level of each factor (same PIP)
     subset = variables, names = names(variables))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/initial_model_select-1.png" width="90%" style="display: block; margin: auto;" />

I have chosen an uninformative flat reference prior on coefficients
(calculated using `BIC` method), that results in fairly conservative
estimates (in contrast to less restrictive approaches, such as `JZS` or
`AIC` prior). All of the chosen predictors turn out to be important,
with
![p(\\beta_j\\neq 0) > 0.5](https://latex.codecogs.com/png.latex?p%28%5Cbeta_j%5Cneq%200%29%20%3E%200.5 "p(\beta_j\neq 0) > 0.5")
nearly equal 1.

One can compare relative plausibility of different models, using
calculated `PostProbs` (their ratio then reduces to Bayes factors for
our uniform prior distribution). The resulting model space and content
is visualized graphically, where each color corresponds to the log
posterior odds (over the null model):

``` r
# I have silently modified plot.bas() and image.bas() to customize variable names
image(initial.bas, cex.axis = 1, rotate = FALSE, subset = variables, names = names(variables))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" style="display: block; margin: auto;" />

Given our choice of priors, the single best model is the full initial
model, including all 10 variables, according to either Highest
Probability Model (optimal w.r.t. 0-1 loss), Median Probability Model
(pips \> 0.5), or the Best Predictive Model (whose predictions are
closest to that of BMA). We can compare the `BAS` results with the
stepwise model selection, using “information criteria”.

``` r
n <- nrow(d_initial)
# Backward elimination using BIC, rather than AIC, penalized -2*log(average likelihood) + k
step_initial.lm <- step(initial.lm, k = log(n), trace = TRUE)
```

    ## Start:  AIC=-3600.12
    ## log(price) ~ log_area + log_Lot.Area + log_Total.Bsmt.SF + Year.Built + 
    ##     Overall.Cond + Overall.Qual + Exter.Qual + Garage.Cars + 
    ##     Fireplaces + BsmtFin.Type.1
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## <none>                            9.5001 -3600.1
    ## - Garage.Cars        1    0.3550  9.8550 -3576.3
    ## - Fireplaces         1    0.4815  9.9816 -3565.7
    ## - Exter.Qual         3    0.7333 10.2334 -3558.4
    ## - log_Total.Bsmt.SF  1    0.9539 10.4540 -3527.2
    ## - BsmtFin.Type.1     6    1.8416 11.3416 -3493.0
    ## - log_Lot.Area       1    1.9112 11.4113 -3454.3
    ## - Overall.Qual       1    2.0769 11.5769 -3442.3
    ## - Overall.Cond       1    2.2243 11.7244 -3431.8
    ## - Year.Built         1    2.3901 11.8901 -3420.1
    ## - log_area           1    5.6275 15.1276 -3219.8

In contrast to BMA, the backward elimination strategy does not scan
through all possible combinations of predictors and, hence, it can miss
the model with the better score. Here, elimination of any variable leads
to higher (“worse”) BIC value, so one arrives at the same model as `BAS`
results. (As an alternative, the hybrid selection method alternates
between elimination and inclusion of variables.)

------------------------------------------------------------------------

## Section 2.3 Initial Model Residuals

One way to assess the performance of a model is to examine the model’s
residuals. In the space below, create a residual plot for your preferred
model from above and use it to assess whether your model appears to fit
the data well. Comment on any interesting structure in the residual plot
(trend, outliers, etc.) and briefly discuss potential implications it
may have for your model and inference / prediction you might produce.

------------------------------------------------------------------------

``` r
pred.HPM <- predict(initial.bas, estimator = "HPM")
d_norm.aug <- d_norm %>% mutate(
  initial.fit = pred.HPM$fit, initial.resid = log(price) - initial.fit, initial.pred = exp(initial.fit)
)
```

``` r
d_resid <- mutate(d_norm.aug, fit = initial.fit, resid = initial.resid, .keep = "unused")
# Fit distribution(s)
r <- d_resid$resid
normal.fit <- MASS::fitdistr(r, "normal")$estimate
t.fit <- MASS::fitdistr(
  r, "t", start = list(m = mean(r), s = sd(r), df = 3), lower = c(-1, 0.001, 1)
)$estimate
names(t.fit) <- c("mu", "sigma", "df")
resid_hist <- ggplot(d_resid, aes(x = resid)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(aes(col = "t"), size = 1, alpha = 0.6, fun = ggdist::dstudent_t, args = t.fit) +
  stat_function(aes(col = "Normal"), size = 1, alpha = 0.6, fun = dnorm, args = normal.fit) + 
  scale_color_manual(NULL, values = c("seagreen", "red", "dodgerblue")) + 
  annotate("text", x = mean(r), y = 0.001, label = paste("Kurtosis:", round(e1071::kurtosis(r), 3))) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "Residuals", y = "Probability Density")
resid_qqnorm <- ggplot(d_resid, aes(sample = resid)) + 
  stat_qq_line(aes(col = "t"), distribution = qt, dparams = t.fit["df"], alpha = 0.5, size = 1) +
  stat_qq(aes(col = "t"), distribution = qt, dparams = t.fit["df"], size = 2, shape = 21, alpha = 1) +
  stat_qq_line(aes(col = "Normal"), alpha = 0.5, size = 1) +
  stat_qq(aes(col = "Normal"), size = 2, shape = 21, alpha = 1) + 
  scale_color_manual(NULL, values = c("#F8766D", "#00BFC4")) + 
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Q-Q Plot for Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
resid_fitted <- ggplot(d_resid, aes(x = fit, y = abs(resid))) +
  geom_point(shape = 16, size = 2, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Absolute Values of Residuals")
resid_order <- ggplot(d_resid, aes(x = 1:length(resid), y = resid)) +
  geom_point(shape = 21, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Independence", x = "Order of Collection", y = "Residuals")
grid.arrange(resid_hist, resid_qqnorm, resid_fitted, resid_order, ncol = 2, nrow = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

1.  *The residuals are nearly normal* around 0. They are somewhat
    over-dispersed with heavy tails (corresponding to flipped S-shape on
    the QQ-plot and positive excess kurtosis), but deviations from
    normality are rather small (especially for large data sets like
    ours, n = 832).
2.  *The variability of the residuals is approximately constant*
    (homoscedasticity), without obvious trend, as can be seen from the
    absolute values of residuals, plotted against fitted values of
    `log(price)`. One can identify several observations that look like
    outliers in our model. I will return to this issue shortly.  
3.  The apparent randomness of residuals w.r.t. order of collection
    implies that they are *independent*, coming from a random sample.  
4.  Each variable should be *linearly related* to the outcome, as we
    already discussed in the EDA section on the variable transformation.
    We can additionally check linearity assumption, by plotting model
    residuals against each numeric predictor. There may be a slight
    underestimate of price by our model for very large area values.

``` r
draw_scatter <- function(data, response = "resid", predictors, ...) {
  n <- length(predictors)
  plots <- vector(mode = "list", length = n)
  for (i in 1:n) {
    plots[[i]] <- ggplot(data, aes_string(x = predictors[i], y = response)) + 
      geom_point(shape = 16, size = 2, alpha = 0.5) + 
      geom_smooth(formula = y ~ x, method = "loess", span = 0.9) +
      labs(y = NULL)
  }
  args <- list(...)
  do.call(grid.arrange, c(plots, args))
}
draw_scatter(
  d_resid, predictors = c(
    "log_area", "log_Lot.Area", "log_Total.Bsmt.SF", "Overall.Qual", "Overall.Cond", "Year.Built"
  ), ncol = 3, top = grid::textGrob(
    "Linearity Assumption: Residuals vs. Predictors", gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
) 
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" style="display: block; margin: auto;" />

Notice that an observation can only be unexpected or influential in
light of a particular model. Assuming the residuals come from a Gaussian
distribution, identify up to 4 potential outliers in a dataset:

``` r
EnvStats::rosnerTest( d_norm.aug$initial.resid, k = 4 )$all.stats 
```

    ##   i        Mean.i      SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
    ## 1 0  3.180151e-14 0.1069210 -0.8275675     610 7.739992   3.994246    TRUE
    ## 2 1  9.958694e-04 0.1030521  0.5042049     559 4.883055   3.993944    TRUE
    ## 3 2  3.895935e-04 0.1016205  0.4161432      90 4.091238   3.993642    TRUE
    ## 4 3 -1.119187e-04 0.1006488 -0.3750246      62 3.724958   3.993340   FALSE

We can inspect these observations, having the highest squared residuals,
to find anything unusual about them. For instance, the most extreme
outlier is seriously overpriced for its area and lot size. It can
partially be explained by its location in `Neighborhood = "IDOTRR"`
(i.e., “Iowa DOT and Rail Road”), but even more illuminating, it may
actually be a “barn” building with `Roof.Style == "Gambrel"` (cf. the
[codebook](http://jse.amstat.org/v19n3/decock/DataDocumentation.txt)).

``` r
out_idx <- sort.int( d_norm.aug$initial.resid ^ 2, decreasing = TRUE, index.return = TRUE)$ix[1:3]
d_norm.aug %>% tibble::rownames_to_column(var = "idx") %>% slice(out_idx) %>% 
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
idx
</th>
<th style="text-align:right;">
price
</th>
<th style="text-align:left;">
MS.SubClass
</th>
<th style="text-align:left;">
MS.Zoning
</th>
<th style="text-align:left;">
Alley
</th>
<th style="text-align:left;">
Lot.Shape
</th>
<th style="text-align:left;">
Land.Contour
</th>
<th style="text-align:left;">
Lot.Config
</th>
<th style="text-align:left;">
Land.Slope
</th>
<th style="text-align:left;">
Neighborhood
</th>
<th style="text-align:left;">
Condition.1
</th>
<th style="text-align:left;">
Condition.2
</th>
<th style="text-align:left;">
Bldg.Type
</th>
<th style="text-align:left;">
House.Style
</th>
<th style="text-align:right;">
Overall.Qual
</th>
<th style="text-align:right;">
Overall.Cond
</th>
<th style="text-align:right;">
Year.Built
</th>
<th style="text-align:right;">
Year.Remod.Add
</th>
<th style="text-align:left;">
Roof.Style
</th>
<th style="text-align:left;">
Exterior.1st
</th>
<th style="text-align:left;">
Exterior.2nd
</th>
<th style="text-align:left;">
Mas.Vnr.Type
</th>
<th style="text-align:left;">
Exter.Qual
</th>
<th style="text-align:left;">
Exter.Cond
</th>
<th style="text-align:left;">
Foundation
</th>
<th style="text-align:left;">
Bsmt.Qual
</th>
<th style="text-align:left;">
Bsmt.Cond
</th>
<th style="text-align:left;">
Bsmt.Exposure
</th>
<th style="text-align:left;">
BsmtFin.Type.1
</th>
<th style="text-align:left;">
BsmtFin.Type.2
</th>
<th style="text-align:left;">
Heating.QC
</th>
<th style="text-align:left;">
Central.Air
</th>
<th style="text-align:left;">
Electrical
</th>
<th style="text-align:right;">
Bsmt.Full.Bath
</th>
<th style="text-align:right;">
Full.Bath
</th>
<th style="text-align:right;">
Half.Bath
</th>
<th style="text-align:right;">
Bedroom.AbvGr
</th>
<th style="text-align:right;">
Kitchen.AbvGr
</th>
<th style="text-align:left;">
Kitchen.Qual
</th>
<th style="text-align:right;">
TotRms.AbvGrd
</th>
<th style="text-align:left;">
Functional
</th>
<th style="text-align:right;">
Fireplaces
</th>
<th style="text-align:left;">
Fireplace.Qu
</th>
<th style="text-align:left;">
Garage.Type
</th>
<th style="text-align:left;">
Garage.Finish
</th>
<th style="text-align:right;">
Garage.Cars
</th>
<th style="text-align:left;">
Garage.Qual
</th>
<th style="text-align:left;">
Garage.Cond
</th>
<th style="text-align:left;">
Paved.Drive
</th>
<th style="text-align:left;">
Fence
</th>
<th style="text-align:right;">
Mo.Sold
</th>
<th style="text-align:right;">
Yr.Sold
</th>
<th style="text-align:left;">
Sale.Type
</th>
<th style="text-align:right;">
log_area
</th>
<th style="text-align:right;">
log_X1st.Flr.SF
</th>
<th style="text-align:right;">
log_X2nd.Flr.SF
</th>
<th style="text-align:right;">
log_Lot.Area
</th>
<th style="text-align:right;">
log_Total.Bsmt.SF
</th>
<th style="text-align:right;">
log_BsmtFin.SF.1
</th>
<th style="text-align:right;">
log_BsmtFin.SF.2
</th>
<th style="text-align:right;">
log_Bsmt.Unf.SF
</th>
<th style="text-align:right;">
log_Open.Porch.SF
</th>
<th style="text-align:right;">
log_Enclosed.Porch
</th>
<th style="text-align:right;">
log_Screen.Porch
</th>
<th style="text-align:right;">
log_Garage.Area
</th>
<th style="text-align:right;">
log_Mas.Vnr.Area
</th>
<th style="text-align:right;">
log_Wood.Deck.SF
</th>
<th style="text-align:right;">
initial.fit
</th>
<th style="text-align:right;">
initial.resid
</th>
<th style="text-align:right;">
initial.pred
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
610
</td>
<td style="text-align:right;">
40000
</td>
<td style="text-align:left;">
70
</td>
<td style="text-align:left;">
C (all)
</td>
<td style="text-align:left;">
Pave
</td>
<td style="text-align:left;">
Reg
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Inside
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
IDOTRR
</td>
<td style="text-align:left;">
Feedr
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
1Fam
</td>
<td style="text-align:left;">
2Story
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1920
</td>
<td style="text-align:right;">
1950
</td>
<td style="text-align:left;">
Gambrel
</td>
<td style="text-align:left;">
BrkFace
</td>
<td style="text-align:left;">
BrkFace
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
BrkTil
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
SBrkr
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
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Typ
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Detchd
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
MnPrv
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
WD
</td>
<td style="text-align:right;">
7.183871
</td>
<td style="text-align:right;">
6.476972
</td>
<td style="text-align:right;">
6.505784
</td>
<td style="text-align:right;">
9.047939
</td>
<td style="text-align:right;">
6.476972
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
6.476972
</td>
<td style="text-align:right;">
4.007333
</td>
<td style="text-align:right;">
5.153292
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.525453
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11.42420
</td>
<td style="text-align:right;">
-0.8275675
</td>
<td style="text-align:right;">
91509.88
</td>
</tr>
<tr>
<td style="text-align:left;">
559
</td>
<td style="text-align:right;">
230000
</td>
<td style="text-align:left;">
120
</td>
<td style="text-align:left;">
RM
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
IR1
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Inside
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
GrnHill
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
TwnhsE
</td>
<td style="text-align:left;">
1Story
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1986
</td>
<td style="text-align:right;">
1986
</td>
<td style="text-align:left;">
Gable
</td>
<td style="text-align:left;">
BrkFace
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
CBlock
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
SBrkr
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Typ
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Attchd
</td>
<td style="text-align:left;">
RFn
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
WD
</td>
<td style="text-align:right;">
7.167038
</td>
<td style="text-align:right;">
7.167038
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
9.016756
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.746203
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11.84163
</td>
<td style="text-align:right;">
0.5042049
</td>
<td style="text-align:right;">
138916.69
</td>
</tr>
<tr>
<td style="text-align:left;">
90
</td>
<td style="text-align:right;">
330000
</td>
<td style="text-align:left;">
120
</td>
<td style="text-align:left;">
RM
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
IR1
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Inside
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
GrnHill
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
TwnhsE
</td>
<td style="text-align:left;">
1Story
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1998
</td>
<td style="text-align:right;">
1998
</td>
<td style="text-align:left;">
Gable
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
BrkFace
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
PConc
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
GLQ
</td>
<td style="text-align:left;">
ALQ
</td>
<td style="text-align:left;">
Ex
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
SBrkr
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Typ
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Attchd
</td>
<td style="text-align:left;">
Fin
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:left;">
WD
</td>
<td style="text-align:right;">
7.315218
</td>
<td style="text-align:right;">
7.315218
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
9.186457
</td>
<td style="text-align:right;">
7.315218
</td>
<td style="text-align:right;">
7.005789
</td>
<td style="text-align:right;">
4.290459
</td>
<td style="text-align:right;">
5.796058
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.659482
</td>
<td style="text-align:right;">
5.480639
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
12.29070
</td>
<td style="text-align:right;">
0.4161432
</td>
<td style="text-align:right;">
217663.32
</td>
</tr>
</tbody>
</table>

</div>

Are these points *influential and / or have high leverage?* From the
following plot one can see that outliers are well within the main cloud
of points (low leverage) and have low influence on predictions. Cook’s
distance measures the effect of deleting a point on the combined
parameter vector.

``` r
plot(initial.lm, which = 5, id.n = 2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" style="display: block; margin: auto;" />

**Bonus discussion**: The assumption of the normal distribution of
errors makes the model more easily surprised by rare observations. Were
the data contaminated by *highly influential* outliers, one could employ
some kind of “robust regression” instead (e.g., replacing the Gaussian
error with Student’s t-distribution with thicker tails). I have tried
this approach using `MASS::rlm()` (with either default method, or custom
[psi.t](https://stats.stackexchange.com/questions/117980/regression-with-t-distributed-errors-and-massrlm)
settings). Although the residuals now are distributed in accord with the
new assumption, the performance of the model is worse on both training
and test datasets.

There is a theoretical reason for this effectiveness of ordinary least
squares. According to [the Gauss-Markov
theorem](https://en.wikipedia.org/wiki/Gauss%E2%80%93Markov_theorem),
one “cannot do better than OLS” within the class of linear unbiased
estimators, assuming the errors are uncorrelated, have equal variances
and expectation value of zero. Notice that these conditions do not imply
normality. Therefore, since outliers do not present the problem in our
case, I will stick with the ordinary linear regression, that gives
better results.

------------------------------------------------------------------------

## Section 2.4 Initial Model RMSE

You can calculate it directly based on the model output. Be specific
about the units of your RMSE (depending on whether you transformed your
response variable). The value you report will be more meaningful if it
is in the original units (dollars).

------------------------------------------------------------------------

Lets calculate both RMSE and R-squared, but first, convert predicted
`log(price)` into price in dollars $.

``` r
d_norm.aug %>% mutate(y_bar = mean(price)) %>% 
  summarize(
    RMSE = sqrt( mean( (price - initial.pred) ^ 2 ) ), 
    Rsq = 1 - RMSE ^ 2 / mean( (price - y_bar) ^ 2 )
  ) %>% kable(digits = 4) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
Rsq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
20473.41
</td>
<td style="text-align:right;">
0.9198
</td>
</tr>
</tbody>
</table>

The initial model has root mean squared error of about 20,473$, while
the variance explained according to R-squared is 92%.

------------------------------------------------------------------------

## Section 2.5 Overfitting

The process of building a model generally involves starting with an
initial model (as you have done above), identifying its shortcomings,
and adapting the model accordingly. This process may be repeated several
times until the model fits the data reasonably well. However, the model
may do well on training data but perform poorly out-of-sample (meaning,
on a dataset other than the original training data) because the model is
overly-tuned to specifically fit the training data. This is called
“overfitting.” To determine whether overfitting is occurring on a model,
compare the performance of a model on both in-sample and out-of-sample
data sets. To look at performance of your initial model on out-of-sample
data, you will use the data set `ames_test`.

``` r
load("data/ames_test.Rdata")
```

Use your model from above to generate predictions for the housing prices
in the test data set. Are the predictions significantly more accurate
(compared to the actual sales prices) for the training data than the
test data? Why or why not? Briefly explain how you determined that (what
steps or processes did you use)?

------------------------------------------------------------------------

Since we pre-processed the `ames_train` data set for modelling purposes,
we should do the same with the test data frame. It turns out, if the
model includes factor variables, whose levels are missing from the
training subset, one may eventually encounter “factor has new levels”
issue, when making predictions on previously unseen data. If only a few
such observations, I then choose to simply omit these data points, since
the model cannot be used for prediction in this case. (Alternatively,
the problem could be avoided with more careful train/test split, e.g.,
using some advanced sampling techniques.)

``` r
# Create reusable helper function to omit unused levels, for future references
omit_unused <- function(data, lm, levels = NULL) {
  if ( is.null(levels) ) levels <- lm$xlevels  # as named list
  return( data %>% filter( if_all( all_of(names(levels)), ~ .x %in% levels[[substitute(.x)]] ) ) )
}
d_test <- ames_test %>% prepare_data(
  normal_sale = TRUE, to_factor = "MS.SubClass", remove = c("PID", imbalanced)
) %>% handle_na()  # 1 NA removed
cat("Number of observations:", nrow(ames_test), 
    "\nHow many left:", nrow(d_test), "\nAll NA removed:", ! any(is.na(d_test)))
```

    ## Number of observations: 817 
    ## How many left: 816 
    ## All NA removed: TRUE

Lets test our model on previously unseen data. One way to assess how
well a model reflects uncertainty is through *coverage probability*. For
example, if assumptions are met, a 95% prediction interval for
`log(price)` (colored in grey on the plot) should include the true value
roughly 95% of the time.

``` r
pred.HPM <- predict(initial.bas, newdata = d_test, estimator = "HPM", se.fit = TRUE, nsim = 10 ^ 6)
d_test.aug <- cbind(d_test, cbind(exp(confint(pred.HPM, parm = "pred")))) %>% 
  mutate(initial.pred = pred, initial.lwr = `2.5%`, initial.upr = `97.5%`, .keep = "unused")
cover_prob <- summarize(d_test.aug, mean(price > initial.lwr & price < initial.upr))[1,1]
ggplot(d_test.aug, aes(x = log(price), y = log(initial.pred))) + 
  geom_ribbon(aes(ymin = log(initial.lwr), ymax = log(initial.upr)), fill = "grey70", alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "blue") +
  geom_point(shape = 16, size = 2, alpha = 0.7) +
  annotate("text", x = 12, y = 13, size = 5, label = paste("Coverage Probability:", round(cover_prob, 3))) +
  labs(y = "Prediction", title = "Initial Model Performance on the Test Data")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/initmodel_test-1.png" width="90%" style="display: block; margin: auto;" />

It looks like the model generalizes reasonably well. To check for
overfitting, one could compare RMSE or
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") for training
and test datasets.

``` r
d_test.aug %>% summarize(
  RMSE = sqrt( mean( (price - initial.pred) ^ 2 ) ), 
  Rsq = 1 - RMSE ^ 2 / mean( (price - mean(price)) ^ 2 )
) %>% kable(digits = 3) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
Rsq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
21959.72
</td>
<td style="text-align:right;">
0.909
</td>
</tr>
</tbody>
</table>

The obtained values appear to be close to in-sample results. *Are these
differences big or small*, in other words, does the model performance
differ significantly between train and test subsets? As with any such
claim in statistics, it is not very meaningful to compare two point
estimates without providing the accompanying uncertainty. Since the
standard error reduces as the sample size grows, for such large data
sets as ours, we almost surely will detect the difference, e.g., via
usual null-hypothesis significance test (NHST). However, one can test
not only to reject the strict zero, but *any* effect size, as well as
the finite *range* of values.

**Bonus discussion**: The opposite of NHST is the so called [equivalence
testing](https://journals.sagepub.com/eprint/IW7EgmfBewJdICjP9YYX/full)
(cf. also [Improving your statistical
inferences](https://www.coursera.org/learn/statistical-inferences) on
Coursera), where one assumes now that there is an effect (the new
null-hypothesis
![H_0: \|\\text{effect}\\ d\| > \\Delta](https://latex.codecogs.com/png.latex?H_0%3A%20%7C%5Ctext%7Beffect%7D%5C%20d%7C%20%3E%20%5CDelta "H_0: |\text{effect}\ d| > \Delta"))
and tries to reject it (with a p \< 0.05), using two one-sided tests
![t=\\frac{\\mu_1 - \\mu_2 - \\Delta}{SD\_{\\text{pooled}}\\sqrt{\\frac{1}{n_1} + \\frac{1}{n_1}}}](https://latex.codecogs.com/png.latex?t%3D%5Cfrac%7B%5Cmu_1%20-%20%5Cmu_2%20-%20%5CDelta%7D%7BSD_%7B%5Ctext%7Bpooled%7D%7D%5Csqrt%7B%5Cfrac%7B1%7D%7Bn_1%7D%20%2B%20%5Cfrac%7B1%7D%7Bn_1%7D%7D%7D "t=\frac{\mu_1 - \mu_2 - \Delta}{SD_{\text{pooled}}\sqrt{\frac{1}{n_1} + \frac{1}{n_1}}}")
(TOST). The alternative hypothesis is that the effect is anywhere in the
equivalence range
![H_A:\|d\|\<\\Delta](https://latex.codecogs.com/png.latex?H_A%3A%7Cd%7C%3C%5CDelta "H_A:|d|<\Delta")
(meaning that the effect, which is too small to worth attention, is
deemed “equivalent to the absence of an effect”). This procedure has
certain advantages over the standard NHST:

1.  Since the null-hypothesis can only be rejected, but is never
    accepted, one usually has either a very weak claim
    ![d\\neq0](https://latex.codecogs.com/png.latex?d%5Cneq0 "d\neq0")
    (some effect exists, however small), or it is not possible to
    meaningfully conclude that there is no effect (strong assumption
    ![d=0](https://latex.codecogs.com/png.latex?d%3D0 "d=0")). When the
    situation is reversed as above, the rejection of “large effect”
    assumption, leads to the acceptance of the “weak effect” alternative
    (the difference is too small for all practical matters).

2.  The presence of a threshold
    ![\\Delta](https://latex.codecogs.com/png.latex?%5CDelta "\Delta")
    on the effect size (no more arbitrary than the common significance
    level
    ![\\alpha = 0.05](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%200.05 "\alpha = 0.05"))
    forces us to think about cost and benefit of further investigation,
    power considerations, that is, practical, rather than statistical
    “significance”.

In principle, the previous research can help to elicit the reasonable
equivalence bound
![\\Delta](https://latex.codecogs.com/png.latex?%5CDelta "\Delta"). For
instance, had our model selection discarded the better fit to the sample
in favor of the less accurate one, based on higher posterior probability
![p(M_k\|\\text{data})](https://latex.codecogs.com/png.latex?p%28M_k%7C%5Ctext%7Bdata%7D%29 "p(M_k|\text{data})")
or BIC score (designed to penalize for overfitting), we could have used
this difference as a reference guide. Otherwise, the change is not
relevant if it is within the natural spread of the statistic of
interest. Lets say, we can tolerate the raw difference in
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") no larger than
2% (knowing the market, one could have used the absolute RMSE in $). A
quick way to estimate the sampling variability of a given statistic is
by using the *bootstrap* method, i.e., the repeated sampling with
replacement.

``` r
# Provide a function that computes the statistic of interest for the bootstrap sample (index subset)
RMSE <- function(d, i) sqrt( mean( (d$price[i] - d$pred[i]) ^ 2 ) )
Rsq <- function(d, i) 1 - mean( (d$price[i] - d$pred[i]) ^ 2 ) / mean( (d$price[i] - mean(d$price[i])) ^ 2 )
# Calculate the summary statistics using bootstrap
boot.stats <- function(sample1, sample2, statistic, R = 10 ^ 5) {
  boot1 <- boot(sample1, statistic, R)
  boot2 <- boot(sample2, statistic, R)
  n1 <- nrow(sample1); n2 <- nrow(sample2)
  m1 <- mean(boot1$t); m2 <- mean(boot2$t)
  sd1 <- sd(boot1$t); sd2 <- sd(boot2$t)
  var_p <- ( (n1 - 1) * sd1 ^ 2 +  (n2 - 1) * sd2 ^ 2 ) / ( n1 + n2 - 2 ) 
  return( list( stat = as.character(substitute(statistic)), 
                n1 = n1, n2 = n2, m1 = m1, m2 = m2, dm = m1 - m2, d = (m1 - m2) / sqrt( var_p ), 
                sd1 = sd1, sd2 = sd2, bias1 = m1 - boot1$t0, bias2 = m2 - boot2$t0 ) )
}
# Choose the equivalence bound (either raw dm or Cohen's d)
set.seed(6)
sample_train <- d_norm.aug %>% mutate(pred = initial.pred)
sample_test <- d_test.aug %>% mutate(pred = initial.pred)
RMSE.boot <- boot.stats(sample_test, sample_train, RMSE)
Rsq.boot <- boot.stats(sample_test, sample_train, Rsq)
as.data.frame(mapply(c, RMSE.boot, Rsq.boot, SIMPLIFY = FALSE)) %>% 
  kable(digits = 4) %>% kable_styling("striped") 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
stat
</th>
<th style="text-align:right;">
n1
</th>
<th style="text-align:right;">
n2
</th>
<th style="text-align:right;">
m1
</th>
<th style="text-align:right;">
m2
</th>
<th style="text-align:right;">
dm
</th>
<th style="text-align:right;">
d
</th>
<th style="text-align:right;">
sd1
</th>
<th style="text-align:right;">
sd2
</th>
<th style="text-align:right;">
bias1
</th>
<th style="text-align:right;">
bias2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
RMSE
</td>
<td style="text-align:right;">
816
</td>
<td style="text-align:right;">
832
</td>
<td style="text-align:right;">
21939.0477
</td>
<td style="text-align:right;">
20452.7113
</td>
<td style="text-align:right;">
1486.3364
</td>
<td style="text-align:right;">
1.3739
</td>
<td style="text-align:right;">
1119.0992
</td>
<td style="text-align:right;">
1044.0054
</td>
<td style="text-align:right;">
-20.6718
</td>
<td style="text-align:right;">
-20.6963
</td>
</tr>
<tr>
<td style="text-align:left;">
Rsq
</td>
<td style="text-align:right;">
816
</td>
<td style="text-align:right;">
832
</td>
<td style="text-align:right;">
0.9084
</td>
<td style="text-align:right;">
0.9196
</td>
<td style="text-align:right;">
-0.0112
</td>
<td style="text-align:right;">
-1.5704
</td>
<td style="text-align:right;">
0.0076
</td>
<td style="text-align:right;">
0.0066
</td>
<td style="text-align:right;">
-0.0006
</td>
<td style="text-align:right;">
-0.0002
</td>
</tr>
</tbody>
</table>

Notice that the raw mean difference `dm` is well within the `2 * sd2` of
the sampling distribution for the training set. The standardized measure
of effect size is the Cohen’s
![d = \\frac{\\mu_1 - \\mu_2}{SD\_{\\text{pooled}}}](https://latex.codecogs.com/png.latex?d%20%3D%20%5Cfrac%7B%5Cmu_1%20-%20%5Cmu_2%7D%7BSD_%7B%5Ctext%7Bpooled%7D%7D%7D "d = \frac{\mu_1 - \mu_2}{SD_{\text{pooled}}}"),
and the obtained values are typically considered very large in
experimental design (cf. the
[interpretation](https://rpsychologist.com/cohend/)). Since we want to
exclude particularly big drops in model performance, not surprisingly,
it is rather easy to fit the equivalence bounds.

``` r
set.seed(6)
TOSTtwo.raw(
  n1 = Rsq.boot$n1, m1 = Rsq.boot$m1, sd1 = Rsq.boot$sd1, low_eqbound = -0.02, 
  n2 = Rsq.boot$n2, m2 = Rsq.boot$m2, sd2 = Rsq.boot$sd2, high_eqbound = 0.02,
  verbose = FALSE, plot = TRUE
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-24-1.png" width="90%" style="display: block; margin: auto;" />

*Interpretation*: Provided we consider differences in
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") less than 0.02
as negligible, the performance of our model is equivalent on the
training and test data samples (“no overfitting”), although we can still
detect statistically significant non-zero effect. (Similar
considerations are valid for RMSE statistic with the mean difference
about 1485$ and the bound as low as 1600$, say.)

------------------------------------------------------------------------

**Note to the learner:** If in real-life practice this out-of-sample
analysis shows evidence that the training data fits your model a lot
better than the test data, it is probably a good idea to go back and
revise the model (usually by simplifying the model) to reduce this
overfitting. For simplicity, we do not ask you to do this on the
assignment, however.

# Part 3 Development of a Final Model

Now that you have developed an initial model to use as a baseline,
create a final model with *at most* 20 variables to predict housing
prices in Ames, IA, selecting from the full array of variables in the
dataset and using any of the tools that we introduced in this
specialization.

Carefully document the process that you used to come up with your final
model, so that you can answer the questions below.

## Section 3.1 Final Model

Provide the summary table for your model.

------------------------------------------------------------------------

The question does not specify whether interaction terms should be
counted as additional variables or not, so I assume the answer is “yes”.
The summary of my final linear model is as follows:

``` r
final <- c("log_area", "log_Lot.Area", "log_Total.Bsmt.SF", "Year.Built", "Overall.Cond", 
           "Overall.Qual", "Exter.Qual", "Garage.Cars", "Fireplaces", "BsmtFin.Type.1", "MS.Zoning",
           "Bsmt.Full.Bath", "Central.Air", "Bedroom.AbvGr", "Year.Remod.Add", 
           "log_area:Overall.Qual", "Year.Built:Overall.Cond", "log_Total.Bsmt.SF:Bsmt.Full.Bath",
           "Bsmt.Full.Bath:Bedroom.AbvGr", "Central.Air:Year.Remod.Add")
final_rhs <- paste(final, collapse = " + ")
final.lm <- lm(formula = paste("log(price) ~", final_rhs), data = d_norm)
summary(final.lm)
```

    ## 
    ## Call:
    ## lm(formula = paste("log(price) ~", final_rhs), data = d_norm)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.41065 -0.05339  0.00245  0.05267  0.59071 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      -8.7486580  2.1324801  -4.103 4.50e-05 ***
    ## log_area                          0.1268635  0.0425968   2.978 0.002987 ** 
    ## log_Lot.Area                      0.0934238  0.0084822  11.014  < 2e-16 ***
    ## log_Total.Bsmt.SF                 0.0831659  0.0134609   6.178 1.03e-09 ***
    ## Year.Built                        0.0048646  0.0008192   5.938 4.29e-09 ***
    ## Overall.Cond                      0.9362907  0.2570749   3.642 0.000288 ***
    ## Overall.Qual                     -0.2851144  0.0495124  -5.758 1.21e-08 ***
    ## Exter.QualFa                     -0.1347062  0.0461353  -2.920 0.003601 ** 
    ## Exter.QualGd                     -0.0932207  0.0251753  -3.703 0.000228 ***
    ## Exter.QualTA                     -0.1381471  0.0285379  -4.841 1.55e-06 ***
    ## Garage.Cars                       0.0377506  0.0063596   5.936 4.35e-09 ***
    ## Fireplaces                        0.0359381  0.0063554   5.655 2.17e-08 ***
    ## BsmtFin.Type.1ALQ                -0.3636396  0.0946938  -3.840 0.000133 ***
    ## BsmtFin.Type.1BLQ                -0.3852706  0.0954432  -4.037 5.94e-05 ***
    ## BsmtFin.Type.1GLQ                -0.3555870  0.0944780  -3.764 0.000180 ***
    ## BsmtFin.Type.1LwQ                -0.4349133  0.0947831  -4.589 5.18e-06 ***
    ## BsmtFin.Type.1Rec                -0.3869839  0.0951586  -4.067 5.24e-05 ***
    ## BsmtFin.Type.1Unf                -0.4224251  0.0931944  -4.533 6.71e-06 ***
    ## MS.ZoningFV                       0.3993001  0.0489308   8.160 1.29e-15 ***
    ## MS.ZoningI (all)                  0.3324451  0.1156295   2.875 0.004146 ** 
    ## MS.ZoningRH                       0.2252248  0.0600888   3.748 0.000191 ***
    ## MS.ZoningRL                       0.3389228  0.0459373   7.378 4.03e-13 ***
    ## MS.ZoningRM                       0.2822812  0.0456205   6.188 9.75e-10 ***
    ## Bsmt.Full.Bath                   -0.3757003  0.1345289  -2.793 0.005352 ** 
    ## Central.AirY                      7.3507178  1.6023516   4.587 5.21e-06 ***
    ## Bedroom.AbvGr                    -0.0013614  0.0074049  -0.184 0.854176    
    ## Year.Remod.Add                    0.0041841  0.0007937   5.271 1.74e-07 ***
    ## log_area:Overall.Qual             0.0479768  0.0068418   7.012 4.99e-12 ***
    ## Year.Built:Overall.Cond          -0.0004551  0.0001314  -3.462 0.000564 ***
    ## log_Total.Bsmt.SF:Bsmt.Full.Bath  0.0707582  0.0193169   3.663 0.000266 ***
    ## Bsmt.Full.Bath:Bedroom.AbvGr     -0.0264095  0.0066358  -3.980 7.52e-05 ***
    ## Central.AirY:Year.Remod.Add      -0.0037096  0.0008166  -4.543 6.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09453 on 800 degrees of freedom
    ## Multiple R-squared:  0.9411, Adjusted R-squared:  0.9389 
    ## F-statistic: 412.7 on 31 and 800 DF,  p-value: < 2.2e-16

It contains all of the variables of the initial model, plus 10
additional ones, 5 of each are interaction terms. Some of the predictors
may have low significance in the presence of interactions, but it is
important to include these “main effects” as well (the `BAS` option
`force.heredity = TRUE` ensures such hierarchy rules).

------------------------------------------------------------------------

## Section 3.2 Transformation

Did you decide to transform any variables? Why or why not? Explain in a
few sentences.

------------------------------------------------------------------------

Due to strong right-skewness, I have applied log-transformation to the
response variable `price`, as well as to all predictor variables with
the meaning of “area” (as described in EDA). The latter are first
shifted by insignificant bias of 1 SF, so that the zero area is also
zero on the log-scale (instead of
![-\\infty](https://latex.codecogs.com/png.latex?-%5Cinfty "-\infty")).
The distance from null is stretched on the new scale, but the non-zero
values turn out to be more linearly related to `log(price)`, as result.

I did not center or scale variables, since most of them have natural
boundaries, and I would like to maintain interpretability. Since both
test and validation data sets contain only `Sale.Condition == "Normal"`
entries, I have fitted the model to only this type of sales, instead of
including the corresponding variable and its associated interactions.

------------------------------------------------------------------------

## Section 3.3 Variable Interaction

Did you decide to include any variable interactions? Why or why not?
Explain in a few sentences.

------------------------------------------------------------------------

I have included 5 interactions in total: `log_area * Overall.Qual`,
`Year.Built * Overall.Cond`, `log_Total.Bsmt.SF * Bsmt.Full.Bath`,
`Bsmt.Full.Bath * Bedroom.AbvGr`, `Central.Air * Year.Remod.Add`. Their
effect can be seen on the following plots:

``` r
area_qual <- ggplot(d, aes(y = log(price), x = log_area, col = as.factor(Overall.Qual))) +
  geom_point(size = 1, alpha = 0.5) + 
  labs(col = "Overall.Qual", x = "log(area)") +
  theme(legend.position = "top") + guides(col = guide_legend(byrow = TRUE))
year_cond <- ggplot(d, aes(y = log(price), x = Year.Built, col = as.factor(Overall.Cond))) +
  geom_point(size = 1, alpha = 0.6) + 
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + 
  labs(col = "Overall.Cond") +
  theme(legend.position = "top") + guides(col = guide_legend(byrow = TRUE))
cond_year <- ggplot(d, aes(y = log(price), x = Overall.Cond, col = Year.Built)) +
  geom_point(size = 1, alpha = 0.6) + 
  viridis::scale_color_viridis(option = "D") + 
  theme(legend.position = "top") + guides(col = guide_legend(nrow = 1))
bsmtSF_fullbath <- ggplot(d, aes(y = log(price), x = log_Total.Bsmt.SF, col = Bsmt.Full.Bath)) +
  geom_point(size = 1, alpha = 0.6) + 
  labs(x = "log(Total.Bsmt.SF)") +
  theme(legend.position = "top") + guides(col = guide_legend(nrow = 1)) 
fullbath_bed <- ggplot(d, aes(x = Bedroom.AbvGr, y = Bsmt.Full.Bath)) +
  geom_tile(aes(fill = log(price))) + viridis::scale_fill_viridis() +
  theme(legend.position = "top") + guides(col = guide_legend(nrow = 1))
remod_air <- ggplot(d, aes(y = log(price), x = Year.Remod.Add, col = Central.Air)) +
  geom_point(size = 1, alpha = 0.6) +
  theme(legend.position = "top") + guides(col = guide_legend(nrow = 1))
grid.arrange(
  area_qual, year_cond, bsmtSF_fullbath, cond_year, remod_air, fullbath_bed, ncol = 2,
  top = grid::textGrob(
    "Interactions", gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/model_inter-1.png" width="90%" style="display: block; margin: auto;" />

Note that the peak in price for medium condition houses is explained by
the large number of more expensive recently built “average” houses. The
accompanying plot against `Year.Built` illuminates how old houses split
in price, according to `Overall.Cond`, that can compensate for the age
of the house.

------------------------------------------------------------------------

## Section 3.4 Variable Selection

What method did you use to select the variables you included? Why did
you select the method you used? Explain in a few sentences.

------------------------------------------------------------------------

Starting with the full model, containing *all* available predictors, one
cannot guarantee that the found best one will have *at most 20* of them.
On the other hand, the stepwise approach can help us select the set of
most promising predictors (and even rank them by the impact on BIC
scores). Since the initial model is already quite accurate, I take it as
a starting point and consecutively add or remove variables, using
`direction = "both"` method. I have also included all 2-way interactions
in the scope of the search. One arrives at the following model:

``` r
n <- nrow(d_norm)
rhs_initial <- paste(initial, collapse = " + ")
rhs_all <- paste(colnames(d_norm[ , -1]), collapse = " + ")
step_final.lm <- step(
  lm(formula = paste("log(price) ~", rhs_initial), data = d_norm), 
  scope = paste("log(price) ~ (", rhs_all, ") ^ 2"), 
  direction = "both", k = log(n), trace = FALSE
)
step_final.lm
```

    ## 
    ## Call:
    ## lm(formula = log(price) ~ log_area + log_Lot.Area + log_Total.Bsmt.SF + 
    ##     Year.Built + Overall.Cond + Overall.Qual + Exter.Qual + Garage.Cars + 
    ##     Fireplaces + BsmtFin.Type.1 + MS.Zoning + Bsmt.Full.Bath + 
    ##     Central.Air + Bedroom.AbvGr + Year.Remod.Add + log_BsmtFin.SF.1 + 
    ##     log_area:Overall.Qual + Year.Built:Overall.Cond + log_Total.Bsmt.SF:Bsmt.Full.Bath + 
    ##     log_Total.Bsmt.SF:Overall.Qual + Bsmt.Full.Bath:Bedroom.AbvGr + 
    ##     Central.Air:Year.Remod.Add + log_Total.Bsmt.SF:Year.Remod.Add + 
    ##     Year.Built:Fireplaces, data = d_norm)
    ## 
    ## Coefficients:
    ##                      (Intercept)                          log_area  
    ##                       -1.5380975                         0.0146057  
    ##                     log_Lot.Area                 log_Total.Bsmt.SF  
    ##                        0.0891833                        -1.0423896  
    ##                       Year.Built                      Overall.Cond  
    ##                        0.0053912                         1.0850672  
    ##                     Overall.Qual                      Exter.QualFa  
    ##                       -0.3242238                        -0.1420135  
    ##                     Exter.QualGd                      Exter.QualTA  
    ##                       -0.1008848                        -0.1462883  
    ##                      Garage.Cars                        Fireplaces  
    ##                        0.0394878                         1.1638737  
    ##                BsmtFin.Type.1ALQ                 BsmtFin.Type.1BLQ  
    ##                       -0.5360012                        -0.5551732  
    ##                BsmtFin.Type.1GLQ                 BsmtFin.Type.1LwQ  
    ##                       -0.5229607                        -0.6003789  
    ##                BsmtFin.Type.1Rec                 BsmtFin.Type.1Unf  
    ##                       -0.5539269                        -0.5010491  
    ##                      MS.ZoningFV                  MS.ZoningI (all)  
    ##                        0.4129436                         0.3862684  
    ##                      MS.ZoningRH                       MS.ZoningRL  
    ##                        0.2283814                         0.3547061  
    ##                      MS.ZoningRM                    Bsmt.Full.Bath  
    ##                        0.2932753                        -0.3784716  
    ##                     Central.AirY                     Bedroom.AbvGr  
    ##                        7.1924096                        -0.0018668  
    ##                   Year.Remod.Add                  log_BsmtFin.SF.1  
    ##                        0.0002403                         0.0152193  
    ##            log_area:Overall.Qual           Year.Built:Overall.Cond  
    ##                        0.0653658                        -0.0005326  
    ## log_Total.Bsmt.SF:Bsmt.Full.Bath    log_Total.Bsmt.SF:Overall.Qual  
    ##                        0.0698158                        -0.0128531  
    ##     Bsmt.Full.Bath:Bedroom.AbvGr       Central.AirY:Year.Remod.Add  
    ##                       -0.0251537                        -0.0036359  
    ## log_Total.Bsmt.SF:Year.Remod.Add             Year.Built:Fireplaces  
    ##                        0.0006069                        -0.0005725

None of the initial model variables has been removed. To get the final
model of at most 20 variables, including interactions, I have eliminated
4 terms without substantial impact on performance (3 interactions and 1
predictor `log_BsmtFin.SF.1`, that seems to me redundant with respect to
`log_Total.Bsmt.SF`). At last, confirm that the selected model is indeed
the best one according to BMA (HPM = MPM = BPM).

``` r
set.seed(200)
# Get the variables as strings from the step output
call_rows <- trimws(deparse(summary(step_final.lm)$call))
call_rhs <- strsplit(paste(call_rows, collapse = " "), split = "~ |, ")[[1]][2]
vars <- strsplit(call_rhs, split = " + ", fixed = TRUE)[[1]]
# Omit the least relevant terms
rhs_final <- paste(vars[c(1:15, 17:19, 21:22)], collapse = " + ")
final.bas <- bas.lm(
  formula = paste("log(price) ~", rhs_final), data = d_norm,
  prior = "BIC", modelprior = uniform(), force.heredity = TRUE
)
# Display PIP's
variables <- idx_per_lvl( d_norm[ , c("price", vars[c(1:15)])], interactions = vars[c(17:19, 21:22)] )
plot(final.bas, which = 4, ask = FALSE, caption = "", sub.caption = "", 
     col.in = "blue", col.ex = "darkgrey", lwd = 3, cex = 0.4,
     main = "Importance of Coefficients Under BMA",
     subset = variables, names = names(variables))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-25-1.png" width="90%" style="display: block; margin: auto;" />

``` r
image(final.bas, cex.axis = 0.5, rotate = FALSE, subset = variables, names = names(variables))
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-26-1.png" width="90%" style="display: block; margin: auto;" />

------------------------------------------------------------------------

## Section 3.5 Model Testing

How did testing the model on out-of-sample data affect whether or how
you changed your model? Explain in a few sentences.

------------------------------------------------------------------------

24 variables in total were selected by the stepwise approach, using BIC
criterion. I used the results on the test subset in order to eliminate 4
of them, without noticeable drop in model performance, as estimated by
the test RMSE or
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") metric. In
fact, while removing `log_Total.Bsmt.SF:Overall.Qual` reduced
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") by only 0.003,
it actually increased the coverage probability by about 0.01, which is a
better reflection of uncertainty by our model.

``` r
pred.HPM <- predict(final.bas, newdata = d_test, estimator = "HPM", se.fit = TRUE, nsim = 10 ^ 6)
d_test.aug <- cbind(d_test.aug, cbind(exp(confint(pred.HPM, parm = "pred")))) %>% 
  mutate(final.pred = pred, final.lwr = `2.5%`, final.upr = `97.5%`, .keep = "unused") 
cover_prob <- summarize(d_test.aug, mean(price > final.lwr & price < final.upr))[1,1]
ggplot(d_test.aug, aes(x = log(price), y = log(final.pred))) + 
  geom_ribbon(aes(ymin = log(final.lwr), ymax = log(final.upr)), fill = "grey70", alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "blue") +
  geom_point(shape = 16, size = 2, alpha = 0.7) +
  annotate("text", x = 12, y = 13, size = 5, label = paste("Coverage Probability:", round(cover_prob, 3))) +
  labs(y = "Prediction", title = "Final Model Performance on the Test Data")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/final_model_testing-1.png" width="90%" style="display: block; margin: auto;" />

The final model performance on the test set is as follows:

``` r
d_test.aug %>% summarize(
  RMSE = sqrt( mean( (price - final.pred) ^ 2 ) ), 
  Rsq = 1 - RMSE ^ 2 / mean( (price - mean(price)) ^ 2 )
) %>% kable(digits = 4) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
Rsq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
18964.12
</td>
<td style="text-align:right;">
0.9321
</td>
</tr>
</tbody>
</table>

Another way how the test data can help in bias-variance tradeoff is
through drawing the *learning curves*, that is the error on both the
training and test subsets, plotted against the sample size, used for
model fitting.

``` r
set.seed(33)
steps <- seq(from = 32, to = 832, by = 10)
n_steps <- length(steps)
learn <- data.frame(s = steps, train.RMSE = integer(n_steps), test.RMSE = integer(n_steps))
d_train <- d_norm[sample(832), ]
for (s in steps) {
  d_s <- d_train[1:s, ]
  fit.lm <- lm(paste("log(price) ~", rhs_final), data = d_s)
  learn$train.RMSE[which(steps == s)] <- RMSE(
    cbind(d_s, pred = exp(predict(fit.lm))), i = 1:s
  )
  d_test_s <- d_test %>% omit_unused(fit.lm)
  learn$test.RMSE[which(steps == s)] <- RMSE(
    cbind(d_test_s, pred = exp(predict(fit.lm, newdata = d_test_s))), i = 1:nrow(d_test_s)
  )
}
learn_longer <- learn %>% rename(batch = s, train = train.RMSE, test = test.RMSE) %>% 
  pivot_longer(cols = 2:3, names_to = "sample", values_to = "RMSE")
ggplot(learn_longer, aes(x = batch, y = RMSE, col = sample)) + geom_line(size = 1.2)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-28-1.png" width="90%" style="display: block; margin: auto;" />

As the model learns from data, the two errors converge and more-or-less
level-off after 400 sample points, with the gap being around 1000$ (“low
variance”). This means, adding more data is unlikely to improve the fit.
(In fact, the training error might even decrease a bit, as the model
learns more from the sample.) In order to further reduce the error one
should use more complex models (higher variance), that in turn may
result in poorer generalizability (higher train-test gap).

------------------------------------------------------------------------

# Part 4 Final Model Assessment

## Section 4.1 Final Model Residual

For your final model, create and briefly interpret an informative plot
of the residuals.

------------------------------------------------------------------------

All that we said about initial model residuals is also valid for the
final one: 1) close to normality, but slightly over-dispersed, 2)
constant variability, with few notable outliers, etc.

``` r
pred.HPM <- predict(final.bas, estimator = "HPM")
d_norm.aug <- d_norm.aug %>% mutate(
  final.fit = pred.HPM$fit, final.resid = log(price) - final.fit, final.pred = exp(final.fit)
)
```

``` r
d_resid <- mutate(d_norm.aug, resid = final.resid, fit = final.fit, .keep = "unused")
r <- d_resid$resid
normal.fit <- MASS::fitdistr(r, "normal")$estimate
resid_hist <- ggplot(d_resid, aes(x = resid)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, col = "white", alpha = 0.3) +
  geom_density(aes(col = "Data"), size = 1, fill = "mediumseagreen", alpha = 0.15) + 
  stat_function(aes(col = "Normal"), size = 1, fun = dnorm, args = normal.fit) +
  scale_color_manual(NULL, values = c("seagreen", "red")) + 
  annotate("text", x = mean(r), y = 0.001, label = paste("Kurtosis:", round(e1071::kurtosis(r), 3))) +
  theme(legend.position = c(0.15, 0.9)) +
  labs(title = "Histogram", x = "Residuals", y = "Probability Density")
resid_qqnorm <- ggplot(d_resid, aes(sample = resid)) + 
  stat_qq(size = 2, shape = 21) + stat_qq_line() +
  labs(title = "Normal Q-Q Plot for Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
resid_fitted <- ggplot(d_resid, aes(x = fit, y = resid)) +
  geom_point(shape = 16, size = 2, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Absolute Values of Residuals")
grid.arrange(resid_hist, resid_qqnorm, resid_fitted, ncol = 3)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-29-1.png" width="90%" style="display: block; margin: auto;" />

Although the outliers are not as extreme as in the initial model, one
can see from the next plot, that the point 610 has higher leverage and
is more influential now. This is probably due to inclusion of
`MS.Zoning`, where only 5 observations are also classified as
“Commercial”.

``` r
plot(final.lm, which = 5, id.n = 2)
```

    ## Warning: not plotting observations with leverage one:
    ##   471

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-30-1.png" width="90%" style="display: block; margin: auto;" />

The observation 471 has been identified to have leverage 1, so that
![\\hat{y}\_i = y_i](https://latex.codecogs.com/png.latex?%5Chat%7By%7D_i%20%3D%20y_i "\hat{y}_i = y_i").
This is peculiar, meaning that only observation itself influences its
own prediction, all the other observations being irrelevant for this
point. This is also maybe due to the inclusion of factors with multiple
levels (making this observation an isolated point “one of the kind”).

------------------------------------------------------------------------

## Section 4.2 Final Model RMSE

For your final model, calculate and briefly comment on the RMSE.

------------------------------------------------------------------------

``` r
d_norm.aug %>% mutate(final = final.pred, initial = initial.pred, y_bar = mean(price)) %>% 
  pivot_longer(c(final, initial), names_to = "model", values_to = "prediction") %>% 
  group_by(model) %>% 
  summarize(
    RMSE = sqrt( mean( (price - prediction) ^ 2 ) ), 
    Rsq = 1 - RMSE ^ 2 / mean( (price - y_bar) ^ 2 )
  ) %>% kable(digits = 4) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
model
</th>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
Rsq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
final
</td>
<td style="text-align:right;">
17290.97
</td>
<td style="text-align:right;">
0.9428
</td>
</tr>
<tr>
<td style="text-align:left;">
initial
</td>
<td style="text-align:right;">
20473.41
</td>
<td style="text-align:right;">
0.9198
</td>
</tr>
</tbody>
</table>

The final model has lower RMSE and higher
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") than our
initial guess. The change is significantly different from zero (not
surprisingly, given the small standard error and high statistical power)
and is also consistent with the assumption that the difference is large
(failed to reject
![\\Delta R^2 > 0.02](https://latex.codecogs.com/png.latex?%5CDelta%20R%5E2%20%3E%200.02 "\Delta R^2 > 0.02")
in the TOST procedure for two models on the same training set).

``` r
set.seed(6)
sample_train_initial <- d_norm.aug %>% mutate(pred = initial.pred)
sample_train_final <- d_norm.aug %>% mutate(pred = final.pred)
Rsq.boot <- boot.stats(sample_train_final, sample_train_initial, Rsq)
TOSTtwo.raw(
  n1 = Rsq.boot$n1, m1 = Rsq.boot$m1, sd1 = Rsq.boot$sd1, low_eqbound = -0.02, 
  n2 = Rsq.boot$n2, m2 = Rsq.boot$m2, sd2 = Rsq.boot$sd2, high_eqbound = 0.02,
  verbose = FALSE, plot = TRUE
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-31-1.png" width="90%" style="display: block; margin: auto;" />

------------------------------------------------------------------------

## Section 4.3 Final Model Evaluation

What are some strengths and weaknesses of your model?

------------------------------------------------------------------------

The linear model is relatively simple, and it is quite easy to
understand how various predictors affect the expected price. From the
prediction plot above, one can see that the model generalizes rather
well on previously unseen data, explaining about 93% of variation in
response. Its results are more-or-less reliable across almost the whole
price range.

However, one can see that predictions become slightly more volatile for
low prices (higher variance). Possibly, some other aspects (e.g.,
`Neighborhood`), that add little value in general, start playing higher
role in this regime. The normal error assumptions also does not seem to
be fully justified, because of heavier tails (for almost any model).

------------------------------------------------------------------------

## Section 4.4 Final Model Validation

Testing your final model on a separate, validation data set is a great
way to determine how your model will perform in real-life practice.

You will use the “ames_validation” dataset to do some additional
assessment of your final model. Discuss your findings, be sure to
mention: \* What is the RMSE of your final model when applied to the
validation data?  
\* How does this value compare to that of the training data and/or
testing data? \* What percentage of the 95% predictive confidence (or
credible) intervals contain the true price of the house in the
validation data set?  
\* From this result, does your final model properly reflect uncertainty?

``` r
load("data/ames_validation.Rdata")
```

------------------------------------------------------------------------

Prepare the validation data set, filtering observations with NA and “new
levels” (absent from the training sample).

``` r
d_val <- ames_validation %>% prepare_data(
  normal_sale = TRUE, to_factor = "MS.SubClass", remove = c("PID", imbalanced)
) %>% handle_na(
  filter_rows = "Bsmt.Full.Bath",  # 1 row removed
  omit_cols = c("log_Lot.Frontage" , "Garage.Yr.Blt" ,"BsmtFin.Type.2", "Electrical")
) %>% omit_unused( final.lm )  # 1 new level: MS.Zoning == "A (agr)"
cat("Number of observations:", nrow(ames_validation), 
    "\nHow many left:", nrow(d_val), "\nAll NA removed:", ! any(is.na(d_val)))
```

    ## Number of observations: 763 
    ## How many left: 761 
    ## All NA removed: TRUE

About 93% of the 95% prediction intervals contain the true price of the
house in the validation set, slightly less than expected, but still
reflects uncertainty well.

``` r
pred.HPM <- predict(final.bas, newdata = d_val, estimator = "HPM", se.fit = TRUE, nsim = 10 ^ 6)
d_val.aug <- cbind(d_val, cbind(exp(confint(pred.HPM, parm = "pred")))) %>% 
  mutate(resid = log(price) - log(pred))
cover_prob <- summarize(d_val.aug, mean(price > `2.5%` & price < `97.5%`))[1,1]
ggplot(d_val.aug, aes(x = log(price), y = log(pred))) + 
  geom_ribbon(aes(ymin = log(`2.5%`), ymax = log(`97.5%`)), fill = "grey70", alpha = 0.7) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "blue") +
  geom_point(shape = 16, size = 2, alpha = 0.7) +
  annotate("text", x = 12, y = 13, size = 5, label = paste("Coverage Probability:", round(cover_prob, 3))) +
  labs(y = "Prediction", title = "Final Model Performance on the Validation Data")
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/final_model_validation-1.png" width="90%" style="display: block; margin: auto;" />

Two houses have been somewhat overvalued by our final model:

``` r
EnvStats::rosnerTest( d_val.aug$resid )$all.stats
```

    ##   i       Mean.i       SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
    ## 1 0 0.0007954425 0.10197502 -0.4890327     336 4.803413   3.971776    TRUE
    ## 2 1 0.0014399533 0.10047922 -0.4126907     158 4.121555   3.971443    TRUE
    ## 3 2 0.0019855799 0.09941246  0.3855972     197 3.858789   3.971110   FALSE

``` r
d_val.aug %>% tibble::rownames_to_column(var = "idx") %>% arrange(desc(resid ^ 2)) %>% head(3) %>% 
  kable() %>% kable_styling("striped") %>% scroll_box(width = "100%")
```

<div
style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
idx
</th>
<th style="text-align:right;">
price
</th>
<th style="text-align:left;">
MS.SubClass
</th>
<th style="text-align:left;">
MS.Zoning
</th>
<th style="text-align:left;">
Alley
</th>
<th style="text-align:left;">
Lot.Shape
</th>
<th style="text-align:left;">
Land.Contour
</th>
<th style="text-align:left;">
Lot.Config
</th>
<th style="text-align:left;">
Land.Slope
</th>
<th style="text-align:left;">
Neighborhood
</th>
<th style="text-align:left;">
Condition.1
</th>
<th style="text-align:left;">
Condition.2
</th>
<th style="text-align:left;">
Bldg.Type
</th>
<th style="text-align:left;">
House.Style
</th>
<th style="text-align:right;">
Overall.Qual
</th>
<th style="text-align:right;">
Overall.Cond
</th>
<th style="text-align:right;">
Year.Built
</th>
<th style="text-align:right;">
Year.Remod.Add
</th>
<th style="text-align:left;">
Roof.Style
</th>
<th style="text-align:left;">
Exterior.1st
</th>
<th style="text-align:left;">
Exterior.2nd
</th>
<th style="text-align:left;">
Mas.Vnr.Type
</th>
<th style="text-align:left;">
Exter.Qual
</th>
<th style="text-align:left;">
Exter.Cond
</th>
<th style="text-align:left;">
Foundation
</th>
<th style="text-align:left;">
Bsmt.Qual
</th>
<th style="text-align:left;">
Bsmt.Cond
</th>
<th style="text-align:left;">
Bsmt.Exposure
</th>
<th style="text-align:left;">
BsmtFin.Type.1
</th>
<th style="text-align:left;">
Heating.QC
</th>
<th style="text-align:left;">
Central.Air
</th>
<th style="text-align:right;">
Bsmt.Full.Bath
</th>
<th style="text-align:right;">
Full.Bath
</th>
<th style="text-align:right;">
Half.Bath
</th>
<th style="text-align:right;">
Bedroom.AbvGr
</th>
<th style="text-align:right;">
Kitchen.AbvGr
</th>
<th style="text-align:left;">
Kitchen.Qual
</th>
<th style="text-align:right;">
TotRms.AbvGrd
</th>
<th style="text-align:left;">
Functional
</th>
<th style="text-align:right;">
Fireplaces
</th>
<th style="text-align:left;">
Fireplace.Qu
</th>
<th style="text-align:left;">
Garage.Type
</th>
<th style="text-align:left;">
Garage.Finish
</th>
<th style="text-align:right;">
Garage.Cars
</th>
<th style="text-align:left;">
Garage.Qual
</th>
<th style="text-align:left;">
Garage.Cond
</th>
<th style="text-align:left;">
Paved.Drive
</th>
<th style="text-align:left;">
Fence
</th>
<th style="text-align:right;">
Mo.Sold
</th>
<th style="text-align:right;">
Yr.Sold
</th>
<th style="text-align:left;">
Sale.Type
</th>
<th style="text-align:right;">
log_area
</th>
<th style="text-align:right;">
log_X1st.Flr.SF
</th>
<th style="text-align:right;">
log_X2nd.Flr.SF
</th>
<th style="text-align:right;">
log_Lot.Area
</th>
<th style="text-align:right;">
log_Total.Bsmt.SF
</th>
<th style="text-align:right;">
log_BsmtFin.SF.1
</th>
<th style="text-align:right;">
log_BsmtFin.SF.2
</th>
<th style="text-align:right;">
log_Bsmt.Unf.SF
</th>
<th style="text-align:right;">
log_Open.Porch.SF
</th>
<th style="text-align:right;">
log_Enclosed.Porch
</th>
<th style="text-align:right;">
log_Screen.Porch
</th>
<th style="text-align:right;">
log_Garage.Area
</th>
<th style="text-align:right;">
log_Mas.Vnr.Area
</th>
<th style="text-align:right;">
log_Wood.Deck.SF
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
<th style="text-align:right;">
resid
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
336
</td>
<td style="text-align:right;">
35000
</td>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
RL
</td>
<td style="text-align:left;">
Grvl
</td>
<td style="text-align:left;">
Reg
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Inside
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
Feedr
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
1Fam
</td>
<td style="text-align:left;">
1Story
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1922
</td>
<td style="text-align:right;">
1955
</td>
<td style="text-align:left;">
Gable
</td>
<td style="text-align:left;">
MetalSd
</td>
<td style="text-align:left;">
MetalSd
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
BrkTil
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
N
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
<td style="text-align:left;">
TA
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Typ
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Detchd
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
ConLD
</td>
<td style="text-align:right;">
6.212606
</td>
<td style="text-align:right;">
6.212606
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
8.998260
</td>
<td style="text-align:right;">
6.212606
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.212606
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
4.615120
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.379897
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
46864.40
</td>
<td style="text-align:right;">
69512.26
</td>
<td style="text-align:right;">
57075.83
</td>
<td style="text-align:right;">
-0.4890327
</td>
</tr>
<tr>
<td style="text-align:left;">
158
</td>
<td style="text-align:right;">
110000
</td>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
RL
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Reg
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Corner
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
NAmes
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
1Fam
</td>
<td style="text-align:left;">
1Story
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1968
</td>
<td style="text-align:right;">
1968
</td>
<td style="text-align:left;">
Hip
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
BrkFace
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
CBlock
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:left;">
Gd
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Maj2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Attchd
</td>
<td style="text-align:left;">
RFn
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:left;">
WD
</td>
<td style="text-align:right;">
7.239215
</td>
<td style="text-align:right;">
7.239215
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
9.514953
</td>
<td style="text-align:right;">
7.239215
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7.239215
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
5.484797
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.357842
</td>
<td style="text-align:right;">
5.379897
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
137609.29
</td>
<td style="text-align:right;">
200722.56
</td>
<td style="text-align:right;">
166196.54
</td>
<td style="text-align:right;">
-0.4126907
</td>
</tr>
<tr>
<td style="text-align:left;">
197
</td>
<td style="text-align:right;">
89000
</td>
<td style="text-align:left;">
70
</td>
<td style="text-align:left;">
C (all)
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Reg
</td>
<td style="text-align:left;">
Lvl
</td>
<td style="text-align:left;">
Corner
</td>
<td style="text-align:left;">
Gtl
</td>
<td style="text-align:left;">
IDOTRR
</td>
<td style="text-align:left;">
Feedr
</td>
<td style="text-align:left;">
Norm
</td>
<td style="text-align:left;">
1Fam
</td>
<td style="text-align:left;">
2Story
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1895
</td>
<td style="text-align:right;">
1950
</td>
<td style="text-align:left;">
Gable
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
Wd Sdng
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Stone
</td>
<td style="text-align:left;">
Po
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:left;">
Ex
</td>
<td style="text-align:left;">
N
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
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Typ
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Detchd
</td>
<td style="text-align:left;">
Unf
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Fa
</td>
<td style="text-align:left;">
TA
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
WD
</td>
<td style="text-align:right;">
7.216710
</td>
<td style="text-align:right;">
6.716595
</td>
<td style="text-align:right;">
6.285998
</td>
<td style="text-align:right;">
8.571871
</td>
<td style="text-align:right;">
5.159055
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.159055
</td>
<td style="text-align:right;">
4.820282
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.225747
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
48926.27
</td>
<td style="text-align:right;">
74870.78
</td>
<td style="text-align:right;">
60523.95
</td>
<td style="text-align:right;">
0.3855972
</td>
</tr>
</tbody>
</table>

</div>

The accuracy of prediction, as measured by RMSE or
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2"), is also a
little bit lower than previously calculated using test or train data:

``` r
d_val.aug %>% summarize(
  RMSE = sqrt( mean( (price - pred) ^ 2 ) ), 
  Rsq = 1 - RMSE ^ 2 / mean( (price - mean(price)) ^ 2 )
) %>% kable(digits = 4) %>% kable_styling("striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
Rsq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
18432.88
</td>
<td style="text-align:right;">
0.9247
</td>
</tr>
</tbody>
</table>

Should we be alarmed by this difference in performance? For instance, if
one takes the test RMSE as an estimate of the “true” error rate, the
results are well within the natural sampling variability (`|dm| < sd2`,
so that one is likely to pass the equivalence test with a very mild
![\\Delta](https://latex.codecogs.com/png.latex?%5CDelta "\Delta")).

``` r
set.seed(6)
sample_test_final <- d_test.aug %>% mutate(pred = final.pred)
RMSE.boot <- boot.stats(d_val.aug, sample_test_final, RMSE)
as.data.frame(RMSE.boot) %>% kable(digits = 4) %>% kable_styling("striped") 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
stat
</th>
<th style="text-align:right;">
n1
</th>
<th style="text-align:right;">
n2
</th>
<th style="text-align:right;">
m1
</th>
<th style="text-align:right;">
m2
</th>
<th style="text-align:right;">
dm
</th>
<th style="text-align:right;">
d
</th>
<th style="text-align:right;">
sd1
</th>
<th style="text-align:right;">
sd2
</th>
<th style="text-align:right;">
bias1
</th>
<th style="text-align:right;">
bias2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
RMSE
</td>
<td style="text-align:right;">
761
</td>
<td style="text-align:right;">
816
</td>
<td style="text-align:right;">
18401.1
</td>
<td style="text-align:right;">
18952.76
</td>
<td style="text-align:right;">
-551.6625
</td>
<td style="text-align:right;">
-0.5971
</td>
<td style="text-align:right;">
1093.2
</td>
<td style="text-align:right;">
731.4612
</td>
<td style="text-align:right;">
-31.7815
</td>
<td style="text-align:right;">
-11.3554
</td>
</tr>
</tbody>
</table>

On the other hand, the difference between validation and training set
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") scores is
expectedly larger, but is still reliably within our conjectured 2%
bound.

``` r
set.seed(6)
Rsq.boot <- boot.stats(d_val.aug, sample_train_final, Rsq)
TOSTtwo.raw(
  n1 = Rsq.boot$n1, m1 = Rsq.boot$m1, sd1 = Rsq.boot$sd1, low_eqbound = -0.02, 
  n2 = Rsq.boot$n2, m2 = Rsq.boot$m2, sd2 = Rsq.boot$sd2, high_eqbound = 0.02,
  verbose = FALSE, plot = TRUE
)
```

<img src="https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project5_capstone_files/figure-gfm/unnamed-chunk-36-1.png" width="90%" style="display: block; margin: auto;" />

------------------------------------------------------------------------

# Part 5 Conclusion

Provide a brief summary of your results, and a brief discussion of what
you have learned about the data and your model.

------------------------------------------------------------------------

I have fitted two models, predicting house prices for the normal
condition of sale. The final model with 20 variables (including 5
two-way interactions) provides only a slight improvement over the
initial one with just 10 predictors, which is already a quite accurate
fit
(![R^2\_\\text{train} = 0.92](https://latex.codecogs.com/png.latex?R%5E2_%5Ctext%7Btrain%7D%20%3D%200.92 "R^2_\text{train} = 0.92")).
All coefficients turn out to be statistically significant (even if only
as interaction terms). I have found that prediction errors are somewhat
over-dispersed, having heavier tails than the normal distribution
assumption, however, in the absence of highly influential outliers, the
OLS linear regression is our best bet.

The *model comparison*, based on the *relative* differences in a chosen
measure of out-of-sample performance (such as various information
criteria, or cross-validation average error rate), provides advice about
how confident we might be about the models (conditional on the set of
models compared). Using either the raw RMSE (in dollars), or
![R^2](https://latex.codecogs.com/png.latex?R%5E2 "R^2") percentage of
variance explained, the model generalizes reasonably well on previously
unseen data. (Of course, the expert-knowledge of the market and the goal
is required in order to judge the desired accuracy.)

Being a random variable itself, the given statistic is accompanied by
sampling variability. To assess the uncertainty around point estimates,
I have used the *bootstrap* resampling techniques. Instead of the usual
“strict zero” NHST, I have adopted the lesser-known *equivalence
testing* procedure to discard the hypothesis of train/test drop in
performance
![\\Delta R^2 > 0.02](https://latex.codecogs.com/png.latex?%5CDelta%20R%5E2%20%3E%200.02 "\Delta R^2 > 0.02").
I am not aware of other previous uses of equivalence tests to gauge the
generalization gap in a model selection context, as above, but my
knowledge of this topic may be limited though.

------------------------------------------------------------------------
