Extramarital Sex Gender Difference in GSS Data
================
Vadim Belov

-   [Setup](#setup)
    -   [Task](#task)
    -   [Load packages](#load-packages)
    -   [Load data](#load-data)
-   [Part 1: Data](#part-1-data)
-   [Part 2: Research question](#part-2-research-question)
-   [Part 3: Exploratory data
    analysis](#part-3-exploratory-data-analysis)
-   [Part 4: Inference](#part-4-inference)

## Setup

### Task

Describe data, collection method and the scope of inference. Identify a
research question, perform EDA and statistical inference via hypothesis
testing and/or confidence interval (cf. [project
requirements](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info2_stat_inf.md)).

### Load packages

``` r
library(ggplot2)
library(dplyr)    # for data transformation
library(tidyr)    # for data cleaning
library(janitor)  # for data exploration, create "tidy" tables
```

### Load data

``` r
load("data/gss.Rdata")
```

------------------------------------------------------------------------

## Part 1: Data

**Overview and data collection**: General Social Survey (GSS) is a
regular, ongoing interview survey of U.S households conducted by the
National Opinion Research Center (NORC). The mission of the GSS is to
make timely, high-quality, scientifically relevant data available to
social science researchers. The vast majority of GSS data is obtained in
face-to-face interviews. Under some conditions when it has proved
difficult to arrange an in-person interview with a sampled respondent,
GSS interviews may be conducted by telephone. One can find further
details and methodology on the
[website](http://gss.norc.org/About-The-GSS) (see also the brief
[summary](https://www.nsf.gov/pubs/2007/nsf0748/nsf0748_3.pdf)).

**Target population and sample sizes**: The target population of the GSS
is adults (18+) living in households in the United States, speaking
English or Spanish. The target sample size for the annual surveys was
1500; actual sample sizes ranged between 1372 (1990) and 1613 (1972).

**Generalizability**: the results of analysis, derived from this data,
can be generalized to the population of interest. This is guaranteed by
the design of the study:

> surveys were completed using a full-probability sample design,
> producing a high-quality, representative sample of the adult
> population of the U.S.

Of course, even when *random sampling* is being used, such as in this
survey, caution must be exercised if the non-response rate is high, and
for possible other sources of biases. We can use the data on [response
rates](http://gss.norc.org/Documents/other/Response%20rates.pdf) make a
quick estimate:

    ## [1] "Average response rate 0.75"

**Causality**: From this brief description one can conclude that this
survey is purely *observational* in nature. Since no random assignment
has been employed, this study cannot be used for causal inferences.
Strictly speaking, one can only identify the possible association
between variables (but not necessarily the causal dependencies), using
variation in the collected data samples.

------------------------------------------------------------------------

## Part 2: Research question

The section of Controversial Social Issues apparently contains topics on
which people may have polarized or largely opinionated views. For
instance, while adultery is no longer a deal breaker in many marriages,
infidelity is one of the top cited reasons couples decide to get
divorced. According to the American Psychological Association (APA),
\[infidelity in the United States accounted for 20-40 percent of
divorces. It is therefore important to analyze the tolerance to sex with
person other than spouse (`xmarsex`):

> What is your opinion about a married person having sexual relations
> with someone other than the marriage partner?

In particular, it is an interesting question in its own right *whether
the perception of extramarital affairs differ between men and women, and
if so, in which way exactly?* Accordingly, lets state the null
hypothesis that assumes an independence on respondent’s gender (`sex`):

-   ![H_0](https://latex.codecogs.com/png.latex?H_0 "H_0"): The opinion
    of men and women is not different on the topic of sex with person
    other than spouse.  
-   ![H_A](https://latex.codecogs.com/png.latex?H_A "H_A"): There is an
    association of response with respondent’s gender.

This assumption is consistent with the position of complete ignorance.
One can imagine as we had no preconceptions on the question (i.e.,
ignoring all conceivable mechanisms for a possible correlation, such as
societal context or biological differences).

------------------------------------------------------------------------

## Part 3: Exploratory data analysis

First create the data frame with our variables of interest, omitting the
missing values.

``` r
gss_x <- gss %>%
  select(xmarsex, sex) %>% 
  drop_na()
str(gss_x)
```

    ## 'data.frame':    34019 obs. of  2 variables:
    ##  $ xmarsex: Factor w/ 5 levels "Always Wrong",..: 1 1 3 1 1 1 1 3 1 1 ...
    ##  $ sex    : Factor w/ 2 levels "Male","Female": 1 2 2 1 2 2 2 1 2 1 ...

In order to check for (in)dependence between two factors with more than
two levels, we perform a
![\\chi^2](https://latex.codecogs.com/png.latex?%5Cchi%5E2 "\chi^2")-test
(chi-square), that compares observed and expected number of frequency
counts. Lets calculate the summary statistics of interest and verify the
conditions for a test.

Grouping by appropriate variables, we calculate observed counts
![O\_{ij}](https://latex.codecogs.com/png.latex?O_%7Bij%7D "O_{ij}") for
various combinations of outcomes, as well as marginal sums and expected
cell counts in a two-way table (using the formula
![E\_{ij} = \\frac{\\text{row} \\ i \\ \\text{total} \\times \\text{column} \\ i \\ \\text{total}}{\\text{table total}}](https://latex.codecogs.com/png.latex?E_%7Bij%7D%20%3D%20%5Cfrac%7B%5Ctext%7Brow%7D%20%5C%20i%20%5C%20%5Ctext%7Btotal%7D%20%5Ctimes%20%5Ctext%7Bcolumn%7D%20%5C%20i%20%5C%20%5Ctext%7Btotal%7D%7D%7B%5Ctext%7Btable%20total%7D%7D "E_{ij} = \frac{\text{row} \ i \ \text{total} \times \text{column} \ i \ \text{total}}{\text{table total}}")).
One of the categories in `xmarsex` (“Other”) has no entries and can be
safely ignored.

``` r
d <- gss_x %>% 
  group_by(sex, xmarsex) %>% summarize(n = n(), .groups = "drop") %>% 
  group_by(sex) %>% mutate(n_sex = sum(n)) %>% 
  group_by(xmarsex) %>% mutate(n_x = sum(n)) %>% 
  ungroup() %>% mutate(n_exp = n_sex * n_x / sum(n))
d %>% 
  select(-c("n_sex", "n_x"))
```

    ## # A tibble: 8 x 4
    ##   sex    xmarsex              n  n_exp
    ##   <fct>  <fct>            <int>  <dbl>
    ## 1 Male   Always Wrong     10856 11428.
    ## 2 Male   Almst Always Wrg  2246  2019.
    ## 3 Male   Sometimes Wrong   1398  1169.
    ## 4 Male   Not Wrong At All   494   378.
    ## 5 Female Always Wrong     15073 14501.
    ## 6 Female Almst Always Wrg  2335  2562.
    ## 7 Female Sometimes Wrong   1254  1483.
    ## 8 Female Not Wrong At All   363   479.

The appropriate way to organize the result is a contingency table. Lets
pretty print this, including also total number of counts for rows and
columns (our marginal sums), as well as (column) percentages.

``` r
d %>% select(xmarsex, sex, n) %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  tibble::column_to_rownames(var = "xmarsex") %>% 
  knitr::kable()
```

|                  | Male         | Female       | Total        |
|:-----------------|:-------------|:-------------|:-------------|
| Always Wrong     | 72% (10856)  | 79% (15073)  | 76% (25929)  |
| Almst Always Wrg | 15% (2246)   | 12% (2335)   | 13% (4581)   |
| Sometimes Wrong  | 9% (1398)    | 7% (1254)    | 8% (2652)    |
| Not Wrong At All | 3% (494)     | 2% (363)     | 3% (857)     |
| Total            | 100% (14994) | 100% (19025) | 100% (34019) |

It is easier to visualize this table, using the standardized stacked bar
plot.

``` r
ggplot(data = gss_x, aes(x = sex, fill = xmarsex)) + 
  geom_bar(position = "fill") +
  labs(fill = "Sex with person other than spouse", 
       x = "Gender", 
       y = NULL,
       title = "How men and women perceive adultery?")
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project2_stat_inf_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

From the table and the plot we can observe that more women than men
perceive extramarital sex as “Always or Almost Always Wrong”, while more
men than women just consider it “Sometimes Wrong or Not Wrong At All”.
That is, men are more tolerant to this issue than women, in general. But
are the differences significant enough to constitute strong evidence
against ![H_0](https://latex.codecogs.com/png.latex?H_0 "H_0")? (Before
jumping to conclusions, one should note that the question is phrased in
such a way that it can be equally applicable both to respondent and
partner as well.)

There are two conditions that must be checked when performing a
![\\chi^2](https://latex.codecogs.com/png.latex?%5Cchi%5E2 "\chi^2")-test:

-   **Independence.** Each case that contributes a count to the table
    must be independent of all the other cases. The data in the survey
    is a random sample from less than 10% of the population, the counts
    are unrelated, and so independence between observations is
    reasonable.

-   **Sample size.** Each particular scenario (i.e. cell count) must
    have at least 5 expected cases, which is naturally satisfied.

We can continue with our analysis and perform the test now and compare
proportions across several groups. There is no defined parameter of
interest and no associated confidence interval in the context of
multiple level categorical variables (more than 2), so we cannot compare
two methods in this case (but we will double check our results using
programmed solution).

------------------------------------------------------------------------

## Part 4: Inference

We can easily calculate statistic for a
![\\chi^2](https://latex.codecogs.com/png.latex?%5Cchi%5E2 "\chi^2")-test,
using the formula
![\\displaystyle{X^2 = \\sum\_{ij} \\frac{(O\_{ij} - E\_{ij})^2}{E\_{ij}}}](https://latex.codecogs.com/png.latex?%5Cdisplaystyle%7BX%5E2%20%3D%20%5Csum_%7Bij%7D%20%5Cfrac%7B%28O_%7Bij%7D%20-%20E_%7Bij%7D%29%5E2%7D%7BE_%7Bij%7D%7D%7D "\displaystyle{X^2 = \sum_{ij} \frac{(O_{ij} - E_{ij})^2}{E_{ij}}}").

``` r
( X2 <- sum( (d$n - d$n_exp) ^ 2 / d$n_exp ) )
```

    ## [1] 241.1526

The distribution of
![X^2](https://latex.codecogs.com/png.latex?X%5E2 "X^2") is strictly
positive, and hence the test is one-sided. The number of degrees of
freedom is 3, given by the formula
![df = (R - 1)\\times (C - 1)](https://latex.codecogs.com/png.latex?df%20%3D%20%28R%20-%201%29%5Ctimes%20%28C%20-%201%29 "df = (R - 1)\times (C - 1)"),
where ![R](https://latex.codecogs.com/png.latex?R "R") and
![C](https://latex.codecogs.com/png.latex?C "C") are number of rows and
columns in a two-way table, respectively. The p-value is the tail area
under the curve that represents probability of obtaining result as
extreme as the observed test statistic, assuming
![H_0](https://latex.codecogs.com/png.latex?H_0 "H_0") is correct. We
can choose set the significance level to the typical value
![\\alpha = 0.05](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%200.05 "\alpha = 0.05"),
which is the required threshold to accept/reject the null or alternative
hypothesis, based on data.

``` r
( p_hat <- pchisq(X2, df = 3, lower.tail = FALSE) )
```

    ## [1] 5.361003e-52

**Concclusion:** p-value is extremely small, so that we can reject the
null hypothesis ![H_0](https://latex.codecogs.com/png.latex?H_0 "H_0").
That is, the data provide convincing evidence that men and women have
significantly different viewpoints on sex with person other than spouse.
In particular, men express greater tolerance to this issue.

Although tail area is too small to see, we can still draw the
distribution, where we specify the position of observed test statistic
in red.

``` r
xy <- data.frame(x = seq(0, 250, length.out = 500)) %>% 
  mutate(y = dchisq(x, df = 3))
ggplot(xy, aes(x, y)) + 
  geom_area(fill = "sky blue") + 
  annotate("rect", fill = "red", alpha = .3,
           xmin = X2, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = X2, color = "red", size = 1.5) + 
  ggtitle("Chi-square distribution (df = 3) and test statistic")
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project2_stat_inf_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

We explicitly performed “from scratch” all calculations, required for
the test, for clarity of exposition. The process can be streamlined,
using in-built functions that do the necessary job “under the hood”:

``` r
table(gss$xmarsex, gss$sex)
```

    ##                   
    ##                     Male Female
    ##   Always Wrong     10856  15073
    ##   Almst Always Wrg  2246   2335
    ##   Sometimes Wrong   1398   1254
    ##   Not Wrong At All   494    363
    ##   Other                0      0

``` r
chisq.test(gss$xmarsex, gss$sex)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  gss$xmarsex and gss$sex
    ## X-squared = 241.15, df = 3, p-value < 2.2e-16

We can see the agreement with our detailed solution. There are many
possible factors that can influence the difference between men and women
on this particular issue. Probably, the societal context and specific
details of upbringing are the most prevailing candidates for further
exploration. One can therefore look for explanatory factors, lying in
that area (possibly comparing different countries as well). Without
clearly stated causal mechanism that induces the observed association,
there is not much one can say from the data alone.
