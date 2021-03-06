EDA: 3 Questions to BRFSS Data
================
Vadim Belov

-   [Setup](#setup)
    -   [Task](#task)
    -   [Load packages](#load-packages)
    -   [Load data](#load-data)
-   [Part 1: Data](#part-1-data)
-   [Part 2: Research questions](#part-2-research-questions)
-   [Part 3: Exploratory data
    analysis](#part-3-exploratory-data-analysis)
    -   [Research question 1](#research-question-1)
    -   [Research question 2](#research-question-2)
    -   [Research question 3](#research-question-3)

## Setup

### Task

Describe data, collection method and the scope of inference. Come up
with at least 3 research questions and perform simple exploratory data
analysis (EDA) on each of them, containing numerical summaries and
visualizations (cf. [project
requirements](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/requirements/info1_intro_data_prob.md)).

### Load packages

``` r
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmosaic)  # for mosaic plots with ggplot
library(urbnmapr)  # for choropleth/heat maps of USA, from UrbanInstitute
```

### Load data

``` r
# load("data/brfss2013.RData")
# merge partition to obtain the original dataset (identical)
files <- sprintf("data/brfss2013_%d.RData", 1:2)
subsets <- sapply(files, function(x) mget(load(x)), simplify = TRUE)
brfss2013 <- do.call(bind_rows, subsets)
```

------------------------------------------------------------------------

## Part 1: Data

The Behavioral Risk Factor Surveillance System (BRFSS) is an annual
telephone survey designed to identify risk factors in the adult
population and report emerging health trends.

The full details of how the survey is conducted can be found on the
[official website](https://www.cdc.gov/brfss/about/brfss_faq.htm). Here
is the short excerpt:

> With technical and methodological assistance from CDC, state health
> departments use in-house interviewers or contract with telephone call
> centers or universities to administer the BRFSS surveys continuously
> through the year. The states use a standardized core questionnaire,
> optional modules, and state-added questions. The survey is conducted
> using *Random Digit Dialing (RDD)* techniques on both landlines and
> cell phones.

**Causality**: From this brief description one can conclude that this
survey is purely *observational* in nature. Since no random assignment
has been employed, this study cannot be used for causal inferences.
Strictly speaking, one can only identify the possible association
between variables (but not the causal dependencies), using variation in
the collected data samples.

**Generalizability**: RDD techniques, mentioned above, means *random
sampling* has been used. Therefore the results of analysis, derived from
this data, can be generalized to the population of interest. The latter
consists of the non-institutionalized adults (18 years of age and
older), residing in the US. The participating areas include all 50
states, the District of Columbia, Guam, and the Commonwealth of Puerto
Rico.

Of course, even when people are picked at random, such as in this
surveys, caution must be exercised if the non-response rate is high, and
for possible other sources of biases.

------------------------------------------------------------------------

## Part 2: Research questions

**Research question 1:**

The United States has been relying upon private health insurance to
provide much of the financing for medical costs, compared to most West
European countries (and Canada), where costs are paid for by a system of
compulsory “social insurance”. In USA it has been voluntary to buy
insurance, and premiums were linked to risk of a claim, rather than
income, making the functioning of such market-based system “sub-optimal”
at best. Lets see how much prohibitive can be the access to the health
care, for instance: *Evaluate the chances that the health coverage
(`hlthpln1`) will pay the necessary medical costs (`medcost`). How does
this estimate vary with the income level (`income2`)?*

**Research question 2:**

The problem of the excessive obesity is prominent among the US
population. It is intersting to inspect the geographic distribution of
it across the country. *What is the pattern of variation in obesity
rates (`_rfbmi5`) among different states (`_state`)? For instance, what
are the most and the least “obese” states, according to the survey?*

**Research question 3:**

The person’s health and well-being is influenced by the variety of
health practices and risk behaviors. Among other things, it is curious
to see *how the self-assessed level of general “life satisfaction”
(`lsatisfy`) can differ, depending on the marital status (`marital`).*
One can suspect that the latter could vary among males and female, and
it is natural *to check if the found pattern changes for two genders
(`sex`).*

**Scope of inferences:**

The first two questions explore variables from the core component of the
survey, common to all participants. The missing values constitute the
small part of answers, and have been simply omitted in the EDA. Thus,
the findings are generalizable to the population, in principle.

The third question concerns the variable `lsatisfy` from the optional
BRFSS module 22, where the number of respondents is substantially
smaller (about 11,000 of 491775 observations from the entire dataset).
It is therefore unclear, if the findings are representative of the
entire population.

------------------------------------------------------------------------

## Part 3: Exploratory data analysis

### Research question 1

Lets list our 3 variables of interest, two of which concern Health Care
Access (section 3 of the poll):

-   `hlthpln1`: Have Any Health Care Coverage

> Do you have any kind of health care coverage, including health
> insurance, prepaid plans such as HMOs, government plans such as
> Medicare, or Indian Health Service?

-   `medcost`: Could Not See Dr. Because Of Cost

> Was there a time in the past 12 months when you needed to see a doctor
> but could not because of cost?

-   We also include in the analysis the overall level of income
    `income2`.

``` r
health_care_access <- brfss2013 %>% 
  select(have_coverage = hlthpln1, too_costly = medcost, income_level = income2) %>% 
  drop_na()

str(health_care_access)
```

    ## 'data.frame':    418642 obs. of  3 variables:
    ##  $ have_coverage: Factor w/ 2 levels "Yes","No": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ too_costly   : Factor w/ 2 levels "Yes","No": 2 2 2 2 2 2 2 2 2 1 ...
    ##  $ income_level : Factor w/ 8 levels "Less than $10,000",..: 7 8 8 7 6 8 6 8 4 8 ...

The first two variables are binary, encoded as 2-level factors in our
dataframe. One can quickly summarize this data in a frequency counts

``` r
summary(health_care_access)
```

    ##  have_coverage too_costly              income_level   
    ##  Yes:371320    Yes: 52585   $75,000 or more  :115714  
    ##  No : 47322    No :366057   Less than $75,000: 65064  
    ##                             Less than $50,000: 61317  
    ##                             Less than $35,000: 48660  
    ##                             Less than $25,000: 41500  
    ##                             Less than $20,000: 34636  
    ##                             (Other)          : 51751

Both variables are approximately equally divided, having very similar
proportions of Yes/No answers for the health coverage and for the
ability to visit the doctor. Do these fractions represent two different
groups of people, or is there an overlap? In other words, what
proportion of those with health coverage still could not afford seeing
the doctor?

``` r
health_care_access %>% 
  filter(have_coverage == "Yes", too_costly == "Yes") %>% 
  summarise(YY_stat = n() / sum(health_care_access$have_coverage == "Yes"))
```

    ##      YY_stat
    ## 1 0.08246795

This statistic will allow us to roughly estimate the chance of whether
your medical costs can be covered by insurance. (If we had to make a
prediction of paying for your medical costs, based solely on the
`have_coverage`, this fraction would represent `1-precision` of such a
model). It is reasonable to represent the summary visually as a mosaic
plot, with the plotted areas correspond to the overall proportions.

``` r
(mosaic <- ggplot(data = health_care_access) + 
  geom_mosaic(aes(x = product(too_costly, have_coverage), fill = too_costly), show.legend = FALSE) + 
  labs(y = "Could Not See Dr. Because Of Cost", 
       x = "Have Any Health Care Coverage", 
       title = "Health Care Coverage/Access"))
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project1_intro_data_prob_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

This plot essentially depicts a contingency table. The vertical
separation line differ between two groups, suggesting that the two
categorical variables are naturally dependent. This can be additionally
checked, using chi2-test, which gives statistically significant result.

``` r
table(health_care_access$have_coverage, health_care_access$too_costly)
```

    ##      
    ##          Yes     No
    ##   Yes  30622 340698
    ##   No   21963  25359

``` r
chisq.test(health_care_access$have_coverage, health_care_access$too_costly)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  health_care_access$have_coverage and health_care_access$too_costly
    ## X-squared = 55661, df = 1, p-value < 2.2e-16

Evidently, the availability/affordability of health care is likely to be
directly associated with the overall income level, as seen in the
following faceted plot.

``` r
mosaic + facet_grid(~income_level) + theme(strip.text.x = element_text(angle = 90))
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project1_intro_data_prob_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

One can observe that the fraction of expensive edical costs decreases as
the income level increases (but not zero even for the highest income
level).

### Research question 2

The variables of interest are `X_state` and 2-level factor `X_rfbmi5`,
calculated for adults who have a body mass index (mass/height^2) greater
than 25.00, which means *overweight or obese*. We first need to
calculate the obesity index, aggregated on the level of each state,
divided by the overall number of respondenys from that state (to assess
the density).

``` r
obesity_by_state <- brfss2013 %>% 
  select(overweight = X_rfbmi5, state = X_state) %>% 
  filter(!is.na(overweight)) %>% 
  group_by(state) %>% 
  summarize(obesity_rate = sum(overweight == "Yes") / n())
```

We can then find states, which are the most and the least obese,
respecitvely, according to the above statistic:

``` r
obesity_by_state %>% 
  filter(obesity_rate %in% c(max(obesity_rate), min(obesity_rate)))
```

    ## # A tibble: 2 x 2
    ##   state       obesity_rate
    ##   <fct>              <dbl>
    ## 1 Hawaii             0.549
    ## 2 Mississippi        0.700

For the visualization of the distribution of the obesity by states, it
is best to use the choropleth (or heat), where states are colored by the
corresponding value. To deal with the US geospatial data, we employ the
`urbnmapr` package from the Urban Institute.

``` r
# First combine with the states geographic data to create a map
ggplot(data = left_join(obesity_by_state, urbnmapr::states, c("state" = "state_name")), 
       mapping = aes(long, lat, group = group, fill = obesity_rate)) +
  geom_polygon(color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Obesity Rate")
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project1_intro_data_prob_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

States with the highest obesity rate are located centrally
(middle-west?) and more to the south-east, while the darker less obese
states tend to be on the sides (both west and east).

### Research question 3

The variables of interest are `marital`: Marital Status, `sex`:
Respondents Sex and `lsatisfy`: Satisfaction With Life, where the last
one is our suggested response variable.

``` r
satisfaction <- brfss2013 %>% 
  select(lsatisfy, marital, sex) %>% 
  drop_na()

summary(satisfaction)
```

    ##               lsatisfy                               marital         sex      
    ##  Very satisfied   :5371   Married                        :5628   Male  :4074  
    ##  Satisfied        :5503   Divorced                       :1749   Female:7559  
    ##  Dissatisfied     : 598   Widowed                        :2033                
    ##  Very dissatisfied: 161   Separated                      : 376                
    ##                           Never married                  :1729                
    ##                           A member of an unmarried couple: 118

Lets group the current marital status into a binary category of whether
a person (supposedly) have a partner at the moment. We also aggregate 4
levels of satisfaction into 2.

``` r
satisfaction <- satisfaction %>% 
  mutate(in_union = as.factor(ifelse(marital %in% c("Married", "A member of an unmarried couple"), "Yes", "No")),
         satisfied = as.factor(ifelse(lsatisfy %in% c("Very satisfied", "Satisfied"), "Yes", "No")))
```

We can explore the data using the mosaic plot, which we can also facet
on the potential confounding variable. Thus, we have two conditioning of
the satisfaction level - on marital status and sex, respectively.

``` r
ggplot(data = satisfaction) + 
  geom_mosaic(aes(x = product(lsatisfy, sex), fill = lsatisfy), show.legend = FALSE) + 
  labs(y = "Life Satisfaction", 
       x = "In Union", 
       title = "Satisfaction vs. Marriage") +
  facet_grid(~in_union)
```

![](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project-files/project1_intro_data_prob_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

By simply visually inspecting the plot, we can observe the clear pattern
that the fraction of married people is higher among those who are more
satisfied with their lives, i.e. the positive correlation. At the same
time, this dependence is indistinguishably the same for both male and
female (“they are equally happy in marriage”, so to say).

We can confirm our findings and formally check for dependence by
calculating the chi2-statistic and p-value, as before:

``` r
table(satisfaction$in_union, satisfaction$satisfied)
```

    ##      
    ##         No  Yes
    ##   No   531 5356
    ##   Yes  228 5518

``` r
chisq.test(satisfaction$in_union, satisfaction$satisfied)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  satisfaction$in_union and satisfaction$satisfied
    ## X-squared = 120.86, df = 1, p-value < 2.2e-16
