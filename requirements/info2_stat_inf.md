# Course 2 Project: Inferential Statistics

Identify a research question similar to questions we’ve talked about in this course. Use the General Social Survey (GSS) dataset (provided below).

All analysis must be completed using the R programming language via RStudio, and your write up must be an R Markdown document. To help you get started we provide a template Rmd file below (see Rmd template in the Required files section below). Download this file, and fill in each section.

## Instructions 

Your project will consist of 4 parts:

1. **Data:** (3 points) Describe how the observations in the sample are collected, and the implications of this data collection method on the scope of inference (generalizability / causality). Note that you might will need to look into documentation on the GSS to answer this question. See [http://gss.norc.org/](http://gss.norc.org/) as well as the "More information on the data" section below.

2. **Research question:** (3 points) Come up with a research question that you want to answer using these data. You should phrase your research question in a way that matches up with the scope of inference your dataset allows for. You are welcomed to create new variables based on existing ones. Along with your research question include a brief discussion (1-2 sentences) as to why this question is of interest to you and/or your audience.

3. **EDA:** (10 points) Perform exploratory data analysis (EDA) that addresses the research question you outlined above. Your EDA should contain numerical summaries and visualizations. Each R output and plot should be accompanied by a brief interpretation.

4. **Inference:** (28 points) Perform inference that addresses the research question you outlined above. Each R output and plot should be accompanied by a brief interpretation.

In addition to these parts, there are also 6 points allocated to format, overall organization, and readability of your project. Total points add up to 50 points. See the [assessment rubric](https://d3c33hcgiwev3.cloudfront.net/_c130de4ce4e6675cb5488a51e07c99bd_stat_inf_project_rubric.html?Expires=1642032000&Signature=UmQzYEVHq07D-CpnbK12m4BGY7ZwMUf~zKi8OH8w0c3LAZ2-TcLj7NnrB9WW1eP3KpTOezkFMeK2gJ4h74pLvhQAu-BvkDova7E2YoRgSry0hjR9zzIWmPykBouIDX7qZHSRtxl083C-iqOKOMK1QWQJp6X5Ni1sXnSFMyz5eSw_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A) for more details on how your peers will evaluate your work.


### More information on inference

**INFERENCE:** Statistical inference via hypothesis testing and/or confidence interval.

- State hypotheses

- Check conditions

- State the method(s) to be used and why and how

- Perform inference

- Interpret results

- If applicable, state whether results from various methods agree

It is your responsibility to figure out the appropriate methodology. What techniques you use to conduct inference will depend on the type of data you're using, and your sample size. All of you should conduct at least a hypothesis test, and report the associated p-value and the conclusion. Those of you comparing two means, two medians, or two proportions should also calculate a confidence interval for the parameter of interest. Those of you working with categorical variables with more than two levels will need to use methods like ANOVA and chi-square testing for which there is no associated confidence interval, and that's ok. If your data fails some conditions and you can't use a theoretical method, then you should use an appropriate simulation based method.

- If you can use both theoretical and simulation based methods, then choose one and stick with it. You don't have to do both. However if you can't use both, then you need to decide which is appropriate.

- If you can do both a hypothesis test and a confidence interval, do both, and comment on agreement of the results from the two methods. However if your variables do not lend themselves to a confidence interval, that's ok.

- It's essential to make sure the method you're using is appropriate for the dataset and the research question you're working with.

### More information on the data

Since 1972, the General Social Survey (GSS) has been monitoring societal change and studying the growing complexity of American society. The GSS aims to gather data on contemporary American society in order to monitor and explain trends and constants in attitudes, behaviors, and attributes; to examine the structure and functioning of society in general as well as the role played by relevant subgroups; to compare the United States to other societies in order to place American society in comparative perspective and develop cross-national models of human society; and to make high-quality data easily accessible to scholars, students, policy makers, and others, with minimal cost and waiting.

GSS questions cover a diverse range of issues including national spending priorities, marijuana use, crime and punishment, race relations, quality of life, confidence in institutions, and sexual behavior.
