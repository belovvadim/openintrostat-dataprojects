# openintrostat-dataprojects

This repo contains 5 open-ended data analysis projects for [Statistics with R Specialization](https://www.coursera.org/specializations/statistics) on Coursera, written in RMarkdown. The `requirements` are bound to course contents, but otherwise they encourage creativity in data exploration and modeling to show mastery of taught concepts and tools.

1. Introduction to Probability and Data with R
    - [EDA: 3 Questions to BRFSS Data](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project1_intro_data_prob.md)
2. Inferential Statistics
    - [Extramarital Sex Gender Difference in GSS Data](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project2_stat_inf.md)
3. Linear Regression and Modeling
    - [Prediction of IMDB Movie Scores (Not Using RT)](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project3_reg_model.md)
4. Bayesian Statistics
    - [Bayesian Model Averaging for Movie Scores](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project4_bayesian.md)
5. Capstone Project
    - [Predicting House Prices in Ames, Iowa](https://github.com/belovvadim/openintrostat-dataprojects/blob/main/project5_capstone.md)
	
Knitted reports are provided as GitHub Flavored Markdown files `project#_*.md`, for convenience. To preview the version from `html-output` folder, you may go to [https://htmlpreview.github.io/](https://htmlpreview.github.io/), or just prepend it to the original URL. (This may be easier to read, with code-folding enabled, instead of the full code chunks in .md and no vertical overflow-y:scroll for some reason here on GitHub.)

For reproducibility, I included the used datasets in the `data`, together with some description (brfss2013.RData is split in two to comply with < 50 MB). You can clone the repo and knit the .Rmd in `project-files` as you like (note the paths are relative to main/). E.g., if you are taking the same course, you may take inspiration or find ideas for your own data analysis project (beware of plagiarism).