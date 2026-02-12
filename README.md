![](course_logo.png)

# R Programming for Statistics and Machine Learning

## Instructors

- Dr. Nikolay Oskolkov, Metabolic Research Group Leader, LIOS, Riga, Latvia
- Dr. Daniel Rivas, postdoctoral fellow in Metabolic Research Group, LIOS, Riga, Latvia

## Course overview
This course is designed for participants who are at beginner-level or have basic familiarity with R and are ready to advance their programming and computational data-analysis skills. Building on foundational R concepts, the course emphasizes implementing core statistical and machine-learning algorithms from scratch. Alongside deeper coverage of data structures, programming paradigms and functional programming, participants will explore practical applications in bioinformatics. Students will progress from fundamental programming concepts to implementing key analytical tools, including non-parametric and permutation-based statistical tests, linear mixed models (LMMs), dimensionality reduction (PCA, tSNE, UMAP) and clustering algorithms (e.g., k-means), random forests, and simple neural networks directly in base R. 

## Target audience and assumed background
We assume some basic awareness of UNIX environment, as well as at least beginner level of R and / or Python programming.

## Learning outcomes
By completing this course, you will:

- Understand the basics of statistical and machine learning approaches to biological data analysis
- Gain an overview of basic machine learning algorithms and their applications
- Be able to choose the right algorithms and approaches to answer your specific research question
- Gain confidence in learning new methods needed to answer your research question

---

# Schedule

## Before the course

| Time   | Activity                                                          | Link                                                                                                                                                    |
|--------|-------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| ~ 1 h  | In case needed: Recap on Unix                                                                    | [Lab](command-line-basics.md)                                                                                                                           |
| ~ 0.5 h  | R cheatsheet                                                                                   | [Blog](https://iqss.github.io/dss-workshops/R/Rintro/base-r-cheat-sheet.pdf)                                                                                                  |
| ~ 0.5 h  | Refresh basic R skills (part1): Rstudio, data types, reading and writing to file               | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/precourse_material/slides/D01.pdf)                                                                                                    |
| ~ 0.5 h  | Refresh basic R skills (part2): conditionals, loops and functions                              | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/precourse_material/slides/D02.pdf)                                                                                                    |
| ~ 0.5 h  | Refresh basic R skills (part3): graphics and visualization, base R vs. ggplot                  | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/precourse_material/slides/D03.pdf)                                                                                                    |


## Day 1: 9.00 - 14.00 Riga time

| Time           | Activity                                                          | Link                                                                                                                                        |
|----------------|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| 09.00 - 10.00  | Introduction to statistical analysis in R: Frequentist vs. Bayesian approaches             | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture1_IntroStatistics.pdf)                                                                                 |
| 10.00 - 10.30  | Break                                                                                      |                                                                                                                   |
| 10.30 - 11.30  | Practical: comparing parametric and non-parametric tests in R                              | [Lab](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/practicals/Practical1_permutation_test.pdf)  |
| 11.30 - 12.30  | Lunch                                                                                      |                                                                                                                   |
| 12.30 - 14.00  | Introduction to machine learning in R: linear model and gradient descent                   | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture2_IntroMachineLearning.pdf)                                                                                                                                                                                                                     |


## Day 2: 9.00 - 14.00 Riga time

| Time           | Activity                                                         | Link                                                                                                                                        |
|----------------|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| 09.00 - 10.00  | Lecture: Univariate and multivariate feature selection in R: LASSO and PLS                 | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture3_FeatureSelection.pdf)                                                                                |
| 10.00 - 10.30  | Break                                                                                      |                                                                                                                    |
| 10.30 - 11.30  | Practical: univariate feature selection (differential gene expression analysis)            | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/FeatureSelection.html)                                                                             |
| 11.30 - 12.30  | Lunch                                                                                      |                                                                                                                    |
| 12.30 - 14.00  | Practical: multivariate feature selection: comparison between LASSO and PLS                | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/FeatureSelection.html)


## Day 3: 9.00 - 14.00 Riga time

| Time           | Activity                                                         | Link                                                                                                                                        |
|----------------|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| 09.00 - 10.00  | Lecture: unsupervised learning, linear dimensionality reduction, PCA and MDS               | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture4_LinearDimensionReduction.pdf)                                                                        |
| 10.00 - 10.30  | Break                                                                                      |                                                                                                                    |
| 10.30 - 11.30  | Lecture: unsupervised learning, non-linear dimension reduction, tSNE, UMAP                 | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture5_NonLinearDimensionReduction.pdf)                                                                     |
| 11.30 - 12.30  | Lunch                                                                                      |                                                                                                                    |
| 12.30 - 14.00  | Practical: comparing dimensionality reduction techniques on MNIST dataset                  | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/DimensionReduction.html)


## Day 4: 9.00 - 14.00 Riga time

| Time           | Activity                                                         | Link                                                                                                                                        |
|----------------|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| 09.00 - 10.00  | Lecture: unsupervised learning, hierarchical and partitioning clustering in R              | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture6_BasicClustering.pdf)                                                                                |
| 10.00 - 10.30  | Break                                                                                      |                                                                                                                    |
| 10.30 - 11.30  | Lecture: unsupervised learning, spectral, density- and graph-based clustering              | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture7_AdvancedClustering.pdf)                                                                                |
| 11.30 - 12.30  | Lunch                                                                                      |                                                                                                                    |
| 12.30 - 14.00  | Practical: comparing clustering algorithms on spherical and moon-like clusters             | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/Clustering_Lab.html)



## Day 5: 9.00 - 14.00 Riga time

| Time           | Activity                                                         | Link                                                                                                                                        |
|----------------|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| 09.00 - 10.00  | Lecture: non-linear machine learning in R, neural networks and backpropagation                 | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture8_NeuralNetworks.pdf)                                                                                  |
| 10.00 - 10.30  | Break                                                                                      |                                                                                                                    |
| 10.30 - 11.30  | Lecture: non-linear machine learning in R, decision tree and random forest                 | [Slides](https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/raw/main/slides/Lecture9_RandomForest.pdf)                                                                                    |
| 11.30 - 12.30  | Lunch                                                                                      |                                                                                                                    |
| 12.30 - 14.00  | Practical: coding a vanilla neural network from scratch in R on a toy dataset              | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/ANNFromScratch_R.html)
| 12.30 - 14.00  | Bonus practical: comparing linear and non-linear machine learning algorithms               | [Lab](https://html-preview.github.io/?url=https://github.com/NikolayOskolkov/R_course_TARGETWISE_2026/blob/main/practicals/Lab_MachineLearning.html)

