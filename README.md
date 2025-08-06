# Exploratory Data Analysis of Drivers’ Performance, Standings, and Earnings in Formula 1

## Overview
This project analyzes Formula 1 (F1) data to explore how various factors—such as driver experience, starting position, and performance—relate to race results and earnings. The goal is to uncover statistical insights about the sport’s key performance and financial drivers.

## Data Sources
Formula 1 World Championship dataset (1950–2023) from Kaggle
Supplementary data: 2023 F1 driver salaries (RacingNews365.com), 2023 driver points (formula1.com)

## Key Questions
1. Does a driver’s experience affect the average points earned per race in the last 10 years?
2. Does starting from pole position affect the final race position?
3. Is there a relationship between a driver’s experience and salary?
4. Is there a relationship between a driver’s performance (points) and salary?

## Methodology
Data cleaning and integration from multiple sources
Calculation of key metrics (experience level, average points per race, etc.)
Visualization: boxplots, histograms, QQ plots
Statistical analysis: ANOVA, Fisher’s exact test, Kruskal-Wallis test, correlation, linear regression

## Main Findings
Experience & Performance: Higher driver experience generally correlates with higher average points per race.
Pole Position Impact: Starting from pole position significantly increases the likelihood of finishing in a top position.
Experience & Salary: There is a moderate positive correlation between experience and salary, but not statistically significant in median comparison.
Points & Salary: Driver points are strongly correlated with salaries, with regression analysis suggesting performance is a key salary determinant.

## How to Use
Download and review the dataset(s) listed above.
Run the data preparation and analysis scripts (provided separately).
Explore the generated visualizations and statistical outputs to review findings.

## References
Kaggle F1 World Championship Dataset

RacingNews365.com F1 Driver Salaries

Formula1.com 2023 Results

