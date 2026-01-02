# East Boston Gentrification Analysis

## Date 
December, 2024

## Overview
This project examines neighborhood-level gentrification dynamics in East Boston using census block-group panel data. The analysis focuses on how socioeconomic characteristics relate to rising median rents and identifies block-group-specific variations in rental trends.

## Methods
- Ordinary Least Squares (OLS) regression with diagnostic testing
- Multicollinearity, normality, and heteroskedasticity checks
- Unsupervised learning using k-means and hierarchical clustering
- Cluster-specific regression analysis

## Key Findings
- Median household income, labor force participation, and educational attainment are positively associated with rent growth. 
- Clustering reveals distinct gentrification trajectories consistent with early-stage, advanced, and stable patterns.
- Identified clusters correspond to neighborhoods exhibiting relatively stable, transitional, and rapidly changing rental patterns.

## Limitations
This analysis is descriptive does not attempt causal idenfitication. Results are produced based on publically available data with median rent as a proxy for gentrification-related housing affordability pressure. While the results identify evidence-based patterns and meaningful variation in rent dynamics across neighborhoods, they are not intended to be exhaustive or predictive. Stronger causal claims would require richer, purpose-built data.

## Collaboration Note
This project was completed collaboratively. My contributions included compiling the dataset; implementing the pooled regression models and diagnostic analyses; refining and debugging the analysis script; and authoring the Introduction, Data Summary, and Application sections of
the report.

## Tools
R; dplyr, ggplot2, lmtest, car, sandwich, cluster, factoextra, NbClust
