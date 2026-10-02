## Introduction

The Framingham Heart Study is a long-running cohort study focused on cardiovascular health in residents of Framingham, Massachusetts. Since its initiation in 1948, it has played an important role in cardiovascular epidemiology, particularly in establishing the concept of cardiovascular risk factors and the combined influence of multiple risk factors on disease development.

The dataset used in this study is a subset of the Framingham Heart Study data and contains laboratory measurements, clinical information, questionnaire responses, and event-related outcomes for 4,434 individuals. Clinical measurements were collected across three examination periods separated by approximately six years, while participants were followed for up to 24 years for cardiovascular outcomes including angina pectoris, myocardial infarction, cardiovascular disease, stroke, hypertension, and death.

This analysis focuses on measurements from the first examination period and outcomes observed during the subsequent 24-year follow-up. The outcome variables are treated as binary indicators of whether an event occurred during this period; the exact event time is not modeled.

The objective of this study is to estimate the causal effect of current cigarette smoking (`CURSMOKE`) on the risk of hypertension (`HYPERTEN`) using observational data. Because treatment assignment is not randomized, the analysis addresses confounding and causal identification explicitly through directed acyclic graphs (DAGs), propensity-score methods, inverse probability weighting, covariate-balance diagnostics, and weighted regression modeling.

The study estimates both the Average Treatment Effect (ATE) and the Average Treatment Effect on the Treated (ATT), while also examining assumptions such as exchangeability and positivity and discussing the possibility of residual confounding and other limitations of observational causal analysis.

The dataset has been processed in a way intended to preserve participant anonymity and confidentiality and should not be treated as a substitute for the original Framingham Heart Study data.


## Summary

Causal Inference - Framingham Heart Study: Applied DAGs for confounder identification, propensity-score weighting, balance diagnostics, and weighted logistic regression to estimate ATE/ATT for the effect of smoking on hypertension, with sensitivity to residual confounding, and limitations of observational data.


