# Coding Sections

## 1. Data cleaning
(1) 'Data cleaning.Rmd' cleans the row data and puts the data as a dataframe to be fed models.

(2) The two scraping files extract the computing details (e.g. time, cores) from the ORCA output files, and help estimate computation costs.

## 2. Modeling
(1) 'Ensemble Modeling.Rmd' builds the cruciform and fragment ensemble frameworks using XGBoost model. Within the building blocks of the frameworks, we also stored out-of-bag prediction error, ALE for some of the features, and etc for the following analysis.

(2) 'GBM.Rmd' builds the cruciform and fragement ensemble frameworks using GBM model, and is for the comparison purpose. We only checked the prediction performance of it, but did not analyze the model.

## 3. Model Analysis
(1) 'Model Analysis.Rmd' assesses the XGBoost ensemble frameworks. We checked the prediction performance, feature importance, ALE plots of some features of interest, and etc here. Most of analysis from this file is on the main text of the paper.

(2) 'Stability Check.Rmd' assesses the perdiction stability of the ensemble frameworks compared to conventional XGBoost models with random training data, and the analysis of this part is on the supplements of the paper.

## 4. ALE plots
(1) 'ALE modeling.Rmd' extracts all other univariate and bivariate emsemble ALE plots that are not included in the main text of the paper.

(2) 'aleplots_univariate.R' draws the univariate ensemble ALE plots.

(3) 'aleplots_bivariate1.R' draws the bivariate ensemble ALE plots as contour plots.
