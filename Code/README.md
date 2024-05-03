# Coding Sections

## 1. Data cleaning
(1) 'Data cleaning.Rmd' cleans the raw data and puts the data as a dataframe to be fed into the models.

(2) The two scraping files extract the computing details (e.g. time, cores) from the ORCA output files, and help estimate the computation costs. (reported in the main text of the paper on Table 3)

## 2. Modeling
(1) 'Ensemble Modeling.Rmd' builds the cruciform and fragment ensemble frameworks using XGBoost model. Within the building blocks of the frameworks, we also stored out-of-bag prediction error (i.e. RMSE, MAE, MSE, MAPE), ALE for some of the features, and etc for the following analysis.

(2) 'GBM.Rmd' builds the cruciform and fragement ensemble frameworks using GBM model, and is for the comparison purpose. We only checked the prediction performance of it, but did not analyze the model.

## 3. Model Analysis
(1) 'Model Analysis.Rmd' assesses the XGBoost ensemble frameworks. We checked the prediction performance, feature importance, ALE plots of some features of interest, and etc here. Most of analysis from this file is on the main text of the paper. (reported on the paper as Fig 3 (distribution of the DFT descriptors) & 4 (univariate ALE plots for some important DFT features) and Table 2 (prediction performance of CF and FF))

(2) 'Stability Check.Rmd' assesses the perdiction stability of the ensemble frameworks compared to conventional XGBoost models with random training data, and the analysis of this part is on the supplements of the paper. (reported on the SI as Fig S1)

## 4. ALE plots
(1) 'ALE modeling.Rmd' extracts all other univariate and bivariate emsemble ALE plots that are not included in the main text of the paper.

(2) 'aleplots_univariate.R' draws the univariate ensemble ALE plots for other unimportant DFT features. (reported on the SI as Fig S2)

(3) 'aleplots_bivariate1.R' draws the bivariate ensemble ALE plots as contour plots. (reported on the SI as Fig S3,4,5,6,7)
