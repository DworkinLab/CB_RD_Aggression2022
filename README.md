# Data and scripts for Baxter et al. 2022

This is the data and scripts repository associated with the manuscript:

Baxter, C.M., Shams, I., Dworkin, I. and Dukas, R. 2023. Genetic correlation between aggressive signals and fighting. Biology Letters. In Press. https://doi.org/10.1098/rsbl.2022.0616


## General notes

All data used for the analysis is in the folder data, and script for analysis (`CB_RD_Aggression.Rmd`) in the scripts folder. All analysis and figures in the paper (and some we did not use) are included in the script and can all be generated from it. I have included most notes about variables in the .Rmd file directly.


## Data Provenance

- Please see the manuscript for full experimental details.

- All assays were video recorded. Assays were all 10 minutes in length.

- The two experiments were done at different times, and have been analyzed separately.


## Data summary and identifiers

- ID modified column names in original file to maintain consistency between the two experiments prior to generating the CSVs used for analysis.

- For the experiment "Males without females", the information contains day and trial, which needs to be broken up for the mixed model.

- model observer effects as fixed.


Original Excel file (for analysis CSVs were generated and included in the repository).It has 2 sheets for the 2 contexts of males with or without females (see cartoons in Figs).  Threat in light orange and the columns with physical aggression in dark orange.  physical aggression is the sum of these columns.
I highlighted in green the relevant columns, added a few comments and added the last columns, for threat and total physical aggression reported in our standard units of seconds per minute.

explanation for  *trial id*: threat duration and contact aggression duration as dependent measures for each trial and thus included trial ID as a random factor. For each trial, the 2 aren't independent.

in the *males with females data sheet*, day is nested within exp code. So day 1 within exp code A is different from day 1 within Exp code B etc.
