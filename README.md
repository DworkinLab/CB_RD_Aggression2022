# Data and scripts for Baxter et al. 2022

**Not a full readme yet**

All data used for the analysis is in the folder data, and scripts in the scripts folder.


## Data Provenance

TBW

- All assays were video recorded. Assays were all 10 minutes in length.

- The two experiments were done at different times.



## Data summary and identifiers

- ID modified column names in the excel file to maintain consistency between the two experiments.

- For the experiment "Males without females", the information contains day and trial, which needs to be broken up for the mixed model.

- model observer effects as fixed.
TBW

## notes from RD from emails.

RD (Dec 28, 21, gmail):**It has 2 sheets for the 2 contexts of males with or without females (see cartoons in Figs). I highlighted threat in light orange and the columns with physical aggression in dark orange. So physical aggression is the sum of these columns.** I'll look for further notes that I might have.

RD (Dec 30, 21, gmail): Ian, here is the **explanation for including trial id: we had threat duration and contact aggression duration as the dependent measures for each trial and we thus included trial ID as a random factor. So the idea is that for each trial, the 2 aren't independent.**

RD (Dec 31, 21, gmail):I realized that I should mention something about Carling's data set: **in the males with females data sheet, day is nested within exp code. So day 1 within exp code A is different from day 1 within Exp code B etc**

RD (Jan 6, 22, gmail):If you haven't imported the data into R, you may use the attached file, which I made more user friendly for future inclusion with the ms. **I highlighted in green the relevant columns, added a few comments and added the last columns, for threat and total physical aggression reported in our standard units of seconds per minute.**
