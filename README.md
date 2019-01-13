# econJobMarket
This repository contains `R` scripts and file structure to automate the collection of job listings from the primary aggregation websites for economics PhD job candidates: the AEA's JOE network, and EconJobMarket.org. The purpose of this repository is to help job candidates save time by allowing candidates to assert their job preferences ex ante and have the computer do the work of producing a list of jobs that match the candidate's stated preferences. The file structure is as follows:

## Inputs
To process JOE listings, the user needs to download the current listings as an XLS file from the main JOE listings webpage, <https://www.aeaweb.org/joe/listings.php?>. From there, click on "Download Options" and "Native XLS." Then place this .xls file in the "RawDownloads" folder of the repository, and rename the file to include the corresponding date. 

## Outputs
The outputs of the scripts are CSV files which contain the full set of information from each listing.

For JOE:
* `All_JOE_jobs_YYYY-MM-DD_processed.csv`, which is the set of "matched" listings
* `All_JOE_jobs_YYYY-MM-DD_bad_matches_processed.csv`, which is the set of "unmatched" listings. Note that the union of this set and the matched set is the full set of JOE postings.
* `All_JOE_jobs_YYYY-MM-DD_all_processed.csv`, which is the full set of JOE postings
* `All_JOE_jobs_YYYY-MM-DD_clean.csv`, which is an abbreviated version of the set of matches.

For EJM:
* `All_EJM_jobs_YYYY-MM-DD_processed.csv`, which is the set of "matched" listings
* `All_EJM_jobs_YYYY-MM-DD_raw.csv`, which is the full set of listings

## Scripts
There are three `R` scripts in the Rscripts folder:

1. `JOEprocessor.R`, which takes as an input the .xls file referenced above
2. `EJMprocessorNew.R`, which automatically downloads listings 
3. `updateJOEprocessor.R`, which uses the output from two different JOE .xls files to produce a list of new postings since the last date.

EJM posts much less information about listings than does JOE. Therefore the output for EJM listings is much less customizable. Also, the EJM script might take awhile to run because it is entirely scraped. Finally, the EJM script will issue a number of warnings, which the user should ignore.

# Disclaimer
Along with the standard GNU-style disclaimers, I add that the scraping code is written to be compatible with these websites as of Dec 1, 2018. Any future changes to the style and/or structure of these websites might invalidate the code as presently written.

# Acknowledgements
This is an actualization of the code obtained from https://github.com/tyleransom/econJobMarket  
