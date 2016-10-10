# Data & code repository for *Restricting restrictive relativizers*

Documentation and R code for multinomial analysis of English restrictive relativizers, as reported in Grafmiller, Szmrecsanyi & Hinrichs (forthcoming). Restricting restrictive relativizers. Constraints on subject and non-subject English relative clauses. *Corpus Linguistics and Linguistic Theory*

## Organization of repository

This repository contains all the necessary files for reproducing the analysis, including the dataset and R code, along with some additional documentation. The files are organized into the following subdirectories:
- **R**
- **data**
- **documents**

In addition, an [.Rprofile](.Rprofile) script is included which loads all the necessary packages along with all the "Extensions.R" files. This should be sourced at the outset, before running other files.

```
source('.Rprofile')
```

### R 

This subdirectory contains R scripts for setting up the dataframe, running the factor analysis of mixed data, and running the Bayesian regression models using the [MCMCglmm](https://cran.r-project.org/web/packages/MCMCglmm/index.html) package. It also contains a number of supplementary files containing various custom functions and extensions of common packages. 

The following files are all that is necessary for reproducing the the analysis. They should be run in the order listed.
- [data_setup.R](R/data_setup.R): code for loading and readying the dataset
- [FAMD_analysis.R](R/FAMD_analysis.R): code for running factor analysis of mixed data
- [MCMCglmm_models.R](R/MCMCglmm_models.R): code for fitting the MCMCglmm models, obtaining the model predictions, and plotting and displaying the model results. For this code to work, you must first run the factor analyses in [FAMD_analysis.R](R/FAMD_analysis.R).

Extensions:
- [MCMCglmmExtensions.R](R/MCMCglmmExtensions.R): functions for working with MCMCglmm objects, e.g. calculating VIF and &kappa; scores

### data

This subdirectory contains the tab-delimited file [relative_clauses.txt](data/relative_clauses.txt). This is a modified version of the dataset collected and annotated by Hinrichs, Szmrecsanyi & Bohmann ([2015](https://muse.jhu.edu/journals/language/v091/91.4.hinrichs.html)). 

### documents

This subdirectory contains two files documenting the dataset: the [code book](documents/CodeBookDec2011.pdf) and a [description](relativizers/documents/MANUAL FOR CODING RELATIVIZERS IN THE BROWN CORPORA.pdf) of how tokens were extracted. Documentaion for the MCMCglmm package can be found on CRAN.
