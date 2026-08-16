# Dissertation - Public Repository
### Public repository with code used to complete Dissertation for Ecology, Evolution and Biodiversity MSc, University of Edinburgh. 
Project title: Identifying a Signal of Climate Drivers on the Reproductive Success of Southern Giant Petrels and Atlantic Yellow-nosed Albatrosses on Gough Island

This repository contains annotated R code (version 4.6.0), with subsequent model outputs and figures used to assess climate driver associations with the reproductive success of Southern Giant Petrels and Atlantic Yellow-nosed Albatrosses on Gough Island. 

This repository does not contain the data provided to complete the project: the climate data are owned by the South African Weather Service, and the breeding data are owned by the Tristan da Cunha Government and the Royal Society for the Protection of Birds. Restricted access to the raw climate data is detailed in `Weather Data Request SAWS Form` and the breeding data in https://tdc.data.bas.ac.uk/f?p=249:LOGIN:1761607218044. 

## Repository Structure
This repository has been organised into sub-directories accordingly:

`Scripts/` contain the annotated R code used for data processing, candidate window analysis, Bayesian analysis, and figure generation. 

`Figures/` contain the figures and plots produced for analysis. 

`Candidate Window AIC Outputs/` contain tables with all model AIC values generated from biologically defined candidate window analysis. 

`Model summaries/` contain .txt files with Bayesian model summary outputs.

`Model diagnostics/` contain the posterior convergence plots and robustness diagnostics for each Bayesian model. 

## FAIR Data Principles

**Findable:** Repository materials are organised into clearly labelled directories with descriptive file names and README documentation for easy access.

**Accessible:** Analysis scripts, figures and model outputs are publicly available through this repository. Raw data are not provided because they are owned by third-party data providers; their sources and access restrictions are documented above.

**Interoperable:** Scripts and outputs are provided in commonly used formats, including `.R`, `.csv` and `.txt`, with consistent variable names. 

**Reusable:** Annotated R scripts document the data processing and statistical analysis workflow, while model outputs and figures are retained to support interpretation and reproducibility of study methodology. Reuse of the original data is subject to the respective data owners. 
