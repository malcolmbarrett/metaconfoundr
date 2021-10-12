## The Package

The goal of metaconfoundr is to make it easy to visualize confounding control in systematic reviews and meta-analyses of observational studies. Specifically, researchers can create a color-coded data matrix that visually summarizes the confounding variables and how well each study in the systematic review controlled for each: adequately, inadequately, or with some control. 

How does one determine the definitions for adequate confounder control? We recommend using an expert consensus-based process, driven by prior knowledge and causal inference theory. Together, the experts determine which confounders are required to control confounding to validly estimate the causal effect of an exposure on a health outcome. These criteria are then applied to the component studies of the systematic review to determine the level of confounder control across the body of observational evidence.

The level of confounder control may be quantitatively incorporated into meta-analyses, for instance by stratifying by level of confounder control or by simulating a commonly lacking but important confounder. For more information, see Petersen et al. 

## Setting up the Data

To ensure that this app works correctly, please set-up your data according to the following format:
•	In the first row, leave Column A blank; Columns B-F should be labeled: `construct` (Column B), `factor` (Column C), `confounder_y_n` (Column D), `study` (Column E), `control_quality` (Column F). 
•	For the remaining rows, the worksheet should be populated with the following information:  
o	Column A includes a running counter for each subsequent row of data
o	Column B lists each construct/domain name
o	Column C lists each individual variable name under a specific construct/domain
o	Column D specifies if the variable is a confounder or not (Y/N)
o	Column E lists the study identifier for the Y axis in the plot (e.g., last author and year)
o	Column F lists the user-defined level of confounder control (2, 1 or 0) where 2 = adequate, 1 = some concerns, and 0 = inadequate

Download sample data `ipi.csv` for an example of how to set up the data.

## Proper Citation

When using these plots in practice, please cite our package metaconfoundr and original publication detailing the confounder matrix procedure (Petersen et al.).

Barrett M, Petersen JM, Trinquart L. metaconfoundr: An R package for visualizing confounder control in meta-analyses (2020)

Petersen JM, Barrett M, Ahrens KA, Murray EJ, Bryant AS, Hogue CJ, Mumford SL, Gadupudi S, Fox MP, Trinquart L. Confounder Matrix: A Tool to Assess Confounding Bias in Systematic Reviews of Observational Studies. (unpublished manuscript) 

## Functionality and Plot Options

The metaconfoundr package procedures a visual color-coded confounder matrix. With respect to Plot Type, the Heatmap option shows the intersecting cells as squares, whereas Traffic light displays them as circles. The size of the intersecting cells may be modified under the circle size option. With respect to colors, Viridis displays a color-blind friendly palette (yellow = adequate, aqua = some concerns, dark purple = inadequate). Cochrane, on the other hand, displays the color scheme associated with the ROBINS-I risk of bias tool (green = adequate, yellow = some concerns, red = inadequate). 

The optional checkboxes: 
•	Group: display the confounder variables may be grouped by underlying confounding domain,
•	Symbols: superimpose the Cochrane-style symbols for level of confounding control (+ adequate, - some concerns, x inadequate)
•	Dodge and Wrap: display the confounders and constructs labels on multiple lines with wrapped text to avoid overlapping text

The default plot displays level of confounder control by variable, whereas the summary plot displays level of confounder control by each construct and overall. 

The plots may be downloaded in a variety of formats. 

## The Data Example
The example dataset (`ipi.csv`) represents a meta-analysis of 11 retrospective cohorts in high-resource settings to evaluate the effect of short interpregnancy intervals (<6 months versus 18-23 months) on risk of preterm birth (<37 weeks’ gestation) and the adequacy of confounder control across this body of evidence (Petersen et al.). For each study included in the review, adequacy of confounder control was determined overall as well as by variable and construct (groupings of conceptually related variables).These studies are a subset of studies originally identified in a systematic review by Ahrens et al. to summarize associations between short interpregnancy interval and a variety of perinatal outcomes in high-resource settings. 

Please note that in the sample dataset `ipi` the study name is listed as “study_1”, “study_2”, etc. However, when setting up your own data, you may opt to include a more specific label, e.g., lead author last name and publication year. 

### Reference
Ahrens KA, Nelson H, Stidd RL, Moskosky S, Hutcheon JA. Short interpregnancy intervals and adverse perinatal outcomes in high-resource settings: An updated systematic review. Paediatr Perinat Epidemiol. 2019;33(1):O25-O47.

## Installing the R Package

Install the most recent released version of metaconfoundr from CRAN:

``` r
install.packages("metaconfoundr")
```

You can install the development version of metaconfoundr from GitHub with:

``` r
# if needed
# install.packages("remotes")
remotes::install_github("malcolmbarrett/metaconfoundr")
```

## Issues

Please file any issues at our [GitHub page](https://github.com/malcolmbarrett/metaconfoundr).
