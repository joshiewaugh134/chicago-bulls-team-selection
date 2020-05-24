# Chicago Bulls Team Selection

This project is a reproducible data analysis on the Chicago Bulls. The aim of this project was to choose the starting 5 players for the team in the 2020 season.

## Packages Required

```r
library(tidyverse)
library(broom)
```

## Contents

There are numerous files and folders in this project. 

[data](data) folder -> includes `raw` and `processed` subfolders

* [raw](data/raw) - contains raw data files
* [processed](data/processed) - contains converted tidy data files

[figs](figs) folder -> contains images of graphs developed during the analysis process

[packrat](packrat) folder -> contains packages that were used in the analysis

[R](R) folder -> contains R files that have specific functions created for the analysis

[chicago-bulls-team-selection.html](chicago-bulls-team-selection.html) -> `knitr` report to present the results of the analysis

[chicago-bulls-team-selection.Rmd](chicago-bulls-team-selection.Rmd) -> `RMarkdown` file used to create the above html document

[chicago-bulls-team-selection.Rproj](chicago-bulls-team-selection.Rproj) -> `RProject` file with local working directory

There are also 5 processes to be opened in a specific order for the analysis to function properly:

1. [tidying_data](tidying_data.R) -> process of reading and tidying the raw data
2. [positional_filtering_process](positional_filtering_process.R) -> process of filtering positions into their own data sets
3. [positional_selection_process](positional_selection_process.R) -> process of refining data sets to choose a player to be in starting lineup
4. [modelling_graphs](modelling_graphs_by_position.R) -> visualisations of trends in the data
5. [graphics_for_communication](graphics_for_communication.R) -> visualisations used in the `knitr` report
