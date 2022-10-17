# Low awareness of the transitivity assumption in complex networks of interventions: empirical evidence from 356 reviews

## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts and .RData):
* The _data_ folder includes two .RData files: (i) dataset and (ii) judge_trans_quality. The file __dataset.RData__ contains the extracted data for all items from the collected 356 systematic reviews. The file __judge_trans_quality.RData__ includes data to create the Sankey plot (Figure 4);
* The _R_ folder includes the scripts to create all Tables Figures of the article. To create Tables 1 to 4 and Figures 1 to 3, the corresponding R-scripts load the file __dataset.RData__. To create Figure 4, the R-script _Create Figure 4.R_ loads the file __udge_trans_quality.RData__.<br>

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: [dplyr](https://CRAN.R-project.org/package=dplyr),
[gmodels](https://CRAN.R-project.org/package=gmodels),
[ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [ggpubr](https://CRAN.R-project.org/package=ggpubr), and
[plyr](https://CRAN.R-project.org/package=plyr),
[rnmamod](https://CRAN.R-project.org/package=rnmamod),
[stringr](https://CRAN.R-project.org/package=stringr).

The user should also install the development version of ggsankey R-package:

    install.packages("devtools")
    devtools::install_github("davidsjoberg/ggsankey")
