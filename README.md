# anchor-r

Several r packages that provide utility functions for the output of the Anchor platform

| Package | Description |
|--------|--------|
| anchor-r |  general functions for anchor output  |
| anchor-r-features | for the feature-tables CSV exports (viewing, training learners, etc.)
| anchor-r-nuclei | for nuclei segmentation pipelines |
| anchor-r-experiment | for conveniently accessing the output of anchor experiments |

## Documentation

* Each function is documented in its header (in the respective *R/* subfolder)
* The *NAMESPACE* file specifies which functions are publicly-exposed
* R-package dependencies are listed in the *DESCRIPTION* file and in *NAMESPACE*

## Installation

Either
* Use [RStudio](https://www.rstudio.com/), open each project, and select the *Build & Reload* button in the *Build* panel in the top-right.
* Use the make script (**make.bat** / **make.sh**) in the repository's top-level folder. **N.B. please first add your R executable to the system path**