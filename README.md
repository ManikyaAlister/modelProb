# ModelProb: an R package to convert raw model comparsion information criteria into relative probabilities. 

Raw model comparison information critea, like AIC and BIC, can be somewhat unintuitive to interpret, and are often reported in a way that tells the reader which model is best, but that does not facilitate easy interpretation as to how *much* better a model is relative to other candidates. Converting these criteria into relative probabilities facilitates a simple yet intuitive way of conveying these results. The ``moelProb`` package offers a simple way to do this for AIC and BIC, as well as some basic plotting functions.
## Installation

The package can be installed from github and can be installed using the command ``devtools::install_github("https://github.com/ManikyaAlister/modelProb")`` (you may need to install the package ``devtools`` first).


## Library usage

To use the package, load it into the R environment:

```{r}
library(modelProb)
```
