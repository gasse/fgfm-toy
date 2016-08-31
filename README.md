# Disclaimer

These scripts are made available for reproducibility purpose, and are neither well documented nor maintained. If you find a bug you can let me know, but I do not guarantee that I will take time to fix it.


# Factorized GFM

This repository contains the R code used to run the synthetic experiment in the ECML 2016 paper [*F-measure Maximization in Multi-Label Classification with Conditionally Independent Label Subsets*](http://ecmlpkdd2016.org/program.html#Accepted).

## Dependencies

A modified version of the [bnlearn](http://www.bnlearn.com/) R package is required for compatibility reasons.

``` R
install.packages("devtools")
library("devtools")
install_github("gasse/bnlearn-clone-3.4")
```

The following base learner:

``` R
install.packages("nnet") # multinom
```

## Run the experiment

``` R
source("00.main.R")
```

should run the whole thing.


## Description of the scripts

* **00.main.R**
is the main script to configure and run the whole benchmark.

* **01.make.bn.R**
generates Bayesian networks with fixed structure (corresponding to different ILF decompositions) and random distributions.

* **02.gen.data.R**
generates the training/test data sets.

* **03.learn.ydeps.R**
extracts ILF decompositions with the [*ILF-Compo*](http://jmlr.org/proceedings/papers/v37/gasse15.html) algorithm.

* **04.run.models.R**
trains the F-measure maximizer (GFM, F-GFM with ILF-Compo and F-GFM with the true decomposition), and makes predictions for test data.

* **05.plot.loss.R**
produces figures comparing the performance of the different F-measure maximizing methods.


## Parameters

The default parameters to configure the experiment can be changed in *00.main.R*

``` R
# fixed seed for reproducibility
seed = 0

# CI test configuration, used in "03.learn.ydeps.R"
ci.test = "sp-mi"
ci.alpha = 0.01
ci.test.args = list(power.rule=NULL, B=100)

# number of repetitions (random distributions)
n.reps = 100

# number of training/test samples
n.test = 5000
n.train.reps = c(50, 100, 200, 500, 1000, 2000, 5000)

# number of labels
n.ys = 8

# number of relevant input binary features
n.xs = 4

# number of irrelevantinput binary features (noise)
n.zs = 1

# configuration of the ILF decompositions (factors with 2 labels, 4 labels etc.)
lp.sizes = c(2, 4, 6, 8)

```