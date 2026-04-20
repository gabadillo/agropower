# agroPower

**agroPower** is an interactive R Shiny application designed for Monte Carlo power analysis in complex agronomic field trials. Given a user-defined experimental design and variance structure, the app simulates scenarios in which a causal treatment effect is confounded by genetic, environmental, and stochastic factors. It then fits linear mixed models to estimate the probability of detecting the effect. Although it was developed with the purpose of screening transgenic citrus for disease resistance in mind, it can be used for any multi-environment trial in which the detectability of a treatment must be evaluated prior to conducting a full experiment.

---

## Installation

### 1. Clone the repository

Using Git:
```bash
git clone https://github.com/gabadillo/agropower.git
```

Using GitHub CLI:
```bash
gh repo clone gabadillo/agropower
```

### 2. Install R dependencies

All required packages are listed in `requirements.txt`. Open an R console and run:

```r
pkgs <- readLines("requirements.txt")
install.packages(pkgs)
```

### 3. Run the app

Open `app.R` in RStudio and click the **Run App** button in the top-right corner of the editor, or run from the R console:

```r
shiny::runApp()
```

> The app was developed and tested with R >= 4.2. If you run into issues, make sure your R version and packages are up to date.
