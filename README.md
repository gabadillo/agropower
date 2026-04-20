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

---

## Output

After a simulation run completes, two files are saved to the `output/` directory. Both are named with the number of iterations and the random seed used, following the pattern `<type>_<nIter>_<seed>`:

| File | Pattern | Description |
|------|---------|-------------|
| Summary table | `output/summary_<nIter>_<seed>.csv` | Aggregated metrics displayed in the **Summary Metrics** tab: design parameters, variance components, power estimates, ranking statistics, and empirical false positive rate. |
| Estimates plot | `output/estimates_<nIter>_<seed>.png` | Point estimates with 95% confidence intervals for the control gene across all iterations, as shown in the **Summary Plots** tab. |
