# Recall reliability in retrospective flood surveys and its propagation into flood damage models

Analysis code for:

Tejada Jr., A., Kawasaki, A., Di Baldassarre, G., Jaranilla-Sanchez, P.A., Ohara, M.
*How Reliable are Retrospective Surveys for Flood Vulnerability Studies?*
Submitted to *Environmental Modelling & Software*.

## What this repository contains

`recall_reliability_fdm.R` — a single self-contained script reproducing the full
analysis, organised by research question:

| Section | Question | Outputs |
|---|---|---|
| RQ1 | How reliable are recalled variables, by item type and flood event? | ICC(2,1), Gwet's AC1, percentage agreement; Table 2; Figure 3 |
| RQ2 | Who recalls consistently — the event or the household? | Mixed-effects logistic model with event and household terms; Table 5 |
| RQ3 | Does measured reliability propagate into flood damage models? | Exhaustive model-family enumeration, coefficient stability, performance drift; Figures 4, 8, 9 |

## What it does not contain

The household survey data. Responses can identify participating households and
cannot be released under the terms on which participants gave consent.

The script therefore generates a **synthetic dataset** with the same structure,
variable names and missingness pattern as the real survey, so that the analysis
runs end to end and every step can be inspected and verified. Numerical results
from the synthetic data will not match those in the manuscript; the code path is
identical.

To reproduce the published results, replace `simulate_survey()` and
`simulate_wave2()` with a read of the two-wave survey file. Everything
downstream is unchanged.

## Requirements

R ≥ 4.2, with the following CRAN packages (all open source):

`dplyr`, `tidyr`, `purrr`, `ggplot2`, `irr`, `irrCAC`, `lme4`, `broom`

The script installs any that are missing.

## Running it

```r
source("recall_reliability_fdm.R")
```

Runtime is a few minutes, dominated by the RQ3 model-family enumeration
(382 specifications per event group in the demonstration; 8,104 in the paper,
where the predictor pool is larger).

## Licence

MIT. See `LICENSE`.
