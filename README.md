## Burnout in Trauma Surgeons: A Systematic Review and Meta-Analysis

[![DOI](https://zenodo.org/badge/DOI/10.1136/tsaco-2025-001873.svg)](https://doi.org/10.1136/tsaco-2025-001873)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-4.4.1-blue.svg)](https://cran.r-project.org/)

This repository contains the data and R code for reproducing the meta-analysis of burnout prevalence among trauma surgeons, including comparative analyses with other surgical specialities.

# Overview

This meta-analysis investigates burnout prevalence among trauma surgeons through a systematic review of published literature. The analysis includes:

* Primary meta-analysis of burnout prevalence 
* Subgroup analysis comparing MBI vs non-MBI studies
* Publication bias assessment
* Sensitivity analyses
* Meta-regression examining moderator effects
* Comparative analysis with other surgical specialities

## Data files
meta_trauma_extraction.csv
Contains extracted data from included studies with fields:

burnout_comparison_updated.csv
Contains comparative data across surgical specialities with fields

## Analysis Scripts
main-ma.R
Primary meta-analysis script

MBI-MR.R
Conducts meta-regression and subgroup analyses

comparison.R
Generates comparative analysis forest plot


## AI Statement

This code was edited with the assistance of Claude Sonnet 3.5 (Anthropic, San Francisco: CA)

## Citation
If you use this code or data, please cite:

<details>
<summary>BibTeX</summary>
<pre><code>@article{kirdarsmith2025burnout,
  title={Burnout among trauma surgeons: a systematic review and meta-analysis},
  author={Kirdar-Smith, Sebastian and Knight, Alec and Twumasi, Ricardo},
  journal={Trauma Surgery \& Acute Care Open},
  year={2025},
  volume={10},
  pages={e001873},
  doi={10.1136/tsaco-2025-001873},
  url={https://doi.org/10.1136/tsaco-2025-001873}
}
</code></pre>
</details>

<details>
<summary>APA</summary>
<pre><code>Kirdar-Smith, S., Knight, A., & Twumasi, R. (2025). Burnout among trauma surgeons: a systematic review and meta-analysis. Trauma Surgery & Acute Care Open, 10, e001873. https://doi.org/10.1136/tsaco-2025-001873
</code></pre>
</details>

<details>
<summary>Vancouver</summary>
<pre><code>Kirdar-Smith S, Knight A, Twumasi R. Burnout among trauma surgeons: a systematic review and meta-analysis. Trauma Surgery & Acute Care Open. 2025;10:e001873. https://doi.org/10.1136/tsaco-2025-001873
</code></pre>
</details>
