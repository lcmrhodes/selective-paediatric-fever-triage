# Selective biomarkers for paediatric fever triage

This repository contains the analysis code, fixed model files, tests, and the Progressive Web App related to the research:

> Selective augmentation for traffic-light triage of paediatric febrile illness: external validation in a prospective multicountry cohort

[Open Spot Sepsis](https://lcmrhodes.github.io/selective-paediatric-fever-triage/)

The provided application is for research and demonstration only. It is not validated or approved for clinical use. It does not replace clinical assessment or local care pathways.

## Models

Stage 1 estimates the risk of death or organ support within two days. It uses age, sex, recent hospital admission, weight-for-age z score, illness duration, alertness, heart rate, respiratory rate, axillary temperature, and prolonged capillary refill.

- Green: probability below 0.005
- Amber: probability from 0.005 through 0.02
- Red: probability above 0.02

Stage 2 runs only after an Amber Stage 1 result. Green and Red Stage 1 results do not change. Stage 2 uses the parent-study severe-illness outcome. This outcome includes discharge home for end-of-life care. A child who later met this definition was not suitable for reclassification to Green.

The seven Stage 2 strategies are:

1. SpO₂
2. sTREM-1
3. CRP + sTREM-1
4. CRP + sTREM-1 + glucose
5. SpO₂ + sTREM-1
6. SpO₂ + sTREM-1 + CRP
7. SpO₂ + sTREM-1 + CRP + glucose

## Restricted data

This repository contains no participant-level research data. Authorised researchers must put these unchanged files in `data/raw/`:

```text
AllFields_Follow-up_Nov2025.csv
AllFields_Discharge_Nov2025.csv
AllFields_Enrolment_Nov2025.csv
predictive.analysis.univariate_17Oct2024.xlsx
```

The pipeline does not download these files. It stops if a required file or field is absent. Do not commit files under `data/raw/` or `data/derived/`.

## Reproduce the results

Use R 4.6.0. From the repository root, run:

```bash
Rscript tools/restore_environment.R
Rscript run_analysis.R
```

`renv.lock` fixes the R package versions. The pipeline writes tables to `outputs/tables/` and figures to `outputs/figures/`. It writes its pass report to `outputs/verification/reproduction_report.json`.

Main tables use `main_table_1.csv` through `main_table_3.csv`. Supplementary tables use `supp_table_*.csv`. Each figure has PNG, PDF, and source-value files.

See [reproducibility.md](docs/reproducibility.md) for the analysis sequence, checks, seeds, and tolerances.

## Run tests

```bash
Rscript tests/testthat.R
npm ci
npm run test:all
```

The fixed test inputs contain no participant records or identifiers.

## Run the application

```bash
npm ci
npm run build
npm run serve
```

Open `http://127.0.0.1:4173/`. The application calculates predictions on the device. It does not transmit or store entered observations. After one complete load, it can operate without a network connection.

## Repository map

- `analysis/` and `R/`: analysis pipeline and R reference implementation
- `app/`: web application and local prediction engine
- `models/`: distributable prediction files
- `tests/`: model, output, parity, and PWA checks
- `docs/`: detailed reproducibility and deployment instructions

## Citation and license

Use [CITATION.cff](CITATION.cff) to cite the paper and repository. The MIT License applies to the code, documentation, and distributable model files. It does not grant access to the restricted study data.
