# Reproducibility

## Analysis workflow

`Rscript run_analysis.R` runs the analysis in this order:

1. Construct endpoints and cohorts.
2. Fit and calibrate the fixed Stage 1 model.
3. Fit the seven reported Stage 2 strategies.
4. Evaluate the cascade in Cambodia and calculate 5,000 validation-only bootstrap replicates.
5. Reproduce and check all tables.
6. Reproduce and check all figures.

The expected cohorts are:

| Cohort | Children | Events | Outcome |
|---|---:|---:|---|
| Stage 1 development | 2,578 | 92 | Death or organ support within two days |
| Stage 2 development | 2,581 | 97 | Parent-study severe illness |
| Cambodia validation | 824 | 36 | Parent-study severe illness |

The analysis seed is 202504. The bootstrap seed is 209504. Bootstrap samples are stratified by the parent-study outcome and Stage 1 disposition. Models are not refitted within bootstrap samples.

The pipeline stops if data, cohort counts, predictions, coefficients, tables, or figure values differ from the fixed references. Prediction and fitted-model tolerance is `1e-12`. Bootstrap interval tolerance is `5e-12`. Displayed table values must match exactly.

## Published model files

The `models/` directory contains only the parameters required for prediction:

| File | Contents |
|---|---|
| `stage1_preprocessor.rds` | Fitted preprocessing and compact imputation trees |
| `stage1_booster.ubj` | Fitted XGBoost trees |
| `stage1_calibration.json` | Weighted Platt calibration parameters |
| `stage2_models.json` | Stage 2 medians, strategies, offsets, and coefficients |
| `browser_model_bundle.json` | Browser-compatible form of the same prediction parameters |

These files contain no participant rows, identifiers, outcomes, predictions, resamples, or model frames. Full fitted objects that contain analysis data are rebuilt only under ignored `data/derived/`. `tools/build_model_bundle.R` removes training vectors and verifies the minimal files before distribution.

## Browser equivalence

`R/prediction.R` is the scientific prediction reference. `tools/export_browser_model.R` converts its fixed artifacts for browser use without fitting or changing a model.

`npm run test:parity` compares the browser engine with R using fixed manual inputs and deterministic synthetic inputs. It covers Stage 1 preprocessing and calibration, all seven Stage 2 strategies, threshold boundaries, missing values, and weight-for-age calculation. Probability differences must not exceed `1e-12`, and classifications must match exactly.

## Offline application

The web interface calls a local Web Worker, which calls the browser scoring engine and the versioned model bundle. Prediction does not require a server or network connection. Inputs remain in page memory and are not saved or transmitted.

The interface, scoring engine, and model bundle are released as one unit. The application stops without calculating if files are missing, corrupted, incompatible, or incomplete. There is no remote scoring fallback.

## Generated files

Participant-level intermediate files remain under ignored `data/derived/`. Generated tables, figures, and reports remain under ignored `outputs/`.

Files under `tests/reference/` contain aggregate results only. They contain no participant rows or identifiers.
