# Local research data

This repository does not distribute participant-level research data.

Authorized researchers place the publication source files in `data/raw/`. The complete pipeline verifies the required filenames and schemas before analysis. No automated download is available.

`data/derived/` contains reproducible participant-level intermediate objects created by `run_analysis.R`. Both directories are ignored except for their placeholder files. Do not add their contents to Git.

Required source filenames are documented in the top-level README and `config/analysis.yml`.
