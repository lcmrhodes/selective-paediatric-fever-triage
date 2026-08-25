#!/usr/bin/env bash
set -euo pipefail

repository_root="$(git rev-parse --show-toplevel)"
cd "$repository_root"

failed=0
while IFS= read -r path; do
  case "$path" in
    data/raw/.gitkeep|data/derived/.gitkeep|data/README.md) ;;
    data/raw/*|data/derived/*)
      echo "Prohibited tracked research-data path: $path"
      failed=1
      ;;
  esac
  case "$path" in
    *.RData|*.rda|*.xlsx|*.xls|*.parquet|*.feather|*.ipynb|*.log|*.Rout|*.env)
      echo "Prohibited tracked file type: $path"
      failed=1
      ;;
    *.rds)
      if [[ "$path" != "models/stage1_preprocessor.rds" ]]; then
        echo "Unexpected tracked R object: $path"
        failed=1
      fi
      ;;
    *.csv)
      if [[ "$path" != tests/reference/* ]]; then
        echo "Unexpected tracked CSV: $path"
        failed=1
      fi
      ;;
  esac
done < <(git ls-files)

while IFS= read -r path; do
  case "$path" in
    .git/*|renv/library/*|data/raw/*|data/derived/*|outputs/*) ;;
    *)
      echo "Unexpected local environment, cache, or build file: $path"
      failed=1
      ;;
  esac
done < <(find . -type f \( \
  -name '.env' -o -name '.env.*' -o -name '*.RData' -o -name '*.rda' \
  -o -name '*.xlsx' -o -name '*.xls' -o -name '*.parquet' -o -name '*.feather' \
  -o -name '*.ipynb' -o -name '*.log' -o -name '*.Rout' \
\) -print | sed 's#^[.]/##')

secret_pattern='[A][K][I][A][A-Z0-9]{16}|[g][h][p]_[A-Za-z0-9]{20,}|[s][k]-[A-Za-z0-9]{20,}|BEGIN[[:space:]]+(RSA[[:space:]]+)?PRIVATE[[:space:]]+KEY'
if git grep -I -n -E "$secret_pattern" -- .; then
  echo "A credential-like value is present in tracked files."
  failed=1
fi

while IFS= read -r commit; do
  if git grep -I -n -E "$secret_pattern" "$commit" -- .; then
    echo "A credential-like value is present in Git history."
    failed=1
  fi
done < <(git rev-list --all)

while read -r object_type object_size object_name; do
  if [[ "$object_type" == "blob" && "$object_size" -gt 10485760 ]]; then
    echo "Git object exceeds 10 MiB: $object_name ($object_size bytes)"
    failed=1
  fi
done < <(
  git rev-list --objects --all |
    git cat-file --batch-check='%(objecttype) %(objectsize) %(rest)'
)

if [[ "$failed" -ne 0 ]]; then
  exit 1
fi

echo "Repository distribution check: PASS"
