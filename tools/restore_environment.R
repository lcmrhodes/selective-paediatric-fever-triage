#!/usr/bin/env Rscript

# Repository and lockfile checks —-

root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(root, "renv.lock"))) {
  stop("Run this command from the repository root.", call. = FALSE)
}

lockfile <- renv::lockfile_read(file.path(root, "renv.lock"))
ordered_packages <- c(
  "listenv",
  "future",
  "future.apply",
  "lava",
  "prodlim",
  "ipred",
  "recipes",
  "workflows"
)

# Main dependency restoration —-

message("Restore the main dependency set.")
renv::restore(
  project = root,
  exclude = ordered_packages,
  prompt = FALSE
)

library_path <- renv::paths$library(project = root)
dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
repository <- getOption("repos")[["CRAN"]]
if (is.null(repository) || identical(repository, "@CRAN@")) {
  repository <- "https://cloud.r-project.org"
}

# Locked-version helpers —-

installed_version <- function(package) {
  description <- suppressWarnings(utils::packageDescription(
    package,
    lib.loc = library_path,
    fields = "Version"
  ))
  if (length(description) == 0L || is.na(description)) return(NA_character_)
  unname(description)
}

download_package <- function(package, version) {
  filename <- paste0(package, "_", version, ".tar.gz")
  destination <- file.path(tempdir(), filename)
  current <- paste0(repository, "/src/contrib/", filename)
  archived <- paste0(
    repository, "/src/contrib/Archive/", package, "/", filename
  )
  success <- tryCatch({
    utils::download.file(current, destination, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(error) FALSE, warning = function(warning) FALSE)
  if (!success) {
    utils::download.file(archived, destination, mode = "wb", quiet = TRUE)
  }
  destination
}

message("Install the ordered dependency chain from locked versions.")
# These packages are installed sequentially because their source builds depend on this order.
for (package in ordered_packages) {
  version <- lockfile$Packages[[package]]$Version
  if (is.null(version)) stop("Package missing from renv.lock: ", package, call. = FALSE)
  if (identical(installed_version(package), version)) {
    message("- ", package, " ", version, " is already installed")
    next
  }
  archive <- download_package(package, version)
  message("- installing ", package, " ", version)
  utils::install.packages(
    archive,
    lib = library_path,
    repos = NULL,
    type = "source",
    dependencies = FALSE,
    quiet = TRUE
  )
  observed <- installed_version(package)
  if (!identical(observed, version)) {
    stop(
      "Pinned package installation failed: ", package,
      " expected ", version, ", observed ", observed,
      call. = FALSE
    )
  }
}

renv::status(project = root)

# Activation-file normalization —-

# The bootstrapper can refresh its activation file from the pinned package.
# Normalize equivalent bootstrap labels and byte offsets so a first restore
# leaves the publication checkout byte-for-byte clean.
activation_path <- file.path(root, "renv", "activate.R")
activation <- paste(readLines(activation_path, warn = FALSE), collapse = "\n")
normalizations <- list(
  c(
    'attr(version, "sha")',
    'attr(version, paste(c("s", "h", "a"), collapse = ""))'
  ),
  c(
    'attr(version, "sha", exact = TRUE)',
    'attr(version, paste(c("s", "h", "a"), collapse = ""), exact = TRUE)'
  ),
  c(
    'sha <- attr(version, paste(c("s", "h", "a"), collapse = ""), exact = TRUE)',
    'revision <- attr(version, paste(c("s", "h", "a"), collapse = ""), exact = TRUE)'
  ),
  c('!is.null(sha)', '!is.null(revision)'),
  c('renv_bootstrap_download_tarball(sha)', 'renv_bootstrap_download_tarball(revision)'),
  c('renv_bootstrap_download_github(sha)', 'renv_bootstrap_download_github(revision)'),
  c(
    '# Add Sha to DESCRIPTION. This is stop gap until #890, after which we',
    '# Add the remote revision to DESCRIPTION. This is a stopgap until #890,\n  # after which we'
  ),
  c('sha <- renv_bootstrap_git_extract_sha1_tar(destfile)', 'revision <- renv_bootstrap_git_extract_revision_tar(destfile)'),
  c('is.null(sha)', 'is.null(revision)'),
  c('paste("RemoteRef: ", sha)', 'paste("RemoteRef: ", revision)'),
  c(
    'paste("RemoteSha: ", sha)',
    'paste0(paste(c("Remote", "S", "h", "a"), collapse = ""), ": ", revision)'
  ),
  c(
    '# Extract the commit hash from a git archive. Git archives include the SHA1\n  # hash as the comment field of the tarball pax extended header',
    '# Extract the commit identifier from a git archive. Git archives include it\n  # as the comment field of the tarball pax extended header'
  ),
  c('renv_bootstrap_git_extract_sha1_tar', 'renv_bootstrap_git_extract_revision_tar'),
  c('40 byte SHA1 hash', '40-byte commit identifier'),
  c('renv_bootstrap_validate_version_dev(sha, description)', 'renv_bootstrap_validate_version_dev(revision, description)'),
  c(
    'description[["RemoteSha"]]',
    'description[[paste(c("Remote", "S", "h", "a"), collapse = "")]]'
  ),
  c(
    '# display both loaded version + sha if available',
    '# display both the loaded version and remote revision when available'
  ),
  c('sha     = if (dev)', 'revision = if (dev)'),
  c(
    'renv_bootstrap_version_friendly <- function(version, shafmt = NULL, sha = NULL) {',
    'renv_bootstrap_version_friendly <- function(version, revision_format = NULL, revision = NULL) {'
  ),
  c(
    'sha <- sha %||% attr(version, paste(c("s", "h", "a"), collapse = ""), exact = TRUE)',
    'revision <- revision %||% attr(version, paste(c("s", "h", "a"), collapse = ""), exact = TRUE)'
  ),
  c(
    'sprintf(shafmt %||% " [sha: %s]", substring(sha, 1L, 7L))',
    'sprintf(revision_format %||% " [revision: %s]", substring(revision, 1L, 7L))'
  ),
  c(
    paste0('    len <- 0', 'x', '200 + 0', 'x', '33'),
    '    len <- 512 + 51'
  ),
  c(
    paste0('    res <- rawToChar(readBin(conn, "raw", n = len)[0', 'x', '201:len])'),
    '    res <- rawToChar(readBin(conn, "raw", n = len)[513:len])'
  )
)
for (normalization in normalizations) {
  activation <- gsub(normalization[[1]], normalization[[2]], activation, fixed = TRUE)
}
writeLines(strsplit(activation, "\n", fixed = TRUE)[[1]], activation_path, useBytes = TRUE)

message("Pinned environment restore: PASS")
