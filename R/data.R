# Authorized source-file checks —-

spot_required_source_paths <- function(config) {
  unlist(lapply(config$source_files, function(name) file.path(config$paths$raw, name)), use.names = TRUE)
}

spot_check_source_files <- function(config) {
  paths <- spot_required_source_paths(config)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Restricted study data are not present. Place these authorized files in data/raw/: ",
      paste(basename(missing), collapse = ", "),
      ". No data are downloaded by this repository.",
      call. = FALSE
    )
  }

  invisible(paths)
}

spot_require_columns <- function(data, columns, source_name) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(source_name, " is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

spot_date <- function(x) as.Date(x, format = "%d-%b-%y")

# Endpoint construction —-

spot_make_endpoints <- function(enrolment, follow_up, discharge) {
  spot_require_columns(
    enrolment,
    c("Label", "ENDAT", "IPDOPD"),
    "Enrolment source"
  )
  spot_require_columns(
    follow_up,
    c("Label", "FUALI", "FUD2RSN", "FUDIEDAT", "FUCARE", "FUADMYN", "FUMEDYN", "FUD2ORG"),
    "Follow-up source"
  )
  spot_require_columns(
    discharge,
    c("Label", "DCDAT", "DCDIEDAT", "DCORGDAT", "DCALI", "DCORG", "DCDIE"),
    "Discharge source"
  )

  follow_discharge <- merge(follow_up, discharge, by = "Label", all.x = TRUE, sort = FALSE)
  rows <- merge(enrolment, follow_discharge, by = "Label", all.x = TRUE, sort = FALSE)
  rows$ENDAT <- spot_date(rows$ENDAT)
  rows$DCDAT <- spot_date(rows$DCDAT)
  rows$FUDIEDAT <- spot_date(rows$FUDIEDAT)
  rows$DCDIEDAT <- spot_date(rows$DCDIEDAT)
  rows$DCORGDAT <- spot_date(rows$DCORGDAT)

  days_to_discharge <- as.numeric(rows$DCDAT - rows$ENDAT)
  days_to_follow_up_death <- as.numeric(rows$FUDIEDAT - rows$ENDAT)
  days_to_discharge_death <- as.numeric(rows$DCDIEDAT - rows$ENDAT)
  days_to_organ_support <- as.numeric(rows$DCORGDAT - rows$ENDAT)

  death_positive <-
    rows$FUALI %in% 0 |
    rows$FUD2RSN %in% 3 |
    (!is.na(days_to_follow_up_death) & days_to_follow_up_death <= 2) |
    (!is.na(days_to_discharge_death) & days_to_discharge_death <= 2) |
    (rows$DCALI %in% 0 & !is.na(days_to_discharge) & days_to_discharge <= 2)
  death_negative <-
    rows$FUALI %in% 1 |
    rows$DCALI %in% 1 |
    (!is.na(days_to_follow_up_death) & days_to_follow_up_death > 2) |
    (!is.na(days_to_discharge_death) & days_to_discharge_death > 2)
  death_d2 <- ifelse(
    death_positive & !death_negative,
    1,
    ifelse(death_negative & !death_positive, 0, NA_real_)
  )

  organ_positive <- rows$DCORG %in% 1 & !is.na(days_to_organ_support) & days_to_organ_support <= 2
  organ_negative <-
    (rows$DCORG %in% 1 & !is.na(days_to_organ_support) & days_to_organ_support > 2) |
    rows$DCORG %in% 0
  organ_d2 <- ifelse(
    organ_positive & !organ_negative,
    1,
    ifelse(organ_negative & !organ_positive, 0, NA_real_)
  )

  discharge_positive <- rows$DCDIE %in% 1 & !is.na(days_to_discharge) & days_to_discharge <= 2
  discharge_negative <-
    (rows$DCDIE %in% 1 & !is.na(days_to_discharge) & days_to_discharge > 2) |
    rows$DCDIE %in% 0
  discharge_d2 <- ifelse(
    discharge_positive & !discharge_negative,
    1,
    ifelse(discharge_negative & !discharge_positive, 0, NA_real_)
  )

  outpatient_safe_zero <-
    (rows$IPDOPD == "O" & rows$FUCARE %in% 0) |
    (rows$IPDOPD == "O" & rows$FUCARE %in% 1 & rows$FUADMYN %in% 0 & rows$FUMEDYN %in% 0) |
    (rows$IPDOPD == "O" & rows$FUCARE %in% 1 & rows$FUMEDYN %in% 1 & rows$FUADMYN %in% 0) |
    (rows$IPDOPD == "O" & rows$FUD2ORG %in% 0)
  inpatient_safe_zero <-
    rows$IPDOPD == "I" & !is.na(days_to_discharge) & days_to_discharge %in% c(0, 1)
  safe_zero_d2 <- outpatient_safe_zero | inpatient_safe_zero

  death_d2[is.na(death_d2) & safe_zero_d2] <- 0
  organ_d2[
    is.na(organ_d2) &
      ((safe_zero_d2 & rows$IPDOPD == "O") | rows$DCORG %in% 0)
  ] <- 0
  discharge_d2[is.na(discharge_d2) & safe_zero_d2 & rows$IPDOPD == "O"] <- 0

  stage1_outcome <- ifelse(
    death_d2 == 1 | organ_d2 == 1,
    1,
    ifelse(death_d2 == 0 & organ_d2 == 0, 0, NA_real_)
  )

  data.frame(
    label = as.character(rows$Label),
    death_d2 = death_d2,
    organ_support_d2 = organ_d2,
    discharge_home_end_of_life_d2 = discharge_d2,
    stage1_outcome = stage1_outcome,
    stringsAsFactors = FALSE
  )
}

# Sampling weights —-

spot_sampling_weights <- function(site, care_setting) {
  outpatient <- c(
    bd006 = 34, kh005 = 64, id003 = 2, la004 = 22,
    la011 = 5, vn009 = 66, vn010 = 50
  )
  weight <- ifelse(care_setting == "I", 1, unname(outpatient[site]))
  if (any(!is.finite(weight) | weight <= 0)) {
    stop("A site or care-setting value has no sampling-weight definition.", call. = FALSE)
  }
  as.numeric(weight)
}

# Analysis dataset assembly —-

spot_load_analysis_data <- function(config) {
  paths <- spot_check_source_files(config)

  # Read the four authorized source files.
  follow_up <- as.data.frame(read.csv(paths[["follow_up"]], check.names = FALSE))
  discharge <- as.data.frame(read.csv(paths[["discharge"]], check.names = FALSE))
  enrolment <- as.data.frame(read.csv(paths[["enrolment"]], check.names = FALSE))
  fields <- as.data.frame(readxl::read_excel(paths[["analysis_fields"]], sheet = 1))

  # Validate the analysis-file contract before deriving endpoints.
  required_fields <- c(
    "label", "site", "ipdopd", "age.months", "sex", "adm.recent", "wfaz",
    "cidysymp", "not.alert", "hr.inf", "hr.child", "rr.inf", "rr.child",
    "envhtemp", "crt.long", "oxy.ra", "STREM1", "CRP", "lbglu", "outcome.binary"
  )
  spot_require_columns(fields, required_fields, "Analysis-fields source")
  if (anyNA(fields$label) || anyDuplicated(fields$label)) {
    stop("Analysis-fields labels must be present and unique.", call. = FALSE)
  }

  # Align the objective endpoint data to the analysis rows by participant label.
  endpoints <- spot_make_endpoints(enrolment, follow_up, discharge)
  if (anyNA(endpoints$label) || anyDuplicated(endpoints$label)) {
    stop("Endpoint labels must be present and unique.", call. = FALSE)
  }
  endpoint_index <- match(fields$label, endpoints$label)
  if (anyNA(endpoint_index)) stop("Some analysis rows have no endpoint source row.", call. = FALSE)

  fields$death_d2 <- endpoints$death_d2[endpoint_index]
  fields$organ_support_d2 <- endpoints$organ_support_d2[endpoint_index]
  fields$discharge_home_end_of_life_d2 <- endpoints$discharge_home_end_of_life_d2[endpoint_index]
  fields$stage1_outcome <- endpoints$stage1_outcome[endpoint_index]
  fields$stage2_outcome <- as.numeric(fields$outcome.binary)
  fields$hr.all <- ifelse(!is.na(fields$hr.inf), fields$hr.inf, fields$hr.child)
  fields$rr.all <- ifelse(!is.na(fields$rr.inf), fields$rr.inf, fields$rr.child)
  fields$spo2 <- fields$oxy.ra
  fields$strem1 <- fields$STREM1
  fields$crp <- fields$CRP
  fields$glucose <- fields$lbglu
  fields$sampling_weight <- spot_sampling_weights(fields$site, fields$ipdopd)

  # Split the development and held-out validation cohorts.
  retained <- fields[!is.na(fields$stage2_outcome), , drop = FALSE]
  stage1_development <- fields[
    fields$site != config$analysis$held_out_site & !is.na(fields$stage1_outcome),
    , drop = FALSE
  ]
  stage2_development <- retained[retained$site != config$analysis$held_out_site, , drop = FALSE]
  validation <- retained[retained$site == config$analysis$held_out_site, , drop = FALSE]

  # Fail if the expected cohort fingerprint changes.
  checks <- c(
    parent_rows = nrow(fields),
    paper_rows = nrow(retained),
    paper_events = sum(retained$stage2_outcome == 1),
    stage1_rows = nrow(stage1_development),
    stage1_events = sum(stage1_development$stage1_outcome == 1),
    stage2_rows = nrow(stage2_development),
    stage2_events = sum(stage2_development$stage2_outcome == 1),
    validation_rows = nrow(validation),
    validation_events = sum(validation$stage2_outcome == 1)
  )
  expected <- c(
    parent_rows = 3423L, paper_rows = 3405L, paper_events = 133L,
    stage1_rows = 2578L, stage1_events = 92L,
    stage2_rows = 2581L, stage2_events = 97L,
    validation_rows = 824L, validation_events = 36L
  )
  if (any(checks != expected)) {
    stop(
      "Cohort fingerprint failed. Observed: ",
      paste(names(checks), checks, sep = "=", collapse = ", "),
      call. = FALSE
    )
  }
  if (sum(validation$stage1_outcome == 1, na.rm = TRUE) != 36L) {
    stop("The validation endpoint cross-check failed.", call. = FALSE)
  }

  list(
    parent = fields,
    paper = retained,
    stage1_development = stage1_development,
    stage2_development = stage2_development,
    validation = validation,
    cohort_fingerprint = checks
  )
}
