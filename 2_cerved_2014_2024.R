# 2. Cerved CR: prepare and audit the 2014-2024 panel
# Source: CervedX_cr_2005_24.RData
# Run after FATTEL 2019 extraction; this script does not merge or change FATTEL.

library(dplyr)
library(stringr)
library(arrow)

# 0. Paths
f_cerved <- "~/private/cerved_cr/CervedX_cr_2005_24.RData"
dir_data <- "~/corporate finance/data"
dir_work <- file.path("/tmp/work", Sys.info()[["user"]], "intangibles")
dir.create(dir_work, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(f_cerved), dir.exists(dir_data), dir.exists(dir_work))

# 1. Load Cerved and retain 2014-2024
# Loading RData requires memory for the entire source object.
e <- new.env(parent = emptyenv())
loaded <- load(f_cerved, envir = e)
stopifnot("CERVEDX_CR" %in% loaded)

db <- e$CERVEDX_CR
rm(e, loaded)

stopifnot(is.data.frame(db))
stopifnot(all(c("year", "n_codfisc") %in% names(db)))

n_original <- nrow(db)
db$year <- as.integer(as.character(db$year))
db <- db[!is.na(db$year) & db$year >= 2014 & db$year <= 2024, ]

cat("Original Cerved observations:", n_original, "\n")
cat("Cerved 2014-2024 observations:", nrow(db), "\n")
cat("Observations outside period / missing year:", n_original - nrow(db), "\n")
stopifnot(nrow(db) > 0, 2019 %in% db$year)

# 2. Construct a 16-character key for matching with FATTEL
# Do not convert fiscal identifiers from character to numeric.
if (is.numeric(db$n_codfisc)) {
  stopifnot(!any(abs(db$n_codfisc) > 2^53, na.rm = TRUE))
  cf <- format(db$n_codfisc, scientific = FALSE, trim = TRUE)
} else {
  cf <- as.character(db$n_codfisc)
}

cf <- trimws(cf)
cf[is.na(db$n_codfisc) | cf == ""] <- NA_character_
db$cf <- str_pad(cf, width = 16, side = "left", pad = "0")
db$valid_cf <- !is.na(db$cf) & grepl("^[0-9]{16}$", db$cf) &
  !grepl("^0+$", db$cf)
rm(cf)

# Check that the normalized key agrees with the original fiscal code where possible.
if ("codfisc" %in% names(db)) {
  cf_raw <- str_pad(trimws(as.character(db$codfisc)), 16, side = "left", pad = "0")
  valid_raw <- !is.na(cf_raw) & grepl("^[0-9]{16}$", cf_raw)
  comparable <- which(db$valid_cf & valid_raw)
  different <- comparable[db$cf[comparable] != cf_raw[comparable]]
  cat("Conflicting normalized/original fiscal codes:", length(different), "\n")
  if (length(different)) {
    write.csv(head(db[different, intersect(c("codfisc", "n_codfisc", "cf", "year"), names(db))], 1000),
              file.path(dir_work, "cerved_2014_2024_identifier_conflicts.csv"),
              row.names = FALSE)
    stop("Cerved fiscal identifier definitions disagree. Inspect before merging.")
  }
  rm(cf_raw, valid_raw, comparable, different)
}

# 3. Annual and identifier audits
ay <- db %>%
  group_by(year) %>%
  summarise(
    n_rows = n(),
    n_valid_cf = sum(valid_cf),
    n_missing_or_invalid_cf = sum(!valid_cf),
    n_distinct_cf = n_distinct(cf[valid_cf]),
    .groups = "drop"
  ) %>%
  arrange(year)

print(ay, n = 11)
# write.csv(ay, file.path(dir_work, "cerved_2014_2024_audit_year.csv"), row.names = FALSE)

# A firm-year must have a unique matching key; never silently drop duplicates.
idx <- which(db$valid_cf)
key <- paste(db$cf[idx], db$year[idx], sep = "_")
dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
cat("Duplicate valid Cerved firm-year rows:", sum(dup), "\n")

if (any(dup)) {
  cols <- intersect(c("cf", "year", "codfisc", "databil", "denomsta", "schema"), names(db))
  audit_dup <- db[idx[dup], cols]
  write.csv(head(audit_dup, 1000),
            file.path(dir_work, "cerved_2014_2024_duplicate_sample.csv"),
            row.names = FALSE)
  stop("Duplicate Cerved firm-year keys found. Inspect the duplicate sample before merging.")
}
rm(idx, key, dup)

# 4. Prepare 2019 Cerved cross-section for the forthcoming FATTEL merge
cv19 <- db[db$year == 2019 & db$valid_cf, ]
stopifnot(!anyDuplicated(cv19$cf), all(cv19$year == 2019))

vars <- c("revenues_cerved", "ebitda_cerved", "labour_cerved",
          "materials_cerved", "assets_cerved", "ateco07")
cat("Cerved 2019 firms with valid identifiers:", nrow(cv19), "\n")
cat("Available accounting fields:", paste(intersect(vars, names(cv19)), collapse = ", "), "\n")
cat("Missing accounting fields:", paste(setdiff(vars, names(cv19)), collapse = ", "), "\n")

# 5. Optional matching audit against the existing FATTEL 2019 cross-section
f_fattel <- file.path(dir_data, "fattel_fy_2019_categories_v02.parquet")
if (!file.exists(f_fattel))
  f_fattel <- file.path(dir_data, "fattel_fy_2019_categories.parquet")

if (file.exists(f_fattel)) {
  fy19 <- read_parquet(f_fattel, col_select = "cessionario")
  stopifnot(!anyDuplicated(fy19$cessionario))
  matched <- fy19$cessionario %in% cv19$cf
  audit_match <- data.frame(
    fattel_file = basename(f_fattel),
    n_fattel = nrow(fy19),
    n_cerved_2019 = nrow(cv19),
    n_matched = sum(matched),
    n_unmatched = sum(!matched),
    match_rate = mean(matched)
  )
  print(audit_match)
  write.csv(audit_match,
            file.path(dir_work, "cerved_fattel_2019_match_audit.csv"),
            row.names = FALSE)
  rm(fy19, matched)
}

# The full 2014-2024 panel is in db; the 2019 cross-section is in cv19.
# No accounting values or units are changed here; no merge is performed.
# Audit files are saved in /tmp/work to avoid the existing home-directory quota.
