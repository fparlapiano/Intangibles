# 1. FATTEL 2019: original supplier ATECO -> 14 categories -> firm-year
# Mapping version: v02 (7021 -> marketing; 781/7810 -> management; 7410 -> technical)
# Run this script once. It sources 0_firm_id.R automatically.

library(arrow)
library(dplyr)
library(stringr)
library(tidyr)

# 0. Paths
fy <- 2019L
f_raw <- "/home/group/main/rdc/private/FATTELGQ/dati/parquet/FATTELGQ_2019.parquet"
dir_project <- "~/corporate finance"
dir_code <- file.path(dir_project, "code")
dir_data <- file.path(dir_project, "data")
dir_work <- file.path("/tmp/work", Sys.info()[["user"]], "intangibles")

dir.create(dir_work, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(f_raw), dir.exists(dir_code), dir.exists(dir_data))
stopifnot(file.access(dir_work, 2) == 0)

f_ateco <- file.path(dir_work, "fattel_fy_2019_ateco_v02.parquet")
f_wide <- file.path(dir_work, "fattel_fy_2019_categories_v02.parquet")
f_map <- file.path(dir_work, "fattel_ateco_mapping_v02.csv")
f_audit <- file.path(dir_work, "fattel_fy_2019_audit_v02.csv")
f_changes <- file.path(dir_work, "fattel_reclassified_codes_v02.csv")
f_final <- file.path(dir_data, "fattel_fy_2019_categories_v02.parquet")

# Never overwrite the original 2019 cross-section or a prior v02 run
stopifnot(!file.exists(f_ateco), !file.exists(f_wide), !file.exists(f_map))
stopifnot(!file.exists(f_audit), !file.exists(f_changes), !file.exists(f_final))

# 1. Cerved selection
source(file.path(dir_code, "0_firm_id.R"))

# 2. Aggregate FATTEL by buyer and original supplier ATECO
# Outliers excluded; negative invoice amounts retained
ds <- open_dataset(f_raw)
q <- ds %>%
  filter(cessionario %in% list_cf$cf) %>%
  mutate(intra_vat = if_else(
    flag_gruppo_iva_cedente == 1 &
      flag_gruppo_iva_cessionario == 1 &
      !is.na(codice_fiscale_gruppo_iva_cedente) &
      !is.na(codice_fiscale_gruppo_iva_cessionario) &
      codice_fiscale_gruppo_iva_cedente != "" &
      !str_detect(codice_fiscale_gruppo_iva_cedente, "^0+$") &
      codice_fiscale_gruppo_iva_cedente == codice_fiscale_gruppo_iva_cessionario,
    1L, 0L, missing = 0L
  ))

df <- q %>%
  filter(is.na(flag_outlier) | flag_outlier != 1L) %>%
  group_by(cessionario, ateco_cedente) %>%
  summarise(
    exp = sum(imponibile, na.rm = TRUE),
    n_doc = sum(n_documenti, na.rm = TRUE),
    exp_intra = sum(if_else(intra_vat == 1L, imponibile, 0), na.rm = TRUE),
    n_doc_intra = sum(if_else(intra_vat == 1L, n_documenti, 0L), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()

df$year <- fy
stopifnot(is.character(df$cessionario))
stopifnot(!anyDuplicated(df[c("cessionario", "ateco_cedente")]))
stopifnot(sum(!unique(df$cessionario) %in% list_cf$cf) == 0)

cat("Cerved firms:", nrow(list_cf), "\n")
cat("FATTEL buyers:", length(unique(df$cessionario)), "\n")
cat("Buyer-ATECO observations:", nrow(df), "\n")
cat("Original supplier ATECO codes:", length(unique(df$ateco_cedente)), "\n")

# Detailed checkpoint: essential for auditing and any future reclassification
write_parquet(df, f_ateco, compression = "snappy")
stopifnot(file.exists(f_ateco), file.info(f_ateco)$size > 0)

# 3. Map original supplier codes to 14 expenditure categories
map <- data.frame(ateco_cedente = unique(as.character(df$ateco_cedente)))
map$code2 <- substr(map$ateco_cedente, 1, 2)
map$category <- case_when(
  is.na(map$ateco_cedente) ~ "residual",
  str_detect(map$ateco_cedente, "^0+$") ~ "residual",
  str_starts(map$ateco_cedente, "72") ~ "rd",
  str_starts(map$ateco_cedente, "62") |
    str_starts(map$ateco_cedente, "631") |
    str_starts(map$ateco_cedente, "582") ~ "software",
  map$ateco_cedente %in% c("58", "63") ~ "ambiguous_intangible",
  map$ateco_cedente == "7021" ~ "marketing",  # v02: public relations
  str_starts(map$ateco_cedente, "73") ~ "marketing",
  str_starts(map$ateco_cedente, "702") ~ "management",
  map$ateco_cedente %in% c("781", "7810") ~ "management", # v02: recruitment
  map$ateco_cedente == "70" ~ "ambiguous_intangible",
  map$ateco_cedente == "7410" ~ "technical",   # v02: specialized design
  str_starts(map$ateco_cedente, "71") ~ "technical",
  map$ateco_cedente %in% c("855", "8552", "8559", "856", "8560") ~ "training",
  map$ateco_cedente == "85" ~ "ambiguous_intangible",
  map$code2 %in% sprintf("%02d", c(1:3, 5:25, 31:32)) ~ "intermediates",
  map$code2 %in% sprintf("%02d", 26:30) ~ "capital",
  map$code2 %in% sprintf("%02d", 35:39) ~ "utilities",
  map$code2 %in% sprintf("%02d", 41:43) ~ "construction",
  map$code2 %in% sprintf("%02d", 45:47) ~ "trade",
  map$code2 %in% sprintf("%02d", 49:99) |
    map$code2 == "33" ~ "other_services",
  TRUE ~ "residual"
)

cats <- c("rd", "software", "marketing", "management",
          "technical", "training", "ambiguous_intangible",
          "intermediates", "capital", "utilities",
          "construction", "trade", "other_services", "residual")

stopifnot(!anyNA(map$category), all(map$category %in% cats))
stopifnot(all(map$category[map$ateco_cedente == "7021"] == "marketing"))
stopifnot(all(map$category[map$ateco_cedente %in% c("781", "7810")] == "management"))
stopifnot(all(map$category[map$ateco_cedente == "7410"] == "technical"))
write.csv(map, f_map, row.names = FALSE)

# Audit the four source codes separately; a code may be absent in 2019
change_codes <- c("7021", "781", "7810", "7410")
change_audit <- df %>%
  filter(ateco_cedente %in% change_codes) %>%
  group_by(ateco_cedente) %>%
  summarise(n_firms = n(), exp = sum(exp), n_doc = sum(n_doc),
            exp_intra = sum(exp_intra), n_doc_intra = sum(n_doc_intra),
            .groups = "drop")
change_audit$category_v02 <- map$category[match(change_audit$ateco_cedente,
                                                 map$ateco_cedente)]
print(change_audit)
write.csv(change_audit, f_changes, row.names = FALSE)

df$category <- map$category[match(df$ateco_cedente, map$ateco_cedente)]
stopifnot(!anyNA(df$category))

# 4. Aggregate by buyer and expenditure category
db <- df %>%
  group_by(cessionario, category) %>%
  summarise(
    exp = sum(exp, na.rm = TRUE),
    n_doc = sum(n_doc, na.rm = TRUE),
    exp_intra = sum(exp_intra, na.rm = TRUE),
    n_doc_intra = sum(n_doc_intra, na.rm = TRUE),
    n_ateco = n(),
    .groups = "drop"
  )
db$year <- fy

# 5. Reconcile supplier ATECO data with the category aggregation
audit <- data.frame(
  n_initial = nrow(df),
  n_assigned = sum(db$n_ateco),
  n_category_rows = nrow(db),
  n_firms = length(unique(df$cessionario)),
  exp_initial = sum(df$exp, na.rm = TRUE),
  exp_final = sum(db$exp, na.rm = TRUE),
  docs_initial = sum(df$n_doc, na.rm = TRUE),
  docs_final = sum(db$n_doc, na.rm = TRUE),
  intra_initial = sum(df$exp_intra, na.rm = TRUE),
  intra_final = sum(db$exp_intra, na.rm = TRUE),
  intra_docs_initial = sum(df$n_doc_intra, na.rm = TRUE),
  intra_docs_final = sum(db$n_doc_intra, na.rm = TRUE)
)

tol <- max(0.05, 1e-11 * sum(abs(df$exp), na.rm = TRUE))
tol_intra <- max(0.05, 1e-11 * sum(abs(df$exp_intra), na.rm = TRUE))
stopifnot(audit$n_initial == audit$n_assigned)
stopifnot(audit$docs_initial == audit$docs_final)
stopifnot(audit$intra_docs_initial == audit$intra_docs_final)
stopifnot(abs(audit$exp_initial - audit$exp_final) <= tol)
stopifnot(abs(audit$intra_initial - audit$intra_final) <= tol_intra)
print(audit, digits = 16)

# 6. One row per firm, 14 categories in columns
db$category <- factor(db$category, levels = cats)
db_wide <- db %>%
  select(-year) %>%
  pivot_wider(
    id_cols = cessionario,
    names_from = category,
    values_from = c(exp, n_doc, exp_intra, n_doc_intra, n_ateco),
    names_glue = "{.value}_{category}",
    values_fill = 0,
    names_expand = TRUE
  )

db_wide$year <- fy
stopifnot(!anyDuplicated(db_wide$cessionario))
stopifnot(nrow(db_wide) == length(unique(df$cessionario)))
stopifnot(all(paste0("exp_", cats) %in% names(db_wide)))
stopifnot(all(paste0("exp_intra_", cats) %in% names(db_wide)))

# Reconcile the wide file (exp and exp_intra are distinct measures)
exp_wide <- sum(vapply(db_wide[paste0("exp_", cats)], sum,
                       numeric(1), na.rm = TRUE))
docs_wide <- sum(vapply(db_wide[paste0("n_doc_", cats)], sum,
                        numeric(1), na.rm = TRUE))
intra_wide <- sum(vapply(db_wide[paste0("exp_intra_", cats)], sum,
                         numeric(1), na.rm = TRUE))
intra_docs_wide <- sum(vapply(db_wide[paste0("n_doc_intra_", cats)], sum,
                              numeric(1), na.rm = TRUE))

stopifnot(abs(exp_wide - audit$exp_final) <= tol)
stopifnot(docs_wide == audit$docs_final)
stopifnot(abs(intra_wide - audit$intra_final) <= tol_intra)
stopifnot(intra_docs_wide == audit$intra_docs_final)

audit$n_wide_rows <- nrow(db_wide)
audit$exp_wide <- exp_wide
audit$docs_wide <- docs_wide
audit$intra_wide <- intra_wide
audit$intra_docs_wide <- intra_docs_wide
write.csv(audit, f_audit, row.names = FALSE)

# 7. Save and check the revised firm-year file in temporary work space
write_parquet(db_wide, f_wide, compression = "snappy")
check <- read_parquet(f_wide, col_select = c("cessionario", "year", "exp_rd"))
stopifnot(nrow(check) == nrow(db_wide))
stopifnot(!anyDuplicated(check$cessionario))
stopifnot(identical(check$cessionario, db_wide$cessionario))
stopifnot(all(check$year == fy))
stopifnot(isTRUE(all.equal(check$exp_rd, db_wide$exp_rd)))
rm(check)

# Copy the final data and audit files into the permanent project folder
# If the home quota is exhausted, keep verified copies under /tmp/work
f_partial <- paste0(f_final, ".partial")
stopifnot(!file.exists(f_partial))
ok <- file.copy(f_wide, f_partial, overwrite = FALSE)
if (ok && file.info(f_partial)$size == file.info(f_wide)$size) {
  stopifnot(file.rename(f_partial, f_final))
  cat("Permanent firm-year file:", f_final, "\n")
} else {
  if (file.exists(f_partial)) unlink(f_partial)
  warning("Home storage quota may be full. Validated firm-year file remains at: ", f_wide)
}

for (f in c(f_map, f_audit, f_changes)) {
  dest <- file.path(dir_data, basename(f))
  if (!file.exists(dest)) {
    ok <- file.copy(f, dest, overwrite = FALSE)
    if (!ok) warning("Could not copy audit file: ", dest)
  }
}

print(data.frame(
  file = c(f_ateco, f_wide, f_map, f_audit, f_changes),
  size_mb = file.info(c(f_ateco, f_wide, f_map, f_audit, f_changes))$size / 1024^2
))

# /tmp/work may be cleared: archive the detailed ATECO checkpoint permanently
# when storage space is available. The old v01 file remains untouched.
