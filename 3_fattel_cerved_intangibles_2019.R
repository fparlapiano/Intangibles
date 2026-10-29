# 3. Cerved 2019 x FATTEL 2019: intangible expenditure and intensity
# Run 2_cerved_2014_2024.R first (creates db and cv19).
# FATTEL: EUR, signed net expenditure. Cerved accounts: thousands of EUR.
# The full 2014-2024 Cerved panel remains available as cv_panel.

library(arrow)
library(dplyr)

# 0. Directories and source file
f_fattel <- "~/corporate finance/data/fattel_fy_2019_categories_v02.parquet"
dir_work <- file.path("/tmp/work", Sys.info()[["user"]], "intangibles")
dir_data <- "~/corporate finance/data"
dir.create(dir_work, recursive = TRUE, showWarnings = FALSE)

stopifnot(exists("cv19"), exists("db"), file.exists(f_fattel))
stopifnot(dir.exists(dir_data), dir.exists(dir_work))

# Keep the full Cerved panel; use db for the 2019 analytical cross-section.
cv_panel <- db
db <- cv19

# 1. Check Cerved and FATTEL keys before joining
cv_vars <- c("cf", "year", "revenues_cerved", "assets_cerved",
             "materials_cerved", "intangible_capex_cerved",
             "intangible_fixed_assets_cerved")
stopifnot(all(cv_vars %in% names(db)))
stopifnot(all(db$year == 2019), !anyNA(db$cf))
stopifnot(!anyDuplicated(db[c("cf", "year")]))
stopifnot(all(vapply(db[cv_vars[3:7]], is.numeric, logical(1))))

fy <- read_parquet(f_fattel)
cat("Cerved 2019 before join:", nrow(db), "\n")
cat("FATTEL 2019 before join:", nrow(fy), "\n")

ints <- c("rd", "software", "marketing", "management", "technical", "training")
cats <- c(ints, "ambiguous_intangible", "intermediates", "capital",
          "utilities", "construction", "trade", "other_services", "residual")
exp_cols <- paste0("exp_", cats)
intan_cols <- paste0("exp_", ints)
intra_cols <- paste0("exp_intra_", ints)

stopifnot(all(c("cessionario", "year", exp_cols, intra_cols) %in% names(fy)))
stopifnot(all(fy$year == 2019), !anyNA(fy$cessionario))
stopifnot(!anyDuplicated(fy[c("cessionario", "year")]))
stopifnot(all(nchar(fy$cessionario) == 16))

# 2. Audit matching BEFORE the join
matched <- db$cf %in% fy$cessionario
n_matched <- sum(matched)

audit_match <- data.frame(
  n_cerved = nrow(db),
  n_fattel = nrow(fy),
  n_matched = n_matched,
  n_cerved_unmatched = sum(!matched),
  n_fattel_unmatched = sum(!fy$cessionario %in% db$cf),
  share_cerved_matched = mean(matched),
  share_fattel_matched = mean(fy$cessionario %in% db$cf)
)
print(audit_match)

# Check FATTEL expenditure retained for matched Cerved firms
sel <- fy$cessionario %in% db$cf
exp_expected <- sum(vapply(fy[sel, exp_cols, drop = FALSE],
                           sum, numeric(1), na.rm = TRUE))

# 3. One-to-one join, Cerved 2019 as master
fy$matched_fattel <- 1L
n_before <- nrow(db)
db <- left_join(db, fy, by = c("cf" = "cessionario", "year" = "year"))
db$matched_fattel[is.na(db$matched_fattel)] <- 0L
rm(fy, matched, sel)

stopifnot(nrow(db) == n_before)
stopifnot(!anyDuplicated(db[c("cf", "year")]))
stopifnot(sum(db$matched_fattel) == n_matched)
stopifnot(all(db$year == 2019))

exp_joined <- sum(vapply(db[exp_cols], sum, numeric(1), na.rm = TRUE))
tol <- max(0.05, 1e-11 * abs(exp_expected))
stopifnot(abs(exp_joined - exp_expected) <= tol)
cat("Expenditure in matched firms, before/after join:",
    format(exp_expected, digits = 16), format(exp_joined, digits = 16), "\n")

# 4. FATTEL intangible expenditure (all six categories; retain signed amounts)
ix <- db$matched_fattel == 1L
stopifnot(!anyNA(db[ix, intan_cols, drop = FALSE]))
stopifnot(!anyNA(db[ix, intra_cols, drop = FALSE]))

db$intan_exp <- NA_real_
db$intan_exp[ix] <- rowSums(db[ix, intan_cols, drop = FALSE])
db$intan_exp_intra <- NA_real_
db$intan_exp_intra[ix] <- rowSums(db[ix, intra_cols, drop = FALSE])
db$intan_exp_external <- db$intan_exp - db$intan_exp_intra

# 5. Denominators: convert Cerved thousands of EUR into EUR
# Missing, zero or negative denominators lead to NA intensity.
db$rev_eur <- 1000 * db$revenues_cerved
db$assets_eur <- 1000 * db$assets_cerved
db$materials_eur <- 1000 * db$materials_cerved

# Total FATTEL intangible expenditure intensity
db$intan_rev <- ifelse(db$rev_eur > 0, db$intan_exp / db$rev_eur, NA_real_)
db$intan_assets <- ifelse(db$assets_eur > 0, db$intan_exp / db$assets_eur, NA_real_)
db$intan_materials <- ifelse(db$materials_eur > 0, db$intan_exp / db$materials_eur, NA_real_)

# Six FATTEL components / revenues
db$rd_rev <- ifelse(db$rev_eur > 0, db$exp_rd / db$rev_eur, NA_real_)
db$software_rev <- ifelse(db$rev_eur > 0, db$exp_software / db$rev_eur, NA_real_)
db$marketing_rev <- ifelse(db$rev_eur > 0, db$exp_marketing / db$rev_eur, NA_real_)
db$management_rev <- ifelse(db$rev_eur > 0, db$exp_management / db$rev_eur, NA_real_)
db$technical_rev <- ifelse(db$rev_eur > 0, db$exp_technical / db$rev_eur, NA_real_)
db$training_rev <- ifelse(db$rev_eur > 0, db$exp_training / db$rev_eur, NA_real_)

# Six FATTEL components / total assets
db$rd_assets <- ifelse(db$assets_eur > 0, db$exp_rd / db$assets_eur, NA_real_)
db$software_assets <- ifelse(db$assets_eur > 0, db$exp_software / db$assets_eur, NA_real_)
db$marketing_assets <- ifelse(db$assets_eur > 0, db$exp_marketing / db$assets_eur, NA_real_)
db$management_assets <- ifelse(db$assets_eur > 0, db$exp_management / db$assets_eur, NA_real_)
db$technical_assets <- ifelse(db$assets_eur > 0, db$exp_technical / db$assets_eur, NA_real_)
db$training_assets <- ifelse(db$assets_eur > 0, db$exp_training / db$assets_eur, NA_real_)

# Six FATTEL components / material costs
db$rd_materials <- ifelse(db$materials_eur > 0, db$exp_rd / db$materials_eur, NA_real_)
db$software_materials <- ifelse(db$materials_eur > 0, db$exp_software / db$materials_eur, NA_real_)
db$marketing_materials <- ifelse(db$materials_eur > 0, db$exp_marketing / db$materials_eur, NA_real_)
db$management_materials <- ifelse(db$materials_eur > 0, db$exp_management / db$materials_eur, NA_real_)
db$technical_materials <- ifelse(db$materials_eur > 0, db$exp_technical / db$materials_eur, NA_real_)
db$training_materials <- ifelse(db$materials_eur > 0, db$exp_training / db$materials_eur, NA_real_)

# 6. Cerved benchmark: accounting intangible CAPEX (FLOW), same denominators
# Cerved numerator and denominator are both measured in thousands of EUR.
db$cv_intan_rev <- ifelse(db$revenues_cerved > 0,
                           db$intangible_capex_cerved / db$revenues_cerved, NA_real_)
db$cv_intan_assets <- ifelse(db$assets_cerved > 0,
                              db$intangible_capex_cerved / db$assets_cerved, NA_real_)
db$cv_intan_materials <- ifelse(db$materials_cerved > 0,
                                 db$intangible_capex_cerved / db$materials_cerved, NA_real_)

# Separate book-stock benchmark; not directly comparable with expenditure flows.
db$cv_book_intan_assets <- ifelse(db$assets_cerved > 0,
                                   db$intangible_fixed_assets_cerved / db$assets_cerved,
                                   NA_real_)

# Absolute difference: FATTEL expenditure minus Cerved capitalized CAPEX, in EUR.
db$cv_intan_capex_eur <- 1000 * db$intangible_capex_cerved
db$intan_gap_eur <- db$intan_exp - db$cv_intan_capex_eur

# 7. Audit denominator coverage and compare intensity distributions
ok_rev <- is.finite(db$intan_rev) & is.finite(db$cv_intan_rev)
ok_assets <- is.finite(db$intan_assets) & is.finite(db$cv_intan_assets)
ok_materials <- is.finite(db$intan_materials) & is.finite(db$cv_intan_materials)

audit_denom <- data.frame(
  denominator = c("revenues", "assets", "materials"),
  n_cerved_positive = c(sum(db$rev_eur > 0, na.rm = TRUE),
                        sum(db$assets_eur > 0, na.rm = TRUE),
                        sum(db$materials_eur > 0, na.rm = TRUE)),
  n_fattel_matched_positive = c(sum(ix & db$rev_eur > 0, na.rm = TRUE),
                                sum(ix & db$assets_eur > 0, na.rm = TRUE),
                                sum(ix & db$materials_eur > 0, na.rm = TRUE)),
  n_comparable = c(sum(ok_rev), sum(ok_assets), sum(ok_materials))
)
print(audit_denom)

audit_compare <- data.frame(
  denominator = c("revenues", "assets", "materials"),
  n = c(sum(ok_rev), sum(ok_assets), sum(ok_materials)),
  median_fattel = c(median(db$intan_rev[ok_rev]),
                    median(db$intan_assets[ok_assets]),
                    median(db$intan_materials[ok_materials])),
  median_cerved = c(median(db$cv_intan_rev[ok_rev]),
                    median(db$cv_intan_assets[ok_assets]),
                    median(db$cv_intan_materials[ok_materials])),
  p90_fattel = c(unname(quantile(db$intan_rev[ok_rev], 0.90)),
                 unname(quantile(db$intan_assets[ok_assets], 0.90)),
                 unname(quantile(db$intan_materials[ok_materials], 0.90))),
  p90_cerved = c(unname(quantile(db$cv_intan_rev[ok_rev], 0.90)),
                 unname(quantile(db$cv_intan_assets[ok_assets], 0.90)),
                 unname(quantile(db$cv_intan_materials[ok_materials], 0.90))),
  spearman = c(
    if (sum(ok_rev) > 1) cor(db$intan_rev[ok_rev], db$cv_intan_rev[ok_rev],
                             method = "spearman") else NA_real_,
    if (sum(ok_assets) > 1) cor(db$intan_assets[ok_assets], db$cv_intan_assets[ok_assets],
                                method = "spearman") else NA_real_,
    if (sum(ok_materials) > 1) cor(db$intan_materials[ok_materials],
                                   db$cv_intan_materials[ok_materials],
                                   method = "spearman") else NA_real_
  )
)
print(audit_compare, digits = 5, row.names = FALSE)

# Correlation of each FATTEL component with Cerved total intangible CAPEX intensity.
corr_rev <- cor(db[c(paste0(ints, "_rev"), "cv_intan_rev")],
                use = "pairwise.complete.obs", method = "spearman")
print(corr_rev[paste0(ints, "_rev"), "cv_intan_rev"])

# 8. Save ONE merged analytical 2019 file plus small audit tables
f_out <- file.path(dir_work, "cerved_fattel_2019_intan_v01.parquet")
stopifnot(!file.exists(f_out))  # prevent accidental replacement of a prior run
write_parquet(db, f_out, compression = "snappy")
stopifnot(file.exists(f_out), file.info(f_out)$size > 0)

check <- read_parquet(f_out, col_select = c("cf", "year", "matched_fattel", "intan_exp"))
stopifnot(nrow(check) == n_before)
stopifnot(!anyDuplicated(check$cf))
stopifnot(sum(check$matched_fattel) == n_matched)
stopifnot(isTRUE(all.equal(check$intan_exp, db$intan_exp)))
rm(check)

write.csv(audit_match, file.path(dir_work, "cerved_fattel_2019_match_audit.csv"), row.names = FALSE)
write.csv(audit_denom, file.path(dir_work, "cerved_fattel_2019_denominator_audit.csv"), row.names = FALSE)
write.csv(audit_compare, file.path(dir_work, "cerved_fattel_2019_intensity_comparison.csv"), row.names = FALSE)

# Attempt permanent backup; keep /tmp/work original if the home quota blocks copying.
f_perm <- file.path(dir_data, "cerved_fattel_2019_intan_v01.parquet")
if (!file.exists(f_perm)) {
  copied <- file.copy(f_out, f_perm, overwrite = FALSE)
  if (!copied) message("Permanent copy failed (possibly home quota); file remains at: ", f_out)
} else {
  message("Permanent output already exists; not overwritten: ", f_perm)
}

print(data.frame(file = f_out, size_mb = file.info(f_out)$size / 1024^2))
cat("Full Cerved panel: cv_panel (2014-2024). Merged cross-section: db (2019).\n")

# 9. Remove temporary objects; retain db, cv_panel and audit tables
rm(cv19, cv_vars, ints, cats, exp_cols, intan_cols, intra_cols,
   n_matched, n_before, exp_expected, exp_joined, tol, ix,
   ok_rev, ok_assets, ok_materials, corr_rev)
gc(verbose = FALSE)
