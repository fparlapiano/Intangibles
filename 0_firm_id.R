# 0. Cerved fiscal identifiers for FATTEL 2019

library(stringr)

# Load the original Cerved firm universe
load("~/private/cerved_cr/unique_num_codf.RData")
stopifnot(exists("list_cf"), "num_codf" %in% names(list_cf))

# Preserve identifiers as character and pad to 16 digits
list_cf <- as.character(list_cf$num_codf)
list_cf <- as.data.frame(list_cf)
list_cf$cf <- str_pad(list_cf$list_cf, width = 16, side = "left", pad = "0")

# Audit selected fiscal codes
stopifnot(!anyNA(list_cf$cf))
stopifnot(all(grepl("^[0-9]{16}$", list_cf$cf)))
stopifnot(!anyDuplicated(list_cf$cf))

cat("Cerved fiscal codes:", nrow(list_cf), "\n")
