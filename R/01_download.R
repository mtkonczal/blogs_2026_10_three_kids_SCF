# 01_download.R
# Download the SCF Summary Extract Public Data (CSV-in-zip, "excel" files) for every
# modern survey wave, 1989-2025. (The full public data and replicate-weight files used by
# R/07, R/22-24 are not downloaded here; see README.md.)
#
# Source pattern: https://www.federalreserve.gov/econres/files/scfp{YEAR}excel.zip
# All historical extracts are re-released with each new wave and expressed in the
# dollars of the newest survey (here: 2025 dollars, CPI-U-RS "current methods").
# We record a manifest (md5 + timestamp) so later re-pulls can be diffed against
# this vintage rather than silently overwriting it.
#
# Usage: Rscript R/01_download.R            (skips files already present)
#        Rscript R/01_download.R --force    (re-downloads into a datestamped folder)

suppressPackageStartupMessages(library(tools))

root   <- getwd()   # run from the project root
if (!dir.exists(file.path(root, "R"))) stop("Run from the project root (folder containing R/).")
force  <- "--force" %in% commandArgs(trailingOnly = TRUE)
years  <- seq(1989, 2025, by = 3)
base   <- "https://www.federalreserve.gov/econres/files"

zip_dir <- file.path(root, "data_raw", "zips")
csv_dir <- file.path(root, "data_raw", "xlsx")
if (force) {
  stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  zip_dir <- file.path(root, "data_raw", paste0("zips_", stamp))
  csv_dir <- file.path(root, "data_raw", paste0("xlsx_", stamp))
}
dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
rel <- function(p) substring(p, nchar(root) + 2)   # manifest paths relative to project root

manifest <- list()
for (y in years) {
  url  <- sprintf("%s/scfp%dexcel.zip", base, y)
  dest <- file.path(zip_dir, basename(url))
  if (!file.exists(dest) || force) {
    message("Downloading ", url)
    status <- download.file(url, dest, mode = "wb", quiet = TRUE)
    if (status != 0) stop("Download failed for ", y)
  }
  unzip(dest, exdir = csv_dir, overwrite = TRUE)
  csv <- file.path(csv_dir, sprintf("SCFP%d.csv", y))
  if (!file.exists(csv)) stop("Expected ", csv, " after unzip; zip contents: ",
                              paste(unzip(dest, list = TRUE)$Name, collapse = ", "))
  manifest[[length(manifest) + 1]] <- data.frame(
    year = y, url = url, zip = rel(dest), zip_md5 = unname(md5sum(dest)),
    zip_bytes = file.size(dest), csv = rel(csv),
    file_mtime = format(file.mtime(dest), "%Y-%m-%d %H:%M:%S"),
    recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
}

manifest <- do.call(rbind, manifest)
write.csv(manifest, file.path(root, "data_raw", "download_manifest.csv"), row.names = FALSE)
print(manifest[, c("year", "zip_bytes", "zip_md5")])
message("Done. ", nrow(manifest), " waves in ", csv_dir)
