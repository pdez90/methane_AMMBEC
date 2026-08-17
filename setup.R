# setup.R --------------------------------------------------------------------
# Dependency check for the AMMBEC Denver urban-methane pipeline.
#
#   Rscript setup.R
#
# The core analysis (readers, enhancements, ratios, the ratio-to-inventory
# emission estimate, Table 1, and the beta_source sensitivity) runs on BASE R
# ALONE. Packages are needed only for specific optional stages, listed below.
# -----------------------------------------------------------------------------
cat("R version:", R.version.string, "\n")
if (getRversion() < "4.0.0") {
  warning("R >= 4.0 recommended; you have ", getRversion())
}

# Which stage needs what. Everything here is OPTIONAL: run_all.R skips the
# corresponding stage (with a message) when the package or its input is absent.
needs <- data.frame(
  pkg    = c("ncdf4", "terra", "png", "sf", "tigris"),
  stage  = c("08 lidar boundary-layer height; wind-profiler option in 05",
             "14 gridded EPA GHGI; 16 Vulcan box sum; 18 GRA2PES box sum; 20 basemaps",
             "30 combined source figure (stacks Figures 4A and 4B)",
             "17 NEI county CO totals",
             "make_metro_outline.R (one-time; writes the committed county outline)"),
  stringsAsFactors = FALSE)

have <- vapply(needs$pkg, requireNamespace, logical(1), quietly = TRUE)
for (i in seq_len(nrow(needs))) {
  cat(sprintf("%-8s %-9s %s\n", needs$pkg[i], if (have[i]) "[present]" else "[MISSING]",
              needs$stage[i]))
}

missing <- needs$pkg[!have]
if (length(missing)) {
  cat("\nInstall the missing ones with:\n  install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))\n", sep = "")
  cat("Stages needing them are skipped automatically by run_all.R.\n")
} else {
  cat("\nAll optional packages present; the full pipeline can run.\n")
}

cat("\nData root (METHANE_DATA_DIR): ",
    path.expand(Sys.getenv("METHANE_DATA_DIR", unset = "~/MethaneData")), "\n", sep = "")
cat("Output root (METHANE_OUT_DIR): ",
    path.expand(Sys.getenv("METHANE_OUT_DIR",
                unset = file.path(dirname(path.expand(Sys.getenv("METHANE_DATA_DIR",
                                  unset = "~/MethaneData"))), "MethaneData_outputs"))), "\n", sep = "")
cat("\nSee README.md for how to obtain the input data, then run:  Rscript run_all.R\n")
