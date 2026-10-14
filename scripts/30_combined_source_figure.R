# 30_combined_source_figure.R -------------------------------------------------
# Manuscript Figure 4: the aggregate source-attribution bars (Fig5_attribution.png,
# script 23), the airborne-derived split against the EPA gridded GHGI and GRA2PES
# v2.0beta. The spatial fossil-signature maps (script 25) are no longer a panel of
# Figure 4: a cell-by-cell comparison of aircraft samples with an inventory does not
# account for transport (B. McDonald, Oct 2026), so the aircraft signature map is shown
# only in the SI (Figure S11, script 58) and the inventory comparison in the main text
# is the domain-wide one.
#
# Out: <OUT_DIR>/figures/Fig4_source_combined.png  (same file name as before)
# Run: Rscript scripts/30_combined_source_figure.R   (after script 23)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
FIG <- file.path(OUT_DIR, "figures")
fa  <- file.path(FIG, "Fig5_attribution.png")
if (!file.exists(fa)) stop("Missing ", fa, " -- run scripts/23_source_attribution.R first.")
ok <- file.copy(fa, file.path(FIG, "Fig4_source_combined.png"), overwrite = TRUE)
if (!ok) stop("could not write Fig4_source_combined.png")
message("Wrote ", file.path(FIG, "Fig4_source_combined.png"), " (Figure 4 = the attribution bars of script 23)")
