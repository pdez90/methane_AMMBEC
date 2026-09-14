# 23_source_attribution.R -----------------------------------------------------
# Inventory-proportional allocation of the top-down urban methane budget (NOT an
# inversion). Two quantities are taken from the atmospheric data, per flight:
#   (1) the tracer-scaled total urban CH4 emission E_total, from the PRIMARY box-
#       consistent method only: CH4:CO x GRA2PES (script 15), accepted only where
#       its reliability flag usable_co is TRUE. We deliberately do NOT merge this
#       with the CH4:CO2/Vulcan estimate or with any closed-loop value (no flight
#       passes closure), because averaging heterogeneous estimators is ill-defined.
#   (2) the fossil fraction f from the airborne ethane:methane ratio.
# These split each flight's total: E_fossil = f*E_total, E_biogenic = (1-f)*E_total.
# The campaign-representative fossil and biogenic totals are the PAIRED medians
# median(E_total*f) and median(E_total*(1-f)) -- not median(E)*median(f), which is
# not the median of the product. The fine split WITHIN each class (gas distribution
# vs post-meter; landfills vs wastewater) is the only inventory-borrowed quantity:
# each broad total is allocated across sub-sectors in proportion to the EPA gridded
# GHGI shares within that class. This is an illustrative allocation, not an
# independent, grid-resolved source attribution.
#
# Out: <OUT_DIR>/source_attribution.csv, source_attribution_representative.csv,
#      <OUT_DIR>/figures/Fig5_attribution.png   (manuscript Figure 4)
# Run: Rscript scripts/23_source_attribution.R   (after 14, 15, 21)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

em  <- read.csv(file.path(OUT_DIR, "emission_estimates.csv"), stringsAsFactors = FALSE)
rmf <- read.csv(file.path(OUT_DIR, "ratio_method_flux.csv"),  stringsAsFactors = FALSE)
inv <- read.csv(file.path(OUT_DIR, "inventory_comparison.csv"), stringsAsFactors = FALSE)

# observed fossil fraction per flight (from ethane); primary total = CH4:CO/GRA2PES
em$fossil  <- rmf$fossil_frac_york[match(em$flight, rmf$flight)]
em$E_total <- em$CH4CO_GRA2PES_t_hr          # already gated on usable_co in script 21
ok <- is.finite(em$E_total) & is.finite(em$fossil)
a  <- em[ok, ]
a$E_fossil   <- round(a$fossil * a$E_total, 2)
a$E_biogenic <- round((1 - a$fossil) * a$E_total, 2)
att <- data.frame(flight = a$flight, method = "CH4:CO x GRA2PES",
                  E_total_t_hr = round(a$E_total, 2), fossil_pct = round(100 * a$fossil),
                  E_fossil_t_hr = a$E_fossil, E_biogenic_t_hr = a$E_biogenic)
write.csv(att, file.path(OUT_DIR, "source_attribution.csv"), row.names = FALSE)

# within-class inventory shares (the only inventory-borrowed quantity)
fsec <- inv[inv$group == "fossil",   ]; fsec$share <- fsec$t_hr / sum(fsec$t_hr)
bsec <- inv[inv$group == "biogenic", ]; bsec$share <- bsec$t_hr / sum(bsec$t_hr)

# campaign-representative totals: PAIRED medians (median of the product)
Ef <- if (nrow(a)) stats::median(a$E_total * a$fossil) else NA_real_
Eb <- if (nrow(a)) stats::median(a$E_total * (1 - a$fossil)) else NA_real_
E_rep <- Ef + Eb; f_rep <- if (is.finite(E_rep) && E_rep > 0) Ef / E_rep else NA_real_
rep_tbl <- rbind(
  data.frame(class = "fossil",   sector = fsec$sector, top_down_t_hr = round(fsec$share * Ef, 3),
             inventory_t_hr = round(fsec$t_hr, 3)),
  data.frame(class = "biogenic", sector = bsec$sector, top_down_t_hr = round(bsec$share * Eb, 3),
             inventory_t_hr = round(bsec$t_hr, 3)))
rep_tbl <- rep_tbl[order(-rep_tbl$top_down_t_hr), ]
write.csv(rep_tbl, file.path(OUT_DIR, "source_attribution_representative.csv"), row.names = FALSE)

message(sprintf("Representative urban CH4 (CH4:CO x GRA2PES, %d usable flights): total = %.1f t/hr, fossil = %.0f%%",
                nrow(a), E_rep, 100*f_rep))
message(sprintf("  fossil    = %.2f t/hr  (allocated across gas-system sub-sectors by inventory share)", Ef))
message(sprintf("  biogenic  = %.2f t/hr  (dominated by landfills + wastewater)", Eb))
# cross-check with the secondary anchor, reported separately (not merged)
v_ok <- is.finite(em$CH4CO2_Vulcan_t_hr) & is.finite(em$fossil)
if (any(v_ok)) message(sprintf("  [secondary CH4:CO2 x Vulcan, %d flights: total median %.1f t/hr]",
                sum(v_ok), stats::median(em$CH4CO2_Vulcan_t_hr[v_ok])))
message("\nPer-flight allocation:"); print(att, row.names = FALSE)
message("\nTop sectors (top-down allocation vs EPA inventory, t/hr):"); print(head(rep_tbl, 8), row.names = FALSE)

# ---- Figure 4 (file Fig5_attribution.png): allocated split vs EPA inventory ----
inv_f <- sum(fsec$t_hr); inv_b <- sum(bsec$t_hr); inv_c <- sum(inv$t_hr[inv$group == "combustion"])
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "Fig5_attribution.png"), width = 1150, height = 780, res = 150)
par(mar = c(4, 4.5, 3, 1))
M <- cbind(`Top-down (CH4:CO x GRA2PES)` = c(Ef, Eb, 0),
           `EPA GHGI inventory`         = c(inv_f, inv_b, inv_c))
bp <- barplot(M, beside = FALSE, col = c("#c0392b", "#27ae60", "#7f8c8d"),
              ylab = "Urban CH4 emission (t/hr)", ylim = c(0, max(colSums(M))*1.22),
              main = "Airborne-derived source split vs bottom-up inventory")
legend("topright", fill = c("#c0392b", "#27ae60", "#7f8c8d"),
       legend = c("fossil (gas system)", "biogenic (landfills, wastewater)", "combustion"), bty = "n", cex = 0.9)
text(bp, colSums(M) + max(colSums(M))*0.03, adj = c(0.5, 0),
     labels = sprintf("%.1f t/hr\n%.0f%% fossil", colSums(M), 100*c(Ef/(Ef+Eb), inv_f/(inv_f+inv_b))), cex = 0.85)
dev.off()
message("\nWrote source_attribution.csv, source_attribution_representative.csv, figures/Fig5_attribution.png")
