# 38_gra2pes_county_summary.R -------------------------------------------------
# Reproducible county/state methane summary from the GRA2PES v2.0beta county
# spreadsheet (US_counties_GRA2PESv2.0beta_total_trends_2021-2023.xlsx,
# "Summary Data" sheet). Answers: national methane total, state ranking, and the
# Denver seven-county total, for a chosen year/month/day-type. Replaces an
# earlier ad-hoc extraction; base R + readxl only.
#
# Run:  Rscript scripts/38_gra2pes_county_summary.R
#       Rscript scripts/38_gra2pes_county_summary.R /path/to/counties.xlsx 2023 7 weekdy
# Out:  <OUT_DIR>/gra2pes_county_methane_<YYYY>_<MM>_<dow>.csv     (per-state)
#       <OUT_DIR>/figures/gra2pes_v2_methane_by_state.png
# ----------------------------------------------------------------------------
if (!requireNamespace("readxl", quietly = TRUE))
  stop("This script needs the 'readxl' package: install.packages('readxl')")

proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else path.expand("~/MethaneData_outputs")
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

args  <- commandArgs(trailingOnly = TRUE)
XLSX  <- if (length(args) >= 1) args[1] else
  path.expand("~/MethaneData/GRA2PES_v2/US_counties_GRA2PESv2.0beta_total_trends_2021-2023.xlsx")
YEAR  <- if (length(args) >= 2) as.integer(args[2]) else 2023L
MONTH <- if (length(args) >= 3) as.integer(args[3]) else 7L
DOW   <- if (length(args) >= 4) args[4] else "weekdy"
if (!file.exists(XLSX)) stop("spreadsheet not found: ", XLSX)

DENVER7 <- c("08001","08005","08013","08014","08031","08035","08059")   # 7-county metro FIPS

# --- read the Summary Data sheet; match columns by pattern (robust to header text)
raw <- readxl::read_excel(XLSX, sheet = "Summary Data")
nm  <- names(raw)
col <- function(pat) { i <- grep(pat, nm, ignore.case = TRUE); if (!length(i)) stop("no column ~ ", pat); nm[i[1]] }
c_sec <- col("^Sector$"); c_yr <- col("^Year$"); c_mo <- col("^Month$"); c_dow <- col("DayOfWeek")
c_fips<- col("^FIPS$");   c_st <- col("State");  c_ch4 <- col("CH4"); c_co <- col("^CO \\(")

d <- as.data.frame(raw)
d[[c_fips]] <- sprintf("%05s", as.character(d[[c_fips]]))     # keep leading zeros
keep <- tolower(d[[c_sec]]) == "total" & as.integer(d[[c_yr]]) == YEAR &
        as.integer(d[[c_mo]]) == MONTH & d[[c_dow]] == DOW
d <- d[keep, ]
if (!nrow(d)) stop("no rows for ", YEAR, "-", MONTH, " ", DOW, " sector=total")
d$ch4_t_hr <- as.numeric(d[[c_ch4]]) / 24     # mt/d -> t/hr

# --- aggregates ---------------------------------------------------------------
nat <- sum(d$ch4_t_hr, na.rm = TRUE)
byst <- aggregate(ch4_t_hr ~ get(c_st), d, sum); names(byst) <- c("state","ch4_t_hr")
byst <- byst[order(-byst$ch4_t_hr), ]; byst$rank <- seq_len(nrow(byst))
den  <- sum(d$ch4_t_hr[d[[c_fips]] %in% DENVER7], na.rm = TRUE)
co_rank <- byst$rank[byst$state == "CO"]

cat(sprintf("\nGRA2PES v2.0beta methane, %d-%02d %s, sector=total (%d counties)\n", YEAR, MONTH, DOW, nrow(d)))
cat(sprintf("  national        : %.0f t/hr  (%.1f Tg/yr)\n", nat, nat*8.766/1000))
cat(sprintf("  Colorado        : %.1f t/hr  (rank #%d of %d states)\n", byst$ch4_t_hr[byst$state=="CO"], co_rank, nrow(byst)))
cat(sprintf("  Denver 7-county : %.1f t/hr  (%.1f%% of national)\n", den, 100*den/nat))
cat("  top 8 states    : ", paste(sprintf("%s %.0f", head(byst$state,8), head(byst$ch4_t_hr,8)), collapse=", "), "\n")

write.csv(byst, file.path(OUT, sprintf("gra2pes_county_methane_%d_%02d_%s.csv", YEAR, MONTH, DOW)), row.names = FALSE)

# --- figure: top-20 states, Colorado highlighted ------------------------------
top <- head(byst, 20); top <- top[order(top$ch4_t_hr), ]
base_col <- "#3a7d7b"; accent <- "#d1691e"; ink <- "#1c2321"; muted <- "#6b7671"
cols <- ifelse(top$state == "CO", accent, base_col)
png(file.path(OUT, "figures", "gra2pes_v2_methane_by_state.png"), width = 1500, height = 1450, res = 200)
par(mar = c(6.5,5,5.5,7), family = "sans")
bp <- barplot(top$ch4_t_hr, horiz = TRUE, col = cols, border = NA, names.arg = top$state,
              las = 1, xlim = c(0, max(top$ch4_t_hr)*1.12), cex.names = 0.95, col.axis = muted)
text(top$ch4_t_hr + max(top$ch4_t_hr)*0.012, bp, sprintf("%.0f", top$ch4_t_hr), adj = 0, cex = 0.82,
     col = ifelse(top$state=="CO", accent, ink), xpd = NA, font = ifelse(top$state=="CO",2,1))
title(main = "GRA2PES v2.0 beta methane by state", adj = 0, cex.main = 1.35, col.main = ink, font.main = 2, line = 3.0)
mtext(sprintf("%d-%02d %s  ·  top 20 states  ·  Colorado highlighted", YEAR, MONTH, DOW),
      side = 3, adj = 0, line = 1.4, cex = 0.92, col = muted)
mtext("t CH4 / hr", side = 1, line = 2.4, cex = 0.95, col = muted)
mtext(sprintf("National total %.0f t/hr (%.0f Tg/yr).  Colorado ranks #%d of %d,", nat, nat*8.766/1000, co_rank, nrow(byst)),
      side = 1, adj = 0, line = 4.2, cex = 0.82, col = muted)
mtext("in line with its oil, gas and agriculture — not an outlier.", side = 1, adj = 0, line = 5.1, cex = 0.82, col = muted)
dev.off()
cat("wrote", file.path(OUT, "figures", "gra2pes_v2_methane_by_state.png"), "\n")
