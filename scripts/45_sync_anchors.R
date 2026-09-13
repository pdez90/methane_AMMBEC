# 45_sync_anchors.R -----------------------------------------------------------
# Compare the hand-transcribed anchors in config.R against the values the pipeline
# has actually emitted, and (with --apply) rewrite config.R so the constants equal
# today's outputs. This is the tool that keeps "reproducible from current code"
# true: scripts 16, 17, 18 and 36 are too heavy to live inside run_all.R, so their
# results are carried in config.R, and this script is what re-syncs them.
#
#   Rscript scripts/45_sync_anchors.R            # report only
#   Rscript scripts/45_sync_anchors.R --apply    # rewrite config.R
#
# Reads, when present, from OUT_DIR:
#   vulcan_co2_boxsum.csv                 -> E_CO2_DENVER          (script 16)
#   nei_co_metro.csv                      -> E_CO_NEI              (script 17)
#   gra2pes_CO_<month>_boxsum.csv         -> E_CO_DENVER           (script 18)
#   gra2pes_CH4_<month>_boxsum.csv        -> E_CH4_GRA2PES         (script 18)
#   gra2pes_CO_<month>_*county_box.csv    -> GRA2PES_BOX_OVER_COUNTY_CO (script 36)
# E_CO_NEI_BOX is derived in config.R from the last two, so it is never written here.
#
# AFTER APPLYING: re-run the pipeline (bash run_local.sh). Any anchor that moved
# changes emission_estimates.csv and paper_values.json, so the manuscript sentences
# and the expectations in scripts/verify_paper_values.R must be updated to match —
# this script prints exactly which, and does NOT touch them, because a verification
# file that updates itself verifies nothing.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
apply_it <- "--apply" %in% commandArgs(trailingOnly = TRUE)

# Find the outputs without needing environment variables: config.R's default OUT_DIR
# points at ~/MethaneData_outputs, which is not where this project writes after
# reorganize_data.sh. Prefer whichever candidate actually holds pipeline output.
if (!nzchar(Sys.getenv("METHANE_OUT_DIR"))) {
  sib <- normalizePath(file.path(proj, "..", "outputs"), mustWork = FALSE)
  has_out <- function(d) dir.exists(d) && length(list.files(d, pattern = "\\.csv$"))
  if (!has_out(OUT_DIR) && has_out(sib)) OUT_DIR <- sib
}
message("reading emitted anchors from: ", OUT_DIR)

rd <- function(f) {
  if (is.null(f) || !nzchar(f)) return(NULL)
  p <- file.path(OUT_DIR, f)
  if (!file.exists(p)) return(NULL)
  tryCatch(read.csv(p, stringsAsFactors = FALSE), error = function(e) NULL)
}
first_num <- function(d, cands) {
  if (is.null(d)) return(NA_real_)
  hit <- cands[cands %in% names(d)]
  if (!length(hit)) return(NA_real_)
  v <- suppressWarnings(as.numeric(d[[hit[1]]][1]))
  if (is.finite(v)) v else NA_real_
}
month_file <- function(pat, prefer = "v1.1") {
  # Pick a matching file, preferring the inventory version the anchor is DOCUMENTED
  # against rather than whichever ran last. Selecting purely by mtime meant that once
  # scripts 18/36 had been run for both v1.1 and v2.0beta, --apply could write a v2
  # number into config.R under a comment saying v1.1, mixing versions inside the
  # derived E_CO_NEI_BOX with nothing to show it had happened.
  f <- list.files(OUT_DIR, pattern = pat)
  if (!length(f)) return(NULL)
  if (length(f) > 1) {
    tagged <- grep(prefer, f, fixed = TRUE, value = TRUE)
    others <- setdiff(f, tagged)
    if (length(tagged) && length(others))
      message("  note: several candidates for this anchor (", paste(f, collapse = ", "),
              "); using the ", prefer, " one, which is what config.R documents.")
    if (length(tagged)) f <- tagged
  }
  f[order(file.info(file.path(OUT_DIR, f))$mtime, decreasing = TRUE)][1]
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
found <- list(
  E_CO2_DENVER  = first_num(rd("vulcan_co2_boxsum.csv"), c("Gg_CO2_yr", "Gg_yr")),
  E_CO_NEI      = first_num(rd("nei_co_metro.csv"), c("Gg_CO_yr", "Gg_yr", "E_CO_NEI")),
  E_CO_DENVER   = first_num(rd(month_file("^gra2pes_CO_\\d{6}.*boxsum\\.csv$") %||% ""),
                            c("Gg_per_yr", "Gg_yr", "Gg_CO_yr")),
  E_CH4_GRA2PES = first_num(rd(month_file("^gra2pes_CH4_\\d{6}.*boxsum\\.csv$") %||% ""),
                            c("t_per_hr", "t_hr", "t_CH4_hr")),
  GRA2PES_BOX_OVER_COUNTY_CO =
    first_num(rd(month_file("^gra2pes_CO_\\d{6}.*county_box\\.csv$") %||% ""),
              c("box_over_county", "frac_box_over_county", "ratio"))
)
current <- list(E_CO2_DENVER = E_CO2_DENVER, E_CO_NEI = E_CO_NEI,
                E_CO_DENVER = E_CO_DENVER, E_CH4_GRA2PES = E_CH4_GRA2PES,
                GRA2PES_BOX_OVER_COUNTY_CO = GRA2PES_BOX_OVER_COUNTY_CO)
digits <- list(E_CO2_DENVER = 1, E_CO_NEI = 1, E_CO_DENVER = 1,
               E_CH4_GRA2PES = 2, GRA2PES_BOX_OVER_COUNTY_CO = 4)

cat("anchor                       config      emitted     status\n")
changes <- list()
for (k in names(current)) {
  cur <- current[[k]]; new <- found[[k]]
  if (!is.finite(new)) {
    cat(sprintf("  %-26s %-11s %-11s not emitted yet (run its script)\n", k, format(cur), "-"))
    next
  }
  new <- round(new, digits[[k]])
  same <- isTRUE(all.equal(as.numeric(cur), new, tolerance = 1e-8))
  cat(sprintf("  %-26s %-11s %-11s %s\n", k, format(cur), format(new),
              if (same) "matches" else "DIFFERS"))
  if (!same) changes[[k]] <- new
}

if (!length(changes)) { cat("\nEvery emitted anchor matches config.R. Nothing to do.\n"); quit(save = "no") }

cat("\n", length(changes), " anchor(s) differ.\n", sep = "")
if (!apply_it) { cat("Re-run with --apply to rewrite config.R.\n"); quit(save = "no") }

cfg <- file.path(proj, "config.R")
txt <- readLines(cfg, warn = FALSE)
stamp <- format(Sys.Date(), "%d %b %Y")
for (k in names(changes)) {
  # Match the assignment line, keeping any trailing comment and env-var wrapper.
  i <- grep(sprintf("^%s\\s*<-", k), txt)
  if (!length(i)) { warning("no assignment line for ", k, " in config.R"); next }
  old <- txt[i[1]]
  # Replace the VALUE, not the first number-like token on the line. The earlier version
  # matched the leading digit inside the identifier itself, turning E_CO2_DENVER into
  # E_CO<new>_DENVER and GRA2PES_BOX_OVER_COUNTY_CO into GRA<new>PES_..., leaving the
  # number untouched while destroying the variable name -- and still reporting success.
  # The two real forms in config.R are:
  #     NAME <- 123.4
  #     NAME <- as.numeric(Sys.getenv("METHANE_X", unset = 123.4))
  # so anchor the match to `unset =` first, then to the assignment arrow.
  val <- format(changes[[k]], scientific = FALSE)
  num <- "[0-9]+(?:\\.[0-9]+)?"
  new_line <- NA_character_
  if (grepl(paste0("unset\\s*=\\s*", num), old, perl = TRUE)) {
    new_line <- sub(paste0("(unset\\s*=\\s*)", num), paste0("\\1", val), old, perl = TRUE)
  } else if (grepl(paste0("<-\\s*", num), old, perl = TRUE)) {
    new_line <- sub(paste0("(<-\\s*)", num), paste0("\\1", val), old, perl = TRUE)
  }
  if (is.na(new_line) || identical(new_line, old)) {
    warning("could not locate the numeric value on config.R's line for ", k,
            "; left unchanged:\n  ", trimws(old))
    next
  }
  # The identifier must survive untouched -- the exact failure the old regex caused.
  ident <- sub("\\s*<-.*$", "", old)
  if (!startsWith(new_line, ident))
    stop("refusing to write: rewriting ", k, " would change the identifier\n  before: ",
         trimws(old), "\n  after:  ", trimws(new_line))
  txt[i[1]] <- new_line
  txt <- append(txt, sprintf("# re-synced %s by scripts/45 from the pipeline's own output (was %s)",
                             stamp, format(current[[k]])), after = i[1] - 1)
  cat("  updated: ", trimws(old), "  ->  ", trimws(new_line), "\n", sep = "")
}
writeLines(txt, cfg)
cat("\nconfig.R rewritten. Now:\n",
    "  bash run_local.sh                      # re-run everything downstream\n",
    "  then update scripts/verify_paper_values.R and the manuscript for:\n", sep = "")
if ("E_CO_NEI" %in% names(changes) || "GRA2PES_BOX_OVER_COUNTY_CO" %in% names(changes))
  cat("    E_CO_NEI_BOX, nei_box_lo/hi/median (section 4.2 and Table of anchors)\n")
if ("E_CO_DENVER" %in% names(changes))
  cat("    the CH4:CO emission range gra_lo/hi/median (abstract, sections 4.2 and 5)\n")
if ("E_CO2_DENVER" %in% names(changes))
  cat("    the CH4:CO2 estimate and the Vulcan sentence in section 3.4\n")
if ("E_CH4_GRA2PES" %in% names(changes))
  cat("    the bottom-up GRA2PES methane comparison in section 5\n")
