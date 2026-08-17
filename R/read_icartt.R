# read_icartt.R --------------------------------------------------------------
# Base-R reader for ICARTT (.ict) files — the format the NOAA Twin Otter
# AMMBEC data is distributed in:
#   https://csl.noaa.gov/groups/csl7/measurements/2024ammbec/TwinOtter/
# Supports ICARTT file-format index 1001 (single independent variable), which
# is what the AMMBEC "ARL-Suite" files use. No package dependencies.
# -----------------------------------------------------------------------------

# Sentinels ICARTT uses for missing / out-of-range values.
.ICARTT_MISSING <- c(-9999, -99999, -7777, -8888)

# Read a file's lines treating bytes as latin1, so a stray non-ASCII byte (e.g.
# the "±" in some headers) can't truncate the read the way an encoded connection
# does. Handles \n, \r\n and \r line endings.
.read_lines_latin1 <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  raw[raw == as.raw(0)] <- as.raw(32)          # guard against embedded NULs
  txt <- rawToChar(raw); Encoding(txt) <- "latin1"
  strsplit(txt, "\r\n|\n|\r")[[1]]
}

#' Read one ICARTT 1001 file.
#'
#' @param path Path to a .ict file.
#' @return A list with:
#'   $data  : data.frame of the records (numeric), plus a POSIXct `timestamp`.
#'   $meta  : list(pi, organization, mission, date, source).
#'   $units : named character vector, units per column.
read_icartt <- function(path) {
  lines <- .read_lines_latin1(path)

  hdr1 <- as.integer(strsplit(lines[1], ",")[[1]][1:2])
  nlhead <- hdr1[1]; ffi <- hdr1[2]
  if (ffi != 1001)
    stop(sprintf("%s: only ICARTT FFI 1001 is supported (got %d).",
                 basename(path), ffi))

  meta <- list(
    pi           = trimws(lines[2]),
    organization = trimws(lines[3]),
    source       = trimws(lines[4]),
    mission      = trimws(lines[5])
  )
  date_parts <- as.integer(strsplit(lines[7], ",")[[1]][1:3])
  meta$date <- as.Date(sprintf("%04d-%02d-%02d", date_parts[1],
                               date_parts[2], date_parts[3]))

  ivar  <- .split_vardef(lines[9])         # independent variable
  nvar  <- as.integer(trimws(lines[10]))   # number of dependent variables
  scale <- as.numeric(strsplit(lines[11], ",")[[1]])  # per-var scale factors (FFI 1001)
  miss  <- as.numeric(strsplit(lines[12], ",")[[1]])  # per-var missing values
  if (length(scale) != nvar || length(miss) != nvar)
    stop("ICARTT header: scale-factor / missing-value count does not match NV (", nvar, ").")

  dep <- lapply(lines[13:(12 + nvar)], .split_vardef)
  names_vec <- c(ivar$name, vapply(dep, `[[`, "", "name"))
  units_vec <- c(ivar$unit, vapply(dep, `[[`, "", "unit"))
  names(units_vec) <- names_vec
  if (length(names_vec) != nvar + 1L)
    stop("ICARTT header: variable-definition count does not match NV (", nvar, ").")

  # Data records begin right after the header block.
  data_txt <- lines[(nlhead + 1):length(lines)]
  data_txt <- data_txt[nzchar(trimws(data_txt))]
  df <- utils::read.csv(text = paste(data_txt, collapse = "\n"),
                        header = FALSE, col.names = names_vec,
                        colClasses = "numeric", check.names = FALSE)
  if (ncol(df) != nvar + 1L)
    stop("ICARTT: data column count (", ncol(df), ") does not match NV+1 (", nvar + 1L, ").")

  # Blank out missing values (raw units), then apply the per-variable scale factor.
  for (j in seq_along(dep)) {
    col <- names_vec[j + 1]
    df[[col]][df[[col]] == miss[j]] <- NA
    if (is.finite(scale[j]) && scale[j] != 1) df[[col]] <- df[[col]] * scale[j]
  }
  for (m in .ICARTT_MISSING) df[df == m] <- NA

  # Real UTC timestamp from seconds-since-midnight of the flight date.
  df$timestamp <- as.POSIXct(as.character(meta$date), tz = "UTC") +
    df[[ivar$name]]

  list(data = df, meta = meta, units = units_vec)
}

# 'CH4_ppb, ppb, CH4 mixing ratio' -> list(name='CH4_ppb', unit='ppb')
.split_vardef <- function(line) {
  parts <- trimws(strsplit(line, ",")[[1]])
  list(name = parts[1], unit = if (length(parts) > 1) parts[2] else "")
}
