# This file contain functions that are redundant with fmri.pipeline and should eventually be merged


#' convert a number of hours to a days, hours, minutes, seconds format
#'
#' @importFrom lubridate day hour minute second seconds_to_period dhours
#' @keywords internal
#' @details REDUNDANT WITH fmri.pipeline
hours_to_dhms <- function(hours, frac = FALSE) {
  checkmate::assert_number(hours, lower = 0)
  dur <- lubridate::dhours(hours)
  period <- seconds_to_period(dur)

  if (isTRUE(frac)) {
    str <- sprintf("%02d:%02d:%.03f", hour(period), minute(period), second(period))
  } else {
    str <- sprintf("%02d:%02d:%02d", hour(period), minute(period), round(second(period)))
  }

  if (day(period) > 0) {
    str <- paste0(sprintf("%d-", day(period)), str)
  }

  return(str)
}


#https://bids-specification.readthedocs.io/en/stable/appendices/entities.html

#' Function for extracting fields from BIDS filenames (mostly generated correctly by ChatGPT)
#' @param filenames a character vector of BIDS filenames
#' @return a `data.frame` containing the fields extraced from `filenames`
#' @importFrom checkmate assert_character
#' @keywords internal
#' @examples 
#'  filenames <- c(
#'     "/proj/fmap-phase/task-memory_sub-01_ses-02_run-1_space-MNI2009c_acq-highres_desc-preproc_bold.nii.gz",
#'     "acq-lowres_desc-smoothed_sub-02_task-attention_run-2_bold.nii.gz",
#'    "sub-03_space-MNI152NLin6Asym_task-motor_desc-raw_echo-2_dir-PA_bold.nii.gz",
#'    "hemi-L_desc-denoised_task-vision_rec-magnitude_fmap-phase_sub-04_bold.nii.gz"
#'  )
#' 
#' extract_bids_info(filenames)
extract_bids_info <- function(filenames, drop_unused=FALSE) {
  checkmate::assert_character(filenames)
  filenames <- basename(filenames) # avoid matching on path components

  # Define regex patterns for each BIDS entity
  patterns <- list(
    subject = "sub-(\\d+)",
    session = "ses-(\\d+)",
    task = "task-([a-zA-Z0-9]+)",
    run = "run-(\\d+)",
    space = "space-([a-zA-Z0-9]+)",
    acquisition = "acq-([a-zA-Z0-9]+)",
    description = "desc-([a-zA-Z0-9]+)",
    modality = "mod-([a-zA-Z0-9]+)",
    echo = "echo-(\\d+)",
    direction = "dir-([a-zA-Z0-9]+)",
    hemisphere = "hemi-([a-zA-Z0-9]+)",
    reconstruction = "rec-([a-zA-Z0-9]+)",
    fieldmap = "fmap-([a-zA-Z0-9]+)"
  )
  
  # Function to extract an entity from a filename
  extract_entity <- function(filename, pattern) {
    match <- regmatches(filename, regexpr(pattern, filename))
    if (length(match) > 0) {
      return(sub(".*-", "", match))  # Extract value after the last "-"
    } else {
      return(NA)
    }
  }
  
  # Extract suffix (the last part before file extension)
  extract_suffix <- function(filename) {
    stripped <- sub("\\.nii\\.gz$|\\.tsv\\.gz$|\\.tsv$|\\.json$|\\.nii$", "", filename)
    parts <- unlist(strsplit(stripped, "_"))
    last_part <- tail(parts, 1)
    if (!grepl("-", last_part)) {
      return(last_part)
    } else {
      return(NA)
    }
  }

  # Process each filename
  extracted_info <- lapply(filenames, function(filename) {
    # Extract each entity independently
    info <- lapply(patterns, extract_entity, filename = filename)
    return(as.data.frame(info, stringsAsFactors = FALSE))
  })
  
  # Combine results into a single data frame
  df <- do.call(rbind, extracted_info)

  # Add suffixes
  suffixes <- vapply(filenames, extract_suffix, character(1))
  df$suffix <- suffixes

  if (isTRUE(drop_unused)) {
    all_na <- sapply(df, function(i) all(is.na(i)))
    df <- df[!all_na]
  }
  
  return(df)
}
