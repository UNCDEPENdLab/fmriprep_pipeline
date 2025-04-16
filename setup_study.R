#' produce YAML file
#' @importFrom yaml read_yaml
#' @importFrom checkmate test_file_exists
setup_study <- function(file = NULL) {
  if (test_file_exists(file)) {
    scfg <- read_yaml(file)
  }

  # location of fmriprep container
  scfg$fmriprep_container <- prompt_input(
    instruct = glue("
      The pipeline depends on having a working fmriprep container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://fmriprep.org/en/stable/installation.html#containerized-execution-docker-and-singularity
    "),
    prompt = "Location of fmriprep container: ",
    type = "file"
  )

  # location of heudiconv container
  scfg$heudiconv_container <- prompt_input(
    instruct = glue("
      The pipeline depends on having a working heudiconv container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://heudiconv.readthedocs.io/en/latest/installation.html#install-container
    "),
    prompt = "Location of heudiconv container: ",
    type = "file"
  )

  # locartion of mriqc container
  scfg$mriqc_container <- prompt_input(
    instruct = glue("
      The pipeline can use MRIQC to produce automated QC reports. This is suggested, but not required.
      If you'd like to use MRIQC, you need a working mriqc container (docker or singularity).
      If you don't have this yet, this should work to build the latest version:
        singularity build /location/to/mriqc-latest.simg docker://nipreps/mriqc:latest
    "),
    prompt = "Location of mriqc container (press enter to skip): ",
    type = "file"
  )
  


  
}

setup_postproc <- function(scfg) {
  scfg <- setup_temporal_filter(scfg)
}


setup_confound_regression <- function(scfg = list()) {
  cur_val <- "confound_regression" %in% scfg$processing_steps
  if ("confound_regression" %in% names(scfg)) {
    cat(strwrap(glue("
      Current confound regression settings:
        Apply confound regression: {cur_val}
        Columns that will be filtered to match fMRI data: {paste(scfg$confound_regression$columns)}
        Columns that will not be filtered: {paste(scfg$confound_regression$noproc_columns)}
        File prefix: {scfg$intensity_normalize$prefix}
    "), width=80, extent=4), sep="\n")

    change <- prompt_input("Change settings? (yes/no) ", among = c("yes", "no"))
    if (change == "no") return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply confound regression? (yes/no) ", among = c("yes", "no"))
  if (apply_step == "yes" && !cur_val) {
    scfg$processing_steps <- c(scfg$processing_steps, "confound_regression")
  } else if (apply_step == "no" && cur_val) {
    scfg$processing_steps <- scfg$processing_steps[scfg$processing_steps != "confound_regression"]
  }

  scfg$confound_regression$columns <- prompt_input(": ", type = "character")
  scfg$confound_regression$noproc_columns <- prompt_input(": ", type = "character")
  scfg$confound_regression$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}


setup_intensity_normalization <- function(scfg = list()) {
  cur_val <- "intensity_normalize" %in% scfg$processing_steps
  if ("intensity_normalize" %in% names(scfg)) {
    cat(glue("
      Current intensity normalization settings:
        Apply intensity normalization: {cur_val}
        Global (4D) median intensity: {scfg$intensity_normalize$global_median}
        File prefix: {scfg$intensity_normalize$prefix}
    "))

    change <- prompt_input("Change settings? (yes/no) ", among = c("yes", "no"))
    if (change == "no") return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply intensity normalization? (yes/no) ", among = c("yes", "no"))
  if (apply_step == "yes" && !cur_val) {
    scfg$processing_steps <- c(scfg$processing_steps, "intensity_normalize")
  } else if (apply_step == "no" && cur_val) {
    scfg$processing_steps <- scfg$processing_steps[scfg$processing_steps != "intensity_normalize"]
  }

  scfg$intensity_normalize$fwhm_mm <- prompt_input("Global (4D) median intensity: ", type="numeric", lower=-1e8, upper=1e8)
  scfg$intensity_normalize$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}


setup_spatial_smooth <- function(scfg = list()) {
  cur_val <- "spatial_smooth" %in% scfg$processing_steps
  if ("spatial_smooth" %in% names(scfg)) {
    cat(glue("
      Current spatial smoothing settings:
        Apply spatial smoothing: {cur_val}
        Smoothing FWHM (mm): {scfg$spatial_smooth$fwhm_mm}
        File prefix: {scfg$spatial_smooth$prefix}
    "))

    change <- prompt_input("Change settings? (yes/no) ", among = c("yes", "no"))
    if (change == "no") return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply spatial smoothing? (yes/no) ", among = c("yes", "no"))
  if (apply_step == "yes" && !cur_val) {
    scfg$processing_steps <- c(scfg$processing_steps, "spatial_smooth")
  } else if (apply_step == "no" && cur_val) {
    scfg$processing_steps <- scfg$processing_steps[scfg$processing_steps != "spatial_smooth"]
  }

  scfg$spatial_smooth$fwhm_mm <- prompt_input("Spatial smoothing FWHM (mm): ", type="numeric", lower=0.1, upper=100)
  scfg$spatial_smooth$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}

setup_temporal_filter <- function(scfg = list()) {
  cur_val <- "temporal_filter" %in% scfg$processing_steps
  if ("temporal_filter" %in% names(scfg)) {
    cat(glue("
      Current temporal filtering settings:
        Apply temporal filter: {cur_val}
        Low-pass cutoff (Hz): {scfg$temporal_filter$low_pass_hz}
        High-pass cutoff (Hz): {scfg$temporal_filter$high_pass_hz}
        File prefix: {scfg$temporal_filter$prefix}
    "))

    change <- prompt_input("Change settings? (yes/no) ", among = c("yes", "no"))
    if (change == "no") return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply temporal filter? (yes/no) ", among = c("yes", "no"))
  if (apply_step == "yes" && !cur_val) {
    scfg$processing_steps <- c(scfg$processing_steps, "temporal_filter")
  } else if (apply_step == "no" && cur_val) {
    scfg$processing_steps <- scfg$processing_steps[scfg$processing_steps != "temporal_filter"]
  }

  scfg$temporal_filter$low_pass_hz <- prompt_input("Low-pass cutoff (Hz): ", type="numeric", lower=0)
  scfg$temporal_filter$high_pass_hz <- prompt_input("High-pass cutoff (Hz): ", type="numeric", lower=0)
  if (scfg$temporal_filter$low_pass_hz > scfg$temporal_filter$high_pass_hz) stop("Low-pass cutoff cannot be larger than high-pass cutoff")
  scfg$temporal_filter$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}

#' @importFrom checkmate test_string
prompt_input <- function(prompt = "", instruct = NULL, type = "character", lower = -Inf, upper = Inf, len = 1L, split = NULL, among=NULL, required=TRUE) {
  checkmate::assert_string(prompt, null.ok = TRUE)
  checkmate::assert_string(instruct, null.ok = TRUE)
  checkmate::assert_subset(type, c("numeric", "integer", "character", "file"))
  checkmate::assert_number(len, lower = 1L)
  if (len > 1 && !checkmate::test_string(split)) {
    stop("When multiple return values are needed, you must specify character(s) to split the input.")
  }
  if (checkmate::test_string(instruct)) cat(instruct, "\n")

  res <- ""
  while (res[1L] == "") {
    r <- readline(prompt)
    if (len > 1L && !is.null(split)) r <- strsplit(r, split, perl = TRUE)[[1]]
    r <- type.convert(r, as.is = TRUE) # convert to apparent atomic type for validation
    
    if (isFALSE(required) && is.na(r)) {
      return(r) # empty "" input converts to NA
    } else if (type == "integer") {
      if (!checkmate::test_integerish(r, lower = lower, upper = upper, len = len)) {
        cat(glue("Input must be {len} integers between {lower} and {upper}\n"))
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be {len} integers in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }
      }
    } else if (type == "numeric") {
      if (!checkmate::test_numeric(r, lower = lower, upper = upper, len = len)) {
        if (len > 1L) {
          cat(glue("Input must be {len} numbers between {lower} and {upper}\n"))
        } else {
          cat(glue("Input must be a number between {lower} and {upper}\n"))
        }
      } else {
        res <- r
      }
    } else if (type == "character") {
      if (!checkmate::test_character(r, len = len)) {
        if (len > 1L) {
          cat(glue("Input must be {len} strings separated by {split}\n"))
        } else {
          cat(glue("Input must be a character string\n"))
        }
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be {len} strings in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }        
      }
    } else if (type == "file") {
      # should probably think harder about unquoted filenames containing spaces throwing off len
      exist <- sapply(r, checkmate::test_file_exists)
      if (!all(exist)) {
        cat(glue("The following files could not be found: {paste(r[!exist], collapse=', ')}\n"))
      } else {
        res <- r
      }
    }
  }

  return(res)
}