# little helper function to allow a nested list to be traversed using key syntax
# of the form parent/child/grandchild where this corresponds to my_list$parent$child$granchild
get_nested_values <- function(lst, key_strings, sep = "/", simplify = TRUE) {
  split_keys_list <- strsplit(key_strings, sep)
  
  ret <- sapply(seq_along(split_keys_list), function(i) {
    keys <- split_keys_list[[i]]
    val <- lst
    
    for (j in seq_along(keys)) {
      key <- keys[[j]]
      
      if (j == length(keys) && is.atomic(val) && !is.null(names(val))) {
        # If last key and val is a named vector
        return(val[[key]])
      } else if (!is.list(val) || is.null(val[[key]])) {
        # Otherwise, standard list traversal
        return(NULL)
      }
      val <- val[[key]]
    }
    return(val)
  }, USE.NAMES = FALSE, simplify = simplify)
  names(ret) <- key_strings
  return(ret)
}

#' validate the structure of a study configuration object
#' @param scfg a study configuration file as produced by `load_study` or `setup_study`
#' @importFrom checkmate assert_flag test_class test_directory_exists test_file_exists
#' @keywords internal
validate_scfg <- function(scfg = list(), quiet = FALSE) {
  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    if (inherits(scfg, "list")) {
      class(scfg) <- c(class(scfg), "bg_study_cfg")
    } else {
      stop("scfg must be a list of bg_study_cfg object")
    }
  }

  checkmate::assert_flag(quiet)

  gaps <- c()

  if (is.null(scfg$project_name)) {
    if (!quiet) message("Config file is missing project_name. You will be asked for this.")
    gaps <- c(gaps, "project_name")
  }

  required_dirs <- c("project_directory", "dicom_directory", "bids_directory", "fmriprep_directory", "scratch_directory")
  for (rr in required_dirs) {
    if (!checkmate::test_directory_exists(scfg[[rr]])) {
      message("Config file is missing valid ", rr, ". You will be asked for this.")
      gaps <- c(gaps, rr)
    }
  }

  required_files <- c("compute_environment/fmriprep_container", "compute_environment/heudiconv_container", "heudiconv/heuristic_file")
  for (rr in required_files) {
    if (!checkmate::test_file_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid ", rr, ". You will be asked for this.")
      gaps <- c(gaps, rr)
    }
  }

  # optional files
  if (!is.null(scfg$compute_environment$bids_validator) && !checkmate::test_file_exists(scfg$compute_environment$bids_validator)) {
    message("Cannot find bids_validator at ", scfg$compute_environment$bids_validator, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/bids_validator")
    scfg$compute_environment$bids_validator <- NULL
  }

  if (!is.null(scfg$compute_environment$mriqc_container) && !checkmate::test_file_exists(scfg$compute_environment$mriqc_container)) {
    message("Cannot find MRIQC container at ", scfg$compute_environment$mriqc_container, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/mriqc_container")
    scfg$compute_environment$mriqc_container <- NULL
  }

  if (!is.null(scfg$compute_environment$aroma_container) && !checkmate::test_file_exists(scfg$compute_environment$aroma_container)) {
    message("Cannot find AROMA container at ", scfg$compute_environment$aroma_container, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/aroma_container")
    scfg$compute_environment$aroma_container <- NULL
  }

  # helper subfunction to convert NULL, empty list, or "" to character(0) for conformity
  validate_char <- function(arg) {
    if (is.null(arg) || identical(arg, list()) || length(arg) == 0L || arg[1L] == "") {
      arg <- character(0)
    }
    return(arg)
  }

  # check memgb, nhours, ncores, cli_options, and sched_args for all jobs
  validate_job_settings <- function(job_name) {
    if (!checkmate::test_number(scfg[[job_name]]$memgb, lower=1, upper=1000)) {
      message("Invalid memgb setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "memgb", sep="/"))
      scfg[[job_name]]$memgb <- NULL
    }

    if (!checkmate::test_number(scfg[[job_name]]$nhours, lower=1, upper=1000)) {
      message("Invalid nhours setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "nhours", sep="/"))
      scfg[[job_name]]$nhours <- NULL
    }

    if (!checkmate::test_number(scfg[[job_name]]$ncores, lower = 1, upper = 250)) {
      message("Invalid ncores setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "ncores", sep = "/"))
      scfg[[job_name]]$ncores <- NULL
    }
    
    # conform cli_options to character(0) on empty
    scfg[[job_name]]$cli_options <- validate_char(scfg[[job_name]]$cli_options)
    scfg[[job_name]]$sched_args <- validate_char(scfg[[job_name]]$sched_args)
    
  }

  # validate job settings
  for (job in c("heudiconv", "fmriprep", "mriqc", "aroma", "postprocess")) {
    validate_job_settings(job)
  }

  # Postprocessing settings validation
  # Validate temporal filtering
  if ("temporal_filter" %in% names(scfg$postprocess)) {
    if (!checkmate::test_number(scfg$postprocess$temporal_filter$low_pass_hz, lower=0)) {
      message("Missing low_pass_hz in $postprocess. You will be asked for this.")
      gaps <- c(gaps, "postprocess/temporal_filter/low_pass_hz")
      scfg$postprocess$temporal_filter$low_pass_hz <- NULL
    }

    if (!checkmate::test_number(scfg$postprocess$temporal_filter$high_pass_hz, lower = 0)) {
      message("Missing high_pass_hz in $postprocess. You will be asked for this.")
      gaps <- c(gaps, "postprocess/temporal_filter/high_pass_hz")
      scfg$postprocess$temporal_filter$high_pass_hz <- NULL
    }

    if (!is.null(scfg$postprocess$temporal_filter$low_pass_hz) && !is.null(scfg$postprocess$temporal_filter$high_pass_hz) && 
    scfg$postprocess$temporal_filter$high_pass_hz < scfg$postprocess$temporal_filter$low_pass_hz) {
      message("high_pass_hz is greter than low_pass_hz $postprocess$temporal_filter. You will be asked to respecify valid values.")
      if (!"postprocess/temporal_filter/low_pass_hz" %in% gaps) gaps <- c(gaps, "postprocess/temporal_filter/low_pass_hz")
      if (!"postprocess/temporal_filter/high_pass_hz" %in% gaps) gaps <- c(gaps, "postprocess/temporal_filter/high_pass_hz")
      scfg$postprocess$temporal_filter$low_pass_hz <- NULL
      scfg$postprocess$temporal_filter$high_pass_hz <- NULL
    }

    if (!checkmate::test_string(scfg$postprocess$temporal_filter$prefix)) {
      message("No valid prefix found for $postprocess$temporal_filter")
      gaps <- c(gaps, "postprocess/temporal_filter/prefix")
      scfg$postprocess$temporal_filter$prefix <- NULL
    }
  }

  # Validate spatial smoothing
  if ("spatial_smooth" %in% names(scfg$postprocess)) {
    if (!checkmate::test_number(scfg$postprocess$spatial_smooth$fwhm_mm, lower = 0.1)) {
      if (!quiet) message("Missing fwhm_mm in $postprocess$spatial_smooth. You will be asked for this.")
      gaps <- c(gaps, "postprocess/spatial_smooth/fwhm_mm")
      scfg$postprocess$spatial_smooth$fwhm_mm <- NULL
    }
    
    if (!checkmate::test_string(scfg$postprocess$spatial_smooth$prefix)) {
      message("No valid prefix found for $postprocess$spatial_smooth")
      gaps <- c(gaps, "postprocess/spatial_smooth/prefix")
      scfg$postprocess$spatial_smooth$prefix <- NULL
    }
  }

  if ("intensity_normalize" %in% names(scfg$postprocess)) {
    if (!checkmate::test_number(scfg$postprocess$intensity_normalize$global_median, lower = 0.1)) {
      if (!quiet) message("Invalid global_median in $postprocess$intensity_normalize. You will be asked for this.")
      gaps <- c(gaps, "postprocess/intensity_normalize/global_median")
      scfg$postprocess$intensity_normalize$global_median <- NULL
    }
    
    if (!checkmate::test_string(scfg$postprocess$intensity_normalize$prefix)) {
      message("No valid prefix found for $postprocess$intensity_normalize")
      gaps <- c(gaps, "postprocess/intensity_normalize/prefix")
      scfg$postprocess$intensity_normalize$prefix <- NULL
    }
  }

  if ("confound_calculate" %in% names(scfg$postprocess)) {
    if (!checkmate::test_flag(scfg$postprocess$confound_calculate$demean)) {
      if (!quiet) message("Invalid demean field in $postprocess$confound_calculate. You will be asked for this.")
      gaps <- c(gaps, "postprocess/confound_calculate/demean")
      scfg$postprocess$confound_calculate$demean <- NULL
    }

    if (!checkmate::test_string(scfg$postprocess$confound_calculate$output_file)) {
      message("Invalid output_file field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/output_file")
      scfg$postprocess$confound_calculate$output_file <- NULL
    }

    if (!checkmate::test_character(scfg$postprocess$confound_calculate$columns)) {
      message("Invalid columns field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/columns")
      scfg$postprocess$confound_calculate$columns <- NULL
    }

    if (!checkmate::test_character(scfg$postprocess$confound_calculate$noproc_columns)) {
      message("Invalid noproc_columns field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/noproc_columns")
      scfg$postprocess$confound_calculate$noproc_columns <- NULL
    }
  }

  # Validate AROMA settings
  if ("apply_aroma" %in% names(scfg$postprocess)) {
    if (!checkmate::test_flag(scfg$postprocess$apply_aroma$nonaggressive)) {
      if (!quiet) message("Invalid nonaggressive field in $postprocess$apply_aroma You will be asked for this.")
      gaps <- c(gaps, "postprocess/confound_calculate/demean")
      scfg$postprocess$apply_aroma$nonaggressive <- NULL
    }

    if (!checkmate::test_string(scfg$postprocess$apply_aroma$prefix)) {
      message("No valid prefix found for $postprocess$apply_aroma")
      gaps <- c(gaps, "postprocess/apply_aroma/prefix")
      scfg$postprocess$apply_aroma$prefix <- NULL
    }
  }

  attr(scfg, "gaps") <- gaps

  return(scfg)
}

#' Load a study configuration from a file
#' @param file a YAML file containing a valid study configuration
#' @importFrom yaml read_yaml
#' @export
load_study <- function(file = NULL) {
  if (!checkmate::test_file_exists(file)) {
    stop("Cannot find file: ", file)
  } else {
    scfg <- read_yaml(file)
    scfg <- validate_scfg(scfg)
  }
}

#' summary method for study configuration object
#' @export
summary.bg_study_cfg <- function(x) {
  pretty_print_list(x)
}

#' Setup the processing pipeline for a new fMRI study
#' @param file a YAML file specifying the location of an existing configuration file to be loaded
#' @importFrom yaml read_yaml
#' @importFrom checkmate test_file_exists
#' @export
setup_study <- function(file = NULL) {
  if (test_file_exists(file)) {
    scfg <- read_yaml(file) # should validate... but should setup actually allow import of a file, or just load??
  } else {
    scfg <- list()
  }

  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    class(scfg) <- c(class(scfg), "bg_study_cfg")
  }

  if (is.null(scfg$project_name)) scfg$project_name <- prompt_input("What is the name of your project?", type = "character")

  scfg$project_directory <- prompt_input("What is the root directory where project files will be stored?", type = "character")
  if (!checkmate::test_directory_exists(scfg$project_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$project_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$project_directory, recursive = TRUE)
  }
  if (!checkmate::test_directory_exists(scfg$project_directory, "r")) {
    warning(glue("You seem not to have read permission to: {scfg$project_directory}. This could cause problems in trying to run anything!"))
  }

  # location of DICOMs
  # /nas/longleaf/home/willasc/repos/clpipe/tests/temp/clpipe_dir0/data_DICOMs
  scfg$dicom_directory <- prompt_input("Where are DICOM files files stored?", type = "character")
  if (!checkmate::test_directory_exists(scfg$dicom_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$dicom_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$dicom_directory, recursive = TRUE)
  }

  # location of BIDS data -- enforce that this must be within the project directory
  scfg$bids_directory <- file.path(scfg$project_directory, "data_BIDS")
  if (!checkmate::test_directory_exists(scfg$bids_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$bids_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$bids_directory, recursive = TRUE) # should probably force this to happen
  }

  # location of fmriprep outputs -- enforce that this must be within the project directory
  scfg$fmriprep_directory <- file.path(scfg$project_directory, "data_fmriprep")

  scfg$scratch_directory <- prompt_input("Work directory: ",
    instruct = glue("
    \nfmriprep uses a lot of disk space for processing intermediate files. It's best if these
    are written to a scratch/temporary directory that is cleared regularly so that you don't
    use up precious disk space for unnecessary files. Please indicate where these intermediate
    file should be written.\n
  "), type = "character")
  if (!checkmate::test_directory_exists(scfg$scratch_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$scratch_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$scratch_directory, recursive = TRUE)
  }

  scfg$run_aroma <- prompt_input("Run AROMA?",
    instruct = glue("
      \nAs of v24, fmriprep has now removed ICA-AROMA from its codebase, splitting this off to
      a separate BIDS app called fmripost-aroma. Do you want to run the fMRI data through ICA-AROMA?
      If so, you will subsequently be asked for the location of an ICA-AROMA container file. Note that
      saying 'yes' to this, only runs AROMA, but does not remove motion-related components from the fMRI
      timeseries. That is a postprocessing decision, which you will be asked about in that context.\n
      "), type = "flag"
  )

  scfg <- setup_compute_environment(scfg)

  # logging
  scfg$log_txt <- prompt_input("Create subject-level logs?",
    instruct = glue("
      The package can write plain-text logs to each subject's sub-<id> directory.
      These contain messages related to job submission and job status.
      We strongly recommend these for tracking and debugging purposes.
      ", .trim = FALSE), type = "flag"
  )

  # run through configuration of each step
  scfg <- setup_bids_conversion(scfg)
  scfg <- setup_bids_validation(scfg)
  scfg <- setup_fmriprep(scfg)
  scfg <- setup_mriqc(scfg)
  if (isTRUE(scfg$run_aroma)) scfg <- setup_aroma(scfg)
  scfg <- setup_postprocessing(scfg)

  return(scfg)
}

#' Specify the fmriprep settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$fmriprep` populated
#' @keywords internal
setup_fmriprep <- function(scfg = NULL, prompt_all = FALSE) {
  # https://fmriprep.org/en/stable/usage.html
  # [--omp-nthreads OMP_NTHREADS] [--mem MEMORY_MB] [--low-mem]  [--nprocs NPROCS]
  defaults <- list(
    memgb = 48,
    nhours = 24,
    ncores = 12,
    cli_options = "",
    sched_args = ""
  )

  #scfg$fmriprep <- populate_defaults(scfg$fmriprep, defaults)

  cat("This step sets up fmriprep.  (For details, see https://fmriprep.org/en/stable/usage.html)\n")

  scfg <- setup_job(scfg, "fmriprep", defaults = defaults)

  # pass forward the memgb job setting to fmriprep itself to bound memory use
  if (!is.null(scfg$fmriprep$memgb)) {
    scfg$fmriprep$cli_options <- set_cli_options(scfg$fmriprep$cli_options, glue("--mem {scfg$fmriprep$memgb*1000}"))
  }

  return(scfg)

}

setup_bids_validation <- function(scfg) {
  defaults <- list(
    memgb = 32,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  scfg <- setup_job(scfg, "bids_validation", defaults = defaults)

  scfg$bids_validation$outfile <- prompt_input(
    instruct = glue("
      \nWhat should be the name of the output file created by bids_validator? The default is bids_validator_output.html
      You can also include the subject and session IDs in the filename by using the following
      placeholders: {sub_id} and {ses_id}. For example, bids_validation_sub-{sub_id}_ses-{ses_id}.html will substitute
      the subject and session IDs into the filename. This is useful if you want to place the output files in a common
      directory for all subjects and sessions, but still want to be able to identify which file belongs to which subject.
      \n
    "),
    prompt = "What is the name of the output file for bids-validator?",
    type = "character", default = "bids_validator_output.html"
  )

  return(scfg)
}

#' Specify the mriqc settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$mriqc` populated
#' @keywords internal
setup_mriqc <- function(scfg) {
  defaults <- list(
    memgb = 32,
    nhours = 12,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  # scfg$mriqc <- populate_defaults(scfg$fmriprep, defaults)
  scfg <- setup_job(scfg, "mriqc", defaults = defaults)

  return(scfg)
}

#' Specify the heudiconv settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$heudiconv` populated
#' @keywords internal
setup_bids_conversion <- function(scfg, prompt_all = FALSE) {
  defaults <- list(
    memgb = 16,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  # scfg$heudiconv <- populate_defaults(scfg$heudiconv, defaults)

  cat(glue("
    \nThis step sets up DICOM to BIDS conversion using heudiconv. Heudiconv uses a heuristic
    file to match DICOM files to expected scans, allowing the tool to convert DICOMs to NIfTI images
    and reorganize them into BIDS format.

    The heuristic file is a python script that tells heudiconv how to convert the DICOM files
    to BIDS format. For details, see https://heudiconv.readthedocs.io/en/latest/usage.html

    Here, you will be asked for the location of the folder containing DICOM images for all subjects.
    This should be a folder that contains subfolders for each subject, with the DICOM files inside
    those subfolders. For example, if you have a folder called 'data_DICOMs' that contains
    subfolders 'sub-001', 'sub-002', etc., then you would specify the location of 'data_DICOMs' here.

    The BIDS-compatible outputs will be written to a folder called 'data_BIDS' within the project directory.

    You will also be asked for a regular expression that matches the subject IDs in the DICOM folder names.
    The default is 'sub-[0-9]+', which matches sub-001, sub-002, etc. If you have a different naming scheme,
    please specify it here. For example, if your subject folders are named '001', '002', etc., you would
    specify '^[0-9]+$' here. Importantly, the pipeline will always extract only the numeric portion of the
    subject ID, so if you have a folder called 'sub-001', the subject ID will be '001' regardless of the
    regex you specify here.

    Similarly, if you have multisession data, you will be asked for a regular expression that matches the
    session IDs in the DICOM folder names. Crucially, sessions must always be subfolders within a given subject
    folder. Here is an example where the subject regex is 'sub-[0-9]+' and the session regex is 'ses-[0-9]+':

    /data/dicom/
    ├── sub-01/
    │   ├── ses-01/
    │   │   ├── 1.dcm
    │   │   ├── 2.dcm
    │   └── ses-02/
    │       ├── 1.dcm
    │       ├── 2.dcm

    You will also be asked for the location of the heuristic file. If you don't have a heuristic file,
    please see some examples here: https://github.com/nipy/heudiconv/tree/master/heudiconv/heuristics.\n\n
    "))

  scfg$heudiconv$sub_regex <- prompt_input(
    instruct = glue("
      \nWhat is the regex pattern for the subject IDs? This is used to identify the subject folders
      within the DICOM directory. The default is sub-[0-9]+, which matches sub-001, sub-002, etc.
      If you have a different naming scheme, please specify it here.\n
    "),
    prompt = "What is the regex pattern for the subject IDs?",
    type = "character", default = "sub-[0-9]+"
  )

   scfg$heudiconv$sub_id_match <- prompt_input(
    instruct = glue("
      \nWhat is the regex pattern for extracting the ID from the subject folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      subject folder is named 'sub-001', the ID will be '001'. If your subject folder is named
      '001', the ID will be '001'. If the entire folder name is the subject ID, such as '001ra_2May2024',
      the id matching expression should be (.+), which matches all characters in the folder name.\n
    "),
    prompt = "What is the regex pattern for extracting the subject ID from the folder name?",
    type = "character", default = "(.+)"
  )

  scfg$heudiconv$ses_regex <- prompt_input(
    instruct = glue("
      If you have multisession data, specify the the regex pattern for session IDs within the subject folders.
      If you don't have multisession data, just press Enter to skip this step.
    ", .trim = FALSE),
    prompt = "What is the regex pattern for the session IDs?",
    type = "character", required = FALSE
  )

  if (!is.na(scfg$heudiconv$ses_regex)) {
    scfg$heudiconv$ses_id_match <- prompt_input(
    instruct = glue("
      \nWhat is the regex pattern for extracting the ID from the subject folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      subject folder is named 'sub-001', the ID will be '001'. If your subject folder is named
      '001', the ID will be '001'. If the entire folder name is the subject ID, such as '001ra_2May2024',
      the id matching expression should be (.+), which matches all characters in the folder name.\n
    "),
    prompt = "What is the regex pattern for extracting the subject ID from the folder name?",
    type = "character", default = "(.+)"
  )
  } else {
    scfg$heudiconv$ses_id_match <- NA_character_
  }
  

  scfg$heudiconv$heuristic_file <- prompt_input(instruct = glue("What is the location of the heudiconv heuristic file?"), type = "file")

  scfg$heudiconv$overwrite <- prompt_input(instruct = glue("Should existing BIDS files be overwritten by heudiconv?"), type = "flag", default = TRUE)
  scfg$heudiconv$clear_cache <- prompt_input(
    instruct = glue("Heudiconv caches its matching results inside the root of the BIDS folder in a hidden
    directory called .heudiconv. This provides a record of what heudiconv did for each subject conversion.
    It also speeds up conversions in future if you reprocess data. That said, if you modify the heuristic file,
    the cache can interfere because it will use the old heuristic file to match DICOMs to BIDS.
    If you want to clear the cache, say 'yes' here. If you want to keep the cache, say 'no'.
    ", .trim = FALSE),
    prompt = glue("Should the heudiconv cache be cleared?"),
    type = "flag", default = FALSE
  )
  
  scfg$heudiconv$validate_bids <- prompt_input(
    instruct = glue("
      Should the BIDS folder be validated after conversion? This requires the bids-validator program to be installed.
      This is generally a good idea to ensure that the BIDS folder is valid and conforms to the BIDS specification.
      It can prevent downstream errors in fmriprep and other processing steps.
    "),
    type = "flag", default = TRUE
  )

  scfg <- setup_job(scfg, "heudiconv", defaults = defaults)

  return(scfg)
}

#' Specify the aroma settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$aroma` populated
#' @keywords internal
setup_aroma <- function(scfg, prompt_all = FALSE) {
  defaults <- list(
    memgb = 32,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  cat("This step sets up ICA-AROMA.\n")

  scfg <- setup_job(scfg, "aroma", defaults = defaults)
  return(scfg)
}

#' helper function to extract numbers from number-like strings
#' @param x a character vector containing number-like strings
#' @return a numeric vector containing the parsed values
#' @examples 
#'   parse_number_base(c("1,234.56", "7,890", "12.34"))
#' @keywords internal
parse_number_base <- function(x) {
  sapply(x, function(s) {
    # Extract first number-like sequence: optional minus, digits, optional commas, optional decimal
    match <- regmatches(s, regexpr("-?\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|-?\\d+(?:\\.\\d+)?", s, perl = TRUE))
    if (length(match) == 0 || is.na(match)) {
      return(NA_real_)
    }
    # Remove grouping commas and convert to numeric
    as.numeric(gsub(",", "", match))
  })
}

#' Helper function to obtain all subject and session directories from a root folder
#' @param root The path to a root folder containing subject folders. 
#' @param sub_regex A regex pattern to match the subject folders. Default: `"[0-9]+"`.
#' @param sub_id_match A regex pattern to indicate how to extract the subject ID from the subject folder name.
#'  Default: `"([0-9]+)"`. The default will extract the first number-like sequence from the folder name.
#'  Note that this can include multiple capturing groups if the ID has multiple parts.
#' @param ses_regex A regex pattern to match the session folders. Default: `NULL`.
#'  If `NULL`, only the subject folders are returned. If not `NULL`, the session folders
#'    are returned as well.
#' @param ses_id_match A regex pattern to indicate how to extract the session ID from the session folder name.
#'  Default: `"([0-9]+)"`. The default will extract the first number-like sequence from the folder name.
#' @param full.names If `TRUE`, return the absolute paths to the subject folders. If
#'   `FALSE`, return paths relative to `root`. Default: `TRUE`.
#' @details This function is used to find all subject folders within a root folder. 
#'   It is used internally by the package to find the subject DICOM and BIDS folders for processing.
#'   The function uses the `list.dirs` function to list all directories within the
#'   folder and then filters the directories based on the regex patterns provided.
#'   The function returns a character vector of the subject folders found.
#' 
#'   The function also extracts the subject and session IDs from the folder names
#'   using the regex patterns provided. The IDs are extracted using the `extract_capturing_groups`
#'   function, which uses the `regexec` and `regmatches` functions to extract the capturing groups
#'   from the folder names. The function returns a data frame with the subject and session IDs
#'   and the corresponding folder paths.
#' @return A data frame with the following columns:
#'   - `sub_id`: The subject ID extracted from the subject folder name.
#'   - `ses_id`: The session ID extracted from the session folder name. If no session folders are found,
#'     this column will be `NA`.
#'   - `sub_dir`: The path to the subject folder.
#'   - `ses_dir`: The path to the session folder. If no session folders are found, this column will be `NA`.
#' @examples
#'   get_subject_dirs(root = "/path/to/root", sub_regex = "[0-9]+", sub_id_match = "([0-9]+)",
#'     ses_regex = "ses-[0-9]+", ses_id_match = "([0-9]+)", full.names = TRUE)
#' @keywords internal
#' @importFrom checkmate assert_directory_exists assert_flag
get_subject_dirs <- function(root = NULL, sub_regex = "[0-9]+", sub_id_match = "([0-9]+)", 
  ses_regex = NULL, ses_id_match = "([0-9]+)", full.names = FALSE) {
  
  checkmate::assert_directory_exists(root)
  checkmate::assert_string(sub_regex)
  if (is.null(sub_id_match)) sub_id_match <- "(.*)" # all characters
  checkmate::assert_string(sub_id_match)
  checkmate::assert_string(ses_regex, null.ok = TRUE, na.ok = TRUE)
  if (is.null(ses_id_match) || is.na(ses_id_match[1L])) ses_id_match <- "(.*)" # all characters
  checkmate::assert_string(ses_id_match)
  checkmate::assert_flag(full.names)

  # List directories in the root folder
  entries <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  subject_entries <- entries[grepl(sub_regex, entries)]
  subject_ids <- extract_capturing_groups(subject_entries, sub_id_match)
  
  if (length(subject_entries) == 0) {
    warning("No subject directories found in the root folder matching the regex pattern.")
    return(data.frame(sub_id = character(0), ses_id = character(0), sub_dir = character(0), ses_dir = character(0), stringsAsFactors = FALSE))
  }

  result <- list()

  for (ss in seq_along(subject_entries)) {

    sub_dir <- if (full.names) file.path(root, subject_entries[ss]) else subject_entries[ss]

    # Create subject-level row
    subject_row <- list(sub_id = subject_ids[ss], ses_id = NA_character_, sub_dir = sub_dir, ses_dir = NA_character_)

    if (is.null(ses_regex) || is.na(ses_regex[1L])) {
      # not a multisession study
      result[[length(result) + 1]] <- subject_row
    } else {
      ses_dirs <- list.dirs(file.path(root, subject_entries[ss]), recursive = TRUE, full.names = FALSE)
      ses_matches <- ses_dirs[grepl(ses_regex, basename(ses_dirs))]

      if (length(ses_matches) > 0) {
        for (ses_dir in ses_matches) {
          ses_id <- extract_capturing_groups(basename(ses_dir), ses_id_match)
          ses_dir <- file.path(sub_dir, ses_dir) # add the subject directory to the session directory
          result[[length(result) + 1]] <- list(sub_id = subject_ids[ss], ses_id = ses_id, sub_dir = sub_dir,ses_dir = ses_dir)
        }
      } else {
        warning(sprintf("No session directories found in '%s' matching '%s'", sub_dir, ses_regex))
        result[[length(result) + 1]] <- subject_row
      }
    }
  }

  return(do.call(rbind.data.frame, result))
}



# old version that just retuns a vector of subject directories
 
# get_subject_dirs <- function(root = NULL, sub_regex = "[0-9]+", ses_regex = NULL, full.names = TRUE) {
#   checkmate::assert_directory_exists(root)
#   checkmate::assert_string(sub_regex)
#   checkmate::assert_string(ses_regex, null.ok = TRUE)
#   checkmate::assert_flag(full.names)

#   # List directories in the root folder
#   entries <- list.dirs(root, recursive = FALSE, full.names = FALSE)

#   # Keep only those directories matching the subject regex
#   subject_dirs <- entries[grepl(sub_regex, entries)]

#   if (!is.null(ses_regex)) {
#     found_dirs <- character()
#     for (subj in subject_dirs) {
#       ses_dirs <- list.dirs(file.path(root, subj), recursive = TRUE, full.names = FALSE)
#       matched_ses <- ses_dirs[grepl(ses_regex, basename(ses_dirs))]

#       if (length(matched_ses) > 0) {
#         found_dirs <- c(found_dirs, file.path(subj, matched_ses))
#       } else {
#         warning(sprintf("No session directories found in '%s' matching '%s'; using subject directory.", subj, ses_regex))
#         found_dirs <- c(found_dirs, subj)
#       }
#     }
#     subject_dirs <- found_dirs
#   } else {
#     sub_ids <- NA
#   }

#   if (isTRUE(full.names)) subject_dirs <- file.path(root, subject_dirs)

#   return(subject_dirs)
# }

#' helper function to extract capturing groups from a string
#' @param strings a character vector containing the strings to be processed
#' @param pattern a regex pattern to match the strings
#' @param sep a character string to separate the captured groups. Default: `"_"`.
#' @param groups a numeric vector specifying the indices of the capturing groups to be extracted.
#'   Default: `NULL`, which extracts all capturing groups.
#' @param ... additional arguments passed to `regexec` (e.g., `perl = TRUE`)
#' @details This function uses the `regexec` and `regmatches` functions to extract
#'   the capturing groups from the strings. The function returns a character vector
#'   containing the captured groups. If no matches are found, `NA` is returned.
#' @return a character vector containing the captured groups
#' @keywords internal
extract_capturing_groups <- function(strings, pattern, groups = NULL, sep = "_", ...) {
  stopifnot(is.character(strings), is.character(pattern), length(pattern) == 1)

  matches <- regexec(pattern, strings, ...)
  reg_list <- regmatches(strings, matches)

  # For each string, extract and paste selected groups
  result <- vapply(reg_list, function(m) {
    if (length(m) <= 1) return(NA_character_)
    capture_groups <- m[-1]  # drop full match
    if (!is.null(groups)) {
      if (any(groups > length(capture_groups))) {
        warning("Some requested group indices are out of range.")
        return(NA_character_)
      }
      capture_groups <- capture_groups[groups]
    }
    paste(capture_groups, collapse = sep)
  }, character(1))

  return(result)
}

#strings <- c("sub-123_datavisit1", "sub-456_datatest1", "badstring")
#pattern <- "sub-([0-9]+)_data((visit|test)1)"
#pattern <- "([0-9]+)"
#extract_capturing_groups(strings, pattern)



# helper function to grab the subject id from a subject's bids directory
get_sub_id <- function(bids_dir, regex="sub-[0-9]+") {
  checkmate::assert_directory_exists(bids_dir)
  
  sub("sub-", "", basename(bids_dir))
}

# helper function to populate defaults for config
populate_defaults <- function(target = NULL, defaults) {
  if (is.null(target)) target <- list()
  checkmate::assert_list(defaults, names = "unique")

  miss_fields <- setdiff(names(defaults), names(target))
  if (length(miss_fields) > 0L) {
    for (mm in miss_fields) {
      target[[mm]] <- defaults[[mm]]
    }
  }

  return(target)
}

get_compute_environment_from_file <- function(scfg) {
  if (length(scfg) > 0L && !"compute_environment" %in% names(scfg)) {
    cat(glue("
      No compute environment settings were found in your configuration. This includes settings such
      as the location of the fmriprep container or the location of heudiconv.
      ", .trim = FALSE))

    from_file <- prompt_input("Would you like to import these from an existing YAML file?", type = "flag")
    if (from_file) {
      file_loc <- prompt_input("Specify the file location (or press Enter to cancel):", type = "file", len = 1L)
      if (!is.na(file_loc)) {
        ff <- read_yaml(file_loc)
        possible_fields <- c(
          "fmriprep_container", "heudiconv_container", "mriqc_container",
          "aroma_container", "scheduler"
        )

        if ("compute_environment" %in% names(ff)) {
          scfg$compute_environment <- ff$compute_environment
        } else if (any(possible_fields %in% names(ff))) {
          # config has the settings as the top layer (malformed, but okay)
          for (field in possible_fields) {
            scfg$compute_environment[[field]] <- ff[[field]]
          }
        } else {
          warning("Could not find any compute environment information in file: ", file_loc)
        }
      }
    }
  }

  return(scfg)
}

#' Setup the compute environment for a study
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$compute_environment` populated
#' @keywords internal
#' @importFrom checkmate assert_list
setup_compute_environment <- function(scfg = list()) {
  checkmate::assert_list(scfg)

  # if empty, allow population from external file
  scfg <- get_compute_environment_from_file(scfg)

  if (is.null(scfg$compute_environment$scheduler) || !checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"))) {
    scfg$compute_environment$scheduler <- prompt_input("Scheduler (slurm/torque): ",
      instruct = "The pipeline currently runs on TORQUE (aka qsub) and SLURM clusters.\nWhich will you use?",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
  }

  # location of fmriprep container
  if (!validate_exists(scfg$compute_environment$fmriprep_container, description = "fmriprep container", prompt_change = TRUE)) {
    scfg$compute_environment$fmriprep_container <- prompt_input(
      instruct = glue("
      The pipeline depends on having a working fmriprep container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://fmriprep.org/en/stable/installation.html#containerized-execution-docker-and-singularity
    ", .trim = FALSE),
      prompt = "Location of fmriprep container: ",
      type = "file"
    )
  }

  # location of heudiconv container
  if (!validate_exists(scfg$compute_environment$heudiconv_container, description = "heudiconv container", prompt_change = TRUE)) {
    scfg$compute_environment$heudiconv_container <- prompt_input(
      instruct = glue("
      The pipeline depends on having a working heudiconv container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://heudiconv.readthedocs.io/en/latest/installation.html#install-container
    ", .trim = FALSE),
      prompt = "Location of heudiconv container: ",
      type = "file"
    )
  }

  # location of bids-validator binary
  if (!validate_exists(scfg$compute_environment$bids_validator, description = "bids-validator program", prompt_change = TRUE)) {
    scfg$compute_environment$bids_validator <- prompt_input(
      instruct = glue("
      After BIDS conversion, the pipeline can pass resulting BIDS folders to bids-validator to verify that 
      the folder conforms to the BIDS specification. You can read more about validtion here: 
      https://bids-validator.readthedocs.io/en/stable/index.html.
      
      If you'd like to include BIDS validation in the processing pipeline, specify the location of the 
      bids-validator program here. If you need help building this program, follow these instructions: 
      https://bids-validator.readthedocs.io/en/stable/user_guide/command-line.html.
    ", .trim = FALSE),
      prompt = "Location of bids-validator program: ",
      type = "file", required = FALSE
    )
  }

  # location of mriqc container
  if (!validate_exists(scfg$compute_environment$mriqc_container, description = "mriqc container", prompt_change = TRUE)) {
    scfg$compute_environment$mriqc_container <- prompt_input(
      instruct = glue("
      The pipeline can use MRIQC to produce automated QC reports. This is suggested, but not required.
      If you'd like to use MRIQC, you need a working mriqc container (docker or singularity).
      If you don't have this yet, this should work to build the latest version:
        singularity build /location/to/mriqc-latest.simg docker://nipreps/mriqc:latest
    ", .trim = FALSE),
      prompt = "Location of mriqc container: ",
      type = "file", required = FALSE
    )
  }

  # location of ICA-AROMA fMRIprep container
  if (!validate_exists(scfg$compute_environment$aroma_container, description = "ICA-AROMA container", prompt_change = TRUE)) {
    scfg$compute_environment$aroma_container <- prompt_input(
      instruct = glue("
      The pipeline can use ICA-AROMA to denoise fMRI timeseries. As descried in Pruim et al. (2015), this
      is a data-driven step that produces a set of temporal regressors that are thought to be motion-related.
      If you would like to use ICA-AROMA in the pipeline, you need to build a singularity container of this
      workflow. Follow the instructions here: https://fmripost-aroma.readthedocs.io/latest/

      This is required if you say 'yes' to running AROMA during study setup.
    ", .trim = FALSE),
      prompt = "Location of ICA-AROMA container: ",
      type = "file", required = FALSE
    )
  }

  return(scfg)
}

# not currently used -- just testing
get_singularity_string <- function(scfg) {
  SINGULARITY_CMD <- "singularity run --cleanenv -B $BIDS_DIR:/data -B ${TEMPLATEFLOW_HOST_HOME}:${SINGULARITYENV_TEMPLATEFLOW_HOME} -B $L_SCRATCH:/work -B ${LOCAL_FREESURFER_DIR}:/fsdir $STUDY/images/poldracklab_fmriprep_1.5.0.simg"
}