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

  # location of fmriprep outputs -- enforce that this must be within the project directory
  scfg$fmriprep_directory <- file.path(scfg$project_directory, "data_fmriprep")

  scfg$scratch_directory <- prompt_input("Work directory: ",
    instruct = glue("
    fmriprep uses a lot of disk space for processing intermediate files. It's best if these
    are written to a scratch/temporary directory that is cleared regularly so that you don't
    use up precious disk space for unnecessary files. Please indicate where these intermediate
    file should be written.
  ", .trim = FALSE), type = "character"
  )
  if (!checkmate::test_directory_exists(scfg$scratch_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$scratch_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$scratch_directory, recursive = TRUE)
  }

  scfg$run_aroma <- prompt_input("Run AROMA?",
    instruct = glue("
      As of v24, fmriprep has now removed ICA-AROMA from its codebase, splitting this off to
      a separate BIDS app called fmripost-aroma. Do you want to run the fMRI data through ICA-AROMA?
      If so, you will subsequently be asked for the location of an ICA-AROMA container file. Note that
      saying 'yes' to this, only runs AROMA, but does not remove motion-related components from the fMRI
      timeseries. That is a postprocessing decision, which you will be asked about in that context.
      ", .trim = FALSE), type = "flag"
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
  scfg <- setup_heudiconv(scfg)
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
setup_heudiconv <- function(scfg, prompt_all = FALSE) {
  defaults <- list(
    memgb = 16,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  # scfg$heudiconv <- populate_defaults(scfg$heudiconv, defaults)

  cat("This step sets up heudiconv.\n")

  scfg$heudiconv$heuristic_file <- prompt_input(instruct = glue("What is the location of the heudiconv heuristic file?"), type = "file", prompt = ">")
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


#' Helper function to local all subject directories within a BIDS-compatible folder
#' @param bids_root The path to a BIDS-compatible folder
#' @param full.names If `TRUE`, return the absolute paths to the subject folders. If
#'   `FALSE`, return paths relative to `bids_root`. Default: `FALSE`.
#' @return a character vector of subject folders located in `bids_root`
#' @keywords internal
#' @importFrom checkmate assert_directory_exists assert_flag
get_bids_subjects <- function(bids_root = NULL, full.names = FALSE) {
  checkmate::assert_directory_exists(bids_root)
  checkmate::assert_flag(full.names)

  # List directories in the root BIDS folder
  entries <- list.dirs(bids_root, recursive = FALSE, full.names = FALSE)

  # Keep only those starting with "sub-"
  subject_dirs <- entries[grepl("^sub-[a-zA-Z0-9]+", entries)]

  if (isTRUE(full.names)) subject_dirs <- file.path(bids_root, subject_dirs)
  return(subject_dirs)
}

# helper function to grab the subject id from a subject's bids directory
get_sub_id <- function(bids_dir) {
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