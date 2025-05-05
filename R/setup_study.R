#' validate the structure of a study configuration object
#' @param scfg a study configuration file as produced by `load_study` or `setup_study`
#' @keywords internal
validate_scfg <- function(scfg = list()) {
  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    class(scfg) <- c(class(scfg), "bg_study_cfg")
  }



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
    scfg <- read_yaml(file)
  } else {
    scfg <- list()
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

  scfg$compute_environment <- setup_compute_environment(scfg)

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

}

#' Specify the mriqc settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$mriqc` populated
#' @keywords internal
setup_mriqc <- function(scfg) {
  # TOOO once I have internet
  defaults <- list(
    memgb = 32,
    nhours = 12,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  #scfg$mriqc <- populate_defaults(scfg$fmriprep, defaults)
  scfg <- setup_job(scfg, "mriqc", defaults = defaults)
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

  scfg <- setup_job(scfg, "heudiconv", defaults = defaults)

  scfg$heudiconv$heuristic_file <- prompt_input(instruct = glue("What is the location of the heudiconv heurtistic file?"), type = "file", prompt = ">")
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
          scfg$compute_enviroment <- ff$compute_enviroment
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

  if (is.null(scfg$scheduler) || !checkmate::test_subset(scfg$scheduler, c("slurm", "torque"))) {
    scfg$scheduler <- prompt_input("Scheduler (slurm/torque): ",
      instruct = "The pipeline currently runs on TORQUE (aka qsub) and SLURM clusters.\nWhich will you use?",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
  }

  # location of fmriprep container
  if (!validate_exists(scfg$compute_enviroment$fmriprep_container, description = "fmriprep container", prompt_change = TRUE)) {
    scfg$compute_enviroment$fmriprep_container <- prompt_input(
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
  if (!validate_exists(scfg$compute_enviroment$heudiconv_container, description = "heudiconv container", prompt_change = TRUE)) {
    scfg$compute_enviroment$heudiconv_container <- prompt_input(
      instruct = glue("
      The pipeline depends on having a working heudiconv container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://heudiconv.readthedocs.io/en/latest/installation.html#install-container
    ", .trim = FALSE),
      prompt = "Location of heudiconv container: ",
      type = "file"
    )
  }

  # location of mriqc container
  if (!validate_exists(scfg$compute_enviroment$mriqc_container, description = "mriqc container", prompt_change = TRUE)) {
    scfg$compute_enviroment$mriqc_container <- prompt_input(
      instruct = glue("
      The pipeline can use MRIQC to produce automated QC reports. This is suggested, but not required.
      If you'd like to use MRIQC, you need a working mriqc container (docker or singularity).
      If you don't have this yet, this should work to build the latest version:
        singularity build /location/to/mriqc-latest.simg docker://nipreps/mriqc:latest
    ", .trim = FALSE),
      prompt = "Location of mriqc container (press enter to skip): ",
      type = "file", required = FALSE
    )
  }

  # location of ICA-AROMA fMRIprep container
  if (!validate_exists(scfg$compute_enviroment$aroma_container, description = "ICA-AROMA container", prompt_change = TRUE)) {
    scfg$compute_enviroment$aroma_container <- prompt_input(
      instruct = glue("
      The pipeline can use ICA-AROMA to denoise fMRI timeseries. As descried in Pruim et al. (2015), this
      is a data-driven step that produces a set of temporal regressors that are thought to be motion-related.
      If you would like to use ICA-AROMA in the pipeline, you need to build a singularity container of this
      workflow. Follow the instructions here: https://fmripost-aroma.readthedocs.io/latest/

      This is required if you say 'yes' to running AROMA during study setup.
    ", .trim = FALSE),
      prompt = "Location of ICA-AROMA container (press enter to skip): ",
      type = "file", required = FALSE
    )
  }

  return(scfg)
}

# not currently used -- just testing
get_singularity_string <- function(scfg) {
  SINGULARITY_CMD <- "singularity run --cleanenv -B $BIDS_DIR:/data -B ${TEMPLATEFLOW_HOST_HOME}:${SINGULARITYENV_TEMPLATEFLOW_HOME} -B $L_SCRATCH:/work -B ${LOCAL_FREESURFER_DIR}:/fsdir $STUDY/images/poldracklab_fmriprep_1.5.0.simg"
}