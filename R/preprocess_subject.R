
#' Preprocess a single subject
#' @param scfg A list of configuration settings
process_subject <- function(scfg, sub_cfg = NULL, steps = NULL) {
  checkmate::assert_class(scfg, "bg_study_cfg")
  checkmate::assert_list(sub_cfg)
  expected_fields <- c("sub_id", "ses_id", "dicom_sub_dir", "dicom_ses_dir", "bids_sub_dir", "bids_ses_dir")
  checkmate::assert_names(names(sub_cfg), must.include = expected_fields, type = "unique")
  checkmate::assert_logical(steps, names = "unique")
  expected <- c("bids_conversion", "bids_validation", "mriqc", "fmriprep", "aroma", "postprocess")
  for (ee in expected) if (is.na(steps[ee])) steps[ee] <- FALSE # ensure we have valid logicals for expected fields

  log_str <- "sub-{sub_cfg$sub_id}"
  if (!is.na(sub_cfg$ses_id)) log_str <- paste0(log_str, "_ses-{sub_cfg$ses_id}")

  # use session BIDS directory if it exists, otherwise use subject BIDS directory
  bids_sub_dir <- ifelse(is.na(sub_cfg$bids_ses_dir), sub_cfg$bids_sub_dir, sub_cfg$bids_ses_dir)
  dicom_sub_dir <- ifelse(is.na(sub_cfg$dicom_ses_dir), sub_cfg$dicom_sub_dir, sub_cfg$dicom_ses_dir)
  
  lg <- lgr::get_logger_glue(log_str)

  bids_conversion_id <- mriqc_id <- fmriprep_id <- aroma_id <- postproc_id <- NULL

  # .*complete files should always be placed in the subject BIDS directory
  # determine status of processing -- seems like we could swap in queries from job tracker
  submit_step <- function(name, parent_ids = NULL) {
    complete_file <- file.path(bids_sub_dir, glue(".{name}_complete"))
    file_exists <- checkmate::test_file_exists(complete_file)

    job_id <- NULL
    if (steps[name] && (scfg$force || !file_exists)) {
      if (file_exists) {
        lg$debug("Removing existing .{name}_complete file {complete_file}")
        unlink(complete_file)
      }

      # launch submission function
      dir <- ifelse(name == "bids_conversion", dicom_sub_dir, bids_sub_dir)
      job_id <- do.call(glue("submit_{name}"), list(scfg, dir, sub_cfg$sub_id, sub_cfg$ses_id, parent_ids))
    } else {
      job_id <- NULL # job is already complete or is not requested
      if (file_exists) {
        lg$debug("Skipping {name} for {sub_dir} because .{name}_complete file already exists.")
      } else {
        lg$debug("Skipping {name} for {sub_dir} because step is not requested.")
      }
    }
    return(job_id)
  }

  ## Handle BIDS conversion
  bids_conversion_id <- submit_step("bids_conversion")

  return(TRUE) # quick drop for now

  ## Handle BIDS validation
  bids_validation_id <- submit_step("bids_validation", parent_ids = bids_conversion_id)

  ## Handle MRIQC
  mriqc_id <- submit_step("mriqc", parent_ids = c(bids_conversion_id, bids_validation_id))

  ## Handle fmriprep
  fmriprep_id <- submit_step("fmriprep", parent_ids = c(bids_conversion_id, bids_validation_id))

  ## Handle aroma
  aroma_id <- submit_step("aroma", parent_ids = c(bids_conversion_id, bids_validation_id, fmriprep_id))

  ## Handle postprocessing
  postprocess_id <- submit_step("postprocess", parent_ids = c(bids_conversion_id, bids_validation_id, fmriprep_id, aroma_id))

  return(TRUE) # nothing interesting for now
}

submit_fmriprep <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  checkmate::assert_list(scfg)
  checkmate::assert_directory_exists(sub_dir)

  lg <- get_subject_logger(sub_dir)

  if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
    lg$debug("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    return(NULL)
  }

  script <- get_job_script(scfg, "fmriprep")
  sched_args <- get_job_sched_args(scfg, "fmriprep")

  cli_options <- set_cli_options(scfg$fmriprep$cli_options, c(
    glue("--nthreads {scfg$fmriprep$ncores}"),
    glue("--omp-nthreads {scfg$fmriprep$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$scratch_directory}"),
  ))

  env_variables <- c(
    fmriprep_container = scfg$compute_environment$fmriprep_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$bids_directory,
    loc_mrproc_root = scfg$fmriprep_directory,
    debug_pipeline = scfg$debug,
    cli_options = scfg$fmriprep$cli_options,
    pkg_dir=system.file(package = "BGprocess")
  )

  job_id <- cluster_job_submit(script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

  return(job_id)
}

submit_bids_conversion <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  # heudiconv  --files dicom/219/itbs/*/*.dcm -o Nifti -f Nifti/code/heuristic1.py -s 219 -ss itbs -c dcm2niix -b --minmeta --overwrite

  jobid_str <- if (!is.null(ses_id)) {
    glue("heudiconv-sub-{sub_id}_ses-{ses_id}")
  } else {
    glue("heudiconv-sub-{sub_id}")
  }

  # heudiconv
  script <- get_job_script(scfg, "heudiconv")
  sched_args <- get_job_sched_args(scfg, "heudiconv")
  sched_args <- set_cli_options(sched_args, glue("--job-name={jobid_str}"))

  env_variables <- c(
    heudiconv_container = scfg$compute_environment$heudiconv_container,
    loc_sub_dicoms = sub_dir,
    loc_bids_root = scfg$bids_directory,
    heudiconv_heuristic = scfg$heudiconv$heuristic_file,
    validate_bids = scfg$heudiconv$validate_bids,
    sub_id = sub_id,
    ses_id = ses_id,
    debug_pipeline = scfg$debug,
    pkg_dir = system.file(package = "BGprocess"), # root of inst folder for installed R package
    cmd_log="/proj/mnhallqlab/projects/preproc_pipeline_test_data/output.txt"
  )

  job_id <- fmri.pipeline::cluster_job_submit(script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

  return(job_id)

}


submit_bids_validation <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  jobid_str <- if (!is.null(ses_id)) {
    glue("bids_validation-sub-{sub_id}_ses-{ses_id}")
  } else {
    glue("bids_validation-sub-{sub_id}")
  }

  # bids_validator
  script <- get_job_script(scfg, "bids_validator_subject")
  sched_args <- get_job_sched_args(scfg, "bids_validator_subject")
  sched_args <- set_cli_options(sched_args, glue("--job-name={jobid_str}"))

  env_variables <- c(
    bids_validator = scfg$compute_environment$bids_validator,
    bids_dir = sub_dir,
    sub_id = sub_id,
    ses_id = ses_id,
    debug_pipeline = scfg$debug,
    pkg_dir = system.file(package = "BGprocess"), # root of inst folder for installed R package
    outfile = scfg$bids_validation$outfile
  )

  job_id <- fmri.pipeline::cluster_job_submit(script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

  return(job_id)
}



submit_postprocess <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  #postprocess_id <- submit_step("postprocess", parent_ids = c(bids_conversion_id, bids_validation_id, fmriprep_id, aroma_id))

  # postprocessing
  script <- get_job_script(scfg, "postprocess")
  sched_args <- get_job_sched_args(scfg, "postprocess")
  env_variables <- c(
    heudiconv_container = scfg$compute_environment$heudiconv_container,
    loc_sub_dicoms = scfg$dicom_directory,
    loc_bids_root = scfg$bids_directory,
    heudiconv_heuristic = scfg$heudiconv$heuristic_file,
    sub = get_sub_id(sub_dir),
    debug_pipeline = scfg$debug
  )

  job_id <- cluster_job_submit(script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

}

get_job_script <- function(scfg = NULL, job_name) {
  checkmate::assert_string(job_name)
  
  ext <- ifelse(scfg$compute_environment$scheduler == "torque", "pbs", "sbatch")
  expect_file <- glue("hpc_scripts/{job_name}_subject.{ext}")
  script <- system.file(expect_file, package = "BGprocess")
  if (!checkmate::test_file_exists(script)) {
    stop("In get_job_script, cannot find expected script file: ", expect_file)
  }
  return(script)
}

#' Convert scheduler arguments into a scheduler-specific string
#' @param scfg A list of configuration settings
#' @param job_name The name of the job (e.g., "fmriprep", "heudiconv")
#' @return A character string of scheduler arguments
#' @importFrom glue glue
#' @importFrom checkmate assert_string
#' @keywords internal
#' @noRd
get_job_sched_args <- function(scfg=NULL, job_name) {
  checkmate::assert_string(job_name)

  # TODO: need to use cli_opts approach to remove conflicting/redundant fields in sched_args for -n, -N, etc.

  sched_args <- scfg[[job_name]]$sched_args
  # convert empty strings to NULL for compatibility with glue
  if (length(sched_args) == 0L || is.na(sched_args[1L]) || sched_args[1L] == "") sched_args <- NULL

   if (scfg$compute_environment$scheduler == "slurm") {
     sched_args <- glue(
       "-N 1",
       "-n {scfg[[job_name]]$ncores}",
       "--time={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "--mem={scfg[[job_name]]$memgb}g",
       "{sched_args}",
       .trim = TRUE, .sep = " ", .null = NULL
     )
   } else {
     sched_args <- glue(
       "-l nodes1:ppn={scfg[[job_name]]$ncores}",
       "-l walltime={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "-l mem={scfg[[job_name]]$memgb}",
       "{sched_args}",
       .trim = TRUE, .sep = " ", .null = NULL
     )
   }
   
  return(sched_args)

}

submit_aroma <- function(scfg, sub_dir = NULL, parent_ids = NULL) {
  if (!isTRUE(scfg$run_aroma)) {
    # logging needed
    # DEBUGmessage(glue("Skipping AROMA in {sub_dir} because run_aroma is FALSE in config")
    return(NULL)
  } else if (!validate_exists(scfg$compute_environment$aroma_container)) {
    # WARNINGmessage(glue("Skipping AROMA in {sub_dir} because could not find AROMA container {scfg$compute_environment$aroma_container}")
    return(NULL)
  }

  # okay, push job
}