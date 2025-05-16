
#' Preprocess a single subject
#' @param scfg A list of configuration settings
#' @param sub_cfg A data.frame of subject configuration settings
#' @param steps A named logical vector indicating which steps to run
#' @return A logical value indicating whether the preprocessing was successful
#' @importFrom glue glue
#' @importFrom checkmate assert_class assert_list assert_names assert_logical
#' @keywords internal
process_subject <- function(scfg, sub_cfg = NULL, steps = NULL) {
  checkmate::assert_class(scfg, "bg_study_cfg")
  checkmate::assert_data_frame(sub_cfg)
  expected_fields <- c("sub_id", "ses_id", "dicom_sub_dir", "dicom_ses_dir", "bids_sub_dir", "bids_ses_dir")
  checkmate::assert_names(names(sub_cfg), must.include = expected_fields, type = "unique")
  stopifnot(length(unique(sub_cfg$sub_id)) == 1L)
  multi_session <- nrow(sub_cfg) > 1L
  if (multi_session) {
    if (any(is.na(sub_cfg$ses_id))) stop("Session IDs are required for multi-session inputs to process_subject.")
    if (any(duplicated(sub_cfg$ses_id))) stop("Duplicate session IDs found in sub_cfg. process_subject requires unique session IDs.")
  }
  checkmate::assert_logical(steps, names = "unique")
  expected <- c("bids_conversion", "bids_validation", "mriqc", "fmriprep", "aroma", "postprocess")
  for (ee in expected) if (is.na(steps[ee])) steps[ee] <- FALSE # ensure we have valid logicals for expected fields

  sub_id <- sub_cfg$sub_id[1L]
  bids_sub_dir <- sub_cfg$bids_sub_dir[1L]
  log_str <- "sub-{sub_cfg$sub_id}"
  lg <- lgr::get_logger_glue(log_str)
  
  bids_conversion_ids <- mriqc_id <- fmriprep_id <- aroma_id <- postprocess_ids <- NULL

  # N.B. fmriprep processes a subject, not a session... Thus, we need to submit a top-level job for the subject

  # BIDS conversion and postprocessing are session-specific, so we need to check for the session ID
  # BIDS validation, fmriprep, mriqc, and aroma are subject-specific (sessions nested within subjects)

  # .*complete files should always be placed in the subject BIDS directory
  # determine status of processing -- seems like we could swap in queries from job tracker
  submit_step <- function(name, row_idx = 1L, parent_ids = NULL) {
    session_level <- name %in% c("bids_conversion", "postprocess") # only these two are session-level

    ses_id <- sub_cfg$ses_id[row_idx]
    has_ses <- !is.na(ses_id)
    ses_str <- ifelse(has_ses && session_level, glue("_ses-{ses_id}"), "") # qualifier for .complete file
    complete_file <- file.path(sub_cfg$bids_sub_dir[row_idx], glue(".{name}{ses_str}_complete"))
    file_exists <- checkmate::test_file_exists(complete_file)

    job_id <- NULL
    if (steps[name] && (scfg$force || !file_exists)) {
      if (file_exists) {
        lg$debug("Removing existing .{name}{ses_str}_complete file {complete_file}")
        unlink(complete_file)
      }

      # determine the directory to use for the job submission
      if (session_level && has_ses) {
        # if it's a session-level process and we have a valid session-level input, use the session directory
        dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_ses_dir[row_idx], sub_cfg$bids_ses_dir[row_idx])
      } else {
        # if it's a subject-level process or we don't have a valid session-level input, use the subject directory
        dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_sub_dir[row_idx], sub_cfg$bids_sub_dir[row_idx])
      }
      
      # launch submission function
      job_id <- do.call(glue("submit_{name}"), list(scfg, dir, sub_cfg$sub_id[row_idx], sub_cfg$ses_id[row_idx], parent_ids))
    } else {
      job_id <- NULL # job is already complete or is not requested
      if (file_exists) {
        lg$debug("Skipping {name} for {sub_id} because .{name}{ses_str}_complete file already exists.")
      } else {
        lg$debug("Skipping {name} for {sub_id} because step is not requested.")
      }
    }
    return(job_id)
  }

  ## Handle BIDS conversion -- session-level
  n_inputs <- nrow(sub_cfg)

  # need unlist because NULL will be returned for jobs not submitted -- yielding a weird list of NULLs
  bids_conversion_ids <- unlist(lapply(seq_len(n_inputs), function(idx) submit_step("bids_conversion", row_idx = idx)))
  
  if (isTRUE(steps["bids_conversion"])) {  
    # Use expected directory as input to subsequent steps, anticipating that conversion completes
    # and the expected directory is created. If conversion fails, the dependent jobs should automatically fail.
    bids_sub_dir <- file.path(scfg$bids_directory, glue("sub-{sub_cfg$sub_id[1L]}"))
    bids_ses_dir <- if (multi_session) file.path(scfg$bids_directory, glue("sub-{sub_cfg$sub_id}"), glue("ses-{sub_cfg$ses_id}")) else rep(NA_character_, nrow(sub_cfg))

    # When bids_sub_dir and bids_ses_dir exist, do they match these expectations?
    extant_bids <- !is.na(sub_cfg$bids_sub_dir)
    if (!identical(sub_cfg$bids_sub_dir[extant_bids], bids_sub_dir[extant_bids])) {
      lg$warn(glue("Exiting process_subject for {sub_id} because expected BIDS directory does not match: {bids_sub_dir}"))
      return(TRUE)
    }

    extant_bids_ses <- !is.na(sub_cfg$bids_ses_dir)
    if (multi_session && !identical(sub_cfg$bids_ses_dir[extant_bids_ses], bids_ses_dir[extant_bids_ses])) {
      lg$warn(glue("Exiting process_subject for {sub_id} because expected BIDS session directory does not match: {bids_ses_dir[1L]}"))
      return(TRUE)
    }
    
  } else if (!checkmate::test_directory_exists(bids_sub_dir)) {
    lg$warn(glue("Exiting process_subject for {sub_id} because expected BIDS directory does not exist: {bids_sub_dir}"))
    return(TRUE)
  }

  # Everything after BIDS conversion depends on the BIDS directory existing

  ## Handle BIDS validation
  bids_validation_id <- submit_step("bids_validation", parent_ids = bids_conversion_ids)

  ## Handle MRIQC
  # mriqc_id <- submit_step("mriqc", parent_ids = c(bids_conversion_ids, bids_validation_id))

  ## Handle fmriprep
  fmriprep_id <- submit_step("fmriprep", parent_ids = c(bids_conversion_ids, bids_validation_id))

  return(TRUE)

  ## Handle aroma
  aroma_id <- submit_step("aroma", parent_ids = c(bids_conversion_ids, bids_validation_id, fmriprep_id))

  ## Handle postprocessing
  postprocess_ids <- unlist(lapply(seq_len(n_inputs), function(idx) submit_step("postprocess", row_idx = idx, parent_ids = c(bids_conversion_ids, bids_validation_id, fmriprep_id, aroma_id))))

  return(TRUE) # nothing interesting for now
}


submit_bids_conversion <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  # heudiconv  --files dicom/219/itbs/*/*.dcm -o Nifti -f Nifti/code/heuristic1.py -s 219 -ss itbs -c dcm2niix -b --minmeta --overwrite

  jobid_str <- if (!is.null(ses_id) || is.na(ses_id[1L])) {
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
  jobid_str <- if (!is.null(ses_id) || is.na(ses_id[1L])) {
    glue("bids_validation-sub-{sub_id}_ses-{ses_id}")
  } else {
    glue("bids_validation-sub-{sub_id}")
  }

  # bids_validation
  script <- get_job_script(scfg, "bids_validation")
  sched_args <- get_job_sched_args(scfg, "bids_validation")
  sched_args <- set_cli_options(sched_args, glue("--job-name={jobid_str}"))

  env_variables <- c(
    bids_validator = scfg$compute_environment$bids_validator,
    bids_dir = sub_dir,
    sub_id = sub_id,
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

submit_fmriprep <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, parent_ids = NULL) {
  checkmate::assert_list(scfg)
  checkmate::assert_character(parent_ids, null.ok = TRUE)

  jobid_str <- if (!is.null(ses_id) || is.na(ses_id[1L])) {
    glue("fmriprep-sub-{sub_id}_ses-{ses_id}")
  } else {
    glue("fmriprep-sub-{sub_id}")
  }

  #lg <- get_subject_logger(sub_dir)

  if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
    #lg$debug("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    warning("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    return(NULL)
  }

  script <- get_job_script(scfg, "fmriprep")
  sched_args <- get_job_sched_args(scfg, "fmriprep")

  cli_options <- set_cli_options(scfg$fmriprep$cli_options, c(
    glue("--nthreads {scfg$fmriprep$ncores}"),
    glue("--omp-nthreads {scfg$fmriprep$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$scratch_directory}")
  ))

  env_variables <- c(
    fmriprep_container = scfg$compute_environment$fmriprep_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$bids_directory,
    loc_mrproc_root = scfg$fmriprep_directory,
    loc_scratch = scfg$scratch_directory,
    debug_pipeline = scfg$debug,
    cli_options = cli_options,
    pkg_dir=system.file(package = "BGprocess")
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