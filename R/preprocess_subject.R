
process_subject <- function(scfg, sub_dir = NULL, steps = NULL, debug=FALSE) {
  checkmate::assert_directory_exists(sub_dir)
  checkmate::assert_list(scfg)
  checkmate::assert_logical(steps, names = "unique")
  expected <- c("bids_convert", "bids_validate", "mriqc", "fmriprep", "aroma", "postprocess")
  for (ee in expected) if (is.na(steps[ee])) steps[ee] <- FALSE # ensure we have valid logicals for expected fields

  sub_id <- get_sub_id(sub_dir)
  lg <- lgr::get_logger_glue(c("sub", sub_id))

  bids_convert_id <- mriqc_id <- fmriprep_id <- aroma_id <- postproc_id <- NULL

  # determine status of processing -- seems like we could swap in queries from job tracker
  submit_step <- function(name, parent_ids = NULL) {
    complete_file <- file.path(sub_dir, glue(".{name}_complete"))
    file_exists <- checkmate::test_file_exists(complete_file)

    job_id <- NULL
    if (steps[name] && (force || !file_exists)) {
      if (file_exists) {
        lg$debug("Removing existing .{name}_complete file {complete_file}")
        unlink(complete_file)
      }

      # launch submission function
      job_id <- do.call(glue("submit_{name}"), list(scfg, sub_dir, parent_ids, debug))
    }
    return(job_id)
  }

  ## Handle BIDS conversion
  bids_conversion_id <- submit_step("bids_conversion")

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



submit_fmriprep <- function(scfg, sub_dir = NULL, parent_ids = NULL) {
  checkmate::assert_list(scfg)
  checkmate::assert_directory_exists(sub_dir)

  lg <- get_subject_logger(sub_dir)

  if (!validate_exists(scfg$compute_enviroment$fmriprep_container)) {
    lg$debug("Unable to submit fmriprep for {sub_dir} because $compute_enviroment$fmriprep_container is missing.")
    return(NULL)
  }

  script <- get_job_script(scfg, "fmriprep")
  sched_args <- get_job_sched_args(scfg, "fmriprep")

  env_variables <- c(
    fmriprep_container = scfg$compute_enviroment$fmriprep_container,
    sub = subid, # TODO: strip from sub folder name?
    loc_bids_root = scfg$bids_directory,
    loc_mrproc_root = scfg$fmriprep_directory,
    debug_pipeline = scfg$debug,
    cli_options = scfg$fmriprep$cli_options
  )

  job_id <- cluster_job_submit(script,
    scheduler = scfg$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

  return(job_id)
}

submit_bids_conversion <- function(scfg, sub_dir = NULL, parent_ids = NULL) {
  # for now there, are no configuration settings -- a bit of future-proofing
  if (is.null(scfg$heudiconv$ncpus)) scfg$heudiconv$ncpus <- 1
  if (is.null(scfg$heudiconv$memgb)) scfg$heudiconv$memgb <- 16
  if (is.null(scfg$heudiconv$sched_args)) scfg$heudiconv$sched_args <- NULL

  # heudiconv
  script <- get_job_script(scfg, "heudiconv")
  sched_args <- get_job_sched_args(scfg, "heudiconv")
  env_variables <- c(
    heudiconv_container = scfg$compute_enviroment$heudiconv_container,
    loc_dicom_root = scfg$dicom_directory,
    loc_bids_root = scfg$bids_directory,
    heudiconv_heuristic = scfg$heudiconv$heuristic_file,
    sub = get_sub_id(sub_dir),
    debug_pipeline = scfg$debug
  )

  job_id <- cluster_job_submit(script,
    scheduler = scfg$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

  return(job_id)
}

submit_postprocess <- function(scfg, sub_dir = NULL, parent_ids = NULL) {
  #postprocess_id <- submit_step("postprocess", parent_ids = c(bids_conversion_id, bids_validation_id, fmriprep_id, aroma_id))

  # postprocessing
  script <- get_job_script(scfg, "postprocess")
  sched_args <- get_job_sched_args(scfg, "postprocess")
  env_variables <- c(
    heudiconv_container = scfg$compute_enviroment$heudiconv_container,
    loc_dicom_root = scfg$dicom_directory,
    loc_bids_root = scfg$bids_directory,
    heudiconv_heuristic = scfg$heudiconv$heuristic_file,
    sub = get_sub_id(sub_dir),
    debug_pipeline = scfg$debug
  )

  job_id <- cluster_job_submit(script,
    scheduler = scfg$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids
  )

}
  


get_job_script <- function(scfg = NULL, job_name) {
  checkmate::assert_string(job_name)
  ext <- ifelse(scfg$scheduler == "torque", "pbs", "sbatch")
  script <- system.file(glue("{job_name}_subject.{ext}"), package = "BrainGnomes")
  if (!checkmate::test_file_exists(script)) {
    stop("In get_job_script, cannot find expected script file: ", script)
  }
  return(script)
}

get_job_sched_args <- function(scfg=NULL, job_name) {
  checkmate::assert_string(job_name)

   if (scfg$scheduler == "slurm") {
     sched_args <- glue(
       "-N 1",
       "-n {scfg[[job_name]]$ncpus}",
       "--time={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "--memgb={scfg[[job_name]]$memgb}",
       "{scfg[[job_name]]$sched_args}",
       .trim = TRUE, .sep = " ", .null = NULL
     )
   } else {
     sched_args <- glue(
       "-l nodes1:ppn={scfg[[job_name]]$ncpus}",
       "-l walltime={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "-l mem={scfg[[job_name]]$memgb}",
       "{scfg[[job_name]]$sched_args}",
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
  } else if (!validate_exists(scfg$compute_enviroment$aroma_container)) {
    # WARNINGmessage(glue("Skipping AROMA in {sub_dir} because could not find AROMA container {scfg$compute_enviroment$aroma_container}")
    return(NULL)
  }

  # okay, push job
}