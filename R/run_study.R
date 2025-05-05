
run_study <- function(scfg, prompt = TRUE, force = FALSE) {
  checkmate::assert_list(scfg)
  checkmate::assert_flag(prompt)

  if (is.null(scfg$project_name)) stop("Cannot run a nameless project. Have you run setup_study() yet?")
  if (is.null(scfg$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_study() yet?")
  cat(glue("Running processing pipeline for: {scfg$project_name}\n"))
  cat(glue("  Project directory:   {pretty_arg(scfg$project_directory)}\n"))
  cat(glue("  BIDS directory:      {pretty_arg(scfg$bids_directory)}\n"))
  cat(glue("  fmriprep directory:  {pretty_arg(scfg$fmriprep_directory)}\n"))

  if (isFALSE(prompt)) {
    steps <- c(
      bids_convert = ifelse(is.null(scfg$heudiconv_container), FALSE, TRUE),
      bids_validate = ifelse(is.null(scfg$bidsvalidator_container), FALSE, TRUE),
      mriqc = ifelse(is.null(scfg$mriqc_container), FALSE, TRUE),
      fmriprep = ifelse(is.null(scfg$fmriprep_container), FALSE, TRUE),
      aroma = ifelse(is.null(scfg$aroma_container), FALSE, TRUE),
      postprocess = ifelse(is.null(scfg$postprocess$processing_steps), FALSE, TRUE)
    )
    if (isTRUE(scfg$run_aroma)) steps <- c(steps, "aroma")
    scfg$log_level <- "INFO" # how much detail to park in logs
  } else {
    steps <- c()
    run_bids_conversion <- ifelse(is.null(scfg$heudiconv_container), FALSE, prompt_input(instruct = "Run BIDS conversion?", type = "flag"))
    run_bids_validation <- ifelse(is.null(scfg$bidsvalidator_container), FALSE, prompt_input(instruct = "Run BIDS validation?", type = "flag"))
    run_mriqc <- ifelse(is.null(scfg$mriqc_container), FALSE, prompt_input(instruct = "Run MRIQC?", type = "flag"))
    run_fmriprep <- ifelse(is.null(scfg$fmriprep_container), FALSE, prompt_input(instruct = "Run fmriprep?", type = "flag"))
    run_aroma <- ifelse(is.null(scfg$aroma_container) && isTRUE(scfg$run_aroma), FALSE, prompt_input(instruct = "Run ICA-AROMA?", type = "flag"))
    run_postproc <- ifelse(is.null(scfg$postprocess$processing_steps), FALSE, prompt_input(instruct = "Run postprocessing?", type = "flag"))
    if (isFALSE(run_aroma) && "apply_aroma" %in% scfg$postprocess$processing_steps) {
      warning(
        "Postprocessing includes the removal of motion-related AROMA components from the fMRI data, but you declined ",
        "to run AROMA as part of the pipeline. Postprocessing will likely fail if AROMA components cannot be found."
      )
    }
    steps <- c(
        bids_convert = run_bids_conversion,
        bids_validate = run_bids_validation,
        mriqc = run_mriqc,
        fmriprep = run_fmriprep,
        aroma = run_aroma,
        postprocess = run_postproc
    )
    
    scfg$log_level <- prompt_input(
      prompt = ">", instruct = "What level of detail would you like in logs? Options are INFO, DEBUG,..",
      type = "character", among=c("INFO", "ERROR", "DEBUG")
    )
  }

  sub_dirs <- get_bids_subjects(scfg$bids_directory)

  debug <- prompt_input(instruct = "Run pipeline in debug mode? This will echo commands to logs, but not run them.", type = "flag")

  if (length(sub_dirs) == 0L) {
    stop(glue("Cannot find any sub-* folders inside the bids directory: {scfg$bids_directory}"))
  } else {
    for (ss in sub_dirs) {
      process_subject(scfg, ss, steps, force = force, debug = debug)
    }
  }
}
