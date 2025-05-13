
#' Run the processing pipeline
#' @param scfg A list containing the study configuration.
#' @param prompt A logical value indicating whether to prompt the user for input on which steps to run.
#' @param force A logical value indicating whether to force the execution of all steps, regardless of their current status.
#' @return A logical value indicating whether the processing pipeline was successfully run.
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have a valid study configuration list named `study_config`
#' run_study(study_config, prompt = TRUE, force = FALSE)
#' }
#' @importFrom glue glue
#' @importFrom checkmate assert_list assert_flag assert_directory_exists
#' @importFrom lgr get_logger_glue
run_study <- function(scfg, prompt = TRUE, force = FALSE) {
  checkmate::assert_list(scfg)
  checkmate::assert_flag(prompt)

  if (is.null(scfg$project_name)) stop("Cannot run a nameless project. Have you run setup_study() yet?")
  if (is.null(scfg$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_study() yet?")
  cat(glue("
    \nRunning processing pipeline for: {scfg$project_name}
      Project directory:   {pretty_arg(scfg$project_directory)}
      DICOM directory:     {pretty_arg(scfg$dicom_directory)}
      BIDS directory:      {pretty_arg(scfg$bids_directory)}
      fmriprep directory:  {pretty_arg(scfg$fmriprep_directory)}\n
      "))
  
  if (isFALSE(prompt)) {
    steps <- c(
      bids_conversion = ifelse(is.null(scfg$compute_environment$heudiconv_container), FALSE, TRUE),
      bids_validation = ifelse(is.null(scfg$compute_environment$bids_validator), FALSE, TRUE),
      mriqc = ifelse(is.null(scfg$compute_environment$mriqc_container), FALSE, TRUE),
      fmriprep = ifelse(is.null(scfg$compute_environment$fmriprep_container), FALSE, TRUE),
      aroma = ifelse(is.null(scfg$compute_environment$aroma_container), FALSE, TRUE),
      postprocess = ifelse(is.null(scfg$postprocess$processing_steps), FALSE, TRUE)
    )
    if (isTRUE(scfg$run_aroma)) steps <- c(steps, "aroma")
    scfg$log_level <- "INFO" # how much detail to park in logs
  } else {
    steps <- c()
    cat("\nPlease select which steps to run:\n")
    run_bids_conversion <- ifelse(is.null(scfg$compute_environment$heudiconv_container), FALSE, prompt_input(instruct = "Run BIDS conversion?", type = "flag"))
    run_bids_validation <- ifelse(is.null(scfg$compute_environment$bids_validator), FALSE, prompt_input(instruct = "Run BIDS validation?", type = "flag"))
    run_mriqc <- ifelse(is.null(scfg$compute_environment$mriqc_container), FALSE, prompt_input(instruct = "Run MRIQC?", type = "flag"))
    run_fmriprep <- ifelse(is.null(scfg$compute_environment$fmriprep_container), FALSE, prompt_input(instruct = "Run fmriprep?", type = "flag"))
    run_aroma <- ifelse(is.null(scfg$compute_environment$aroma_container) && isTRUE(scfg$run_aroma), FALSE, prompt_input(instruct = "Run ICA-AROMA?", type = "flag"))
    run_postproc <- ifelse(is.null(scfg$postprocess$processing_steps), FALSE, prompt_input(instruct = "Run postprocessing?", type = "flag"))
    if (isFALSE(run_aroma) && "apply_aroma" %in% scfg$postprocess$processing_steps) {
      warning(
        "Postprocessing includes the removal of motion-related AROMA components from the fMRI data, but you declined ",
        "to run AROMA as part of the pipeline. Postprocessing will likely fail if AROMA components cannot be found."
      )
    }
    steps <- c(
        bids_conversion = run_bids_conversion,
        bids_validation = run_bids_validation,
        mriqc = run_mriqc,
        fmriprep = run_fmriprep,
        aroma = run_aroma,
        postprocess = run_postproc
    )
    
    scfg$log_level <- prompt_input(
      instruct = "What level of detail would you like in logs? Options are INFO, DEBUG,..",
      type = "character", among=c("INFO", "ERROR", "DEBUG")
    )
  }

  # look for subject directories in the DICOM directory
  if (isTRUE(run_bids_conversion)) {
    subject_dicom_dirs <- get_subject_dirs(scfg$dicom_directory, sub_regex = scfg$heudiconv$sub_regex, ses_regex = scfg$heudiconv$ses_regex, full.names=TRUE)

    # add DICOM prefix
    names(subject_dicom_dirs) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(subject_dicom_dirs))
    
    if (length(subject_dicom_dirs) == 0L) {
      stop(glue("Cannot find any valid subject folders inside the DICOM directory: {scfg$dicom_directory}"))
    }
  } else {
    subject_dicom_dirs <- character()
  }

  # look for all existing subject BIDS directories
  subject_bids_dirs <- get_subject_dirs(scfg$bids_directory, sub_regex = "^sub-.+", ses_regex = "^ses-.+", sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE)
  names(subject_bids_dirs) <- sub("(sub|ses)_dir", "bids_\\1_dir", names(subject_bids_dirs))
  
  subject_dirs <- merge(subject_dicom_dirs, subject_bids_dirs, by = c("sub_id", "ses_id"), all = TRUE)

  # determine which subjects are missing from the BIDS directory

  debug <- prompt_input(instruct = "Run pipeline in debug mode? This will echo commands to logs, but not run them.", type = "flag")

  if (length(sub_dirs) == 0L) {
    stop(glue("Cannot find any sub-* folders inside the bids directory: {scfg$bids_directory}"))
  } else {
    for (ss in sub_dirs) {
      process_subject(scfg, ss, steps, force = force, debug = debug)
    }
  }
}
