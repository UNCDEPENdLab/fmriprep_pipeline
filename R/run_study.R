
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
run_study <- function(scfg, steps=NULL, prompt = TRUE, debug = FALSE, force = FALSE) {
  checkmate::assert_list(scfg)
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert_flag(prompt)
  checkmate::assert_flag(debug)
  checkmate::assert_flag(force)

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
    if ("bids_conversion" %in% steps) {
      if (is.null(scfg$heudiconv$sub_regex)) stop("Cannot run BIDS conversion without a subject regex.")
      if (is.null(scfg$heudiconv$ses_regex)) stop("Cannot run BIDS conversion without a session regex.")
      if (is.null(scfg$compute_environment$heudiconv_container)) stop("Cannot run BIDS conversion without a heudiconv container.")
    }

    if ("bids_validation" %in% steps && !validate_exists(scfg$compute_environment$bids_validator)) {
      stop("Cannot run BIDS conversion without a bids_validator location.")
    }

    if ("mriqc" %in% steps && !validate_exists(scfg$compute_environment$mriqc_container)) {
      stop("Cannot run MRIQC without a valid MRIQC container.")
    }

    if ("fmriprep" %in% steps && !validate_exists(scfg$compute_environment$fmriprep_container)) {
      stop("Cannot run fmriprep without a valid fmriprep container.")
    }

    if ("aroma" %in% steps && !validate_exists(scfg$compute_environment$aroma_container)) {
      stop("Cannot run AROMA without a valid AROMA container.")
    }

    if ("postprocess" %in% steps && (is.null(scfg$postprocess$processing_steps))) {
      stop("Cannot run postprocessing without a valid postprocess configuration.")
    }

    # if (isTRUE(scfg$run_aroma)) steps <- c(steps, "aroma")
    nm <- steps
    steps <- rep(TRUE, length(steps))
    names(steps) <- nm

    scfg$log_level <- "INFO" # how much detail to park in logs
    scfg$debug <- debug # pass forward debug flag from arguments
    scfg$force <- force # pass forward force flag from arguments
  } else {
    steps <- c()
    cat("\nPlease select which steps to run:\n")
    steps["bids_conversion"] <- ifelse(is.null(scfg$compute_environment$heudiconv_container), FALSE, prompt_input(instruct = "Run BIDS conversion?", type = "flag"))
    steps["bids_validation"] <- ifelse(is.null(scfg$compute_environment$bids_validator), FALSE, prompt_input(instruct = "Run BIDS validation?", type = "flag"))
    steps["mriqc"] <- ifelse(is.null(scfg$compute_environment$mriqc_container), FALSE, prompt_input(instruct = "Run MRIQC?", type = "flag"))
    steps["fmriprep"] <- ifelse(is.null(scfg$compute_environment$fmriprep_container), FALSE, prompt_input(instruct = "Run fmriprep?", type = "flag"))
    steps["aroma"] <- ifelse(is.null(scfg$compute_environment$aroma_container) && isTRUE(scfg$run_aroma), FALSE, prompt_input(instruct = "Run ICA-AROMA?", type = "flag"))
    steps["postprocess"] <- ifelse(is.null(scfg$postprocess$processing_steps), FALSE, prompt_input(instruct = "Run postprocessing?", type = "flag"))
    if (isFALSE(steps["aroma"]) && "apply_aroma" %in% scfg$postprocess$processing_steps) {
      warning(
        "Postprocessing includes the removal of motion-related AROMA components from the fMRI data, but you declined ",
        "to run AROMA as part of the pipeline. Postprocessing will likely fail if AROMA components cannot be found."
      )
    }
    # check whether to run in debug mode
    scfg$debug <- prompt_input(instruct = "Run pipeline in debug mode? This will echo commands to logs, but not run them.", type = "flag")

    scfg$force <- prompt_input(instruct = "Force each processing step, even if it appears to be complete?", type = "flag")

    scfg$log_level <- prompt_input(
      instruct = "What level of detail would you like in logs? Options are INFO, DEBUG, ERROR.",
      type = "character", among=c("INFO", "ERROR", "DEBUG")
    )
  }

  # look for subject directories in the DICOM directory
  # empty default data.frame for dicom directories
  subject_dicom_dirs <- data.frame(
    sub_id = character(), ses_id = character(),
    dicom_sub_dir = character(), dicom_ses_dir = character(), stringsAsFactors = FALSE
  )

  if (isTRUE(steps["bids_conversion"])) {
    subject_dicom_dirs <- get_subject_dirs(scfg$dicom_directory, sub_regex = scfg$heudiconv$sub_regex, ses_regex = scfg$heudiconv$ses_regex, full.names = TRUE)

    # add DICOM prefix
    names(subject_dicom_dirs) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(subject_dicom_dirs))

    if (length(subject_dicom_dirs) == 0L) {
      warning(glue("Cannot find any valid subject folders inside the DICOM directory: {scfg$dicom_directory}"))
    }
  }
  
  # look for all existing subject BIDS directories
  subject_bids_dirs <- get_subject_dirs(scfg$bids_directory, sub_regex = "^sub-.+", ses_regex = "^ses-.+", sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE)
  names(subject_bids_dirs) <- sub("(sub|ses)_dir", "bids_\\1_dir", names(subject_bids_dirs))
  
  subject_dirs <- merge(subject_dicom_dirs, subject_bids_dirs, by = c("sub_id", "ses_id"), all = TRUE)

  if (nrow(subject_dirs) == 0L) {
    stop(glue("Cannot find any valida subject folders in bids directory: {scfg$bids_directory}"))
  } else {
    # split data.frame by subject (some steps are subject-level, some are session-level)
    subject_dirs <- split(subject_dirs, subject_dirs$sub_id)

    for (ss in seq_along(subject_dirs)) {
      process_subject(scfg, subject_dirs[[ss]], steps)
      break
    }
  }
}
