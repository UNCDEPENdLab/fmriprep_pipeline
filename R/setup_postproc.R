#' Specify postprocessing setting for a study
#' @param scfg a study configuration object produced by `setup_study`
setup_postprocessing <- function(scfg = list()) {
  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    stop("scfg input must be a bg_study_cfg object produced by setup_study")
  }

  defaults <- list(
    memgb = "50g",
    walltime = "24:00:00",
    ncores = "12",
    cli_options = "",
  )

  # need a way to only bother the user when something is missing.
  # maybe a "prompt_on_null" option to prompt input, with a current value?
  print(defaults)
  accept_defaults <- prompt_input("These be the defaults. Ye want?", type = "flag")
  if (accept_defaults) {
    scfg$postprocess <- populate_defaults(scfg$postprocess, defaults)
  }

  # global settings
  scfg$postprocess$keep_intermediates <- prompt_input("Do you want to keep postprocess intermediate files? This is typically only for debugging.", type = "flag")
  scfg$postprocess$overwrite <- prompt_input("Overwrite existing postprocess files?", type = "flag")
  scfg$postprocess$tr <- prompt_input("Repetition time (in seconds) of the scan sequence:", type = "numeric", lower = 0.01, upper = 100, len = 1)

  scfg$postprocess$apply_mask <- prompt_input(
    "Apply brain mask to postprocessed data?",
    type = "flag",
    instruct = glue("
      A brain mask is used in postprocessing to calculate quantiles (e.g., median) of the image to be used in smoothing and
      intensity normalization. Optionally, you can also apply a mask to the data as part of postprocessing, though
      many people analyze their data without a mask, applying one later (e.g., when reviewing group maps).
      
      Do you want to apply a brain mask to the postprocessed data? This will mask out non-brain voxels, which can
      make it faster for analysis (since empty voxels are omitted) and easier for familywise error correction
      (since you don't want to correct over non-brain voxels). If you say yes, I would strongly recommend specifying
      a single brain mask to be used for all subjects in the pipeline to avoid heterogeneity in masks from data-driven
      brain-extraction algorithms, which can lead to inconsistencies in the group (intersection) mask. If you do not
      apply the mask to the postprocessed data, I wouldn't worry too much about providing one here, though if you have
      a good one handy, it's a good idea.

      You'll be asked about providing a custom mask next.
    ", .trim = FALSE)
  )

  scfg$postprocess$brain_mask <- prompt_input("Brain mask to be used in postprocessing: ",
    instruct = glue("
      Here, you can specify a single file (e.g., the brain mask provided by MNI) that can be used
      across datasets. This is especially desirable if you will *apply* that mask to the data, an optional step, as
      having a common high-quality mask is important to ensure comparability across subjects.

      If you provide a mask here, please make sure that it matches the resolution and orientation of the data to which
      it will be applied, as the pipeline will not check this for you. If you have a mask that is correct in the same 
      stereotaxic space as the data, but has a different resolution, I recommend using AFNI's 3dresample like so: 
        3dresample -input <current_mask> -master <a_preproc_nifti_file_from_study> -prefix <resampled_mask> -rmode NN

      If you do not provide a brain mask, the pipeline will look for the mask calculated by fmriprep ('_desc-brain_mask')
      and if this is not available, the pipeline will calculate a mask using FSL's 98/2 method used in its preprocessing stream.

    ", .trim=FALSE),
    type = "file", len = 1L, required=FALSE
  )

  scfg <- setup_temporal_filter(scfg)
  scfg <- setup_spatial_smooth(scfg)
  scfg <- setup_intensity_normalization(scfg)
  scfg <- setup_confound_calculate(scfg)
  scfg <- setup_confound_regression(scfg)
  scfg <- setup_postproc_steps(scfg)

}

setup_postproc_steps <- function(scfg = list()) {
  if (is.null(scfg$postprocess$processing_steps)) {
    stop("missing processing_steps. Rung out of order?")
  }

  # arrange them in a desirable order
  processing_steps <- c()
  if ("apply_mask" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "apply_mask")
  if ("spatial_smooth" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "spatial_smooth")
  if ("apply_aroma" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "apply_aroma")
  if ("temporal_filter" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "temporal_filter")
  if ("confound_regression" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "confound_regression")
  if ("intensity_normalize" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "intensity_normalize")

  scfg$postprocess$force_processing_order <- prompt_input("Do you want to specify the postprocessing sequence?",
    instruct = glue("
    The order of postprocessing steps is important, particularly because if we filter certain frequencies from the fMRI data, we must filter
    any regressors that we later apply to the data -- that is, confounds and fMRI data must match in frequency content prior to regression.
    See Hallquist, Hwang, & Luna (2013) or Lindquist (2019) for details.

    Here, we have ordered the processing steps in what we believe is the best sequence for ensuring a sensible pipeline that
    avoids pitfalls, including the aforementioned matter of frequency alignment. Note that if temporal filtering is used,
    confound regressors are filtered to match. And likewise, if AROMA is used, confound regressors will also have AROMA components removed.

    You can specify a different postprocessing order than what is recommended, but with the cautionary note that we have not
    tested all possible sequences and unfortunate sequences could occur.
  ", .trim = FALSE),
    type = "flag", required = TRUE
  )
  
  if (isTRUE(scfg$postprocess$force_processing_order)) {
    proceed <- FALSE
    while (!proceed) {
      seq_glue <- glue("\nProcessing steps:\n\n{paste(seq_along(processing_sequence), processing_sequence, collapse = '\n', sep = '. ')}\n", .trim = FALSE)
      ss <- prompt_input(
        "Choose the order (separated by spaces): ",
        instruct = seq_glue,
        type = "integer", lower = 1, upper = length(processing_sequence), len = length(processing_sequence), split = "\\s+", uniq = TRUE
      )

      proceed_glue <- glue("\nYou specified the following processing order:\n\n{paste(seq_along(ss), processing_sequence[ss], collapse = '\n', sep = '. ')}\n", .trim = FALSE)
      proceed <- prompt_input("Is this correct?", instruct = proceed_glue, type = "flag")
    }

    scfg$postprocess$processing_steps <- processing_sequence[ss]
  } else {
    scfg$postprocess$processing_steps <- processing_sequence
  }
  
  if ("confound_calculate" %in% scfg$postprocess$processing_steps) {
    cat(glue("
      Confound calculation is also included as a part of post-processing.
      Whatever (relevant) processing steps are applied to the fMRI data will also be applied
      to the calculated confounds file. If you look at the config file, you'll also see 
      'confound_calculate' as a step in 'processing_steps'.
    "), .trim=FALSE)
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_calculate")
  }

  return(scfg)
}

#' Specify the confound regression settings for postprocessing
#' @param scfg a study configuration object created by `setup_study`
#' @return a modified version of `scfg` with the `$postprocess$confound_regression` field populated
#' @keywords internal
setup_confound_regression <- function(scfg = list()) {
  cur_val <- "confound_regression" %in% scfg$postprocess$processing_steps
  if ("confound_regression" %in% names(scfg)) {
    cat(strwrap(glue("
      Current confound regression settings:
        Apply confound regression: {cur_val}
        Columns that will be filtered to match fMRI data: {paste(scfg$postprocess$confound_regression$columns)}
        Columns that will not be filtered: {paste(scfg$postprocess$confound_regression$noproc_columns)}
        File prefix: {scfg$postprocess$confound_regression$prefix}
    "), width = 80, extent = 4), sep = "\n")

    change <- prompt_input("Change settings?", type = "flag")
    if (!change) return(scfg) # skip out
  }

  cat(glue("
    If applied, confound regression removes one or more confounds from the fMRI data using voxelwise regession.
    Two kinds of confounds can be removed: those that are first filtered to match the fMRI data and those that
    are not filtered. Any continuous-valued regressor that was generated from the fMRI timeseries data or head motion
    parameters -- for example, DVARS, components from CompCor, the global signal, or cerebrospinal fluid -- should
    be filtered. Regressors that are discrete-valued -- usually 0/1 spike regressors -- should not be filtered
    (e.g., motion_outlier* regressors in the confounds.tsv file produced by fmriprep).

    Here, you can specify '*' to include all components matching that wildcard, such as 'a_comp_cor_*'. You can also
    specify a range of values using the syntax '[low-high]', such as 'a_comp_cor_[1-10]', which would include the
    first 10 of these regressors as confounds.
  ", .trim = FALSE))

  apply_step <- prompt_input("Apply confound regression?", type = "flag")
  if (apply_step && !cur_val) {
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_regression")
  } else if (!apply_step && cur_val) {
    scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "confound_regression"]
  }

  scfg$postprocess$confound_regression$columns <- prompt_input("Confounds that will be filtered: ", type = "character", split = "\\s+")
  scfg$postprocess$confound_regression$noproc_columns <- prompt_input("Confounds that will not be filtered: ", type = "character", split = "\\s+")
  scfg$postprocess$confound_regression$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}

#' Specify the confound calculation settings for postprocessing
#' @param scfg a study configuration object created by `setup_study`
#' @return a modified version of `scfg` with the `$postprocess$confound_calculate` field populated
#' @keywords internal
setup_confound_calculate <- function(scfg = list()) {
  cur_val <- "confound_calculate" %in% scfg$postprocess$processing_steps
  if ("confound_calculate" %in% names(scfg)) {
    cat(strwrap(glue("
      Current confound calculation settings:
        Calculate confounds file: {cur_val}
        Columns that will be filtered to match fMRI data: {paste(scfg$postprocess$confound_calculate$columns)}
        Columns that will not be filtered: {paste(scfg$postprocess$confound_calculate$noproc_columns)}
        Demean confounds: {paste(scfg$postprocess$confound_calculate$demean)}
        Confound file name: {scfg$postprocess$confound_calculate$output_file}
    "), width = 80, extent = 4), sep = "\n")

    change <- prompt_input("Change settings?", type = "flag")
    if (!change) return(scfg) # skip out
  }
  
  cat(glue("
    Confound calculation creates a file containing a set of confound regressors, but it does *not* apply these
    to the fMRI data in any way. This file could be used subsequently -- often in the context of a task-based fMRI
    GLM analysis -- to remove nuisance regressors while also computing the effects for regressors of interest
    (usually, task-related modulation).

    Two kinds of confounds can be added to the confounds file: those that are first filtered to match the fMRI data 
    and those that are not filtered. Any continuous-valued regressor that was generated from the fMRI timeseries data
    or head motion parameters -- for example, DVARS, components from CompCor, the global signal, or cerebrospinal 
    fluid -- should be filtered. Regressors that are discrete-valued -- usually 0/1 spike regressors -- should not 
    be filtered (e.g., motion_outlier* regressors in the confounds.tsv file produced by fmriprep).

    Here, you can specify '*' to include all components matching that wildcard, such as 'a_comp_cor_*'. You can also
    specify a range of values using the syntax '[low-high]', such as 'a_comp_cor_[1-10]', which would include the
    first 10 of these regressors as confounds.
  ", .trim=FALSE))

  apply_step <- prompt_input("Calculate confounds?", type="flag")
  if (apply_step && !cur_val) {
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_calculate")
  } else if (!apply_step && cur_val) {
    scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "confound_calculate"]
  }

  scfg$postprocess$confound_calculate$columns <- prompt_input("Confounds that will be filtered: ", type = "character", split="\\s+")
  scfg$postprocess$confound_calculate$noproc_columns <- prompt_input("Confounds that will not be filtered: ", type = "character", split = "\\s+")
  scfg$postprocess$confound_calculate$demean <- prompt_input("Demean (filtered) regressors?", type = "flag")
  scfg$postprocess$confound_calculate$output_file <- prompt_input("Confound file name: ", type = "character")
  return(scfg)
}

#' Specify the intensity normalization settings for postprocessing
#' @param scfg a study configuration object created by `setup_study`
#' @return a modified version of `scfg` with the `$postprocess$intensity_normalize` field populated
#' @keywords internal
setup_intensity_normalization <- function(scfg = list()) {
  cur_val <- "intensity_normalize" %in% scfg$postprocess$processing_steps
  if ("intensity_normalize" %in% names(scfg)) {
    cat(glue("
      Current intensity normalization settings:
        Apply intensity normalization: {cur_val}
        Global (4D) median intensity: {scfg$postprocess$intensity_normalize$global_median}
        File prefix: {scfg$postprocess$intensity_normalize$prefix}
    "))

    change <- prompt_input("Change settings?", type="flag")
    if (!change) return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply intensity normalization?", type="flag")
  if (apply_step && !cur_val) {
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "intensity_normalize")
  } else if (!apply_step && cur_val) {
    scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "intensity_normalize"]
  }

  scfg$postprocess$intensity_normalize$global_median <- prompt_input("Global (4D) median intensity: ", type="numeric", lower=-1e8, upper=1e8)
  scfg$postprocess$intensity_normalize$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}

#' Specify the spatial smoothing settings for postprocessing
#' @param scfg a study configuration object created by `setup_study`
#' @return a modified version of `scfg` with the `$postprocess$spatial_smooth` field populated
#' @keywords internal
setup_spatial_smooth <- function(scfg = list()) {
  cur_val <- "spatial_smooth" %in% scfg$postprocess$processing_steps
  if ("spatial_smooth" %in% names(scfg)) {
    cat(glue("
      Current spatial smoothing settings:
        Apply spatial smoothing: {cur_val}
        Smoothing FWHM (mm): {scfg$postprocess$spatial_smooth$fwhm_mm}
        File prefix: {scfg$postprocess$spatial_smooth$prefix}
    "))

    change <- prompt_input("Change settings?", type = "flag")
    if (!change) return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply spatial smoothing?", type="flag")
  if (apply_step && !cur_val) {
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "spatial_smooth")
  } else if (!apply_step && cur_val) {
    scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "spatial_smooth"]
  }

  scfg$postprocess$spatial_smooth$fwhm_mm <- prompt_input("Spatial smoothing FWHM (mm): ", type="numeric", lower=0.1, upper=100)
  scfg$postprocess$spatial_smooth$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}

#' Specify the temporal filtering settings for postprocessing
#' @param scfg a study configuration object created by `setup_study`
#' @return a modified version of `scfg` with the `$postprocess$temporal_filter` field populated
#' @keywords internal
setup_temporal_filter <- function(scfg = list()) {
  cur_val <- "temporal_filter" %in% scfg$postprocess$processing_steps
  if ("temporal_filter" %in% names(scfg)) {
    cat(glue("
      Current temporal filtering settings:
        Apply temporal filter: pretty_arg({cur_val})
        Low-pass cutoff (Hz): {pretty_arg(scfg$postprocess$temporal_filter$low_pass_hz)}
        High-pass cutoff (Hz): {pretty_arg(scfg$postprocess$temporal_filter$high_pass_hz)}
        File prefix: {pretty_arg(scfg$postprocess$temporal_filter$prefix)}
    "))

    change <- prompt_input("Change settings?", type="flag")
    if (!change) return(scfg) # skip out
  }

  apply_step <- prompt_input("Apply temporal filter?", type="flag")
  if (apply_step && !cur_val) {
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "temporal_filter")
  } else if (!apply_step && cur_val) {
    scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "temporal_filter"]
  }

  scfg$postprocess$temporal_filter$low_pass_hz <- prompt_input("Low-pass cutoff (Hz): ", type="numeric", lower=0)
  scfg$postprocess$temporal_filter$high_pass_hz <- prompt_input("High-pass cutoff (Hz): ", type="numeric", lower=0)
  if (scfg$postprocess$temporal_filter$low_pass_hz > scfg$postprocess$temporal_filter$high_pass_hz) stop("Low-pass cutoff cannot be larger than high-pass cutoff")
  scfg$postprocess$temporal_filter$prefix <- prompt_input("File prefix: ", type = "character")
  return(scfg)
}
