#!/usr/bin/env Rscript
## simple script to handle post-fmriprep processing

#read in command line arguments.
args <- commandArgs(trailingOnly = FALSE)

scriptpath <- dirname(sub("--file=", "", grep("--file=", args, fixed=TRUE, value=TRUE), fixed=TRUE))
argpos <- grep("--args", args, fixed=TRUE)
if (length(argpos) > 0L) {
  args <- args[(argpos+1):length(args)]
} else {
  args <- c()
}

if (is.null(args) || length(args) < 2L) {
  message("Usage: post_fmriprep <input_file> <config_file.yaml> [<fsl_singularity_image>]")
  #printHelp()
  quit(save="no", 1, FALSE)
}

if (!suppressMessages(require("BrainGnomes", character.only=TRUE))) {
  stop("This script must be run in an R environment with BrainGnomes installed.")
}

#handle package dependencies
for (pkg in c("glue", "oro.nifti", "checkmate", "data.table", "yaml")) {
  if (!suppressMessages(require(pkg, character.only=TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only=TRUE))
  }
}

# Jun 2024: brain mask is required for calculating image quantiles for 2nd and 50th percentiles -- smoothing and intensity normalization
# Given that it is used only for these quantiles, the fmriprep mask should be fine for this purpose
# apply_mask is now considered an additional step that is optional and uses the brain_mask in the cfg

to_log <- function(str=NULL, log_file=NULL, stdout=TRUE) {
  checkmate::assert_string(str)
  checkmate::assert_string(log_file, null.ok = TRUE)
  if (is.null(str)) return(invisible(NULL))
  if (isTRUE(stdout)) cat(str, sep = "\n")
  if (!is.null(log_file)) cat(str, file = log_file, sep = "\n", append = TRUE)
  return(invisible(NULL))
}



# matrix must be time x units/regions
mat_to_nii <- function(mat, ni_out="mat") {
  require(oro.nifti)
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  # this always puts regressors along the x dimension; y and z are singletons
  ydim <- zdim <- 1 # size of y and z dimensions
  xsz <- ysz <- zsz <- 1 # voxel size in x y z
  tr <- 1
  xorigin <- yorigin <- zorigin <- 0

  run_fsl_command(glue("fslcreatehd {ncol(mat)} {ydim} {zdim} {nrow(mat)} {xsz} {ysz} {zsz} {tr} {xorigin} {yorigin} {zorigin} 64 {ni_out}"), singularity_img = fsl_img)

  ## read empty NIfTI into R
  nif <- readNIfTI(ni_out, reorient = FALSE)
  nif <- drop_img_dim(nif) # need to cleanup dim_ attribute to avoid writeNIfTI failure

  # populate nifti -- need to transpose to be consistent with column-wise array filling
  nif@.Data <- array(t(mat), dim = c(ncol(mat), 1, 1, nrow(mat))) # add singleton dimensions for y and z
  nif[is.na(nif)] <- 0 # cannot handle missingness in NIfTIs

  # write NIfTI with regressors back to file
  writeNIfTI(nif, filename = ni_out) # this returns the filename to the caller
}

nii_to_mat <- function(ni_in) {
  checkmate::assert_file_exists(ni_in)

  nii <- readNIfTI(ni_in, reorient = FALSE, rescale_data = FALSE)
  mat <- t(nii[, 1, 1, ]) # x and z -- make back into time x variables
  return(mat)
}

out_file_exists <- function(in_file, prefix, overwrite=TRUE) {
  # helper subfunction to enforce hyphen after initial postprocessing prefix
  p <- function(in_file, prefix) {
    has_prefix <- grepl("^\\w+-(sub|confounds).*", in_file, perl = TRUE)
    if (isTRUE(has_prefix)) {
      return(prefix)
    } else {
      return(paste0(prefix, "-")) # need to append hyphen
    }
  }

  in_dir <- dirname(in_file)
  in_file <- basename(in_file)

  # handle extant file
  out_file <- glue("{in_dir}/{p(in_file, prefix)}{in_file}")
  skip <- FALSE
  if (checkmate::test_file_exists(out_file)) {
    if (isFALSE(overwrite)) {
      message(glue("Processed image already exists: {out_file}. Skipping this step."))
      skip <- TRUE
    } else {
      message(glue("Overwriting image: {out_file}."))
    }
  }
  return(list(out_file=out_file, skip=skip))
}




# function to create a loose brain mask using the BET + 98/2 + dilate approach of FSL
compute_brain_mask <- function(in_file, log_file = NULL) {
  # use the 98 - 2 method from FSL (featlib.tcl ca. line 5345)

  to_log("# Computing brain mask from fMRI data using FSL's 98-2 percentile method", log_file=log_file)

  # first use FSL bet on the mean functional to get a starting point
  tmean_file <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {tmean_file}"), log_file = log_file, singularity_img = fsl_img)
  
  temp_bet <- tempfile()
  run_fsl_command(glue("bet {tmean_file} {temp_bet} -R -f 0.3 -m -n"), log_file = log_file, singularity_img = fsl_img)

  temp_stripped <- tempfile(pattern="epi_bet")
  run_fsl_command(glue("fslmaths {in_file} -mas {temp_bet}_mask {temp_stripped}"), log_file = log_file, singularity_img = fsl_img)

  # now compute 2nd and 98th percentiles on skull-stripped image
  p2 <- get_image_quantile(temp_stripped, quantile=2, exclude_zero = FALSE, log_file = log_file)
  p98 <- get_image_quantile(temp_stripped, quantile=98, exclude_zero = FALSE, log_file = log_file)
  
  thresh <- p2 + (p98 - p2)/10

  # apply this threshold to the epi_bet image, then take Tmin and binarize to form mask
  temp_mask <- tempfile(pattern = "mask_98_2")
  run_fsl_command(glue("fslmaths {temp_stripped} -thr {thresh} -Tmin -bin {temp_mask}"), log_file=log_file, singularity_img = fsl_img)

  # create dil1x copy as well if this is used elsewhere
  run_fsl_command(glue("fslmaths {temp_mask} -dilF {temp_mask}_dil1x"), log_file = log_file, singularity_img = fsl_img)

  # cleanup temp files
  rm_niftis(c(tmean_file, temp_bet, temp_stripped))
  
  return(temp_mask)
}



get_image_quantile <- function(in_file, brain_mask=NULL, quantile=50, exclude_zero=FALSE, log_file=NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_number(quantile, lower = 0, upper = 100)
  pstr <- ifelse(isTRUE(exclude_zero), "-P", "-p")
  if (is.null(brain_mask)) {
     quantile_value <- as.numeric(run_fsl_command(glue("fslstats {in_file} {pstr} {quantile}"), intern = TRUE, log_file = log_file, singularity_img = fsl_img))
  } else {
    if (!checkmate::test_file_exists(brain_mask)) checkmate::assert_file_exists(paste0(brain_mask, ".nii.gz"))
    quantile_value <- as.numeric(run_fsl_command(glue("fslstats {in_file} -k {brain_mask} {pstr} {quantile}"), intern = TRUE, log_file = log_file, singularity_img = fsl_img))
  }
  return(quantile_value)
}




apply_mask <- function(in_file, mask_file, prefix="m", overwrite=FALSE, log_file=NULL) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_string(prefix)

  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  run_fsl_command(glue("fslmaths {in_file} -mas {mask_file} {out_file} -odt float"), log_file = log_file, singularity_img = fsl_img)
  return(out_file)
}

get_fmriprep_outputs <- function(in_file) {
  if (grepl("_space-", in_file)) {
    space_chars <- sub(".*sub-\\d+_task-[^_]+_run-\\d+(.*)_desc-preproc_bold.*", "\\1", in_file)
  } else {
    space_chars <- ""
  }
  first_chars <- sub("(sub-\\d+_task-[^_]+_run-\\d+).*", "\\1", in_file, perl=TRUE)
  bold <- Sys.glob(glue("{first_chars}{space_chars}*preproc_bold*nii*"))
  brain_mask <- Sys.glob(glue("{first_chars}{space_chars}*_desc-brain_mask*nii*"))
  confounds <- glue("{first_chars}_desc-confounds_timeseries.tsv")
  if (!checkmate::test_file_exists(confounds)) confounds <- glue("{first_chars}_desc-confounds_regressors.tsv")

  melodic_mix <- glue("{first_chars}_desc-MELODIC_mixing.tsv")
  noise_ics <- glue("{first_chars}_AROMAnoiseICs.csv")
  ret_list <- list(bold = bold, brain_mask = brain_mask, confounds = confounds, melodic_mix = melodic_mix, noise_ics = noise_ics)
  have_files <- sapply(ret_list, checkmate::test_file_exists)
  ret_list[!have_files] <- NULL  # NULL out missing files
  
  ret_list[["prefix"]] <- first_chars # sub id info
  return(ret_list)
}

# helper function to populate defaults for config
populate_defaults <- function(target = NULL, defaults) {
  if (is.null(target)) target <- list()
  
  miss_fields <- setdiff(names(defaults), names(target))
  if (length(miss_fields) > 0L) {
    for (mm in miss_fields) {
      target[[mm]] <- defaults[[mm]]
    }
  }

  return(target)
}

# default configuration settings
default_cfg <- list(
  tr = NULL, 
  force_processing_order = FALSE, 
  log_file = "{proc_files$prefix}_post_fmriprep.log",
  brain_mask = "{proc_files$brain_mask}",
  overwrite = FALSE, keep_intermediates = FALSE, 
  processing_steps = c(
    "apply_mask", "spatial_smooth", "apply_aroma", 
    "temporal_filter", "intensity_normalize", "confound_calculate"
  ),
  apply_mask = list(prefix = "m"),
  spatial_smooth = list(prefix = "s", fwhm_mm = 6),
  apply_aroma = list(prefix = "a", aggressive = FALSE),
  temporal_filter = list(prefix = "f", low_pass_hz = 0, high_pass_hz = 0.008333333),
  intensity_normalize = list(prefix = "n", global_median = 10000),
  confound_regression = list(
    prefix = "r", columns = c("csf", "csf_derivative1", "white_matter", "white_matter_derivative1"), 
    noproc_columns = list(),
    output_file = "{proc_files$prefix}_confound_regressors.txt"
  ),
  confound_calculate = list(
    columns = c("csf", "csf_derivative1", "white_matter", "white_matter_derivative1"),
    noproc_columns = "framewise_displacement", 
    output_file = "{proc_files$prefix}_postprocessed_confounds.txt",
    demean = FALSE
  )  
)


input_file <- args[1L]
config_file <- args[2L]
fsl_img <- if (length(args) == 3L) args[3L] else NULL

if (!checkmate::test_file_exists(input_file)) {
  stop("Cannot find input_file: ", args[1])
}

if (!checkmate::test_file_exists(config_file)) {
  stop("Cannot find config_file: ", args[2])
}

result <- postprocess_subject(input_file, config_file)
cat("Processing completed. Output file:", result, "\n")

# for testing
# sdir <- "/proj/mnhallqlab/studies/bsocial/clpipe/data_fmriprep/fmriprep/sub-221256/func"
# setwd(sdir)
# postprocess_subject("sub-221256_task-clock_run-2_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
#   cfg = "/proj/mnhallqlab/users/michael/fmri.pipeline/R/post_fmriprep.yaml"
# )

# for testing
# sdir <- "~/Downloads/func"
# setwd(sdir)
# postprocess_subject("sub-221256_task-clock_run-2_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
#  cfg = "~/fmri_processing_scripts/post_fmriprep.yaml"
# )

