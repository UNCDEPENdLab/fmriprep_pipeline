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

rm_niftis <- function(files=NULL) {
  if (is.null(files)) return(invisible(NULL))
  checkmate::assert_character(files)
  for (ff in files) {
    tnif <- ifelse(grepl(".*\\.nii(\\.gz)?$", ff), ff, paste0(ff, ".nii.gz")) # add suffix if none provided
    if (checkmate::test_file_exists(tnif)) unlink(tnif)
  }
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

run_fsl_command <- function(args, fsldir=NULL, echo=TRUE, run=TRUE, log_file=NULL, intern=FALSE, stop_on_fail=TRUE, singularity_img=NULL) {
  
  if (!is.null(singularity_img)) {
    # if we are using a singularity container, always look inside the container for FSLDIR
    checkmate::assert_file_exists(singularity_img, access = "r")
    fsldir <- system(glue("singularity exec {singularity_img} printenv FSLDIR"), intern = TRUE)
    if (length(fsldir) == 0L) stop("Cannot find FSLDIR inside singularity container")
  } else if (is.null(fsldir)) {
    # look for FSLDIR in system environment if not passed in    
    fsldir <- Sys.getenv("FSLDIR")
    if (isFALSE(nzchar(fsldir))) {
      # check for FSLDIR in .bashrc or .profile
      bashrc_fsldir <- ""
      if (file.exists("~/.profile")) {
        bashrc_fsldir <- system("source ~/.profile && echo $FSLDIR", intern = TRUE)
      }

      if (nzchar(bashrc_fsldir) && file.exists("~/.bashrc")) {
        bashrc_fsldir <- system("source ~/.bashrc && echo $FSLDIR", intern = TRUE)
      }

      # Fallback: look for location of fsl feat on PATH
      if (nzchar(bashrc_fsldir)) {
        feat_loc <- system("command -v feat", intern = TRUE)
        exit_code <- attr(feat_loc, "status")
        if (!is.null(exit_code) && exit_code == 1) {
          warning("Could not find FSL using FSLDIR or system PATH. Defaulting to Defaulting to /usr/local/fsl.")
          fsldir <- "/usr/local/fsl"
        } else {
          fsldir <- dirname(dirname(feat_loc))
        }
      }
    }
  }

  Sys.setenv(FSLDIR=fsldir) #export to R environment
  fslsetup <- paste0("FSLDIR=", fsldir, "; PATH=${FSLDIR}/bin:${PATH}; . ${FSLDIR}/etc/fslconf/fsl.sh; ${FSLDIR}/bin/")

  # Command to run (basic or singularity-wrapped)
  base_cmd <- paste0(fslsetup, " ", args)

  if (!is.null(singularity_img)) {
    # Get absolute working directory to mount
    workdir <- normalizePath(getwd())
    singularity_cmd <- paste(
      "singularity exec",
      paste0("--bind ", workdir, ":", workdir),
      singularity_img,
      "bash -c",
      shQuote(base_cmd)
    )
    full_cmd <- singularity_cmd
  } else {
    full_cmd <- base_cmd
  }

  ofile <- tempfile(pattern="stdout")
  efile <- tempfile(pattern = "stderr")
  full_cmd <- paste(full_cmd, ">", shQuote(ofile), "2>", shQuote(efile))

  #cat("FSL command: ", full_cmd, "\n")
  if (!is.null(log_file)) cat(args, file=log_file, append=TRUE, sep="\n")
  if (isTRUE(echo)) cat(args, "\n")
  
  retcode <- if (isTRUE(run)) system(full_cmd) else 0 # return 0 if not run

  if (file.exists(efile)) {
    stderr <- readLines(efile)
    if (identical(character(0), stderr)) stderr <- ""
  } else {
    stderr <- ""
  }

  if (file.exists(ofile)) {
    stdout <- readLines(ofile)
    if (identical(character(0), stdout)) stdout <- ""
  } else {
    stdout <- ""
  }

  to_return <- retcode # return exit code of command
  # if specified, switch to stdout as return
  if (isTRUE(intern)) {
    to_return <- stdout # return output of command
    attr(to_return, "retcode") <- retcode
  }

  attr(to_return, "stdout") <- stdout
  attr(to_return, "stderr") <- stderr

  if (retcode != 0) {    
    errmsg <- glue("run_fsl_command failed with exit code: {retcode}, stdout: {paste(stdout, collapse='\n')}, stderr: {paste(stderr, collapse='\n')}")
    cat(errmsg, "\n", file = log_file, append = TRUE)
    if (isTRUE(stop_on_fail)) stop(errmsg)
  }

  return(to_return)
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

temporal_filter <- function(in_file, prefix="f", low_pass_hz=0, high_pass_hz=1/120, tr=NULL, overwrite=FALSE, log_file=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(low_pass_hz)
  checkmate::assert_number(high_pass_hz)
  stopifnot(low_pass_hz < high_pass_hz)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else  {
    out_file <- res$out_file
  }

  # bptf specifies its filter cutoffs in terms of volumes, not frequencies
  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html

  if (is.infinite(high_pass_hz)) {
    #message("Low-pass filtering")
    hp_volumes <- -1 # do not apply high-pass
  } else {
    hp_volumes <- 1 / (high_pass_hz * fwhm_to_sigma * tr)
  }

  if (is.infinite(low_pass_hz) || low_pass_hz==0) {
    #message("High-pass filtering")
    lp_volumes <- -1 # do not apply low-pass
  } else {
    lp_volumes <- 1 / (low_pass_hz * fwhm_to_sigma * tr)
  }

  temp_tmean <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file=log_file, singularity_img = fsl_img)
  run_fsl_command(glue("fslmaths {in_file} -bptf {hp_volumes} {lp_volumes} -add {temp_tmean} {out_file}"), log_file = log_file, singularity_img = fsl_img)
  
  rm_niftis(temp_tmean) # clean up temporal mean image
  
  return(out_file)
}

apply_aroma <- function(in_file, prefix = "a", mixing_file, noise_file, overwrite = FALSE, log_file = NULL, use_R = FALSE) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  if (isFALSE(checkmate::test_file_exists(mixing_file))) {
    warning(glue("Cannot find mixing file corresponding to {in_file}. Skipping AROMA regression"))
    return(in_file)
  }

  if (isFALSE(checkmate::test_file_exists(noise_file))) {
    warning(glue("Cannot find ICA noise components file corresponding to {in_file}. Skipping AROMA regression"))
    return(in_file)
  }

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  # just read in the comma-separated noise ICs
  noise_ics <- readLines(noise_file, warn = FALSE)

  # for some reason, fsl_regfilt blows up when we try to feed a regressors x 1 x 1 x timepoints NIfTI
  # fall back to R in this case
  if (isTRUE(use_R)) {
    cmd <- glue("fsl_regfilt.R {in_file} {mixing_file} {noise_file} 1 {out_file}")
    to_log(cmd, log_file=log_file)
    system(cmd)
  } else {
    cmd <- glue("fsl_regfilt -i {in_file} -o {out_file} -d {mixing_file} -f {noise_ics}")
    run_fsl_command(cmd, log_file = log_file, singularity_img = fsl_img)
  }
  return(out_file)
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

spatial_smooth <- function(in_file, prefix="s", fwhm_mm=6, brain_mask=NULL, overwrite=FALSE, log_file=NULL) {
  #checkmate::assert_file_exists(in_file)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html
  sigma <- fwhm_mm / fwhm_to_sigma

  p2_intensity <- get_image_quantile(in_file, brain_mask, 2, log_file=log_file)
  median_intensity <- get_image_quantile(in_file, brain_mask, 50, log_file = log_file)
  susan_thresh <- (median_intensity - p2_intensity) * .75  # also see featlib.tcl

  # always compute extents mask that is reapplied to data post-smoothing to avoid any "new" voxels
  extents_mask <- tempfile(pattern="extents_mask")
  run_fsl_command(glue("fslmaths {in_file} -Tmin -bin {extents_mask} -odt char"), log_file = log_file, singularity_img = fsl_img) # save extents to temp file
  
  # compute mean functional image used in susan
  temp_tmean <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file=log_file, singularity_img = fsl_img) # save tmean to temporary file
  run_fsl_command(glue("susan {in_file} {susan_thresh} {sigma} 3 1 1 {temp_tmean} {susan_thresh} {out_file}"), log_file = log_file, singularity_img = fsl_img)

  # apply extents mask
  run_fsl_command(glue("fslmaths {out_file} -mul {extents_mask} {out_file} -odt float"), log_file = log_file, singularity_img = fsl_img)
  
  rm_niftis(c(temp_tmean, extents_mask, glue("{out_file}_usan_size"))) # cleanup temp files
  
  return(out_file)
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

intensity_normalize <- function(in_file, prefix="n", brain_mask=NULL, global_median=10000, overwrite=FALSE, log_file=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(global_median)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  median_intensity <- get_image_quantile(in_file, brain_mask, 50, log_file=log_file)
  rescaling_factor <- global_median / median_intensity

  run_fsl_command(glue("fslmaths {in_file} -mul {rescaling_factor} {out_file} -odt float"), log_file=log_file, singularity_img = fsl_img)
  return(out_file)
}

confound_regression <- function(in_file, to_regress=NULL, prefix="r", overwrite=FALSE, log_file=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(to_regress)
  checkmate::assert_string(prefix)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  # convert text file to FSL vest file for fsl_glm to accept it
  vest_file <- tempfile(pattern = "regressors", fileext = ".mat")
  run_fsl_command(glue("Text2Vest {to_regress} {vest_file}"), log_file = log_file, singularity_img = fsl_img)
  
  # because the residuals will be demeaned and intensity normalization should follow this step, add back in the temporal mean from the pre-regression image
  temp_tmean <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file=log_file, singularity_img = fsl_img)
  run_fsl_command(glue("fsl_glm -i {in_file} -d {vest_file} --out_res={out_file}"), log_file = log_file, singularity_img = fsl_img)
  run_fsl_command(glue("fslmaths {out_file} -add {temp_tmean} {out_file}"), log_file=log_file, singularity_img = fsl_img)

  # 3dTproject for regression (deprecated to keep all commands in FSL)
  # regress_cmd <- glue("3dTproject -input {in_file} -prefix {out_file}_afni -ort {to_regress} -polort 0")

  rm_niftis(temp_tmean)
  return(out_file)
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

### primary function to process a given fmriprep subject dataset
process_subject <- function(in_file, cfg="post_fmriprep.yaml") {
  checkmate::assert_file_exists(in_file)  
  
  # checkmate::assert_list(processing_sequence)
  proc_files <- get_fmriprep_outputs(in_file)

  sdir <- dirname(in_file)
  setwd(sdir)

  if (is.list(cfg)) {
    # for now, nothing here -- just use list as-is
  } else if (checkmate::test_string(cfg)) {
    checkmate::assert_file_exists(cfg)
    cfg <- yaml::read_yaml(cfg)
  }

  # add any defaults if user's config is incomplete
  cfg <- populate_defaults(cfg, default_cfg)

  if (!checkmate::test_number(cfg$tr, lower = 0.01, upper = 30)) {
    stop("YAML config must contain a tr field specifying the repetition time in seconds")
  }

  # default to not enforcing user-specified order of processing steps
  if (!checkmate::test_flag(cfg$force_processing_order)) cfg$force_processing_order <- FALSE

  log_file <- glue(cfg$log_file) #evaluate location of log
  complete_file <- paste0(".", basename(proc_files$prefix), "_complete") # evaluate location of log

  if (isFALSE(cfg$overwrite) && file.exists(complete_file)) {
    message(glue("Already completed postprocessing for {in_file}. Skipping"))
    return(NULL)
  }

  start_time <- Sys.time()
  to_log(paste0("# Start fmriprep postprocessing: ", as.character(start_time)), log_file = log_file)
  
  # determine brain mask to be used for computing intensity thresholds for susan and normalization
  if (is.null(cfg$brain_mask)) {
    # if no brain mask is provided, use fmriprep brain mask
    if (!is.null(proc_files$brain_mask)) {
      brain_mask <- proc_files$brain_mask
    } else {
      # if fmriprep mask is not available, compute mask
      brain_mask <- compute_brain_mask(in_file, log_file)
    }
  } else {
    # use user-specified brain mask
    brain_mask <- glue(cfg$brain_mask)
  }
  
  cur_file <- proc_files$bold
  file_set <- cur_file

  ## setup order of processing steps
  checkmate::assert_character(cfg$processing_steps) # ensure we have a character vector
  cfg$processing_steps <- tolower(cfg$processing_steps) # avoid case issues

  # handle small glitches in nomenclature
  cfg$processing_steps <- sub("spatial_smoothing", "spatial_smooth", cfg$processing_steps, fixed=TRUE)
  cfg$processing_steps <- sub("temporal_filtering", "temporal_filter", cfg$processing_steps, fixed=TRUE)
  cfg$processing_steps <- sub("confound_regress", "confound_regression", cfg$processing_steps, fixed = TRUE)
  cfg$processing_steps <- sub("intensity_normalization", "intensity_normalize", cfg$processing_steps, fixed = TRUE)

  if (isTRUE(cfg$force_processing_order)) {
    processing_sequence <- cfg$processing_steps
    to_log("# We will follow the user-specified processing order, with no guarantees about whether this may yield data problems.", log_file = log_file)
  } else {
    processing_sequence <- c()
    if ("apply_mask" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "apply_mask")
    if ("spatial_smooth" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "spatial_smooth")
    if ("apply_aroma" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "apply_aroma")
    if ("temporal_filter" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "temporal_filter")
    if ("confound_regression" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "confound_regression")
    if ("intensity_normalize" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "intensity_normalize")
  }

  to_log(glue("# Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}"), log_file = log_file)
  
  #### handle confounds, filtering to match MRI data
  if ("confound_regression" %in% cfg$processing_steps || "confound_calculate" %in% cfg$processing_steps) {
    confounds <- data.table::fread(proc_files$confounds, na.strings = c("n/a", "NA", "."))
    confound_cols <- as.character(union(cfg$confound_regression$columns, cfg$confound_calculate$columns))
    noproc_cols <- as.character(union(cfg$confound_regression$noproc_columns, cfg$confound_calculate$noproc_columns)) # no AROMA or filter
    if (any(noproc_cols %in% confound_cols)) {
      stop("Cannot handle overlaps in noproc_columns and columns for confounds")
    }
    
    confounds_to_filt <- subset(confounds, select = confound_cols)
    confound_nii <- mat_to_nii(confounds_to_filt, ni_out = tempfile(pattern = "confounds"))

    # apply AROMA denoising to confounds if AROMA is applied to MRI data
    if ("apply_aroma" %in% cfg$processing_steps) {
      to_log("# Removing AROMA noise components from confounds", log_file=log_file)
      confound_nii <- apply_aroma(confound_nii,
        mixing_file = proc_files$melodic_mix,
        noise_file = proc_files$noise_ics, overwrite=cfg$overwrite, log_file=log_file, use_R=TRUE
      )
    }

    # apply temporal filter to confounds if temporal filter is applied to MRI data
    if ("temporal_filter" %in% cfg$processing_steps) {
      to_log("# Temporally filtering confounds", log_file=log_file)
      confound_nii <- temporal_filter(confound_nii,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz, overwrite=cfg$overwrite, log_file=log_file
      )
    }

    # read in processed confounds and convert back to time x signals data.frame
    filtered_confounds <- data.frame(nii_to_mat(confound_nii))
    filtered_confounds <- setNames(filtered_confounds, confound_cols)
    
    if ("confound_calculate" %in% cfg$processing_steps) {
      df <- subset(filtered_confounds, select = cfg$confound_calculate$columns)
      if (!is.null(cfg$confound_calculate$noproc_columns) && length(cfg$confound_calculate$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select = cfg$confound_calculate$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }

      if (isTRUE(cfg$confound_calculate$demean)) {
        df[, cfg$confound_calculate$columns] <- lapply(df[, cfg$confound_calculate$columns], function(x) x - mean(x, na.rm = TRUE))
      }
      confile <- glue(cfg$confound_calculate$output_file)
      to_log(glue("# Writing filtered confounds to: {confile}"), log_file = log_file)
      to_log(glue("# Columns are: {paste(names(df), collapse=', ')}"), log_file = log_file)
      data.table::fwrite(df, file = confile, sep = "\t", col.names = FALSE)
    }

    if ("confound_regression" %in% cfg$processing_steps) {
      df <- subset(filtered_confounds, select = cfg$confound_regression$columns)

      # mean center columns
      df <- as.data.frame(lapply(df, function(cc) cc - mean(cc)))
      
      if (!is.null(cfg$confound_regression$noproc_columns) && length(cfg$confound_regression$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select=cfg$confound_regression$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }
      to_regress <- glue(cfg$confound_regression$output_file)
      
      const_cols <- sapply(df, function(x) all(x == x[1L]))
      if (any(const_cols)) df <- df[, !const_cols] # remove any constant columns
      df <- cbind(1, df) # add intercept

      data.table::fwrite(df, file = to_regress, sep = "\t", col.names = FALSE)
    }
  }
  
  #### loop over fMRI processing steps in sequence
  proc_prefix <- "-"

  for (step in processing_sequence) {
    if (step == "spatial_smooth") {
      proc_prefix <- paste0(cfg$spatial_smooth$prefix, proc_prefix)
      to_log(glue("# Spatial smoothing with FHWM {cfg$spatial_smooth$fwhm_mm}mm kernel"), log_file = log_file)
      cur_file <- spatial_smooth(cur_file,
        brain_mask = brain_mask, prefix = cfg$spatial_smooth$prefix,
        fwhm_mm = cfg$spatial_smooth$fwhm_mm, overwrite = cfg$overwrite, log_file = log_file
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "apply_aroma") {
      proc_prefix <- paste0(cfg$apply_aroma$prefix, proc_prefix)
      to_log("# Removing AROMA noise components from fMRI data", log_file=log_file)
      cur_file <- apply_aroma(cur_file, prefix = cfg$apply_aroma$prefix,
        mixing_file = proc_files$melodic_mix,
        noise_file = proc_files$noise_ics,
        overwrite=cfg$overwrite, log_file=log_file
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "temporal_filter") {
      proc_prefix <- paste0(cfg$temporal_filter$prefix, proc_prefix)
      to_log(glue("# Temporal filtering with lp: {cfg$temporal_filter$low_pass_hz}Hz, hp: {cfg$temporal_filter$high_pass_hz}Hz given TR: {cfg$tr}"), log_file = log_file)
      cur_file <- temporal_filter(cur_file, prefix = cfg$temporal_filter$prefix,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite=cfg$overwrite, log_file=log_file
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "intensity_normalize") {
      proc_prefix <- paste0(cfg$intensity_normalize$prefix, proc_prefix)
      to_log(glue("# Intensity normalizing fMRI data to global median: {cfg$intensity_normalize$global_median}"), log_file = log_file)
      cur_file <- intensity_normalize(cur_file, prefix = cfg$intensity_normalize$prefix,
        brain_mask = brain_mask,
        global_median = cfg$intensity_normalize$global_median,
        overwrite=cfg$overwrite, log_file=log_file
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "confound_regression") {
      proc_prefix <- paste0(cfg$confound_regression$prefix, proc_prefix)
      to_log(glue("# Removing confound regressors from fMRI data using file: {to_regress}"), log_file = log_file)
      cur_file <- confound_regression(cur_file, prefix = cfg$confound_regression$prefix,
        to_regress = to_regress,
        overwrite=cfg$overwrite, log_file = log_file
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "apply_mask") {
      proc_prefix <- paste0(cfg$apply_mask$prefix, proc_prefix)
      to_log(glue("# Masking fMRI data using file: {brain_mask}"), log_file = log_file)
      cur_file <- apply_mask(cur_file, prefix = cfg$apply_mask$prefix,
        mask_file = brain_mask,
        overwrite=cfg$overwrite, log_file = log_file
      )
      file_set <- c(file_set, cur_file)
    }
  }

  if (isFALSE(cfg$keep_intermediates) && length(file_set) > 2L) {
    # initial file is the BOLD input from fmriprep, last file is the final processed image
    to_delete <- file_set[2:(length(file_set) - 1)]
    for (ff in to_delete) {
      cat("# Removing", ff, "\n", file = log_file, append=TRUE)
      if (file.exists(ff)) unlink(ff)
    }
  }

  # move the final file into a BIDS-friendly file name with a desc field
  if (!is.null(cfg$bids_desc) && is.character(cfg$bids_desc)) {
    final_filename <- sub(proc_prefix, "", cur_file) # strip prefix
    if (grepl("_desc-", final_filename)) final_filename <- sub("_desc-[^_\\.]+", paste0("_desc-", cfg$bids_desc), final_filename)
    else final_filename <- sub("((_bold)?\\.nii(\\.gz)?)", paste0("_desc-", cfg$bids_desc, "\\1"), final_filename)
    file.rename(cur_file, final_filename)
  }

  end_time <- Sys.time()
  to_log(glue("# End fmriprep postprocessing: {as.character(end_time)}"), log_file = log_file)
  cat(as.character(start_time), as.character(end_time), file = complete_file, sep = "\n")
  return(cur_file)
}


input_file <- args[1L]
config_file <- args[2L]
fsl_img <- if (length(args) == 3L) args[3L] else NULL

if (!checkmate::test_file_exists(input_file)) {
  stop("Cannot find input_file: ", args[1])
}

if (!checkmate::test_file_exists(config_file)) {
  stop("Cannot find config_file: ", args[2])
}

result <- process_subject(input_file, config_file)
cat("Processing completed. Output file:", result, "\n")

# for testing
# sdir <- "/proj/mnhallqlab/studies/bsocial/clpipe/data_fmriprep/fmriprep/sub-221256/func"
# setwd(sdir)
# process_subject("sub-221256_task-clock_run-2_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
#   cfg = "/proj/mnhallqlab/users/michael/fmri.pipeline/R/post_fmriprep.yaml"
# )

# for testing
# sdir <- "~/Downloads/func"
# setwd(sdir)
# process_subject("sub-221256_task-clock_run-2_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
#  cfg = "~/fmri_processing_scripts/post_fmriprep.yaml"
# )

