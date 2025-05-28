#!/usr/bin/env Rscript
## simple script to handle post-fmriprep processing

#read in command line arguments.
args <- commandArgs(trailingOnly = FALSE)

scriptpath <- dirname(sub("--file=", "", grep("--file=", args, fixed=TRUE, value=TRUE), fixed=TRUE))
argpos <- grep("--args", args, fixed=TRUE)
if (length(argpos) > 0L) {
  args <- args[(argpos + 1):length(args)]
} else {
  args <- c()
}

if (is.null(args) || length(args) < 2L) {
  message("Minimal usage: post_fmriprep.R --in=<input_file> --<config_yaml.yaml> [<fsl_singularity_image>]")
  #printHelp()
  quit(save="no", 1, FALSE)
}

if (!suppressMessages(require("BGprocess", character.only=TRUE))) {
  stop("This script must be run in an R environment with BrainGnomes installed.")
}

# handle package dependencies
for (pkg in c("glue", "oro.nifti", "checkmate", "data.table", "yaml")) {
  if (!suppressMessages(require(pkg, character.only = TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only = TRUE))
  }
}

# parse CLI inputs into a nested list, if relevant
cli_args <- parse_cli_args(args)

if (!is.null(cli_args$config_yaml)) {
  checkmate::assert_file_exists(cli_args$config_yaml)
  cfg <- yaml::read_yaml(cli_args$config_yaml)
  cli_args$confg_yaml <- NULL # remove prior to additional updates
} else {
  cfg <- list()
}

# Now add additional command line arguments to cfg -- this leads any settings in YAML to be overridden by the same CLI arguments
cfg <- set_nested_values(cli_args, lst=cfg)

input_file <- cfg$in
if (!checkmate::test_file_exists(input_file)) {
  

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

result <- postprocess_subject(input_file, cfg)
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

