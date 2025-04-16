#!/usr/bin/env Rscript

# --- Initialization ---
args <- commandArgs(trailingOnly = TRUE)

print_help <- function() {
  cat("
-----------------------------------
preprocess_study.R does an incremental pull of raw MRI data (DICOMS) from a remote server,
restructures these into BIDS format using heudiconv, then queues fmriprep, mriqc, and fidelity checks.

Usage:
  Rscript preprocess_study.R study_cfg/neuromap.cfg
  Rscript preprocess_study.R study_cfg/neuromap.cfg alternate_compute_environment.cfg
-----------------------------------
")
}

if (length(args) == 0 || args[1] %in% c("-help", "--help")) {
  print_help()
  quit(status = 1)
}

library(glue)
library(stringr)

# --- Setup Paths and Source Configs ---
pipedir <- dirname(normalizePath(sys.frame(1)$ofile))
source(file.path(pipedir, "pipeline_functions.R"))

env_file <- if (length(args) > 1) args[2] else file.path(pipedir, "compute_environment.cfg")
if (!file.exists(env_file)) stop(glue("Cannot access config file {env_file}"))
source(env_file)

study_cfg_file <- args[1]
if (!file.exists(study_cfg_file)) stop(glue("Cannot access config file {study_cfg_file}"))
source(study_cfg_file)

# --- Set Defaults ---
sync_raw_data <- as.integer(Sys.getenv("sync_raw_data", unset = 1))
run_fmriprep <- as.integer(Sys.getenv("run_fmriprep", unset = 1))
run_mriqc <- as.integer(Sys.getenv("run_mriqc", unset = 1))
debug_pipeline <- as.integer(Sys.getenv("debug_pipeline", unset = 0))

if (debug_pipeline == 1) {
  heudiconv_walltime <- "00:01:00"
  mriqc_walltime <- "00:01:00"
  fmriprep_walltime <- "00:01:00"
}
rel_suffix <- if (debug_pipeline == 2) "c" else "o"

log_file <- if (!nzchar(Sys.getenv("log_file"))) {
  base <- tools::file_path_sans_ext(basename(study_cfg_file))
  file.path(pipedir, paste0(base, "_log.txt"))
} else {
  Sys.getenv("log_file")
}
Sys.setenv(log_file = log_file)

# --- Pull Raw Data if requested ---
if (sync_raw_data == 1) {
  rel(file.path(pipedir, "syncMRCTR_MRRaw"), rel_suffix)
}

# --- Loop Over Subjects ---
subdirs <- list.dirs(loc_mrraw_root, recursive = FALSE, full.names = TRUE)
subdirs <- subdirs[grepl(subid_regex, basename(subdirs))]
allJobIds <- ""
writeLines("", con = expectation_file)

for (sdir in subdirs) {
  rel(glue("Processing subject directory: {sdir}"), "c")
  sub <- basename(sdir)
  
  job_ids <- list()

  # Heudiconv
  if (!file.exists(file.path(loc_bids_root, paste0("sub-", sub), ".heudiconv.complete"))) {
    heudiconvID <- rel(glue("qsub {build_qsub_string(walltime = heudiconv_walltime)} -v {envpass(
      'rel_suffix', 'sub', 'loc_bids_root', 'loc_mrraw_root',
      'log_file', 'pipedir', 'heudiconv_location', 'heudiconv_heuristic', 'debug_pipeline'
    )} {file.path(pipedir, 'qsub_heudiconv_subject.sh')}"), rel_suffix, paste0("heudiconvID-", sub))
    job_ids$heudiconvID <- heudiconvID
  }

  # MRIQC
  if (run_mriqc == 1 && !file.exists(file.path(loc_bids_root, paste0("sub-", sub), ".mriqc.complete"))) {
    mriqcID <- rel(glue("qsub {build_depend_string('afterok', job_ids$heudiconvID)} {build_qsub_string(
      nodes = glue('1:ppn={mriqc_nthreads}'), walltime = mriqc_walltime
    )} -v {envpass('rel_suffix', 'loc_bids_root', 'debug_pipeline', 'sub', 'loc_root', 'log_file', 'pipedir')} {file.path(pipedir, 'qsub_mriqc_subject.sh')}"),
    rel_suffix, paste0("mriqcID-", sub))
    job_ids$mriqcID <- mriqcID
  }

  # FMRIPREP
  if (run_fmriprep == 1 && !file.exists(file.path(loc_bids_root, paste0("sub-", sub), ".fmriprep.complete"))) {
    fmriprepID <- rel(glue("qsub {build_depend_string('afterok', job_ids$heudiconvID)} {build_qsub_string(
      nodes = glue('1:ppn={fmriprep_nthreads}'), walltime = fmriprep_walltime
    )} -v {envpass('rel_suffix', 'debug_pipeline', 'sub', 'loc_root', 'loc_bids_root', 'loc_mrproc_root', 'fmriprep_nthreads', 'log_file', 'pipedir')} {file.path(pipedir, 'qsub_fmriprep_subject.sh')}"),
    rel_suffix, paste0("fmriprepID-", sub))
    job_ids$fmriprepID <- fmriprepID
  }

  # Fidelity Checks
  if (run_fidelity_checks == 1 && !file.exists(file.path(loc_bids_root, paste0("sub-", sub), ".fidelity.complete"))) {
    fidelityID <- rel(glue("qsub {build_depend_string('afterok', job_ids$heudiconvID, 'afterany', job_ids$fidelityID)} {build_qsub_string()} -v {envpass(
      'rel_suffix', 'debug_pipeline', 'sub', 'fidelity_json', 'loc_root', 'loc_bids_root', 'log_file', 'pipedir')} {file.path(pipedir, 'mri_fidelity_checks/qsub_fidelity_checks.sh')}"),
      rel_suffix, paste0("fidelityID-", sub))
    job_ids$fidelityID <- fidelityID
  }

  if (debug_pipeline == 2) {
    print(job_ids)
  }

  linked_jobs <- link_job_listings(unlist(job_ids))
  write(glue("{sub}\t{gsub(',+$', '', linked_jobs)}"), file = expectation_file, append = TRUE)

  job_string <- paste(sapply(names(job_ids), function(id) glue("afterany \"{job_ids[[id]]}\"")), collapse = " ")
  allJobIds <- paste(allJobIds, job_string)

  rel("", "c")
}

# --- Final Report Job ---
if (nchar(gsub('[afterany" ]', '', allJobIds)) > 0) {
  allDeps <- build_depend_string(strsplit(allJobIds, " ")[[1]])
  rel(glue("qsub {allDeps} -d {getwd()} {build_qsub_string()} -v {envpass('rel_suffix', 'debug_pipeline', 'aci_output_dir', 'expectation_file', 'loc_root', 'qsub_email', 'loc_yaml')} {file.path(pipedir, 'report.sh')}"),
      rel_suffix)
} else {
  rel("All subjects that have raw DICOM data have been fully processed: not submitting any jobs to qsub", "c")
}