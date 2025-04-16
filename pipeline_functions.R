# Load required libraries
library(glue)
library(stringr)

# Helper: emulate envpass from bash
envpass <- function(...) {
  keys <- unlist(list(...))
  kv_pairs <- sapply(keys, function(k) {
    if (str_detect(k, '=')) {
      return(k)
    } else {
      val <- Sys.getenv(k, unset = NA)
      if (is.na(val)) message(glue("{k} is empty"))
      return(glue("{k}='{val}'"))
    }
  })
  paste(kv_pairs, collapse = ",")
}

# Helper: emulate build_qsub_string from bash
build_qsub_string <- function(...) {
  args <- unlist(list(...))
  qsub_allocation <- Sys.getenv("qsub_allocation", unset = "open")
  qsub_email <- Sys.getenv("qsub_email", unset = NA)
  qsub_string <- glue("-A {qsub_allocation}")

  if (!is.na(qsub_email)) {
    qsub_string <- glue("{qsub_string} -M {qsub_email}")
  }

  if (length(args) > 0) {
    for (arg in args) {
      if (str_detect(arg, '=')) {
        qsub_string <- glue("{qsub_string} -l {arg}")
      } else {
        val <- Sys.getenv(arg, unset = NA)
        if (!is.na(val)) {
          qsub_string <- glue("{qsub_string} -l {arg}='{val}'")
        }
      }
    }
  }
  qsub_string
}

# Helper: emulate rel from bash
rel <- function(cmd, mode = NULL, substitute = NULL) {
  log_file <- Sys.getenv("log_file", unset = NA)
  comment <- "c" %in% mode
  timeit <- "t" %in% mode
  print_stdout <- "o" %in% mode

  tic <- Sys.time()
  output <- NULL

  if (!is.na(log_file)) {
    write(glue("{if (comment) '## ' else ''}{cmd}"), file = log_file, append = TRUE)
  }

  if (comment && !is.null(substitute)) {
    return(substitute)
  }

  if (!comment) {
    if (!print_stdout) cat(glue("{cmd}\n"))
    output <- system(cmd, intern = print_stdout)
    toc <- Sys.time()
    if (timeit) {
      dur <- round(difftime(toc, tic, units = "secs"), 2)
      if (!is.na(log_file)) write(glue("# took {dur} seconds"), file = log_file, append = TRUE)
    }
    if (print_stdout) cat(output, sep = "\n")
    return(output)
  } else {
    cat(glue("----  {cmd}\n"))
  }
}

# Helper: emulate link_job_listings
link_job_listings <- function(...) {
  args <- unlist(list(...))
  cleaned <- gsub("\\.torque01\\.[a-z0-9.]*edu", "", args)
  paste0(cleaned, collapse = ",")
}

# Helper: emulate build_depend_string
build_depend_string <- function(...) {
  args <- unlist(list(...))
  if (length(args) %% 2 != 0) stop("Expected even number of arguments: condition jobid ...")
  res <- ""
  for (i in seq(1, length(args), by = 2)) {
    if (!is.null(args[i + 1]) && args[i + 1] != "\"\"") {
      res <- paste0(res, glue("{args[i]}:{args[i + 1]}"), ",")
    }
  }
  res <- str_remove(res, ",$")
  if (res == "") "" else glue("-W depend={res}")
}

# The rest of your preprocess_subject script logic would go here and make use of the above helper functions.
