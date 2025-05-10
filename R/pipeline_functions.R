### 

#' helper function that takes a character vector of CLI arguments and replaces matching old values with
#'   intended new values
#' @param args a character vector of existing CLI arguments
#' @param new_values a character vector of new CLI arguments to be substituted into `args`
#' @return a modified character vector of CLI arguments
#' @importFrom checkmate assert_character
#' @keywords internal
set_cli_options <- function(args = NULL, new_values = NULL) {
  checkmate::assert_character(new_values)

  # helper to convert a character vector of CLI arguments to a parsed data.frame with
  # arguments and values
  args_to_df <- function(arg_vec = NULL) {
    checkmate::assert_character(arg_vec)
    # if (is.character(arg_vec)) arg_vec <- list(arg_vec) # allow character vector input
    results <- list()

    # Split the string by whitespace to get individual arguments
    all_split <- strsplit(arg_vec, "\\s+")

    for (i in seq_along(arg_vec)) {
      tokens <- all_split[[i]]
      j <- 1
      while (j <= length(tokens)) {
        token <- tokens[j]
        nhyphens <- ifelse(grepl("^--", token), 2, ifelse(grepl("^-", token), 1, 0))

        if (nhyphens > 0) {
          token_naked <- sub("^--?", "", token)

          if (grepl("=", token_naked)) {
            parts <- strsplit(token_naked, "=", fixed = TRUE)[[1]]
            lhs <- parts[1]
            rhs <- parts[2]
            has_equals <- TRUE
          } else {
            lhs <- token_naked
            # Check if next token exists and is *not* another option (doesn't start with hyphen)
            if (j + 1 <= length(tokens) && !grepl("^-", tokens[j + 1])) {
              rhs <- tokens[j + 1]
              j <- j + 1 # consume the rhs token
            } else {
              rhs <- NA
            }
            has_equals <- FALSE
          }

          results[[length(results) + 1]] <- data.frame(
            argpos = i,
            lhs = lhs,
            rhs = rhs,
            has_equals = has_equals,
            nhyphens = nhyphens,
            stringsAsFactors = FALSE
          )
        }
        j <- j + 1
      }
    }

    do.call(rbind, results)
  }

  # helper to convert a parsed CLI data.frame back into a character string of CLI arguments
  df_to_args <- function(df) {
    split_df <- split(df, ~argpos)
    result <- character(length(split_df))

    for (i in seq_along(split_df)) {
      args <- split_df[[i]]
      tokens <- character(nrow(args))

      for (j in seq_len(nrow(args))) {
        prefix <- strrep("-", args$nhyphens[j])
        if (is.na(args$rhs[j])) {
          # No rhs
          tokens[j] <- paste0(prefix, args$lhs[j])
        } else {
          sep <- ifelse(args$has_equals[j], "=", " ")
          tokens[j] <- paste0(prefix, args$lhs[j], sep, args$rhs[j])
        }
      }

      # Concatenate the tokens with spaces
      result[i] <- paste(tokens, collapse = " ")
    }

    result
  }

  # helper to update rows in base_df, a parsed CLI data.frame with rows in updates_df, another parsed CLI data.frame
  update_cli_args <- function(base_df = NULL, updates_df) {
    for (i in seq_len(nrow(updates_df))) {
      upd <- updates_df[i, ]

      # Find matching lhs in base
      match_idx <- which(base_df$lhs == upd$lhs)

      if (length(match_idx) > 0) {
        # If found, update all matching rows
        base_df[match_idx, c("rhs", "has_equals", "nhyphens")] <- upd[, c("rhs", "has_equals", "nhyphens")]
      } else {
        # If not found, add the update to the first argpos (or choose strategy)
        upd$argpos <- ifelse(is.null(base_df), 1, min(base_df$argpos))
        base_df <- rbind(base_df, upd)
      }
    }

    # Re-sort by argpos if needed
    base_df <- base_df[order(base_df$argpos), ]
    rownames(base_df) <- NULL
    return(base_df)
  }

  if (is.null(args)) {
    args <- new_values
  } else {
    args_df <- args_to_df(args)
    new_values_df <- args_to_df(new_values)
    args <- df_to_args(update_cli_args(args_df, new_values_df))
  }

  return(args)
}


pretty_print_list <- function(x, indent = 0, width = 80) {
  indent_str <- strrep("  ", indent)

  for (name in names(x)) {
    value <- x[[name]]

    if (is.list(value)) {
      cat(sprintf("%s%s:\n", indent_str, name))
      pretty_print_list(value, indent + 1, width)
    } else {
      # Format value as character string
      value_str <- paste(value, collapse = ", ")

      # Wrap long lines
      wrapped <- strwrap(value_str,
        width = width - nchar(indent_str) - nchar(name) - 2,
        exdent = 2, simplify = FALSE
      )[[1]]

      # Print wrapped lines
      cat(sprintf("%s%s: %s\n", indent_str, name, wrapped[1]))
      if (length(wrapped) > 1) {
        for (line in wrapped[-1]) {
          cat(sprintf("%s%s  %s\n", indent_str, strrep(" ", nchar(name)), line))
        }
      }
    }
  }
}

# pretty_print_list(defaults)


setup_job <- function(scfg, job_name = NULL, prompt_all = FALSE, defaults = NULL) {
  if (prompt_all || is.null(scfg[[job_name]]$memgb)) {
    default_str <- ifelse(is.null(defaults$memgb), "", glue(" ({defaults$memgb} GB recommended)"))
    scfg[[job_name]]$memgb <- prompt_input(instruct = glue("How many GB of memory should be used for running {job_name}?{default_str}"), type = "integer", prompt = ">", lower = 1, upper = 1024, len = 1L)
  }

  # if (prompt_all || is.null(scfg[[job_name]]$walltime)) {
  #   default_str <- ifelse(is.null(defaults$walltime), "", glue(" ({defaults$walltime} GB recommended)"))
  #   scfg[[job_name]]$walltime <- hours_to_dhms(prompt_input(instruct = glue("How many hours should each run of {job_name} request? (min. 12 hours recommended)"), type = "integer", prompt = ">", lower = 1, upper = 1000, len = 1L))
  # }

  if (prompt_all || is.null(scfg[[job_name]]$nhours)) {
    default_str <- ifelse(is.null(defaults$nhours), "", glue(" ({defaults$nhours} hours recommended)"))
    scfg[[job_name]]$nhours <- prompt_input(instruct = glue("How many hours should each run of {job_name} request?{default_str}"), type = "numeric", prompt = ">", lower = 0.1, upper = 1000, len = 1L)
  }

  if (prompt_all || is.null(scfg[[job_name]]$ncores)) {
    default_str <- ifelse(is.null(defaults$ncores), "", glue(" ({defaults$ncores} recommended)"))
    scfg[[job_name]]$ncores <- prompt_input(instruct = glue("How many cores/CPUs should each job request?{default_str}"), type = "integer", prompt = ">", lower = 1, upper = 1000, len = 1L)
  }

  if (prompt_all || is.null(scfg[[job_name]]$cli_options)) {
    scfg[[job_name]]$cli_options <- build_cli_args(args = scfg[[job_name]]$cli_options, instruct = glue("Specify any other {job_name} command line arguments. Press Enter when done."))
  }

  if (prompt_all || is.null(scfg[[job_name]]$sched_args)) {
    sched_queue <- ifelse(!is.null(scfg$compute_environment$scheduler) && scfg$compute_environment$scheduler == "torque", "#PBS", "#SBATCH")
    scfg[[job_name]]$sched_args <- build_cli_args(args = scfg[[job_name]]$sched_args, instruct = glue("Specify any other arguments to pass to the job scheduler These usually begin {sched_queue}. Press Enter when done."))
  }

  return(scfg)
}

# test_scfg <- setup_job(list(), "fmriprep", defaults = list(memgb = 32, nhours = 48, ncores = 6))


pretty_arg <- function(x, width = 80) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L])) {
    "[None]"
  } else {
    strwrap(x, width, exdent = 2)
  }
}


#' This function returns an lgr object corresponding to logging for a single subject directory
#' @param scfg The study configuration object
#' @param sub_dir The directory of the target subject's folder within the BIDS structure
#' @return a configured lgr object for logging subject processing messages
#' @importFrom lgr get_logger_glue
#' @keywords internal
get_subject_logger <- function(scfg, sub_dir) {
  sub_id <- get_sub_id(sub_dir)
  lg <- lgr::get_logger_glue(c("sub", sub_id))
  if (isTRUE(scfg$log_txt) && !"subject_logger" %in% names(lg$appenders)) {
    lg$add_appender(lgr::AppenderFile$new(file.path(sub_dir, glue("sub-{sub_id}_log.txt"))), name = "subject_logger")
  }

  return(lg)
}

#' Obtain user input from the console
#' @param prompt The character string to place in front of the user input prompt. For example, "> "
#' @param instruct The instructions to display above the prompt.
#' @param lower For numeric inputs, the lowest valid value
#' @param upper For numeric inputs, the highest valid value
#' @importFrom checkmate test_string assert_string assert_subset assert_number
#' @keywords internal
prompt_input <- function(prompt = "", instruct = NULL, type = "character", lower = -Inf, upper = Inf, 
  len = NULL, min.len=NULL, max.len=NULL, split = NULL, among = NULL, required = TRUE, uniq=FALSE) {

  checkmate::assert_string(prompt, null.ok = TRUE)
  checkmate::assert_string(instruct, null.ok = TRUE)
  checkmate::assert_subset(type, c("numeric", "integer", "character", "file", "flag"))
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_number(len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(min.len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(max.len, lower = 1L, null.ok = TRUE)
  if (!is.null(min.len) && !is.null(max.len) && max.len < min.len) {
    stop("max.len must be greater than or equal to min.len")
  }

  if (type == "flag" && !is.null(len) && len > 1L) {
    warning("Ignoring len > 1 for type 'flag' -- only one return supported")
    len <- 1L
  }

  if (!is.null(len) && len > 1 && !checkmate::test_string(split)) {
    stop("When multiple return values are required, you must specify character(s) to split the input.")
  }

  # setup feedback about the number and type of inputs expected
  inp_expect <- ""
  plural <- "s"
  if (!is.null(len)) {
    n_expect <- ifelse(len == 1L, "a", glue("{len}"))
    if (len==1L) plural <- ""
  } else if ((is.null(min.len) || min.len == 1L) && (is.null(max.len) || is.infinite(max.len))) {
    n_expect <- ""
  } else if (is.null(min.len) || min.len == 1L) {
    # max only
    n_expect <- glue("no more than {max.len}")
  } else if (is.null(max.len) || is.infinite(max.len)) {
    # min only
    n_expect <- glue("at least {min.len}")
  } else {
    # min and max
    n_expect <- glue("{min.len}-{max.len}")
  }

  if (nchar(n_expect) > 0L) n_expect <- paste0(n_expect, " ") # add spacing for formatting

  if (type=="integer") {
    if (n_expect=="a ") n_expect <- "an "
    inp_expect <- glue("Input must be {n_expect}integer{plural} between {lower} and {upper}\n")
  } else if (type == "numeric") {
    inp_expect <- glue("Input must be {n_expect}number{plural} between {lower} and {upper}\n")
  } else if (type == "character") {
    inp_expect <- glue("Input must be {n_expect}string{plural} separated by '{split}'\n")
  }

  # add options for flag prompt
  if (type == "flag") {
    # always ask user for yes/no input
    prompt <- paste(prompt, ifelse(required, "(yes/no)", "(yes/no; press Enter to skip)"))
  } else if (!required) {
    prompt <- paste(prompt, "(Press enter to skip)") # let user know how to skip optional input
  }

  # always add trailing space to make prompt clear
  if (substr(prompt, nchar(prompt), nchar(prompt)) != " ") prompt <- paste0(prompt, " ")

  # print instructions
  if (checkmate::test_string(instruct)) cat(instruct, "\n")

  # obtain user input
  res <- ""
  while (is.na(res[1L]) || res[1L] == "") {
    r <- readline(prompt)
    if (!is.null(split)) r <- strsplit(r, split, perl = TRUE)[[1]]

    if (isFALSE(required) && r[1L] == "") {
      return(NA) # empty input
    } else if (isTRUE(uniq) && length(unique(r)) != length(r)) {
      cat("All entries must be unique.\n")
    } else if (type == "flag") {
      r <- tolower(r)
      if (!r[1L] %in% c("yes", "no", "y", "n")) {
        cat("Please respond yes or no.\n")
      } else {
        res <- ifelse(substr(r[1L], 1, 1) == "y", TRUE, FALSE)
      }
    } else if (type == "integer") {
      r <- type.convert(r, as.is = TRUE) # convert to apparent atomic type for validation
      if (!checkmate::test_integerish(r, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be integers in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }
      }
    } else if (type == "numeric") {
      r <- type.convert(r, as.is = TRUE) # convert to apparent atomic type for validation
      if (!checkmate::test_numeric(r, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        res <- r
      }
    } else if (type == "character") {
      if (!checkmate::test_character(r, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be {len} strings in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }
      }
    } else if (type == "file") {
      # should probably think harder about unquoted filenames containing spaces throwing off len
      exist <- sapply(r, checkmate::test_file_exists)
      if (!all(exist)) {
        cat(glue("The following files could not be found: {paste(r[!exist], collapse=', ')}\n"))
      } else {
        res <- r
      }
    }
  }

  return(res)
}

# x <- prompt_input(prompt = "test me?", type = "character", split = " ")
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", len = 3)
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", min.len = 2)
# x <- prompt_input(prompt="test me?", type="character", split=" ", max.len=2)
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", min.len = 2, max.len = 5)
# x <- prompt_input(prompt = "test me?", type = "numeric", split = " ", min.len = 2, max.len = 5)
# x <- prompt_input(prompt="test me?", type="integer", split=" ", min.len=2, max.len=5)

#' Helper function to check whether a given file or directory exists and, optionally, is readable
#' @param f a file or directory to check for existence
#' @param description a character string describing what this file is if we are prompted to change it
#' @param directory if TRUE, check whether a directory exists. If FALSE (default), check that the file exists
#' @param prompt_change if TRUE, if the file/directory exists, ask the user if they wish to change the value. If so, return FALSE
#' @param check_readable if TRUE, validation fails (return `FALSE`) when the file/directory exists but is not readable
#' @return a boolean (`TRUE/FALSE`) indicating whether the file or directory exists and is valid
#' @importFrom checkmate assert_flag assert_string test_directory_exists test_file_exists
#' @keywords internal
validate_exists <- function(f, description="", directory=FALSE, prompt_change=FALSE, check_readable=TRUE) {  
  checkmate::assert_string(description)
  checkmate::assert_flag(directory)
  checkmate::assert_flag(prompt_change)
  checkmate::assert_flag(check_readable)

  if (directory) {
    func <- checkmate::test_file_exists
    type <- "directory"
  } else {
    func <- checkmate::test_directory_exists
    type <- "file"
  }

  if (!checkmate::test_atomic(f) || is.null(f) || length(f) == 0L || is.na(f[1L])) {
    return(FALSE)
  } else if (checkmate::test_character(f)) {
    if (length(f) > 1L) {
      warning("validate_exists only works with a single string as input, but we received a character vector. Using first element.")
      f <- f[1]
    }

    if (func(f)) {
      if (check_readable && !func(f, access = "r")) {
        warning(glue("Found existing {type}, but you do not have read permission: {f}"))
        return(FALSE)
      }

      if (isTRUE(prompt_change)) {
        cat(glue("Found existing {description}: {f}\n"))
        change <- prompt_input("Change setting?", type = "flag")
        if (isFALSE(change)) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(TRUE)
      }
    }
  } else {
    # some other non-character data type that somehow made it past check_atomic?
    return(FALSE)
  }
}

###

#' helper function
read_multiline_input <- function(instruct=NULL, prompt="> ", n_blank=1, collapse=NULL) {
  if (!is.null(instruct)) cat(instruct, "\n")
  lines <- character()
  empty_count <- 0
  
  repeat {
    line <- readline(prompt)
    
    if (nchar(trimws(line)) == 0) {
      empty_count <- empty_count + 1
    } else {
      empty_count <- 0
    }
    
    if (empty_count >= n_blank) break
    
    lines <- c(lines, line)
  }
  
  # Collapse into one string or return vector of lines
  if (!is.null(collapse)) {
    return(paste(lines, collapse = collapse))
  } else {
    return(lines)
  }
}

build_cli_args <- function(args=NULL, prompt="> ", instruct = "Enter arguments (press Enter to finish): ", collapse=NULL) {
  # Step 1: Multi-line input
  cat(instruct, "\n")
  lines <- character()

  # If existing args are passed in, prompt edits and confirmation of changes. If no args, just accept entry and return
  has_args <- !is.null(args)
  
  # Step 2: Interactive edit loop
  repeat {
    if (!is.null(args)) {
      cat("\nCurrent arguments:\n")
      if (length(args) == 0) {
        cat("  [None]\n")
      } else {
        for (i in seq_along(args)) {
          cat(sprintf("  [%d] %s\n", i, args[i]))
        }
      }

      cat("\nOptions:\n")
      cat("  1: Add argument\n")
      cat("  2: Edit argument\n")
      cat("  3: Delete argument\n")
      cat("  4: Done\n")

      choice <- readline("Enter choice [1-4]: ")
    } else {
      choice <- "1" # if no arguments, start by prompting
    }
    
    if (choice == "1") {
      new_arg <- read_multiline_input(prompt=prompt)
      args <- c(args, new_arg)
      if (!has_args) break # don't require confirmation on first entry of arguments
    } else if (choice == "2") {
      idx <- as.integer(readline("Enter argument number to edit: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        current_val <- args[idx]
        new_val <- readline(sprintf("New value for [%s] (press Enter to keep): ", current_val))
        if (nzchar(new_val)) {
          args[idx] <- new_val
        } else {
          cat("Keeping existing value.\n")
        }
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "3") {
      idx <- as.integer(readline("Enter argument number to delete: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        args <- args[-idx]
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "4") {
      break # Done action
    } else {
      cat("Invalid choice. Please enter 1, 2, 3, or 4.\n")
    }
  }
  
  # Collapse into one string or return vector of lines
  if (!is.null(collapse)) {
    return(paste(args, collapse = collapse))
  } else {
    return(args)
  }
}

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

