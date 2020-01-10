
withWarnings <- function(expr) {
  myWarnings <- NULL
  myErrors   <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  eHandler <- function(e) {
    myErrors <<- c(myErrors, list(e))
    NULL
  }
  val <- withCallingHandlers(tryCatch(expr, error = eHandler), warning = wHandler)
  list(value = val, warnings = myWarnings, errors = myErrors)
}

collect_ya_errs <- function(e, fmt) {
  if (is.null(e)) return(NULL)
  vapply(e, "[[", character(1), "message")
  # warn <- paste0(" ", warn, collapse = "\n  ....")
  # paste(sprintf("  %s__:\n  ....%s", fmt, warn), collapse = "\n")
}

process_werrors <- function(warns, errs) {
  alertfun <- function(nm, wrn, color = "yellow") {
    cols <- c("yellow" = cli::symbol$warning, "red" = cli::symbol$error)
    sym  <- paste(cols[color], "")
    cli::cli_li(nm)
    dvid <- cli::cli_div(theme = list(span.emph = list(color = color, before = sym)))
    ulid <- cli::cli_ol()
    for (i in wrn[[nm]]){
      cli::cli_li("{.emph {i}}")
    }
    cli::cli_end(ulid)
    cli::cli_end(dvid)
  }
  warns <- warns[lengths(warns) > 0]
  errs  <- errs[lengths(errs) > 0]
  warned <- length(warns) > 0
  errored <- length(errs) > 0
  if (warned || errored) {
  
    cli::cli_ul()
    if (warned) {
      cli::cli_h2("Warnings were found in the following columns")
      for (i in names(warns)) {
        alertfun(i, warns)
      }
    }
    if (errored) {
      cli::cli_h2("Errors were found in the following columns")
      for (i in names(errs)) {
        alertfun(i, errs, "red")
      }
    }
    cli::cli_end()

  } else {
    NULL
  }
}
