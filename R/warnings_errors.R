
withWarnings <- function(expr) {
  myWarnings <- NULL
  myErrors   <- NULL
  # NOTE: This function is properly scoped since myWarnings is defined directly
  #       above. Thus, the <<- operator is valid and won't reach outside of
  #       this scope.
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
}

process_werrors <- function(warns, errs, dname) {

  alertfun <- function(nm, wrn, dname, color = "yellow") {
    nam <- sprintf("{.code %s}", nm)
    dnam <- sprintf("\\1 {.code %s}\\2", dname)
    wrn[[nm]] <- gsub("x[[i_x]]", nam, wrn[[nm]], fixed = TRUE)
    wrn[[nm]] <- gsub("(in|of) g([.;:])", "\\1 the global dictionary\\2", wrn[[nm]], perl = TRUE)
    wrn[[nm]] <- gsub("(in|of) d([.;:])", dnam, wrn[[nm]], perl = TRUE)
    cols <- c("yellow" = cli::symbol$warning, "red" = cli::symbol$error)
    sym  <- paste(cols[color], "")

    cli::cli_li(nm)

    dvid <- cli::cli_div(theme = list(span.emph = list(color = color, before = sym)))
    on.exit(cli::cli_end(dvid), add = TRUE, after = FALSE)

    ulid <- cli::cli_ol()
    on.exit(cli::cli_end(ulid), add = TRUE, after = FALSE)

    for (i in wrn[[nm]]){
      the_warn <- sprintf("{.emph %s}", i)
      cli::cli_li(the_warn)
    }
  }

  warns <- warns[lengths(warns) > 0]
  errs  <-  errs[lengths(errs) > 0]

  warned  <- length(warns) > 0
  errored <-  length(errs) > 0

  if (warned || errored) {
  
    cli::cli_ul()
    if (warned) {
      cli::cli_h2("Warnings were found in the following columns")
      for (i in names(warns)) {
        alertfun(i, warns, dname)
      }
    }
    if (errored) {
      cli::cli_h2("Errors were found in the following columns")
      for (i in names(errs)) {
        alertfun(i, errs, dname, "red")
      }
    }
    cli::cli_end()
  } else {
    NULL
  }
}
