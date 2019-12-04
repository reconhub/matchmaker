#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @description This function allows you to clean your data according to 
#' pre-defined rules encapsulated in either a data frame or list of data frames.
#' It has application for addressing mis-spellings and recoding variables (e.g.
#' from electronic survey data). 
#'
#' @param matchbook a data frame or named list of data frames with at least two
#'   columns defining the word list to be used. If this is a data frame, a third
#'   column must be present to split the matchbook by column in `x` (see
#'   `spelling_vars`).
#'
#' @param spelling_vars character or integer. If `matchbook` is a data frame,
#'   then this column in defines the columns in `x` corresponding to each
#'   section of the `matchbook` data frame. This defaults to `3`, indicating the
#'   third column is to be used.
#'
#' @param sort_by a character the column to be used for sorting the values in
#'   each data frame. If the incoming variables are factors, this determines how
#'   the resulting factors will be sorted.
#' 
#' @param warn if `TRUE`, warnings and errors from [match_me()] will be 
#'   shown as a single warning. Defaults to `FALSE`, which shows nothing.
#'
#' @inheritParams clean_variable_labels
#' @inheritParams match_me
#'
#' @details By default, this applies the function [match_me()] to all
#'   columns specified by the column names listed in `spelling_vars`, or, if a
#'   global dictionary is used, this includes all `character` and `factor`
#'   columns as well.
#'
#' \subsection{spelling_vars}{
#' 
#' Spelling variables within `matchbook` represent keys that you want to match
#' to column names in `x` (the data set). These are expected to match exactly
#' with the exception of two reserved keywords that starts with a full stop:
#'
#'  - `.regex [pattern]`: any column whose name is matched by `[pattern]`. The
#'  `[pattern]` should be an unquoted, valid, PERL-flavored regular expression.
#'  - `.global`: any column (see Section *Global matchbook*)
#'
#' }
#'
#' \subsection{Global matchbook}{
#' 
#' A global matchstick is a set of definitions applied to all valid columns of `x`
#' indiscriminantly.
#'
#'  - **.global spelling_var**: If you want to apply a set of definitions to all
#'     valid columns in addition to specified columns, then you can include a
#'     `.global` group in the `spelling_var` column of your `matchbook` data
#'     frame. This is useful for setting up a dictionary of common spelling 
#'     errors. *NOTE: specific variable definitions will override global
#'     defintions.* For example: if you have a column for cardinal directions
#'     and a definiton for `N = North`, then the global variable `N = no` will
#'     not override that. See Example.
#'
#'  - **`spelling_vars = NULL`**: If you want your data frame to be applied to
#'    all character/factor columns indiscriminantly, then setting 
#'    `spelling_vars = NULL` will use that matchstick globally.
#'
#' }
#'
#' 
#' @note This function will only parse character and factor columns to protect
#'   numeric and Date columns from conversion to character. 
#'
#' @return a data frame with re-defined data based on the dictionary 
#'
#' @seealso [match_me()], which this function wraps.
#'
#' @author Zhian N. Kamvar
#' @author Patrick Barks
#'
#' @export
#'
#' @examples
#' 
#' # Read in dictionary and coded date examples --------------------
#'
#' matchstick <- read.csv(matchmaker_example("spelling-dictionary.csv"), 
#'                      stringsAsFactors = FALSE)
#' dat      <- read.csv(matchmaker_example("coded-data.csv"), 
#'                      stringsAsFactors = FALSE)
#' dat$date <- as.Date(dat$date)
#'
#' # Clean spelling based on matchstick ------------------------------ 
#'
#' matchstick # show the matchstick
#' head(dat) # show the data
#' 
#' res1 <- match_us(dat,
#'                                 matchbook = matchstick,
#'                                 from = "options",
#'                                 to = "values",
#'                                 spelling_vars = "grp")
#' head(res1)
#' 
#' # You can ensure the order of the factors are correct by specifying 
#' # a column that defines order.
#'
#' dat[] <- lapply(dat, as.factor)
#' as.list(head(dat))
#' res2 <- match_us(dat, 
#'                                 matchbook = matchstick, 
#'                                 from = "options",
#'                                 to = "values",
#'                                 spelling_vars = "grp", 
#'                                 sort_by = "orders")
#' head(res2)
#' as.list(head(res2))
#' 
match_us <- function(x = data.frame(), matchbook = list(),
                                    from = 1, to = 2, spelling_vars = 3,
                                    sort_by = NULL, classes = NULL, 
                                    warn = FALSE) {
  
  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }
  if (is.null(classes)) {
    classes <- i_find_classes(x)
  }

  # Define columns viable for manipulation ------------------------------------
  # Because this is a global manipulator, only work on characters or factors
  unprotected <- names(x)[classes %in% c("character", "factor")]
  
  if (length(matchbook) == 0 || !is.list(matchbook)) {
    stop("matchbook must be a list of data frames")
  } 

  # There is one big dictionary with spelling_vars -----------------------------
  if (is.data.frame(matchbook)) {

    # the from and to columns exist
    from_exists <- i_check_scalar(from) && i_check_column_name(from, names(matchbook))
    to_exists   <- i_check_scalar(to)   && i_check_column_name(to, names(matchbook))

    if (!from_exists || !to_exists) {
      stop("`from` and `to` must refer to columns in the matchstick")
    }

    # There is a spelling_vars column ----------------------------------------
    spelling_vars_exists <- i_check_scalar(spelling_vars)

    if (spelling_vars_exists) {
      valid_spelling_vars <- i_check_column_name(spelling_vars, names(matchbook))
      if (valid_spelling_vars) {
        matchbook <- split(matchbook, matchbook[[spelling_vars]])
      } else {
        stop("spelling_vars must be the name or position of a column in the matchstick")
      }
    } else {
      warning("Using matchstick globally across all character/factor columns.")
    }

  } else {
    # Not everything is a data frame :( ---------------------------------------
    if (!all(vapply(matchbook, is.data.frame, logical(1)))) {
      stop("everything in matchbook must be a data frame")
    }

    # Not all dictionaries are named ------------------------------------
    if (any(names(matchbook) == "")) {
      stop("all dictionaries must be named")
    }
  }

  one_big_dictionary <- is.data.frame(matchbook)
  exists_sort_by     <- !is.null(sort_by)
  
  if (one_big_dictionary) {
    # If there is one big dictionary ------------------------------------
    if (exists_sort_by && sort_by %in% names(matchbook)) {
      matchbook <- matchbook[order(matchbook[[sort_by]]), , drop = FALSE]
    }
    # Iterate over the names of the data -------------------
    to_iterate_x <- unprotected
    to_iterate_matchstick <- unprotected
  } else {
    # If there is a list of dictionaries --------------------------------
    if (exists_sort_by) {
      for (i in names(matchbook)) {
        di <- matchbook[[i]]
        # Only sort if there is something to sort by -------
        the_sorts  <- if (any(names(di) == sort_by)) order(di[[sort_by]]) else TRUE
        matchbook[[i]] <- matchbook[[i]][the_sorts, , drop = FALSE]
      }
    }
    global_words <- matchbook[[".global"]]
    matchbook    <- matchbook[names(matchbook) != ".global"]
    has_global   <- !is.null(global_words)
    
    # Identify columns of x to clean, and matching entry in matchbook ----------
    
    # Extract vars to check from matchbook (both plain and regex) ------------
    vars_check <- names(matchbook)
    is_var_regex <- grepl("^\\.regex[[:space:]]", vars_check)
    
    # If any .regex keys... ------------
    if (any(is_var_regex)) {
      
      vars_check_plain <- vars_check[!is_var_regex]
      vars_check_regex <- vars_check[is_var_regex]
      vars_check_regex_extract <- gsub("\\.regex[[:space:]]", "", vars_check_regex)
      
      # which cols in x match each regex var (1 element for each .regex key)
      vars_regex_match_list <- lapply(
        vars_check_regex_extract,
        FUN = grep,
        x = names(x), value = TRUE, perl = TRUE
      )
      
      # check for matchstick variables with no match in x
      vars_regex_match_n <- lengths(vars_regex_match_list)
      vars_plain_nomatch <- vars_check_plain[!vars_check_plain %in% names(x)]
      vars_regex_nomatch <- vars_check_regex[vars_regex_match_n == 0]
      vars_nomatch <- unique(c(vars_plain_nomatch, vars_regex_nomatch))
      
      # all columns of x that match variable in matchbook
      cols_match_plain <- vars_check_plain[vars_check_plain %in% names(x)]
      cols_match_regex <- unlist(vars_regex_match_list, use.names = FALSE)
      cols_match <- c(cols_match_plain, cols_match_regex)
      
      # matchstick variable name corresponding to each matching column in x
      matching_var_plain <- cols_match_plain
      matching_var_regex <- rep(vars_check_regex, vars_regex_match_n)
      matching_var <- c(matching_var_plain, matching_var_regex)
      
      # columns of x to iterate over, and matching key from matchstick
      to_iterate_x <- cols_match
      to_iterate_matchstick <- matching_var
      
    # Else no .regex keys, column names in x and matchstick keys matched literally
    } else { 
      
      to_iterate_x <- to_iterate_matchstick <- intersect(vars_check, names(x))
      vars_nomatch <- setdiff(vars_check, names(x))
    }
    
    # Warn if any variables in matchstick don't match any columns in x ------
    if (length(vars_nomatch) > 0) {
      warning("The following variable(s) in 'matchstick' did not match any ",
              "columns in 'x': ", paste(vars_nomatch, collapse = ", "),
              call. = FALSE)
    }
    
    # If .global keyword in matchbook, add all uprotected columns to to_iterate_
    if (has_global) {
      unprotected_to_add <- setdiff(unprotected, to_iterate_x)
      to_iterate_x <- c(to_iterate_x, unprotected_to_add)
      to_iterate_matchstick <- c(to_iterate_matchstick, unprotected_to_add)
    }
  }

  # check if there is a ".default" value in the global dictionary
  global_with_default <- one_big_dictionary && 
    any(matchbook[[1]] == ".default") || 
    (
     !one_big_dictionary && 
     has_global && 
     any(global_words[[1]] == ".default")
    )

  if (global_with_default) {
  
    stop("the .default keyword cannot be used with .global")
  
  }
  
  # Prepare warning/error labels ---------------------------------------------
  warns <- errs <- vector(mode = "list", length = length(to_iterate_x))
  iter_print <- gsub(" ", "_", format(to_iterate_x))
  names(iter_print) <- names(warns) <- names(errs) <- to_iterate_x

  # Loop over the variables and clean spelling --------------------------------
  for (i in seq_along(to_iterate_x)) {
    
    i_x <- to_iterate_x[i]
    i_w <- to_iterate_matchstick[i]
    
    d <- if (one_big_dictionary) matchbook else matchbook[[i_w]]

    if (is.null(d)) {
    # d is null because this is a variable without a specific spelling def
      d <- global_words
    } else if (!one_big_dictionary) {
    # d is not null, but the input has specific variables
      # find the words that match the matchstick
      gw <- !global_words[[1]] %in% d[[1]]
      if (sum(gw) > 0) {
      # If there are still global words to clean, pass them through
        g <- global_words[gw, , drop = FALSE]
        w <- withWarnings({
          match_me(x[[i_x]], g, from = from, to = to, quiet = FALSE)
        })
        x[[i_x]] <- if(is.null(w$val)) x[[i_x]] else w$val
        if (warn) {
          warns[[i_x]] <- collect_ya_errs(w$warnings, iter_print[i_x])
          errs[[i_x]]  <- collect_ya_errs(w$errors, iter_print[i_x])
        }
      }
    } else {
      # There is one big, global dictionary
      d <- d
    }
    # Evaluate and collect any warnings/errors that pop up
    w <- withWarnings({
      match_me(x[[i_x]], d, from = from, to = to, quiet = FALSE)
    })
    x[[i_x]] <- if(is.null(w$val)) x[[i_x]] else w$val
    if (warn) {
      warns[[i_x]] <- c(warns[[i_x]], collect_ya_errs(w$warnings, iter_print[i_x]))
      errs[[i_x]]  <- c(errs[[i_x]], collect_ya_errs(w$errors, iter_print[i_x]))
    }
  }

  # Process warnings and errors and give a warning if there were any
  if (warn) {
    wemsg <- process_werrors(warns, errs)
    if (!is.null(wemsg)) warning(wemsg)
  }

  x
}
