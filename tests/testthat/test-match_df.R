context("match_df() tests")

{
corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", ".missing", "unknown", "Yes", "Y", "No", "N", ".missing"),
  good = c("foobar", "foobar", "foobar", "missing", "missing", "yes", "yes", "no", "no", "missing"),
  column = c(rep("raboof", 5), rep("treatment", 5)),
  orders = c(1:5, 5:1),
  stringsAsFactors = FALSE
)

clist <- split(corrections, corrections$column)

my_data_frame <- data.frame(
  raboof = c(letters[1:5], "foubar", "foobr", "fubar", "", "unknown", "fumar"),
  treatment = c(letters[5:1], "Y", "Yes", "N", NA, "No", "yes"),
  region = state.name[1:11]
)

cleaned_data <- data.frame(
  raboof = factor(c(letters[1:5], "foobar", "foobar", "foobar", "missing", "missing", "fumar"),
    levels = c("foobar", "missing", letters[1:5], "fumar")
  ),
  treatment = factor(c(letters[5:1], "yes", "yes", "no", "missing", "no", "yes"),
    levels = c("yes", "no", "missing", letters[1:5])
  ),
  region = state.name[1:11]
)
}


test_that("a data frame is needed for the first part", {
  expect_error(match_df(), "x must be a data frame")
  expect_error(match_df(clist), "x must be a data frame")
  expect_error(match_df(my_data_frame$raboof), "x must be a data frame")
})


test_that("a list of data frames is needed for the second part", {
  expect_error(
    match_df(my_data_frame),
    "dictionary must be a list of data frames"
  )
  expect_error(
    match_df(my_data_frame, list(1:10)),
    "everything in dictionary must be a data frame"
  )
  expect_error(
    match_df(my_data_frame, c(clist, list(corrections))),
    "all dictionaries must be named"
  )
  expect_warning(
    match_df(my_data_frame, c(clist, funkytime = list(corrections))),
    "funkytime"
  )
})

test_that("columns can be specified for the data despite order", {
  expect_identical(
    match_df(my_data_frame, corrections[sample(4)],
      from = "bad",
      to = "good",
      by = "column"
    ),
    cleaned_data
  )
})

test_that("definitions with missing from, to, and order will be ignored", {

  # Example: x column is specified
  mdf <- cbind(my_data_frame, x = 1:11, y = 1:11 > 5)
  cxn <- rbind(corrections, c(bad = NA, good = NA, column = "x", orders = NA))
  cxn$junk <- sample(letters, 11)
  cxn$more_junk <- runif(11)

  res <- match_df(mdf, cxn)

  expect_equal(res$x, 1:11)
  expect_equal(res$y, 1:11 > 5)

  # Example: y column is specified
  cxn[nrow(cxn), "column"] <- "y"

  res <- match_df(mdf, cxn)

  expect_equal(res$x, 1:11)
  expect_equal(res$y, 1:11 > 5)

  # Example, missing data are entered into the "treatment" column
  new_junk <- data.frame(
    bad = NA,
    good = NA,
    column = "treatment",
    orders = NA,
    junk = "what",
    more_junk = pi
  )
  res <- match_df(mdf, rbind(cxn, new_junk))

  expect_equal(res$x, 1:11)
  expect_equal(res$y, 1:11 > 5)
  expect_equal(res$treatment, cleaned_data$treatment)
})

test_that("a single error will be thrown if the columns are not in the correct order", {
  expect_error(
    match_df(my_data_frame, corrections,
      from = "hello", to = "there"
    ),
    "`from` and `to` must refer to columns in the dictionary"
  )
  expect_error(
    match_df(my_data_frame, corrections,
      from = 0, to = 11
    ),
    "`from` and `to` must refer to columns in the dictionary"
  )
  expect_error(
    match_df(my_data_frame, corrections,
      from = 1, to = 11
    ),
    "`from` and `to` must refer to columns in the dictionary"
  )
  expect_error(
    match_df(my_data_frame, corrections,
      from = 6, to = "good"
    ),
    "`from` and `to` must refer to columns in the dictionary"
  )
  expect_error(
    match_df(my_data_frame, corrections,
      from = "bad", to = 99
    ),
    "`from` and `to` must refer to columns in the dictionary"
  )
})

test_that("spelling cleaning works as expected", {
  test_cleaned <- match_df(my_data_frame, clist)
  expect_identical(test_cleaned$raboof, cleaned_data$raboof)
  # Uncomment if this is mis-behaving. <3, Zhian
  # print(fct_recode(my_data_frame$treatment, yes = "Yes", yes = "Y", no = "No", no = "N"))
  # print(test_cleaned$treatment)
  # print(cleaned_data$treatment)
  expect_identical(test_cleaned$treatment, cleaned_data$treatment)
  expect_identical(test_cleaned$region, cleaned_data$region)
})

test_that("default errors will be thrown", {

  corr <- data.frame(
    bad = c(".default", ".default", "kruh"),
    good = c("check data", "check data", "hurk"),
    column = c("raboof", "treatment", "raboof"),
    orders = Inf,
    stringsAsFactors = FALSE
  )
  corr <- rbind(corrections, corr, corrections)
  expect_message(match_df(my_data_frame, corr, warn = TRUE), "Duplicate keys")

  skip_if(!cli::is_utf8_output())

  verify_output(path = test_path("cli-messages", "default-errs-1.txt"),
    match_df(my_data_frame, corr, warn = TRUE),
    crayon = FALSE,
    unicode = FALSE,
  )
})


test_that("errors will be captured and passed through; error'd cols are preserved", {
  with_list <- my_data_frame
  with_list$listcol <- as.list(with_list$region)
  corr <- corrections
  corr[12, ] <- c("Florida", "Flo Rida", "listcol", 1)
  lc <- match_df(with_list, corr, warn = FALSE)
  expect_length(lc, 4)
  expect_is(lc[[4]], "list")
  expect_named(lc, names(with_list))
  expect_identical(with_list[[4]], lc[[4]])

  skip_if(!cli::is_utf8_output())

  verify_output(path = test_path("cli-messages", "default-errs-2.txt"),
    match_df(with_list, corr, warn = TRUE),
    crayon = FALSE,
    unicode = FALSE,
  )
})


test_that("sorting works as expected", {

  # sorting by data.frame
  test_sorted_df <- match_df(my_data_frame,
    corrections,
    by = "column",
    order = "orders"
  )

  # sorting by list
  test_sorted_ls <- match_df(my_data_frame,
    clist,
    order = "orders"
  )
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, c("missing", "no", "yes"))
  expect_identical(test_sorted_df, test_sorted_ls)
  expect_identical(test_sorted_df$raboof, cleaned_data$raboof)
  expect_identical(test_sorted_df$treatment, resorted_trt)
})

test_that("global data frame works if by = NULL", {
  expect_error(
    {
      global_test <- match_df(my_data_frame, corrections, by = 69)
    },
    "`by` must be the name or position of a column in the dictionary"
  )

  # global, no order -----------------------------------------------
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "missing")
  expect_warning(
    {
      global_test <- match_df(my_data_frame, corrections, by = NULL)
    },
    "Using dictionary globally across all character/factor columns."
  )
  expect_identical(global_test$raboof, cleaned_data$raboof)
  expect_identical(global_test$treatment, resorted_trt)
})

test_that("global dictionary works with order", {
  # global, with order ---------------------------------------------
  # The order specifies missing, no, yes
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "missing", "no")
  expect_warning(
    {
      global_order_test <- match_df(my_data_frame, corrections, order = "orders", by = NULL)
    },
    "Using dictionary globally across all character/factor columns.",
    fixed = TRUE
  )
  expect_identical(global_order_test$raboof, cleaned_data$raboof)
  expect_identical(global_order_test$treatment, resorted_trt)

  skip_if(!cli::is_utf8_output())

  verify_output(path = test_path("cli-messages", "global-errs-1.txt"),
    match_df(my_data_frame, corrections, order = "orders", by = NULL, warn = TRUE),
    crayon = FALSE,
    unicode = FALSE,
  )
})

test_that("global dictionary works with a reverse order", {
  # global, reverse order ------------------------------------------
  cxns <- corrections
  cxns$orders <- c(5:1, 1:5)
  # In both instances, missing will come first
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "missing")
  resorted_foo <- forcats::fct_relevel(cleaned_data$raboof, "missing")
  expect_warning(
    {
      global_rev_test <- match_df(my_data_frame, cxns, order = "orders", by = NULL)
    },
    "Using dictionary globally across all character/factor columns."
  )
  expect_identical(global_rev_test$raboof, resorted_foo)
  expect_identical(global_rev_test$treatment, resorted_trt)

  skip_if(!cli::is_utf8_output())

  verify_output(path = test_path("cli-messages", "global-errs-2.txt"),
    match_df(my_data_frame, cxns, order = "orders", by = NULL, warn = TRUE),
    crayon = FALSE,
    unicode = FALSE,
  )
})

test_that("global dictionary works with the .global keyword", {
  # gobal dictionary with the .global keyword ---------------------
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "no")
  cxns <- corrections
  cxns$column[6:10] <- ".global"
  cxns$orders[cxns$good == "missing"] <- Inf
  global_wrd_test <- match_df(my_data_frame, cxns, by = "column", order = "orders")
  expect_identical(global_wrd_test$raboof, cleaned_data$raboof)
  expect_identical(global_wrd_test$treatment, resorted_trt)

  # global dictionary with .global and .default throws ------------
  cxns$bad[9] <- ".default"
  expect_error(
    {
      match_df(my_data_frame, cxns, by = "column", order = "orders", warn = TRUE)
    },
    "the .default keyword cannot be used with .global",
    fixed = TRUE
  )
})


test_that("regex matching works as expected", {

  # create dictionary
  d1 <- data.frame(
    val = c("a", "b", "c"),
    replace = c("alpha", "bravo", "charlie"),
    var = rep(".regex ^column_[[:digit:]]", 3),
    stringsAsFactors = FALSE
  )

  d2 <- data.frame(
    val = c("a", "b", "c"),
    replace = c("apple", "banana", "cherry"),
    var = rep("my_column", 3),
    stringsAsFactors = FALSE
  )

  dict <- rbind.data.frame(d1, d2)

  # create data
  df <- data.frame(
    stringsAsFactors = FALSE,
    column_1 = c("a", "a", "b", "b", "c", "c", "b", "b", "b", "b"),
    column_2 = c("b", "b", "b", "c", "b", "a", "a", "b", "a", "b"),
    my_column = c("b", "b", "a", "a", "c", "c", "a", "b", "c", "b"),
    column_xx = c("b", "a", "a", "c", "b", "a", "b", "c", "b", "b")
  )

  # clean
  df_clean <- match_df(df, dict)

  # column_[[:digit:]] cols matched by dictionary d1 (via .regex keyword)
  expect_setequal(df_clean$column_1, d1$replace)
  expect_setequal(df_clean$column_2, d1$replace)

  # my_column matched literally by dictionary d2
  expect_setequal(df_clean$my_column, d2$replace)

  # column_xx not matched by dictionary, so unchanged
  expect_identical(df_clean$column_xx, df$column_xx)


  ### expect warning if a .regex key doesn't match any columns in x
  d3 <- data.frame(
    val = c("a", "b", "c"),
    replace = c("A", "B", "C"),
    var = rep(".regex capitalize", 3),
    stringsAsFactors = FALSE
  )

  dict <- rbind.data.frame(d1, d2, d3)

  expect_warning(match_df(df, dict), "\\.regex capitalize")
})
