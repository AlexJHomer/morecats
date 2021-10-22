#' Lump together infrequent levels
#'
#' This function is a variant of `fct_lump_infreq` from forcats, but instead of
#' lumping together levels such that the "Other" category is smaller than all
#' others, it lumps together levels such that the "Other" category is smaller
#' than the most frequent level.
#'
#' As you might imagine, this function borrows heavily from the source code from
#' forcats for `fct_lump_infreq`, and is used with the appropriate permission.
#'
#' @param f A factor
#' @param other_level The name to use for the "Other" level.  Defaults to
#'   `"Other"`.
#' @return A reordered factor
#' @export
#' @examples
#' x <- factor(rep(LETTERS[1:9], times = c(40, 10, 5, 27, 1, 1, 1, 1, 1)))
#' x %>% table()
#' x %>% fct_lump_lowfreq() %>% table()
#' # (Borrowed from forcats.)
fct_lump_lowfreq2 <- function (f, other_level = "Other") {
  calcs <- check_calc_levels(f)
  f <- calcs$f
  new_levels <- ifelse(!in_smallest2(calcs$count), levels(f),
                       other_level)
  if (other_level %in% new_levels) {
    f <- forcats::lvls_revalue(f, new_levels)
    forcats::fct_relevel(f, other_level, after = Inf)
  }
  else {
    f
  }
}

check_calc_levels <- function(f) {
  f <- check_factor(f)
  count <- as.vector(table(f))
  total <- length(f)
  list(f = f, count = count, total = total)
}


check_factor <- function(f) {
  if (is.character(f)) {
    factor(f)
  } else if (is.factor(f)) {
    f
  } else {
    stop("`f` must be a factor (or character vector).", call. = FALSE)
  }
}

# Lump together smallest groups, ensuring that the collective
# "other" is still smaller than the largest groups. Assumes x
# is vector of counts in descending order
lump_cutoff2 <- function(x) {
  left <- sum(x)

  for (i in seq_along(x)) {
    # After group, there are this many left
    left <- left - x[i]

    if (x[1] > left)
      return(i + 1)
  }

  length(x) + 1
}

# Given vector of counts, returns logical vector if in
# smallest groups
in_smallest2 <- function(x) {
  ord_x <- order(x, decreasing = TRUE)
  idx <- lump_cutoff2(x[ord_x])

  to_lump <- seq_along(x) >= idx
  # Undo initial ordering
  to_lump[order(ord_x)]
}
