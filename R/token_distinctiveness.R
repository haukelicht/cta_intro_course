require(quanteda, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(purrr, quietly = TRUE)


#' Get most distinctive tokens by group
#'
#' @param x named vector
#' @param groups character vector with values of groups to compare
#' @param .n integer (default 20)
get_top_n_terms <- function(x, groups, .n = 20) {
  a <- head(sort(x, decreasing = TRUE), n = .n)
  a <- tibble::enframe(a, name = "feature")
  a["group"] <- groups[[1]]
  
  b <- head(sort(x, decreasing = FALSE), n = .n)
  b <- tibble::enframe(b, name = "feature")
  b["group"] <- groups[[2]]
  
  dplyr::bind_rows(a, b)
}



#' Monroe et la. (2008) "Figthin' words" implementation
#' @note Figthin' words is a model-based feature selection and evaluation method
#' \quote{
#'   With feature evaluation, the goal is to quantify our information about different features.
#'   We want to know, for example, the extent to which each word is used differently by two political parties.
#' 
#' }
#' 
#' @export
textstat_fighting_words <- function(
    x
    , group.var 
    , prior = 0.1
    , .comparison = c("pairwise", "one-against-all")
    , .pairs = c("combinations", "permutations")
) {
    stopifnot(
        "`group.var` must be a unit-length character vector" = is.atomic(group.var) && is.character(group.var) && length(group.var) == 1 && !is.na(group.var)
        , "`prior` must be a non-zero scalar/vector" = is.atomic(prior) && is.numeric(prior) && all(prior > 0)
    )
    UseMethod("textstat_fighting_words", x)
} 

textstat_fighting_words.default <- function(x, ...) {
    stop("`textstat_fighting_words()` not implemented for objects of class ", class(x)[1])
}



#' @param prior Defaults to .1 (i.e. an uninformative prior).
textstat_fighting_words.dfm <- function(
  x
  , group.var
  , prior = 0.1
  , .comparison = "pairwise"
  , .pairs = "combinations"
) {
  stopifnot(
    "`group.vars` not in `x`'s document vars (check `docvars(x)`)" = group.var %in% names(x@docvars)
    , "length of `prior` must be 1 or equal to `nfeat(x)`" = length(prior) %in% c(1L, nfeat(x))
    , "`.comparison` must be 'pairwise' (one-against-all comparison not yet implemented)" = is.character(.comparison) && length(.comparison) == 1L && .comparison == "pairwise"
    , "`.pairs` must be 'combinations' or 'permutations'" = is.character(.pairs) && length(.pairs) == 1L && .pairs %in% c("combinations", "permutations")
  )
  
  n_feats <- nfeat(x)
  
  # priors (prior word counts): alpha hyper-parameters passed to pi ~ Dirichlet(alpha)
  if (length(prior) > 1) {
    priors <- if (!is.null(names(prior))) prior[featnames(x)] else prior
  } else {
    priors <- rep(prior, n_feats)
  }

  a0 <- sum(priors)
  
  # aggregate counts by group
  x <- dfm_group(x, x@docvars[[group.var]])
  gc() 
  
  # get group indicator
  groups <- docid(x)
  n <- rowSums(x)
  
  x <- as.matrix(x)
  gc() 
  
  # compute z-scores (equation 14 in Monroe et al., 2008)
  terms <- log((x + priors) / (n + a0 - x - priors))
  
  if (.comparison == "pairwise") {
    pairings <- combn(as.character(groups), 2) # grps <- pairings[,1]
    res <- apply(
      X = pairings
      , MARGIN = 2
      , FUN = textstat_fighting_words_pairwise
      # `apply` args
      , simplify = FALSE
      # additional args passed to `textstat_fighting_words_pairwise`
      , grp.var = group.var
      , grp.var.lvls = levels(groups)
      , .x = x
      , .terms = terms
      , .priors = priors
    )
    names(res) <- apply(pairings, 2, paste, collapse = "-")
    
    if (inherits(res[[1]], "pairwise.zscores") && .pairs[1] == "permutations") {
      for (grp in names(res)) {
        splitted <- strsplit(grp, "-")[[1]]
        tmp <- paste(rev(splitted), collapse = "-")
        if (tmp %in% names(res))
          next
        
        res[[tmp]] <- res[[grp]]
        res[[tmp]]$z_score[] <- -res[[tmp]]$z_score
        res[[tmp]]$delta[] <- -res[[tmp]]$delta
        res[[tmp]]$group <- factor(res[[tmp]]$group, c("reference", "target"), c("target", "reference"))
        names(res[[tmp]])[7:8] <- c("n_reference", "n_target")
        res[[tmp]][] <- res[[tmp]][names(res[[grp]])]
      }
    }
    
    
  }
  
  # return
  class(res) <- c(class(res[[1]])[1], "textstat", "list")
  attr(res, "group.var") <- group.var
  attr(res, "groups") <- levels(groups)
  
  return(res)
}

#' @param grps a character vector with two elements determining which groups to compare.
#'      It is _assumed_ that `grps` is in `grp.var.lvls` (i.e. `all(grps %in% grp.var.lvls)` must be `TRUE`).
#' @param grp.var a unit-length character vector providing the name of the group variable.
#' @param grp.var.lvls a character vector with at least as many elements as `grps` (i.e. $\geq$ 2)
#'      Used to assign levels to the `grp.var` factor variable contained in the return object.
#' @param .x a `matrix` of feature counts in format group &times; feature.
#' @param .terms a `matrix` object resulting from equation 14 in Monroe et al. (2008).
#'      Columns must be named like features, rows according to `grp.var.lvls`.
#' @param .priors a vector of positive (non-zero) values used to 
#'      compute estimates (approximate) variance (according to equation 20 in Monroe et al., 2008).
#'      Must be as long as `.terms` has columns.
#'      
#' @returns a `pairwise.zscores` bject (inheriting from `data.frame`) with columns
#'   \itemize{
#'      \item{\code{feature} (chr): feature names} 
#'      \item{\code{z_score} (dbl): estimated z-score ($\hat{\zeta}_{\cdot}^(i-j)$)} 
#'      \item{\code{delta} (dbl): difference between estimated $\hat{\pi}_{\cdot}$s ($\hat{\delta}_{\cdot}^(i-j)$)} 
#'      \item{\code{var} (dbl): estimated variance of \code{delta} ($\sigma^2\left(\hat{\delta}_{\cdot}^(i-j)\right)$)} 
#'      \item{\code{group} (fct): a group indicator, "target" for \code{grp[1]} and "reference" for \code{grp[2]}}
#'      \item{named like \code{grp.var} (fct): a group indicator with values like \code{grp} and levels like \code{grp.var.lvls}} 
#'      \item{\code{n_target} (int): feature count for target group (i.e. \code{grp[1]})} 
#'      \item{\code{n_reference} (int): feature count for reference group (i.e. \code{grp[2]})} 
#'   }      
textstat_fighting_words_pairwise <- function(grps, grp.var, grp.var.lvls, .x, .terms, .priors) {
  # message(grps)
  stopifnot(
    "`grps` must be a character vector with two non-NA elements" = is.atomic(grps) && is.character(grps) && length(grps) == 2L && !any(is.na(grps))
    , "`grp.var` must be a unit-length, non-NA character vector" = is.atomic(grp.var) && is.character(grp.var) && length(grp.var) == 1L && !is.na(grp.var)
    , "`grp.var.lvls` must be a character vector with only non-NA elements" = is.atomic(grp.var.lvls) && is.character(grp.var.lvls) && !any(is.na(grp.var.lvls))
    , "all elemets of `grps` must be in `grp.var.lvls" = all(grps %in% grp.var.lvls)
    , "`.x` must be a matrix object" = is.atomic(.x) && inherits(.x, "matrix") 
    , "`.x` must have as many rows as `grp.var.lvls` has elements" = nrow(.x) == length(grp.var.lvls)
    , "all names of rows in `.x` must be in `grp.var.lvls`" = !is.null(rownames(.x)) && all(rownames(.x) %in% grp.var.lvls)
    # , "columns of `.terms` must be named" = !is.null(colnames(.x))
    , "`.x` must have as many columns as `.priors` has elements" = ncol(.x) == length(.priors)
    , "`.terms` must be a matrix object" = is.atomic(.terms) && inherits(.terms, "matrix") 
    , "`.terms` must have as many rows as `grp.var.lvls` has elements" = nrow(.terms) == length(grp.var.lvls)
    , "all names of rows in `.terms` must be in `grp.var.lvls`" = !is.null(rownames(.terms)) && all(rownames(.terms) %in% grp.var.lvls)
    , "columns of `.terms` must be named" = !is.null(colnames(.terms))
    , "`.terms` must have as many columns as `.priors` has elements" = ncol(.terms) == length(.priors)
  )
  
  # equ 16 in Monroe et al.
  deltas <- .terms[grps[1], ] - .terms[grps[2], ]
  # equ 20 (approximate variance)
  vars <- colSums((.x[grps, ] + .priors)^-1)
  # equ 22 (z-scores)
  z_scores <- deltas/sqrt(vars)
  
  # combine estimates
  res <- data.frame(
    feature = dimnames(.terms)[[2]]
    , z_score = z_scores
    , delta = deltas
    , var = vars
  )
  
  # add target/reference indicator
  res$group <- factor(if_else(res$z_score >= 0, "target", "reference"), levels = c("target", "reference"))
  # add actual group variable column
  res[[grp.var]] <- factor(if_else(res$group == "target", grps[1], grps[2]), levels = grp.var.lvls)
  # add group-wise feature counts
  res[7:8] <- setNames(as.data.frame.matrix(t(.x[grps, ])), paste0("n_", c("target", "reference")))
  # remove row names
  rownames(res) <- NULL
  
  # declare class
  class(res) <- c("pairwise.zscores", class(res))
  
  # return
  return(res)
}


plot_scores <- function(
    x, 
    feature.var = "feature", 
    value.var = "value", 
    group.var = "group", 
    xlab = "Score",
    ncol = 2
) {
  x_lims_ <- max(abs(x[[value.var]]), na.rm = TRUE) * c(-1.05, 1.05)
  ggplot(
    data = x,
    mapping = aes(
      y    = reorder(.data[[feature.var]], abs(.data[[value.var]])),
      x    = .data[[value.var]],
      fill = .data[[group.var]]
    )
  ) +
    geom_col() + xlim(x_lims_) +
    facet_wrap(
      vars(.data[[group.var]]), 
      ncol = ncol, 
      scales = "free_y", 
      space = ifelse(ncol==1, "free_y", "fixed")
    ) +
    labs(y = NULL, x = xlab) +
    theme(legend.position = "none")
}
