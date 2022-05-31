# Helper Functions for DEI Reports ============================================
# Dependencies are only run if debugging this file directly on a console.
if(interactive()) {
  library("jsonlite")
  library("multcomp")
  library("emmeans")
  library("magrittr")
  library("readxl")
  # trim when shipping
  library("tidyverse")
  # load shiny last so it is not masked
  library("shiny")
  library("shinydashboard")
  library("shinydashboardPlus")
}

.Summarize <- function(data, y, x) {
  data %<>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarize(mean = mean(.data[[y]]), n = dplyr::n())
  
  colnames(data)[[1]] <- "Group"
  
  data %<>%
    dplyr::mutate(mean = ifelse(Group == "Other", NA_real_, mean)) %>%
    return(.)
}

.MakeMatrix <- function(groups, lower, upper = NULL) {
  stopifnot(
    is.character(groups),
    is.numeric(lower)
  )
  if(is.null(upper)) upper <- lower
  l <- length(groups)
  m <- matrix(0, l, l)
  dimnames(m) <- list(groups, groups)
  m[lower.tri(m)] <- lower
  m[upper.tri(m)] <- upper
  m %>%
    tibble::as_tibble(., rownames = NA) %>%
    tibble::rownames_to_column(var = "Group") %>%
    return(.)
}

# .DoAnalysis will eventually return an object for ALL x variables in the
# given y variable, so the function signature will be (data, y, n = 3)
.DoAnalysis <-  function(data, y, x, n = 3) {
  result <- list(
    error =  FALSE,
    context = y,
    lens = x
  )
  
  # filter missing observations
  data %<>% dplyr::filter(is.numeric(.data[[y]]) & !is.na(.data[[y]]))
  result$summary <- .Summarize(data, y, x)
  data %<>%
    dplyr::filter(.data[[x]] != "Other") %>%
    droplevels(.)
  
  # trim any variables that do not have at least 2 levels.
  all_deps <- c("race", "gender", "FT", "FG", "pell")
  valid_dep <- all_deps %>%
    purrr::map(~ nlevels(data[[.x]])) %>%
    magrittr::is_greater_than(1)
  deps <- all_deps[valid_dep]
  
  formula <- paste(paste0("`", y, "`"), "~", paste(deps, collapse = " + "))
  formula %<>% stats::as.formula(.)
  
  if(x %in% deps) {
    # fit combined model
    if (y == "Total") {
      model <- stats::glm(formula, quasipoisson(link = "log"), data = data) 
    } else {
      model <- stats::glm(formula, binomial(link = "logit"), data = data) 
    }
    
    # perform Tukey contrasts with emmeans
    contrasts <- emmeans::emmeans(model, as.character(x)) %>%
      emmeans::contrast(
        .,
        method = "pairwise",
        ratios = TRUE,
        type = "response") %>%
      tibble::as_tibble(.)
    
    # report the results list as a matrix of comparisons
    if (y == "Total") {
      # from ratio to percent-change
      effects <- purrr::map_dbl(contrasts$ratio, ~ 100 * (.x - 1))
      effects_rev <- purrr::map_dbl(contrasts$ratio, ~ 100 * (1/.x - 1))
      notes <- "Results are percentage change in total score between groups."
    } else {
      # from log(odds ratio) to odds ratio
      effects <- round(contrasts$odds.ratio, digits = 1)
      effects_rev <- purrr::map_dbl(effects, ~ -1 * .x)
      notes <- paste("Results are how many times more or less",
        "likely to answer correctly between groups.")
    }
    
    groups <- levels(data[[x]])
    effects <- .MakeMatrix(groups, effects, effects_rev)
    pvalues <- .MakeMatrix(groups, round(contrasts$p.value, digits = 4))
    
    result$effects <- effects
    result$pvalues <- pvalues
    result$notes <- notes
  } else {
    result$error = TRUE
    result$message = paste("Cannot run analysis with only one",
      "significant group or where all outcomes are the same.")
  }
  return(result)
}