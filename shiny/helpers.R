# Helper Functions for DEI Reports ============================================
# Dependencies are only run if debugging this file directly on a console.
if(interactive()) {
  library("jsonlite")
  library("multcomp")
  library("magrittr")
  library("readxl")
  # trim when shipping
  library("tidyverse")
  # load shiny last so it is not masked
  library("shiny")
  library("shinydashboard")
  library("shinydashboardPlus")
}

.FilterData <- function(data, y, x) {
  data %<>%
    dplyr::filter(!is.na(.data[[y]])) %>%
    dplyr::mutate("{x}" := forcats::fct_infreq(.data[[x]])) %>%
    dplyr::mutate("{x}" := fct_lump_min(.data[[x]], min = 3))
  
  if ("Other" %in% levels(data[[x]])) {
    data %>%
      dplyr::mutate("{x}" := fct_relevel(.data[[x]], "Other", after = Inf))
  }
  
  return(data)
}

.Summarize <- function(data, y, x) {
  data %<>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarize(mean = mean(.data[[y]]), n = n())
  
  colnames(data)[[1]] <- "Group"
  
  data %>%
    dplyr::mutate(mean = ifelse(Group == "Other", NA_real_, mean))
}

.QPEstimateEffects <- function(model, x) {
  v <- coef(model)
  names(v) <- sub(x, "", names(v))
  est <- outer(v, v, function(x,y) { round((exp(x-y)-1)*100, digits = 1) })
  tibble::as_tibble(est) %>%
    tibble::add_column(Group = names(v), .before = 1)
}


.TPValues = function(data, y, x) {
  test <- pairwise.t.test(data[[as.character(y)]], data[[x]])
  p <- test$p.value
  last_group <- tail(rownames(p), 1)
  p %>%
    tibble::as_tibble(.) %>%
    tibble::add_row(.before = 1) %>%
    tibble::add_column({{ last_group }} := 0, .after = Inf) %>%
    purrr::map_df(function(x) { coalesce(x, 0) }) %>%
    magrittr::add(t(.)) %>%
    tibble::as_tibble(.) %>%
    tibble::add_column(Group = colnames(.), .before = 1)
}


.DoAnalysis <-  function(data, y, x, n = 3) {
  result <- list(
    error =  FALSE,
    context = y,
    lens = x
  )
  
  data %<>% .FilterData(., y, x)
  
  result$summary <-  .Summarize(data, y, x)
  
  data %<>%
    dplyr::filter(.data[[x]] != "Other")
  
  if(nrow(result$summary) > 1) {
    # Allow for numeric response variable names.
    frm <- stats::as.formula(stringr::str_glue("`{y}` ~ {x} - 1"))
    #model <- glm(frm, binomial, data = data)
    model <- glm(frm, quasipoisson(link = log), data = data)
    effects <- .QPEstimateEffects(model, x)
    pvalues <- .TPValues(data, y, x)
    
    result$effects <- effects
    result$pvalues <- pvalues
  } else {
    result$error = TRUE
    result$message = "Cannot run analysis with only one significant group."
  }
  
  return(result)
  
  # staging for future version of .DoAnalysis
  
}