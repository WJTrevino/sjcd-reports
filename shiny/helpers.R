# Helper Functions for DEI Reports ============================================
# Dependencies are only run if debugging this file directly on a console.
if(interactive()) {
  library(shiny)
  library(jsonlite)
  library(magrittr)
  library(readxl)
  # trim when shipping
  library(tidyverse)
  # load shiny last so it is not masked
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
}

.Summarize <- function(data, y, x) {
  data %<>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarize(mean = mean(.data[[y]]), n = n())
  
  colnames(data)[[1]] <- "group"
  
  data %>%
    dplyr::mutate(mean = ifelse(group == "Other", NA_integer_, mean))
}

.QPEstimateEffects <- function(model, x) {
  v <- coef(model)
  names(v) <- sub(x, "", names(v))
  est <- outer(v, v, function(x,y) (exp(x-y)-1)*100)
  tibble::as_tibble(est) %>%
    tibble::add_column(group = names(v), .before = 1)
}


.TPValues = function(data, x, y) {
  test <- pairwise.t.test(data[[x]], data[[y]])
  p <- test$p.value
  last_group <- rownames(p)[-1]
  p %<>%
    tibble::as_tibble(.) %>%
    tibble::add_column(group = row.names(p), .before = 1) %>%
    tibble::add_row(group = colnames(.)[2], .before = 1) %>%
    tibble::add_column({{ last_group }} := NA, .after = Inf)
}


.DoAnalysis <-  function(data, y, x, n = 3) {
  result <- list(
    error =  FALSE,
    context = y,
    lens = x
  )
  
  data %<>%
    dplyr::filter(!is.na(.data[[y]])) %>%
    dplyr::mutate("{x}" := forcats::fct_infreq(.data[[x]])) %>%
    dplyr::mutate("{x}" := fct_lump_min(.data[[x]], min = 3)) %>%
    # FIND A WAY TO MOVE OTHER TO END WITHOUT ERROR
    dplyr::mutate("{x}" := fct_relevel(.data[[x]], "Other", after = Inf))
  
  result$summary <-  .Summarize(data, y, x)
  
  data %<>%
    dplyr::filter(.data[[x]] != "Other")
  
  if(nrow(result[["summary"]]) > 1) {
    frm <- reformulate(x, response = y, intercept = FALSE)
    model <- glm(frm, quasipoisson(link =), data = data)
    effects <- .QPEstimateEffects(model, x)
    pvalues <- .TPValues(data, y, x)
    
    result$effects <- effects
    result$pvalues <- pvalues
  } else {
    result$error = TRUE
    result$message = "Cannot run analysis with only one significant group."
  }
  
  result
}