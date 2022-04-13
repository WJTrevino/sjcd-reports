# Helper Functions ===========================================================

.Summarize <- function(data, y, x) {
  data %<>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarize(mean = mean(.data[[y]]), n = n()) %>%
    dplyr::arrange(desc(n))
  
  colnames(data)[[1]] <- "group"
  
  data %>%
    dplyr::mutate(mean = ifelse(group == "Other", NA_integer_, mean))  # mask "Other"
}

.QPEstimateEffects <- function(model, x) {
  v <- coef(model)
  names(v) <- sub(x, "", names(v))
  est <- outer(v, v, function(x,y) (exp(x-y)-1)*100)
  tibble::as_tibble(est) %>%
    tibble::add_column(group = names(v), .before = 1)
}


.TPValues = function(x, y) {
  test = pairwise.t.test(x, y)
  p = test$p.value
  tibble::as_tibble(p) %>%
    tibble::add_column(group = row.names(p), .before = 1)
}


.DoAnalysis <-  function(data, y, x, n = 3) {
  result <- list(
    error =  FALSE,
    context = y,
    lens = x
  )

  data %<>%
    dplyr::filter(!is.na(.data[[y]])) %>%
    dplyr::mutate(x = fct_infreq(x)) %>%
    dplyr::mutate(x = fct_lump_min(x, min = 3))

  result[["summary"]] <-  .Summarize(data, y, x)

  data %<>%
    dplyr::filter(.data[[x]] != "Other")

  if(nrow(result[["summary"]]) > 2) {
    frm <- reformulate(x, response = y, intercept = FALSE)
    model <- glm(frm, quasipoisson, data = data)
    result[["effects"]] <- .QPEstimateEffects(model, x)
    result[["pvalues"]] <- .TPValues(data[[y]], data[[x]])
  } else {
    result[["error"]] = TRUE
    result[["message"]] = "Cannot run analysis with one group."
  }

  # list(
  #   summary = summary,
  #   effects = .QPEstimateEffects(model, x),
  #   pvalues = .TPValues(data[[y]], data[[x]]),
  #   context = y,
  #   lens = x
  # )
  result
}


DEIReports <- function(str) {
  # # library(tidyverse)
  # library(dplyr)
  # library(tidyr)
  # library(tibble)
  # library(purrr)
  # library(forcats)
  # library(stringr)
  # library(magrittr)
  # library(jsonlite)
  
  # Import Data --------------------------------------------------------------
  
  data <- jsonlite::fromJSON(str) %>%
    dplyr::as_tibble() %>%
    purrr::modify_if(is.character, as.factor) %>%
    purrr::modify_at("sid", as.character) %>%
    tidyr::unnest_wider(responses, names_sep = "_") %>%
    dplyr::mutate(
      race = forcats::fct_other(
        race, drop = "Unknown or Not Reported")) %>%
    dplyr::mutate(pell = forcats::fct_other(pell, drop = "Unkn"))
  
  # Perform Analysis ---------------------------------------------------------
  
  num_items <- data %>%
    dplyr::select(starts_with("responses")) %>% 
    ncol()

  pad_length <- nchar(num_items)
  contexts <- 1:num_items %>%
    sapply(., function(x) {
      v <- paste0("responses_", x);
      stringr::str_pad(v, pad_length, "left")
    })
  contexts <- c("score", contexts)
  
  lenses <- c("race", "gender","status","fg","pell")
  
  analysis = list()
  for (context in contexts) {
    this_context = list()
    for (lens in lenses) {
      print(paste("Now:", context, "~", lens))
      this_context[[lens]] <- .DoAnalysis(data, context, lens)
    }
    analysis[[context]] <- this_context
  }
  
  # Return Values ------------------------------------------------------------

  analysis
}