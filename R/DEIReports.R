# Package Dependencies and Declarations ======================================



# Helper Functions ===========================================================
.TrimData <- function(data) {}

.Summarize <- function(data, col) {
  data <- data %>%
    group_by(.data[[col]]) %>%
    summarize(mean = mean(score), n = n()) %>%
    arrange(desc(n))
    
  colnames(data)[[1]] <- "group"
  
  data %>%
    mutate(mean = ifelse(group == "Other", NA_integer_, mean))  # mask "Other"
}

.EstimateEffects <- function(model) {
  v <- coef(model)
  est <- outer(v, v, function(x,y) (exp(x-y)-1)*100)
}


.TPValues = function(x, y) {
  test = pairwise.t.test(x, y)
  p = test$p.value
  as_tibble(p) %>%
    add_column(Group = row.names(p), .before = 1)
}


.DoAnalysis <-  function(data, y, x, n = 3) {
  data <- data %>%
    mutate_at(x, ~ fct_infreq(.)) %>%
    mutate_at(x, ~ fct_lump_min(., min = 3))
  
  summary <-  .Summarize(data,x)
  groups <- summary[[1]]
  
  data <- data %>%
    filter(.data[[x]] != "Other")
  
  frm <- reformulate(x, response = y, intercept = FALSE)
  model <- glm(frm, quasipoisson, data = data)
  list(groups = summary,
       effects = .EstimateEffects(model),
       pvalues = .PValues(data[[y]], data[[x]]))
}


DEIReports <- function(str) {
  library(tidyverse)
  library(dplyr)
  library(magrittr)
  data <- dplyr::as_tibble(jsonlite::fromJSON(str))
  
  # Import Data --------------------------------------------------------------
  
  data <- data %>%
    modify_if(is.character, as.factor) %>%
    modify_at("sid", as.character) %>%
    unnest_wider(responses, names_sep = "_") %>%
    mutate(race = fct_other(race, drop = "Unknown or Not Reported")) %>%
    mutate(pell = fct_other(pell, drop = "Unkn"))

  num_items = data %>%
    select(starts_with("responses")) %>% 
    tally() %>%
    as.integer()
  
  # Trim Data ----------------------------------------------------------------
  
  
  # Return Values ------------------------------------------------------------
  
  .DoAnalysis(data,"score","race")
}