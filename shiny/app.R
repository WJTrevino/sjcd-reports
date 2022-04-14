# Load Dependencies ==========================================================
library(shiny)
library(jsonlite)
library(magrittr)
library(readxl)
# trim when shipping
library(tidyverse)

# UI Definition ==============================================================
ui <- fluidPage(
  title = "EAC DEI Report",
  sidebarPanel(
    h3("EAC DEI Report"),
    # fileInput("file", "EAC Spreadsheet", accept = ".xlsx"),
    textInput("file","Mock file upload (File path)"),
    conditionalPanel(
        condition = "input.file",
        conditionalPanel(
          condition = "output.dataValid == '1'",
          hr(),
          h4("About This Exam"),
          div("Name: ", textOutput("examName", inline = TRUE)),
          div("Students: ", textOutput("nStudents", inline = TRUE)),
          div("Questions: ", textOutput("nQuestions", inline = TRUE)),
          div("Questions: ", textOutput("nGoals", inline = TRUE)),
          actionButton(
            "doAnalysis",
            "Run Analysis",
            icon = icon("calculator")
          )
        )
    )
  ),
  mainPanel(
    textOutput("file"),
    verbatimTextOutput("sqlData"),
    verbatimTextOutput("joinedData"),
    verbatimTextOutput("helper")
  )
)

# Server Definition ==========================================================
server <- function(input, output, session) {
  data <- reactive({
    # req(input$file$datapath)
    req(input$file)
    
    sheetNames = c(
      "Test Info",
      "Courses Included",
      "Summary Statistics",
      "Item Analysis",
      "Student Questions",
      "Goals Summary"
    )
    progress <- shiny::Progress$new(max = length(sheetNames))
    on.exit(progress$close())
    
    tryCatch({
      sheetNames %>%
        set_names() %>%
        map(function(x) {
          progress$inc(amount = 1, message = "Processing Sheets")
        # map(read_xlsx, path = input$file$datapath, .name_repair = "minimal")
          read_xlsx(
            x,
            path = input$file,
            na = c("--"),
            .name_repair = "minimal"
          )
        })
    },
    warning = function(e) {
      NA # Silently swallow error.
    },
    error = function(e) {
      NA # Silently swallow error.
    })
  })
  
  validateData <- reactive({
    validate(
      need(input$file, "Waiting for file.")
    )
    d <- data()
    m <- "Invalid Formatting"
    validate(
      need(d$'Summary Statistics'[[2,1]] == "Scorable Questions", m),
      need(d$'Summary Statistics'[[2,2]] > 0, m)
    )
    "Valid"
  })

  dataValid <- reactive({
    ifelse(validateData() == "Valid", TRUE, FALSE)
  })
  output$dataValid <- renderText({
    ifelse(dataValid(), "1", "0")
  })
  
  sqlData <- reactive({
    read_json("../secrets/test.json", simplifyVector = TRUE) %>% tibble
  })
  output$sqlData <- renderPrint({ sqlData() })
  
  joinedData <- reactive({
    d <- req(data()$'Student Questions')
    s <- req(sqlData())
    left_join(d, s, by = c("Student_id" = "sid")) %>%
      purrr::modify_if(is.character, as.factor) %>%
      purrr::modify_at("Student_id", as.character) %>%
      dplyr::mutate(
        race = forcats::fct_other(
          race, drop = "Unknown or Not Reported")) %>%
      dplyr::mutate(pell = forcats::fct_other(pell, drop = "Unkn"))
  })
  output$joinedData <- renderPrint({ joinedData() })

  output$examName <- renderText({
    req(dataValid())
    d <- data()
    sub(" (**Webcam**) - Requires Respondus LockDown Browser",
        "",
        d$`Test Info`[1,1],
        fixed = TRUE)
  })
  
  output$nQuestions = renderText({
    req(dataValid())
    d <- data()
    d$'Summary Statistics'[[2,2]]
  })
  
  output$file <- renderText({
    input$file
  })
  
  output$helper <- renderPrint({
    .DoAnalysis(joinedData(), "Total", "race")
  })
}

# Embedded Helper Functions ==================================================

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

# Return App Object ==========================================================
shinyApp(ui = ui, server = server)