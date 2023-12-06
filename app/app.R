# UI ####

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
        body {
          max-width: 1500px;
          margin: auto;
        }
        .title {
          padding: 10px;
          background-color: #bbd4ff;
          border: 0px solid #829ecf;
          border-radius: 4px;
          margin-top: 4px;
          margin-bottom: 4px;
        }
        .title > h2 {
          font-weight: bold;
        }
        
        /* Side panel */
        
        .sidepanel {
          margin-bottom: 30px;
        }
        #addr_file_progress {
          display: none;
        }
        .text-small {
          font-size: .85em;
        }
        
        /* Main panel */
        
        .preview-section {
          margin-bottom: 4em;
        }
        .table-a td {
          padding: 2px 4px;
          vertical-align: top;
          border: 1px solid #e3e3e3;
        }
        .modal-header {
          color: red;
        }
        "
      )
    )
  ),
  tags$div(
    class = "title",
    titlePanel("USPS Address Finder")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(
        class = "sidepanel",
        h4("Download address template (CSV)"),
        downloadButton(
          outputId = "addr_temp",
          label = "Download"
        ),
      ),
      tags$div(
        class = "sidepanel",
        h4("Upload your address file"),
        fileInput(
          inputId = "addr_file",
          label = NULL,
          accept = c("text/csv", ".csv")
        )
      ),
      tags$div(
        class = "sidepanel",
        h4("Search addresses on USPS.com"),
        shinyjs::disabled(
          actionButton(
            inputId = "btn1",
            label = "Search USPS"
          )
        )
      ),
      tags$div(
        class = "sidepanel-last",
        h4("Download your results"),
        shinyjs::disabled(
          downloadButton(
            outputId = "addr_results",
            label = "Download"
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Instructions",
          tags$div(
            tags$h3("About"),
            tags$p("This app submits a table of addresses to the", tags$a(href = "https://tools.usps.com/zip-code-lookup.htm?byaddress", "ZIP Code by Address"), "lookup tool at USPS.com. Upload your addresses, search them rapidly at USPS.com, and download your results from the side panel."),
            tags$p("The results file will contain any matches to your submitted addresses that are found in the USPS database. Matched addresses are returned in a standardized format along with a DPV code (learn more about this on the DPV codes tab)."),
            tags$p("A preview of your address file and the search results will appear in the Preview data tab. To clear your data and start a new search, refresh the page."),
            tags$h3("Address file"),
            tags$h4("Requirements"),
            tags$ul(
              tags$li("All personally identifying information (i.e., anything that could link an address to a person) must be removed before uploading."),
              tags$li("It must be formatted as a CSV file."),
              tags$li("The first row must have the column names as shown in the example below (case sensitive).")
            ),
            tags$table(
              class = "table-a",
              tags$tr(tags$td("1"), tags$td("street"), tags$td("unit"), tags$td("city"), tags$td("state"), tags$td("zip")),
              tags$tr(tags$td("2"), tags$td("1000 SW Jackson"), tags$td("Ste 120"), tags$td("Topeka"), tags$td("KS"), tags$td("66612"))
            ),
            br(),
            tags$p("To be searched, an address needs to have values in, at minimum, 1) the street and zip columns, or 2) the street, city, and state columns."),
            tags$h4("Row ID"),
            tags$p("A unique row identifier (row ID) is not required for the uploaded address file, but it may help you when matching the results to the original addresses, especially when multiple addresses are returned for a single address. In case no row ID is present, the results file will contain the columns n_row_src (corresponding to the address order in the uploaded file) and n_result (corresponding to the result(s) returned for each searched address) to help with matching."),
            tags$p("If the source file contains only one column in addition to the required columns, this column will be treated as a row identifier and will be included in the results table. If more than one additional column is present, all additional columns will be ignored and omitted from the results table."),
            tags$h4("File template"),
            tags$p("A preformatted CSV template for your addresses is available to download from the side panel.")
          ),
        ),
        tabPanel(
          "Preview data",
          tags$div(
            br(),
            tags$p(em("Preview your address file and results here."))
          ),
          tags$div(
            class = "preview-section",
            htmlOutput("header1"),
            DT::dataTableOutput("table1")
          ),
          tags$div(
            class = "preview-section",
            htmlOutput("header2"),
            DT::dataTableOutput("table2")
          )
        ),
        tabPanel(
          "DPV codes",
          tags$h3("Delivery Point Validation"),
          tags$p("The Delivery Point Validation (DPV) Confirmation Indicator is provided by the USPS to show if an address, or part of an address, is deliverable. In the definitions below, the primary number precedes the street name, and the secondary number follows it (e.g., a unit or lot number)."),
          tags$p("Code definitions:"),
          tags$table(
            class = "table-a",
            tags$tbody(
              tags$tr(
                tags$td("Y"),
                tags$td("The address is validated for the primary and (if present) secondary number.")
              ),
              tags$tr(
                tags$td("D"),
                tags$td("Only the primary number is validated. The secondary number is missing.")
              ),
              tags$tr(
                tags$td("S"),
                tags$td("Only the primary number is validated. The secondary number provided is unconfirmed.")
              ),
              tags$tr(
                tags$td("N"),
                tags$td("The address is not validated.")
              )
            )
          )
        )
      )
    )
  )
)



# Server ####

server <- function(input, output) {
  source("fn.R", local = TRUE)
  
  # Template table download
  output$addr_temp <- downloadHandler(
    filename = function() {
      "address_template.csv"
    },
    content = function(file) {
      file.copy("address_template.csv", file)
    },
    contentType = "text/csv"
  )

  # Uploaded table reactive
  df_in <- reactive({
    req(input$addr_file)
    df <- read.csv(input$addr_file$datapath, colClasses = "character")
    df
  })

  # Enable "Search" button
  observeEvent(df_in(), {
    if (is.data.frame(df_in())) {
      shinyjs::enable("btn1")
    }
  })

  # Uploaded table output
  output$table1 <- DT::renderDataTable({
    req(is.data.frame(df_in()))
    DT::datatable(df_in(), options = list(pageLength = 5))
  })

  # Uploaded table header
  output$header1 <- renderText({
    req(is.data.frame(df_in()))
    HTML("<h3>Source file</h3>")
  })

  # Searched table reactive
  df_out <- eventReactive(input$btn1, {
    req(is.data.frame(df_in()))
    df1 <- read.csv(input$addr_file$datapath, colClasses = "character")
    df2 <- tryCatch(usps_lookup_shiny(df1),
      error = function(c) {
        conditionMessage(c)
      }
    )
    if (!is.data.frame(df2)) {
      showModal(modalDialog(
        title = "Yikes.",
        "There was a problem with your table:",
        df2,
        easyClose = T
      ))
    }
    df2
  })

  # Enable "Download" button
  observeEvent(df_out(), {
    if (is.data.frame(df_out())) {
      shinyjs::enable("addr_results")
    }
  })

  # Searched table output
  output$table2 <- DT::renderDataTable({
    req(is.data.frame(df_out()))
    DT::datatable(df_out(),
      extensions = "Buttons",
      options = list(
        pageLength = 5,
        rownames = F,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      )
    )
  })

  # Searched table header
  output$header2 <- renderText({
    req(is.data.frame(df_out()))
    HTML("<h3>Results from USPS</h3>")
  })

  # Searched table download
  output$addr_results <- downloadHandler(
    filename = function() {
      paste0("usps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(df_out(), file, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)
