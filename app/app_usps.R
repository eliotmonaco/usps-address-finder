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
        
        .col-sm-4 > form {
          border-width: ;
        }
        .form-group.shiny-input-container, #file1_progress {
          margin-bottom: 0!important;
        }
        #btn1 {
          margin-bottom: 20px;
        }
        .sidepanel {
          margin-bottom: 15px;
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
          border: 1px solid #e3e3e3
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
      width = 4,
      tags$div(
        class = "sidepanel",
        h4("Upload an address file"),
        # tags$p(class = "text-small", "*Anonymized data only"),
        fileInput(
          inputId = "file1",
          label = NULL,
          # label = "CSV",
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
        h4("Download results"),
        shinyjs::disabled(
          downloadButton(
            outputId = "down1",
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
            tags$h3("How to use"),
            tags$p("This app submits a table of addresses to the", tags$a(href = "https://tools.usps.com/zip-code-lookup.htm?byaddress", "ZIP Code by Address"), "lookup tool at USPS.com. Upload your addresses, search them rapidly at USPS.com, and download your results from the lefthand panel."),
            tags$p("Previews of your source file and the search results will appear in the Data Preview tab. To clear your data and start over, refresh the page."),
            tags$h3("Source file"),
            tags$table(
              class = "table-a",
              tags$tr(
                tags$td("1"), tags$td("street"), tags$td("unit"), tags$td("city"), tags$td("state"),
                tags$td("zip")
              ),
              tags$tr(tags$td("2"), tags$td(""), tags$td(""), tags$td(""), tags$td(""), tags$td(""))
            ),
            tags$h4("Requirements"),
            tags$ul(
              tags$li("CSV format."),
              tags$li("A first row with column names as shown in the example above."),
              tags$li("Any personally identifying information (i.e., anything that could link an address to a person) removed before uploading.")
            ),
            tags$p("To be searched, and address needs values in, at minimum, 1) both street and zip, or 2) all of street, city, and state."),
            tags$h4("Row ID"),
            tags$p("Individual addresses submitted to USPS can return multiple results, all of which are included in the results table. Columns (n_row_src and n_result) are added to help you to match each result to the submitted address. However, it's a good idea to include your own unique row identifier variable to make matching easier."),
            tags$p("If the source file contains only one column in addition to the required columns, it will be treated as a row identifier and included in the results table. If more than one additional column is present, all additional columns will be ignored and omitted from the results table."),
            tags$h4("Download template"),
            tags$p("Click to download a pre-formatted file for your addresses."),
            downloadButton("down2", label = "Download"),
            tags$h3("Results"),
            tags$p("Each result contains the following information:"),
            tags$ul(
              tags$li("The source file row number of the address that was submitted (n_row_src). Note: the header row is not counted."),
              tags$li("The number of the result returned by USPS (n_result)."),
              tags$li("The address returned by USPS in standardized format (street, city, state, zip4, zip5, and county)."),
              tags$li("A Delivery Point Validation code (more about this in the DPV Codes tab).")
            )
          ),
        ),
        tabPanel(
          "Data Preview",
          tags$div(
            br(),
            tags$p(em("Preview your source file and results here."))
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
          "DPV Codes",
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
        )#,
        # tabPanel(
        #   "About",
        #   tags$h3("About"),
        #   tags$p(
        #     "This app was created to speed up the process of validating addresses before geocoding.",
        #     "If you have suggestions for improvement, or if you notice any problems with the app, please",
        #     "contact the developer."
        #   )
        # )
      )
    )
  )
)



# Server ####

server <- function(input, output) {
  source("func_address_scrapery.R", local = T)

  # Template table download
  output$down2 <- downloadHandler(
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
    req(input$file1)
    df <- read.csv(input$file1$datapath, colClasses = "character")
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
    df1 <- read.csv(input$file1$datapath, colClasses = "character")
    df2 <- tryCatch(usps_df_all(df1),
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
      shinyjs::enable("down1")
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
  output$down1 <- downloadHandler(
    filename = function() {
      paste0("usps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(df_out(), file, row.names = F)
    }
  )
}

shinyApp(ui = ui, server = server)
