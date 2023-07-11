# Published as usps_add_finder

#### UI ####

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML("
           body {max-width: 1500px; margin: auto;}
           .title {padding: 10px; background-color: #bbd4ff; border: 0px solid #829ecf;
              border-radius: 4px; margin-top: 4px; margin-bottom: 4px;}
           .title > h2 {font-weight: bold;}
           /* Side panel */
           .col-sm-4 > form {border-width: ;}
           .form-group.shiny-input-container, #file1_progress {margin-bottom: 0!important;}
           #btn1 {margin-bottom: 20px;}
           .sidepanel1, .sidepanel2 {margin-bottom: 15px;}
           .text-small {font-size: .85em;}
           /* Main panel */
           .preview-section {margin-bottom: 4em;}
           .table-a td {padding: 2px 4px; vertical-align: top; border: 1px solid #e3e3e3}
           .modal-header {color: red;}"
      )
    )
  ),
  tags$div(class = "title",
           titlePanel("USPS Address Finder")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tags$div(class = "sidepanel1",
               h4("Upload an address file"),
               tags$p(class = "text-small", "*Anonymized data only"),
               fileInput("file1",
                         label = "CSV",
                         accept = c("text/csv", ".csv"))
      ),
      tags$div(class = "sidepanel2",
               h4("Search addresses on USPS.com"),
               shinyjs::disabled(
                 actionButton("btn1",
                            label = "Search USPS")
               )
      ),
      tags$div(class = "sidepanel3",
               h4("Download results"),
               shinyjs::disabled(
                 downloadButton("down1",
                                label = "Download")
               )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Main",
                 tags$div(tags$h3("How to use this"),
                          tags$p("This app searches a table of addresses using the",
                                 tags$a(href = "https://tools.usps.com/zip-code-lookup.htm?byaddress",
                                        "ZIP Code by Address"),
                                 "lookup tool at USPS.com. Use the buttons in the sidebar to upload your table, search them",
                                 "rapidly at USPS.com, and download the results. A preview of the both your source file and",
                                 "search results will appear in the Data Preview tab. To clear your data and reset the app,",
                                 "refresh the page."),
                          tags$h3("Source file"),
                          tags$p("The uploaded table of addresses must be a CSV file with the first row containing the column",
                                 "names shown below. (Other columns are allowed but will be ignored.) For an address to be",
                                 "searched, a row must contain values in 1) street and zip, or 2) street, city, and state."),
                          tags$table(class = "table-a",
                                     tags$tr(tags$td("1"), tags$td("street"), tags$td("unit"), tags$td("city"),tags$td("state"),
                                             tags$td("zip")),
                                     tags$tr(tags$td("2"), tags$td(""), tags$td(""), tags$td(""), tags$td(""), tags$td(""))),
                          br(),
                          tags$p(strong("Important:"), "Before uploading a file, be sure to remove all personally",
                                 "identifying information and/or anything that could link an address to an individual."),
                          tags$p("Click to download a pre-formatted file for your addresses:"),
                          downloadButton("down2", label = "Download"),
                          tags$h3("Results"),
                          tags$p("The results file will contain the following information:"),
                          tags$ul(tags$li("A number relating each result back to the source address in the uploaded file"),
                                  tags$li("A number for the result returned by USPS"),
                                  tags$li("The address data from USPS (street address, city, state, 4- and 5- digit zip codes,",
                                          "and county)"),
                                  tags$li("A Delivery Point Validation code (explained in the DPV Codes tab)"))
                 ),
        ),
        tabPanel("Data Preview",
                 tags$div(br(),
                          tags$p(em("A preview of the uploaded file and search results will appear here."))
                 ),
                 tags$div(class = "preview-section",
                          htmlOutput("header1"),
                          DT::dataTableOutput("table1")
                 ),
                 tags$div(class = "preview-section",
                          htmlOutput("header2"),
                          DT::dataTableOutput("table2")
                 )
        ),
        tabPanel("DPV Codes",
                 tags$h3("Delivery Point Validation"),
                 tags$p("The Delivery Point Validation (DPV) Confirmation Indicator is provided by the USPS to show",
                        "if an address, or part of an address, is deliverable. In the definitions below, the primary",
                        "number precedes the street name, and the secondary number follows it (e.g., a unit or lot number)."),
                 tags$p("Code definitions:"),
                 tags$table(class = "table-a",
                   tags$tbody(
                     tags$tr(tags$td("Y"),
                             tags$td("The address is validated for the primary and (if present) secondary number.")),
                     tags$tr(tags$td("D"),
                             tags$td("Only the primary number is validated. The secondary number is missing.")),
                     tags$tr(tags$td("S"),
                             tags$td("Only the primary number is validated. The secondary number provided is unconfirmed.")),
                     tags$tr(tags$td("N"),
                             tags$td("The address is not validated."))
                   )
                 )
        ),
        tabPanel("About",
                 tags$h3("About"),
                 tags$p("This app was created to speed up the process of validating addresses before geocoding.",
                 "If you have suggestions for improvement, or if you notice any problems with the app, please",
                 "contact the developer.")
        )
      )
    )
  )
)

#### Server ####

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
    if (is.data.frame(df_in())) {shinyjs::enable("btn1")}
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
                    })
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
    if (is.data.frame(df_out())) {shinyjs::enable("down1")}
  })
  
  # Searched table output
  output$table2 <- DT::renderDataTable({
    req(is.data.frame(df_out()))
    DT::datatable(df_out(), extensions = "Buttons",
                  options = list(pageLength = 5,
                                 rownames = F,
                                 dom = "Bfrtip",
                                 buttons = c("copy", "csv", "excel", "pdf", "print")))
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



