library(shiny)

ui <- fluidPage(
  titlePanel("Trabajo Parcial - Administracion de la Informacion"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data", "Data", c("Data 7", "Data 12")),
      x <- uiOutput("x"),
      y <- uiOutput("y")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("table")),
        tabPanel("Plot", plotOutput("plot", click = "plot_click"), verbatimTextOutput("info"))
      )
    )
  )
)


server <- function(input, output){
  output$x <- renderUI({
    if (input$data == "Data 7") {
      options <- c("DepartmentID", "Object.NameID", "TitleID", "CultureID", "MediumID", "ClassificationID")
    }
    else if (input$data == "Data 12") {
      options <- c("issn", "citation_count_sum", "paper_count_sum", "avg_cites_per_paper", "proj_ai", "proj_ai_year")
    }
    radioButtons('property1', 'X', options, selected = character(0))
  })
  
  output$y <- renderUI({
    if (input$data == "Data 7") {
      options <- c("DepartmentID", "Object.NameID", "TitleID", "CultureID", "MediumID", "ClassificationID")
    }
    else if (input$data == "Data 12") {
      options <- c("issn", "citation_count_sum", "paper_count_sum", "avg_cites_per_paper", "proj_ai", "proj_ai_year")
    }
    radioButtons('property2', 'Y', options, selected = character(0))
  })
  
  output$table <- renderTable({
    
    tryCatch(
      {
        if (input$data == "Data 9") {
          data = readRDS("wData1.rds")
        }
        else if (input$data == "Data 11") {
          data = readRDS("wData2.rds")
        }
      },
      error = function(e){
        stop(safeError(e))
      }
    )
    return(data)
  })
  
  output$plot <- renderPlot({
    
    if (input$data == "Data 9") {
      data = readRDS("wData1.rds")
    }
    else if (input$data == "Data 11") {
      data = readRDS("wData2.rds")
    }
    
    plot(data[,input$property1], data[,input$property2])
  })
  
  output$info <- renderPrint({
    if (input$data == "Data 9") {
      data = readRDS("wData1.rds")
    }
    else if (input$data == "Data 11") {
      data = readRDS("wData2.rds")
    }
    
    x = input$property1
    y = input$property2
    nearPoints(data, input$plot_click, xvar = x, yvar = y)
  })
}

shinyApp(ui, server)