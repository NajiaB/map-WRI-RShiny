library(shiny)
library(plotly)

setwd('C:/Users/BOUADDOUCH Najia/Documents/programmation R')
risque = read.csv2('risque.csv', header=TRUE, dec='.',na.strings=c("","NA"),encoding = "UTF-8")

if (interactive()) {
  
  ui <- navbarPage(
    title="Les risques naturels" ,
    tabPanel(title='data',
             fileInput("file1", "Choose CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
             ),
             
             
             tags$hr(),
             checkboxInput("header", "Header", TRUE),
             dataTableOutput("data")
             
             
             
             
             
    ),
    tabPanel(title='carte',
             selectInput('annee',label='Select a year',choices=c(2011:2021)),
             
             selectInput('indic',label='Select the variable to observe',choices=c('WRI',"Exposure",'Vulnerability','Susceptibility','Lack.of.Adaptive.Capacities','Lack.of.Coping.Capabilities ')),
             
             selectInput('couleur',label='Choose a colour palette', choices=c('Viridis','Greys','heat','Greens','rainbow')),
             
             
             plotlyOutput('carte'),
             
             
    )
    
    
  )
  
  
  server <- function(input, output) {
    output$data <- renderDataTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,sep=';')
      
    })
    
    
    output$carte = renderPlotly({
      
      a=subset(risque, Year == input$annee  )%>%
        group_by(CODE)
      colonne=0
      colonne2=0
      indice=input$indic
      if (indice == 'WRI'){colonne=a$WRI ; colonne2=a$WRI.Category}
      if (indice == 'Vulnerability'){colonne=a$Vulnerability ; colonne2=a$Vulnerability.Category}
      if (indice == 'Susceptibility'){colonne=a$Susceptibility ; colonne2=a$Susceptibility.Category}
      if (indice == "Lack.of.Adaptive.Capacities"){colonne=a$Lack.of.Adaptive.Capacities ; colonne2=NULL}
      if (indice == "Exposure"){colonne=a$Exposure ; colonne2=a$Exposure.Category}
      if (indice == "Lack.of.Coping.Capabilities"){colonne=a$Lack.of.Coping.Capabilities ; colonne2=NULL}
      
      colonne3=with(a, paste(Region, '<br>','Category :',colonne2))
      plot_ly(a,type='choropleth',locations=a$CODE, z=colonne , text=colonne3, colorscale=input$couleur,marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>%
        layout(title = paste(indice,'in',input$annee)) %>%
        colorbar(title = indice)
    }
    
    )
  }
  
  shinyApp(ui, server)
}