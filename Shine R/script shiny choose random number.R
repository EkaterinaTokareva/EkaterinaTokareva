library(shiny)
library(png)
library(dplyr)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(tags$style(
  HTML('
         #sidebar {
            background-color: #D3D3D3;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),

  
  setBackgroundColor("#D3D3D3"),
  
  titlePanel(
    HTML('<center><FONT color="#8B8878"><FONT size="15pt">Randomizer</FONT></FONT></center>'),),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("go", HTML('<center><FONT size="4pt">Generate</FONT></center>')),
      align = "center",
      helpText(HTML('<center><FONT size="3pt">Random number:</FONT></center>'))
      ),
    column(9,
           textOutput('selected_var'
             #HTML(paste('<FONT size="20pt">', 'selected_var', '</FONT>'))
             ),
           tags$head(tags$style("#selected_var{color: black;
                                 font-size: 80px;
                                 text-align:center;
                                 }"
           )),
           imageOutput("image2")
    )
  )
)





server <- function(input, output, session) {
  randomVals <- eventReactive(input$go, {
    names <- c(1, 2, 3, 5, 6, 7, 8, 11, 12, 13, 15)
    s <- sample(names, 1)
    #s1 <- paste0('<center><FONT size="10pt">',sample(names, 1),'</FONT></center>')
    #s2 <- HTML(s1)

  })
  
  output$selected_var <- renderText({ 
    #HTML(paste0('<font size="12pt"', randomVals(), '</font>'))
    randomVals()
    })
  
  
  # image2 sends pre-rendered images
  output$image2 <- renderImage({
    list(
        src = "C:/Users/EkaterinaTokareva/Desktop/randomaser/gener.png",
        contentType = "image/png"
      )
    
  }, deleteFile = FALSE)
}

runApp(list(ui = ui, server = server),host="157.35.116.68",port=5013, launch.browser = TRUE)

