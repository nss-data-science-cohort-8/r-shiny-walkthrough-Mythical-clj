

fluidPage(
  
  # Application title
  titlePanel("Affordable Housing in Davidson County"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput('hudid',
                  label = h3('select a property:'),
                  choices = c( LIHTC |> pull(HUD_ID) |> sort()),
                  selected = 1)
    ),
    
    
    mainPanel(
      fluidRow(
        column(
          width = 6,
          plotOutput("distPlot", 
                     height = '300px')
        ),
        column(
          width = 6,
          plotOutput('distPlot2',
                     height = '300px')
        )
      )
    )
  )
)