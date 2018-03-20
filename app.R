


# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Business Simulation Game"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # h2(textOutput("currentTime")),
      h3(textOutput("round")),
      
      sliderInput("e", "Epsilon",
                  min = 0, max = 1,
                  value = 0.2, step = 0.1,
                  animate = TRUE),
      # Input: 
      sliderInput("exp", "Experience",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      # Input: Animation with custom interval 
      sliderInput("n", "Number of Customer:",
                  min = 0, max = 2000000,
                  value = 10, step = 10000,pre="#",sep=","),
      
      
      
      
      # Include clarifying text ----
      helpText("Note: กด RUN เพื่อคำนวณ, Clear เพื่อ reset customer experience"),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("run", "RUN"),
      
      actionButton("reset", "Clear")
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Output", tableOutput("myOut")),
                  tabPanel("Input", tableOutput("myIn")),
                  tabPanel("EXP", tableOutput("myExp")),
                  tabPanel("Assumption", tableOutput("customer_assumption")),
                  tabPanel("Mass", plotOutput("customer_mass")),
                  tabPanel("Premium", plotOutput("customer_premium"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  library(shiny)
  library(googlesheets)
  mass_sample <-160000
  premium_sample <-40000
  count<-mass_sample+premium_sample
  df <- data.frame( Name = c("Price" , "Packaging" , "MKT-T" , "MKT-O" , "Material"),
                    Mass = c(0.45 , 0.2 ,0.05, 0.1, 0.2),
                    Premium = c( 0.1, 0.35, 0.1, 0.15, 0.3))
  
  r<-0.2
  customer_mass<- cbind(runif(mass_sample, df$Mass[1]*(1-r), df$Mass[1]*(1+r)),
                        runif(mass_sample, df$Mass[2]*(1-r), df$Mass[2]*(1+r)),
                        runif(mass_sample, df$Mass[3]*(1-r), df$Mass[3]*(1+r)),
                        runif(mass_sample, df$Mass[4]*(1-r), df$Mass[4]*(1+r)),
                        runif(mass_sample, df$Mass[5]*(1-r), df$Mass[5]*(1+r)))
  customer_premium<- cbind(runif(premium_sample, df$Premium[1]*(1-r), df$Premium[1]*(1+r)),
                           runif(premium_sample, df$Premium[2]*(1-r), df$Premium[2]*(1+r)),
                           runif(premium_sample, df$Premium[3]*(1-r), df$Premium[3]*(1+r)),
                           runif(premium_sample, df$Premium[4]*(1-r), df$Premium[4]*(1+r)),
                           runif(premium_sample, df$Premium[5]*(1-r), df$Premium[5]*(1+r)))
  customer<-rbind(customer_mass,customer_premium)
  
  exp<-array(0,c(count,4))
  filter<-array(0,c(count,4))
  filter[1:160000,]<-1
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$run, {
    v$data <-loadData()
    v$out <-cal(v$data)
  })
  
  observeEvent(input$reset, {
    v$data <- NULL
    v$out <- NULL
    exp<-NULL
  })  
  cal<- function(data){
    player_in <- as.matrix(data[2:6,])
    class(player_in)<-"numeric"
    rel_exp <- as.numeric(data[7,])
    for(i in 1:4){
      if(data[1,i]=="Premium"){
        filter[1:160000,i]<-0
        filter[160001:count,i]<-1
      }
    }
    
    output<-customer %*% player_in +exp+ rnorm(dim(exp)[1]*dim(exp)[2],0,input$e)
    output <-output*filter
    max <- apply(output[, 1:4], 1, max)
    output <- output - cbind(max,max,max,max)
    output[output==0]<-1
    output[output<0]<-0
     # if(is.null(exp)){exp<-array(0,c(count,4))}
     #  for(i in 1:4){
     # exp[,i] <- exp[,i]+input$exp*output[,i]*rel_exp[i]
     # }
    colSums(output)*12
  }
  
  loadData <- function() {
    # Grab the Google Sheet
    sheet <- gs_title("Simulation Game")
    # Read the data
    gs_read(sheet,ws="input",range="c9:f16")
  }
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c(
        "E",
        "Experience",
        "No. of Customer"
        ),
      Value = as.character(c(
        input$e,
        input$exp,
        input$n
        )),
      stringsAsFactors = FALSE)
    
  })
  

  
  output$myIn <- renderTable({
    if (is.null(v$data)) return()
    v$data
  })
  output$myOut <- renderTable({
    if (is.null(v$data)) return()
    v$out
  })
  output$myExp <- renderTable({
    if (is.null(exp)) return()
    exp
  })
  # Generate a summary of the data ----
  output$customer_assumption <- renderTable({
    sliderValues()
  })
  # Generate an HTML table view of the data ----
  output$customer_mass <- renderPlot({
    boxplot(customer_mass)
  })
  output$customer_premium <- renderPlot({
    boxplot(customer_premium)
  })
  round<-reactive({
    paste("XXXX",as.numeric(input$run))
  })
  output$round <- renderText({
    round()
  })

}

# Create Shiny app ----
shinyApp(ui, server)