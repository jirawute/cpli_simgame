


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
      # 
      # sliderInput("e", "Epsilon",
      #             min = 0, max = 1,
      #             value = 0.2, step = 0.1,
      #             animate = TRUE),
      # 
      # sliderInput("exp", "Experience",
      #             min = 0, max = 1,
      #             value = 0.1, step = 0.1),
      sliderInput("n", "Number of Customer:",
                  min = 0, max = 2000000,
                  value = 1000000, step = 10000,pre="#",sep=","),
      
      
      
      
      # Include clarifying text ----
      helpText("Note: กด RUN เพื่อคำนวณปีถัดไป,  Reset เพื่อเริ่มต้นใหม่และ reset customer experience"),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("run", "RUN"),
      
      actionButton("reset", "Reset")
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Output", tableOutput("myOut")),
                  tabPanel("Input", tableOutput("myIn")),
                  tabPanel("EXP", tableOutput("myExp")),
                  tabPanel("Temp1", tableOutput("t1")),
                  tabPanel("Temp2", tableOutput("t2")),
                  #tabPanel("Assumption", tableOutput("customer_assumption")),
                  tabPanel("Mass", plotOutput("customer_mass")),
                  tabPanel("Premium", plotOutput("customer_premium"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  library(googlesheets)
  suppressMessages(library(dplyr))
  library(shiny)
  mass_sample <-1600000
  premium_sample <-400000
  count<-mass_sample+premium_sample
  players <- 7
  dimension <- c(count,players)
  r<-0.2
  eps<- 0.2
  exp <- 0.1
  year<-0
  
  df <- data.frame( Name = c("Price" , "Packaging" , "MKT-T" , "MKT-O" ,"MKT-P", "Material"),
                    Mass = c(0.45 , 0.2 ,0.01, 0.1,0.04, 0.2),
                    Premium = c( 0.2, 0.25, 0.05, 0.15, 0.05,0.3))
  
  
  customer_mass<- cbind(runif(mass_sample, df$Mass[1]*(1-r), df$Mass[1]*(1+r)),
                        runif(mass_sample, df$Mass[2]*(1-r), df$Mass[2]*(1+r)),
                        runif(mass_sample, df$Mass[3]*(1-r), df$Mass[3]*(1+r)),
                        runif(mass_sample, df$Mass[4]*(1-r), df$Mass[4]*(1+r)),
                        runif(mass_sample, df$Mass[5]*(1-r), df$Mass[5]*(1+r)),
                        runif(mass_sample, df$Mass[6]*(1-r), df$Mass[6]*(1+r)))
  customer_premium<- cbind(runif(premium_sample, df$Premium[1]*(1-r), df$Premium[1]*(1+r)),
                           runif(premium_sample, df$Premium[2]*(1-r), df$Premium[2]*(1+r)),
                           runif(premium_sample, df$Premium[3]*(1-r), df$Premium[3]*(1+r)),
                           runif(premium_sample, df$Premium[4]*(1-r), df$Premium[4]*(1+r)),
                           runif(premium_sample, df$Premium[5]*(1-r), df$Premium[5]*(1+r)),
                           runif(premium_sample, df$Premium[6]*(1-r), df$Premium[6]*(1+r)))
  customer<-rbind(customer_mass,customer_premium)
  
  colnames(customer_mass)<-df$Name
  colnames(customer_premium)<-df$Name
  
  
  cal<- function(v){
    
    player_in <- as.matrix(v$data[2:7,])
    class(player_in) <- "numeric"
    stock <- as.numeric(v$data[8,])
    expTY <- as.numeric(v$data[9,])#Experience This Year
    
    if(is.null(v$exp)){v$exp <- array(0,dimension)}
    
    filter<-array(0,dimension)
    
    for(i in 1:dimension[2]){
      if(v$data[1,i]=="Mass"){
        filter[1:(input$n*4/5),i]<-1
      }else{
        filter[(mass_sample+1):(mass_sample+input$n/5),i]<-1
      }
    }
    
    output<-customer %*% player_in +v$exp+ rnorm(dimension[1]*dimension[2],0,eps)+filter*10
    max <- apply(output, 1, max)
    output <- output - max+1
    output[output<1]<-0
    output <-output*filter

    min <- pmin(colSums(output)*5,stock)
    
    v$out<-rbind(min,colSums(output)*5,stock)
    
     temp<- output
     for(i in 1:players){
     temp[,i] <- output[,i]*expTY[i]
     }
     ret <- v$exp*output
     ret[ret!=0]<-1
     v$ret <- colSums(ret)/colSums(output)
     v$exp<- v$exp+ temp*exp*0.1

     v$t1<-rbind(output[1:10,],array("X",c(2,players)),output[(mass_sample+1):(mass_sample+10),])
     v$t2<-rbind(customer[1:10,],array("X",c(2,6)),customer[(mass_sample+1):(mass_sample+10),])
     min
      }
  
  readData <- function() {
    sheet <- gs_title("Simulation Game")

    data <- gs_read(sheet,ws="input",range="c9:i18")
    # rownames(data)<-df$Name
    
    data
    }
  writeData <- function(data,y) {
    sheet <- gs_title("Simulation Game")
    rows <- paste("c",35+y,sep="")
    gs_edit_cells(sheet, ws = "input", anchor=rows, byrow=TRUE, input = data, trim = FALSE)
  }

  # sliderValues <- 
  #   ({
  #     data.frame(
  #       Name = c(
  #         "E",
  #         "Experience",
  #         "No. of Customer"
  #       ),
  #       Value = as.character(c(
  #         input$e,
  #         input$exp,
  #         input$n
  #       )),
  #       stringsAsFactors = FALSE)
  #     
  #   })
  
  values <- reactiveValues(data = NULL,out=NULL,exp=NULL,year=0)
  
  observeEvent(input$run, {
    
    values$year<-values$year+1
    values$data <-readData()
    out<- cal(values)
  
    writeData(out,values$year)
    
    
  })
  
  observeEvent(input$reset, {
    values$data <- NULL
    values$out <- NULL
    values$exp <- NULL
    values$year<-0
  })  
  output$myIn <- renderTable({
    if (is.null(values$data)) return("NULL")
    values$data
  })
  output$myOut <- renderTable({
    if (is.null(values$out)) return("NULL")
    values$out
  })
  output$myExp <- renderTable({
    if (is.null(values$exp)) return("NULL")
    rbind(colSums(values$exp),values$ret)
  })
  output$t1 <- renderTable({
     values$t1
  })
  output$t2 <- renderTable({
    values$t2
  })
  # Generate a summary of the data ----
  output$customer_assumption <- renderTable({
    sliderValues()
  })
  # Generate an HTML table view of the data ----
  output$customer_mass <- renderPlot({
    boxplot(customer_mass,main="Mass Customer",ylab="Decision Weighted", ylim=c(0,0.5))
  })
  output$customer_premium <- renderPlot({
    boxplot(customer_premium,main="Premium Customer",ylab="Decision Weighted", ylim=c(0,0.5))
  })
  round<-reactive({
    paste("Year",values$year)
  })
  output$round <- renderText({
    round()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)