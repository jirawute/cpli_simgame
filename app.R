


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
      sliderInput("fade", "EXP fade",
                  min = 0.5, max = 1,
                  value = 0.8, step = 0.05,
                  animate = TRUE),
      
      sliderInput("exp", "Experience",
                  min = 0, max = 1,
                  value = 0.1, step = 0.1),
      sliderInput("eps", "Epsilon",
                  min = 0, max = 1,
                  value = 0.2, step = 0.1),
      # sliderInput("n", "Market SalesQTY:",
      #             min = 0, max = 10000000,
      #             value = 5000000, step = 10000,pre="#",sep=","),
      
      
      
      
      # Include clarifying text ----
      helpText("Version:180605 Note: กด RUN เพื่อคำนวณปีถัดไป,  Reset เพื่อเริ่มต้นใหม่และ reset customer experience"),
      
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
                  tabPanel("Temp3", tableOutput("t3")),
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
  
  
  premium_ratio <- 0.25
  total_customer <- 1000000  # x 10 pcs/year
  mass_sample <-total_customer*(1-premium_ratio)
  premium_sample <-total_customer*premium_ratio
  
  players <- 7
  dimension <- c(total_customer,players)
  r<-0.2
  eps<- 0.2
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
  
  #consumption <- floor(runif(total_customer,1,19))#each customer consume 1-9 icecreme/year
  colnames(customer_mass)<-df$Name
  colnames(customer_premium)<-df$Name
  
  
  cal<- function(v,n){
    player_in <- as.matrix(v$data[2:7,])
    class(player_in) <- "numeric"
    stock <- as.numeric(v$data[8,])
    expTY <- as.numeric(v$data[9,])#Experience This Year
    
    if(is.null(v$exp)){v$exp <- array(0,dimension)}
    
    filter<-array(0,dimension)
    
    for(i in 1:players){
      if(v$data[1,i]=="Mass"){
        filter[1:(n*(1-premium_ratio)),i]<-1
      }else{
        filter[(mass_sample+1):(mass_sample+n*premium_ratio),i]<-1
      }
    }
    
    calculation<-customer %*% player_in +v$exp+ rnorm(dimension[1]*dimension[2],0,input$eps)+filter*100
    max <- apply(calculation, 1, max)
    a <- calculation - max+1
    a[a<1]<-0
    winner <-a*filter #choice selected
    
    consume <- winner *10# consumption #Actual consumption to calculate total SalesQTY of each player
    
    min <- pmin(colSums(consume),stock) #SalesQTY capped by total stock available
    
    v$out<-rbind(min,colSums(consume),stock)
    
    
    temp <- t(t(winner)*expTY*input$exp)
    
    ret <- v$exp*winner
    ret[ret!=0]<-1
    v$ret <- colSums(ret)/colSums(winner)
    v$exp<- v$exp*input$fade + temp
    v$t1<-rbind(consume[1:10,],array("X",c(2,players)),consume[(mass_sample+1):(mass_sample+10),])
    v$t2<-rbind(v$exp[1:10,],array("X",c(2,7)),v$exp[(mass_sample+1):(mass_sample+10),])
    v$t3 <- rbind(calculation[1:10,],array("X",c(2,players)),calculation[(mass_sample+1):(mass_sample+10),])
    #v$t3 <- head(max-100,n=100)
    min
  }
  
  readData <- function(sheet,r) {
    
    gs_read(sheet,ws="Input",range=r)
  }
  writeData <- function(sheet,data,y) {
    rows <- paste("c",35+y,sep="")
    gs_edit_cells(sheet, ws = "Input", anchor=rows, byrow=TRUE, input = data, trim = FALSE)
  }

  
  values <- reactiveValues(data = NULL,out=NULL,exp=NULL,year=0)
  
  observeEvent(input$run, {
    
    sheet <- gs_title("Simulation Game")
    values$year<-values$year+1
    df<- readData(sheet,"b35:b42")
    x<- as.numeric(unlist(df))
    qty<- x[values$year]
    values$data<- readData(sheet,"c9:i18")
    out<- cal(values,qty/10)
    
    values$msg <- qty
    writeData(sheet,out,values$year)
    
    
    
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
    if (is.null(values$exp)) {return("NULL")}
    temp_exp <- values$exp
    temp_exp[temp_exp!=0]<- 1
    rbind(colSums(values$exp)/colSums(temp_exp),values$ret)
    
  })
  output$t1 <- renderTable({
    values$t1
  })
  output$t2 <- renderTable({
    values$t2
  })
  output$t3 <- renderTable({
    values$t3
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
    paste("Year",values$year,"      qty:",values$msg)
  })
  output$round <- renderText({
    round()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)