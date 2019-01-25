


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
      sliderInput("exp_fade", "EXP fade",
                  min = 0, max = 0.2,
                  value = 0.15, step = 0.01,
                  animate = TRUE),
      
      sliderInput("eps", "Epsilon",
                  min = 0, max = 0.3,
                  value = 0.2, step = 0.01),
      
      #sliderInput("mp_sep", "Mass vs Premium Price",
      #            min = 1, max = 60,
      #            value = 30, step = 1),
      
      #sliderInput("range", "Price Limit:",
      #            min = 1, max = 100,
      #            value = c(30,60)),
      
      sliderInput("premium_ratio", "no. of premium/10ppl:",
                  min = 1, max = 10,
                  value = 2, step = 0.5),
      
      sliderInput("year", "YEAR:",
                  min = 1, max = 10,
                  value = 1, step = 1),
      
      
      # Include clarifying text ----
      helpText("Version:190125"),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("run", "RUN")
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Output", tableOutput("myOut")),
                  tabPanel("Input", tableOutput("myIn")),
                  tabPanel("M-EXP", tableOutput("m_Exp")),
                  tabPanel("P-EXP", tableOutput("p_Exp")),
                #  tabPanel("Temp1", tableOutput("t1")),
                #  tabPanel("Temp2", tableOutput("t2")),
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
  googlesheets.httr_oauth_cache = FALSE

  mass_sample <-2000000 #create sample of 2M+1M x 10 pcs each
  premium_sample <-1000000
  mp_sep <- 30  # Using 30 to separate mass or premium market
  
  
  r<-0.2      # random between 1-r -> 1+r
  
  options(scipen=999)
  df <- data.frame( Name = c("Price" , "Packaging" , "MKT-T" , "MKT-O" ,"MKT-P", "Material"),
                    Mass = c(0.45 , 0.2 ,0.01, 0.1,0.04, 0.2),
                    Premium = c( 0.2, 0.25, 0.05, 0.15, 0.05,0.3))
  
  
  m_customer<- cbind(runif(mass_sample, df$Mass[1]*(1-r), df$Mass[1]*(1+r)),
                        runif(mass_sample, df$Mass[2]*(1-r), df$Mass[2]*(1+r)),
                        runif(mass_sample, df$Mass[3]*(1-r), df$Mass[3]*(1+r)),
                        runif(mass_sample, df$Mass[4]*(1-r), df$Mass[4]*(1+r)),
                        runif(mass_sample, df$Mass[5]*(1-r), df$Mass[5]*(1+r)),
                        runif(mass_sample, df$Mass[6]*(1-r), df$Mass[6]*(1+r)))
  p_customer<- cbind(runif(premium_sample, df$Premium[1]*(1-r), df$Premium[1]*(1+r)),
                           runif(premium_sample, df$Premium[2]*(1-r), df$Premium[2]*(1+r)),
                           runif(premium_sample, df$Premium[3]*(1-r), df$Premium[3]*(1+r)),
                           runif(premium_sample, df$Premium[4]*(1-r), df$Premium[4]*(1+r)),
                           runif(premium_sample, df$Premium[5]*(1-r), df$Premium[5]*(1+r)),
                           runif(premium_sample, df$Premium[6]*(1-r), df$Premium[6]*(1+r)))
  customer<-rbind(m_customer,p_customer)
  
 
  
  #consumption <- floor(runif(total_customer,1,19))#each customer consume 1-9 icecreme/year
  colnames(m_customer)<-df$Name
  colnames(p_customer)<-df$Name
  
  convertInput<-function(arr,filter){
    x<-arr
    x[,!filter]<-0
    x[1,filter]<-1/(sum(1/x[1,filter])*x[1,filter])
    for(i in 2:6){
      x[i,filter]<-x[i,filter]/max (sum(x[i,filter]),1) #At least divided by 1
    }
    print(x)
    x
  }
  cal<- function(keyin,qty){
    
    m_exp <- array(0,c(mass_sample,7))
    p_exp <- array(0,c(premium_sample,7))
    
    consumer = array(0,dim=c(10,7))
    for(i in 1:input$year){  # up to 10 years
      if(any(is.na(keyin[,,i]))){
        
        values$qty <- "INPUT ERROR"
        break
      }else{
        mass <- qty[i]*(1-input$premium_ratio/10)
        
        premium <- qty[i]* (input$premium_ratio/10)
        print(mass)
        print(premium)
        
        m_input<-convertInput(keyin[,,i],keyin[1,,i]<=mp_sep)
        p_input<-convertInput(keyin[,,i],keyin[1,,i]>mp_sep)
        print(input$year)#______________
        m_exp[1:mass,] <-m_exp[1:mass,]*input$exp_fade+m_customer[1:mass,] %*% m_input[1:6,]
        p_exp[1:premium,] <- p_exp[1:premium,]*input$exp_fade+ p_customer[1:premium,] %*% p_input[1:6,]
        
        for (j in 1:mass){
          eps<-rnorm(7,0,input$eps)
          eps[keyin[1,,i]>mp_sep]<- 0
          temp<-which.max(m_exp[j,]+eps)
          consumer[i,temp] <- consumer[i,temp]+1
        }
        for (j in 1:premium){
          eps<-rnorm(7,0,input$eps)
          eps[keyin[1,,i]<=input$mp_sep]<- 0
          temp<-which.max(p_exp[j,]+eps)
          consumer[i,temp] <- consumer[i,temp]+1
        } 
       print(consumer)
      }
    }
    values$m_exp <- m_exp
    values$p_exp <- p_exp
    consumer
      
  }
  

  writeData <- function(gs,data,y) {
    rows <- paste("M",1+y,sep="")
    gs_edit_cells(gs, ws = "Process", anchor=rows, byrow=TRUE, input = data, trim = FALSE)
  }

  
  values <- reactiveValues(out = 0,p_exp=NULL,m_exp=NULL)
  
  observeEvent(input$run, {
    
    gs <- gs_title("CPLI-GAME")
    df<- gs_read(gs,ws="Process",range="B2:H71",col_names=FALSE)
    values$qty<- gs_read(gs,ws="Process",range="W2:W11",col_names=FALSE)
    values$keyin<- aperm(`dim<-`(t(df), list(7, 7, 10)),c(2,1,3))
    values$out<- cal(values$keyin,as.matrix(values$qty/10))*10
    
      writeData(gs,values$out[input$year,],input$year)
      
  })
  

  output$myIn <- renderTable({
    if (is.null(values$keyin)) return("NULL")
    values$keyin
  })
  output$myOut <- renderTable({
    if (is.null(values$out)) return("NULL")
    values$out
  })
  output$m_Exp <- renderTable({
    if (is.null(values$m_exp)) {return("NULL")}
      summary(values$m_exp)
    #  temp_exp <- values$exp
  #  temp_exp[temp_exp!=0]<- 1
  #  rbind(colSums(values$exp)/colSums(temp_exp),values$ret)
    
  })
  output$p_Exp <- renderTable({
    if (is.null(values$p_exp)) {return("NULL")}
    summary(values$p_exp)
  })
  output$t1 <- renderTable({
    
  })
  output$t2 <- renderTable({
    
  })

  # Generate an HTML table view of the data ----
  output$customer_mass <- renderPlot({
    boxplot(m_customer,main="Mass Customer",ylab="Decision Weighted", ylim=c(0,0.5))
  })
  output$customer_premium <- renderPlot({
    boxplot(p_customer,main="Premium Customer",ylab="Decision Weighted", ylim=c(0,0.5))
  })
  round<-reactive({
    paste("Year",input$year,"      qty:",values$qty)
  })
  output$round <- renderText({
    round()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)