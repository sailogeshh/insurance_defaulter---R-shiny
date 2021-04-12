library(shiny)
library(shinydashboard)
library(plyr)
library(randomForest)
library(DT)

Logged = FALSE
my_username <- "test"
my_password <- "test"


########################################################################################################################################################################

ui <- dashboardPage(skin='green',
                    
                    dashboardHeader(title="Fradulant detection of Insurance claims",titleWidth = 400,dropdownMenu(type="messages",
                                                                                               messageItem(from="sailogesh",message="WELCOME TO the App"))
                                    
                    ),
                    
                    
                    dashboardSidebar(
                      fileInput("file","Upload a Data",accept=c('csv','comma-seperated-values','.csv')),
                      downloadButton("downloadData", "Download"),
                      actionButton("myuser","Logout",icon=icon("user")),br(),
                      tags$div(class = "header", checked = NA,
                               tags$tbody("Need Help ?"),br(),
                               tags$a(href = "https://github.com/sailogeshh", "Contact...")
                      )
                    ),
                    dashboardBody(
                      fluidRow(fluidPage(theme="bootstrap.min.css",
                                         
                                         tabBox ( width = 2000, height = 5500,
                                                  
                                                  
                                                  
                                                  tabPanel("Output", p("The aim of this application is to Predict will the company likely to default or not...",
                                                                       style="font-family:'Berlin sans FB Demi';font-size:12pt"), 
                                                           box( width = 12, height = 600 ,
                                                                title = "Will the company likely to default ? ",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                                                dataTableOutput("y_cap", height = 250, width = NULL))), 
                                                  
                                                  
                                                  tabPanel("Not Likely to fraud_reported",
                                                           box( width = 12, height = 600,
                                                                title = "Not likely to default ",status = "success", solidHeader = TRUE,collapsible = TRUE,
                                                                dataTableOutput("rrr",height = 250, width = NULL))), 
                                                  
                                                  
                                                  tabPanel("Likely to fraud_reported", 
                                                           box( width = 12, height = 600,
                                                                title = "Likely to default",status = "danger", solidHeader = TRUE,collapsible = TRUE,
                                                                dataTableOutput("yyy",height = 250, width = NULL)))
                                         )
                      )),verbatimTextOutput("dataInfo")
      )
                    )
########################################################################################################################################################################       
set.seed(100000)
load("modelblr.rda")


server = function(input, output,session) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(title = "Welcome",
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        #modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    } 
  })
  

  

  dataModal2 <- function(failed = FALSE) {
      modalDialog(fade = FALSE,title = tagList(h3("Thank You !!")),footer = NULL,
                  tags$div(class = "header", checked = NA,
                           tags$h4("Visit us for more..."),
                           tags$a(href = "https://github.com/sailogeshh", "sailogesh")
                  )
      )
  }
  

   obs4 <- observe({
    if(Logged <<- TRUE)
    req(input$myuser)
    showModal(dataModal2())
  })
   
###########################################################################   
   
  output$dataInfo <- renderPrint({
    
    out <-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      data <- read.csv(file1$datapath,header=TRUE)
      withProgress(message='Loading table',value=30,{
        n<-10
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      })
      
      testprob=predict(fit,newdata=data,type="response")
      data$fraud_reported <- ifelse(data$fraud_reported=="Y",1,0)
      prob=predict(fit,type="response")
      observed=data$fraud_reported
      cutpoints = seq(0,1,0.01)
      sensitivity = seq(1,101,1)
      specificity = seq(1,101,1)
      cutpoint_perf = cbind(cutpoints,sensitivity,specificity)
      x=data.frame(table(observed))
      n1=x[2,2]
      n2=x[1,2]
      
      for (i in 1:101)
      {
        pred = ifelse(prob < cutpoint_perf[i,1],0,1)
        sumed = pred + observed
        pred1_1 = ifelse(sumed==2,1,0)
        correct1_1 = sum(pred1_1)
        pred0_0 = ifelse(sumed==0,1,0)
        correct0_0 = sum(pred0_0)
        cutpoint_perf[i,2] = correct1_1/n1
        cutpoint_perf[i,3] = correct0_0/n2
      }
      
      cutvalue_table = data.frame(cutpoint_perf)
      cutvalue_table$diff = abs(cutvalue_table$sensitivity-cutvalue_table$specificity)
      cut_point=subset(cutvalue_table,cutvalue_table$diff==min(cutvalue_table$diff))
      optimum_cut=mean((cut_point$cutpoints))
      
      test_predicted=ifelse(testprob < optimum_cut,"Not Likely to fraud_reported","Likely to fraud_reported")
      data.frame(data[],test_predicted)
      
    })
    
    output$y_cap <- renderDataTable({
      out()
    }) 
    
    output$rrr <- renderDataTable({
      subset(out(),out()$test_predicted=="Not Likely to fraud_reported")
    }) 
    
    
    output$yyy <- renderDataTable({
      subset(out(),out()$test_predicted=="Likely to fraud_reported")
    }) 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("final_table_output", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(out(), file, row.names = FALSE)
      }
    )
############################################################################################################################################################ 
 
  })
  
}

############################################################################################################################################################

shinyApp(ui,server)
  
  