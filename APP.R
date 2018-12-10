library(shiny)
library(shiny)
library(lpSolveAPI)
library(shinythemes)
ui<-( navbarPage(theme = shinytheme("superhero"),"Linear Programing Optimization model",
             tabPanel("Instructions",
                      mainPanel(
                        h3('Instructions for use'),
                        h4('This app asks the user to input three values'),
                        h4('The three inputs are the demand in the problem ,
                             
                          the linear program will provide the quantities to produce and the optimised cost 
                          '),
                        h4('the problem statement is given on the next tab below the model displayed'),
                        h4('the r code can be found in the below given github account'),
                        h3(a('Github Account',
                             "https://github.com/NTomarRajput/Learning.git",
                             target="_blank"))
                      )),        
             tabPanel("LP Model and Problem",
                      mainPanel(
                        h3('Production Planning problem :A manufacturing manager is in charge of minimizing the total costs (raw materials, labor and storage costs) of four months.
                             + Labor costs are of 12 dollars per hour
                             + Each unit of final product needs 30 minutes of labor
                             + Storage costs are equal to 2 e for each unit stored at the end of the month.
                             + Any unit produced at a given month can be used to cover the demand of the same month, or be stored to cover the demand of months to come.
                             + At the beginning of month 1 there is no stock, and there are no minimum stock requirements for any month
                           
                                 SOLUTION: model :
                                Decison variables used in to define the model are defined for i = 1, . . . , 4:),


                        • Variables qi representing the quantity produced in month i


                        • Variables si representing the stock at the end of month i


                        • the objective function get teh minimised cost 


                        • the user can alter the demand and depending on that the production variables will change and the cost is optimised.'),
                        verbatimTextOutput("model"))),
             
             tabPanel("Optimization results ",
                      sidebarPanel(
                        h3('Please select variables'),
                        numericInput('demand1','total demand1 selected', 100, min = 50, max = 1000, step = 1),
                        numericInput('demand2','total demand2 selected', 200, min = 50, max = 500, step = 1),
                        numericInput('demand3','total demand3 selected', 150, min = 50, max = 1000, step = 1),
                        numericInput('demand4','total demand4 selected', 400, min = 50, max = 1000, step = 1),
                        submitButton('Submit')),
                      mainPanel(
                        h3('Results of optimization'),
                        h4('demand1 selected'),
                        verbatimTextOutput("outdemand1"),
                        h4('demand2 selected'),
                        verbatimTextOutput("outdemand2"),
                        h4('demand3 selected'),
                        verbatimTextOutput("outdemand3"),
                        h4('demand4 selected'),
                        verbatimTextOutput("outdemand4"),
                        h4('quantities to produce'),
                        verbatimTextOutput("variables"),
                        h4("optimization result:cost of production" ),
                        verbatimTextOutput("objective")))
            
)
            
             
                        )

server <- function(input, output) {
  lprec <- make.lp(4,8)
  invisible(lp.control(lprec, sense = "min"))
  set.objfn(lprec, c(12,14,16,18,2,2,2,2))
  set.type(lprec, 5 , "binary")
  set.constr.value(lprec, rhs = c(100,200,150,400), constraints=(1:4))
  set.constr.type(lprec, rep("=", 4))
  set.row(lprec, 1, c(1, -1), indices = c(1,5))
  set.row(lprec, 2, c(1,1, -1),indices = c(2,5,6))
  set.row(lprec, 3, c(1,1,-1), indices = c(3,6,7))
  set.row(lprec, 4, c(1,1, -1) ,indices =c(4,7,8))
  set.bounds(lprec, upper = c(400,400,300,300), columns = c(1,2,3,4))
  name.lp(lprec, "Optimization")
  output$outdemand1<- renderPrint({input$demand1})
  output$outdemand2<- renderPrint({input$demand2})
  output$outdemand3 <- renderPrint({input$demand3})
  output$outdemand4 <- renderPrint({input$demand4})
  output$objective <- renderText({
    set.constr.value(lprec, c(input$demand1,input$demand2,input$demand3,input$demand4), constraints=(1:4))
    solve(lprec)
    get.objective(lprec)
  })

  output$variables <- renderText({
    set.constr.value(lprec, c(input$demand1,input$demand2,input$demand3,input$demand4), constraints=(1:4))
    solve(lprec)
    get.variables(lprec)
  })
  
  output$model <- renderPrint({
    set.constr.value(lprec, c(input$demand1,input$demand2,input$demand3,input$demand4), constraints=(1:4))
    solve(lprec)
    print(lprec)
  })
}

shinyApp(ui = ui, server = server)