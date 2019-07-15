library(shiny)
library(gtools)
library(ggplot2)
library(xtable)
library(plotly)
library(agricolae)
library(dplyr)
library(DT)
library(stringr)
source("dado.R")
source("exponencial.R")
source("uniforme.R")
source("frec.R")
source("ruleta.R")
source("binomial.R")


server <- function(input, output) {
    
    output$UIunidad2 <- renderUI({
        
        if(input$SELtema == "proba"){
            
            selectInput("SELproba", "Experimento:", list("Moneda" = "mone", 
                                                         "Ruleta" = "rul"))
            } else return(NULL)
    })
    
#-----------Parámetros de unidad 2------------------------
    output$UIparamUnidad2 <- renderUI({
        
        if(input$SELtema == "proba")
            if(input$SELproba == "mone"){
            
            wellPanel(
                numericInput("prob",
                             "Prob. teórica de cara :",
                             min = 0,
                             max = 1,
                             step = 0.05,
                             value = 0.5),
                numericInput("exp",
                             "N° de experimentos:", 2,
                             min = 1,
                             max = 100000))
                
            } else if(input$SELproba == "rul") {
                wellPanel(numericInput("exp",
                                       "N° de experimentos:", 36,
                                       min = 36,
                                       max = 10000))
                
                } else return(NULL)
    })
    
#----------Parámetros de unidad 3------------------------------------------    
    output$UIunidad3 <- renderUI({
        
        if(input$SELtema == "binom"){
                
                wellPanel(
                    numericInput("nBin",
                                 "N° de experimentos:", 10,
                                 min = 0,
                                 max = 1000),
                    numericInput("defec",
                                 "N° máximo de art. defec. aceptables:", 1,
                                 min = 0,
                                 max = 1000))
                
            } else return(NULL)
    })
    
  
#----------Elijo distribuciones para TCL-------------------        
    output$UIplotdistr <- renderUI({   
    
        if(input$SELtema == "tcl"){
            
            wellPanel(
                selectInput("SELgrafico","Distribución:",list("Discreta" = "dis", 
                                                              "Exponencial" = "exp",
                                                              "Uniforme" = "uni")),
                radioButtons("SELvar", "Elegir tipo de variable:", 
                            c("Promedio" = "x_raya", 
                              "Suma" = "x_suma")))
        }
    })

#------------Parámetros de las distribuciones de TCL-------------------
    output$UIplotparams <- renderUI({
        
        if(input$SELtema == "tcl") {
            
            if(input$SELgrafico == "uni") {
                wellPanel(
                    sliderInput("n",
                                "N° de variables a sumar:",
                                min = 1,
                                max = 50,
                                value = 1),
                    sliderInput("n1",
                                "N° de intervalos:",
                                min = 5,
                                max = 20,
                                value = 10))
            
            } else if(input$SELgrafico == "exp") {
                
                wellPanel(
                    sliderInput("n",
                                "N° de variables a sumar:",
                                min = 1,
                                max = 50,
                                value = 1),
                    sliderInput("n1",
                                "N° de intervalos:",
                                min = 5,
                                max = 20,
                                value = 10),
                    sliderInput("alfa",
                                "Valor de alfa:",
                                min = 0.01,
                                max = 10,
                                value = 1))
                
                } else {
                        wellPanel(
                            sliderInput("n",
                                        "N° de variables a sumar:",
                                        min = 1,
                                        max = 5,
                                        value = 1))
                  }
        } else return (NULL)
    })
    
    
    # output$UIplottype2 <- renderUI({
    #     if(input$SELgrafico == "exp" | input$SELgrafico == "uni"){
    #         
    #         wellPanel(
    #             sliderInput("n",
    #                         "N° de variables a sumar:",
    #                         min = 5,
    #                         max = 20,
    #                         value = 10))
    #     } else {}
    # })

#---------------Gráfico de la unidad 2 con Plotly-------------------   
    output$distPlot1 <- renderPlotly({
        
        if(input$SELtema == "proba") {
            if(input$SELproba == "mone") {
                frec(pteo = input$prob, repe = input$exp)
            } else {
                  ruleta(n = input$exp)
            }
        } else return (null)
    })

#---------Gráficos de las distribuciones de TCL----------------------    
    output$distPlot2 <- renderPlot({
        
        if(input$SELtema == "tcl") {
            if(input$SELgrafico == "exp"){
                exponencial(input$n, tipo.var = input$SELvar, intervalos = input$n1, 
                            input$alfa, grafico =  TRUE, parametros = "no parametros")

              } else if (input$SELgrafico == "dis") {
                    
                    dado(input$n, imp = "gráfico", tipo.var = input$SELvar)

                } else {

                        uniforme(input$n, tipo.var = input$SELvar, intervalos = input$n1, table = FALSE)
                }
        }
    })
    
#------------Gráfico de la dist. Binomial-------------------------------
        output$plotBinom1 <- renderPlotly({
        
        if(input$SELtema == "binom") {
            binomial(n = input$nBin, defec = input$defec, tipo.graf = "max.defec", p = 0.5)
        } else return (null)
    })
    
    output$plotBinom2 <- renderPlotly({
        
        if(input$SELtema == "binom") {
            binomial(n = input$nBin, defec = input$defec, tipo.graf = "barras", p = input$probBinom)
        } else return (null)
    })

#----------Tablas de TCL---------------------------------   
    output$descrip <- renderDataTable({
        
        if (input$SELgrafico == "dis") {
            
            dado(input$n, imp = "tabla", tipo.var = input$SELvar, tabla = "permut")
        
        } else if (input$SELgrafico == "exp") {
            
            exponencial(input$n, tipo.var = input$SELvar, intervalos = input$n1, 
                        input$alfa, grafico = FALSE, parametros = "no parametros")
        
            } else if (input$SELgrafico == "uni") {
              
                uniforme(input$n, tipo.var = input$SELvar, intervalos = input$n1, 
                         table = TRUE)
            
              } else return(NULL)
    
    }, hover = FALSE)
    
    output$descrip2 <- renderDataTable({
        
        if (input$SELgrafico == "dis") {
           dado(input$n, imp = "tabla", tipo.var = input$SELvar, tabla = "prob")
        } else return(NULL)
    }, hover = FALSE)

#------------Genero textos--------------------------------        
    output$probAcum <- renderText ({
        
        paste("P(X < ", input$defec + 1, ") = ", round(pbinom(input$defec, input$nBin, input$probBinom), 4))
    })
    
    output$valor_esperado <- renderText ({
        
        if (input$SELgrafico == "exp") {
            
            paste("E(X) = ", 1/input$alfa)
        
        } else if(input$SELgrafico == "uni") {
            
            paste("E(X) = ", 0.5)
            
            }
    })
    
    output$promedio_continua <- renderText ({
        
        if (input$SELgrafico == "exp") {
            
            paste("x_raya = ", exponencial(input$n, tipo.var = input$SELvar, intervalos = input$n1, 
                                           input$alfa, grafico = FALSE, parametros = "media"))
        } else if(input$SELgrafico == "uni") {
            
            paste("x_raya = ", 2)
            
        }
    })

#------------Paneles de salidas-----------------   
    output$UItabs <- renderUI ({
        
        if(input$SELtema == "proba") {
                tabsetPanel(
                    tabPanel("Gráfico", plotlyOutput("distPlot1")))
        } else if (input$SELtema == "binom") {
                 tabsetPanel(
                    tabPanel("Distr. Cant. Defectuosas", plotlyOutput("plotBinom2"),
                             column(6, sliderInput("probBinom",
                                         "Probabilidad de art. defectuoso:",
                                         min = 0.01,
                                         max = 1,
                                         step = 0.01,
                                         value = 0.5)),
                             column(6, "Probabilidad de aceptar el lote", verbatimTextOutput("probAcum"))),
                    tabPanel("Curva Carac. de Operaciones", plotlyOutput("plotBinom1")))
          } else {
                   if(input$SELgrafico == "dis") {
                       tabsetPanel(
                            tabPanel("Tabla", 
                                column(6, wellPanel(dataTableOutput("descrip"))) , 
                                column(5, wellPanel(dataTableOutput("descrip2")))),
                            tabPanel("Gráfico", plotOutput("distPlot2")))
                       
                   } else {
                       tabsetPanel(
                            tabPanel("Tabla", 
                                     column(8, wellPanel(dataTableOutput("descrip")))),
                            tabPanel("Gráfico", 
                                     column(6, "Parámetros", verbatimTextOutput("valor_esperado")),
                                     column(6, "Estimadores", verbatimTextOutput("promedio_continua")),
                                     column(6, verbatimTextOutput("valor_esperado")),
                                     plotOutput("distPlot2")))
                   }
         }
    })
}

#-------------------ui---------------------------
ui <- fluidPage(
    
    # Application title
    titlePanel("Probabildad y Estadística - UTN"),
    
    sidebarPanel(
        selectInput("SELtema","Tema:",list("Unidad 2: Probabilidades" = "proba",
                                           "Unidad 3: Distribución Binomial" = "binom",
                                           "Unidad 6: Teorema Central del Límite" = "tcl")
        ),
        uiOutput("UIunidad2"),
        uiOutput("UIunidad3"),
        #uiOutput("UIrul"),
        uiOutput("UIparamUnidad2"),
        uiOutput("UIplotdistr"),
        uiOutput("UIvar"),
        uiOutput("UIplotparams"),
        uiOutput("UIplotBinom")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
        uiOutput("UItabs"))
    #     tabsetPanel(
    #         tabPanel("Tabla", 
    #                  column(6, wellPanel(tableOutput("descrip"))) , 
    #                  column(4, wellPanel(tableOutput("descrip2")))),
    #         tabPanel("Gráfico", plotlyOutput("distPlot"))               
    #     )
    # ) 
)


shinyApp(ui = ui, server = server)