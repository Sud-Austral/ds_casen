# Casen
# Victor Enamorado - Christian Castro
# 30 de Octubre del 2020
library(ggplot2)
library(ggpubr)
library(markdown)
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(xts)
library(dygraphs)
library(kableExtra)
library(knitr)
library("readxl")
library(rsconnect)
library(dplyr)
library(summarytools)
library(epiDisplay)
#library(leaflet)
library(haven)
library(epiDisplay)
library("readxl")

library(hrbrthemes)
library(viridis)
library(viridisLite)
library(DescTools)
library(roperators)

library(writexl)

oldw <- getOption("warn")
options(warn = -1)

#rsconnect::deployApp('C:/Users/usuario/Desktop/shinycasen1')

dataset <- read.csv('Casen_no_humano.csv')


#dataset = read_sav("Casen_no_humano.csv")
datos_df_exp <- colnames(dataset)






datos_df_1000  <- read_xlsx("casen_2006_mil.xlsx")
# datos_df_1000 <- cbind(casen2017_1, casen2017_2)
datos_df_educacion <- datos_df_1000[, 1:32]
datos_df_educacion_preg <- colnames(datos_df_educacion)


data_2006_filtros_terr <- datos_df_1000[, 1:2]
data_2006_filtros_terr_ddl <- colnames(data_2006_filtros_terr)
data_2006_filtros_cat <- datos_df_1000[, 9:32]
data_2006_filtros_cat_ddl <- colnames(data_2006_filtros_cat)

datos_df_2000 <- read_xlsx("casen_2009_mil_ymt.xlsx")
datos_df_2009_ymt <- datos_df_2000[, 1:5]
datos_df_2009_ymt_preg <- colnames(datos_df_2009_ymt)

datos_df_casen_2009_mil_mn <- read_xlsx("casen_2009_mil_mn.xlsx")
datos_df_casen_2009_mil_mn <- datos_df_casen_2009_mil_mn[, 1:34]
datos_df_casen_2009_mil_mn_preg <- colnames(datos_df_casen_2009_mil_mn)

datos_df_casen_2011_mil_mn <- read_xlsx("casen_2011_mil_mn.xlsx")
datos_df_casen_2011_mil_mn <- datos_df_casen_2011_mil_mn[, 1:34]
datos_df_casen_2011_mil_mn_preg <- colnames(datos_df_casen_2011_mil_mn)

datos_df_casen_2011_mil_ymt <- read_xlsx("casen_2011_mil_ymt.xlsx")
datos_df_casen_2011_mil_ymt  <- datos_df_casen_2011_mil_ymt [, 1:24]
datos_df_casen_2011_mil_ymt_preg <- colnames(datos_df_casen_2011_mil_ymt )

###################

datos_df_casen_2013_mil <- read_xlsx("casen_2013_mil.xlsx")
datos_df_casen_2013_mil <- datos_df_casen_2013_mil[, 1:600]
datos_df_casen_2013_mil_preg <- colnames(datos_df_casen_2013_mil)

datos_df_casen_2015_mil <- read_xlsx("casen_2015_mil.xlsx")
datos_df_casen_2015_mil <- datos_df_casen_2015_mil[, 1:776]
datos_df_casen_2015_mil_preg <- colnames(datos_df_casen_2015_mil)

datos_df_casen_2017_mil <- read_xlsx("casen_2017_mil.xlsx")
datos_df_casen_2017_mil <- datos_df_casen_2017_mil[, 1:808]
datos_df_casen_2017_mil_preg <- colnames(datos_df_casen_2017_mil)

datos_df_casen_2017_miledu <- read_xlsx("casen_2017_mil.xlsx")
datos_df_casen_2017_miledu <- datos_df_casen_2017_miledu[, 43:102]
datos_df_casen_2017_mil_pregedu <- colnames(datos_df_casen_2017_miledu)



ui <- fluidPage(theme = shinytheme("cerulean"),
                
                selectInput("variable_anio", "Seleccione base de datos:",
                            
                            c("Seleccione año" = "2",
                              "Casen 2006" = "2006",
                              "Casen 2009 ymt" = "20090",
                              "Casen 2009 mn" = "20091",
                              "Casen 2011 ymt" = "20110",
                              "Casen 2011 mn" = "20111",
                              "Casen 2013" = "2013",
                              "Casen 2015" = "2015",
                              "Casen 2017" = "2017"
                              
                            )),       
                titlePanel("Manual de interpretación de variables y análisis estadísticos de la CASEN"),
                br(),
                uiOutput("navbarPageUI")
                
) 

# Definir la logica del server
server <- function(input, output, session) {
    
    
    output$navbarPageUI <- renderUI({
        
        user <- input$variable_anio
        
        if (user == 2006) {
            
            navbarPage(
                
                br(),
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("info_2006_intro.md")))),
                
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(3, includeMarkdown("info_2006_tabla.md")),
                                  column(12,  tableOutput("contents12"),
                                         dataTableOutput('table')))),
                
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("info_2006_frec.md")),
                    selectInput("ptabla", "prueba tabla:", c(datos_df_educacion_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_promedios", "prueba tabla:", c(datos_df_educacion_preg)),
                               column(12, renderPrint("promedios"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb", "prueba tabla:", c(datos_df_educacion_preg)),
                               column(12, plotOutput("cyb"))
                           ))
                           
                ),
                
                
                
                navbarMenu("Filtros",
                           tabPanel("a nivel social", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("nivel_filtro", "Seleccione unidad social:", c(data_2006_filtros_terr_ddl)),
                               selectInput("categoria_filtro", "Seleccione atributo:", c(data_2006_filtros_cat_ddl)),
                               column(12, tableOutput("promedios_filtros"))
                           ))),
                
                
                
                
                
                
                tabPanel("Descargas", titlePanel("Descarga de datos Casen"),
                         
                         sidebarLayout(
                             
                             sidebarPanel(
                                 
                                 selectInput("dataset", "Escoja una base de datos:",
                                             choices = c("casen 2006", "plot", "casen 2009 ymt")),
                                 
                                 downloadButton("dl", "Descargar"),
                                 br(),
                                 downloadButton("downloadPlot", "Descargar el plot")
                             ),
                             
                             
                             
                             mainPanel()
                             
                             
                         ))
                
                
            )
            
        }
        
        
        else if (user == 20090){
            
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(3,  tableOutput("contents12")))),
                
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla20090", "prueba tabla:", c(datos_df_2009_ymt_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_promedios_2009", "prueba tabla:", c(datos_df_2009_ymt_preg)),
                               column(12, verbatimTextOutput("promedios_2009"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb_2009", "prueba tabla:", c(datos_df_2009_ymt_preg)),
                               column(12, plotOutput("cyb_2009"))
                           ))
                           
                ),
                tabPanel("Descargas", titlePanel("Descarga de datos Casen"),
                         
                         sidebarLayout(
                             
                             sidebarPanel(
                                 
                                 selectInput("dataset2009", "Escoja una base de datos:",
                                             choices = c("casen 2009 ymt", "plot")),
                                 
                                 downloadButton("downloadData2009", "Descargar"),
                                 br(),
                                 downloadButton("downloadPlot2009", "Descargar el plot")
                             ),
                             
                             
                             
                             mainPanel()
                             
                             
                         ))
            )
        }
        
        #### aca vamos
        
        else if (user == 20091){
            navbarPage(
                
                br(),
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(3,  tableOutput("contents12")))),
                
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla20091", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_2009_mn", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                               column(12, verbatimTextOutput("promedios_2009_mn"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb_2009_mn", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                               column(12, plotOutput("cyb_2009_mn"))
                           ))
                           
                )
            )
        }
        
        # cuatro anio
        
        else if (user == 20110){
            navbarPage(
                
                br(),
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(3,  tableOutput("contents12")))),
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla20110", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_2011", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
                               column(12, verbatimTextOutput("promedios_2011"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb_2011", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
                               column(12, plotOutput("cyb_2011"))
                           ))
                           
                )
            )
        }
        
        else if (user == 20111){
            navbarPage(
                
                br(),
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(3,  tableOutput("contents12")))),
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla20111", "prueba tabla:", c(datos_df_casen_2011_mil_ymt_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_2011_ymt", "prueba tabla:", c(datos_df_casen_2011_mil_ymt_preg)),
                               column(12, verbatimTextOutput("promedios_2011_ymt"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb_2011_ymt", "prueba tabla:", c(datos_df_casen_2011_mil_ymt_preg)),
                               column(12, plotOutput("cyb_2011_ymt"))
                           ))
                           
                )
            )
        }
        
        
        else if (user == 2013){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),
                
                navbarMenu("pruebas",
                           
                           "----",
                           "",
                           tabPanel(" ")),
                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("facintro.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.txt")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               selectInput("ptabla2013", "prueba tabla:", c(datos_df_casen_2013_mil_preg)),
                               column(12, dataTableOutput("prueba_tabla"))
                           )),
                           tabPanel("Segundo módulo (E): Educacion ",
                                    fluidRow(column(6, includeMarkdown("about_educacion.md.txt")),
                                             column(3,  tableOutput("contents2")))),
                           
                           tabPanel("Tercer módulo (O): Trabajo ",
                                    fluidRow(column(6, includeMarkdown("about_trabajo.txt")),
                                             column(3,  tableOutput("contents3")))),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos ",
                                    fluidRow(column(6, includeMarkdown("about_ingresos.txt")),
                                             column(3,  tableOutput("contents4")))),
                           
                           tabPanel("Quinto módulo (S): Salud ",
                                    fluidRow(column(6, includeMarkdown("about_salud.txt")),
                                             column(3,  tableOutput("contents5")))),
                           
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación ",
                                    fluidRow(column(6, includeMarkdown("about_identidades.txt")),
                                             column(3,  tableOutput("contents6")))),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno ",
                                    fluidRow(column(6, includeMarkdown("about_vivienda.txt")),
                                             column(3,  tableOutput("contents7")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Ingresos corregidos",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Estadísticas",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           tabPanel("Tabla educacion", tableOutput("contents_educacion")),
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           selectInput("ptabla2013_primerav", "ingrese primera variable:", c()),
                           selectInput("ptabla2013_segundav", "ingrese segunda variable:", c()),
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5, verbatimTextOutput("tabla_d_c13")))),
                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3, verbatimTextOutput("tabla_chi13"))))
                ) 
            )
        }
        
        else if (user == 2015){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),
                
                navbarMenu("pruebas",
                           
                           "----",
                           "",
                           tabPanel(" ")),
                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("facintro.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.txt")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               selectInput("ptabla2013", "prueba tabla:", c(datos_df_casen_2013_mil_preg)),
                               column(12, dataTableOutput("prueba_tabla"))
                           )),
                           tabPanel("Segundo módulo (E): Educacion ",
                                    fluidRow(column(6, includeMarkdown("about_educacion.md.txt")),
                                             column(3,  tableOutput("contents2")))),
                           
                           tabPanel("Tercer módulo (O): Trabajo ",
                                    fluidRow(column(6, includeMarkdown("about_trabajo.txt")),
                                             column(3,  tableOutput("contents3")))),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos ",
                                    fluidRow(column(6, includeMarkdown("about_ingresos.txt")),
                                             column(3,  tableOutput("contents4")))),
                           
                           tabPanel("Quinto módulo (S): Salud ",
                                    fluidRow(column(6, includeMarkdown("about_salud.txt")),
                                             column(3,  tableOutput("contents5")))),
                           
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación ",
                                    fluidRow(column(6, includeMarkdown("about_identidades.txt")),
                                             column(3,  tableOutput("contents6")))),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno ",
                                    fluidRow(column(6, includeMarkdown("about_vivienda.txt")),
                                             column(3,  tableOutput("contents7")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Ingresos corregidos",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Estadísticas",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           tabPanel("Tabla educacion", tableOutput("contents_educacion")),
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           selectInput("ptabla2015_primerav", "ingrese primera variable:", c()),
                           selectInput("ptabla2015_segundav", "ingrese segunda variable:", c()),
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5, verbatimTextOutput("tabla_d_c15")))),
                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3, verbatimTextOutput("tabla_chi15"))))
                ) 
            )
        }
        
        else if (user == 2017){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),
                
                navbarMenu("pruebas",
                           
                           "----",
                           "",
                           tabPanel(" ")),
                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("facintro.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.txt")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               selectInput("ptabla2017", "prueba tabla:", c(datos_df_casen_2017_mil_preg)),
                               column(12, dataTableOutput("prueba_tabla"))
                           )),
                           tabPanel("Segundo módulo (E): Educacion ",
                                    fluidRow(column(6, includeMarkdown("about_educacion.md.txt")),
                                             selectInput("ptabla2017edu", "prueba tabla:", c(datos_df_casen_2017_mil_pregedu)),
                                             column(3,  tableOutput("prueba_tablaedu")))),
                           
                           tabPanel("Tercer módulo (O): Trabajo ",
                                    fluidRow(column(6, includeMarkdown("about_trabajo.txt")),
                                             column(3,  tableOutput("contents3")))),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos ",
                                    fluidRow(column(6, includeMarkdown("about_ingresos.txt")),
                                             column(3,  tableOutput("contents4")))),
                           
                           tabPanel("Quinto módulo (S): Salud ",
                                    fluidRow(column(6, includeMarkdown("about_salud.txt")),
                                             column(3,  tableOutput("contents5")))),
                           
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación ",
                                    fluidRow(column(6, includeMarkdown("about_identidades.txt")),
                                             column(3,  tableOutput("contents6")))),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno ",
                                    fluidRow(column(6, includeMarkdown("about_vivienda.txt")),
                                             column(3,  tableOutput("contents7")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Ingresos corregidos",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Estadísticas",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           tabPanel("Tabla educacion", tableOutput("contents_educacion")),
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5,
                                                                                    selectInput("ptabla2017_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                                    selectInput("ptabla2017_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                    verbatimTextOutput("tabla_d_c")))),
                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3,
                                                                                 selectInput("ptabla2017_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                                 selectInput("ptabla2017_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                 verbatimTextOutput("tabla_chi"))))
                           
                           
                           
                           
                           
                ) ,
                
                navbarMenu("Tablas de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(column(7,
                                                                                    selectInput("ptabla2017_primeravx", "ingrese primera variable:", c(datos_df_exp)),
                                                                                    selectInput("ptabla2017_segundavx", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_terceravx", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                    verbatimTextOutput("tabla_d_c_generalizada")))),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(3,
                                                                                 selectInput("ptabla2017_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                                 selectInput("ptabla2017_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2017_tercerav", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                 verbatimTextOutput("tabla_chi_generalizada"))))
                           
                           
                           
                           
                           
                )
                
            )
        }
        
        
        
        
    })
    
    output$promedios_filtros<-renderTable({
        
        a <- input$nivel_filtro
        b <- input$categoria_filtro

        base_del_2006 <- mydata_educacion_1000()
        
        
        
        
        base_del_2006_terr <- base_del_2006[,b]
        
        base_del_2006_terr[is.na(base_del_2006_terr)] <- 0
        
        
        
        base_del_2006_cat <- base_del_2006[,a]
        
        promedios <- data.frame(aggregate(base_del_2006_terr, base_del_2006_cat, mean))

        return((promedios))

    })
    
    

    
    
    mydata_educacion_exp <- reactive({
        datos_dfe <- dataset[, 1:32]
        
        return(datos_dfe)
    })
    
    
    
    mydata_educacion_1000 <- reactive({
        datos_dfe <- datos_df_1000[, 1:32]
        datos_dfe
        return(datos_dfe)
    })
    
    
    mydata_educacion_2000 <- reactive({
        datos_dfe <- datos_df_2000[, 1:5]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_3000 <- reactive({
        datos_dfe <- datos_df_casen_2009_mil_mn[, 1:34]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_4000 <- reactive({
        datos_dfe <- datos_df_casen_2011_mil_mn[, 1:34]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_5000 <- reactive({
        datos_dfe <- datos_df_casen_2011_mil_ymt[, 1:24]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_6000 <- reactive({
        datos_dfe <- datos_df_casen_2013_mil[, 1:600]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_7000 <- reactive({
        datos_dfe <- datos_df_casen_2015_mil[, 1:776]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_8000 <- reactive({
        datos_dfe <- datos_df_casen_2017_mil[, 1:808]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion <- reactive({
        datos_dfe <- datos_df_casen_2017_miledu[, 43:102]
        datos_dfe 
        return(datos_dfe)
    })
    
    # estas lineas on muy importantes:
    ############
    ############
    
    output$table <- renderDataTable(mydata_educacion_1000())
    output$table2009ymt <- renderDataTable(mydata_educacion_2000())
    output$table2009mn <- renderDataTable(mydata_educacion_3000())
    output$table2011mn <- renderDataTable(mydata_educacion_4000())
    output$table20ymt <- renderDataTable(mydata_educacion_5000())
    output$table2013 <- renderDataTable(mydata_educacion_6000())
    output$table2015 <- renderDataTable(mydata_educacion_7000())
    output$table2017 <- renderDataTable(mydata_educacion_8000())
    
    
    ########################################################################## 2006  ##########################################################################  
    
    
    
    output$cyb <- renderPlot({
        a <- input$ptabla_cyb
        
        preguntaseternas2001_chi <- mydata_educacion_1000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    
    
    
    
    output$promedios<-renderPrint({
        a <- input$ptabla_promedios
        preguntaseternas2001_chi <- mydata_educacion_1000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
        return(b)
        
    })
    
    ########################################################################## 2009 ymt  ######################################################################
    
    
    
    output$cyb_2009 <- renderPlot({
        a <- input$ptabla_cyb_2009
        
        preguntaseternas2001_chi <- mydata_educacion_2000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    output$promedios_2009<-renderPrint({
        a <- input$ptabla_promedios_2009
        preguntaseternas2001_chi <- mydata_educacion_2000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
        return(b)
        
    })
    
    
    ########################################################################## 2009 mn  ######################################################################
    
    
    
    output$cyb_2009_mn <- renderPlot({
        a9mn <- input$ptabla_cyb_2009_mn
        
        preguntaseternas2001_chi <- mydata_educacion_3000()
        preguntaseternas_sub2001_a9mn <- preguntaseternas2001_chi[,a9mn]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a9mn, y = unlist(preguntaseternas_sub2001_a9mn), fill = a9mn)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    output$promedios_2009_mn<-renderPrint({
        a9mn <- input$ptabla_promedios_2009_mn
        preguntaseternas2001_chi <- mydata_educacion_3000()
        preguntaseternas_sub2001_a9mn <- preguntaseternas2001_chi[,a9mn]
        b <- summary(preguntaseternas_sub2001_a9mn)
        return(b)
        
    })
    
    
    
    
    ########################################################################## 2011 mn  ######################################################################
    
    output$cyb_2011 <- renderPlot({
        a <- input$ptabla_cyb_2011
        
        preguntaseternas2001_chi <- mydata_educacion_4000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    
    
    
    
    output$promedios_2011<-renderPrint({
        a <- input$ptabla_2011
        preguntaseternas2001_chi <- mydata_educacion_4000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
        return(b)
        
    })
    
    
    ########################################################################## 2011 ymt  ######################################################################
    
    output$cyb_2011_ymt <- renderPlot({
        a <- input$ptabla_cyb_2011_ymt
        
        preguntaseternas2001_chi <- mydata_educacion_5000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    
    
    
    
    output$promedios_2011_ymt<-renderPrint({
        a <- input$ptabla_2011_ymt
        preguntaseternas2001_chi <- mydata_educacion_5000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
        return(b)
        
    })
    
    
    ########################################################################## 2013  ##########################################################################  
    
    
    output$tabla_d_c15<-renderPrint({
        a <- input$ptabla2015_primerav
        b <- input$ptabla2015_segundav
        preguntaseternas2001_ab <- mydata_educacion_6000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab15 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab15)
    })
    
    output$tabla_chi15<-renderPrint({
        a <- input$ptabla2015_primerav
        b <- input$ptabla2015_segundav
        preguntaseternas2001_ab <- mydata_educacion_6000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab15 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado15 <- chisq.test(cross_tab15)
        return(chicuadrado15)
    })
    
    
    ########################################################################## 2015  ##########################################################################  
    
    
    output$tabla_d_c13<-renderPrint({
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        preguntaseternas2001_ab <- mydata_educacion_7000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab13 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab13)
    })
    
    output$tabla_chi13<-renderPrint({
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        preguntaseternas2001_ab <- mydata_educacion_7000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab13 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado13 <- chisq.test(cross_tab13)
        return(chicuadrado13)
    })
    
    
    ########################################################################## 2017  ##########################################################################  
    
    
    output$tabla_d_c_generalizada<-renderPrint({
        d <- input$ptabla2017_primeravx
        e <- input$ptabla2017_segundavx
        f <- input$ptabla2017_terceravx
        preguntaseternas2001_ab <- mydata_educacion_exp()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
        preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
        
        # cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        
        cross_tab = table(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b, preguntaseternas_sub2001_c)
        
        return(cross_tab)
    })
    
    
    
    output$tabla_d_c<-renderPrint({
        a <- input$ptabla2017_primerav
        b <- input$ptabla2017_segundav
        preguntaseternas2001_ab <- mydata_educacion_exp()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab)
    })
    
    output$tabla_chi<-renderPrint({
        a <- input$ptabla2017_primerav
        b <- input$ptabla2017_segundav
        preguntaseternas2001_ab <- mydata_educacion_exp()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado <- chisq.test(cross_tab)
        return(chicuadrado)
    })
    
    
    #########################################################################################################################################
    #########################################################################################################################################
    #########################################################################################################################################
    
    
    
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("input$plot3",'.png',sep='')},
        content = function(file){
            ggsave(file,plot=ggplot(mydata_educacion_1000(), aes(mydata_educacion_1000()$"Ingreso Del Trabajo")) + geom_density())})
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            "la_data.csv"
        },
        content = function(file) {
            write.table(mydata_educacion_1000(), file)
        }
    )
    
    ################################################################################################################################3
    
    
    
    output$downloadPlot2009 <- downloadHandler(
        filename = function(){paste("input$plot3",'.png',sep='')},
        content = function(file){
            ggsave(file,plot=ggplot(mydata_educacion_2000(), aes(mydata_educacion_2000()$"Ingreso Del Trabajo")) + geom_density())})
    
    
    output$downloadData2009 <- downloadHandler(
        filename = function() {
            "la_data.csv"
        },
        content = function(file) {
            write.table(mydata_educacion_2000(), file)
        }
    )
    
    
    
    ################################################################################################################################3
    output$prueba_tabla <- renderDataTable({
        
        if(input$variable_anio == 2006)
        {
            preguntaseternas2001 <- mydata_educacion_1000()
            preguntaseternas_sub2001 <- preguntaseternas2001[,input$ptabla]
            w2001 = table(preguntaseternas_sub2001)
            t = as.data.frame(w2001)
            
            
            
        } 
        
        else if(input$variable_anio == 20090)
        {
            preguntas20090 <- mydata_educacion_2000()
            preguntaseternas_sub2001 <- preguntas20090[,input$ptabla20090]
            w2002 = table(preguntaseternas_sub2001)
            t2 = as.data.frame(w2002)
        } 
        
        else if(input$variable_anio == 20091)
        {
            preguntas20091 <- mydata_educacion_3000()
            preguntaseternas_sub2002 <- preguntas20091[,input$ptabla20091]
            w2003 = table(preguntaseternas_sub2002)
            t3 = as.data.frame(w2003)
        } 
        
        else if(input$variable_anio == 20110)
        {
            preguntas20110 <- mydata_educacion_4000()
            preguntaseternas_sub20110 <- preguntas20110[,input$ptabla20110]
            w2004 = table(preguntaseternas_sub20110)
            t = as.data.frame(w2004)
        } 
        else if(input$variable_anio == 20111)
        {
            preguntaseternas20111<- mydata_educacion_5000()
            preguntaseternas_sub20111 <- preguntaseternas20111[,input$ptabla20111]
            w2005 = table(preguntaseternas_sub20111)
            t = as.data.frame(w2005)
        } 
        else if(input$variable_anio == 2013)
        {
            preguntaseternas2013<- mydata_educacion_6000()
            preguntaseternas_sub2013 <- preguntaseternas2013[,input$ptabla2013]
            w2006 = table(preguntaseternas_sub2013)
            t = as.data.frame(w2006)
        } 
        else if(input$variable_anio == 2015)
        {
            preguntaseternas2015<- mydata_educacion_7000()
            preguntaseternas_sub2015 <- preguntaseternas2015[,input$ptabla2015]
            w2006 = table(preguntaseternas_sub2015)
            t = as.data.frame(w2006)
        } 
        else if(input$variable_anio == 2017)
        {
            preguntaseternas2017<- mydata_educacion_8000()
            preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla2017]
            w2007 = table(preguntaseternas_sub2017)
            t = as.data.frame(w2007)
            
            
            
            
        } 
    })
    ################################################################################################################################3
    output$prueba_tablaedu <- renderDataTable({
        
        
        if(input$variable_anio == 2017)
        {
            preguntaseternas2017<- mydata_educacion()
            preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla2017edu]
            w2007 = table(preguntaseternas_sub2017)
            t = as.data.frame(w2007)
            
        } 
    })
}

options(warn = oldw)

# Correr la aplicacion
shinyApp(ui = ui, server = server)
