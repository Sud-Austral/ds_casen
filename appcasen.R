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
library(writexl)

oldw <- getOption("warn")
options(warn = -1)

# archivo necesario para generar las tablas de contingencia
casen_no_humano <- read.csv('Casen_no_humano.csv')

# cargamos los dd para la generacion de las tablas de contingencia
dataset_colnames <- colnames(casen_no_humano)

#dataset = read_sav("Casen_no_humano.csv")



data_2006  <- read_xlsx("casen_2006_mil.xlsx")

data_2006_primer_modulo <- data_2006[, 1:32]
data_2006_primer_modulo_colnames <- colnames(data_2006_primer_modulo)

# con estos subsets cargamos los nombres de los ddl de los filtros de la casen 2006
data_2006_filtros_terr <- data_2006[, 1:2]
data_2006_filtros_terr_ddl <- colnames(data_2006_filtros_terr)

data_2006_filtros_cat <- data_2006[, 9:32]
data_2006_filtros_cat_ddl <- colnames(data_2006_filtros_cat)




data_2009 <- read_xlsx("casen_2009_mil_ymt.xlsx")
data_2009_ymt_colnames <- data_2009[, 1:5]
data_2009_ymt_colnames <- colnames(datos_2009_ymt_colnames)

data_2009_mn <- read_xlsx("casen_2009_mil_mn.xlsx")
data_2009_mn_colnames <- data_2009_mn[, 1:34]
data_2009_mn_colnames <- colnames(data_2009_mn_colnames)


data_2011 <- read_xlsx("casen_2011_mil_ymt.xlsx")
data_2011_ymt_colnames  <- data_2011[, 1:24]
data_2011_ymt_colnames <- colnames(data_2011_ymt_colnames)


data_2011_mn <- read_xlsx("casen_2011_mil_mn.xlsx")
data_2011_mn_colnames <- data_2011_mn[, 1:34]
data_2011_mn_colnames <- colnames(data_2011_mn_colnames)

data_2013<- read_xlsx("casen_2013_mil.xlsx")
data_2013_colnames_total <- casen_2013[, 1:600]
data_2013_colnames_total <- colnames(data_2013_colnames_total)

###############################################################################
######################  modulos casen_2013 ########################
###############################################################################

data_2013_colnames_1_18 <- data_2013[, 1:18]
data_2013_colnames_1_18 <- colnames(data_2013_colnames_1_18)

###############################################################################
######################  ########################
###############################################################################


data_2015 <- read_xlsx("casen_2015_mil.xlsx")
data_2015_colnames_total  <- data_2015[, 1:776]
data_2015_colnames_total <- colnames(data_2015_colnames_total )


data_2017 <- read_xlsx("casen_2017_mil.xlsx")
data_2017_colnames_total <- data_2017[, 1:808]
data_2017_colnames_total <- colnames(data_2017_colnames_total)


###############################################################################
######################  modulos casen_2017 ########################
###############################################################################




data_2017_colnames_43_102 <- data_2017[, 43:102]
data_2017_colnames_43_102 <- colnames(data_2017_colnames_43_102)


















