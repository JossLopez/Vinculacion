#--------#
#LIBRERIAS
#--------#
library(shiny)
library(shinythemes)
library(shinyalert)
library(markdown)
library(bslib)

library(plotly)
library(EpiModel)
library(WriteXLS)
library(readxl)

#--------#
#CARGA TASAS_UIO.xlsx
#--------#

#Path
setwd('C:/Users/Jairo/Desktop/Jairo/UCE/Vinculacion/Nueva_Propuesta') 
#Cargamos Cte
load("Cte.RData")
#Cargamos Tasas UIO
Tasas1 <- read_excel("TASAS_UIO.xlsx", col_names = TRUE,
                     col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))
head(Tasas1)
tasas1<- as.matrix(Tasas1)
tasas1<-(tasas1+t(tasas1))/2
tasas2<-matrix(data=rep(0,25), nrow=5) 
tasas2[,]<-tasas1[,] 
tasas<-tasas2*Cte
tasas3<-as.vector(tasas)
tasas<-matrix(tasas3,nrow=5,ncol=5)


#--------#
# FLUID PAGE
#--------#

ui <- fluidPage(
  theme = shinytheme("simplex"),
  navbarPage(
    "CONTAGIOS POR COVID",
    navbarMenu("Idioma",
               
               #--------#
               #ESPAÑOL
               #--------#
               
               tabPanel(span(img(src = "español.png", height = 20),"Español"),
                        tabsetPanel(
                          
                          #--------#
                          #ESPAÑOL - INTRODUCCIÓN
                          #--------#
                          
                          tabPanel(
                            "ACERCA DE",
                            titlePanel(strong("¿ACERCA DE LA INVESTIGACIÓN?")),
                            titlePanel(h4("Aqui se pone Informacion de la investigación")),
                            
                            titlePanel(strong("¿ACERCA DE LA APP?")),
                            titlePanel(h4("Aqui se pone Informacion del uso de la app"))
                          ),
                          
                          #--------#
                          #ESPAÑOL - DATOS SIMULADOS
                          #--------#
                          
                          tabPanel(
                            "DATOS SIMULADOS",
                            titlePanel(strong("CONTAGIOS EN QUITO"),),
                            sidebarPanel(
                              titlePanel(h3("Parámetros")),
                              
                              #RANGO ETARIOS
                              selectInput(inputId = "rangoEtario", 
                                          label =h5("Rango etario ", 
                                                    actionButton("infoRango", "?")),
                                          choices = list("20 a 59 años" = 1, 
                                                         "60 o más" = 2), 
                                          selected = 1),
                              
                              
                              #Variante
                              selectInput(inputId = "variante", 
                                          label = h5("Variante de covid", 
                                                     actionButton("infoVariante", "?")), 
                                          choices = list("Alfa" = 1, 
                                                         "Delta" = 2), 
                                          selected = 1),
                              
                              #Vacuna
                              selectInput(inputId = "vacuna", 
                                          label = h5("Vacuna", 
                                                     actionButton("infoVacuna", "?")), 
                                          choices = list("Pfizer" = 1, 
                                                         "Sinovac" = 2,
                                                         "AstraZeneca"=3), 
                                          selected = 1),
                              
                            ), # sidebarPanel
                            mainPanel(
                              h1("SIMULACION"),
                              plotlyOutput(outputId = "grafico")
                            )
                          ),
                          
                          #--------#
                          #ESPAÑOL - DATOS MANUALES
                          #--------#
                          
                          tabPanel(
                            "DATOS MANUALES",
                            titlePanel(strong("CONTAGIOS EN QUITO"),),
                            sidebarPanel(
                              titlePanel(h3("Parámetros")),
                              sliderInput("varBetaI", label = "Valor Beta I", min = 0, 
                                          max = 0.1, value = 0.05),
                              actionButton("infoBetaI", "?"),
                              
                              sliderInput("varBetaE", label = "Valor Beta E", min = 0, 
                                          max = 0.1, value = 0.05),
                              actionButton("infoBetaE", "?"),
                              
                              sliderInput("varVacu", label = "Datos vacuna", min = 0, 
                                          max = 1, value = 0.05),
                              actionButton("infoDVacuna", "?"),
                              
                            ), # sidebarPanel
                            mainPanel(
                              h1("Header 1"),
                            )
                          )
                        )
                        
               ),
               
               #--------#
               #PORTUGUES
               #--------#
               
               tabPanel(span(img(src = "portugues.png", height = 20),"Português"), 
                        tabsetPanel(
                          
                          #--------#
                          #PORTUGUES - sobre o aplicativo
                          #--------#
                          
                          
                          tabPanel(
                            "SOBRE",
                            titlePanel(strong("¿SOBRE PESQUISA")),
                            titlePanel(h4("Informações de pesquisa")),
                            titlePanel(strong("¿SOBRE O APLICATIVO?")),
                            titlePanel(h4("Informações sobre o uso do aplicativo"))
                          ),
                          
                          #--------#
                          #PORTUGUES - DADOS SIMULADOS
                          #--------#
                          
                          tabPanel(
                            "DADOS SIMULADOS",
                            sidebarPanel(
                              
                              titlePanel(h3("Parâmetros")),
                              
                              #RANGO ETARIOS
                              selectInput(inputId = "rangoEtario", 
                                          label = h5("Faixa etária", 
                                                     actionButton("infoFaixa", "?")), 
                                          choices = list("20 a 59 ano" = 1, 
                                                         "60 o más/mais" = 2), 
                                          selected = 1),
                              
                              #Variante
                              selectInput(inputId = "variante", 
                                          label = h5("variante covid", 
                                                     actionButton("infoVariante1", "?")), 
                                          choices = list("Alfa" = 1, 
                                                         "Delta" = 2,
                                                         "Otra" = 3), 
                                          selected = 1),
                              
                              #Vacuna
                              selectInput(inputId = "vacuna", 
                                          label = h5("vacina", 
                                                     actionButton("infoVacina", "?")), 
                                          choices = list("Pfizer" = 1, 
                                                         "Sinovac" = 2,
                                                         "AstraZeneca"=3,
                                                         "Otra" = 4), 
                                          selected = 1),
                              
                            ), # sidebarPanel
                            mainPanel(
                              h1("Header YU"),
                              
                            )
                          ),
                          
                          #--------#
                          #PORTUGUES - DADOS MANUAIS
                          #--------#
                          
                          tabPanel(
                            "DADOS MAUAIS",
                            sidebarPanel(
                              titlePanel(h3("Parâmetros")),
                              #Datos Manuales
                              h3("Dados manuais"),
                              sliderInput("varBetaI", label = "Valor Beta I", min = 0, 
                                          max = 0.1, value = 0.05),
                              actionButton("infovarBetaI1", "?"),
                              
                              sliderInput("varBetaE", label = "Valor Beta E", min = 0, 
                                          max = 0.1, value = 0.05),
                              actionButton("infovarBetaE1", "?"),
                              
                              sliderInput("varVacu", label = "dados da vacina", min = 0, 
                                          max = 1, value = 0.05),
                              actionButton("infovarVacu1", "?"),
                              
                            ),
                            mainPanel(
                              h1("Header OP"),
                            )
                          ),
                        )
               ) 
    )
    
  ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  #bs_themer() error en esto
  
  #--------#
  #OUTPUT - GRAFICO ESPAÑOL
  #--------#

  output$grafico <- renderPlotly({
    
    #############
    #  DATOS SIN MITIGACION
    #############
    
    # n <- switch(input$variante, input$rangoEtario,3)
    # beta_I<-switch(n,0.02,0.043,0.09)
    # 
    # beta_E<-switch(n,0.02,0.043,0.09)
    
    if (input$variante == 1) {
      
      if (input$rangoEtario == 1) {
        
        beta_I<-0.02
        beta_E<-0.02
        
      }else{
        
        beta_I<-0.043
        beta_E<-0.043
        
      }
      
    }else{
      
      beta_I<-0.09
      beta_E<-0.09
      
    }
    
    if (input$variante == 3){
      beta_I = as.numeric(input$varBetaI)
      beta_E = as.numeric(input$varBetaE)
    }
    
    param <- param.dcm( beta_I = beta_I , beta_E = beta_E , #Mira la diferencia entre los dos mirando la carga viral
                        e1.dur = 3.69, i1.dur = 3.47, e2.dur = 3.69, i2.dur = 3.47, 
                        e3.dur = 3.69, i3.dur = 3.47, e4.dur = 3.69, i4.dur = 3.47,
                        e5.dur = 3.69, i5.dur = 3.47, mu=0, #mu es la tasa de mortalidad anual (número de muertes en un día) estacionaria N=O
                        tasas_11 = tasas[1,1],tasas_12 = tasas[1,2],tasas_13 = tasas[1,3],tasas_14 = tasas[1,4],tasas_15 = tasas[1,5],
                        tasas_21 = tasas[2,1],tasas_22 = tasas[2,2],tasas_23 = tasas[2,3],tasas_24 = tasas[2,4],tasas_25 = tasas[2,5],
                        tasas_31 = tasas[3,1],tasas_32 = tasas[3,2],tasas_33 = tasas[3,3],tasas_34 = tasas[3,4],tasas_35 = tasas[3,5],
                        tasas_41 = tasas[4,1],tasas_42 = tasas[4,2],tasas_43 = tasas[4,3],tasas_44 = tasas[4,4],tasas_45 = tasas[4,5],
                        tasas_51 = tasas[5,1],tasas_52 = tasas[5,2],tasas_53 = tasas[5,3],tasas_54 = tasas[5,4],tasas_55 = tasas[5,5]
    )
    
    init <- init.dcm(s1.num = 676995, e1.num = 5502, i1.num = 18262, r1.num =5166,
                     s2.num = 140454, e2.num = 17450, i2.num = 59883, r2.num = 17179,
                     s3.num = 466800, e3.num = 43180, i3.num = 144791, r3.num = 41138,
                     s4.num = 587173, e4.num = 21960, i4.num = 67733, r4.num = 18531,
                     s5.num = 144490, e5.num = 17383, i5.num = 57587, r5.num = 16277)
    
    SEIR2 <- function(t, t0, parms) {
      with(as.list(c(t0, parms)), {
        # Tamaño de la población
        num1 <- s1.num + e1.num + i1.num + r1.num 
        num2 <- s2.num + e2.num + i2.num + r2.num
        num3 <- s3.num + e3.num + i3.num + r3.num
        num4 <- s4.num + e4.num + i4.num + r4.num
        num5 <- s5.num + e5.num + i5.num + r5.num
        num<-num1+num2+num3+num4+num5
        delta1 <- 1/ e1.dur
        delta2 <- 1/ e2.dur
        delta3 <- 1/ e3.dur
        delta4 <- 1/ e4.dur
        delta5 <- 1/ e5.dur
        nu1<-1/i1.dur
        nu2<-1/i2.dur
        nu3<-1/i3.dur
        nu4<-1/i4.dur
        nu5<-1/i5.dur
        dS1 <-mu*num1 -beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                        tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                        tasas_15*(i5.num/num5))-
          beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                           tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                           tasas_15*(e5.num/num5))-mu*s1.num
        dE1 <- beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                                tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                                tasas_15*(e5.num/num5))-delta1*e1.num-mu*e1.num
        dI1 <- beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                tasas_15*(i5.num/num5))+delta1*e1.num-
          nu1*i1.num-mu*i1.num
        dR1 <- nu1*i1.num-mu*r1.num
        dS2 <-mu*num2 -beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                        tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                        tasas_25*(i5.num/num5))-
          beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                           tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                           tasas_25*(e5.num/num5))-mu*s2.num
        dE2 <-beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                               tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                               tasas_25*(e5.num/num5))-delta2*e2.num-mu*e2.num
        dI2 <- beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                tasas_25*(i5.num/num5))+delta2*e2.num-
          nu2*i2.num-mu*i2.num
        dR2 <- nu2*i2.num-mu*r2.num
        dS3 <-mu*num3 -beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                                        tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                                        tasas_35*(i5.num/num5))-
          beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                           tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                           tasas_35*(e5.num/num5))-mu*s3.num
        dE3 <-beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                               tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                               tasas_35*(e5.num/num5))-delta3*e3.num-mu*e3.num
        dI3 <-beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                               tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                               tasas_35*(i5.num/num5))+delta3*e3.num-
          nu3*i3.num-mu*i3.num
        dR3 <- nu3*i3.num-mu*r3.num
        dS3 <-mu*num3 -beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                                        tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                                        tasas_35*(i5.num/num5))-
          beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                           tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                           tasas_35*(e5.num/num5))-mu*s3.num
        dE3 <-beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                               tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                               tasas_35*(e5.num/num5))-delta3*e3.num-mu*e3.num
        dI3 <-beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                               tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                               tasas_35*(i5.num/num5))+delta3*e3.num-
          nu3*i3.num-mu*i3.num
        dR3 <- nu3*i3.num-mu*r3.num
        dS4 <-mu*num4 -beta_I*s4.num*(tasas_41*(i1.num/num1)+tasas_42*(i2.num/num2)+
                                        tasas_43*(i3.num/num3)+tasas_44*(i4.num/num4)+
                                        tasas_45*(i5.num/num5))-
          beta_E*s4.num*(tasas_41*(e1.num/num1)+tasas_42*(e2.num/num2)+
                           tasas_43*(e3.num/num3)+tasas_44*(e4.num/num4)+
                           tasas_45*(e5.num/num5))-mu*s4.num
        dE4 <- beta_E*s4.num*(tasas_41*(e1.num/num1)+tasas_42*(e2.num/num2)+
                                tasas_43*(e3.num/num3)+tasas_44*(e4.num/num4)+
                                tasas_45*(e5.num/num5))-delta4*e4.num-mu*e4.num
        dI4 <- beta_I*s4.num*(tasas_41*(i1.num/num1)+tasas_42*(i2.num/num2)+
                                tasas_43*(i3.num/num3)+tasas_44*(i4.num/num4)+
                                tasas_45*(i5.num/num5))+delta4*e4.num-
          nu4*i4.num-mu*i4.num
        dR4 <- nu4*i4.num-mu*r4.num
        dS5 <-mu*num5 -beta_I*s5.num*(tasas_51*(i1.num/num1)+tasas_52*(i2.num/num2)+
                                        tasas_53*(i3.num/num3)+tasas_54*(i4.num/num4)+
                                        tasas_55*(i5.num/num5))-
          beta_E*s5.num*(tasas_51*(e1.num/num1)+tasas_52*(e2.num/num2)+
                           tasas_53*(e3.num/num3)+tasas_54*(e4.num/num4)+
                           tasas_55*(e5.num/num5))-mu*s5.num
        dE5 <- beta_E*s5.num*(tasas_51*(e1.num/num1)+tasas_52*(e2.num/num2)+
                                tasas_53*(e3.num/num3)+tasas_54*(e4.num/num4)+
                                tasas_55*(e5.num/num5))-delta5*e5.num-mu*e5.num
        dI5 <- beta_I*s5.num*(tasas_51*(i1.num/num1)+tasas_52*(i2.num/num2)+
                                tasas_53*(i3.num/num3)+tasas_54*(i4.num/num4)+
                                tasas_55*(i5.num/num5))+delta5*e5.num-
          nu5*i5.num-mu*i5.num
        dR5 <- nu5*i5.num-mu*r5.num
        list(c(dS1, dE1, dI1, dR1, 
               dS2, dE2, dI2, dR2,
               dS3, dE3, dI3, dR3,
               dS4, dE4, dI4, dR4,
               dS5, dE5, dI5, dR5
        ))
      })
    }
    
    control <- control.dcm(nsteps = 40, dt = 1, new.mod = SEIR2)
    
    mod <- dcm(param, init, control)
    
    M1 <- mod$epi$i1.num$run1[40]+mod$epi$e1.num$run1[40]+mod$epi$r1.num$run1[40] #masa infectada en el carril 1 en el día 150
    M2 <- mod$epi$i2.num$run1[40]+mod$epi$e2.num$run1[40]+mod$epi$r2.num$run1[40] #masa infectada en el carril 2 en el día 150
    M3 <- mod$epi$i3.num$run1[40]+mod$epi$e3.num$run1[40]+mod$epi$r3.num$run1[40] #masa infectada en el carril 3 en el día 150
    M4 <- mod$epi$i4.num$run1[40]+mod$epi$e4.num$run1[40]+mod$epi$r4.num$run1[40] #masa infectada en el carril 4 en el día 150
    M5 <- mod$epi$i5.num$run1[40]+mod$epi$e5.num$run1[40]+mod$epi$r5.num$run1[40] #masa infectada en el carril 5 en el día 150
    
    
    #############
    #  DATAFRAME PARA PLOTLY CON DATOS SIN MITIGACION
    #############
    
    label_ge <- c("0 a 14 años","15 a 19 años","20 a 34 años","35 a 59 años","60 o más años")
    
    sinMitigacionP <- c(M1,M2,M3,M4,M5)
    
    
    #############
    #  VACUNAS 20 - 59
    #############
    
    SEIR2 <- function(t, t0, parms) {
      with(as.list(c(t0, parms)), {
        # Population size
        num1 <- s1.num + e1.num + i1.num + r1.num 
        num2 <- s2.num + e2.num + i2.num + r2.num
        num3 <- s3.num + e3.num + i3.num + r3.num
        num4 <- s4.num + e4.num + i4.num + r4.num
        num5 <- s5.num + e5.num + i5.num + r5.num
        num<-num1+num2+num3+num4+num5
        delta1 <- 1/ e1.dur
        delta2 <- 1/ e2.dur
        delta3 <- 1/ e3.dur
        delta4 <- 1/ e4.dur
        delta5 <- 1/ e5.dur
        nu1<-1/i1.dur
        nu2<-1/i2.dur
        nu3<-1/i3.dur
        nu4<-1/i4.dur
        nu5<-1/i5.dur
        
        ###### S1
        
        dS1 <-mu*num1 -beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                        tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                        tasas_15*(i5.num/num5))-
          beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                           tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                           tasas_15*(e5.num/num5))-mu*s1.num
        dE1 <- beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                                tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                                tasas_15*(e5.num/num5))-delta1*e1.num-mu*e1.num
        dI1 <- beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                tasas_15*(i5.num/num5))+delta1*e1.num-
          nu1*i1.num-mu*i1.num
        dR1 <- nu1*i1.num-mu*r1.num
        
        ###### S2
        
        dS2 <-mu*num2 -beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                        tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                        tasas_25*(i5.num/num5))-
          beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                           tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                           tasas_25*(e5.num/num5))-mu*s2.num
        dE2 <-beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                               tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                               tasas_25*(e5.num/num5))-delta2*e2.num-mu*e2.num
        dI2 <- beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                tasas_25*(i5.num/num5))+delta2*e2.num-
          nu2*i2.num-mu*i2.num
        dR2 <- nu2*i2.num-mu*r2.num
        
        ###### vacunación a S3
        
        dS3 <-mu*num3 -beta_I*s3.num*(tasas_31*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_32*(1-rb)*(1-p_v)*(i2.num/num2)+
                                        tasas_33*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_34*(1-rb)*(1-p_v)*(i4.num/num4)+
                                        tasas_35*(1-rb)*(1-p_v)*(i5.num/num5))-
          beta_E*s3.num*(tasas_31*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_32*(1-rb)*(1-p_v)*(e2.num/num2)+
                           tasas_33*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_34*(1-rb)*(1-p_v)*(e4.num/num4)+
                           tasas_35*(1-rb)*(1-p_v)*(e5.num/num5))-mu*s3.num
        dE3 <- beta_E*s3.num*(tasas_31*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_32*(1-rb)*(1-p_v)*(e2.num/num2)+
                                tasas_33*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_34*(1-rb)*(1-p_v)*(e4.num/num4)+
                                tasas_35*(1-rb)*(1-p_v)*(e5.num/num5))-delta3*e3.num-mu*e3.num
        dI3 <- beta_I*s3.num*(tasas_31*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_32*(1-rb)*(1-p_v)*(i2.num/num2)+
                                tasas_33*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_34*(1-rb)*(1-p_v)*(i4.num/num4)+
                                tasas_35*(1-rb)*(1-p_v)*(i5.num/num5))+delta3*e3.num-
          nu3*i3.num-mu*i3.num
        
        dR3 <- nu3*i3.num-mu*r3.num
        
        ###### vacunacion a S4
        
        dS4 <-mu*num4 -beta_I*s4.num*(tasas_41*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_42*(1-rb)*(1-p_v)*(i2.num/num2)+
                                        tasas_43*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_44*(1-rb)*(1-p_v)*(i4.num/num4)+
                                        tasas_45*(1-rb)*(1-p_v)*(i5.num/num5))-
          beta_E*s4.num*(tasas_41*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_42*(1-rb)*(1-p_v)*(e2.num/num2)+
                           tasas_43*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_44*(1-rb)*(1-p_v)*(e4.num/num4)+
                           tasas_45*(1-rb)*(1-p_v)*(e5.num/num5))-mu*s4.num
        dE4 <- beta_E*s4.num*(tasas_41*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_42*(1-rb)*(1-p_v)*(e2.num/num2)+
                                tasas_43*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_44*(1-rb)*(1-p_v)*(e4.num/num4)+
                                tasas_45*(1-rb)*(1-p_v)*(e5.num/num5))-delta4*e4.num-mu*e4.num
        dI4 <- beta_I*s4.num*(tasas_41*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_42*(1-rb)*(1-p_v)*(i2.num/num2)+
                                tasas_43*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_44*(1-rb)*(1-p_v)*(i4.num/num4)+
                                tasas_45*(1-rb)*(1-p_v)*(i5.num/num5))+delta4*e4.num-
          nu4*i4.num-mu*i4.num
        
        dR4 <- nu4*i4.num-mu*r4.num
        
        ###### vacunacion a S5
        
        
        dS5 <-mu*num5 -beta_I*s5.num*(tasas_51*(i1.num/num1)+tasas_52*(i2.num/num2)+
                                        tasas_53*(i3.num/num3)+tasas_54*(i4.num/num4)+
                                        tasas_55*(i5.num/num5))-
          beta_E*s5.num*(tasas_51*(e1.num/num1)+tasas_52*(e2.num/num2)+
                           tasas_53*(e3.num/num3)+tasas_54*(e4.num/num4)+
                           tasas_55*(e5.num/num5))-mu*s5.num
        dE5 <- beta_E*s5.num*(tasas_51*(e1.num/num1)+tasas_52*(e2.num/num2)+
                                tasas_53*(e3.num/num3)+tasas_54*(e4.num/num4)+
                                tasas_55*(e5.num/num5))-delta5*e5.num-mu*e5.num
        dI5 <- beta_I*s5.num*(tasas_51*(i1.num/num1)+tasas_52*(i2.num/num2)+
                                tasas_53*(i3.num/num3)+tasas_54*(i4.num/num4)+
                                tasas_55*(i5.num/num5))+delta5*e5.num-
          nu5*i5.num-mu*i5.num
        
        dR5 <- nu5*i5.num-mu*r5.num
        list(c(dS1, dE1, dI1, dR1, 
               dS2, dE2, dI2, dR2,
               dS3, dE3, dI3, dR3,
               dS4, dE4, dI4, dR4,
               dS5, dE5, dI5, dR5
        ))
      })
    }
    
    if (input$vacuna == 1) {
      #Variante
      if(input$variante == 1){
        vacu = 1-((log10(1-8/21720))/(log10(1-162/21728)))
        beta_I<-0.02
        beta_E<-0.02
      }else{
        vacu =0.46
        beta_I<-0.09
        beta_E<-0.09
      }
      
      if (input$variante == 3){
        vacu <- 1-((log10(1-8/21720))/(log10(1-162/21728)))
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
      
    }else if (input$vacuna == 2){
      #Variante
      if(input$variante == 1){
        vacu =0.52
        beta_I<-0.02
        beta_E<-0.02
      }else{
        vacu =0.59
        beta_I<-0.09
        beta_E<-0.09
      }
      if (input$variante == 3){
        vacu <- as.numeric(input$varVacu)
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
    }else{
      #Variante
      if(input$variante == 1){
        vacu =0.70
        beta_I<-0.02
        beta_E<-0.02
      }else{
        vacu =0.36
        beta_I<-0.09
        beta_E<-0.09
      }
      if (input$variante == 3){
        vacu <- as.numeric(input$varVacu)
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
    }
    
    # 30 % de la poblacion vacunada
    
    pob_vacu=0.3
    
    param <- param.dcm( p_v=pob_vacu, rb=vacu, beta_I =beta_I , beta_E = beta_E, #ver la diferencia entre los dos viendo la carga viral
                        e1.dur = 3.69, i1.dur = 3.47, e2.dur = 3.69, i2.dur = 3.47, 
                        e3.dur = 3.69, i3.dur = 3.47, e4.dur = 3.69, i4.dur = 3.47,
                        e5.dur = 3.69, i5.dur = 3.47, mu=0.01, #mu es la tasa de mortalidad estacionaria en el a?o (n?mero de muertes en un d?a) N = O
                        tasas_11 = tasas[1,1],tasas_12 = tasas[1,2],tasas_13 = tasas[1,3],tasas_14 = tasas[1,4],tasas_15 = tasas[1,5],
                        tasas_21 = tasas[2,1],tasas_22 = tasas[2,2],tasas_23 = tasas[2,3],tasas_24 = tasas[2,4],tasas_25 = tasas[2,5],
                        tasas_31 = tasas[3,1],tasas_32 = tasas[3,2],tasas_33 = tasas[3,3],tasas_34 = tasas[3,4],tasas_35 = tasas[3,5],
                        tasas_41 = tasas[4,1],tasas_42 = tasas[4,2],tasas_43 = tasas[4,3],tasas_44 = tasas[4,4],tasas_45 = tasas[4,5],
                        tasas_51 = tasas[5,1],tasas_52 = tasas[5,2],tasas_53 = tasas[5,3],tasas_54 = tasas[5,4],tasas_55 = tasas[5,5]
    )
    
    init <- init.dcm(s1.num = 676995, e1.num = 5502, i1.num = 18262, r1.num =5166,
                     s2.num = 140454, e2.num = 17450, i2.num = 59883, r2.num = 17179,
                     s3.num = 466800, e3.num = 43180, i3.num = 144791, r3.num = 41138,
                     s4.num = 587173, e4.num = 21960, i4.num = 67733, r4.num = 18531,
                     s5.num = 144490, e5.num = 17383, i5.num = 57587, r5.num = 16277)
    
    control <- control.dcm(nsteps = 40, dt = 1, new.mod = SEIR2) 
    
    mod <- dcm(param, init, control)
    
    M1 <- mod$epi$i1.num$run1[40]+mod$epi$e1.num$run1[40]+mod$epi$r1.num$run1[40] #masa infectada en el carril 1 en el día 150
    M2 <- mod$epi$i2.num$run1[40]+mod$epi$e2.num$run1[40]+mod$epi$r2.num$run1[40] #masa infectada en el carril 2 en el día 150
    M3 <- mod$epi$i3.num$run1[40]+mod$epi$e3.num$run1[40]+mod$epi$r3.num$run1[40] #masa infectada en el carril 3 en el día 150
    M4 <- mod$epi$i4.num$run1[40]+mod$epi$e4.num$run1[40]+mod$epi$r4.num$run1[40] #masa infectada en el carril 4 en el día 150
    M5 <- mod$epi$i5.num$run1[40]+mod$epi$e5.num$run1[40]+mod$epi$r5.num$run1[40] #masa infectada en el carril 5 en el día 150
    
    vac30pob20a59P <- c(M1,M2,M3,M4,M5)
    
    datos <- data.frame(label_ge,sinMitigacionP,sinMitigacionP-vac30pob20a59P,vac30pob20a59P)
    # 90 % de la poblacion vacunada
    
    pob_vacu=0.9
    
    param <- param.dcm( p_v=pob_vacu, rb=vacu, beta_I =beta_I , beta_E = beta_E, #ver la diferencia entre los dos viendo la carga viral
                        e1.dur = 3.69, i1.dur = 3.47, e2.dur = 3.69, i2.dur = 3.47, 
                        e3.dur = 3.69, i3.dur = 3.47, e4.dur = 3.69, i4.dur = 3.47,
                        e5.dur = 3.69, i5.dur = 3.47, mu=0.01, #mu es la tasa de mortalidad estacionaria en el a?o (n?mero de muertes en un d?a) N = O
                        tasas_11 = tasas[1,1],tasas_12 = tasas[1,2],tasas_13 = tasas[1,3],tasas_14 = tasas[1,4],tasas_15 = tasas[1,5],
                        tasas_21 = tasas[2,1],tasas_22 = tasas[2,2],tasas_23 = tasas[2,3],tasas_24 = tasas[2,4],tasas_25 = tasas[2,5],
                        tasas_31 = tasas[3,1],tasas_32 = tasas[3,2],tasas_33 = tasas[3,3],tasas_34 = tasas[3,4],tasas_35 = tasas[3,5],
                        tasas_41 = tasas[4,1],tasas_42 = tasas[4,2],tasas_43 = tasas[4,3],tasas_44 = tasas[4,4],tasas_45 = tasas[4,5],
                        tasas_51 = tasas[5,1],tasas_52 = tasas[5,2],tasas_53 = tasas[5,3],tasas_54 = tasas[5,4],tasas_55 = tasas[5,5]
    )
    
    init <- init.dcm(s1.num = 676995, e1.num = 5502, i1.num = 18262, r1.num =5166,
                     s2.num = 140454, e2.num = 17450, i2.num = 59883, r2.num = 17179,
                     s3.num = 466800, e3.num = 43180, i3.num = 144791, r3.num = 41138,
                     s4.num = 587173, e4.num = 21960, i4.num = 67733, r4.num = 18531,
                     s5.num = 144490, e5.num = 17383, i5.num = 57587, r5.num = 16277)
    
    control <- control.dcm(nsteps = 40, dt = 1, new.mod = SEIR2) 
    
    mod <- dcm(param, init, control)
    
    M1 <- mod$epi$i1.num$run1[40]+mod$epi$e1.num$run1[40]+mod$epi$r1.num$run1[40] #masa infectada en el carril 1 en el día 150
    M2 <- mod$epi$i2.num$run1[40]+mod$epi$e2.num$run1[40]+mod$epi$r2.num$run1[40] #masa infectada en el carril 2 en el día 150
    M3 <- mod$epi$i3.num$run1[40]+mod$epi$e3.num$run1[40]+mod$epi$r3.num$run1[40] #masa infectada en el carril 3 en el día 150
    M4 <- mod$epi$i4.num$run1[40]+mod$epi$e4.num$run1[40]+mod$epi$r4.num$run1[40] #masa infectada en el carril 4 en el día 150
    M5 <- mod$epi$i5.num$run1[40]+mod$epi$e5.num$run1[40]+mod$epi$r5.num$run1[40] #masa infectada en el carril 5 en el día 150
    
    vac90pob20a59P <- c(M1,M2,M3,M4,M5)
    #datos$vac30pob20a59<-vac30pob20a59
    datos$vac30pob20a59<-vac30pob20a59P-vac90pob20a59P
    datos$vac90pob20a59P<-vac90pob20a59P
    
    
    
    
    #############
    #  VACUNAS 60 o mas
    #############
    
    SEIR2 <- function(t, t0, parms) {
      with(as.list(c(t0, parms)), {
        # Population size
        num1 <- s1.num + e1.num + i1.num + r1.num 
        num2 <- s2.num + e2.num + i2.num + r2.num
        num3 <- s3.num + e3.num + i3.num + r3.num
        num4 <- s4.num + e4.num + i4.num + r4.num
        num5 <- s5.num + e5.num + i5.num + r5.num
        num<-num1+num2+num3+num4+num5
        delta1 <- 1/ e1.dur
        delta2 <- 1/ e2.dur
        delta3 <- 1/ e3.dur
        delta4 <- 1/ e4.dur
        delta5 <- 1/ e5.dur
        nu1<-1/i1.dur
        nu2<-1/i2.dur
        nu3<-1/i3.dur
        nu4<-1/i4.dur
        nu5<-1/i5.dur
        
        ###### S1
        
        dS1 <-mu*num1 -beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                        tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                        tasas_15*(i5.num/num5))-
          beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                           tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                           tasas_15*(e5.num/num5))-mu*s1.num
        dE1 <- beta_E*s1.num*(tasas_11*(e1.num/num1)+tasas_12*(e2.num/num2)+
                                tasas_13*(e3.num/num3)+tasas_14*(e4.num/num4)+
                                tasas_15*(e5.num/num5))-delta1*e1.num-mu*e1.num
        dI1 <- beta_I*s1.num*(tasas_11*(i1.num/num1)+tasas_12*(i2.num/num2)+
                                tasas_13*(i3.num/num3)+tasas_14*(i4.num/num4)+
                                tasas_15*(i5.num/num5))+delta1*e1.num-
          nu1*i1.num-mu*i1.num
        dR1 <- nu1*i1.num-mu*r1.num
        
        ###### S2
        
        dS2 <-mu*num2 -beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                        tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                        tasas_25*(i5.num/num5))-
          beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                           tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                           tasas_25*(e5.num/num5))-mu*s2.num
        dE2 <-beta_E*s2.num*(tasas_21*(e1.num/num1)+tasas_22*(e2.num/num2)+
                               tasas_23*(e3.num/num3)+tasas_24*(e4.num/num4)+
                               tasas_25*(e5.num/num5))-delta2*e2.num-mu*e2.num
        dI2 <- beta_I*s2.num*(tasas_21*(i1.num/num1)+tasas_22*(i2.num/num2)+
                                tasas_23*(i3.num/num3)+tasas_24*(i4.num/num4)+
                                tasas_25*(i5.num/num5))+delta2*e2.num-
          nu2*i2.num-mu*i2.num
        dR2 <- nu2*i2.num-mu*r2.num
        
        ###### S3
        
        dS3 <-mu*num3 -beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                                        tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                                        tasas_35*(i5.num/num5))-
          beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                           tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                           tasas_35*(e5.num/num5))-mu*s3.num
        dE3 <-beta_E*s3.num*(tasas_31*(e1.num/num1)+tasas_32*(e2.num/num2)+
                               tasas_33*(e3.num/num3)+tasas_34*(e4.num/num4)+
                               tasas_35*(e5.num/num5))-delta3*e3.num-mu*e3.num
        dI3 <-beta_I*s3.num*(tasas_31*(i1.num/num1)+tasas_32*(i2.num/num2)+
                               tasas_33*(i3.num/num3)+tasas_34*(i4.num/num4)+
                               tasas_35*(i5.num/num5))+delta3*e3.num-
          nu3*i3.num-mu*i3.num
        dR3 <- nu3*i3.num-mu*r3.num
        
        ###### S4
        
        dS4 <-mu*num4 -beta_I*s4.num*(tasas_41*(i1.num/num1)+tasas_42*(i2.num/num2)+
                                        tasas_43*(i3.num/num3)+tasas_44*(i4.num/num4)+
                                        tasas_45*(i5.num/num5))-
          beta_E*s4.num*(tasas_41*(e1.num/num1)+tasas_42*(e2.num/num2)+
                           tasas_43*(e3.num/num3)+tasas_44*(e4.num/num4)+
                           tasas_45*(e5.num/num5))-mu*s4.num
        
        dE4 <- beta_E*s4.num*(tasas_41*(e1.num/num1)+tasas_42*(e2.num/num2)+
                                tasas_43*(e3.num/num3)+tasas_44*(e4.num/num4)+
                                tasas_45*(e5.num/num5))-delta4*e4.num-mu*e4.num
        dI4 <- beta_I*s4.num*(tasas_41*(i1.num/num1)+tasas_42*(i2.num/num2)+
                                tasas_43*(i3.num/num3)+tasas_44*(i4.num/num4)+
                                tasas_45*(i5.num/num5))+delta4*e4.num-
          nu4*i4.num-mu*i4.num
        dR4 <- nu4*i4.num-mu*r4.num
        
        ###### vacunación a S5
        
        
        dS5 <-mu*num5 -beta_I*s5.num*(tasas_51*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_52*(1-rb)*(1-p_v)*(i2.num/num2)+
                                        tasas_53*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_54*(1-rb)*(1-p_v)*(i4.num/num4)+
                                        tasas_55*(1-rb)*(1-p_v)*(i5.num/num5))-
          beta_E*s5.num*(tasas_51*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_52*(1-rb)*(1-p_v)*(e2.num/num2)+
                           tasas_53*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_54*(1-rb)*(1-p_v)*(e4.num/num4)+
                           tasas_55*(1-rb)*(1-p_v)*(e5.num/num5))-mu*s5.num
        dE5 <- beta_E*s5.num*(tasas_51*(1-rb)*(1-p_v)*(e1.num/num1)+tasas_52*(1-rb)*(1-p_v)*(e2.num/num2)+
                                tasas_53*(1-rb)*(1-p_v)*(e3.num/num3)+tasas_54*(1-rb)*(1-p_v)*(e4.num/num4)+
                                tasas_55*(1-rb)*(1-p_v)*(e5.num/num5))-delta5*e5.num-mu*e5.num
        dI5 <- beta_I*s5.num*(tasas_51*(1-rb)*(1-p_v)*(i1.num/num1)+tasas_52*(1-rb)*(1-p_v)*(i2.num/num2)+
                                tasas_53*(1-rb)*(1-p_v)*(i3.num/num3)+tasas_54*(1-rb)*(1-p_v)*(i4.num/num4)+
                                tasas_55*(1-rb)*(1-p_v)*(i5.num/num5))+delta5*e5.num-
          nu5*i5.num-mu*i5.num
        
        
        
        dR5 <- nu5*i5.num-mu*r5.num
        list(c(dS1, dE1, dI1, dR1, 
               dS2, dE2, dI2, dR2,
               dS3, dE3, dI3, dR3,
               dS4, dE4, dI4, dR4,
               dS5, dE5, dI5, dR5
        ))
      })
    }
    
    if (input$vacuna == 1) {
      #Variante
      if(input$variante == 1){
        vacu = 1-((log10(1-8/21720))/(log10(1-162/21728)))
        beta_I<-0.043
        beta_E<-0.043
      }else{
        vacu =0.46
        beta_I<-0.09
        beta_E<-0.09
      }
      if (input$variante == 3){
        vacu <- as.numeric(input$varVacu)
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
    }else if (input$vacuna == 2){
      #Variante
      if(input$variante == 1){
        vacu =0.58
        beta_I<-0.043
        beta_E<-0.043
      }else{
        vacu =0.59
        beta_I<-0.09
        beta_E<-0.09
      }
      if (input$variante == 3){
        vacu <- as.numeric(input$varVacu)
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
    }else{
      #Variante
      if(input$variante == 1){
        vacu =0.70
        beta_I<-0.043
        beta_E<-0.043
      }else{
        vacu =0.36
        beta_I<-0.09
        beta_E<-0.09
      }
      if (input$variante == 3){
        vacu <- as.numeric(input$varVacu)
        beta_I <- as.numeric(input$varBetaI)
        beta_E <- as.numeric(input$varBetaE)
      }
    }
    
    # 30 % de la poblacion vacunada
    
    pob_vacu=0.3
    
    param <- param.dcm( p_v=pob_vacu, rb=vacu, beta_I =beta_I , beta_E = beta_E, #ver la diferencia entre los dos viendo la carga viral
                        e1.dur = 3.69, i1.dur = 3.47, e2.dur = 3.69, i2.dur = 3.47, 
                        e3.dur = 3.69, i3.dur = 3.47, e4.dur = 3.69, i4.dur = 3.47,
                        e5.dur = 3.69, i5.dur = 3.47, mu=0.01, #mu es la tasa de mortalidad estacionaria en el a?o (n?mero de muertes en un d?a) N = O
                        tasas_11 = tasas[1,1],tasas_12 = tasas[1,2],tasas_13 = tasas[1,3],tasas_14 = tasas[1,4],tasas_15 = tasas[1,5],
                        tasas_21 = tasas[2,1],tasas_22 = tasas[2,2],tasas_23 = tasas[2,3],tasas_24 = tasas[2,4],tasas_25 = tasas[2,5],
                        tasas_31 = tasas[3,1],tasas_32 = tasas[3,2],tasas_33 = tasas[3,3],tasas_34 = tasas[3,4],tasas_35 = tasas[3,5],
                        tasas_41 = tasas[4,1],tasas_42 = tasas[4,2],tasas_43 = tasas[4,3],tasas_44 = tasas[4,4],tasas_45 = tasas[4,5],
                        tasas_51 = tasas[5,1],tasas_52 = tasas[5,2],tasas_53 = tasas[5,3],tasas_54 = tasas[5,4],tasas_55 = tasas[5,5]
    )
    
    init <- init.dcm(s1.num = 676995, e1.num = 5502, i1.num = 18262, r1.num =5166,
                     s2.num = 140454, e2.num = 17450, i2.num = 59883, r2.num = 17179,
                     s3.num = 466800, e3.num = 43180, i3.num = 144791, r3.num = 41138,
                     s4.num = 587173, e4.num = 21960, i4.num = 67733, r4.num = 18531,
                     s5.num = 144490, e5.num = 17383, i5.num = 57587, r5.num = 16277)
    
    control <- control.dcm(nsteps = 40, dt = 1, new.mod = SEIR2) 
    
    mod <- dcm(param, init, control)
    
    M1 <- mod$epi$i1.num$run1[40]+mod$epi$e1.num$run1[40]+mod$epi$r1.num$run1[40] #masa infectada en el carril 1 en el día 150
    M2 <- mod$epi$i2.num$run1[40]+mod$epi$e2.num$run1[40]+mod$epi$r2.num$run1[40] #masa infectada en el carril 2 en el día 150
    M3 <- mod$epi$i3.num$run1[40]+mod$epi$e3.num$run1[40]+mod$epi$r3.num$run1[40] #masa infectada en el carril 3 en el día 150
    M4 <- mod$epi$i4.num$run1[40]+mod$epi$e4.num$run1[40]+mod$epi$r4.num$run1[40] #masa infectada en el carril 4 en el día 150
    M5 <- mod$epi$i5.num$run1[40]+mod$epi$e5.num$run1[40]+mod$epi$r5.num$run1[40] #masa infectada en el carril 5 en el día 150
    
    vac30pob60P <- c(M1,M2,M3,M4,M5)
    
    # 90 % de la poblacion vacunada
    
    pob_vacu=0.9
    
    param <- param.dcm( p_v=pob_vacu, rb=vacu, beta_I =beta_I , beta_E = beta_E, #ver la diferencia entre los dos viendo la carga viral
                        e1.dur = 3.69, i1.dur = 3.47, e2.dur = 3.69, i2.dur = 3.47, 
                        e3.dur = 3.69, i3.dur = 3.47, e4.dur = 3.69, i4.dur = 3.47,
                        e5.dur = 3.69, i5.dur = 3.47, mu=0.01, #mu es la tasa de mortalidad estacionaria en el a?o (n?mero de muertes en un d?a) N = O
                        tasas_11 = tasas[1,1],tasas_12 = tasas[1,2],tasas_13 = tasas[1,3],tasas_14 = tasas[1,4],tasas_15 = tasas[1,5],
                        tasas_21 = tasas[2,1],tasas_22 = tasas[2,2],tasas_23 = tasas[2,3],tasas_24 = tasas[2,4],tasas_25 = tasas[2,5],
                        tasas_31 = tasas[3,1],tasas_32 = tasas[3,2],tasas_33 = tasas[3,3],tasas_34 = tasas[3,4],tasas_35 = tasas[3,5],
                        tasas_41 = tasas[4,1],tasas_42 = tasas[4,2],tasas_43 = tasas[4,3],tasas_44 = tasas[4,4],tasas_45 = tasas[4,5],
                        tasas_51 = tasas[5,1],tasas_52 = tasas[5,2],tasas_53 = tasas[5,3],tasas_54 = tasas[5,4],tasas_55 = tasas[5,5]
    )
    
    init <- init.dcm(s1.num = 676995, e1.num = 5502, i1.num = 18262, r1.num =5166,
                     s2.num = 140454, e2.num = 17450, i2.num = 59883, r2.num = 17179,
                     s3.num = 466800, e3.num = 43180, i3.num = 144791, r3.num = 41138,
                     s4.num = 587173, e4.num = 21960, i4.num = 67733, r4.num = 18531,
                     s5.num = 144490, e5.num = 17383, i5.num = 57587, r5.num = 16277)
    
    control <- control.dcm(nsteps = 40, dt = 1, new.mod = SEIR2) 
    
    mod <- dcm(param, init, control)
    
    M1 <- mod$epi$i1.num$run1[40]+mod$epi$e1.num$run1[40]+mod$epi$r1.num$run1[40] #masa infectada en el carril 1 en el día 150
    M2 <- mod$epi$i2.num$run1[40]+mod$epi$e2.num$run1[40]+mod$epi$r2.num$run1[40] #masa infectada en el carril 2 en el día 150
    M3 <- mod$epi$i3.num$run1[40]+mod$epi$e3.num$run1[40]+mod$epi$r3.num$run1[40] #masa infectada en el carril 3 en el día 150
    M4 <- mod$epi$i4.num$run1[40]+mod$epi$e4.num$run1[40]+mod$epi$r4.num$run1[40] #masa infectada en el carril 4 en el día 150
    M5 <- mod$epi$i5.num$run1[40]+mod$epi$e5.num$run1[40]+mod$epi$r5.num$run1[40] #masa infectada en el carril 5 en el día 150
    
    vac90pob60P<- c(M1,M2,M3,M4,M5)
    datos$vac30pob60P<-vac30pob60P 
    datos$vac30pob60<-vac30pob60P-vac90pob60P
    datos$vac90pob60P<-vac90pob60P
    datos$sinMitigacion60<-sinMitigacionP-vac30pob60P
    
    
    #############
    #  GRAFICO PLOTLY
    #############
    
    
    plot_ly(datos, x=c(datos[,1]), y=c(datos[,2]),
            type = "bar", name = "Sin mitigacion", color = "#95b8f6",
            text = c(datos[,3]),textposition = 'auto')%>%
      add_trace(x = c(datos[,1]), y = c(datos[,4]),
                type = "bar", name = "30% Poblacion Vacunada", color = "#f9d99a",
                text = c(datos[,5]),textposition = 'auto')%>%
      add_trace(x = c(datos[,1]), y = c(datos[,6]),
                type = "bar", name = "90% Poblacion Vacunada", color = "#f9a59a",
                text = c(datos[,6]),textposition = 'auto')%>%
      layout(title=" ",
             legend=list(title=list(text='\n\n\n\n\nMasa infectada')),
             plot_bgcolor='#e5ecf6',xaxis = list(title = 'GRUPO ETARIO'), yaxis = list(title = 'MASA POBLACIONAL'))
    
    # if(input$rangoEtario == 1){
    #   plot_ly(datos, x=c(datos[,1]), y=c(datos[,2]),
    #           type = "bar", name = "Sin mitigacion/Sem mitigação", color = "#FFFFFF",
    #           text = c(datos[,3]),textposition = 'auto')%>%
    #     add_trace(x = c(datos[,1]), y = c(datos[,4]),
    #               type = "bar", name = "30% Poblacion Vacunada/População vacinada", color = "#f9d99a",
    #               text = c(datos[,5]),textposition = 'auto')%>%
    #     add_trace(x = c(datos[,1]), y = c(datos[,6]),
    #               type = "bar", name = "90% Poblacion Vacunada/População vacinada", color = "#f9a59a",
    #               text = c(datos[,6]),textposition = 'auto')%>%
    #     layout(title="SIMULACIÓN/SIMULAÇÃO 20 a 59",
    #            legend=list(title=list(text='\n\n\n\n\nMasa infectada/massa infectada')),
    #            plot_bgcolor='#e5ecf6',xaxis = list(title = 'GRUPO ETARIO/FAIXA ETÁRIA'), yaxis = list(title = 'MASA POBLACIONAL/POPULAÇÃO MASSA'))
    # }else{
    #   plot_ly(datos, x=c(datos[,1]), y=c(datos[,2]),
    #           type = "bar", name = "Sin mitigacion/Sem mitigação", color = "#FFFFFF",
    #           text = c(datos[,10]),textposition = 'auto')%>%
    #     add_trace(x = c(datos[,1]), y = c(datos[,7]),
    #               type = "bar", name = "30% Poblacion Vacunada/População vacinada", color = "#f9d99a",
    #               text = c(datos[,8]),textposition = 'auto')%>%
    #     add_trace(x = c(datos[,1]), y = c(datos[,9]),
    #               type = "bar", name = "90% Poblacion Vacunada/População vacinada", color = "#f9a59a",
    #               text = c(datos[,9]),textposition = 'auto')%>%
    #     layout(title="SIMULACIÓN/SIMULAÇÃO 60 o +",
    #            legend=list(title=list(text='\n\n\n\n\nMasa infectada/massa infectada')),
    #            plot_bgcolor='#e5ecf6',xaxis = list(title = 'GRUPO ETARIO/FAIXA ETÁRIA'), yaxis = list(title = 'MASA POBLACIONAL/POPULAÇÃO MASSA'))
    # }
    
  })
  
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - RANGO ETARIO
  #--------#
  
  observeEvent(input$infoRango, {
    # Show a simple modal
    shinyalert(title = "Rango Etario",text = "Aqui se selecciona el rango
               etario.", type = "info")
  })
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - VARIANTE DE COVID
  #--------#
  
  observeEvent(input$infoVariante, {
    # Show a simple modal
    shinyalert(title = "Variante de Covid",text = "Aqui se selecciona la
               variante de covid.", type = "info")
  })
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - TIPO DE VACUNA
  #--------#
  
  observeEvent(input$infoVacuna, {
    # Show a simple modal
    shinyalert(title = "Vacuna",text = "Aqui se selecciona la vacuna.", type = "info")
  })
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - VALOR BETA I
  #--------#
  
  observeEvent(input$infoBetaI, {
    # Show a simple modal
    shinyalert(title = "Valor Beta I",text = "Aqui se selecciona el valor de
               Beta I.", type = "info")
  })
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - VALOR BETA E
  #--------#
  
  observeEvent(input$infoBetaE, {
    # Show a simple modal
    shinyalert(title = "Valor Beta E",text = "Aqui se selecciona el valor de
               Beta E.", type = "info")
  })
  
  #--------#
  #ESPAÑOL - BOTON INFORMACION - EFECTIVIDAD DE LA VACUNA
  #--------#
  
  observeEvent(input$infoDVacuna, {
    # Show a simple modal
    shinyalert(title = "Datos Vacuna",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - FAXIA ETARIA
  #--------#
  
  observeEvent(input$infoFaixa, {
    # Show a simple modal
    shinyalert(title = "Faixa etária",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - VARINATE COVID
  #--------#
  
  observeEvent(input$infoVariante1, {
    # Show a simple modal
    shinyalert(title = "variante covid",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - VACINA
  #--------#
  
  observeEvent(input$infoVacina, {
    # Show a simple modal
    shinyalert(title = "vacina",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - BETA I
  #--------#
  
  observeEvent(input$infovarBetaI1, {
    # Show a simple modal
    shinyalert(title = "Valor Beta I",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - BETA E
  #--------#
  
  observeEvent(input$infovarBetaE1, {
    # Show a simple modal
    shinyalert(title = "Valor Beta E",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  #--------#
  #PORTUGUES - BOTON INFORMACION - DADOS VACINA
  #--------#
  
  observeEvent(input$infovarVacu1, {
    # Show a simple modal
    shinyalert(title = "dados da vacina",text = "Aqui se selecciona los datos
               vacuna.", type = "info")
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)

