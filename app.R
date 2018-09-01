# Loading the libraries

library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyjs)
library(googleAuthR)
library(lubridate)
library(RSQLite)
library(pdftables)
library(data.table)
library("rio")
library(dplyr)
library(lubridate)
library(RSQLite)
library(shinyalert)
library(RecordLinkage)
library(ggplot2)
library(plotly)
# Creating user interface
ui <- dashboardPage(title = "Mechanical Testing Lab" , 
                    
                    # Dashboard Header 
                    
                    dashboardHeader(title = tags$a(href = "http://www.iitb.ac.in/mems/en/mm-362-mech-testing-lab", 
                                                   tags$img(src='logo.png'))),
                    dashboardSidebar(
                      
                      # Declaring sidebarmenus
                      
                      sidebarMenu(id = "tab" ,
                                  menuItem("MM362" , tabName = "" ,icon = icon("th"),
                                  
                                  menuSubItem("Tensile Test" , tabName = "TENSION" ,icon = icon("th")),
                                  
                                  menuSubItem("Compression Test" , tabName = "COMPRESSION" ,icon = icon("th")),
                                  menuSubItem("Flexure Test" , tabName = "F" ,icon = icon("th")),
                                  menuSubItem("Hardness Test" , tabName = "HARDNESS" ,icon = icon("th")),
                                  menuSubItem("Fracture Toughness Test" , tabName = "FRACTURE" ,icon = icon("th")),
                                  menuSubItem("Torsion Test" , tabName = "TORSION" ,icon = icon("th")))
                      )),
                    dashboardBody(
                      
                      tags$head(tags$link(rel = "shortcut icon" ,href = "https://s3.ap-south-1.amazonaws.com/public-cogoport/favicon/favicon-32x32.png")),
                      
                      tabItems(
                        tabItem(tabName = "TENSION" , class = "active", 
                                fluidRow(
                                  box(title = "Upload data of tensile test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.1','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.1','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.1","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Initial Diameter" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.1", "", 0)
                                            
                                        ),
                                        box(title = "Initial Length" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.1", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Final Diameter" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.1", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Final Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.1", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.1" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1_1",
                                                     label = "Types of curve",
                                                     choices = c("Engineering Stress vs Engineering Strain","True Stress vs True Strain","Log(e) vs Log(s)"),
                                                     selected = "Engineering Stress vs Engineering Strain",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1_1 == 'Engineering Stress vs Engineering Strain'",
                                        plotlyOutput("plot1.1.1" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_1 == 'True Stress vs True Strain'",
                                        plotlyOutput("plot2.1.1" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_1 == 'Log(e) vs Log(s)'",
                                        plotlyOutput("plot3.1.1" ,height = 260,width = 560)
                                        
                                      )
                                  )
                                ),
                               
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("YoungsBox1.1"),
                                    shinydashboard::infoBoxOutput("offsetYieldBox1.1"),
                                    shinydashboard::infoBoxOutput("utsBox1.1"),
                                    shinydashboard::infoBoxOutput("fractureStressBox1.1"),
                                    shinydashboard::infoBoxOutput("fractureStrainBox1.1"),
                                    shinydashboard::infoBoxOutput("uniformStrainBox1.1"),
                                    shinydashboard::infoBoxOutput("trueStressAtMaximumBox1.1"),
                                    shinydashboard::infoBoxOutput("trueFractureStressBox1.1"),
                                    shinydashboard::infoBoxOutput("trueFractureStrainBox1.1"),
                                    shinydashboard::infoBoxOutput("trueUniformStrainBox1.1"),
                                    shinydashboard::infoBoxOutput("trueNeckingStrainBox1.1"),
                                    shinydashboard::infoBoxOutput("trueNeckingStressBox1.1")),
                              
                                DT::dataTableOutput("contents1.1")),
                        tabItem(tabName = "COMPRESSION" ,class="active",
                                fluidRow(
                                  box(title = "Upload data of compression test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.2','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.2','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.2","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Initial Diameter" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.2", "", 0)
                                            
                                        ),
                                        box(title = "Initial Length" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.2", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Final Diameter" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.2", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Final Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.2", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.2" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      
                                      
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1_2",
                                                     label = "Types of curve",
                                                     choices = c("Engineering Stress vs Engineering Strain","True Stress vs True Strain","Log(e) vs Log(s)"),
                                                     selected = "Engineering Stress vs Engineering Strain",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1_2 == 'Engineering Stress vs Engineering Strain'",
                                        plotlyOutput("plot1.1.2" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_2 == 'True Stress vs True Strain'",
                                        plotlyOutput("plot2.1.2" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_2 == 'Log(e) vs Log(s)'",
                                        plotlyOutput("plot3.1.2" ,height = 260,width = 560)
                                        
                                        
                                      )
                                      
                                      
                                      
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("YoungsBox1.2"),
                                    
                                    
                                    
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("offsetYieldBox1.2"),
                                    shinydashboard::infoBoxOutput("utsBox1.2"),
                                    shinydashboard::infoBoxOutput("fractureStressBox1.2"),
                                    
                                    shinydashboard::infoBoxOutput("fractureStrainBox1.2"),
                                    shinydashboard::infoBoxOutput("uniformStrainBox1.2"),
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("trueStressAtMaximumBox1.2"),
                                    shinydashboard::infoBoxOutput("trueFractureStressBox1.2"),
                                    shinydashboard::infoBoxOutput("trueFractureStrainBox1.2"),
                                    shinydashboard::infoBoxOutput("trueUniformStrainBox1.2"),
                                    shinydashboard::infoBoxOutput("trueNeckingStrainBox1.2"),
                                    shinydashboard::infoBoxOutput("trueNeckingStressBox1.2")),
                                
                                
                                
                                
                                DT::dataTableOutput("contents1.2")
                        ),
                        
                        
                        
                        tabItem(tabName = "F" ,class="active",
                                fluidRow(
                                  box(title = "Upload data of flexure test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.3','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.3','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.3","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Beam Length" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.3", "", 0)
                                            
                                        ),
                                        box(title = "Beam Width" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.3", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Beam Thickness" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.3", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Span Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.3", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.3" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      
                                      
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1_3",
                                                     label = "Types of curve",
                                                     choices = c("Bending Moment Diagram","Flexural Stress vs Flexural Strain"),
                                                     selected = "Bending Moment Diagram",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1_3 == 'Bending Moment Diagram'",
                                        plotlyOutput("plot1.1.3" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_3 == 'Flexural Stress vs Flexural Strain'",
                                        plotlyOutput("plot2.1.3" ,height = 260,width = 560)
                                        
                                        
                                      )
                                      
                                      
                                      
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("FlexuralStrength"),
                                    
                                    
                                    
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("MaximumFlexuralStrain"),
                                    shinydashboard::infoBoxOutput("FlexuralModulus"),
                                    shinydashboard::infoBoxOutput("DifferenceBetweenTE"),
                                    
                                    shinydashboard::infoBoxOutput("ElasticModulus")
                                    
                                ),
                                
                                
                                
                                
                                DT::dataTableOutput("contents1.3")
                        ),
                        
                        
                        tabItem(tabName = "HARDNESS" ,class="active",
                                fluidRow(
                                  box(title = "Upload data of hardness test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.4','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.4','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.4","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Initial Diameter" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.4", "", 0)
                                            
                                        ),
                                        box(title = "Initial Length" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.4", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Final Diameter" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.4", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Final Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.4", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.4" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      
                                      
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1,4",
                                                     label = "Types of curve",
                                                     choices = c("Engineering Stress vs Engineering Strain","True Stress vs True Strain","Log(e) vs Log(s)"),
                                                     selected = "Engineering Stress vs Engineering Strain",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1,4 == 'Engineering Stress vs Engineering Strain'",
                                        plotlyOutput("plot1.1.4" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_4 == 'True Stress vs True Strain'",
                                        plotlyOutput("plot2.1.4" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_4 == 'Log(e) vs Log(s)'",
                                        plotlyOutput("plot3.1.4" ,height = 260,width = 560)
                                        
                                        
                                      )
                                      
                                      
                                      
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("YoungsBox1.4"),
                                    
                                    
                                    
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("offsetYieldBox1.4"),
                                    shinydashboard::infoBoxOutput("utsBox1.4"),
                                    shinydashboard::infoBoxOutput("fractureStressBox1.4"),
                                    
                                    shinydashboard::infoBoxOutput("fractureStrainBox1.4"),
                                    shinydashboard::infoBoxOutput("uniformStrainBox1.4"),
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("trueStressAtMaximumBox1.4"),
                                    shinydashboard::infoBoxOutput("trueFractureStressBox1.4"),
                                    shinydashboard::infoBoxOutput("trueFractureStrainBox1.4"),
                                    shinydashboard::infoBoxOutput("trueUniformStrainBox1.4"),
                                    shinydashboard::infoBoxOutput("trueNeckingStrainBox1.4"),
                                    shinydashboard::infoBoxOutput("trueNeckingStressBox1.4")),
                                
                                
                                
                                
                                DT::dataTableOutput("contents1.4")
                        ),
                        
                        
                        tabItem(tabName = "FRACTURE" ,class="active",
                                fluidRow(
                                  box(title = "Upload data of fracture toughness test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.5','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.5','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.5","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Initial Diameter" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.5", "", 0)
                                            
                                        ),
                                        box(title = "Initial Length" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.5", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Final Diameter" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.5", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Final Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.5", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.5" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      
                                      
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1,5",
                                                     label = "Types of curve",
                                                     choices = c("Engineering Stress vs Engineering Strain","True Stress vs True Strain","Log(e) vs Log(s)"),
                                                     selected = "Engineering Stress vs Engineering Strain",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1,5 == 'Engineering Stress vs Engineering Strain'",
                                        plotlyOutput("plot1.1.5" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_5 == 'True Stress vs True Strain'",
                                        plotlyOutput("plot2.1.5" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_5 == 'Log(e) vs Log(s)'",
                                        plotlyOutput("plot3.1.5" ,height = 260,width = 560)
                                        
                                        
                                      )
                                      
                                      
                                      
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("YoungsBox1.5"),
                                    
                                    
                                    
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("offsetYieldBox1.5"),
                                    shinydashboard::infoBoxOutput("utsBox1.5"),
                                    shinydashboard::infoBoxOutput("fractureStressBox1.5"),
                                    
                                    shinydashboard::infoBoxOutput("fractureStrainBox1.5"),
                                    shinydashboard::infoBoxOutput("uniformStrainBox1.5"),
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("trueStressAtMaximumBox1.5"),
                                    shinydashboard::infoBoxOutput("trueFractureStressBox1.5"),
                                    shinydashboard::infoBoxOutput("trueFractureStrainBox1.5"),
                                    shinydashboard::infoBoxOutput("trueUniformStrainBox1.5"),
                                    shinydashboard::infoBoxOutput("trueNeckingStrainBox1.5"),
                                    shinydashboard::infoBoxOutput("trueNeckingStressBox1.5")),
                                
                                
                                
                                
                                DT::dataTableOutput("contents1.5")
                        ),
                        
                        
                        tabItem(tabName = "TORSION" ,class="active",
                                fluidRow(
                                  box(title = "Upload data of torsion test obtained from the lab" ,height = 380 , width = 2 ,status = "warning", solidHeader = TRUE ,
                                      fileInput('file1.6','Choose CSV File',
                                                accept = c('text/csv', 
                                                           'text/COMMa-separated-infos,text/plain', 
                                                           '.csv')),
                                      tags$hr(),
                                      checkboxInput('header','Header',TRUE),
                                      
                                      downloadButton('downloadData1.6','Download'),
                                      mainPanel(p("File Name :")),
                                      textInput("name1.6","","")
                                      
                                  ),
                                  
                                  box(title = "Update the dimensions(in mm)" , height = 380 , width = 4,status = "primary" , solidHeader = TRUE,
                                      
                                      fluidRow(
                                        box(title = "Initial Diameter" , width = 6 ,height = 100, status = "primary" , #solidHeader = TRUE,
                                            
                                            numericInput("iD.1.6", "", 0)
                                            
                                        ),
                                        box(title = "Initial Length" , width = 6 , height = 100,status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("iL.1.6", "", 0)
                                            
                                        )),
                                      fluidRow(
                                        box(title = "Final Diameter" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fD.1.6", "", 0)
                                            
                                        ),
                                        
                                        box(title = "Final Length" , width =6 ,height = 100, status = "primary" ,# solidHeader = TRUE,
                                            
                                            numericInput("fL.1.6", "", 0)
                                            
                                            
                                        )),
                                      
                                      actionButton("Add1.6" , "GET TABLE",       style=' font-size:150% ;background-color: white; 
                                                   color: black; margin:auto; width:100% ;
                                                   border: 2px solid #008CBA',icon("arrow-alt-circle-up", class = NULL, lib = "font-awesome"))
                                      
                                      
                                      
                                      
                                      ),
                                  box(width = 6 , status = "primary" , height = 380,
                                      selectizeInput(inputId = "typeCurve1_6",
                                                     label = "Types of curve",
                                                     choices = c("Engineering Stress vs Engineering Strain","True Stress vs True Strain","Log(e) vs Log(s)"),
                                                     selected = "Engineering Stress vs Engineering Strain",
                                                     multiple =FALSE
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.typeCurve1_6 == 'Engineering Stress vs Engineering Strain'",
                                        plotlyOutput("plot1.1.6" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_6 == 'True Stress vs True Strain'",
                                        plotlyOutput("plot2.1.6" ,height = 260,width = 560)
                                        
                                        
                                      ),
                                      conditionalPanel(
                                        condition = "input.typeCurve1_6 == 'Log(e) vs Log(s)'",
                                        plotlyOutput("plot3.1.6" ,height = 260,width = 560)
                                        
                                        
                                      )
                                      
                                      
                                      
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),
                                box(width = 12,solidHeader = TRUE, status="warning",
                                    shinydashboard::infoBoxOutput("YoungsBox1.6"),
                                    
                                    
                                    
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("offsetYieldBox1.6"),
                                    shinydashboard::infoBoxOutput("utsBox1.6"),
                                    shinydashboard::infoBoxOutput("fractureStressBox1.6"),
                                    
                                    shinydashboard::infoBoxOutput("fractureStrainBox1.6"),
                                    shinydashboard::infoBoxOutput("uniformStrainBox1.6"),
                                    
                                    
                                    
                                    shinydashboard::infoBoxOutput("trueStressAtMaximumBox1.6"),
                                    shinydashboard::infoBoxOutput("trueFractureStressBox1.6"),
                                    shinydashboard::infoBoxOutput("trueFractureStrainBox1.6"),
                                    shinydashboard::infoBoxOutput("trueUniformStrainBox1.6"),
                                    shinydashboard::infoBoxOutput("trueNeckingStrainBox1.6"),
                                    shinydashboard::infoBoxOutput("trueNeckingStressBox1.6")),
                                
                                
                                
                                
                                DT::dataTableOutput("contents1.6")
                        )
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                      )))




server <- function(input,output,session)
{
  myValues1.1 <- reactiveValues()
  observe({
    inFile1.1 <- input$file1.1
    if (is.null(input$file1.1))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.1$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      myValues1.1$myDf <- data.frame(matrix(nrow = length(A[[1]]),ncol = 8))
      
      names(myValues1.1$myDf) <- c("Extension" ,"Force","Strain","Stress","TrueStrain","TrueStress","Loge","Logs")
      myValues1.1$myDf$Extension = A[[1]]
      myValues1.1$myDf$Force = A[[2]]
      
      for (i in 1:L)
      {
        myValues1.1$myDf$Strain[i] = A[[1]][i]/(input$iL.1.1)
      }
      
      Area = 3.14 * (input$iD.1.1)*(input$iD.1.1)*(0.25)
      for (i in 1:L)
      {
        myValues1.1$myDf$Stress[i] = A[[2]][i]/Area
      }
      
      for (i in 1:L)
      {
        myValues1.1$myDf$TrueStrain[i] =  log(1+myValues1.1$myDf$Strain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.1$myDf$TrueStress[i] = (myValues1.1$myDf$Stress[i])*(1+myValues1.1$myDf$Strain[i])
      }
      
      
      for (i in 1:L)
      {
        myValues1.1$myDf$Loge[i] = log10(myValues1.1$myDf$TrueStrain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.1$myDf$Logs[i] = log10(myValues1.1$myDf$TrueStress[i])
      }
      
      
      
      
      myValues1.1$fit <- lm(myValues1.1$myDf$Stress~myValues1.1$myDf$Strain )
      myValues1.1$fitt <- lm.fit(cbind(1,myValues1.1$myDf$Stress) ,myValues1.1$myDf$Strain)
      #Youngs modulus
      myValues1.1$YM <- myValues1.1$fitt$coef[2]*1000000
      #UTS
      myValues1.1$UTS <- myValues1.1$myDf$Stress[which.max(myValues1.1$myDf$Stress)]
      #Offset Yield
      for(i in 1:L)
      {
        myValues1.1$offset[i] = myValues1.1$myDf$Strain[i] + 0.002
      }
      
      
    })})
  
  output$contents1.1 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.1$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.1 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.1, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.1$myDf, file)})
  
  
  
  
  
  
  output$plot1.1.1 <- renderPlotly({
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Engineering Strain",
      titlefont = f
    )
    y <- list(
      title = "Engineering Stress (in MPa)",
      titlefont = f
    )
    
    #plot(myValues1.1$myDf$Strain,myValues1.1$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.1$myDf$Strain,
      y = myValues1.1$myDf$Stress,
      name = "Original Curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.1$myDf$Strain,
        y = fitted(myValues1.1$fit),
        name = "Linear Fit",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      add_trace(
        x = myValues1.1$offset,
        y = fitted(myValues1.1$fit),
        name = "Offset Fit",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      ) %>%
      layout(xaxis = x, yaxis = y)
    
    
  })
  output$plot2.1.1 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "True Strain",
      titlefont = f
    )
    y <- list(
      title = "True Stress",
      titlefont = f
    )
    #plot(myValues1.1$myDf$Strain,myValues1.1$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.1$myDf$TrueStrain,
      y = myValues1.1$myDf$TrueStress,
      name = "Original Curve",
      type = "scatter"
    )  %>%
      layout(xaxis = x, yaxis = y)
    
  })
  output$plot3.1.1 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Log(e)",
      titlefont = f
    )
    y <- list(
      title = "Log(s)",
      titlefont = f
    )
    #plot(myValues1.1$myDf$Strain,myValues1.1$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.1$myDf$Loge,
      y = myValues1.1$myDf$Logs,
      name = "Original Curve",
      type = "scatter"
    )%>%
      layout(xaxis = x, yaxis = y)
    
  })
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$YoungsBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.1$YM," MPa") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$offsetYieldBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.1$UTS," MPa") , "Ultimate Tensile Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$utsBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.1$OY," MPA") , "Offset Yield" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStressBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStrainBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$uniformStrainBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueStressAtMaximumBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True stress at Maximum" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStressBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True fracture Stress" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStrainBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueUniformStrainBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStrainBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Necking Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStressBox1.1 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True Necking Stress" , icon = icon("list"),color = "purple"
    )
  })
  
  
  
  
  
  
  myValues1.2 <- reactiveValues()
  observe({
    inFile1.2 <- input$file1.2
    if (is.null(input$file1.2))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.2$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      myValues1.2$myDf <- data.frame(matrix(nrow = length(A[[1]]),ncol = 8))
      
      names(myValues1.2$myDf) <- c("Extension" ,"Force","Strain","Stress","TrueStrain","TrueStress","Loge","Logs")
      myValues1.2$myDf$Extension = A[[1]]
      myValues1.2$myDf$Force = A[[2]]
      
      for (i in 1:L)
      {
        myValues1.2$myDf$Strain[i] = A[[1]][i]/(input$iL.1.2)
      }
      
      Area = 3.14 * (input$iD.1.2)*(input$iD.1.2)*(0.25)
      for (i in 1:L)
      {
        myValues1.2$myDf$Stress[i] = A[[2]][i]/Area
      }
      
      for (i in 1:L)
      {
        myValues1.2$myDf$TrueStrain[i] =  log(1+myValues1.2$myDf$Strain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.2$myDf$TrueStress[i] = (myValues1.2$myDf$Stress[i])*(1+myValues1.2$myDf$Strain[i])
      }
      
      
      for (i in 1:L)
      {
        myValues1.2$myDf$Loge[i] = log10(myValues1.2$myDf$TrueStrain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.2$myDf$Logs[i] = log10(myValues1.2$myDf$TrueStress[i])
      }
      
      
      
      
      myValues1.2$fit <- lm(myValues1.2$myDf$Stress~myValues1.2$myDf$Strain )
      myValues1.2$fitt <- lm.fit(cbind(1,myValues1.2$myDf$Stress) ,myValues1.2$myDf$Strain)
      #Youngs modulus
      myValues1.2$YM <- myValues1.2$fitt$coef[2]*1000000
      #UTS
      myValues1.2$UTS <- myValues1.2$myDf$Stress[which.max(myValues1.2$myDf$Stress)]
      #Offset Yield
      for(i in 1:L)
      {
        myValues1.2$offset[i] = myValues1.2$myDf$Strain[i] + 0.002
      }
      
      
    })})
  
  output$contents1.2 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.2$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.2 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.2, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.2$myDf, file)})
  
  
  
  
  
  
  output$plot1.1.2 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Engineering Strain",
      titlefont = f
    )
    y <- list(
      title = "Engineering Stress (in MPa)",
      titlefont = f
    )
    
    
    #plot(myValues1.2$myDf$Strain,myValues1.2$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.2$myDf$Strain,
      y = myValues1.2$myDf$Stress,
      name = "Original Curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.2$myDf$Strain,
        y = fitted(myValues1.2$fit),
        name = "Linear Fit",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      add_trace(
        x = myValues1.2$offset,
        y = fitted(myValues1.2$fit),
        name = "Offset Fit",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      layout(xaxis = x, yaxis = y)
    
    
  })
  output$plot2.1.2 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "True Strain",
      titlefont = f
    )
    y <- list(
      title = "True Stress (in MPa)",
      titlefont = f
    )
    #plot(myValues1.2$myDf$Strain,myValues1.2$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.2$myDf$TrueStrain,
      y = myValues1.2$myDf$TrueStress,
      name = "Original Curve",
      type = "scatter"
    ) %>%
      layout(xaxis = x, yaxis = y)
    
  })
  output$plot3.1.2 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Log(e)",
      titlefont = f
    )
    y <- list(
      title = "Log(s)",
      titlefont = f
    )
    #plot(myValues1.2$myDf$Strain,myValues1.2$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.2$myDf$Loge,
      y = myValues1.2$myDf$Logs,
      name = "Original Curve",
      type = "scatter"
    )%>%
      layout(xaxis = x, yaxis = y)
    
  })
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$YoungsBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.2$YM," MPa") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$offsetYieldBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.2$UTS," MPa") , "Ultimate Tensile Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$utsBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.2$OY," MPA") , "Offset Yield" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStressBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStrainBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$uniformStrainBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueStressAtMaximumBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True stress at Maximum" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStressBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True fracture Stress" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStrainBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueUniformStrainBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStrainBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Necking Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStressBox1.2 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True Necking Stress" , icon = icon("list"),color = "purple"
    )
  })
  
  myValues1.3 <- reactiveValues()
  observe({
    inFile1.3 <- input$file1.3
    if (is.null(input$file1.3))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.3$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      
      myValues1.3$myDf =A
      
      for (i in 1:10000)
      {
        myValues1.3$Xseries[i] = i*input$iD.1.3/10000
      }
     
      
      
      for ( i in 1:5000)
      {
        myValues1.3$Yseries[i] = A[[2]][L]*(-1)*(0.5)*myValues1.3$Xseries[i]
      }
      for ( i in 5001:10000)
      {
        myValues1.3$Yseries[i] = A[[2]][L]*(1)*(0.5)*myValues1.3$Xseries[i] + 2*myValues1.3$Yseries[5000]
      }
      myValues1.3$fit <- lm(myValues1.3$myDf[[3]]~myValues1.3$myDf[[9]] )
      myValues1.3$fitt <- lm.fit(cbind(1,myValues1.3$myDf[[3]]) ,myValues1.3$myDf[[9]])

      
      #Youngs modulus
      myValues1.3$FM <- myValues1.3$fitt$coef[2]
      myValues1.3$MFS <- myValues1.3$myDf[[9]][which.max(myValues1.3$myDf[[9]])]
      myValues1.3$FS <- myValues1.3$myDf[[3]][which.max(myValues1.3$myDf[[3]])]
      
      
      
    })})
  
  output$contents1.3 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.3$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.3 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.3, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.3$myDf, file)})
  
  
  
  
  
  
  output$plot2.1.3 <- renderPlotly({
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Flexural Strain",
      titlefont = f
    )
    y <- list(
      title = "Flexural Stress (in Pa)",
      titlefont = f
    )
    
    #plot(myValues1.3$myDf$Strain,myValues1.3$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.3$myDf[[9]],
      y = myValues1.3$myDf[[3]],
      name = "Original Curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.3$myDf[[9]],
        y = fitted(myValues1.3$fit),
        name = "Linear Fit",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      
      layout(xaxis = x, yaxis = y)
    
    
  })
  output$plot1.1.3 <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Distance from the left end (in mm)",
      titlefont = f
    )
    y <- list(
      title = "Bending Moment (in mN-m)",
      titlefont = f
    )
    #plot(myValues1.3$myDf$Strain,myValues1.3$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.3$Xseries,
      y = myValues1.3$Yseries,
      
      
      name = "Bending Moment",
      type = "scatter"
    )  %>%
      layout(xaxis = x, yaxis = y)
    
  })
  
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$FlexuralStrength <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.3$FS," MPa") , "Flexural Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$MaximumFlexuralStrain <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.3$MFS," MPa") , "Maximum Flexural Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$FlexuralModulus <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.3$FS/myValues1.3$MFS) , "Flexural Modulus" , icon = icon("list"),color = "purple"
    )
  })
  output$DifferenceBetweenTE <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.3$DBTE," MPa") , "% Difference b/w Theoritical and Experimental Values" , icon = icon("list"),color = "purple"
    )
  })
  output$ElasticModulus <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.3$EM ," MPa") , "Elastic Modulus" , icon = icon("list"),color = "purple"
    )
  })
  
  
  
  myValues1.4 <- reactiveValues()
  observe({
    inFile1.4 <- input$file1.4
    if (is.null(input$file1.4))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.4$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      myValues1.4$myDf <- data.frame(matrix(nrow = length(A[[1]]),ncol = 8))
      
      names(myValues1.4$myDf) <- c("Extension" ,"Force","Strain","Stress","TrueStrain","TrueStress","Loge","Logs")
      myValues1.4$myDf$Extension = A[[1]]
      myValues1.4$myDf$Force = A[[2]]
      
      for (i in 1:L)
      {
        myValues1.4$myDf$Strain[i] = A[[1]][i]/(input$iL.1.4)
      }
      
      Area = 3.14 * (input$iD.1.4)*(input$iD.1.4)*(0.25)
      for (i in 1:L)
      {
        myValues1.4$myDf$Stress[i] = A[[2]][i]/Area
      }
      
      for (i in 1:L)
      {
        myValues1.4$myDf$TrueStrain[i] =  log(1+myValues1.4$myDf$Strain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.4$myDf$TrueStress[i] = (myValues1.4$myDf$Stress[i])*(1+myValues1.4$myDf$Strain[i])
      }
      
      
      for (i in 1:L)
      {
        myValues1.4$myDf$Loge[i] = log10(myValues1.4$myDf$TrueStrain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.4$myDf$Logs[i] = log10(myValues1.4$myDf$TrueStress[i])
      }
      
      
      
      
      myValues1.4$fit <- lm(myValues1.4$myDf$Stress~myValues1.4$myDf$Strain )
      myValues1.4$fitt <- lm.fit(cbind(1,myValues1.4$myDf$Stress) ,myValues1.4$myDf$Strain)
      #Youngs modulus
      myValues1.4$YM <- myValues1.4$fitt$coef[2]*1000000
      #UTS
      myValues1.4$UTS <- myValues1.4$myDf$Stress[which.max(myValues1.4$myDf$Stress)]
      #Offset Yield
      for(i in 1:L)
      {
        myValues1.4$offset[i] = myValues1.4$myDf$Strain[i] + 0.002
      }
      
      
    })})
  
  output$contents1.4 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.4$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.4 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.4, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.4$myDf, file)})
  
  
  
  
  
  
  output$plot1.1.4 <- renderPlotly({
    
    
    
    #plot(myValues1.4$myDf$Strain,myValues1.4$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.4$myDf$Strain,
      y = myValues1.4$myDf$Stress,
      name = "Type of curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.4$myDf$Strain,
        y = fitted(myValues1.4$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      add_trace(
        x = myValues1.4$offset,
        y = fitted(myValues1.4$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )
    
    
  })
  output$plot2.1.4 <- renderPlotly({
    
    #plot(myValues1.4$myDf$Strain,myValues1.4$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.4$myDf$TrueStrain,
      y = myValues1.4$myDf$TrueStress,
      name = "Type of curve",
      type = "scatter"
    ) 
    
  })
  output$plot3.1.4 <- renderPlotly({
    
    #plot(myValues1.4$myDf$Strain,myValues1.4$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.4$myDf$Loge,
      y = myValues1.4$myDf$Logs,
      name = "Type of curve",
      type = "scatter"
    )
    
  })
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$YoungsBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.4$YM," MPa") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$offsetYieldBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.4$UTS," MPa") , "Ultimate Tensile Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$utsBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.4$OY," MPA") , "Offset Yield" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStressBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStrainBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$uniformStrainBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueStressAtMaximumBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True stress at Maximum" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStressBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True fracture Stress" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStrainBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueUniformStrainBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStrainBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Necking Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStressBox1.4 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True Necking Stress" , icon = icon("list"),color = "purple"
    )
  })
  
  
  
  
  
  myValues1.5 <- reactiveValues()
  observe({
    inFile1.5 <- input$file1.5
    if (is.null(input$file1.5))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.5$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      myValues1.5$myDf <- data.frame(matrix(nrow = length(A[[1]]),ncol = 8))
      
      names(myValues1.5$myDf) <- c("Extension" ,"Force","Strain","Stress","TrueStrain","TrueStress","Loge","Logs")
      myValues1.5$myDf$Extension = A[[1]]
      myValues1.5$myDf$Force = A[[2]]
      
      for (i in 1:L)
      {
        myValues1.5$myDf$Strain[i] = A[[1]][i]/(input$iL.1.5)
      }
      
      Area = 3.14 * (input$iD.1.5)*(input$iD.1.5)*(0.25)
      for (i in 1:L)
      {
        myValues1.5$myDf$Stress[i] = A[[2]][i]/Area
      }
      
      for (i in 1:L)
      {
        myValues1.5$myDf$TrueStrain[i] =  log(1+myValues1.5$myDf$Strain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.5$myDf$TrueStress[i] = (myValues1.5$myDf$Stress[i])*(1+myValues1.5$myDf$Strain[i])
      }
      
      
      for (i in 1:L)
      {
        myValues1.5$myDf$Loge[i] = log10(myValues1.5$myDf$TrueStrain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.5$myDf$Logs[i] = log10(myValues1.5$myDf$TrueStress[i])
      }
      
      
      
      
      myValues1.5$fit <- lm(myValues1.5$myDf$Stress~myValues1.5$myDf$Strain )
      myValues1.5$fitt <- lm.fit(cbind(1,myValues1.5$myDf$Stress) ,myValues1.5$myDf$Strain)
      #Youngs modulus
      myValues1.5$YM <- myValues1.5$fitt$coef[2]*1000000
      #UTS
      myValues1.5$UTS <- myValues1.5$myDf$Stress[which.max(myValues1.5$myDf$Stress)]
      #Offset Yield
      for(i in 1:L)
      {
        myValues1.5$offset[i] = myValues1.5$myDf$Strain[i] + 0.002
      }
      
      
    })})
  
  output$contents1.5 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.5$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.5 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.5, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.5$myDf, file)})
  
  
  
  
  
  
  output$plot1.1.5 <- renderPlotly({
    
    
    
    #plot(myValues1.5$myDf$Strain,myValues1.5$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.5$myDf$Strain,
      y = myValues1.5$myDf$Stress,
      name = "Type of curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.5$myDf$Strain,
        y = fitted(myValues1.5$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      add_trace(
        x = myValues1.5$offset,
        y = fitted(myValues1.5$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )
    
    
  })
  output$plot2.1.5 <- renderPlotly({
    
    #plot(myValues1.5$myDf$Strain,myValues1.5$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.5$myDf$TrueStrain,
      y = myValues1.5$myDf$TrueStress,
      name = "Type of curve",
      type = "scatter"
    ) 
    
  })
  output$plot3.1.5 <- renderPlotly({
    
    #plot(myValues1.5$myDf$Strain,myValues1.5$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.5$myDf$Loge,
      y = myValues1.5$myDf$Logs,
      name = "Type of curve",
      type = "scatter"
    )
    
  })
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$YoungsBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.5$YM," MPa") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$offsetYieldBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.5$UTS," MPa") , "Ultimate Tensile Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$utsBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.5$OY," MPA") , "Offset Yield" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStressBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStrainBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$uniformStrainBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueStressAtMaximumBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True stress at Maximum" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStressBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True fracture Stress" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStrainBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueUniformStrainBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStrainBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Necking Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStressBox1.5 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True Necking Stress" , icon = icon("list"),color = "purple"
    )
  })
  
  
  
  
  
  myValues1.6 <- reactiveValues()
  observe({
    inFile1.6 <- input$file1.6
    if (is.null(input$file1.6))
    {return(NULL)}
    isolate({ 
      input$Load
      A <- read.csv(inFile1.6$datapath,stringsAsFactors =FALSE)
      L=length(A[[1]])
      myValues1.6$myDf <- data.frame(matrix(nrow = length(A[[1]]),ncol = 8))
      
      names(myValues1.6$myDf) <- c("Extension" ,"Force","Strain","Stress","TrueStrain","TrueStress","Loge","Logs")
      myValues1.6$myDf$Extension = A[[1]]
      myValues1.6$myDf$Force = A[[2]]
      
      for (i in 1:L)
      {
        myValues1.6$myDf$Strain[i] = A[[1]][i]/(input$iL.1.6)
      }
      
      Area = 3.14 * (input$iD.1.6)*(input$iD.1.6)*(0.25)
      for (i in 1:L)
      {
        myValues1.6$myDf$Stress[i] = A[[2]][i]/Area
      }
      
      for (i in 1:L)
      {
        myValues1.6$myDf$TrueStrain[i] =  log(1+myValues1.6$myDf$Strain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.6$myDf$TrueStress[i] = (myValues1.6$myDf$Stress[i])*(1+myValues1.6$myDf$Strain[i])
      }
      
      
      for (i in 1:L)
      {
        myValues1.6$myDf$Loge[i] = log10(myValues1.6$myDf$TrueStrain[i])
      }
      
      for (i in 1:L)
      {
        myValues1.6$myDf$Logs[i] = log10(myValues1.6$myDf$TrueStress[i])
      }
      
      
      
      
      myValues1.6$fit <- lm(myValues1.6$myDf$Stress~myValues1.6$myDf$Strain )
      myValues1.6$fitt <- lm.fit(cbind(1,myValues1.6$myDf$Stress) ,myValues1.6$myDf$Strain)
      #Youngs modulus
      myValues1.6$YM <- myValues1.6$fitt$coef[2]*1000000
      #UTS
      myValues1.6$UTS <- myValues1.6$myDf$Stress[which.max(myValues1.6$myDf$Stress)]
      #Offset Yield
      for(i in 1:L)
      {
        myValues1.6$offset[i] = myValues1.6$myDf$Strain[i] + 0.002
      }
      
      
    })})
  
  output$contents1.6 <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      
      DT::datatable(myValues1.6$myDf,filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  output$downloadData1.6 <- downloadHandler(
    
    filename = function() { 
      paste(input$name1.6, ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(myValues1.6$myDf, file)})
  
  
  
  
  
  
  output$plot1.1.6 <- renderPlotly({
    
    
    
    #plot(myValues1.6$myDf$Strain,myValues1.6$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly( 
      x = myValues1.6$myDf$Strain,
      y = myValues1.6$myDf$Stress,
      name = "Type of curve",
      type = "scatter"
    )%>%
      add_trace(
        x = myValues1.6$myDf$Strain,
        y = fitted(myValues1.6$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )%>%
      add_trace(
        x = myValues1.6$offset,
        y = fitted(myValues1.6$fit),
        name = "Type of curve",
        type = "scatter",
        mode = "lines",
        name = "Linear Fit",
        line = list(width = 2)
      )
    
    
  })
  output$plot2.1.6 <- renderPlotly({
    
    #plot(myValues1.6$myDf$Strain,myValues1.6$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.6$myDf$TrueStrain,
      y = myValues1.6$myDf$TrueStress,
      name = "Type of curve",
      type = "scatter"
    ) 
    
  })
  output$plot3.1.6 <- renderPlotly({
    
    #plot(myValues1.6$myDf$Strain,myValues1.6$myDf$Stress,type = "l",xlab = "Engineering Strain",ylab = "Engineering Stress in Mpa")
    p <- plot_ly(
      x = myValues1.6$myDf$Loge,
      y = myValues1.6$myDf$Logs,
      name = "Type of curve",
      type = "scatter"
    )
    
  })
  
  
  output$info <- renderText({
    paste0("Strain=",input$plot_click$x , "\nStress=",input$plot_click$y)
  })
  output$YoungsBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.6$YM," MPa") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$offsetYieldBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.6$UTS," MPa") , "Ultimate Tensile Strength" , icon = icon("list"),color = "purple"
    )
  })
  output$utsBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(myValues1.6$OY," MPA") , "Offset Yield" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStressBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Modulus of Elasticity" , icon = icon("list"),color = "purple"
    )
  })
  output$fractureStrainBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$uniformStrainBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueStressAtMaximumBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True stress at Maximum" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStressBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True fracture Stress" , icon = icon("list"),color = "purple"
    )
  })
  output$trueFractureStrainBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Fracture Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueUniformStrainBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Uniform Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStrainBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25) , "True Necking Strain" , icon = icon("list"),color = "purple"
    )
  })
  output$trueNeckingStressBox1.6 <- renderInfoBox({
    shinydashboard::infoBox(
      paste0(25," MPA") , "True Necking Stress" , icon = icon("list"),color = "purple"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}



shinyApp(ui,server)


