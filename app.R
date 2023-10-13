#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# The app displays demographic data regarding the passengers on board the Titanic

library(readxl)
library(shiny)
Open <- "("
Close  <-")"

TitanicData <- read_excel("TitanicData.xls")
names(TitanicData) <- c("Passenger Class", "Survived", "Gender", "Age")

ui <- fluidPage(
  titlePanel("Titanic Demographic Data"),
  radioButtons("Category", "Select Category:",
               
               choiceNames = list(
                 "Passenger Class Totals",
                 "Survived, Yes and No",
                 "Gender by Male and Female",
                 "Age by Adult and Child"
               ),
               choiceValues = list("Passenger Class", "Survived", "Gender", "Age")
               
               
               ),
  textOutput("txt"),

  
  plotOutput("piePlot"),
  
  
  radioButtons("Group", "Select Group to Display Survival Rates",
              
            choiceNames = list( "Passenger Classs","Gender by Male and Female", "Age by Adult and Child" ),
                                                                           
                     choiceValues = list(1,2,3)
) #RadioButtons
  
  
 
  

  
  
  
  

)#fluid page





server <- function(input, output) {
  output$txt <- renderText({
    
    
  if (input$Category == names(TitanicData[1]))   {Frequency <- table(TitanicData$`Passenger Class`);
                                                string <- c( "First Class","Second Class", "Third Class")
                                                Size <- c (Frequency[1], Frequency[2], Frequency[3])
                                                Percentage<- round(100*Size/sum(Size), 1)
                                                output$piePlot <- renderPlot({
                                                pie(Size,labels = Percentage, main = "Passengers by Class", col = rainbow(length(Size)))
                                                legend("topright", string, cex = 0.8,
                                                fill = rainbow(length(Size)))
                                                }) #End RenderPlot
                                                }#End IF


  if (input$Category == names(TitanicData[2]))  {Frequency <- table(TitanicData$`Survived`);
                                                string <- c("Survived", "Perished")
                                                Size <- c (Frequency[1], Frequency[2])
                                                Percentage<- round(100*Size/sum(Size), 1)
                                                output$piePlot <- renderPlot({
                                                pie(Size,labels = Percentage,
                                                main = "Survived, Perished",col = rainbow(length(Size)))
                                                legend("topright", string, cex = 0.8,
                                                fill = rainbow(length(Size)))
                                                }) #End Render Plot
                                                }#End If


 if (input$Category == names(TitanicData[3])) {Frequency <- table(TitanicData$`Gender`)
                                              string <- c("Male", "Female")
                                              Size <- c (Frequency[1], Frequency[2])
                                              Percentage<- round(100*Size/sum(Size), 1)
                                              output$piePlot <- renderPlot({
                                              pie(Size,labels = Percentage,
                                              main = "Passengers by Gender",col = rainbow(length(Size)))
                                              legend("topright", string, cex = 0.8,
                                              fill = rainbow(length(Size)))
                                               }) #End Render Plot
                                               } #End If

 if (input$Category == names(TitanicData[4])) {Frequency <- table(TitanicData$`Age`);
                                              string <- c("Adult", "Child")
                                              Size <- c (Frequency[1], Frequency[2])
                                              Percentage<- round(100*Size/sum(Size), 1)
                                              output$piePlot <- renderPlot({
                                              pie(Size, Percentage,
                                              main = "Passengers by Age")
                                              legend("topright", string, cex = 0.8,
                                              fill = rainbow(length(Size)))
                                              }) #End Render Plot
                                               } #End If


    
    
    
    
  
    

  
    
    
    
       
  }) #End Render Text
  
  ## I could not get the below code toi display, so I did not update to compute survival rates..
  reactive(
  
  if (input$Group == 1) {Frequency <- table(TitanicData$`Passenger Class`);
  string <- c("Adult", "Child")
  Size <- c (Frequency[1], Frequency[2])
  Percentage<- round(100*Size/sum(Size), 1)
  output$piePlot <- renderPlot({
    pie(Size,labels = string,
        main = "Passengers by Class")
    legend("topright", string, cex = 0.8,
           fill = rainbow(length(Size)))
  }) #End Render Plot
  } #End If
  
  )#end reactive
  
##I could not get the below code to display.
#   output$txt <- renderText({
# ##Survivors by Class
# Class1Survived <- sum(TitanicData$`Passenger Class` == 1 & TitanicData$`Survived?`==1 , na.rm=TRUE)
# Class1Survived
# Class1SurvivalRate = Class1Survived / sum(TitanicData$`Passenger Class` == 1)
# Class1SurvivalRate
# 
# Class2Survived <- sum(TitanicData$`Passenger Class` == 2 & TitanicData$`Survived?`==1 , na.rm=TRUE)
# Class2Survived
# Class2SurvivalRate = Class2Survived / sum(TitanicData$`Passenger Class` == 2)
# Class2SurvivalRate
# 
# Class3Survived <- sum(TitanicData$`Passenger Class` == 3 & TitanicData$`Survived?`== 1 , na.rm=TRUE)
# Class3Survived
# Class3SurvivalRate = Class3Survived / sum(TitanicData$`Passenger Class` == 3)
# Class3SurvivalRate
# 
# ##Survivors by Gender
# MaleSurvived <- sum(TitanicData$Gender == "male" & TitanicData$`Survived?`== 1 , na.rm=TRUE)
# MaleSurvived
# MaleSurvivalRate = MaleSurvived / sum(TitanicData$Gender == "male")
# MaleSurvivalRate
# 
# FemaleSurvived <- sum(TitanicData$Gender == "female" & TitanicData$`Survived?`== 1 , na.rm=TRUE)
# FemaleSurvived
# FemaleSurvivalRate = FemaleSurvived / sum(TitanicData$Gender == "female")
# FemaleSurvivalRate
#   
# ##Survivors by Age
# AdultSurvived <- sum(TitanicData$Age == "Adult" & TitanicData$`Survived?`== 1 , na.rm=TRUE)
# AdultSurvived
# AdultSurvivalRate = AdultSurvived / sum(TitanicData$Age== "Adult")
# AdultSurvivalRate
# 
# ChildSurvived <- sum(TitanicData$Age == "Child" & TitanicData$`Survived?`== 1 , na.rm=TRUE)
# ChildSurvived
# ChildSurvivalRate = ChildSurvived / sum(TitanicData$Age == "Child")
# ChildSurvivalRate
# 
# })



  
} #End Server





