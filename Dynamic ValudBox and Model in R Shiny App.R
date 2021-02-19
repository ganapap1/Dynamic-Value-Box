
#Dynamic Value box and modal with Logistic Regression Statistics

######################################################
##code to unload all packages that you have on RStudio
######################################################

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)


################################################################################################
##install packages if not available
# got this code from https://medium.com/swlh/beautiful-charts-with-r-and-ggpubr-c94122d6b7c6> 
################################################################################################

if(!require("shiny")) install.packages("shiny")
if(!require("mlbench")) install.packages("mlbench")
if(!require("shinydashboard")) install.packages("shinydashboard")
if(!require("dplyr")) install.packages("dplyr")
if(!require("shinyBS")) install.packages("shinyBS")
if(!require("caret")) install.packages("caret")
if(!require("InformationValue")) install.packages("InformationValue")


###############################
##Loading required libraries
###############################
library(shiny)
library(mlbench) #to get BreastCancer dataset
library(shinydashboard)
library(dplyr)
library(shinyBS)
library(caret)
library(InformationValue)


##########################################################
##Loading data & remove ID column which is not a predictor
##########################################################
data("BreastCancer")
mydata <- BreastCancer

# remove id column
mydata <- mydata[,-1]


#######################################
## concert dependent variable as factor
#######################################
mydata$Class <- ifelse(mydata$Class == "malignant", 1, 0)
mydata$Class <- factor(mydata$Class, levels = c(0, 1))


###########################################
## Concert Independent variables as numeric
###########################################
for(i in 1:9) {
  mydata[, i] <- as.numeric(as.character(mydata[, i]))
}


#####################################
# Prepare Training and Test data.
#####################################
set.seed(100)
trainDataIndex <- createDataPartition(mydata$Class, p=0.7, list = F)  # 70% training data
trainData <- mydata[trainDataIndex, ]
testData <- mydata[-trainDataIndex, ]


################################################################################
## Building logistic regression model, by considering all independent variables
################################################################################
logitmod <- glm(Class ~ ., family = "binomial", data=trainData)
summary(logitmod)


################################################
## let us now predict using test data as factor
################################################
pred <- predict(logitmod, newdata = testData, type = "response")
y_pred <<- as.numeric(ifelse( pred  > 0.5, 1, 0))
y_pred  <<- factor( y_pred , levels = c(0, 1))
y_act <<- testData[,"Class"]


######################################################################################
## we need prediction as numeric for KS Statistics,somersD and Misclassification Error
######################################################################################
actual <<- as.numeric(as.character(y_act))
predscore <<- as.numeric(as.character(y_pred))


#########################################################
## get all statistics in one dataframe called df
#########################################################
results1 <- caret::confusionMatrix(y_pred, y_act, positive="1", mode="everything")
df1 = broom::tidy(results1)

## now df has first set of statistics
df <- subset.data.frame(x = df1,select = c(1,3))

## we now add KS Statistics to that dataframe
if (nrow(testData)>=10){
  ks_statval <- ks_stat(actuals=actual, predictedScores=predscore)
  df[nrow(df) + 1,] = list(term = "ks_stat",estimate = ks_statval)
}

## we now add somersD Statistics to that dataframe
somersDval <- somersD(actuals=actual, predictedScores=predscore)
df[nrow(df) + 1,] = list(term = "somersD",estimate = somersDval)

## we now add misClassError Statistics to that dataframe
miscalerval <- misClassError(actuals=actual, predictedScores=predscore, threshold=0.5)
df[nrow(df) + 1,] = list(term = "misClassError",estimate = miscalerval)


## we now rename the column header
names(df)[1] <- "Statistics"
names(df)[2] <- "Value"

## we now change to numeric format and characters as upper, just to have nice visual
df$Value <- as.numeric(df$Value)
df$Statistics <- toupper(df$Statistics)


##################################################################
## some times MCNEMAR Statistics from caret package will to be NA, 
#hence we remove that row from the dataframe
##################################################################
df<-df[df$Statistics != "MCNEMAR", ]

## we now format numeric field as four decimal
df <-format.data.frame(df,digits=3) #note if you put 3 digits, you get 4 decimals


#################################################################################################
#Here we build dataframe for the explanations, add explanations, you may alter it here as needed
#################################################################################################

explanationdf <- data.frame(ItemCode = c("ACCURACY", "KAPPA", "SENSITIVITY", "SPECIFICITY", 
                                         "POS_PRED_VALUE", "NEG_PRED_VALUE", "PRECISION", 
                                         "RECALL", "F1", "PREVALENCE", "DETECTION_RATE", 
                                         "DETECTION_PREVALENCE", "BALANCED_ACCURACY", 
                                         "KS_STAT", "SOMERSD", "MISCLASSERROR"),
                            Comments = c("add your explanation in the script ACCURACY", 
                                         "add your explanation in the script KAPPA", 
                                         paste("add your explanation in the script SENSITIVITY",'<br>',"add next paragraph",'<br>',"add next paragraph"), 
                                         "add your explanation in the script SPECIFICITY", 
                                         "add your explanation in the script POS_PRED_VALUE", 
                                         "add your explanation in the script NEG_PRED_VALUE", 
                                         "add your explanation in the script PRECISION", 
                                         "add your explanation in the script RECALL", 
                                         "add your explanation in the script F1", 
                                         "add your explanation in the script PREVALENCE", 
                                         "add your explanation in the script DETECTION_RATE", 
                                         "add your explanation in the script DETECTION_PREVALENCE", 
                                         "add your explanation in the script BALANCED_ACCURACY", 
                                         "add your explanation in the script KS_STAT", 
                                         "add your explanation in the script SOMERSD", 
                                         "add your explanation in the script MISCLASSERROR"))



###################
# UI Code
###################

ui <- dashboardPage(
  dashboardHeader(title = "ValueBox & Modal"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        
        tabName = "tabstatvalbox",   
        box(
          HTML(paste('<p text-align ="center"><h3><b><i>','Statistics and Educational Objectives','</i></b></p><h5>')),
          background = 'black',
          width = 12,
          height = 60
        ),
        
        column(
          width = 12,
          align = "center",
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 95px}"))),   ## where you got: https://www.edureka.co/community/32145/how-to-reduce-the-height-of-valuebox-in-shiny-r
            uiOutput("mstatvalbox"),
            
          ) #fluid row closure
        )  #column closure
      ) #Outer column closure
    )  #fluidRow closure
  )  #dashboard Body closure
)  #dashboard Page closure




######################
# Server Code
######################


server <- function(input,output,session){
  library(shinyWidgets) #for slidercolor function
  slidercolrange <- -2
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-explanationdf
  
  
  output$mstatvalbox <- renderUI({
    mstatvalbox <- lapply(1:nrow(df), function(i) {
      if (slidercolrange==12){
        slidercolrange <- 1
      }
      else{
        slidercolrange <- slidercolrange ++ 2
      }
      
      #getting multicolor based on value of i
      if (i == 1 || i == 5|| i == 9|| i == 13){
        mcol ="aqua"
      }
      else if (i == 2 || i == 6|| i == 10|| i == 14){
        mcol ="green"
      }
      else if (i == 3 || i == 7|| i == 11|| i == 15){
        mcol ="yellow"
      }
      else if (i == 4 || i == 8|| i == 12|| i == 16){
        mcol ="purple"
      }
      else {
        mcol ="aqua"
      }
      
      ############# I am adding 'M' in the front, to avoid clash between name and function, 
      ############# for eg  insted of 'ACCURACY' I use 'MACCURACY' as name    
      
      inputName <- paste0("M",df[i,1])
      div(id=inputName,
          column(slidercolrange+3,
                 valueBox(
                   value = tags$p(df[i,2],style = "font-size: 100%;"),    #where you got formatting: https://www.edureka.co/community/32147/change-font-size-of-valuebox-shiny-r
                   subtitle = tags$p(df[i,1], style = "font-size: 125%;"),
                   color =mcol ,
                   width = "160px")),
          
          
          tags$style(".popover-title{
         background-color:#8A7B57;
         color:white;
         text-align:center;
         font-size:18px;
         overflow-wrap:break-word;
         }"),
          
          tags$style(".popover{
         background-color:#FFF2B3;
         text-align:left;
         font-size:15px;
         border:solid;  # see here for boarder design: https://www.w3schools.com/css/css_border.asp
         border-radius:unset;
         border-color: black;
         border-width: 2px;
         min-width:350px;
         #width:300px;
         # max-width:800px;
         height:325px;
         # max-height:600px;
         overflow-wrap:break-word;
         }"),
          
          bsPopover(id = inputName,
                    title = HTML(paste('<b>', "EDUCATIONAL OBJECTIVE ON",'<br>',df[i,1],'</b>')),
                    content = HTML(paste(eval(parse(text=paste0("(","vals_trich$Data %>% dplyr::filter(vals_trich$Data$ItemCode == ","'",df[i,1],"'","))[,2]"    ))))),
                    placement = "bottom",trigger = "hover",options = list(container = "body") )
          
      )
      
    })
    
    # Create a tagList of sliders (this is important)
    do.call(tagList, mstatvalbox)
  })
  
}



shinyApp(ui,server)