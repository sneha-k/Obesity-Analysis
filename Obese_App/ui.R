#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(caret)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(gbm)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),
                  # Application title
                  #titlePanel("Obesity Risk Analysis"),
                  
                  # Sidebar with a slider input for number of bins
                  # sidebarLayout(
                  #     sidebarPanel(
                  #         sliderInput("bins",
                  #                     "Number of bins:",
                  #                     min = 1,
                  #                     max = 50,
                  #                     value = 30)
                  #     ),
                  
                  # Show a plot of the generated distribution
                  navbarPage("Obesity Risk Analysis", id = "main_tabs",
                             tabPanel("About",
                                      tabsetPanel(
                                        tabPanel("About the App",
                                                 mainPanel(
                                                   p("With obesity on the rise, it is important to spread awareness on the underlying causes of the diseases. 
                                              When we look around, most of the youngsters and young adults are leading a lifestyle that is pushing them to overweight and obesity.
                                              Not many are aware of the health implication of obesity and only consider it as a cosmetic limitation. This application is a small effort to bring awareness 
                                              of those lifestyle habits that influences the risk of obesity. The main objective of this study is to establish a set of risk factors for obesity in adults 
                                              among the available study variables."),
                                                   tags$footer("Disclaimer : In no way is this to spread misinformation or fear. This is not a tool for self-prognosis. If you are at a risk of obesity or are 
                                                         facing medical issues, please consult a healthcare professional.", style = "font-size:10px;")
                                                 )),
                                        tabPanel("Obesity - A threat?",
                                                 mainPanel(
                                                   h3("Why care about Obesity?"),
                                                   p("Overweight and obesity are defined as abnormal or excessive fat accumulation that presents a risk to health. 
                                            A body mass index (BMI) over 25 is considered overweight, and over 30 is obese. Obesity is a complex disease involving an excessive amount of body fat. Obesity isn't just a cosmetic concern. 
                                            It's a medical problem that increases the risk of other diseases and health problems, such as heart disease, diabetes, high blood pressure and certain cancers.
                                            Obesity increases the risk of several debilitating, and deadly diseases, 
                                    including diabetes, heart disease, and some cancers. 
                                    It does this through a variety of pathways, some as 
                                    straightforward as the mechanical stress of carrying extra pounds and some
                                    involving complex changes in hormones and metabolism."),
                                                   span(h4(strong("Obesity is one of the leading risk factors for early death!")), style = "color:#154360"),
                                                   h3("What causes Obesity?"),
                                                   p("The fundamental cause of obesity and overweight is an energy imbalance between calories consumed and calories expended. Globally, there has been:"),
                                                   tags$ul(
                                                     tags$li("an increased intake of energy-dense foods that are high in fat and sugars; and"),
                                                     tags$li("an increase in physical inactivity due to the increasingly sedentary nature of many forms of work, 
                                                      changing modes of transportation, and increasing urbanization.")),
                                                   p("Changes in dietary and physical activity patterns are often the result of environmental and societal changes 
                                              associated with development and lack of supportive policies in sectors such as health, agriculture, transport, 
                                              urban planning, environment, food processing, distribution, marketing, and education."),
                                                   h3("WHO's response to Obesity"),
                                                   p("World Obesity Day is observed globally on 4 March as of March 2020 with the view of promoting practical solutions to end the global obesity crisis. 
                                              World Health Organization (WHO) urged countries to do more to reverse what is a preventable health crisis. 
                                              WHO said the key to preventing obesity is to act early. For example, before even considering having a baby, get healthy. 
                                              At the same time, countries need to work together to create a better food environment so that everyone can access and afford a healthy diet.
                                              To achieve that, steps to be taken include restricting the marketing to children of food and drinks high in fats, sugar, and salt, taxing sugary drinks, and providing better access to affordable, healthy food.
                                              Along with changes in diet, WHO also mentioned the need for exercise. 
                                              Cities and towns need to make space for safe walking, cycling, and recreation, and schools need to help households teach children healthy habits from early on.
                                              WHO continues to address the global obesity crisis by monitoring global trends and prevalence, developing a broad range of guidance to prevent and treat overweight 
                                              and obesity, and providing support and guidance for countries."),
                                                   tags$footer("Disclaimer : In no way is this to spread misinformation or fear. This is not a tool for self-prognosis. If you are at a risk of obesity or are 
                                                         facing medical issues, please consult a healthcare professional.", style = "font-size:10px;"))),
                                        tabPanel("The Statistics",
                                                 mainPanel(
                                                   p("Here are some worldwide facts on Obesity: "),
                                                   tags$ul(
                                                     tags$li("Worldwide obesity has nearly tripled since 1975."),
                                                     tags$li("With the numbers still increasing, WHO estimates that by 2025, approximately 167 million people will become less healthy because they are overweight or obese."),
                                                     tags$li("Obesity is one of the leading risk factors for premature death. It was linked to 4.7 million deaths globally in 2017."),
                                                     tags$li("8% of global deaths were attributed to obesity in 2017."),
                                                     tags$li("There are large differences – 10-fold – in death rates from obesity across the world."),
                                                     tags$li("13% of adults in the world are obese."),
                                                     tags$li("39% of adults in the world are overweight."),
                                                     tags$li("One-in-five children and adolescents, globally, are overweight."),
                                                     tags$li("Obesity is determined by the balance of energy intake and expenditure. Rates have increased as the calories have become more readily available."),
                                                     tags$li(strong("Obesity is preventable."), style = "color:#154360")),
                                                   img(src="share-of-deaths-obesity.png", height = "100%", width = "100%"),
                                                   tags$footer("Disclaimer : In no way is this to spread misinformation or fear. This is not a tool for self-prognosis. If you are at a risk of obesity or are 
                                                         facing medical issues, please consult a healthcare professional.", style = "font-size:10px;")
                                                 )),
                                        tabPanel("Dataset",
                                                 mainPanel(
                                                   h3("Dataset Source"),
                                                   p("This dataset includes data for the estimation of obesity levels in individuals from the countries of Mexico, 
                                              Peru and Colombia, based on their eating habits and physical condition. 77% of the data was generated synthetically 
                                              using the Weka tool and the SMOTE filter, 23% of the data was collected directly from users through a web platform. This users were young undergraduate students between 18 and 25 years old. 
                                              The size of the sample was 712 records, based on the surveys applied to 324 men and 388 women.  This data can be used to generate intelligent computational tools to identify 
                                              the obesity level of an individual and to build recommender systems that monitor obesity levels."),
                                                   tags$a(href="https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition+#", "The dataset is found on UCI Machine Learning Repository."),
                                                   h3("Data Description"),
                                                   p("The data contains 17 attributes and 2111 records. The records are labeled with the class variable NObesity (Obesity Level), that allows classification of the data using the values of 
                                              Insufficient Weight, Normal Weight, Overweight Level I, Overweight Level II, Obesity Type I, Obesity Type II and Obesity Type III according to the classification given by WHO."),
                                                   tableOutput('whoObesity'),
                                                   p("The dataset description is attached below:"),
                                                   img(src="dataset-description.png", height = "100%", width = "100%"),
                                                   tags$footer("Disclaimer : In no way is this to spread misinformation or fear. This is not a tool for self-prognosis. If you are at a risk of obesity or are 
                                                         facing medical issues, please consult a healthcare professional.", style = "font-size:10px;")
                                                 )),
                                        tabPanel("App Navigation Guide",
                                                 mainPanel(
                                                   p("Now that you have understood the purpose of the app, it is time to give a little walkthrough of the different tabs."),
                                                   br(),
                                                   img(src="tabs.png", height = "100%", width = "100%"),
                                                   br(),
                                                   br(),
                                                   tags$ul(
                                                     tags$li(p(strong("About: "), "This is the default tab of the web application. The tab tells you about the purpose of the application and the way about it." )),
                                                     tags$li(p(strong("Data: "), "This tab is to give you a feel of the dataset. You could scroll through the dataset, look at the top and bottom values, subset the dataset by rows or columns, and download the CSV file." )),
                                                     tags$li(p(strong("Exploratory Analysis: "), "Explore the data! Make your own numerical summaries, customize plots, and actually look at the patterns to unfold in the dataset" )),
                                                     tags$li(p(strong("Data Modelling: "), "The final purpose of this application to train a model to make accurate predictions. The Data Modelling page introduces you to the algorithms used to make the predictions. It also helps you be the Data Scientist here! Choose the train-test split, the hyperparameters 
                                                        or just the variables you would like to be seen included in the modelling." )),
                                                     tags$li(p(strong("Prediction: "), "Finally, use this tab to see where you lie based on your lifestyle choices and habits." )),
                                                     tags$footer("Disclaimer : In no way is this to spread misinformation or fear. This is not a tool for self-prognosis. If you are at a risk of obesity or are 
                                                         facing medical issues, please consult a healthcare professional.", style = "font-size:10px;")
                                                   )))
                                      )),
                             tabPanel("Data",
                                      # Sidebar layout with input and output definitions ----
                                      sidebarLayout(
                                        
                                        # Sidebar panel for inputs ----
                                        sidebarPanel(
                                          
                                          # br() element to introduce extra vertical spacing ----
                                          br(),
                                          
                                          # Input: Slider for the number of observations to generate ----
                                          sliderInput("n",
                                                      "Number of observations:",
                                                      value = 500,
                                                      min = 100,
                                                      max = 2111,
                                                      step = 100),
                                          checkboxGroupInput("variable", "Variables to choose:",
                                                             c("Gender" = "Gender",
                                                               "Age" = "Age",
                                                               "Height(in mt)" = "Height",
                                                               "Weight(in kg)" = "Weight",
                                                               "Family with Obesity" = "family_history_with_overweight",
                                                               "Fast Food Intake" = "FAVC",
                                                               "Vegetables Consumption Freq" = "FCVC",
                                                               "Number of meals daily" = "NCP",
                                                               "Food intake between meals" = "CAEC",
                                                               "Smoking" = "SMOKE",
                                                               "Liquid intake daily" = "CH2O",
                                                               "Calories Consumption" = "SCC",
                                                               "Physical Activity" = "FAF",
                                                               "Scheduled deciated to technology" = "TUE",
                                                               "Alcohol Consumption" = "CALC",
                                                               "Type of Transportation Used" = "MTRANS",
                                                               "Vulnerable" = "NObeyesdad")),
                                        actionButton("download", "Download CSV")),
                                      mainPanel(
                                        dataTableOutput('subsetData')
                                        ))),
                             navbarMenu("Exploratory Analysis",
                                        tabPanel("Numerical Summaries",
                                                 # Sidebar layout with input and output definitions ----
                                                 sidebarLayout(
                                                   
                                                   # Sidebar panel for inputs ----
                                                   sidebarPanel(
                                                     
                                                     br(),
                                                     
                                                     selectInput("type1", "Type",
                                                                 c("Summary Statistics" = "ss",
                                                                   "Distributions" = "dist", 
                                                                   "Scatter Plots" = "scatter",
                                                                   "Correlations" = "corr")),
                                                     conditionalPanel(
                                                       condition = "input.type1 == 'dist'",
                                                       checkboxGroupInput("numvar", "Distribution of Numerical Variables:",
                                                                          c(
                                                                            "Age" = "Age",
                                                                            "Height(in mt)" = "Height",
                                                                            "Weight(in kg)" = "Weight"
                                                                            ))
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.type1 == 'scatter'",
                                                       selectInput("var1", "Variable 1:",
                                                                          c(
                                                                            "Age" = "Age",
                                                                            "Height(in mt)" = "Height",
                                                                            "Weight(in kg)" = "Weight"
                                                                          ),
                                                                   selected = NULL),
                                                       selectInput("var2", "Variable 2:",
                                                                   c(
                                                                     "Age" = "Age",
                                                                     "Height(in mt)" = "Height",
                                                                     "Weight(in kg)" = "Weight"
                                                                   ),
                                                                   selected = NULL)
                                                     )
                                                     ),
                                                   mainPanel(
                                                     conditionalPanel(
                                                       condition = "input.type1 =='ss'",
                                                       verbatimTextOutput("summarystats")),
                                                     conditionalPanel(
                                                       condition = "input.type1 == 'dist' && (input.numvar == 'Age' || input.numvar == 'Height' || input.numvar == 'Weight) && input.type1 != 'scatter' && input.type1 != 'corr'",
                                                       plotOutput("distplot")),
                                                     conditionalPanel(
                                                       condition = "input.type1 =='scatter' && input.type1 != 'dist' && input.type1 != 'corr'",
                                                       plotOutput("scatter")),
                                                     conditionalPanel(
                                                       condition = "input.type1 =='corr' && input1.type1 != 'dist' && input.type1 != 'scatter'",
                                                       plotOutput("corr"))
                                                   ))
                                                 ),
                                        tabPanel("Categorical Summaries",
                                                 # Sidebar layout with input and output definitions ----
                                                 sidebarLayout(
                                                   
                                                   # Sidebar panel for inputs ----
                                                   sidebarPanel(
                                                     
                                                     br(),
                                                     
                                                     selectInput("type2", "Select the Categorical Variable you want to Analyze",
                                                                 c("Family with Obesity" = "family_history_with_overweight",
                                                                   "Fast Food Intake" = "FAVC",
                                                                   "Vegetables Consumption Freq" = "FCVC",
                                                                   "Number of meals daily" = "NCP",
                                                                   "Food intake between meals" = "CAEC",
                                                                   "Smoking" = "SMOKE",
                                                                   "Liquid intake daily" = "CH2O",
                                                                   "Calories Consumption" = "SCC",
                                                                   "Physical Activity" = "FAF",
                                                                   "Scheduled deciated to technology" = "TUE",
                                                                   "Alcohol Consumption" = "CALC",
                                                                   "Type of Transportation Used" = "MTRANS")),
                                                     
                                                     selectInput("type3", "Select the Numerical Variable you want to analyze it against",
                                                                 c(
                                                                   "Age" = "Age",
                                                                   "Height(in mt)" = "Height",
                                                                   "Weight(in kg)" = "Weight"
                                                                 ))
                                                   ),
                                                   mainPanel(
                                                     plotOutput("catplot1"),
                                                     plotOutput("catplot2"),
                                                     plotOutput("catplot3")
                                                   )))),
                             tabPanel("Data Modelling",
                                      tabsetPanel(id = "model",
                                        tabPanel("Modelling Info", value = "MI",
                                                 br(),
                                                 p("The problem statement of predicting Obesity is a classification problem. We are trying to categorize a young adults into the WHO approved Obesity classes based on their lifestyle and contributing factors.
                                                   The three models that we used for training and prediction are Generalized Linear Model(Logistic Regression), Gradient Boosting, and Random Forest. Let us take a moment to 
                                                   understand these machine learning models better."),
                                                 h3("Logistic Regression"),
                                                 p("This type of statistical model (also known as logit model) is often used for classification and predictive analytics. Logistic regression estimates the probability of an event occurring, 
                                                   such as voted or didn’t vote, based on a given dataset of independent variables. Since the outcome is a probability, the dependent variable is bounded between 0 and 1. 
                                                   In logistic regression, a logit transformation is applied on the odds—that is, the probability of success divided by the probability of failure. This is also commonly known as the 
                                                   log odds, or the natural logarithm of odds, and this logistic function is represented by the following formulas"),
                                                 uiOutput("ex1"),
                                                 p("Multinomial logistic regression is an extension of logistic regression that adds native support for multi-class classification problems. Logistic regression, by default, 
                                                 is limited to two-class classification problems. Some extensions like one-vs-rest can allow logistic regression to be used for multi-class classification problems, although they require that the classification problem 
                                                   first be transformed into multiple binary classification problems.Instead, the multinomial logistic regression algorithm is an extension to the logistic regression model that involves changing the loss function to cross-entropy loss 
                                                   and predict probability distribution to a multinomial probability distribution to natively support multi-class classification problems. The multinomal logistic regression is more appropriate in our case since we have more than two classes of obesity."),
                                                 p("Advantages of Logistic Regression are: "),
                                                 tags$ul(
                                                   tags$li("Logistic regression is easier to implement, interpret, and very efficient to train."),
                                                   tags$li("It makes no assumptions about distributions of classes in feature space."),
                                                   tags$li("It can easily extend to multiple classes(multinomial regression) and a natural probabilistic view of class predictions.")
                                                 ),
                                                 p("Disadvantages of Logistic Regression are: "),
                                                 tags$ul(
                                                   tags$li("If the number of observations is lesser than the number of features, Logistic Regression should not be used, otherwise, it may lead to overfitting."),
                                                   tags$li("The major limitation of Logistic Regression is the assumption of linearity between the dependent variable and the independent variables."),
                                                   tags$li("Logistic Regression requires average or no multicollinearity between independent variables.")
                                                 ),
                                                 br(),
                                                 h3("Gradient Boosting"),
                                                 p("Boosting is a class of ensamble learning techniques for regression and classification problems. Boosting aims to build a set of weak learners (i.e. predictive models that are only slightly better than random chance) to create one ‘strong’ 
                                                   learner (i.e. a predictive model that predicts the response variable with a high degree of accuracy). Gradient Boosting is a boosting method which aims to optimise an arbitrary (differentiable) cost function (for example, squared error)."),
                                                 p("Basically, this algorithm is an iterative process in which you follow the following steps:"),
                                                 tags$ol(
                                                   tags$li("Fit a model to the data (in the first iteration this is usually a constant): F(1)x = y"),
                                                   tags$li("Fit a new model to the residuals: h1(x) = y-F1(x)"),
                                                   tags$li("Create a new model: F2(x) = F1(x)+h1(x)"),
                                                   tags$li("Repeat steps 2 & 3")),
                                                 p("It should be evident to see that as the algorithm moves through multiple iterations, the model continually gets stronger. Also, it should be noted that the type of model used in gradient boosting is not restrictive (i.e. it can be anything), 
                                                   however in practice they tend to be shallow decision trees."),
                                                 p("Gradient Boosting has three main components:"),
                                                 tags$ul(
                                                   tags$li("Loss Function)- The role of the loss function is to estimate how good the model is at making predictions with the given data. This could vary depending on the problem at hand. For example, if we're trying to predict the weight of a person 
                                                           depending on some input variables (a regression problem), then the loss function would be something that helps us find the difference between the predicted weights and the observed weights. On the other hand, if we're trying to categorize 
                                                           if a person will like a certain movie based on their personality, we'll require a loss function that helps us understand how accurate our model is at classifying people who did or didn't like certain movies."),
                                                   tags$li("Weak Learner - A weak learner is one that classifies our data but does so poorly, perhaps no better than random guessing. In other words, it has a high error rate. These are typically decision trees (also called decision stumps, because they are less 
                                                           complicated than typical decision trees)."),
                                                   tags$li("Additive Model - This is the iterative and sequential approach of adding the trees (weak learners) one step at a time. After each iteration, we need to be closer to our final model. In other words, each iteration should reduce the value of our loss function.")
                                                 ),
                                                 p("Advantages of Gradient Boosting are: "),
                                                 tags$ul(
                                                   tags$li("Often provides predictive accuracy that cannot be trumped."),
                                                   tags$li("Lots of flexibility - can optimize on different loss functions and provides several hyper parameter tuning options that make the function fit very flexible."),
                                                   tags$li("No data pre-processing required - often works great with categorical and numerical values as is."),
                                                   tags$li("Handles missing data - imputation not required.")
                                                 ),
                                                 p("Disadvantages of Gradient Boosting are: "),
                                                 tags$ul(
                                                   tags$li("Gradient Boosting Models will continue improving to minimize all errors. This can overemphasize outliers and cause overfitting."),
                                                   tags$li("Computationally expensive - often require many trees (>1000) which can be time and memory exhaustive."),
                                                   tags$li("The high flexibility results in many parameters that interact and influence heavily the behavior of the approach (number of iterations, tree depth, regularization parameters, etc.). This requires a large grid search during tuning."),
                                                   tags$li("Loss of interpretation.")
                                                 ),
                                                 ),
                                        
                                        
                                        
                                        tabPanel("Model Fitting", value = "MF",
                                                 # Sidebar layout with input and output definitions ----
                                                 sidebarLayout(
                                                   
                                                   # Sidebar panel for inputs ----
                                                   sidebarPanel(
                                                     radioButtons("split", "Train-Test Split Percentage",
                                                                  c("85%" = "0.85",
                                                                    "80%" = "0.8",
                                                                    "75%" = "0.75",
                                                                    "70%" = "0.7"),
                                                                  selected = character(0),
                                                                  inline = TRUE),
                                                     checkboxGroupInput("variable1", "Choose Variables to include in the model:",
                                                                        c("Gender" = "Gender",
                                                                          "Age" = "Age",
                                                                          "Height(in mt)" = "Height",
                                                                          "Weight(in kg)" = "Weight",
                                                                          "Family with Obesity" = "family_history_with_overweight",
                                                                          "Fast Food Intake" = "FAVC",
                                                                          "Vegetables Consumption Freq" = "FCVC",
                                                                          "Number of meals daily" = "NCP",
                                                                          "Food intake between meals" = "CAEC",
                                                                          "Smoking" = "SMOKE",
                                                                          "Liquid intake daily" = "CH2O",
                                                                          "Calories Consumption" = "SCC",
                                                                          "Physical Activity" = "FAF",
                                                                          "Scheduled deciated to technology" = "TUE",
                                                                          "Alcohol Consumption" = "CALC",
                                                                          "Type of Transportation Used" = "MTRANS"
                                                                          )),
                                                     p(strong("Gradient Boosting Hyperparameter Tuning:")),
                                                     sliderInput("boosttune1", "Max Interaction Depth", 
                                                                 value = 2, 
                                                                 min = 1, 
                                                                 max = 15,
                                                                 step = 1),
                                                     sliderInput("boosttune2", "Max Number of Trees", 
                                                                 value = 25, 
                                                                 min = 25, 
                                                                 max = 250,
                                                                 step = 50),
                                                     sliderInput("boosttune3", "Max Shrinkage", 
                                                                 value = 0.1, 
                                                                 min = 0.01, 
                                                                 max = 1,
                                                                 step = 0.1),
                                                     sliderInput("boosttune4", "Max Minobsinnode", 
                                                                 value = 5, 
                                                                 min = 5, 
                                                                 max = 30,
                                                                 step = 5),
                                                     p(strong("Random Forest Hyperparameter Tuning:")),
                                                     sliderInput("rftune1", "Max Number of variables randomly sampled as candidates at each split", 
                                                                 value = 2, 
                                                                 min = 2, 
                                                                 max = 20,
                                                                 step = 1),
                                                     actionButton("build", "Let's get training!")
                                                     ),
                                                   mainPanel(
                                                     fluidRow(
                                                       column(width = 6,
                                                              h4("Train Accuracy"),
                                                              tableOutput("train")),
                                                       column(width = 6,
                                                              h4("Test Accuracy"),
                                                              tableOutput("test"))),
                                                     tabsetPanel(id = "summarystats",
                                                                 tabPanel("Logistic Regression",
                                                                          verbatimTextOutput("logregstats"),
                                                                          plotOutput("varimplogreg"),
                                                                          verbatimTextOutput("cmlogreg")),
                                                                 tabPanel("Gradient Boosting",
                                                                          verbatimTextOutput("gbmstats"),
                                                                          plotOutput("varimpgbm"),
                                                                          verbatimTextOutput("cmgbm")),
                                                                 tabPanel("Random Forest",
                                                                          verbatimTextOutput("rfstats"),
                                                                          plotOutput("varimprf"),
                                                                          verbatimTextOutput("cmrf")))
                                                   ))),
                                        tabPanel("Prediction",
                                                 selectInput("sex", "Gender:",
                                                             c("Male" = "Male",
                                                               "Female" = "Female")),
                                                 textInput("age", "Age", "25"),
                                                 textInput("height", "Height", "1.5"),
                                                 textInput("weight", "Weight", "55"),
                                                 selectInput("family_history_with_overweight", "Family History with Obesity?",
                                                             c("yes" = "yes",
                                                               "no" = "no")),
                                                 selectInput("FAVC", "Fast Food Intake",
                                                             c("yes" = "yes",
                                                               "no" = "no")),
                                                 selectInput("FCVC", "Vegetables Consumption Frequency",
                                                             c("1" = "1",
                                                               "2" = "2",
                                                               "3" = "3")),
                                                 selectInput("NCP", "Number of Main Meals Daily",
                                                             c("1" = "1",
                                                               "2" = "2",
                                                               "3" = "3",
                                                               "4" = "4")),
                                                 selectInput("CAEC", "Food intake between Meals",
                                                             c("Sometimes" = "Sometimes",
                                                               "Frequently" = "Frequently",
                                                               "Always" = "Always",
                                                               "No" = "No")),
                                                 selectInput("SMOKE", "Smoking",
                                                             c("no" = "no",
                                                               "yes" = "yes"
                                                               )),
                                                 selectInput("CH20", "Liquid Intake Daily",
                                                             c("1" = "1",
                                                               "2" = "2",
                                                               "3" = "3"
                                                             )),
                                                 selectInput("SCC", "Calories Consumption Calculator",
                                                             c("no" = "no",
                                                               "yes" = "yes"
                                                             )),
                                                 selectInput("FAF", "Physical Activity",
                                                             c("0" = "0",
                                                               "1" = "1",
                                                               "2" = "2",
                                                               "3" = "3"
                                                             )),
                                                 selectInput("TUE", "Schedule deicated to technology",
                                                             c("0" = "0",
                                                               "1" = "1",
                                                               "2" = "2"
                                                             )),
                                                 selectInput("CALC", "Alcohol Consumption",
                                                             c("Sometimes" = "Sometimes",
                                                               "Frequently" = "Frequently",
                                                               "Always" = "Always",
                                                               "No" = "No")),
                                                 selectInput("MTRANS", "Main Transport Medium",
                                                             c("Public_Transportation" = "Public_Transportation",
                                                               "Walking" = "Walking",
                                                               "Automobile" = "Automobile",
                                                               "Motorbike" = "Motorbike",
                                                               "Bike" = "Bike")),
                                                 actionButton("predict", "Let's get predicting!"),
                                                 mainPanel(verbatimTextOutput("predoutput"))
                                                 ))
                  
                  
)
)
)
)


