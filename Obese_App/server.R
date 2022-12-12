#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Obesity Levels 
classification <- c("Underweight", "Normal", "Overweight", "Obesity I", "Obesity II", "Obesity III")
bmiValue <- c("Less than 18.5", "18.5 to 24.9", "25.0 to 29.9", "30.0 to 34.9", "35.0 to 39.9", "Higher than 40.0")
obesityLevel <- data.frame(classification, bmiValue)

# import dataset
obesity_df = read.csv("./Data/ObesityDataSet_raw_and_data_sinthetic.csv")
obesity_df <- as_tibble(obesity_df)
obesity_df$Gender <- factor(obesity_df$Gender)
obesity_df$family_history_with_overweight <- factor(obesity_df$family_history_with_overweight)
obesity_df$FAVC <- factor(obesity_df$FAVC)
obesity_df$FCVC <- factor(round(obesity_df$FCVC))
obesity_df$NCP <- factor(round(obesity_df$NCP))
obesity_df$CAEC <- factor(obesity_df$CAEC)
obesity_df$SMOKE <- factor(obesity_df$SMOKE)
obesity_df$CH2O <- factor(round(obesity_df$CH2O))
obesity_df$SCC <- factor(obesity_df$SCC)
obesity_df$FAF <- factor(round(obesity_df$FAF))
obesity_df$TUE <- factor(round(obesity_df$TUE))
obesity_df$CALC <- factor(obesity_df$CALC)
obesity_df$MTRANS <- factor(obesity_df$MTRANS)
obesity_df$NObeyesdad <- factor(obesity_df$NObeyesdad)

preprocessing <- function(model_variables) {
  model_var_obesedf = obesity_df[, model_variables, drop = FALSE]
  model_var_obesedf <- cbind(NObeyesdad = obesity_df$NObeyesdad, model_var_obesedf)
  dummy_obesity <- dummyVars(NObeyesdad~ ., data = model_var_obesedf)
  dummy_obesity_ml <- as_tibble(predict(dummy_obesity, newdata = model_var_obesedf))
  dummy_obesity_ml <- cbind(obesityLevel = obesity_df$NObeyesdad, dummy_obesity_ml)
  preProcValues <- preProcess(dummy_obesity_ml, method = c("center", "scale"))
  obeseDfTransformed <- predict(preProcValues, dummy_obesity_ml)
  return(obeseDfTransformed)
}


model_split <- function(obese_df_preproc, p){
  trainIndex <- createDataPartition(obese_df_preproc$obesityLevel, p = 0.8, 
                                    list = FALSE)
  obeseTrain <- obese_df_preproc[trainIndex,]
  obeseTest  <- obese_df_preproc[-trainIndex,]
  return(list(obeseTrain, obeseTest))
}


trControl <- trainControl(method = 'repeatedcv',
                          number = 3,
                          repeats =  3,
                          search = 'random')

logreg_model <- function(obeseTrain){
  return(train(x=obeseTrain, y= obeseTrain$obesityLevel, 
                    method = 'glmnet',
                    trControl = trControl,
                    family = 'multinomial'))
}

gbm_model <- function(obeseTrain, interaction.depth, n.trees, shrinkage, n.minobsinnode){
  gbmGrid <-  expand.grid(interaction.depth = interaction.depth, 
                          n.trees = n.trees, 
                          shrinkage = shrinkage,
                          n.minobsinnode = n.minobsinnode)
  
  return(train(x=obeseTrain, y= obeseTrain$obesityLevel, 
                    method = 'gbm',
                    trControl = trControl,
                    tuneGrid = gbmGrid,
                  verbose = FALSE))
}

randomforest_model <- function(obeseTrain, mtry){
  mtry_list <-  mtry
  return(train(x=obeseTrain, y= obeseTrain$obesityLevel, 
                  method = 'rf',
                  trControl = trControl,
                  tuneGrid = rfGrid))
}

# gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 4, 5, 6, 7, 8),
#                         n.trees = c(25, 50, 100, 150, 200, 250, 300),
#                         shrinkage = 0.1,
#                         n.minobsinnode = c(5, 10, 15, 20))
# gbm.CV <- train(x=obeseTrain[-1]  , y= obeseTrain$obesityLevel,
#                 method = 'gbm',
#                 trControl = trControl,
#                 tuneGrid = gbmGrid)


# data subset 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # observeEvent(input$variable, {
  #   print(typeof(input$variable))
  # })
  output$whoObesity <- renderTable(obesityLevel, align = "c")
  observeEvent(input$download, {
    write_csv(subsetDf, "Output_Data.csv")
  })
  output$subsetData <- renderDataTable({
    subsetDf <- head(obesity_df[, c(input$variable), drop = FALSE], input$n)
})
  observe({
    if(input$type1 == "ss"){
      output$summarystats <- renderPrint(summary(obesity_df))
    }
  })
  
  observe({
  if(length(input$numvar)!=0){
  output$distplot <- renderPlot({
    ggplot(gather(obesity_df[, c(input$numvar)]), aes(value)) + 
      geom_histogram(bins = 10, color="#e9ecef", fill="#69b3a2") + 
      facet_wrap(~key, scales = 'free_x') + 
      theme_light()
  })
  }
  })
  
  observe({
    if(input$type1 == "scatter"){
    output$scatter <- renderPlot({
      ggplot(obesity_df) +
        aes(x = !!sym(input$var1), y = !!sym(input$var2), colour = NObeyesdad) +
        geom_point(shape = "circle", size = 1.5) +
        scale_color_hue(direction = 1) +
        theme_minimal() 
    })
    }
  })

  observe({
  if(input$type1 == "corr"){
    cormat <- round(cor(obesity_df[,c("Height", "Age", "Weight")]),2)
    melted_cormat <- melt(cormat)
    output$corr <- renderPlot({
      ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()
  })
  }
  })
  
  output$catplot1 <- renderPlot({
    ggplot(obesity_df) +
      aes(x = !!sym(input$type3), y = !!sym(input$type2)) +
      geom_boxplot(fill = "#112446") +
      coord_flip() +
      theme_minimal()
  })
  
  output$catplot2 <- renderPlot({
    ggplot(obesity_df) +
      aes(x = !!sym(input$type2), fill = NObeyesdad) +
      geom_bar() +
      scale_fill_hue(direction = 1) +
      theme_minimal()
  })
  
  output$catplot3 <- renderPlot({
    ggplot(obesity_df) +
      aes(x = !!sym(input$type3), y = !!sym(input$type2), fill = NObeyesdad) +
      geom_boxplot() +
      scale_fill_hue(direction = 1) +
      theme_minimal()
  })
  
  observeEvent(input$build, {
    withProgress(message = "Model Building", value = 0, {
    model_variables <- c(input$variable1)
    obese_df_preproc = preprocessing(model_variables)
    p = input$split
    df_list <- model_split(obese_df_preproc, as.numeric(p))
    obeseTrain <- as_tibble(df_list[[1]])
    obeseTest <- as_tibble(df_list[[2]])
    interaction.depth = input$boosttune1
    n.trees = input$boosttune2
    shrinkage = input$boosttune3
    n.minobsinnode = input$boosttune4
    mtry = input$rftune1
    incProgress(17, detail = "Training Logistic Regression Model")
    output$train <- renderTable(obeseTrain)
    logreg = logreg_model(obeseTrain)
    incProgress(34, detail = "Logistic Regression Model Built")
    incProgress(35, detail = "Training Gradient Boosting Model")
    gbm = gbm_model(obeseTrain, interaction.depth, n.trees, shrinkage, n.minobsinnode)
    incProgress(60, detail = "Gradient Boosting Model Built")
    incProgress(62, detail = "Training Random Forest Model")
    rf = randomforest_model(obeseTrain, mtry)
    incProgress(90, detail = "Random Forest Model Built")
    incProgress(91, detail = "Getting Statistics...")
    boosted_tree_pred <- predict(gbm, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    logreg_tree_pred <- predict(logreg, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    rf_tree_pred <- predict(rf, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    logreg_test_accuracy <- postResample(pred = logreg_tree_pred, obs = obeseTest$obesityLevel)
    boosted_test_accuracy <- postResample(pred = logreg_tree_pred, obs = obeseTest$obesityLevel)
    rf_test_accuracy <- postResample(pred = rf_tree_pred, obs = obeseTest$obesityLevel)
    incProgress(100, detail = "All done!")
    models <- c("Logistic Regression", "Gradient Boost Model", "Random Forest")
    train_accuracy <- c(logreg$results$Accuracy[which.max(logreg$results$Accuracy)],
                        gbm$results$Accuracy[which.max(gbm$results$Accuracy)],
                        rf$results$Accuracy[which.max(rf$results$Accuracy)])
    output$train <- renderTable(train_accuracy, align = "c")
    test_accuracy <- logreg_test_accuracy %>%
      bind_rows(boosted_test_accuracy, rf_test_accuracy) %>%
      bind_cols(models)
    output$test <- renderTable(test_accuracy, align = "c")
    })
  })
  
  
})

