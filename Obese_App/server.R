#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

set.seed(100012)

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
  if (length(model_variables)!=0) {
    model_var_obesedf = obesity_df[, model_variables, drop = FALSE]
  }
  else {
    model_var_obesedf <- obesity_df %>% select(-any_of("NObeyesdad"))
  }
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
  return(train(x=obeseTrain[-1], y= obeseTrain$obesityLevel, 
                    method = 'glmnet',
                    trControl = trControl,
                    family = 'multinomial'))
}

gbm_model <- function(obeseTrain, interaction.depth, n.trees, shrinkage, n.minobsinnode){
  gbmGrid <-  expand.grid(interaction.depth = seq(2, interaction.depth), 
                          n.trees = seq(20,n.trees), 
                          shrinkage = shrinkage,
                          n.minobsinnode = seq(1,n.minobsinnode))
  
  return(train(x=obeseTrain[-1], y= obeseTrain$obesityLevel, 
                    method = 'gbm',
                    trControl = trControl,
                    tuneGrid = gbmGrid,
                  verbose = FALSE))
}

randomforest_model <- function(obeseTrain, mtry){
  tuneGrid = expand.grid(.mtry=seq(5,mtry))
  return(train(x=obeseTrain %>% select(-c("obesityLevel")), y= obeseTrain$obesityLevel, 
                  method = 'rf',
                  trControl = trControl,
                  tuneGrid = tuneGrid))
}

# gbmGrid <-  expand.grid(interaction.depth = c(1, 2),
#                         n.trees = c(25, 50),
#                         shrinkage = 0.1,
#                         n.minobsinnode = c(5, 10))

# gbm.CV <- train(x=dplyr::select(obesity_df, -"NObeyesdad"), y= obesity_df$NObeyesdad,
#                 method = 'rf',
#                 trControl = trControl,
#                 data)


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
        ggtitle(paste0(input$var1, "vs", input$var2)) +
        theme_minimal() 
    })
    }
  })

  observe({
  if(input$type1 == "corr"){
    cormat <- round(cor(obesity_df[,c("Height", "Age", "Weight")]),2)
    melted_cormat <- melt(cormat)
    output$corr <- renderPlot({
      ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))
      
  })
  }
  })
  
  output$catplot1 <- renderPlot({
    ggplot(obesity_df) +
      aes(x = !!sym(input$type3), y = !!sym(input$type2)) +
      geom_boxplot(fill = "#112446") +
      coord_flip() +
      ggtitle(paste0("Count of", input$type3, "by", input$type2)) +
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
      ggtitle(paste0("Count of", input$type3, "by", input$type2)) +
      theme_minimal()
  })
  
  values <- reactiveValues(df_list = NULL, rf.fit=NULL)
  
  
  observeEvent(input$build, {
    withProgress(message = "Model Building", value = 0, {
    model_variables <- c(input$variable1)
    obese_df_preproc = preprocessing(model_variables)
    p = input$split
    values$df_list <- model_split(obese_df_preproc, as.numeric(p))
    obeseTrain <- as_tibble(values$df_list[[1]])
    obeseTest <- as_tibble(values$df_list[[2]])
    interaction.depth = as.numeric(input$boosttune1)
    n.trees = as.numeric(input$boosttune2)
    shrinkage = as.numeric(input$boosttune3)
    n.minobsinnode = as.numeric(input$boosttune4)
    mtry = as.numeric(input$rftune1)
    incProgress(17, detail = "Training Logistic Regression Model")
    logreg.fit = logreg_model(obeseTrain)
    incProgress(34, detail = "Logistic Regression Model Built")
    incProgress(35, detail = "Training Gradient Boosting Model")
    gbm.fit = gbm_model(obeseTrain, interaction.depth, n.trees, shrinkage, n.minobsinnode)
    incProgress(60, detail = "Gradient Boosting Model Built")
    incProgress(62, detail = "Training Random Forest Model")
    values$rf.fit = randomforest_model(obeseTrain, mtry)
    incProgress(90, detail = "Random Forest Model Built")
    incProgress(91, detail = "Getting Statistics...")
    boosted_tree_pred <- predict(gbm.fit, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    logreg_tree_pred <- predict(logreg.fit, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    rf_tree_pred <- predict(values$rf.fit, dplyr::select(obeseTest, -"obesityLevel"), type = "raw")
    logreg_test_accuracy <- postResample(pred = logreg_tree_pred, obs = obeseTest$obesityLevel)
    boosted_test_accuracy <- postResample(pred = logreg_tree_pred, obs = obeseTest$obesityLevel)
    rf_test_accuracy <- postResample(pred = rf_tree_pred, obs = obeseTest$obesityLevel)
    incProgress(100, detail = "All done!")
    models <- c("Logistic Regression", "Gradient Boost Model", "Random Forest")
    train_accuracy <- c(logreg.fit$results$Accuracy[which.max(logreg.fit$results$Accuracy)]*100,
                        gbm.fit$results$Accuracy[which.max(gbm.fit$results$Accuracy)]*100,
                        values$rf.fit$results$Accuracy[which.max(values$rf.fit$results$Accuracy)]*100)
    train_accuracy <- tibble(Accuracy = train_accuracy) %>% 
      mutate(Models = models) %>% 
      select(Models, everything())
    
    output$train <- renderTable(train_accuracy, align = "c")
    test_accuracy <- logreg_test_accuracy %>%
      bind_rows(boosted_test_accuracy, rf_test_accuracy) %>% 
      mutate(Models = models, Accuracy_New = Accuracy*100) %>% 
      select(c(Models, Accuracy_New)) %>% 
      rename(Accuracy = Accuracy_New)
    output$test <- renderTable(test_accuracy, align = "c")
    output$logregstats <- renderPrint(logreg.fit)
    output$gbmstats <- renderPrint(gbm.fit)
    output$rfstats <- renderPrint(values$rf.fit)
    
    output$varimplogreg <- renderPlot({
      plot(varImp(logreg.fit, scale = FALSE))
    })
    output$varimprf <- renderPlot({
      plot(varImp(values$rf.fit, scale = FALSE))
    })
    output$varimpgbm <- renderPlot({
      plot(varImp(gbm.fit, scale = FALSE))
    })
    output$cmlogreg <- renderPrint(confusionMatrix(logreg_tree_pred, obeseTest$obesityLevel))
    output$cmgbm <- renderPrint(confusionMatrix(boosted_tree_pred, obeseTest$obesityLevel))
    output$cmrf <- renderPrint(confusionMatrix(rf_tree_pred, obeseTest$obesityLevel))
    })
    })
    
    a <- reactiveValues(result = NULL)
  
    observeEvent(input$predict, {
      obese_df_preproc = obesity_df %>% 
        rename(obesityLevel = NObeyesdad)
      p = 0.8
      df_list <- model_split(obese_df_preproc, p)
      obeseTrain <- as_tibble(df_list[[1]])
      obeseTest <- as_tibble(df_list[[2]])
      print(obeseTrain)
      rf.final <- randomforest_model(obeseTrain, mtry = 6)
      print("rf completed")
      test_pred <- obeseTest %>% 
        select(-c("obesityLevel"))
      print("*****")
      print(test_pred)
      predict_input <- data.frame(input$sex, 
        as.numeric(input$age),
        as.numeric(input$height),
        as.numeric(input$weight),
        input$family_history_with_overweight,
        input$FAVC,
        input$FCVC,
        input$NCP,
        input$CAEC,
        input$SMOKE,
        input$CH20,
        input$SCC,
        input$FAF,
        input$TUE,
        input$CALC,
        input$MTRANS)
      print(predict_input)
      
      print(length(colnames(test_pred)))
      print(length(colnames(predict_input)))
      
      names(predict_input) <- names(test_pred)
      
      #Include the values into the new data
      test_pred <- rbind(test_pred, predict_input)
    
      print("**binding done**")
      a$result <-  predict(rf.final,
                                 newdata = test_pred[nrow(test_pred),])
      
      showModal(modalDialog(
        paste0("You are ",a$result,'.'),
        easyClose = TRUE,
        footer = NULL
      ))
                         

    })
    
    output$predoutput <- renderText({
      #Display the prediction value
      paste(a$result)
    })

      # gbmGrid <-  reactive({expand.grid(interaction.depth = gbm$bestTune$interaction.depth,
      #                         n.trees = gbm$bestTune$n.trees,
      #                         shrinkage = gbm$bestTune$shrinkage,
      #                         n.minobsinnode = gbm$bestTune$n.minobsinnode)})

      # finalGbm <- train(x=dplyr::select(obesity_df, -"NObeyesdad"), y= obesity_df$NObeyesdad,
      #       method = 'gbm',
      #       trControl = trControl,
      #       tuneGrid = gbmGrid(),
      #       verbose = FALSE)
      

      #gbm_pred <- predict(finalGbm, new_data = predict_input(), type = "raw")
      #output$predoutput <- renderPrint(gbmGrid())
      #output$predoutput <- renderTable(predict_input())
    
    output$ex1 <- renderUI({
      withMathJax(helpText('$$\\frac{1}{1+e^{-(\\beta_0+\\beta_1*x)}}$$'))
    })
    
    output$ex2 <- renderUI({
      withMathJax(helpText('$$ni_j = w_jC_j - w_{left}(j)C_{left(j)} = w_{right(j)}C_{right(j)}$$'))
    })
    
})
  
# 
# predict_input <- data.frame(Gender = "Female",
#                             Age = 25,
#                             Height = 1.7,
#                             Weight = 67,
#                             family_history_with_overweight = "yes",
#                             FAVC = "no",
#                             FCVC = 2,
#                             NCP = 3,
#                             CAEC = "Sometimes",
#                             SMOKE = "no",
#                             CH2O = 2,
#                             SCC = "no",
#                             FAF = 0,
#                             TUE = 1,
#                             CALC = "no",
#                             MTRANS = "Public_Transportation")

# predict_input$Gender <- factor(predict_input$Gender)
# predict_input$family_history_with_overweight <- factor(predict_input$family_history_with_overweight)
# predict_input$FAVC <- factor(predict_input$FAVC)
# predict_input$FCVC <- factor(round(predict_input$FCVC))
# predict_input$NCP <- factor(round(predict_input$NCP))
# predict_input$CAEC <- factor(predict_input$CAEC)
# predict_input$SMOKE <- factor(predict_input$SMOKE)
# predict_input$CH2O <- factor(round(predict_input$CH2O))
# predict_input$SCC <- factor(predict_input$SCC)
# predict_input$FAF <- factor(round(predict_input$FAF))
# predict_input$TUE <- factor(round(predict_input$TUE))
# predict_input$CALC <- factor(predict_input$CALC)
# predict_input$MTRANS <- factor(predict_input$MTRANS)
# 
# predict(gbm.CV, new_data = head(obesity_df,5))

