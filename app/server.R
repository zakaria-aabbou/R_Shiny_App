library(shiny)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(DT)
library(CGPfunctions)
library(mice)
library(VIM)
library(arsenal) 


# read the global file for data
source('global.R')


server <- function(session, input, output) {
  
  
  #afficahge de dataset
  output$donnees <- DT::renderDataTable({
    DT::datatable(data, options = list(scrollX = TRUE))
  })
  table_one <- tableby(stroke_c ~ ., data = data) 
  output$summary1 <- renderPrint({ summary(table_one, text=TRUE, title = "Statistiques récapitulatives pour l'ensemble des variables") })
  # output$summary1 <- renderPrint({ summary(data)  })
  
  #le tableau de nombre de NA dans chaque colonne
  #output$nb_NA <- renderTable( sapply(data,function(x) sum(is.na(x))))
  output$plot_nan <- renderPlot({md.pattern(data)})

  #output$missing_values <- renderPlot({aggr(data, prop = FALSE, numbers = TRUE)})
  output$summary_missing_values <- renderPrint({ summary(aggr(data, prop = FALSE, numbers = TRUE , plot=FALSE))  })

  # AVC analysis by sex, age range 
  output$BMI_analysis <- renderPlot({ data %>%
        ggplot(aes(x=age_interval, fill=stroke_c)) +
        geom_bar(aes(y=..count..)) +
        stat_boxplot(geom ='errorbar') +
        scale_fill_manual(values=c("green", "red"), aesthetics = "fill") +
        ggtitle("Analyse d'AVC par sexe, tranche d'âge") +
        facet_grid(stroke_c~gender, scales = "free_y")
  })
    
  
  #affichage de la table de nos données
  #output$contents <- renderTable({(data[,input$column])})
  
  # # Commande pour le calcul du summary
  # output$summary <- renderPrint({ t(summary(data[,input$column])) })
  # 
  # # Commande pour l'affichage du plot des effectifs
  # output$effectifsAge <- renderPlot({ 
  #   plot(table(data$age), col ="azure3", xlab ="âge", ylab ="Effectifs", 
  #        main ="Distribution des effectifs pour l'âge des patients")
  # })
  # output$effectifsGluc <- renderPlot({ 
  #   hist((data$avg_glucose_level), col ="aliceblue", xlab ="avg_glucose_level", ylab ="Effectifs", 
  #        main ="Distribution des effectifs pour le niveau moyen de glucose")
  # })
  # output$effectifsBmi <- renderPlot({ 
  #   hist(data$bmi ,col ="aliceblue", xlab ="bmi", ylab ="Effectifs", 
  #        main ="Distribution des effectifs pour l'indice de masse corporelle")
  # })
  # 
  #commentaires
  output$comment_varqant <- renderText({
  paste("On remarque que ya des valeurs inférieurs à 1 pour l'âge, cela revient à l'interpretation des mois par des valeurs réelles positives.","En ce qui concerne la distribution de chaque variable,
                                                le niveau moyen de glucose et l'indice de masse corporelle (bmi) semblent être normallement distribuées contrairement à lâge des patients."
  )})

  #commentaires3
  output$comment_varqant3 <- renderText({

    if(input$num_var3 == "age"){
      paste("Les graphes montrent que la densité des personnes âgées de 50 ans et plus subissant un AVC est plus élevée.")
      
    }else if (input$num_var3 == "bmi"){
      paste("Les graphes montrent que la densité des personnes en surpoids qui ont subi un AVC est plus élevée.")

      
    }else if (input$num_var3 == "avg_glucose_level"){
      paste("Les graphes montrent que la densité des personnes avec un taux de glucose inférieur à 100 a subi davantage l'AVC.")

    }
  })
  
  
  #commande pour l'affichage d'histogramme
  output$colonnes <- renderPlot({
    barplot(table(data[,input$column2]),main = paste("Hist_",input$column2), ylab="Effectifs",ylim = c(0,3500),col="aquamarine",names.arg = substr(names(table(data[,input$column2])),1,20))
  })
  #commande pour l'affichage de diag en secteurs
  output$secteurs <- renderPlot({
    pie(table(data[,input$column2]), labels = substr(names(table(data[,input$column2])),1,20),main = paste("Pie_",input$column2),col = c())})
  
  
  
#barplot des données desequilibrees 
  output$stroke_deseq <- renderPlot({
    barplot(table(stroke_imbalance),main = "stroke", ylab="Effectifs",ylim = c(0,2000),col=c("aquamarine","orange"),names.arg = substr(names(table(stroke_imbalance)),1,20))
  })
  #output$percent_stroke_imbalance <- renderPrint({ prop.table(stroke_imbalance)  })
  output$percent_stroke_imbalance <- renderText( prop.table(table(stroke_imbalance)))
  output$stroke_eq <- renderPlot({
    barplot(table(data$stroke_c),main = "stroke", ylab="Effectifs",ylim = c(0,2000),col=c("aquamarine","orange"),names.arg = substr(names(table(stroke_imbalance)),1,20))
  })
  
  #########partie bivar
  ################2 var  quantitatives
  # Nuage de points
  # ----
  
  output$nuagePoints <- renderPlot({
    x.var = which(colnames(data)==input$column3); y.var = which(colnames(data)==input$column4);
    
    # # Simple nuage de point
    # options(scipen=999)
    # #x.var = 13; y.var = 11;
    # plot(x = data[, x.var], y = data[, y.var], col = "chocolate1",
    #      las = 2, cex.axis = 0.7,
    #      main = paste(input$column4, "en fonction de", input$column3 ),
    #      xlab = x.var, ylab = y.var, cex.lab = 1.2
    # )
    # options(scipen=0)

# __Correlation Level #####
    data %>% 
      ggplot(aes(data[, x.var], data[, y.var])) +
      geom_point() +
      geom_smooth() +
      labs(title = "Correlation Level" , x= input$column3 , y=input$column4) +
      theme(plot.title = element_text(size = 10, face = "bold")) +
      theme(plot.margin = unit(c(1,0,1,0), "cm"))
    
  })
  
  
  #coeff de correlation
  output$correlation <- renderText({
    x.var = which(colnames(data)==input$column3); y.var = which(colnames(data)==input$column4);
    coeff_value <- cov(data[, x.var], data[, y.var])/(sqrt(var(data[, x.var])*var(data[, y.var])))
    paste("Coefficient de correlation lineaire =", round(coeff_value,digits = 2))
  })

  
  ################2 var  quantitatives
  # Bidimensionnel
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'Level' et 'Sex'
    col1 <- input$column5
    col2 <- input$column6
    ggplot(data, aes_string(x = col1, fill = col2)) + geom_bar()+scale_fill_brewer(palette="Dark2")
  })
  
  
  #-------------
  # training model
  #------------
  
  
  # output$trainModel <- renderText({
  #   GLM_Model<- glm(stroke~age+hypertension+heart_disease+avg_glucose_level+bmi, family = binomial,data = train_data)
  #   #summary(GLM_Model)
  #   pred_train_1 <- predict(GLM_Model, type = "response")
  #   pred_train_2 <- ifelse(pred_train_1 >0.2,1,0)
  #   pred_train_2
  #   
  # })
  
  ############################GLM modle
  GLM_Model<- glm(stroke~age+hypertension+heart_disease+avg_glucose_level+bmi, family = binomial,data = train_data)
  output$sum_train_glm <- renderPrint({summary(GLM_Model)})
  #entrainement
  pred_train_1 <- predict(GLM_Model, type = "response")
  pred_train_2 <- ifelse(pred_train_1 >0.2,1,0)
  
  #matrice de confusion 
  mat_conf_glm_1 <- table(Predictions = pred_train_2, TrueLabels = train_data$stroke)
  output$MatConfGlm1 <- renderPlot({fourfoldplot(mat_conf_glm_1, color = c("red", "green"),
                                                 conf.level = 0, margin = 1, main = "Confusion Matrix-GLM-Train")})
  
  #accuracy_glm_train
  acc_glm_1 <- ( mat_conf_glm_1[2,2] + mat_conf_glm_1[1,1] ) / (length(train_data$stroke))
  cat("Accuracy = ",acc_glm_1 )
  output$Accuracy_glm_train <- renderText({paste("Accuracy_glm_train = ",acc_glm_1)})
  
  #score
  #F1
  Precision1 =  mat_conf_glm_1[2,2] / ( mat_conf_glm_1[2,2] + mat_conf_glm_1[2,1] )
  cat("\nPrecision = ",Precision1 )
  Recall1 =  mat_conf_glm_1[2,2] / ( mat_conf_glm_1[2,2] + mat_conf_glm_1[1,2] )
  cat("\nRecall = ",Recall1 )
  F1score1  = 2*(Recall1 * Precision1) / (Recall1 + Precision1)
  cat("\nF1 score = ",F1score1 )
  output$F1score_glm_train <- renderText({paste("F1 score_glm_train = ",F1score1)})
  
  #test
  
  pred_test_1 <- predict(GLM_Model, newdata = test_data, type = "response")
  pred_test_2<- ifelse(pred_test_1 >0.2,1,0)
  #matrice de confusion
  mat_conf_glm_2 <- table(Predictions = pred_test_2, TrueLabels = test_data$stroke)
  
  output$MatConfGlm2 <-renderPlot({fourfoldplot(mat_conf_glm_2, color = c("#F15854", "#60BD68"),
                                                conf.level = 0, margin = 1, main = "Confusion Matrix-GLM-Test")})
  
  
  #Accuracy_glm_test
  acc_glm_2 <- ( mat_conf_glm_2[2,2] + mat_conf_glm_2[1,1] ) / (length(test_data$stroke))
  cat("Accuracy = ",acc_glm_2 )
  
  output$Accuracy_glm_test <- renderText({paste("Accuracy_glm_test = ",acc_glm_2)})
  
  Precision12 =  mat_conf_glm_2[2,2] / ( mat_conf_glm_2[2,2] + mat_conf_glm_2[2,1] )
  cat("\nPrecision = ",Precision1 )
  Recall12 =  mat_conf_glm_2[2,2] / ( mat_conf_glm_2[2,2] + mat_conf_glm_2[1,2] )
  cat("\nRecall = ",Recall1 )
  F1score12  = 2*(Recall12 * Precision12) / (Recall12 + Precision12)
  cat("\nF1 score = ",F1score12 )
  
  output$F1score_glm_test <- renderText({paste("F1 score_glm_test = ",F1score12)})
  
 #courbe ROC GLM
  
  output$ROC1 <- renderPlot({#define object to plot
    rocobj <- roc(test_data$stroke, pred_test_1)
    auc <- round(auc(test_data$stroke, pred_test_1),4)
    
    #create ROC plot
    ggroc(rocobj,alpha = 0.5, colour = "black")+ggtitle(paste('ROC Curve for GLM model ', '(AUC = ', auc, ')'))
    
  })

  ############################RF modle
  
  data$hypertension <- as.factor(data$hypertension)
  data$heart_disease <- as.factor(data$heart_disease)
  data$stroke <- as.factor(data$stroke)
 
  #train
  rf <- randomForest(stroke ~ age+hypertension+heart_disease+avg_glucose_level+bmi, data = train_data, ntree = 1000, mtry = 1)
  output$sum_train_rf <- renderPrint({summary(rf)})
  predRF_train <- predict(rf)
  
  #Confusion Matrix
  mat_conf_rf_1 <- table(Predictions = predRF_train, TrueLabels = train_data$stroke)
  
  output$MatConfRF1 <-renderPlot({  fourfoldplot(mat_conf_rf_1, color = c("red", "green"),
                                                 conf.level = 0, margin = 1, main = "Confusion Matrix-RF-Train")})
 
  
  
  #accuracy_rf_train
  acc_rf_1 <- ( mat_conf_rf_1[2,2] + mat_conf_rf_1[1,1] ) / (length(train_data$stroke))
  cat("Accuracy = ",acc_rf_1 )
  output$Accuracy_rf_train <- renderText({paste("Accuracy_rf_train = ",acc_rf_1)})
  
  #score
  #F1
  Precision2 =  mat_conf_rf_1[2,2] / ( mat_conf_rf_1[2,2] + mat_conf_rf_1[2,1] )
  cat("\nPrecision = ",Precision1 )
  Recall2 =  mat_conf_rf_1[2,2] / ( mat_conf_rf_1[2,2] + mat_conf_rf_1[1,2] )
  cat("\nRecall = ",Recall1 )
  F1score2  = 2*(Recall2 * Precision2) / (Recall2 + Precision2)
  cat("\nF1 score = ",F1score2 )
  output$F1score_rf_train <- renderText({paste("F1 score_rf_train = ",F1score2)})
  
  #test
  predRF_test <- predict(rf, newdata = test_data)
  print(class(predRF_test))
  mat_conf_rf_2 <- table(Predictions = predRF_test, TrueLabels = test_data$stroke)
 
  output$MatConfRF2 <-renderPlot({fourfoldplot(mat_conf_rf_2, color = c("#F15854", "#60BD68"),
                                               conf.level = 0, margin = 1, main = "Confusion Matrix-RF-Test")}) 
  
  
  
  #Accuracy
  acc_rf_2 <- ( mat_conf_rf_2[2,2] + mat_conf_rf_2[1,1] ) / (length(test_data$stroke))
  cat("Accuracy = ",acc_rf_2 )
  
  output$Accuracy_rf_test <- renderText({paste("Accuracy_rf_test = ",acc_rf_2)})
  
  #F1
  Precision22 =  mat_conf_rf_2[2,2] / ( mat_conf_rf_2[2,2] + mat_conf_rf_2[2,1] )
  cat("\nPrecision = ",Precision2 )
  Recall22 =  mat_conf_rf_2[2,2] / ( mat_conf_rf_2[2,2] + mat_conf_rf_2[1,2] )
  cat("\nRecall = ",Recall2 )
  F1score22  = 2*(Recall22 * Precision22) / (Recall22 + Precision22)
  cat("\nF1 score = ",F1score22 )
  
  
  output$F1score_rf_test <- renderText({paste("F1 score_rf_test = ",F1score22)})
  
  #Courbe ROC rf
  #predRF_test_prob <- predict(rf, newdata = test_data,type ="prob")
  
  predrf <- as.numeric(predRF_test)
  #predrf <-as.numeric(predRF_test_prob)
  print(class(predrf))
  output$ROC2 <- renderPlot({#define object to plot
    rocobj1 <- roc(test_data$stroke, predrf)
    auc1 <- round(auc(test_data$stroke, predrf),4)
    
    #create ROC plot
    ggroc(rocobj1,alpha = 0.5, colour = "black")+ggtitle(paste('ROC Curve for RF model ', '(AUC = ', auc1, ')'))
    
  })
  
  
  
  #-------------
  # prediction
  #------------
  
  output$predictText <- renderText({
    
    
    #display table of the prediction
    strokeLogregModel = glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level + bmi, 
                            data=data, 
                            family=binomial(link="logit"))
    
    
    # strokeRFModel = randomForest(stroke ~ age+hypertension+heart_disease+avg_glucose_level+bmi, data = data1, ntree = 1000, mtry = 1)
    
    newData <- data.frame(
                          # gender = input$gender_choice,
                          age = input$age,
                          hypertension = input$Hypertension, 
                          heart_disease = input$HeartDisease,
                          # ever_married = input$ever_married_choice,
                          # work_type = input$work_type_choice,
                          # Residence_type = input$Residence_type_choice,
                          avg_glucose_level = input$avgGlucLvl,
                          bmi = input$bmi
                          # smoking_status = input$smoking_status_choice
                          )

    strokeProb <- predict(strokeLogregModel,
                          newData,
                          type="response")
    
    print(strokeProb )
    
    predicted.stroke <- ifelse(strokeProb > 0.2, "Attention!! Vous êtes plus à risque de subir un AVC."
                               , "Vous n'êtes pas à risque d'avoir un AVC.")
    
  })
  
  # --------------------
  # bar plot section
  # --------------------
  
  
  
  
  # selectedCatVariable
  output$barchart1 <- renderPlot({
    PlotXTabs(data, stroke, c(1), "percent")
  })
  
  
  output$barchart2 <- renderPlot({
    PlotXTabs(data, stroke, c(5), "percent")
  })
  
  output$barchart3 <- renderPlot({
    PlotXTabs(data, stroke, c(6), "percent")
  })
  
  output$barchart4 <- renderPlot({
    PlotXTabs(data, stroke, c(7), "percent")
  })
  
  output$barchart5 <- renderPlot({
    PlotXTabs(data, stroke, c(10), "percent")
  })
  
  output$barchart6 <- renderPlot({
    PlotXTabs(data, stroke, c(3), "percent")
  })
  
  output$barchart7 <- renderPlot({
    PlotXTabs(data, stroke, c(4), "percent")
  })
  
  # --------------------
  # density plot section
  # --------------------
  
  # filter the checkgroup input:
  
  dent <-  reactive({
    return(dent = input$continuous)
    
  })
  

  ##########################
  # # render density plot1
  
  # output$densityPlot1 <- renderPlot({
  #   cdplot(factor(stroke) ~ bmi, data=data, main="Estimated categ prob", ylab='Stroke')
  #   qplot(bmi, ..count.., data=data, geom="density", fill=factor(stroke), position="fill") + 
  #     ylab('Probability')+theme(legend.position='bottom')
    
  # })
  
  # # render density plot2
  
  # output$densityPlot2 <- renderPlot({
  #   cdplot(factor(stroke) ~ avg_glucose_level, data=data, main="Estimated categ prob", ylab='Stroke')
  #   qplot(avg_glucose_level, ..count.., data=data, geom="density", fill=factor(stroke), position="fill") + 
  #     ylab('Probability')+theme(legend.position='bottom')
    
  # })   
  
  # # render density plot3
  
  # output$densityPlot3 <- renderPlot({
  #   cdplot(factor(stroke) ~ age, data=data, main="Estimated categ prob", ylab='Stroke')
  #   qplot(age, ..count.., data=data, geom="density", fill=factor(stroke), position="fill") + 
  #     ylab('Probability')+theme(legend.position='bottom')
    
  # })
  ##########################

  
  # --------------------
  # boxplot section
  # --------------------
  
  output$boxplot1 <- renderPlot({ggplot(data, aes(x= hypertension, y=age, fill = hypertension)) + 
    geom_boxplot()
  })
  
  output$boxplot2 <- renderPlot({ggplot(data, aes(x= heart_disease, y=age, fill = heart_disease)) + 
      geom_boxplot()})
  
  output$boxplot3 <- renderPlot({ggplot(data, aes(x= work_type, y=age, fill = work_type)) + 
      geom_boxplot()})
  
  output$boxplot4 <- renderPlot({ggplot(data, aes(x= ever_married, y=age, fill = ever_married)) +
      geom_boxplot()})

  output$boxplot5 <- renderPlot({ggplot(data, aes(x= ever_married, y=avg_glucose_level, fill = ever_married)) +
      geom_boxplot()})

  output$boxplot6 <- renderPlot({ggplot(data, aes(x= work_type, y=avg_glucose_level, fill = work_type)) +
      geom_boxplot()})

  output$boxplot7 <- renderPlot({ggplot(data, aes(x= work_type, y=bmi, fill = work_type)) +
      geom_boxplot()})
  
  output$boxplot8 <- renderPlot({ggplot(data, aes(x= ever_married, y=bmi, fill = ever_married)) +
      geom_boxplot()})
  ###s
  
  bmiAndglucose <- reactive({
    if(input$graph_choice == "AgevsBMI"){
      graph1 <- data%>%
        select(age,bmi,stroke,gender,smoking_status)%>%
        group_by(age)%>%
        filter(stroke == "1")%>%
        filter(gender %in% input$gender_choice)%>%
        filter(smoking_status %in% input$smoking_status_choice)
    }else{
      graph2 <- data%>%
        select(age,avg_glucose_level, stroke,gender,smoking_status)%>%
        group_by(age)%>%
        filter(stroke == "1")%>%
        filter(gender %in% input$gender_choice)%>%
        filter(smoking_status %in% input$smoking_status_choice)  
    }
  })
  
  ###
  
  output$bmiAndglucoseGraph <- renderPlot({
    if(input$graph_choice == "AgevsBMI"){
      ggplot(bmiAndglucose(), aes(x = age, y = bmi))+
        geom_point(size = 2, shape = 21)+
        geom_smooth(method = "loess", se = F)
    }else{
      ggplot(bmiAndglucose(), aes(x = age, y = avg_glucose_level))+
        geom_point(size = 2, shape = 21)+
        geom_smooth(method = "loess", se = F)
    }
  }) 
  
  
  
  
  ######### NUM VAR ###############
  
  output$num_var_plot1 <- renderPlot({
    if(input$num_var == "age"){
      # __Density Plot: Age per Class ####
      data %>% ggplot(aes(age)) +
        geom_density(alpha = 0.2, bw = 1) +
        labs(title = "Age density plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))         #theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var == "bmi"){
      # __Density Plot: BMI per Class ####
      data %>% ggplot(aes(bmi)) +
        geom_density(alpha = 0.2, bw = 1) +
        labs(title = "BMI density plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var == "avg_glucose_level"){
      # __Density Plot: Avg Level per Class ####
      data %>% ggplot(aes(avg_glucose_level)) +
        geom_density(alpha = 0.2, bw = 5) +
        labs(title = "Avg Glucose Level density plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
    }
  })
  
  ###
  output$num_var_plot2 <- renderPlot({
    if(input$num_var == "age"){
      data %>% ggplot(aes(age)) +
        geom_bar() +
        labs(title = "Age bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var == "bmi"){
      data %>% ggplot(aes(bmi)) +
        geom_bar() +
        labs(title = "BMI bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var == "avg_glucose_level"){
      data %>% ggplot(aes(avg_glucose_level)) +
        geom_bar() +
        labs(title = "Avg Glucose Level bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
    }
  })
  
  ###
  # output$num_var_plot3 <- renderPlot({
  #   if(input$num_var == "age"){
  #     # __Box Plot: Age per Class ####
  #     data %>% ggplot(aes(stroke_c, age, color = stroke_c)) +
  #       geom_boxplot() +
  #       geom_jitter(alpha = 0.3, width = 0.15) +
  #       labs(title = "Age box plot") +
  #       theme(plot.title = element_text(size = 12, face = "bold")) +
  #       theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
  #   }else if (input$num_var == "bmi"){
  #     # __Box Plot: BMI per Class ####
  #     data %>% ggplot(aes(stroke_c, bmi, color = stroke_c)) +
  #       geom_boxplot() +
  #       geom_jitter(alpha = 0.3, width = 0.15)  +
  #       labs(title = "BMI box plot") +
  #       theme(plot.title = element_text(size = 12, face = "bold")) +
  #       theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
  #   }else if (input$num_var == "avg_glucose_level"){
  #     # __Box Plot: Avg Level per Class ####
  #     data %>% ggplot(aes(stroke_c, avg_glucose_level, color = stroke_c)) +
  #       geom_boxplot() +
  #       geom_jitter(alpha = 0.3, width = 0.15) +
  #       labs(title = "Avg Glucose Level box plot") +
  #       theme(plot.title = element_text(size = 12, face = "bold")) +
  #       theme(plot.margin = unit(c(1,0,1,0), "cm"))
  #   }
  # })

  ######### NUM VAR 2 ############

   ######### NUM VAR ###############
  
  output$num_var_plot1_3 <- renderPlot({
    if(input$num_var3 == "age"){
      # __Density Plot: Age per Class ####
      data %>% ggplot(aes(age, fill = stroke_c)) +
        geom_density(alpha = 0.2, bw = 1) +
        labs(title = "Age density plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))         #theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "bmi"){
      # __Density Plot: BMI per Class ####
      data %>% ggplot(aes(bmi, fill = stroke_c)) +
        geom_density(alpha = 0.2, bw = 1) +
        labs(title = "BMI density plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "avg_glucose_level"){
      # __Density Plot: Avg Level per Class ####
      data %>% ggplot(aes(avg_glucose_level, fill = stroke_c)) +
        geom_density(alpha = 0.2, bw = 5) +
        labs(title = "Avg Glucose Level density plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
    }
  })
  
  ###
  output$num_var_plot2_3 <- renderPlot({
    if(input$num_var3 == "age"){
      data %>% ggplot(aes(age, fill = stroke_c)) +
        geom_bar() +
        labs(title = "Age bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "bmi"){
      data %>% ggplot(aes(bmi, fill = stroke_c)) +
        geom_bar() +
        labs(title = "BMI bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold"))
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "avg_glucose_level"){
      data %>% ggplot(aes(avg_glucose_level, fill = stroke_c)) +
        geom_bar() +
        labs(title = "Avg Glucose Level bar plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) 
        # theme(plot.margin = unit(c(1,0,1,0), "cm"))
    }
  })
  
  ###
  output$num_var_plot3_3 <- renderPlot({
    if(input$num_var3 == "age"){
      # __Box Plot: Age per Class ####
      data %>% ggplot(aes(stroke_c, age, color = stroke_c)) +
        geom_boxplot() +
        geom_jitter(alpha = 0.3, width = 0.15) +
        labs(title = "Age box plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "bmi"){
      # __Box Plot: BMI per Class ####
      data %>% ggplot(aes(stroke_c, bmi, color = stroke_c)) +
        geom_boxplot() +
        geom_jitter(alpha = 0.3, width = 0.15)  +
        labs(title = "BMI box plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        theme(plot.margin = unit(c(1,0,1,0), "cm"))
      
    }else if (input$num_var3 == "avg_glucose_level"){
      # __Box Plot: Avg Level per Class ####
      data %>% ggplot(aes(stroke_c, avg_glucose_level, color = stroke_c)) +
        geom_boxplot() +
        geom_jitter(alpha = 0.3, width = 0.15) +
        labs(title = "Avg Glucose Level box plot") +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        theme(plot.margin = unit(c(1,0,1,0), "cm"))
    }
  })
  
  ######### CAT VAR ###############
  
  ###
  output$num_var_c_plot1 <- renderPlot({
    
    if(input$num_var_c == "gender"){
      # __Gender distribution #####
      data %>% 
        ggplot(aes(x = gender, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Gender distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "hypertension"){
      # __Hypertension distribution #####
      data %>% 
        ggplot(aes(x = hypertension, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Hypertension distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "heart_disease"){
      # __Heart_disease distribution #####
      data %>% 
        ggplot(aes(x = heart_disease, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Heart disease distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "ever_married"){
      # Ever_married distribution #####
      data %>% 
        ggplot(aes(x = ever_married, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Ever married distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "work_type"){
      # __Work_type distribution #####
      data %>% 
        ggplot(aes(x = work_type, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Work type distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "Residence_type"){
      # __Residence_type distribution #####
      data %>% 
        ggplot(aes(x = Residence_type, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Residence type distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }else if (input$num_var_c == "smoking_status"){
      # __Smoking_status distribution #####
      data %>% 
        ggplot(aes(x = smoking_status, y = ..count.., fill = stroke_c)) +
        geom_bar() +
        scale_fill_manual(values = c("gray50", "orangered2")) +
        labs(title = "Smoking status distribution") +
        theme_bw() +
        theme(legend.position = "bottom")
      
    }
  })
  
  
  ###
  
  # output$num_var_c_plot2 <- renderPlot({
    
  #   pie(table(data[,input$num_var_c]),
  #       labels = substr(names(table(data[,input$num_var_c])),1,20),
  #       main = "Les catégories",
  #       col = c()
  #   )
  # })
  
  ###
  output$num_var_c_text2 <- renderText({
    
    # data %>% 
    # group_by(Variable = data[,input$num_var_c]) %>%
    # summarise(total = n(), percent = round(total/n, 3), 
    #           strokes = sum(stroke_c == "stroke"), 
    #           stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
    # unique() %>%
    # knitr::kable() %>%
    # kable_styling(
    #   font_size = 15,
    #   bootstrap_options = c("striped", "bordered"  )
    #   )
    
    if(input$num_var_c == "gender"){
      # __Summary table by gender #####
      # gender, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(gender) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), 
                  strokes = sum(stroke_c == "stroke"), 
                  stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
    }else if (input$num_var_c == "hypertension"){
      # __Summary table by hypertension #####
      # hypertension, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(hypertension) %>%
        dplyr::summarise( total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"),
                   stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
    }else if (input$num_var_c == "heart_disease"){
      # __Summary table by heart_disease #####
      # heart_disease, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(heart_disease) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"),
                  stroke_percent = round(mean(stroke_c =="stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
    }else if (input$num_var_c == "ever_married"){
      # __Summary table by ever_married #####
      # ever_married, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(ever_married) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"),
                  stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
      
    }else if (input$num_var_c == "work_type"){
      # __Summary table by work_type #####
      # work_type, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(work_type) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"),
                  stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
    }else if (input$num_var_c == "Residence_type"){
      # Summary table by Residence_type #####
      # age, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(Residence_type) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"),
                  stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
      
    }else if (input$num_var_c == "smoking_status"){
      # __Summary table by smoking_status  #####
      # smoking_status, total of observations, number of strokes, percent of strokes
      data %>% 
        group_by(smoking_status) %>%
        dplyr::summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke_c == "stroke"), 
                  stroke_percent = round(mean(stroke_c == "stroke"), 3)) %>% 
        unique() %>%
        knitr::kable() %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "bordered"  )
        )
      
    }
  })
  
  
  
  
}