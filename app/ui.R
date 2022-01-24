library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(dygraphs)
library(ggvis)



shinyUI(dashboardPage(
  dashboardHeader(title = "Prédiction d'AVC",titleWidth = 300),
  dashboardSidebar(width = 360,
                   sidebarMenu(
                     menuItem('Auteurs', tabName = "auteurs", icon = icon("user")),
                     menuItem('Introduction',tabName = "Introduction",icon = icon("network-wired")),
                     menuItem('Dataset',tabName = "Dataset",icon = icon("database")),
                     menuItem('Valeurs manquantes',tabName = "Valeurs_manquantes",icon = icon("border-none")),
                     menuItem("Données déséquilibrées", tabName = "Imbalenced_data", icon = icon("balance-scale-right")),
                     menuItem('Data Visualization univariée',tabName = 'Visualization', icon = icon("line-chart"),
                              # menuSubItem('Variables quantitatives',tabName = "Variables_quantitatives",icon = icon("chart-bar")),
                              menuSubItem('Variables quantitatives',tabName = "NUMERICAL_VARIABLES",icon = icon("chart-bar")),
                              menuSubItem('Variables qualitatives',tabName = "Var_qualit",icon = icon("chart-pie"))
                              ),
                              
                     menuItem('Data Visualization bivariée-(var vs stroke)',tabName = 'Visualizations', icon = icon("line-chart"),
                              menuSubItem('Analyse exploratoire',tabName = "CATEGORICAL_VARIABLES",icon = icon("chart-bar")),
                              # menuSubItem("Analyse exploratoire", tabName = "name1", icon = icon("chart-bar")),
                              # menuSubItem("Densité conditionnelle", tabName = "name2", icon = icon("chart-area")),
                              menuSubItem("Densité conditionnelle", tabName = "name3", icon = icon("chart-area"))
                     ),
                     menuItem('Informations initiales',tabName = "Informations",icon = icon("info-circle")),
                     
                     menuItem('Data Visualization bivariée-2',tabName = 'Visualizations2', icon = icon("line-chart"),
                              menuSubItem('BiVariables quantitatives',tabName = "bivar_quant_quant",icon = icon("chart-line")),
                             menuSubItem('BiVariables qualitatives',tabName = "bivar_qual_qual",icon = icon("chart-bar")),
                             menuSubItem('BiVariables qualitative quantitative',tabName = "bivar_qual_quant",icon = icon("chart-area"))
                             
                             ),
                     menuItem('Insights',tabName = 'Insights', icon = icon("info")),
                     menuItem("Analyse d'AVC par gender et tranches dâge", tabName = "name0", icon = icon("chart-bar")),
                     #menuItem("Probabilité d'AVC" ,tabName = "proba",icon = icon("bar-chart")),
                     menuItem("Entraînement des modéles de prédiction", tabName = 'modele', icon = icon("chart-bar")),
                     menuItem('Conclusion',tabName = "prediction",icon = icon("comments-o"))
                     #menuItem('Conclusion',tabName = "Conclusion",icon = icon("project-diagram"))
                    
                   )
  ),
  dashboardBody(
    #change the font size of the side bar to 20
    tags$head(
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) 
    ),
    
    tabItems(
      tabItem(
        tabName = 'auteurs',
        fluidRow(column(12,h2("Projet UE accompagnement",align = "center",style = "font-family: 'times'; font-si19pt"))),
        fluidRow(column(12,h3("Prédiction d'AVC (stroke en anglais)",align = "center",style = "font-family: 'times'; font-si19pt"))),
        br(),
        h3('Réalisé par:'),
        br(),
        h4("¤ Zakaria AABBOU (AMSD)"),
        h4("--->",em("RStudio"),"déja fait /",em("Shiny"),"jamais fait."),
        h4("¤ Tinhinane BEN SLIMANE (MLSD)"), 
        h4("--->",em("RStudio"),"déja fait/",em("Shiny"),"jamais fait."),
        h4("¤ Nathan JAOUADI (MLSD)"),
        h4("--->",em("RStudio"),"jamais fait/",em("Shiny"),"jamais fait."),
        br(),
        h3('Rendu:'),
        h4("02/11/2021.")

        
      )
      ,
      
      
      tabItem(
        tabName = 'Introduction',
        fluidRow(h2('Introduction')),
        br(),
        fluidRow(column(12,
                        tabPanel("Introduction",
                                 h4(strong("Données de prédiction d'AVC:")),
                                 p("Selon l'OMS, l'AVC (Accident Vasculaire Cérébral) est la deuxiéme cause de décés dans le monde."),
                                 p("Pour prédire si un patient est susceptible de subir un AVC  en fonction de paramétres d'entrée (âge, statut tabagique et différentes maladies), on se donne un jeu de données de 2000 lignes et 12 colonnes telles que:"),
                                 p(strong("1) identifiant"), ": identifiant unique (colonne supprimée aprés vérification de l'unicité des identifiants),"),
                                 p(strong("2) gender"), ": <Male>, <Female> ou <Others>,"),
                                 p(strong("3) age")," : âge du patient,"),
                                 p(strong("4) hypertension")," : 0 si le patient n'a pas d'hypertension, 1 si le patient a une hypertension,"),
                                 p(strong("5) heart_disease")," : 0 si le patient n'a pas de maladie cardiaque, 1 si le patient a une maladie cardiaque,"),
                                 p(strong("6) ever_married"), ": <No> ou <Yes>,"),
                                 p(strong("7) work_type "),": <children>, <Govt_job>, <Never_worked>, <Private> ou <Self_employed>,"),
                                 p(strong("8) Residence_type")," : <Rural> ou <Urban>,"),
                                 p(strong("9) avg_glucose_level")," : taux moyen de glucose dans le sang,"),
                                 p(strong("10) bmi"), ": indice de masse corporelle,"),
                                 p(strong("11) smoking_status"), ": <formerly smoked>, <never smoked>, <smokes> ou <Unknown>,"),
                                 p(strong("12) stroke"),": 1 si le patient a eu un AVC, 0 sinon."),
                                 p("Notre cible c'est bien la colonne",strong("stroke."),
                                   br(),
                                h4(strong("Remarque:")),
                                p("Ajout de 3 colonnes", em("age-interval, age_interval_c"), "et", em("stroke_c"),
                                  "qui vont nous être utile pour la visualisation et l'exploration du jeu de données.")
                                   
                                   )
                        )
        )
        )
      ),
      
      tabItem(
        tabName = 'Dataset',
        fluidRow(h2('Dataset')),
        br(),
        fluidRow(column(12,
                        DT::dataTableOutput('donnees'),
                        #tableOutput(outputId = 'donnees'),
                        verbatimTextOutput(outputId = "summary1")
                        
        )
        )
      ),
      
      tabItem(
        tabName = 'Imbalenced_data', 
        fluidRow(h2('Problème de données déséquilibrées')),
        br(),
        fluidRow((column(12,
                tabPanel(
                  'Données déséquilibrées',
                  p("Notre jeu de données est déséquilibrées comme suit:"),
                  plotOutput(outputId = "stroke_deseq"),
                  #verbatimTextOutput(outputId = "percent_stroke_imbalance")
                  p("no_stroke stroke"),
                  textOutput(outputId = "percent_stroke_imbalance"),
                  p("On remarque bien qu'on a 89 % de", em("no_stroke"),"et que 11% de",em("stroke"),"."),
                  h3('Solution:'),
                  p("Pour remédier à ce problème, on utilise",
                  strong("la méthode de rééchantillonnage "), "dont le principe est de 
                                                                      modifier l'ensemble de données avant d'entraîner notre modéle  de prédiction afin d'avoir 
                                                                      des données plus équilibrées."),
                  p("Cette stratégie admet deux méthodes principales qui sert à égaliser les deux classes",
                    em("Oversamplig"), "et", em("Undersampling.")),
                  p("Vu qu'on n'a pas beaucoup de données, on va privilégier l'",
                    strong("Oversampling"),"qui sert à augmenter le nombre d'observation de la classe minoritaire pour arriver à 
                    un ratio classe minoritaire/ classe majoritaire satisfaisant."),
                  p(" Le package R utilisé c'est bien", em("ROSE.")),
                  plotOutput(outputId = "stroke_eq")
                  
                  
                  
                  
                )         
                         
                         
                         
                         )))
             
              
               ),
      
      
      tabItem(
        tabName = 'Informations',
        fluidRow(h2('Informations initiales')),
        br(),
        fluidRow(column(12,
                        tabPanel("Informations initiales",
                                 p("On dispose des deux types de variables (quantitative et qualitative):"),
                                 p(strong("1) quantitatives discrètes/continues : "),"age, avg_glucose_level, bmi,"),
                                 p(strong("2) quantitative binéaires:"),"hypertension,heart_disease, stroke --> convertis en variables catégorielles pour simplifier l'analyse descriptive de notre jeu de données,"),
                                 p(strong("3) qualitatives nominales:"),"gender, ever_married, work_type, Residence_type, smoking_status."),
                                 p("La plupart de nos variables sont qualitatives nominales(catégorielles)."),
                                 br(),
                                 h4(strong("Les premiéres insights tirées de la visualisation des données vs stroke:")),
                                 p(strong("1)"), "L'AVC dépend fortement de l'âge du patient,"),
                                 p(strong("2)"), "L'AVC apparait chez les patients dont la valeur d'indice de la masse corporelle (bmi) est entre 20 et 35 kg/m²,"),
                                 p(strong("3)"), "Les personnes qui travaillent en indépendance ont plus le risque de faire un AVC comparant aux autres types de travail,"),
                                 p(strong("4)"), "Le risque d'avoir un AVC pour les personnes avec hypertension est significatif, plus de 15% de personnes font un AVC quand elles ont de l'hpertension,"),
                                 p(strong("5)"), "Le risque d'avoir un AVC pour les personnes qui ont des maladies cardiaques est trés élevé, plus de 20% de personnes font un AVC quand elles sont atteintes d'une maladie cardiaque,"),
                                 p(strong("6)"), "Environs 50% de personnes marriées risquent de faire un AVC,"),
                                 p(strong("7)"), "les anciens fumeurs sont plus risqués de faire un AVC."),
                                 p(strong("8)"), "L'AVC ne dépend pas forcément du sexe, du type de résidence et le niveau moyen de glucose dans le corps."),
                                 br(),
                                 p("Pour enrichir plus  notre étude, approfondissons nous dans les relations qui sont entre les différentes variables en relation avec l'AVC.") 
                       
                            
                        
        )
        ))
      ),
      
      # tabItem(
      #  	tabName = 'XX',
      #  	fluidRow(h1('XX XX')),
      #  	br(),
      #  	fluidRow(column(12,
      #  		# XXXXX
      #  		)
      #  	)
      # 	),
      
      
   
      tabItem(
        tabName = "name0",
        fluidRow(
          column(12,
                 h2("Analyse de stroke par sexe et tranches d'âge "),
                 plotOutput(outputId = "BMI_analysis" , width = "100%"),
                 h4(strong('Interprétation:')),
                 p(strong("1)"), "On remarque que la tranche d'âge la plus touchée par l'AVC c'est [50,82[ pour les deux sexes,"),
                 p(strong("2)"), "Les femmes sont les plus touchées par rapport aux hommes surtout pour la tranche d'âge [66,82["),
                 p("A l'aide de cette étude, on a réussi à voir que l'AVC dépend plus au moins du sexe.")

          )
        )
      ),
      
      
      
      tabItem(
        tabName = "name1",
        fluidRow(
          column(12,
                 h2("Analyse exploratoire des conditions"),
                 plotOutput(outputId = "barchart1"),
                 tags$br(),
                 plotOutput(outputId = "barchart2"),
                 tags$br(),
                 plotOutput(outputId = "barchart3"),
                 tags$br(),
                 plotOutput(outputId = "barchart4"),
                 tags$br(),
                 plotOutput(outputId = "barchart5"),
                 tags$br(),
                 plotOutput(outputId = "barchart6"),
                 tags$br(),
                 plotOutput(outputId = "barchart7"),
                 tags$br()
          )
        )
      ),
      
      # tabItem(
      #   tabName = "name2",
      #   fluidRow(
      #     column(12,
      #            h2("Plots de densité conditionnelle"),
      #            plotOutput(outputId = "densityPlot1"),
      #            tags$br(),
      #            p("Le graphe montre que la densité des personnes en surpoids qui ont subi un AVC est plus élevée."),
      #            plotOutput(outputId = "densityPlot2"),
      #            tags$br(),
      #            p("Le graphe montre que la densité des personnes avec un taux de glucose inférieur à 100 a subi davantage l'AVC."),
                 
      #            plotOutput(outputId = "densityPlot3"),
      #            tags$br(),
      #            p("Le graphe montre que la densité des personnes âgées de 50 ans et plus subissant un AVC est plus élevée.")
                 
      #     ))),
      
      tabItem(
        tabName = 'name3',
        fluidRow(h2('Représentation graphique des densités conditionnelles')),
        br(),
        fluidRow(column(12,
                        sidebarPanel(
                          selectInput(
                            inputId = 'num_var3', 
                            label = 'Variables',
                            choices = c('age', 'bmi','avg_glucose_level')
                          ),
                          h4(strong('Interprétation:')),
                          textOutput(outputId = "comment_varqant3")
                        ),
                        mainPanel(
                          plotOutput(outputId = "num_var_plot1_3"),
                          plotOutput(outputId = "num_var_plot2_3"),
                          plotOutput(outputId = "num_var_plot3_3")
                        )
        )
        )
      ),
      
    
      
      tabItem(
        tabName = 'Valeurs_manquantes',
        fluidRow(h2('Valeurs manquantes')),
        br(),
        fluidRow(column(12,
                        tabPanel("Valeurs manquantes",
                                 p("Au début, le jeu de données comporte 201  valeurs manquantes dans la colonne", em("bmi")," remplacées par",em("la médiane"),"de la colonne en question."),
                                 p("Le nombre de valeurs manquantes dans chaque colonnes:"),
                                 #tableOutput(outputId = "nb_NA"),
                                 plotOutput(outputId = "plot_nan"),
                                 #plotOutput(outputId = "missing_values"),
                                 verbatimTextOutput(outputId = "summary_missing_values")
                                 

                                 )
        )
        )
      ),
      
      # tabItem(
      #   tabName = 'Tableau_statistique',
      #   fluidRow(h2('Tableau statistique')),
      #   br(),
      #   fluidRow(column(12,
      #                   tabPanel("Tableau statistique/summary",
      #                            sidebarPanel(
      #                              selectInput(
      #                                inputId = 'column', 
      #                                label = 'Variables',
      #                                choices = colnames(data)
      #                              )
      #                            ),
      #                            #tableOutput(outputId = "contents"),
      #                            verbatimTextOutput(outputId = "summary")
      #                   )
      #   )
      #   )
      # ),
      
      
      # tabItem(
      #   tabName = 'Variables_quantitatives',
      #   fluidRow(h2('Représentation graphique de Variables quantitatives')),
      #   br(),
      #   fluidRow(column(12,
      #                   tabPanel("Variables quantitatives",
      #                            # Zone d'affichage du diagramme en bâtons des effectifs age
      #                            plotOutput(outputId = "effectifsAge"),
      #                            fluidRow(
      #                              column(6,#Zone d'affichage du diagramme en bâtons des effectifs  Niveau_moy_de_glucose 
      #                                     plotOutput(outputId = "effectifsGluc")),
      #                              column(6, #Zone d'affichage du diagramme en bâtons des effectifs  bmi
      #                                     plotOutput(outputId = "effectifsBmi"))
      #                            ),
      #                            h4(strong('Interprétation:')),
      #                            textOutput(outputId = "comment_varqant")
      #                   )
      #   )
      #   )
      # ),
      tabItem(
        tabName = 'NUMERICAL_VARIABLES',
        fluidRow(h2('Représentation graphique de Variables quantitatives')),
        br(),
        fluidRow(column(12,
                        sidebarPanel(
                          selectInput(
                            inputId = 'num_var', 
                            label = 'Veuillez parcourir les variables une par une:',
                            choices = c('age', 'bmi','avg_glucose_level')
                          ),
                          h4(strong('Interprétation:')),
                          textOutput(outputId = "comment_varqant")
                        ),
                        mainPanel(
                          plotOutput(outputId = "num_var_plot1"),
                          plotOutput(outputId = "num_var_plot2")
                          # plotOutput(outputId = "num_var_plot3")
                        )
        )
        )
      ),
      
      ##%##
      tabItem(
        tabName = 'Var_qualit',
        fluidRow(h2('Représentation graphique de variables qualitatives')),
        br(),
        fluidRow(column(12,
                        tabPanel("Repr graph Var_qualit", #barplot var qualitative,
                                 sidebarPanel(
                                   selectInput(
                                     inputId = 'column2', 
                                     label = "Veuillez parcourir les variables une par une pour voir l'efféctif de chaque classe de la variable choisie:",
                                     choices = c(
                                       'gender', 'hypertension', 'heart_disease', 
                                       'ever_married', 'work_type', 'Residence_type', 
                                       'smoking_status', 'stroke'
                                     )
                                   )
                                 ),mainPanel(
                                   plotOutput(outputId = "colonnes"),
                                   #camembert var qualitative
                                   plotOutput(outputId = "secteurs"))
                        )
        )
        )
      ),
      
      tabItem(
        tabName = 'CATEGORICAL_VARIABLES',
        fluidRow(h2('Analyse exploratoire des variables qualitatives')),
        br(),
        fluidRow(column(12,
                        sidebarPanel(
                          selectInput(
                            inputId = 'num_var_c', 
                            label = "Veuillez parcourir les variables une par une pour voir l'efféctif de chaque classe de la variable choisie:",
                            choices = c('gender', 'hypertension','heart_disease' , 'ever_married' , 'work_type' , 'Residence_type' , 'smoking_status')
                          )
                        ),mainPanel(
                          plotOutput(outputId = "num_var_c_plot1"),
                          tags$br(),
                          htmlOutput(outputId = "num_var_c_text2")
                          # tags$br(),
                          # plotOutput(outputId = "num_var_c_plot2")
                        )
        )
        )
      ),
      
      
      tabItem(
        tabName = 'bivar_quant_quant',
###
tabsetPanel(
tabPanel("Analyse bivarariée entre les variables",
        fluidRow(
          column(12,
            h2('Analyse bivarariée entre deux variables quantitatives'),
            br(),
            h5("Du plot de densités conditionnelles de variables quantitatives, on a remarqué que l'AVC depend fortement de l'age.
                Dans cette rebrique, on calcule la corrélation qui est entre les différentes variables quantitatives deux à deux:",
                em("age, avg_glucose_level"),"et",em("bmi.")),
                                    
                     sidebarPanel(
                       selectInput(
                         inputId = 'column3', 
                         label = 'Veuillez choisir deux variables quantitatives différentes:',
                         choices = c('age', 'bmi','avg_glucose_level')
                       ),
                       selectInput(
                         inputId = 'column4', 
                         label = 'Variables',
                         choices = c('age', 'bmi','avg_glucose_level')
                       )
                     )   
                    )
        
        ),
        fluidRow(
          column(6,plotOutput("nuagePoints")),
          column(6, textOutput("correlation")),
          column (12,
                  p("D'aprés la valeur de chaque  coefficient  de corrélation linéaire,
                    on déduit que la corrélation linéaire entre les différentes variables quantitatives est faible,
                    ce qui revient à dire qu'il n y'a pas de relations linéaires entre elles.")
          )
          )
        ),
tabPanel("Age VS BMI & Glucose",
              fluidRow(
                column(12,
    
                       h3("Non linéarité de relation entre ageVSbmi et ageVSavg_glucose_level en intégrant d'autres variables comme sexe et statut fumeur:"),
                       p("Veuillez choisir entre Age VS bmi ou bien avg_glucose_level puis séléctionner une ou plus parmi les catégories de sexe ou/et satatut fumeur et regardons l'allure de la courbe bleu:"),
                       tabPanel(
                         "Age vs. BMI/Glucose",
                         sidebarPanel(
                           selectInput(
                             inputId = 'graph_choice', 
                             label = 'BMI ou Glucose',
                             choices = list("AgevsBMI" = "AgevsBMI", "AgevsGlucose" = "AgevsGlucose")
                           ),
                           
                           checkboxGroupInput(
                             inputId = 'gender_choice',
                             label = 'Sexe',
                             choices = list("Male"="Male", "Female"="Female"),
                             selected = "Male"
                           ),
                           
                           checkboxGroupInput(
                             inputId = 'smoking_status_choice',
                             label = 'Fumeur',
                             choices = list("Formerly"="formerly smoked", "Never"="never smoked", "Smokes"="smokes", "Unknown"="Unknown"),
                             selected = "smokes"
                           )
                         ),
                         
                         mainPanel(
                           plotOutput("bmiAndglucoseGraph")
                         )
                       )
                ))
  )
)
        ####
      ),
      
      tabItem(
        tabName = 'bivar_qual_qual',
        fluidRow(h2('Analyse bivariée entre deux variables qualitatives')),
        br(),
        fluidRow(column(12,
                        tabPanel("BiVariables qualitatives",
                                 h5("D'aprés l'analyse exploratoire des variables qualitatives vs notre variable cible stroke, on a constaté que les variables qualitatives contribuants à l'AVC sont:",
                                    em("hypertension, heart_desease, ever_married et work_type."),"Etudions alors la relations entre ces variables significatives:",
                                    strong("hypertension vs ever_married, heart_disease vs ever_married"),"et", strong("work_type vs ever_married.")),
                                 sidebarPanel(
                                   selectInput(
                                     inputId = 'column5', 
                                     label = 'Variables',
                                     choices = c('hypertension', 'heart_disease','ever_married', 'work_type')
                                   ),
                                   selectInput(
                                     inputId = 'column6', 
                                     label = 'Variables',
                                     choices = c(
                                       'hypertension', 'heart_disease','ever_married', 'work_type')
                                   )
                                 )))),
                            br(),
                                 fluidRow(
                                   column(12,plotOutput("barplotBi")),
                                   column (6,strong("Insights tirées de cette étude:"),
                                           p(strong("1)"),"La plupart des gens mariés ont tendance à avoir de l'hypertension et de maladies cardiaques,"),
                                           p(strong("2)"),"Les personnes mariés travaillent plus en indépendance et en privé."),
                                           p(" On peut supposer alors que cela due au stresse cumulé.")
                                   ))
                        
        )
        
      ,
      tabItem(
        tabName = 'bivar_qual_quant',
        fluidRow(
          column(12,
                 h2("Boites à moustaches"),
                 p("y = age:"),
                 column(6, plotOutput(outputId = "boxplot1"),
                        tags$br() ),
                 column(6,plotOutput(outputId = "boxplot2"),
                        tags$br()))),
        
        
        fluidRow(
          column(12,
                 column(6, plotOutput(outputId = "boxplot3"),
                        tags$br() ),
                 column(6,plotOutput(outputId = "boxplot4"),
                        tags$br()))),
        p("y = avg_glucose_level:"),
        
        fluidRow(
          column(12,
                 column(6, plotOutput(outputId = "boxplot6"),
                        tags$br() ),
                 column(6,plotOutput(outputId = "boxplot5"),
                        tags$br()))),
        
        p("y = bmi:"),
        
        fluidRow(
          column(12,
                 column(6, plotOutput(outputId = "boxplot7"),
                        tags$br() ),
                 column(6,plotOutput(outputId = "boxplot8"),
                        tags$br()))),
        
        
        
        ),
      
      tabItem(tabName = "Insights",
              fluidRow(
                column(12,
                       h2("Insights tirées de l'analyse bivariée qualitative-quantitative")),
                column(12,p(strong("1)"),"L'hypertension touche les personnes dont l'âge appartient à [60,80] ans,"),
                       p(strong("2)"),"Les maladies cardiaques touchent les personnes agées entre 65 et 80 ans,"),
                       p(strong("3)"),"les gens qui travaillent dans le self-employed leur âge est compris entre 57 et 77 ans,"),
                       p(strong("4)"),"les personnes mariées sont entre 50 et 77 ans,"),
                       p(strong("5)"),"La bmi est élevée pour les personnes mariées, self-employed et private (>25 kg/m²)."))
                
              )
        
        
      ),
      
      # tabItem(tabName = "proba",
      #         fluidRow(
      #           column(12,
      #                  h2("La probabilité"),
      #                  tabPanel(
      #                    "Age vs. BMI/Glucose",
      #                    titlePanel("Probabilité d'AVC"),
      #                    sidebarPanel(
      #                      selectInput(
      #                        inputId = 'graph_choice', 
      #                        label = 'BMI ou Glucose',
      #                        choices = list("AgevsBMI" = "AgevsBMI", "AgevsGlucose" = "AgevsGlucose")
      #                      ),
                           
      #                      checkboxGroupInput(
      #                        inputId = 'gender_choice',
      #                        label = 'Sexe',
      #                        choices = list("Male"="Male", "Female"="Female"),
      #                        selected = "Male"
      #                      ),
                           
      #                      checkboxGroupInput(
      #                        inputId = 'smoking_status_choice',
      #                        label = 'Fumeur',
      #                        choices = list("Formerly"="formerly smoked", "Never"="never smoked", "Smokes"="smokes", "Unknown"="Unknown"),
      #                        selected = "smokes"
      #                      )
      #                    ),
                         
      #                    mainPanel(
      #                      plotOutput("bmiAndglucoseGraph")
      #                    )
      #                  )
      #           ))
      # ),
      
     tabItem(tabName = "modele",
             fluidRow(column(12,
                  h2("Logistic Regression (LR) vs Random Forest (RF)"),
                  p("On est amené à prédire si un patient est susciptible d'avoir un AVC ou non, cela revient à dire que la variable cible
                                   est catégorique binaire avec des variables explicatives  contenant à la fois  des variables  quantitatives et qualitatives nominales et binaires.
                                   Comme le nombre de variables n'est pas assez grand, le meilleure modéle à utiliser c'est",strong("la régression logistique."),
                    "Par contre, on a plus de variables qualitatives que quantitatives, pour cela",strong("Forêts aléatoires"), "est un modèle aussi qui peut être performant pour notre jeu de données."),
                 p("D'aprés les différentes analyses et explorations de données, on déduit que les variables expliquant le mieux 
                 notre variable cible", strong("stroke"),"sont:",em("age, hypertension, heart_desease, avg_glucose_level"),"et",em("bmi,"),"dont on va 
                s'en servir dans l'entraînement de nos deux modéles de classification",strong("régression liniaire"), "et",strong("forêts aléatoires"),"pour en choisir le plus performant.")),
                 column(6,
                        h4(strong("Logistic Regression:")),
                        #train
                        h5(strong("Training")),
                        h6(strong("Sommaire GLM:")),
                        #verbatimTextOutput("trainModel")
                        verbatimTextOutput(outputId = "sum_train_glm"),
                       h6(strong("Matrice de confusion d'entraînement GLM:")),
                       plotOutput("MatConfGlm1"),
                       verbatimTextOutput(outputId = "Accuracy_glm_train"),
                       verbatimTextOutput(outputId = "F1score_glm_train"),
                       #test
                       h5(strong("Test")),
                       h6(strong("Matrice de confusion de test GLM:")),
                       plotOutput("MatConfGlm2"),
                       verbatimTextOutput(outputId = "Accuracy_glm_test"),
                       verbatimTextOutput(outputId = "F1score_glm_test"),
                       h6(strong("Visualisation de la courbe ROC")),
                       plotOutput("ROC1")
                 ),
                 column(6,
                        h4(strong("Random Forest:")),
                        #train
                        h5(strong("Training")),
                        h6(strong("Sommaire RF:")),
                        verbatimTextOutput(outputId = "sum_train_rf"),
                        h6(strong("Matrice de confusion d'entraînement RF:")),
                        plotOutput("MatConfRF1"),
                        verbatimTextOutput(outputId = "Accuracy_rf_train"),
                        verbatimTextOutput(outputId = "F1score_rf_train"),
                        #test
                        h5(strong("Test")),
                        h6(strong("Matrice de confusion de test RF:")),
                        plotOutput("MatConfRF2"),
                        verbatimTextOutput(outputId = "Accuracy_rf_test"),
                        verbatimTextOutput(outputId = "F1score_rf_test"),
                        h6(strong("Visualisation de la courbe ROC")),
                        plotOutput("ROC2")
                        
                 )

             )),







      tabItem(tabName = "prediction",
              fluidRow(column(12,h2("Prédiction d'AVC"),h4(strong("choix du modéle:")),p("D'aprés l'étude précédente, on a:"),
                                                                                        p(strong("1)"),"l'accuracy du modéle random forest est supérieure à l'accuracy de la régression logistique,"),
                                                                                        p( strong("2)"), "Le score F1 de random forest est aussi grand que le score de la regression logistique,"),
                                                                                         p(strong("3)"),"l'AUC (l'aire sous la courbe ROC de la régression logistique est plus proche de 1 que celle de random forest à 0.03 prés."),
                                                                                         p("Le modéle donnant un classifieur performant pour classer au mieux nos classes c'est",strong("Random Forest.")))),
              fluidRow(column(12,
                             
                              sidebarLayout(
                                sidebarPanel(

                                  # selectInput(
                                  #    inputId = 'gender_choice',
                                  #    label = 'Sexe',
                                  #    choices = list("Male"="Male", "Female"="Female"),
                                  #    selected = "Male"
                                  #  ),
                                  # selectInput(
                                  #    inputId = 'smoking_status_choice',
                                  #    label = 'Fumeur',
                                  #    choices = list("Formerly"="formerly smoked", "Never"="never smoked", "Smokes"="smokes", "Unknown"="Unknown"),
                                  #    selected = "smokes"
                                  #  ),
                                  # selectInput(
                                  #    inputId = 'ever_married_choice',
                                  #    label = 'ever_married',
                                  #    choices = list("Oui"="Yes", "Non"="No"),
                                  #    selected = "No"
                                  #  ),
                                  # selectInput(
                                  #    inputId = 'work_type_choice',
                                  #    label = 'work_type',
                                  #    choices = list("children"="children","Self_employed"="Self_employed", "Private"="Private" , "Never_worked"="Never_worked" , "Govt_job" = "Govt_job"),
                                  #    selected = "children"
                                  #  ),
                                  # selectInput(
                                  #    inputId = 'Residence_type_choice',
                                  #    label = 'Residence_type',
                                  #    choices = list("Rural"="Rural","Urban"="Urban"),
                                  #    selected = "Rural"
                                  #  ),

                                  sliderInput("age",
                                              "Age en années:",
                                              min = 10,
                                              max = 100,
                                              value = 78),
                                  
                                  radioButtons("Hypertension",
                                               label = strong("Vous souffrez d'hypertension ?"),
                                               choices = list("Oui" = 1, "Non" = 0),
                                               selected = 0,
                                               inline = TRUE),
                                  
                                  radioButtons("HeartDisease",
                                               label = strong("Vous souffrez d'une maladie cardiaque ?"),
                                               choices = list("Oui" = 1, "Non" = 0),
                                               selected = 0,
                                               inline = TRUE),   
                                  
                                  numericInput("avgGlucLvl", "Niveau de glucose moyen:", 130,
                                               min = 80, max = 500, width = '400px'),
                                  
                                  numericInput("bmi", "Indice de masse corporelle:", 32,
                                               min = 10, max = 50, width = '400px')
                                  
                                  #numericInput(inputId, label, value, min = NA, max = NA, step = NA,
                                  #width = NULL)
                                  
                                  
                                ), # end side bar layout
                                
                                # Main Panel --- start
                                mainPanel(
                                  # tab1 --- start
                                  tabsetPanel(
                                    tabPanel("Prédiction", 
                                             strong("Résultat prédit: "),
                                             verbatimTextOutput("predictText")
                                    )
                                    
                                    
                                  ) # tab1 --- end
                                  
                                ) # tabset panel --- end
                                # main panel page ---end 
                              ), # end side bar
                              tags$hr(),
                              tags$hr()
              )
              )
      ),
      

      tabItem(tabName = "Conclusion",
              fluidRow(column(12,h2("Conclusion",align = "center",style = "font-family: 'times'; font-si19pt"),
                              p("Bla blabla bbbla bla bla.")
              )))

      
    )
  )
))

