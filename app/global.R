library(readr)
library(ggplot2)
library(ROSE)
library(dplyr)
library(knitr)
library(kableExtra)
library(randomForest)
library(pROC)

#read the data
data <- read.csv("healthcare-dataset-stroke-data.csv")
# data <- read.csv("https://raw.github.com/felmaggilab/edx_data_science_capstone_strokes/master/data/healthcare-dataset-stroke-data.csv")

head(data)
data = data[,2:12]


#Clean the data
data[data == "N/A"] <- NA
data <- na.omit(data)

#data$gender <- as.factor(data$gender)
data$hypertension <- as.factor(data$hypertension)
data$heart_disease <- as.factor(data$heart_disease)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
#data$stroke <- as.factor(data$stroke)
data$bmi <- as.numeric(data$bmi)

data$age_interval <- cut_interval(data$age, n=5 , labels = c("[ 0 , 17 ]" , "] 17 , 33 ]" , "] 33 , 50 ]" , "] 50 , 66 ]" ,"] 66 , 82 ]"))
data$age_interval_c <- cut_interval(data$age, n=5 ,breaks = c(0, 14, 24, 64, Inf),labels = c("Enfants", "Jeunes", "Adultes", "Vieux"))
data$stroke_c <- factor(data$stroke, levels = c(0,1), labels = c("no_stroke", "stroke"))

data <- data[which(data$gender!= "Other"), ]

#sauvegarder les données non équilibrées de notre variable cible
stroke_imbalance = data$stroke_c

# _______________________########
# BALANCE DATA           ########
# _______________________########

# _______________________########
# Over Sampling          ########
# _______________________########=*


table(data$stroke_c)
prop.table(table(data$stroke_c))

n_over = sum(data == "no_stroke")

set.seed(1969, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1969)`
# Every time we run the code, we get a different ovun.sample

stroke_data_over <- ovun.sample(stroke_c ~ ., data = data, method = "over", N = n_over*2)$data
table(stroke_data_over$stroke_c)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke"
stroke_data_over$stroke_c <- relevel(stroke_data_over$stroke_c, ref = "stroke")

table(stroke_data_over$stroke_c) %>% 
  kable()

prop.table(table(stroke_data_over$stroke_c))  %>% 
  kable()

data <- stroke_data_over
n <- nrow(data)

#-----------
#tarin and test data
#------------
library(caret)
data1 <- data[,1:11]
data1$hypertension <- as.factor(data1$hypertension)
data1$heart_disease <- as.factor(data1$heart_disease)
data1$stroke <- as.factor(data1$stroke)
indice<- createDataPartition(data1$stroke, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data1[indice ,]
test_data  <- data1[-indice,]

