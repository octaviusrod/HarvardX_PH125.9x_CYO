###########################################################
###########################################################
##                                                       ##   
## title: "PERCEPTIONS OF SECURITY IN MEXICO"            ##
## subtitle: HarvardX PH125.9x Capstone                  ## 
## author: "Octavio Rodríguez Ferreira"                  ##
## date: JUNE, 2021                                      ##
##                                                       ##
###########################################################
###########################################################

##########################################################
#                                                        #
#             LOADING DATA AND PACKAGES                  #
#                                                        #
##########################################################

#########################################################
##############   PACKAGES AND LIBRARIES    ##############
#########################################################

#Downloading packages if required
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")


#Loading package libraries
library(readxl)
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(devtools)
library(gridExtra)
library(ggpubr)

#########################################################
###############      LOADING DATA        ################
#########################################################

#Load data
envipe <- read_csv("~/InSync/Up.Edu.Mx/02-ORODs/01-ACADEMIC/03-LEARNING/2020_EdX_R/9.CAPSTONE/CYO_Project/data/conjunto_de_datos_envipe2020_csv/conjunto_de_datos_TPer_Vic1_ENVIPE_2020/conjunto_de_datos/conjunto_de_datos_TPer_Vic1_ENVIPE_2020.csv")

## Data on perception of security from Mexico's 2020 National Survey of Crime, Victimization and Public Security Perceptions (Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública, ENVIPE), from the National Institute of Geography and Statistics (Instituto Nacional de Geografía y Estadística, INEGI) available at <https://www.inegi.org.mx/contenidos/programas/envipe/2020/datosabiertos/conjunto_de_datos_envipe2020_csv.zip>

##Downloading data was done with the `read_csv` function because it couldn't be easily downloaded from the website. I downloaded the data manually, looked for the relevant file (there are a lot of different files in the downloaded file), and use the function to download it to RStudio.

##The path to this data set once the full file is downloaded should be: conjunto_de_datos_envipe2020_csv/conjunto_de_datos_TPer_Vic1_ENVIPE_2020/conjunto_de_datos/conjunto_de_datos_TPer_Vic1_ENVIPE_2020.csv

##########################################################
#                                                        #
#                 BACKGROUND ANALYSIS                    #
#                                                        #
##########################################################

#This section is only for the introduction section of the final report.
#Data used for graphs come from the same survey, but from a different data base.
#We decided to input the values directly into the plot without loading an entire other data set to avoid confusion.
#The numbers for the following plots were obtained in the Executive Summary of the survey, available here:  <<https://www.inegi.org.mx/contenidos/programas/envipe/2019/doc/envipe2019_presentacion_nacional.pdf>>

#Values
Years <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
Rates <- c(27337, 28224, 28200, 28202, 28788, 29746, 28269, 24849)
Percentages <- c(63, 64.2, 64.1, 65.1, 66.3, 70, 70.5, 70.3)

#Crime victimization rate plot
rteplot <- ggplot() +
  geom_bar(aes(x=Years, y=Rates),stat="identity", fill="gray", color="black") +
  ggtitle("Crime victimization rate")

#Percentage of perceptions of insecurity
perplot <- ggplot() +
  geom_line(aes(x = Years, y= Percentages), color = "red") +
  scale_y_continuous(limits = c(60,75)) +
  ggtitle("Perceptions of insecurity(%)")

#Grid 
grid.arrange(rteplot, perplot, 
             ncol = 2,
             top = "Crime victimization v. Perceptions of insecurity")

##########################################################
#                                                        #
#                      PREPROCESSING                     #
#                                                        #
##########################################################

#########################################################
###############    INITIAL EXPLORATION    ###############
#########################################################

#View data
View(envipe)

#Check top observations
head(envipe)

#Check variables class
knitr::kable(sapply(lapply(envipe, class), "[", 1))

#Check variables names
colnames(envipe)

#Check values of dependent variable
unique(envipe$AP4_3_2)

##Initial issues:
##Column names are illegible, and there are blank spaces in values.
##All classes are `character`
##There are way to many columns
##There is a value of "No Response" coded with "9" in our dependent variable

#########################################################
###############      WRANGLING DATA       ###############
#########################################################

###############    REMOVE BLANK SPACES    ###############

#Remove spaces
envipe_clean <- as.data.frame(apply(envipe,2,function(x)gsub('\\s+', '',x)))

#Check top observations
head(envipe_clean)

###########  SUBSET OF VARIABLES OF INTEREST  ###########

#Check variables (column names)
colnames(envipe_clean)

#Select only the variables we're interested in. Also, change the order of some columns for better understanding.
my_vars <- c("ID_PER",
             "SEXO",
             "EDAD",
             "ESTRATO",
             "CVE_ENT",
             "CVE_MUN",
             "DOMINIO",
             "AP4_3_2")

##Variables of interest are: id, sex, age, socioeconomic strata, state code, municipality code, type of area of residence (rural vs. urban), and responses about perception of security in their municipality (pos).

#Create a new data frame with variables of interest
pos <- envipe_clean[,my_vars]

#Remove unnecessary objects
rm(my_vars)

#Check top observations
head(pos)

#Check dimensions of data
dim(pos)

##[1] 90571     8

###############      CHANGE CLASSES       ###############

#Check variables class
knitr::kable(sapply(lapply(pos, class), "[", 1))

##Leave `CVE_ENT` and `CVE_MUN` (state and municipality code) as character to keep leading zeroes.

#Change to numeric
pos[,2:4] <- apply(pos[,2:4], 
                   2, function(x) as.numeric(as.character(x)))

#Re code the area variable `DOMINIO` to numeric [Urban(U) = 1, Suburban (C) =  2, Rural (R) = 3]
pos$DOMINIO <- as.factor(gsub("U", 1, gsub("C", 2, gsub("R", 3, pos$DOMINIO))))

#Relocate next to other predictors
pos <- pos %>% relocate(DOMINIO, .after = ESTRATO)

#Define dependent variable `AP4_3_2` that measures for perception of security in municipality (pos) as factor
pos$AP4_3_2 <- as.factor(pos$AP4_3_2)

#Check top observations
head(pos)

#Check new classes
knitr::kable(sapply(lapply(pos, class), "[", 1))

#############   CREATE LOCATION VARIABLE   ##############

#Creating a unique number for the municipality of respondent
#Convert `CVE_ENT` (State) and `CVE_MUN` (Municipality) into `mun`, a new variable for specific location that includes both the state and municipality codes.
pos$mun <- paste(pos$CVE_ENT, pos$CVE_MUN)

#Remove spaces
pos$mun <- gsub('\\s+', '', pos$mun)

#Relocate new `mun` column
pos <- pos %>% relocate(mun, .after = CVE_MUN)

#Change to numeric
pos$mun <- as.numeric(pos$mun)

#Remove unnecessary columns
pos <- pos[,-c(6:7)]

#Check top observations
head(pos)

#Check new classes
knitr::kable(sapply(lapply(pos, class), "[", 1))

###############     RECODE  VARIABLES     ###############

#New column names
nu_colnames <- c("id",
                 "sex",
                 "age",
                 "ses",
                 "area",
                 "mun",
                 "pos")

#Rename columns
colnames(pos) <- nu_colnames

#Remove unnecessary object
rm(nu_colnames)

#Check top observations
head(pos)

############   REMOVE 'NO RESPONSE' VALUES   ############

#Check levels of dependent variable
unique(pos$pos)

#Subset and drop levels (Drop "9" that codes 'No Response')
pos <- subset(pos, pos != "9")
pos$pos<- droplevels(pos)$pos

#Check levels
unique(pos$pos)

############    ADD NEW VARIABLE FOR AGE GROUP   ############

#Explore age variable
which.min(pos$age)
##[1] 11

which.max(pos$age)
#[1] 80

#Create new variable of 'age groups' as categorical variable
pos <- pos %>% 
  mutate(age_grp = 
           cut(age, c(10,19,39,59,79,100),
               labels = c("-20", "20–39", "40–59", "60-79", "80+")))

#Check top observations
head(pos)

#Check levels of new variable
unique(pos$age_grp)

#Relocate
pos <- pos %>% relocate(age_grp, .after = age)

#Check classes
knitr::kable(sapply(lapply(pos, class), "[", 1))

##########################################################
#                                                        #
#                DESCRIPTIVE ANALYSIS                    #
#                                                        #
##########################################################

##################       DIMENSIONS      #################

dim(pos)

############       VARIABLE DESCRIPTION       ############

#Column names
colnames(pos)

#Descriptions of variables
var_desc <- c("Id of respondent",
              "Sex of respondent",
              "Age of respondent",
              "Age group of respondent",
              "Socio-economic strata",
              "Type of area of residence",
              "Unique code for municipality of residence",
              "Perception of security in the municipality")

#Values of variables
var_vals <- c("Unique number",
              "Male = 1, Female =2",
              "11 to 80",
              "-20, 20–39, 40–59, 60-79, 80+",
              "Low = 1, Medium-low = 2, Medium-high = 3, High = 4",
              "Urban = 1, Suburban =  2, Rural = 3",
              "Code composed of State and Municipality official numbers", 
              "1 = Secure, 2 = Insecure")

#Classes of variables
envipe_vars <- (sapply(lapply(pos, class), "[", 1))

#Table of variables
vars_explained <- data.frame(envipe_vars, var_desc, var_vals)

#Column names for table
colnames(vars_explained) <- c("Class", "Variable description", "Values")

#Remove unnecessary objects
rm(var_desc, var_vals, envipe_vars)

#Table of variables
knitr::kable(vars_explained)

##########  MAP OF RESPONDENTS PER MUNICIPALITY ##########

#Packages and data
## Download a map of Mexican municipalities
devtools::install_github("diegovalle/mxmaps", force = TRUE)
library("mxmaps")

#Load data for a municipal level map of Mexico
data("df_mxmunicipio")

#Check classes
knitr::kable(sapply(lapply(df_mxmunicipio, class), "[", 1))

#Check top values
head(df_mxmunicipio)

#Compare merging values.
## To create a graduated map of responses we have to combine the our data with the map, using a join variable that should be the same in both. In this case, region in the map and mun in our data refer to the same thing, a unique code of each Mexican municipality.
class(df_mxmunicipio$region)
class(pos$mun)

head(df_mxmunicipio$region)
head(pos$mun)

#Modify column from map to match values from data set
mapmx <- df_mxmunicipio %>%
  mutate(mun = as.numeric(region)) %>%
  relocate(mun, .after = state_name)

#Merge our data set into map data 
mapmx <- left_join(mapmx, pos, by = "mun")

#Check classes
knitr::kable(sapply(lapply(mapmx, class), "[", 1))

#Remove unnecessary columns
mapmx <- mapmx[,-c(1:4, 6:18)]

#Rename columns 
## The function "mxmunicipio_choropleth" from <https://www.diegovalle.net/mxmaps/index.html> included in the map package allows you to create GIS maps.
##"The data.frame that you provide to the plotting functions must have one column named 'region' with the INEGI codes of the states or municipios and another one named 'value' with the values to plot."
##The `region` variable of the function is exactly the same as our `mun` variable. 
colnames(mapmx)[colnames(mapmx) == "mun"] <- "region"

# Create the map for plotting
mapmx_df <- mapmx %>%
  select(region)  %>%
  group_by(region) %>%
  summarise(value = n())

#Generate map
pos_map <- mxmunicipio_choropleth(mapmx_df, 
                       num_colors = 9,
                       title = "Geolocation of respondents",
                       legend = "Observations")

#Remove unnecessary objects
rm(df_mxmunicipio, mapmx, mapmx_df)

#Plot map
pos_map

##############         DISTRIBUTIONS        ##############

#Distribution of opinions on perception of security
h1 <- ggplot(pos, aes(pos)) +
  geom_bar(width = 1,
           fill = "#E55451",
           color = "#800517") +
  scale_x_discrete(labels = c("1" = "Secure", "2" = "Insecure")) +
  xlab("POS") +
  ylab("Observations")

#Plot
h1

#Distribution of sex of respondents
h2 <- ggplot(pos, aes(as.factor(sex))) +
  geom_bar(width = 1,
           fill = "#8467D7",
           color = "#461B7E") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  xlab("Sex") +
  ylab("Observations")

#Plot
h2

#Distribution of age of respondents
h3 <- ggplot(pos, aes(age)) +
  geom_histogram(bins = 5,
                 fill = "#82CAFF",
                 color = "#0C090A") +
  xlab("Age") +
  ylab("Observations")

#Plot
h3

#Distribution of socio-economic strata
h4<- ggplot(pos, aes(as.factor(ses))) +
  geom_bar(width = 1,
           fill = "#728C00",
           color = "#0C090A") +
  scale_x_discrete(labels = c("1" = "Low", "2" = "Medium-low", "3" = "Medium-high", "4" = "High")) +
  xlab("SES") +
  ylab("Observations")

#Plot
h4

#Distribution of area
h5 <- ggplot(pos, aes(area)) + 
  geom_bar(width = 1,
           fill = "#E69F00",
           color = "#0C090A") +
  scale_x_discrete(labels = c("1" = "Urban", "2" = "Suburban", "3" = "Rural")) +
  xlab("Area") +
  ylab("Observations")

#Plot
h5

#Grid of distribution plots
grid.arrange(heights = 2,
             h1 + theme(axis.text.x = element_text(angle = 90, hjust = 1)),
             h2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)),
             h3 + theme(axis.text.x = element_text(angle = 90, hjust = 1)),
             h4 + theme(axis.text.x = element_text(angle = 90, hjust = 1)),
             h5 + theme(axis.text.x = element_text(angle = 90, hjust = 1)))

############     PERCEPTIONS PER VARAIABLE    ############

#Age, sex, pos, area
f1 <- ggplot(pos, aes(x = as.factor(sex), y = age, fill = as.factor(pos))) + 
  geom_boxplot() +
  facet_wrap(~ area, labeller = as_labeller(c("1" = "Urban", "2" = "Suburban", "3" = "Rural"))) +
  xlab("Sex") +
  ylab("Age") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_discrete(name="POS", labels= c("1" = "Secure", "2" = "Insecure")) +
  ggtitle("Area") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

#Plot
f1

#Age, sex, pos, ses
f2 <- ggplot(pos, aes(x = as.factor(sex), y = age, fill = as.factor(pos))) + 
  geom_boxplot() +
  facet_wrap(~ ses, ncol = 4, labeller = as_labeller(c("1" = "Low", "2" = "Medium-low", "3" = "Medium-high", "4" = "High"))) +
  xlab("Sex") +
  ylab("Age") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_discrete(name="Perception of security", labels= c("1" = "Secure", "2" = "Insecure")) +
  ggtitle("SES") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

#Plot
f2

##########################################################
#                                                        #
#                     DATA PARTITIONS                    #
#                                                        #
##########################################################

#########################################################
#########    TRAINING AND VALIDATION SETS    ############
#########################################################

# Creating test and validation sets for  data
set.seed(1982, sample.kind="Rounding") 

#Create index and train and test sets
test_index <- createDataPartition(y = pos$pos, times = 1, p = 0.2, list = FALSE)
training <- pos %>% slice(-test_index)
validation <- pos %>% slice(test_index)

#Remove all temporary elements
rm(test_index)

#########################################################
#########       TRAIN AND TEST SUBSETS       ############
#########################################################

#Create a smaller subset to initial model testing.

# Creating test and validation sets for data
set.seed(1982, sample.kind="Rounding") 

#Creaete index and train and test sets
index <- createDataPartition(y = training$pos, times = 1, p = 0.5, list = FALSE)
train_set <- training %>% slice(-index)
test_set <- training %>% slice(index)

#Remove all temporary elements
rm(index)

##########################################################
#                                                        #
#                         MODELING                       #
#                                                        #
##########################################################

#########################################################
#############         BASELINE MODEL        #############
#########################################################

############        INITIAL GLM MODEL      ##############

#Initial model tries to test if sex, age and municipality of residents are strong enough values to predict perceptions of security.
#The selected baseline method is Logistic Regression. 

#Train model
initial_bl <- train(pos ~ 
                     sex +
                     age +
                     mun,
                   method = "glm",
                   data = train_set)

#Predict
ibl_pred <- predict(initial_bl, test_set)

#Extract accuracy
ibl_acc <- confusionMatrix(data = ibl_pred, reference = test_set$pos)$overall["Accuracy"]

#Initial Baseline accuracy
ibl_acc

## Accuracy 
##0.6261915 

############         VARIABLE TESTING      ##############

#Build accuracy table by variable
vartest_acc <- tibble(Predictors="Sex+Age+Municipality",
                      Accuracy = ibl_acc)

#Results
vartest_acc %>% knitr::kable()

##|Predictors               |  Accuracy|
##|:------------------------|---------:|
##|Sex+Age+Municipality     | 0.6261915|

#Test socioeconomic strata (Test 1)

#Train model
vartest_1 <- train(pos ~ 
                       sex +
                       age +
                       mun +
                       ses,
                     method = "glm",
                     data = train_set)

#Predict
vartest_1_pred <- predict(vartest_1, test_set)

#Extract accuracy
vartest_1_acc <- confusionMatrix(data = vartest_1_pred, reference = test_set$pos)$overall["Accuracy"]

#Accuracy table by variable
vartest_acc <- bind_rows(vartest_acc, 
                          tibble(Predictors="Sex+Age+Municipality+SES",
                                 Accuracy = vartest_1_acc))

#Results
vartest_acc %>% knitr::kable()  

##|Predictors               |  Accuracy|
##|:------------------------|---------:|
##|Sex+Age+Municipality     | 0.6261915|
##|Sex+Age+Municipality+SES | 0.6261915|

#Test area (Test 2)

#Train model
vartest_2 <- train(pos ~ 
                     sex +
                     age +
                     mun +
                     area,
                   method = "glm",
                   data = train_set)

#Predict
vartest_2_pred <- predict(vartest_2, test_set)

#Extract accuracy
vartest_2_acc <- confusionMatrix(data = vartest_2_pred, reference = test_set$pos)$overall["Accuracy"]

#Accuracy table by variable
vartest_acc <- bind_rows(vartest_acc, 
                         tibble(Predictors="Sex+Age+Municipality+Area",
                                Accuracy = vartest_2_acc))

#Results
vartest_acc %>% knitr::kable()

##|Predictors                |  Accuracy|
##|:-------------------------|---------:|
##|Sex+Age+Municipality      | 0.6261915|
##|Sex+Age+Municipality+SES  | 0.6261915|
##|Sex+Age+Municipality+Area | 0.6261915|


#Test area + socioeconomic strata (Test 3)

#Train model
vartest_3 <- train(pos ~ 
                     sex +
                     age +
                     mun +
                     ses +
                     area,
                   method = "glm",
                   data = train_set)

#Predict
vartest_3_pred <- predict(vartest_3, test_set)

#Extract accuracy
vartest_3_acc <- confusionMatrix(data = vartest_3_pred, reference = test_set$pos)$overall["Accuracy"]

#Accuracy table by variable
vartest_acc <- bind_rows(vartest_acc, 
                         tibble(Predictors="Sex+Age+Municipality+SES+Area",
                                Accuracy = vartest_3_acc))

#Results
vartest_acc %>% knitr::kable()

##|Predictors                    |  Accuracy|
##|:-----------------------------|---------:|
##|Sex+Age+Municipality          | 0.6261915|
##|Sex+Age+Municipality+SES      | 0.6261915|
##|Sex+Age+Municipality+Area     | 0.6261915|
##|Sex+Age+Municipality+SES+Area | 0.6279859|


############      FINAL BASELINE MODEL     ##############

#Train model
fit_bl <- train(pos ~ 
                  sex +
                  age +
                  mun +
                  ses +
                  area,
                method = "glm",
                data = train_set)

#Predict
pred_bl <- predict(fit_bl, test_set)

#Confusion matrix
cm_bl <- confusionMatrix(data = pred_bl, reference = test_set$pos)

#Extract accuracy
acc_bl <- confusionMatrix(data = pred_bl, reference = test_set$pos)$overall["Accuracy"]

#Modeling accuracy table
at_modeling <- tibble(Method="Baseline Logistic Regression",
                      Accuracy = acc_bl)

#Results
at_modeling %>% knitr::kable()

##|Method                       |  Accuracy|
##|:----------------------------|---------:|
##|Baseline Logistic Regression | 0.6279859|

#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_bl$overall)))

#Plot of confusion matrix table
cm_bl_tab <- as.data.frame(cm_bl$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 8) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                                barheight = 10))
#Plot
cm_bl_tab

#########################################################
########   ADJUSTED LOGISTIC REGRESSION MODEL    ########
#########################################################

#Test age group instead of age

#Train model
fit_abl <- train(pos ~ 
                   sex +
                   age_grp +
                   mun +
                   ses +
                   area,
                 method = "glm",
                 data = train_set)

#Predict
pred_abl <- predict(fit_abl, test_set)

#Confusion matrix
cm_abl <- confusionMatrix(data = pred_abl, reference = test_set$pos)

#Extract accuracy
acc_abl <- confusionMatrix(data = pred_abl, reference = test_set$pos)$overall["Accuracy"]

#Accuracy modeling table
at_modeling <- bind_rows(at_modeling, 
                         tibble(Method="Adjusted Logistic Regression",
                                Accuracy = acc_abl))

#Results table
at_modeling %>% knitr::kable()

## Accuracy improved a little.

##|Method                       |  Accuracy|
##|:----------------------------|---------:|
##|Baseline Logistic Regression | 0.6279859|
##|Adjusted Logistic Regression | 0.6285186|

#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_abl$overall)))

#Plot of confusion matrix table
cm_abl_tab <- as.data.frame(cm_abl$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 10) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                               barheight = 10))
#Plot
cm_abl_tab

#########################################################
#############       NAIVE BAYES MODEL       #############
#########################################################

#Train model
fit_nb <- train(pos ~ 
                  sex +
                  age +
                  mun +
                  ses +
                  area,
                method = "nb",
                data = train_set)

#Predict
pred_nb <- predict(fit_nb, test_set)

#Confusion matrix
cm_nb <- confusionMatrix(data = pred_nb, reference = test_set$pos)


#Extract accuracy
acc_nb <- confusionMatrix(data = pred_nb, reference = test_set$pos)$overall["Accuracy"]

#Accuracy modeling table
at_modeling <- bind_rows(at_modeling, 
                         tibble(Method="Naive Bayes",
                                Accuracy = acc_nb))

#Results table
at_modeling %>% knitr::kable()

##Our accuracy kept increasing, but it's still very low (nearly 63%).

##|Method                       |  Accuracy|
##|:----------------------------|---------:|
##|Baseline Logistic Regression | 0.6279859|
##|Adjusted Logistic Regression | 0.6285186|
##|Naive Bayes                  | 0.6290232|

#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_nb$overall)))

#Plot of confusion matrix table
cm_nb_tab <- as.data.frame(cm_nb$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 10) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                               barheight = 10))
#Plot
cm_nb_tab

#########################################################
#############      RANDOM FORESTS  MODEL    #############
#########################################################

#Train model
fit_rf <- train(pos ~ 
                   sex +
                   age +
                   mun +
                   ses +
                   area,
                 method = "rf",
                 data = train_set)

#Predict
pred_rf <- predict(fit_rf, test_set)

#Confusion matrix
cm_rf <- confusionMatrix(data = pred_rf, reference = test_set$pos)

#Extract accuracy
acc_rf <- confusionMatrix(data = pred_rf, reference = test_set$pos)$overall["Accuracy"]

#Accuracy table
at_modeling <- bind_rows(at_modeling, 
                         tibble(Method="Random Forests",
                                Accuracy = acc_rf))
#Results table
at_modeling %>% knitr::kable()

## This model increased the accuracy around 2%.

#|Method                       |  Accuracy|
#|:----------------------------|---------:|
#|Baseline Logistic Regression | 0.6279859|
#|Adjusted Logistic Regression | 0.6285186|
#|Naive Bayes                  | 0.6290232|
#|Random Forests               | 0.6519009|

#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_rf$overall)))

#Plot of confusion matrix table
cm_rf_tab <- as.data.frame(cm_rf$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 10) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                               barheight = 10))
#Plot
cm_rf_tab

#########################################################
###########         ADJUSTED RF  MODEL        ###########
#########################################################

#Age group instead of age

#Train model
fit_arf <- train(pos ~ 
                  sex +
                  age_grp +
                  mun +
                  ses +
                  area,
                method = "rf",
                data = train_set)

#Predict
pred_arf <- predict(fit_arf, test_set)

#Confussion matrix
cm_arf <- confusionMatrix(data = pred_arf, reference = test_set$pos)

#Extract accuracy
acc_arf <- confusionMatrix(data = pred_arf, reference = test_set$pos)$overall["Accuracy"]

#Accuracy table
at_modeling <- bind_rows(at_modeling, 
                         tibble(Method="Adjusted Random Forests",
                                Accuracy = acc_arf))
#Results table
at_modeling %>% knitr::kable()

#|Method                       |  Accuracy|
#|:----------------------------|---------:|
#|Baseline Logistic Regression | 0.6279859|
#|Adjusted Logistic Regression | 0.6285186|
#|Naive Bayes                  | 0.6290232|
#|Random Forests               | 0.6519009|
#|Adjusted Random Forests      | 0.6613771|

#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_arf$overall)))

#Plot of confusion matrix table
cm_arf_tab <- as.data.frame(cm_arf$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 10) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                               barheight = 10))
#Plot
cm_arf_tab

##########################################################
#                                                        #
#                         RESULTS                        #
#                                                        #
##########################################################

#########################################################
#############      RANDOM FORESTS  MODEL    #############
#########################################################

# Model of the entire training set tested in validation.
# With Age Group instead of age.

#Train model
fit_final <- train(pos ~ 
                     sex +
                     age_grp +
                     mun +
                     ses +
                     area,
                   method = "rf",
                   data = training)

#Predict
pred_final <- predict(fit_final, validation)

#Confusion matrix
cm_final <- confusionMatrix(data = pred_final, reference = validation$pos)

#Extract accuracy
acc_final <- confusionMatrix(data = pred_final, reference = validation$pos)$overall["Accuracy"]


#Accuracy table
at_final <- tibble(Method="Random Forests",
                   Accuracy = acc_final)

#Results
at_final %>% knitr::kable()


##|Method         |  Accuracy|
##|:--------------|---------:|
##|Random Forests | 0.6727222|


#Table of confusion matrix statistics
knitr::kable(as.data.frame(t(cm_final$overall)))

#Plot of confusion matrix table
cm_final_tab <- as.data.frame(cm_final$table) %>%
  ggplot(aes(Reference, Prediction, fill= Freq)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#E7E7E7",
                       mid = "#94B447",
                       high = "#003041") +
  geom_text(aes(label = Freq), color = "white", size = 10) +
  guides(fill = guide_colorbar(barwidth = 0.5,
                               barheight = 10))
#Plot
cm_final_tab

##This model gave an accuracy of almost 70%, which is a big improvement from our baseline model.
##However, it seems like our hypothesis, while somewhat accurate, is not sufficiently strong to demonstrated. 

###########################################################
###########################################################
##                                                       ##   
##                                                       ##
##                         END                           ## 
##                                                       ##
##                                                       ##
##                                                       ##
###########################################################
###########################################################
