############################################
############################################ 1. VISUALIZATION ###################################
############################################
############################################
### 1.1 Calling Neccesary Libraries 
l_packages = c("stats", "pls", "factoextra", "NbClust","ggplot2", "ggpubr","gridExtra",
               "fastICA", "plot3D", "caret", "randomForest","bst","pROC",
               "arules","arulesViz", "rpart", "FFTrees","rpart.plot",
               "knitr","kableExtra","formatR", "xtable") # used to create the Report (knitr)

for(p in l_packages){
  if (!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)  
}
############################################
### 1.2 Setting Up the Directory and Variables for Reproducibility
# Student and Assigment Information Variables
AS <- "FINAL-PROJECT"
# Folder Variables
drive="C:"
path.upto <- paste("Users", "Enrique","Documents", 
                   "Carleton", "Winter 2018", "STAT5703 Data Mining I", sep="/" )
code.dir <- paste(drive, path.upto, AS, "Code", sep="/")
data.dir <- paste(drive, path.upto, AS, "Data", sep="/")
work.dir <- paste(drive, path.upto, AS, "Work", sep="/")
report.dir <- paste(drive, path.upto, AS, "Report", sep="/")
setwd(work.dir)
getwd()
# For reproducibility
set.seed(12345)
############################################
## 1.3 Calling Neccesary Functions
source(paste(code.dir, paste(AS, "functions.r", sep=" "), sep="/"))
lsf.str()
############################################
## 1.4 Loading the Data
data.file <- paste(data.dir, "clev.csv", sep="/")
heart.data<-read.csv(data.file, header=TRUE)
heart.data$Ca <- gsub("?",NA,heart.data$Ca, fixed = TRUE)
heart.data$Ca<-as.numeric(heart.data$Ca)
heart.data$Thal<-gsub("?",NA,heart.data$Thal, fixed = TRUE)
heart.data$Thal<-as.numeric(heart.data$Thal)
heart.data<-as.data.frame(heart.data)
summary(heart.data)
############################################
## 1.5 Clean the Data 
# We can notice 6 NA's on variables "Ca" and "Thal", let's see those rows
dim(heart.data)
(heart.data.na <- subset(heart.data,(is.na(heart.data["Ca"]) | is.na(heart.data["Thal"]))))
# Number of rows with NAs values
nrow(heart.data.na)
# Percentage
(nrow(heart.data.na)/nrow(heart.data) * 100 )
# Because is only 2% I suggest to remove those rows 
heart.data <- subset(heart.data, !(is.na(heart.data["Ca"]) | is.na(heart.data["Thal"])))

#Check the new data
summary(heart.data)
str(heart.data)
dim(heart.data)
############################################
## 1.6 Some Plots
pairs(heart.data, upper.panel=panel.cor, diag.panel=panel.hist, 
      main = "Scaterplot Matrix")
kable(var(heart.data))
############################################
## 1.7 Create a New Variable (Disease) 
# This variable will be used in the analysis, if Num == 0 it means heart is Ok (healthy) and
# if Num > 0 means heart disease (non-healthy)
heart.data$Disease <- ifelse((heart.data$Num != 0),1,0)
heart.data$Disease <- factor(heart.data$Disease)
str(heart.data$Disease)
############################################
## 1.8 Create New Dataset with Factor Variables
heart.factor <- heart.data
heart.factor$Age <- factor(heart.factor$Age)
heart.factor$Sex <- factor(heart.factor$Sex)
heart.factor$CP <- factor(heart.factor$CP)
heart.factor$Fbs <- factor(heart.factor$Fbs)
heart.factor$Restecg <- factor(heart.factor$Restecg)
heart.factor$Exang <- factor(heart.factor$Exang)
heart.factor$Slope <- factor(heart.factor$Slope)
heart.factor$Ca <- factor(heart.factor$Ca)
heart.factor$Thal <- factor(heart.factor$Thal)
heart.factor$Num <- factor(heart.factor$Num)
# Check the new Data
summary(heart.factor)
str(heart.factor)
dim(heart.factor)
############################################
## 1.9 Create New Dataset with Factor Variables
heart.factor.names <- heart.factor

levels(heart.factor.names$Sex) <- c("Female", "Male")

levels(heart.factor.names$CP) <- c("Typical angina","Atypical angina",
                                   "Non-anginal pain","Asymptomatic")

levels(heart.factor.names$Fbs) <- c("< 120 mg/dl", "> 120 mg/dl")

levels(heart.factor.names$Restecg) <- c("Normal", "Having ST-T wave abnormality", 
                                        "Showing left ventricular hypertrophy")

levels(heart.factor.names$Exang) <- c("No", "Yes")

levels(heart.factor.names$Slope) <- c("Upsloping", "Flat", "Downsloping")

levels(heart.factor.names$Thal) <- c("Normal", "Fixed Defect", 
                                     "Reversible Defect")

levels(heart.factor.names$Disease) <- c("Healthy", "Non-healthy")

# Check the new Data
summary(heart.factor.names)
str(heart.factor.names)
dim(heart.factor.names)
############################################
## 1.10 Scaterplot Matrix Colored by Disease
# Plot colored by Disease
n.var <- length(heart.factor.names)
pairs(heart.factor.names[,1:(n.var-1)], 
      col = as.numeric(heart.factor.names$Disease)+1, 
      main = "Scaterplot Matrix Colored by Disease")
############################################
## 1.11 Histogram Plots
# Histogram Age by Disease
g.age <- ggplot(heart.factor.names, aes(x = Age, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Histogram: Age by Disease") + xlab("Age (years)")

# Histogram Sex by Disease
g.sex <- ggplot(heart.factor.names, aes(x = factor(Sex), fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Sex by Disease") + xlab("Sex") 

# Histogram CP by Disease
g.cp <- ggplot(heart.factor.names, aes(x = CP, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Type of Chest Pain by Disease") +
  xlab("Type of Chest Pain")

# Histogram TrestBps by Disease
g.trestbps <- ggplot(heart.factor.names, aes(x = Trestbps, fill = Disease)) +
  geom_histogram(alpha = .8,  bins = 30) + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Resting Blood Pressure by Disease") +
  xlab("Resting Blood Pressure (mm Hg)")

# Histogram Chol by Disease
g.chol <- ggplot(heart.factor.names, aes(x = Chol, fill = Disease)) +
  geom_histogram(alpha = .8, bins = 30) + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Serum Cholesterol by Disease") +
  xlab("Serum Cholesterol (mg/dl)")

# Histogram Fbs by Disease
g.fbs <- ggplot(heart.factor.names, aes(x = Fbs, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Fasting Blood Sugar by Disease") + 
  xlab("Fasting Blood Sugar (mg/dl)")

# Histogram Restecg by Disease
g.restegcg <- ggplot(heart.factor.names, aes(x = Restecg, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Resting Electrocardiogram by Disease") + 
  xlab("Resting ECG")

# Histogram Thalach by Disease
g.thalach <- ggplot(heart.factor.names, aes(x = Thalach, fill = Disease)) +
  geom_histogram(alpha = .8, bins = 30) + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Thalium Test Max Heart Rate by Disease") + 
  xlab("Thalium Test Max Heart Rate")

# Histogram Exang by Disease
g.exang <- ggplot(heart.factor.names, aes(x = Exang, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Exercise Induced Angina by Disease") + 
  xlab("Exercise Induced Angina")

# Histogram Oldpeak
g.oldpeak <- ggplot(heart.factor.names, aes(x = Oldpeak, fill = Disease)) +
  geom_histogram(alpha = .8, bins = 30) + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: ST Depression Induced by Exercise") + 
  xlab("ST Depression Induced by Exercise")

# Histogram Slope by Disease
g.slope <- ggplot(heart.factor.names, aes(x = Slope, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Slope of Peak Exercise ST Segment by Disease") + 
  xlab("Slope of Peak Exercise ST Segment")

# Histogram Ca by Disease
g.ca <- ggplot(heart.factor.names, aes(x = Ca, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Major Vessels Colored by Fluoroscopy by Disease") + 
  xlab("Major Vessels Colored by Fluoroscopy")

# Histogram Thal by Disease
g.thal <- ggplot(heart.factor.names, aes(x = Thal, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Thalium Stress Test Results by Disease") + 
  xlab("Thalium Stress Test Results")

# Histogram Num by Disease
g.num <- ggplot(heart.factor.names, aes(x = Num, fill = Disease)) +
  geom_histogram(alpha = .8, stat = "count") + 
  scale_fill_manual(values=c(3, 2)) + theme_bw() +
  ggtitle("Histogram: Major Vessels with > 50% Narrowing by Disease") + 
  xlab("Major Vessels with > 50% Narrowing")

# Plot all the Histograms
g <- grid.arrange(
  top = text_grob(label = "Histogram of Variables Colored by Disease", 
                  color = "blue", size=16 ),
  g.age,
  g.sex,
  g.cp,
  g.trestbps,
  g.chol,
  g.fbs,
  g.restegcg,
  g.thalach,
  g.exang,
  g.oldpeak,
  g.slope,
  g.ca,
  g.thal,
  g.num,
  ncol = 2)
############################################
## 1.12 Mosaic Plots
mosaicplot(heart.factor.names$Age ~ heart.factor.names$Disease,
           main="Fate by Age", shade=FALSE,color=c(3,2),
           xlab="Age (years)", ylab="Heart disease")

mosaicplot(heart.factor.names$Sex ~ heart.factor.names$Disease,
           main="Fate by Gender", shade=FALSE,color=c(3,2),
           xlab="Gender", ylab="Heart disease")

mosaicplot(heart.factor.names$CP ~ heart.factor.names$Disease,
           main="Fate by Chest Pain Type", shade=FALSE,color=c(3,2),
           xlab="Chest Pain Type", ylab="Heart disease")

mosaicplot(heart.factor.names$Trestbps ~ heart.factor.names$Disease,
           main="Fate by Resting Blood Pressure", shade=FALSE,color=c(3,2),
           xlab="Resting Blood Pressure", ylab="Heart disease")

mosaicplot(heart.factor.names$Chol ~ heart.factor.names$Disease,
           main="Fate by Serum Cholesterol", shade=FALSE,color=c(3,2),
           xlab="Serum Cholesterol", ylab="Heart disease")

mosaicplot(heart.factor.names$Fbs ~ heart.factor.names$Disease,
           main="Fate by Fasting Blood Sugar", shade=FALSE,color=c(3,2),
           xlab="Fasting Blood Sugar", ylab="Heart disease")

mosaicplot(heart.factor.names$Restecg ~ heart.factor.names$Disease,
           main="Fate by Resting Electrocardiogram", shade=FALSE,color=c(3,2),
           xlab="Resting Electrocardiogram", ylab="Heart disease")

mosaicplot(heart.factor.names$Thalach ~ heart.factor.names$Disease,
           main="Fate by Thalium Test Max Heart Rate", shade=FALSE,color=c(3,2),
           xlab="Thalium Test Max Heart Rate", ylab="Heart disease")

mosaicplot(heart.factor.names$Exang ~ heart.factor.names$Disease,
           main="Fate by Exercise Induced Angina", shade=FALSE,color=c(3,2),
           xlab="Exercise Induced Angina", ylab="Heart disease")

mosaicplot(heart.factor.names$Oldpeak ~ heart.factor.names$Disease,
           main="Fate by ST Depression Induced by Exercise", shade=FALSE,color=c(3,2),
           xlab="ST Depression Induced by Exercise", ylab="Heart disease")

mosaicplot(heart.factor.names$Slope ~ heart.factor.names$Disease,
           main="Fate by Slope of Peak Exercise ST Segment", shade=FALSE,color=c(3,2),
           xlab="Slope of Peak Exercise ST Segment", ylab="Heart disease")

mosaicplot(heart.factor.names$Ca ~ heart.factor.names$Disease,
           main="Fate by Major Vessels Colored by Fluoroscopy", shade=FALSE,color=c(3,2),
           xlab="Major Vessels Colored by Fluoroscopy", ylab="Heart disease")

mosaicplot(heart.factor.names$Thal ~ heart.factor.names$Disease,
           main="Fate by Thalium Stress Test Results", shade=FALSE,color=c(3,2),
           xlab="Thalium Stress Test Results", ylab="Heart disease")

mosaicplot(heart.factor.names$Num ~ heart.factor.names$Disease,
           main="Fate by Major Vessels with > 50% Narrowing", shade=FALSE,color=c(3,2),
           xlab="Major Vessels with > 50% Narrowing", ylab="Heart disease")
############################################
############################################ 2. DATASET SPLITTING #################################
############################################
############################################
# 2. Split the Dataset into Train and Data
(n.var <- length(heart.data))
# The firsts 13 variables are the predictors and the 15 is the class

inTrainRows <- createDataPartition(heart.data$Num,p=2/3,list=FALSE)
trainData <- heart.data[inTrainRows,]
trainData.factor <- heart.factor[inTrainRows,]
trainData.factor.names <- heart.factor.names[inTrainRows,]

testData <- heart.data[-inTrainRows,]
testData.factor <- heart.factor[-inTrainRows,]
testData.factor.names <-  heart.factor.names[-inTrainRows,]

nrow(trainData)/(nrow(testData)+nrow(trainData)) #checking whether really 2/3 -> OK

# Distribution of each class in the train and test set
summary(trainData.factor.names$Num)
summary(testData.factor.names$Num)

# Create the Train Set
X.trainData <- trainData[,1:13]
X.trainData.factor <- trainData.factor[,1:13]
X.trainData.factor.names <- trainData.factor.names[,1:13]
X.trainData.std <- f.data.std(X.trainData)
Y.trainData <- trainData[,15]
Y.trainData.factor.names <- trainData.factor.names[,15]
summary(Y.trainData.factor.names)

# Create the Test Set
X.testData <- testData[,1:13]
X.testData.factor <- testData.factor[,1:13]
X.testData.factor.names <- testData.factor.names[,1:13]
X.testData.std <- f.data.std(X.testData)
Y.testData <- testData[,15]
Y.testData.factor.names <- testData.factor.names[,15]
summary(Y.testData.factor.names)
############################################
############################################ 3. MINING ASSOCIATION RULES ##########################
############################################
############################################
# 3. Mining Association Rules (apriori)
data.file <- paste(data.dir, "allfactorsclev.csv", sep="/")
#this is a file where all the variables are discrete factors and every level of the factor
# corresponds to something (shown in the document)
allfactors<-read.csv(data.file, header = TRUE)
#make all variables factors so we can do association mining
allfactors$Age.Range<-as.factor(allfactors$Age.Range)
allfactors$Sex<-as.factor(allfactors$Sex)
allfactors$CP<-as.factor(allfactors$CP)
allfactors$Trestbps.range<-as.factor(allfactors$Trestbps.range)
allfactors$Chol.range<-as.factor(allfactors$Chol.range)
allfactors$Fbs<-as.factor(allfactors$Fbs)
allfactors$Restecg<-as.factor(allfactors$Restecg)
allfactors$Thalach.range<-as.factor(allfactors$Thalach.range)
allfactors$Exang<-as.factor(allfactors$Exang)
allfactors$Oldpeak.range<-as.factor(allfactors$Oldpeak.range)
allfactors$Slope<-as.factor(allfactors$Slope)
allfactors$Ca<-as.factor(allfactors$Ca)
allfactors$Thal<-as.factor(allfactors$Thal)
allfactors$Num<-as.factor(allfactors$Num)
#partition the classification factor so we can see the distinction
allfactors$Disease <- ifelse((allfactors$Num != 0),1,0)
allfactors$Disease <- factor(allfactors$Disease)
allfactors$Num<-NULL

#create rules for each subset

rules<-apriori((allfactors), appearance=list(rhs= c("Disease=0"), default="lhs"))
summary(rules)
#sort by count/support
rules_count<-sort(rules, by='count', decreasing = T)
inspect(rules_count[1:50])
#sort by lift
rules_lift<-sort(rules, by='lift', decreasing = T)
inspect(rules_lift[1:50])


rules1<-apriori((allfactors), appearance=list(rhs= c("Disease=1"), default="lhs"))
rules_1count <- sort(rules1, by='count', decreasing = TRUE)
rules_1lift <- sort(rules1, by='lift', decreasing = TRUE)
summary(rules1)
inspect(rules_1count[1:50])
inspect(rules_1lift[1:50])

topRules_count<-rules_count[1:25]
topRules_1count<-rules_1count[1:25]
topRules_lift<-rules_lift[1:25]
topRules_1lift<-rules_1lift[1:25]

plot(topRules_count, method="graph")
plot(topRules_1count, method="graph")

plot(topRules_lift, method="graph")
plot(topRules_1lift, method="graph")

plotly_arules(rules1)

#Validating the results
# Split the Dataset into Train and Data
(n.var <- length(allfactors))
# The firsts 13 variables are the predictors and the 15 is the class
inTrainRows_m <- createDataPartition(allfactors$Disease,p=2/3,list=FALSE)
trainData_m <- allfactors[inTrainRows_m,]
testData_m <-  allfactors[-inTrainRows_m,]

#For training data
rulestrain<-apriori((trainData_m), appearance=list(rhs= c("Disease=0"), default="lhs"))
rules_train_count<-sort(rulestrain, by='count', decreasing = T)
inspect(rules_train_count[1:50])
rules_train_lift<-sort(rulestrain, by='lift', decreasing = T)
inspect(rules_train_lift[1:50])
summary(rulestrain)

rules1train<-apriori((trainData_m), appearance=list(rhs= c("Disease=1"), default="lhs"))
rules_1train_count <- sort(rules1train, by='count', decreasing = TRUE)
inspect(rules_1train_count[1:50])
rules_1train_lift <- sort(rules1train, by='lift', decreasing = TRUE)
inspect(rules_1train_lift[1:50])
summary(rules1train)

topRules_train_count<-rules_train_count[1:25]
topRules_1train_count<-rules_1train_count[1:25]
topRules_train_lift<-rules_train_lift[1:25]
topRules_1train_lift<-rules_1train_lift[1:25]

plot(topRules_train_count, method="graph")
plot(topRules_1train_count, method="graph")
plot(topRules_train_lift, method="graph")
plot(topRules_1train_lift, method="graph")

#For test Data

rulestest<-apriori((testData_m), appearance=list(rhs= c("Disease=0"), default="lhs"))
rules_test_count<-sort(rulestest, by='count', decreasing = T)
inspect(rules_test_count[1:50])
rules_test_lift<-sort(rulestrain, by='lift', decreasing = T)
inspect(rules_test_lift[1:50])
summary(rulestest)

rules1test<-apriori((testData_m), appearance=list(rhs= c("Disease=1"), default="lhs"))
rules_1test_count<- sort(rules1test, by='count', decreasing = TRUE)
inspect(rules_1test_count[1:50])
rules_1test_lift<- sort(rules1test, by='lift', decreasing = TRUE)
inspect(rules_1test_lift[1:50])
summary(rules1test)

topRules_test_count<-rules_test_count[1:25]
topRules_1test_count<-rules_1test_count[1:25]
topRules_test_lift<-rules_test_lift[1:25]
topRules_1test_lift<-rules_1test_lift[1:25]

plot(topRules_test_count, method="graph")
plot(topRules_1test_count, method="graph")
plot(topRules_test_lift, method="graph")
plot(topRules_1test_lift, method="graph")
############################################
############################################ PREDICTIONS ###################################
############################################
############################################
# 4. Logistic Regression (Quasibinomial)
model.name <- "Logistic Regression (Quasibinomial) (glm)"
model.name1 <- model.name
file.name  <- paste(work.dir, "logistic.Rds", sep="/")
file.name.et  <- paste(work.dir, "logistic-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  (reg.logistic <- train(Disease ~ Age + Sex + CP + Trestbps + Chol + Fbs + Restecg 
                         + Thalach + Exang + Oldpeak + Slope + Ca + Thal, 
                         data = trainData,
                         method = "glm", 
                         family = "quasibinomial"))
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(reg.logistic, file = file.name)
  save(et, file = file.name.et)
}
reg.logistic
summary(reg.logistic)
plot(varImp(reg.logistic), main = paste0(model.name,"Variable Importance"))

plot(reg.logistic$finalModel, main = model.name)

test_pred_logistic <- predict(reg.logistic, newdata = X.testData)
test_pred_logistic

res.logistic <- confusionMatrix(test_pred_logistic, Y.testData) 
res.logistic

# This function is used to plot the confusion matrix
print_confusionm <- function(confmatrix, main, labels) {
  temp <- confmatrix$table
  colnames(temp) <- labels
  rownames(temp) <- labels
  
  fourfoldplot(temp, main = main, conf.level = 0)  
}

print_confusionm(res.logistic, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))



# Let's see the Complete Classification/Missclassification considering the Type the next
# function will calculate that

plot_results_by_type <- function(predict, real.class, main, testData) {
  
  new.set <- data.frame(predict) 
  new.set$classification <- predict == real.class 
  new.set$type <- testData$Num
  new.set$factorC <- with(new.set, 
                          interaction(factor(classification),  factor(type)))
  plot(new.set$factorC, main = main, col = c(1,1,2,2,3,3,4,4,5,5))
}

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_logistic,
                     Y.testData,
                     main, testData)

#This function will be used to plot the results 

plot_results <- function(X.data, Y.data, Y.predict, title, 
                         var1 = NULL, var2 = NULL, labels = TRUE) {
  
  res <- data.frame(X.data)
  res$class <- Y.data
  res$ID <- row.names(res)
  res$predict_class <- Y.predict
  res$col <- ifelse(res$class == res$predict_class, res$class,3)
  
  if (is.null(var1)) {
    var1 <- colnames(res)[1]
  }
  
  if (is.null(var2)) {
    var2 <- colnames(res)[2]
  }
  
  g <- ggplot(res,
              aes(res[[var1]], res[[var2]],
                  color = as.factor(res$col))) + 
    scale_color_manual(values = c(1,2,4), 
                       name ="Classification/Missclasification Results",
                       labels =c("Black (Class 0)","Red (Class 1)","Blue (Missclassified)")) +
    geom_point() + 
    labs(title = title) + xlab(var1) + ylab(var2) +   
    stat_chull(aes(group = as.factor(res$col)),
               geom = "polygon", fill = NA)
  
  if (labels) {
    # Add the labels to the graph
    g <- g + geom_text(aes(label = res$ID), size = 3, vjust = -0.7)
  }
  g
}

plot_results(X.testData, Y.testData,test_pred_logistic , 
             paste0(model.name," Classification/Missclasiffication\n"), 
             "Trestbps","Chol",labels = FALSE)

plot_results(X.testData, Y.testData, test_pred_logistic, 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)

# RMSE Resulting
(rmse_logistic <- sqrt(mean((as.numeric(test_pred_logistic)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_logistic <- (as.numeric(test_pred_logistic)-as.numeric(Y.testData)))
plot(r_logistic, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_logistic_prob <- predict(reg.logistic, newdata = X.testData, type ="prob")[2]

res.logistic.roc <- roc(as.numeric(Y.testData),
                        as.numeric(
                          as.matrix((test_pred_logistic_prob))))$auc

# Store the Values for Report
results_all <- data.frame("Method"= model.name,
                          "Prediction Accuracy in Training Set" = 
                            reg.logistic$results$Accuracy,
                          "Prediction Accuracy in Test Set" =  res.logistic$overall[1],
                          "RMSE Test"= rmse_logistic,
                          "ROC" = res.logistic.roc,
                          "Time Elapsed" = et,
                          stringsAsFactors = FALSE)
############################################
# 5. Logistic Regression (Binomial) (glm)
model.name <- "Logistic Regression (Binomial) (glm)"
model.name2 <- model.name
file.name  <- paste(work.dir, "logistic2.Rds", sep="/")
file.name.et  <- paste(work.dir, "logistic2-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  (reg.logistic2 <- train(factor(Disease) ~ Age + Sex + CP + Trestbps + Chol + Fbs + Restecg 
                          + Thalach + Exang + Oldpeak + Slope + Ca + Thal, 
                          data = trainData,
                          method = "glm", 
                          family = "binomial"))
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(reg.logistic2, file = file.name)
  save(et, file = file.name.et)
}
reg.logistic2
summary(reg.logistic2)
plot(varImp(reg.logistic2), 
     main = paste0(model.name," Variable Importance"))

plot(reg.logistic2$finalModel, main = model.name)

test_pred_logistic2 <- predict(reg.logistic2, newdata = X.testData)
test_pred_logistic2

res.logistic2 <- confusionMatrix(test_pred_logistic2, Y.testData) 
res.logistic2

print_confusionm(res.logistic2, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name, " Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_logistic2,
                     Y.testData,
                     main, testData)

plot_results(X.testData, Y.testData,test_pred_logistic2 , 
             paste0(model.name," Classification/Missclasiffication\n"), 
             "Trestbps","Chol",labels = FALSE)

plot_results(X.testData, Y.testData, test_pred_logistic2, 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)

# RMSE Resulting
(rmse_logistic2 <- sqrt(mean((as.numeric(test_pred_logistic2)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_logistic2 <- (as.numeric(test_pred_logistic2)-as.numeric(Y.testData)))
plot(r_logistic2, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_logistic2_prob <- predict(reg.logistic2, newdata = X.testData, type ="prob")[2]

res.logistic2.roc <- roc(as.numeric(Y.testData),
                         as.numeric(
                           as.matrix((test_pred_logistic2_prob))))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  reg.logistic2$results$Accuracy,
                                "Prediction Accuracy in Test Set" =  
                                  res.logistic2$overall[1],
                                "RMSE Test"= rmse_logistic2,
                                "ROC" = res.logistic2.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 6. Logistic Regression (Binomial) with Factor Variables (glm) 
model.name <- "Logistic Regression (Binomial) with Factor Variables (glm)" 
model.name4 <- model.name
file.name  <- paste(work.dir, "logistic4.Rds", sep="/")
file.name.et  <- paste(work.dir, "logistic4-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  (reg.logistic4 <- train(
    factor(Disease) ~ as.numeric(Age) + factor(Sex) + factor(CP) + 
      as.numeric(Trestbps) + as.numeric(Chol) + factor(Fbs) + 
      factor(Restecg) + as.numeric(Thalach) + factor(Exang) + 
      as.numeric(Oldpeak) + factor(Slope) + factor(Ca) + factor(Thal), 
    data = trainData,
    method = "glm", 
    family = "binomial"))
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(reg.logistic4, file = file.name)
  save(et, file = file.name.et)
}
reg.logistic4
summary(reg.logistic4)
plot(varImp(reg.logistic4), 
     main = paste0(model.name," Importance"))

plot(reg.logistic4$finalModel, 
     main = model.name)

test_pred_logistic4 <- predict(reg.logistic4, newdata = X.testData)
test_pred_logistic4

res.logistic4 <- confusionMatrix(test_pred_logistic4, Y.testData) 
res.logistic4

print_confusionm(res.logistic4, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"),
                 labels = c("Healthy", "Non-Healthy"))


main <- paste0(model.name, " Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_logistic4,
                     Y.testData,
                     main, testData)

#This function will be used to plot the results 

plot_results(X.testData, Y.testData,test_pred_logistic4, 
             paste0(model.name," Classification/Missclasiffication\n"), 
             "Trestbps","Chol",labels = FALSE)

plot_results(X.testData, Y.testData, test_pred_logistic4, 
             paste0(model.name, " Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)

# RMSE Resulting
(rmse_logistic4 <- sqrt(mean((as.numeric(test_pred_logistic4)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_logistic4 <- (as.numeric(test_pred_logistic4)-as.numeric(Y.testData)))
plot(r_logistic4, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_logistic4_prob <- predict(reg.logistic4, newdata = X.testData, type ="prob")[2]

res.logistic4.roc <- roc(as.numeric(Y.testData),
                         as.numeric(
                           as.matrix((test_pred_logistic4_prob))))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  reg.logistic4$results$Accuracy,
                                "Prediction Accuracy in Test Set" =  
                                  res.logistic4$overall[1],
                                "RMSE Test"= rmse_logistic4,
                                "ROC" = res.logistic4.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))  
############################################
# 7. Linear Model with Stepwise Feature Selection (glmStepAIC)
model.name <- "Linear Model with Stepwise Feature Selection (glmStepAIC)"
model.name5 <- model.name
file.name  <- paste(work.dir, "glmStepAIC.Rds", sep="/")
file.name.et  <- paste(work.dir, "glmStepAIC-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  (glmStepAIC <- train(factor(Disease) ~ Age + Sex + CP + Trestbps + Chol + Fbs + Restecg 
                       + Thalach + Exang + Oldpeak + Slope + Ca + Thal, data = trainData,
                       method = "glmStepAIC",
                       preProcess = c("center", "scale")))
  
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name, " Executed ... time:", et))
  save(glmStepAIC, file = file.name)
  save(et, file = file.name.et)
}
glmStepAIC
summary(glmStepAIC)
plot(varImp(glmStepAIC), 
     main = paste0(model.name," Variable Importance"))

plot(glmStepAIC$finalModel, 
     main = model.name)

test_pred_glmStepAIC <- predict(glmStepAIC, newdata = X.testData)
test_pred_glmStepAIC

res.glmStepAIC <- confusionMatrix(test_pred_glmStepAIC, Y.testData) 
res.glmStepAIC

print_confusionm(res.glmStepAIC, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))


main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% ",
               "narrowing (0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_glmStepAIC,
                     Y.testData,
                     main, testData)

plot_results(X.testData, Y.testData,test_pred_glmStepAIC , 
             paste0(model.name, " Classification/Missclasiffication\n"), 
             "Trestbps","Chol",labels = FALSE)

plot_results(X.testData, Y.testData, test_pred_glmStepAIC, 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)

# RMSE Resulting
(rmse_glmStepAIC <- sqrt(mean((as.numeric(test_pred_glmStepAIC)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_glmStepAIC <- (as.numeric(test_pred_glmStepAIC)-as.numeric(Y.testData)))
plot(r_glmStepAIC, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_glmStepAIC_prob <- predict(glmStepAIC, newdata = X.testData, type ="prob")[2]

res.glmStepAIC.roc <- roc(as.numeric(Y.testData),
                          as.numeric(
                            as.matrix((test_pred_glmStepAIC_prob))))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  glmStepAIC$results$Accuracy,
                                "Prediction Accuracy in Test Set" =  
                                  res.glmStepAIC$overall[1],
                                "RMSE Test"= rmse_glmStepAIC,
                                "ROC" = res.glmStepAIC.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 8. Kmeans (eclust)
############################################
## 8.1 PCA
pc.train <- prcomp(as.matrix(X.trainData.std), scale = TRUE, center = TRUE)

# Eigenvalues 
eig.val <- get_eig(pc.train)

# Let's see the PC
kable(eig.val)

fviz_screeplot(pc.train, addlabels = TRUE, ncp=13)

# Extract the results for variables
var <- get_pca_var(pc.train)

print(var)

# Let see the model generated (first five dimension and 10 variables)
kable(var$coord)

# Let see the contribution of each variable to each PC
kable(var$contrib)

# Contributions of variables to PC1
fviz_contrib(pc.train, choice = "var", axes = 1, top = 13)

# Contributions of variables to PC2
fviz_contrib(pc.train, choice = "var", axes = 2, top = 13)

# Contributions of variables to PC3
fviz_contrib(pc.train, choice = "var", axes = 3, top = 13)

# Extract the results for individuals
ind <- get_pca_ind(pc.train)

# Let's use the first 8 variables for the model
xx.pc.train <- as.matrix(ind$coord[,1:8])
min_k <- 2
max_k <- 4
min_seed <- 1
max_seed <- 100
method <- "euclidean"
type_data <- " PCA"

# Find Best Seed and K Using the PCA Data on training 
file.name  <- paste(work.dir, "best_seeds_std_pca.Rds", sep="/")
file.name.et  <- paste(work.dir, "best_seeds_std_pca-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  
  best_seeds_std_pca <- find_best_seedv2(xx.pc.train,
                                         method,
                                         min_seed,
                                         max_seed,
                                         min_k,
                                         max_k,
                                         Y.trainData)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Best Seed for Kmeans PCA Finded ... time:",et))
  save(best_seeds_std_pca, file = file.name)
  save(et, file = file.name.et)
}
(k <- best_seeds_std_pca$best_k)
(best_seed <- best_seeds_std_pca$best_seed)

res.train.kmeans_pca <- confusionMatrix(best_seeds_std_pca$best_km$cluster, 
                                        Y.trainData)
res.train.kmeans_pca


pc.test <- prcomp(as.matrix(X.testData.std), scale = TRUE, center = TRUE)

# Eigenvalues 

eig.val <- get_eig(pc.test)

# Let's see the PC
kable(eig.val)

fviz_screeplot(pc.test, addlabels = TRUE, ncp=13)

# Extract the results for variables
var <- get_pca_var(pc.test)

print(var)

# Let see the model generated (first five dimension and 10 variables)
kable(var$coord)

# Let see the contribution of each variable to each PC
kable(var$contrib)

# Contributions of variables to PC1
fviz_contrib(pc.test, choice = "var", axes = 1, top = 13)

# Contributions of variables to PC2
fviz_contrib(pc.test, choice = "var", axes = 2, top = 13)

# Contributions of variables to PC3
fviz_contrib(pc.test, choice = "var", axes = 3, top = 13)

# Extract the results for individuals
ind <- get_pca_ind(pc.test)

# Let's use the first 8 variables for the model
xx.pc.test <- as.matrix(ind$coord[,1:8])
model.name <- "Kmeans PCA (8)"
model.name6 <- model.name
xx.km <- eclust(xx.pc.test,k=k,hc_method=method, nstart=10, graph = FALSE, seed = best_seed)
kmeans_class_pca <- correct_kmeans_ids(Y.testData,xx.km$cluster)
kmeans_class_pca <- factor(kmeans_class_pca)


res.kmeans_pca <- confusionMatrix(kmeans_class_pca, Y.testData)
res.kmeans_pca

print_confusionm(res.kmeans_pca, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(kmeans_class_pca,
                     Y.testData,
                     main, testData)

# RMSE
(rmse_kmeans_pca <- sqrt(mean((as.numeric(kmeans_class_pca)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_kmeans_pca <- (as.numeric(kmeans_class_pca)-as.numeric(Y.testData)))
plot(r_kmeans_pca, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's Calculate the ROC
res.kmeans_pca.roc <- roc(as.numeric(Y.testData),
                          as.numeric(
                            as.matrix(kmeans_class_pca)))$auc


# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  res.train.kmeans_pca$overall[1] ,
                                "Prediction Accuracy in Test Set" =  
                                  res.kmeans_pca$overall[1],
                                "RMSE Test"= rmse_kmeans_pca,
                                "ROC" = res.kmeans_pca.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
fviz_cluster(xx.km)

plot_results(xx.pc.test, Y.testData, kmeans_class_pca, 
             paste0("Kmeans Clustering Classification/Missclasiffication (PCA)\n"),
             "Dim.1","Dim.2", labels = TRUE)

plot_results(xx.pc.test, Y.testData, kmeans_class_pca, 
             paste0("Kmeans Clustering Classification/Missclasiffication (PCA)\n"),
             "Dim.1","Dim.2", labels = FALSE)

############################################
## 8.2 All Data Whithout PCA
model.name <- "Kmeans (13)"
model.name7 <- model.name
min_k <- 2
max_k <- 4
min_seed <- 1
max_seed <- 100
method <- "euclidean"

# Find Best Seed and K and Kmeans for the all Std Data
file.name  <- paste(work.dir, "best_seeds_std_m.Rds", sep="/")
file.name.et  <- paste(work.dir, "best_seeds_std_m-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  best_seeds_std_m <- find_best_seedv2(X.trainData.std,
                                       method,
                                       min_seed,
                                       max_seed,
                                       min_k,
                                       max_k,
                                       Y.trainData)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Best Seed for Kmeans Finded ... time:", et))
  save(best_seeds_std_m, file = file.name)
  save(et, file = file.name.et)
}
(k <- best_seeds_std_m$best_k)
(best_seed <- best_seeds_std_m$best_seed)

res.train.kmeans <- confusionMatrix(best_seeds_std_m$best_km$cluster, Y.trainData)
res.train.kmeans

# Use the k and best seeds in the test set

xx.km2 <- eclust(X.testData.std,k=k,hc_method=method, nstart=10, 
                 graph = FALSE, seed = best_seed)

kmeans_class <- correct_kmeans_ids(Y.testData,xx.km2$cluster)
kmeans_class <- factor(kmeans_class)

res.kmeans <- confusionMatrix(kmeans_class, Y.testData)
res.kmeans

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(kmeans_class,
                     Y.testData,
                     main, testData)

# RMSE
(rmse_kmeans <- sqrt(mean((as.numeric(kmeans_class)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_kmeans <- (as.numeric(kmeans_class)-as.numeric(Y.testData)))
plot(r_kmeans, main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's Calculate the ROC
res.kmeans.roc <- roc(as.numeric(Y.testData),
                      as.numeric(
                        as.matrix(kmeans_class)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" =
                                  res.train.kmeans$overall[1] ,
                                "Prediction Accuracy in Test Set" =  
                                  res.kmeans$overall[1],
                                "RMSE Test"= rmse_kmeans,
                                "ROC" = res.kmeans.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
fviz_cluster(xx.km2)

plot_results(X.testData.std, Y.testData, kmeans_class, 
             paste0("Kmeans Clustering Classification/Missclasiffication (All Data)\n"),
             "Trestbps","Chol", labels = TRUE)

plot_results(X.testData.std, Y.testData, kmeans_class, 
             paste0("Kmeans Clustering Classification/Missclasiffication (All Data)\n"),
             "Trestbps","Chol", labels = FALSE)
############################################
# 9. Support Vector Machine (SVM)
############################################
## 9.1 SVM Radial (svmRadial)
model.name <- "SVM Radial"
model.name8 <- model.name
(d <- log2(13))
(sigma_values <- 2^(c(-d-3, -d-2, -d-1, -d, -d + 1, -d +2, -d+3)))
(c_values <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))
kernel_method_type <- "svmRadial"
# Grid Parameters
(grid_radial <- expand.grid(C = c_values, sigma = sigma_values))
# Cross Validation Parameters
trctrl <- trainControl(method = "repeatedcv", 
                       number = 10,
                       repeats = 3)

file.name  <- paste(work.dir, "svm_radial.Rds", sep="/")
file.name.et  <- paste(work.dir, "svm_radial-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  svm_Radial_Grid <- train(y = factor(Y.trainData),
                           x = X.trainData,
                           method = kernel_method_type,
                           trControl=trctrl,
                           preProcess = c("center", "scale"), 
                           trace = FALSE,
                           tuneGrid = grid_radial)
  t2 <- proc.time()
  et <-  elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:",et))
  save(svm_Radial_Grid, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
svm_Radial_Grid 
summary(svm_Radial_Grid)
plot(varImp(svm_Radial_Grid), 
     main = paste0(model.name," Variable Importance"))

plot(svm_Radial_Grid, main = model.name)

# Let's check the detail in the Training Set
res.svmradial.training <- confusionMatrix(svm_Radial_Grid$finalModel@fitted, Y.trainData)
res.svmradial.training

# Let's use the model in test set
test_pred_rgrid <- predict(svm_Radial_Grid, newdata = X.testData)
test_pred_rgrid

res.svmradial <- confusionMatrix(test_pred_rgrid, Y.testData)

res.svmradial

print_confusionm(res.svmradial, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_rgrid,
                     Y.testData,
                     main, testData)

plot_results(X.testData, Y.testData,test_pred_rgrid , 
             paste0(model.name," Classification/Missclasiffication\n"), 
             "Trestbps","Chol",labels = FALSE)

plot_results(X.testData, Y.testData,test_pred_rgrid, 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)


# RMSE Resulting
(rmse_svmradial <- sqrt(mean((as.numeric(test_pred_rgrid)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_svmradial <- (as.numeric(test_pred_rgrid)-as.numeric(Y.testData)))
plot(r_svmradial, 
     main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  res.svmradial.training$overall[1] ,
                                "Prediction Accuracy in Test Set" =  
                                  res.svmradial$overall[1],
                                "RMSE Test"= rmse_svmradial,
                                "ROC" = NA,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))

plot_results(X.testData, Y.testData, test_pred_rgrid , 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol",labels = TRUE)

plot_results(X.testData, Y.testData, test_pred_rgrid , 
             paste0(model.name," Classification/Missclasiffication\n"),
             "Trestbps","Chol", labels = FALSE)

############################################
## 9.2 Linear SVM (svmLinear)
# Setting Hyperparameters According with Paper
model.name <- "SVM Linear"
model.name9 <- model.name
kernel_method_type <- "svmLinear"

grid_Linear <- expand.grid(C = c_values)

file.name  <- paste(work.dir, "svm_linear.Rds", sep="/")
file.name.et  <- paste(work.dir, "svm_linear-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  svm_Linear_Grid <- train(y = factor(Y.trainData), 
                           x = X.trainData,
                           method = kernel_method_type,
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_Linear,
                           trace = FALSE,
                           tuneLength = 10)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(svm_Linear_Grid, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
svm_Linear_Grid 
summary(svm_Linear_Grid)
plot(varImp(svm_Linear_Grid), 
     main = paste0(model.name," Variable Importance"))

plot(svm_Linear_Grid, main = model.name)

# Let's check the detail in the Training Set
res.svmlinear.training <- confusionMatrix(svm_Linear_Grid$finalModel@fitted, Y.trainData)
res.svmlinear.training

# Let's use the model in test set

test_pred_lgrid <- predict(svm_Linear_Grid, newdata = X.testData)
test_pred_lgrid

res.svmlinear <- confusionMatrix(test_pred_lgrid, Y.testData)

res.svmlinear

print_confusionm(res.svmlinear, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))


main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_lgrid,
                     Y.testData,
                     main, testData)


# RMSE Resulting
(rmse_svmlinear <- sqrt(mean((as.numeric(test_pred_lgrid)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_svmlinear <- (as.numeric(test_pred_lgrid)-as.numeric(Y.testData)))
plot(r_svmlinear, main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  res.svmlinear.training$overall[1] ,
                                "Prediction Accuracy in Test Set" =  
                                  res.svmlinear$overall[1],
                                "RMSE Test"= rmse_svmlinear,
                                "ROC" = NA,
                                
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 10. Neural Networks
model.name <- "Neural Networks (nnet)"
model.name10 <- model.name
file.name  <- paste(work.dir, "nnet.Rds", sep="/")
file.name.et  <- paste(work.dir, "nnet-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  grid_n_net <- expand.grid(decay = c(0.0005, 0.005, 0.05), size = c(0,1,2,3))
  t1 <- proc.time()
  n_net <- train(y = factor(Y.trainData), 
                 x = X.trainData,
                 method="nnet",
                 preProcess = c("center", "scale"),
                 tuneGrid = grid_n_net,
                 trace = FALSE,
                 maxit = 500)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(n_net, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
n_net 
summary(n_net)
plot(varImp(n_net), main = paste0(model.name, " Variable Importance"))

plot(n_net, main = model.name)

# Let's use the model in test set
test_pred_n_net <- predict(n_net, newdata = X.testData)
test_pred_n_net

res.n_net <- confusionMatrix(test_pred_n_net, Y.testData)
res.n_net

print_confusionm(res.n_net, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")


plot_results_by_type(test_pred_n_net,
                     Y.testData,
                     main, testData)

# RMSE Resulting
(rmse_n_net <- sqrt(mean((as.numeric(test_pred_n_net)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_n_net <- (as.numeric(test_pred_n_net)-as.numeric(Y.testData)))
plot(r_n_net, main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_n_net_prob <- predict(n_net, newdata = X.testData, type ="prob")[2]

res.n_net.roc <- roc(as.numeric(Y.testData),
                     as.numeric(
                       as.matrix(test_pred_n_net_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(n_net$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.n_net$overall[1],
                                "RMSE Test"= rmse_n_net,
                                "ROC" = res.n_net.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 11. K-Nearest Neighbors
model.name <- "K-Nearest Neighbors (knn)"
model.name11 <- model.name
file.name  <- paste(work.dir, "knn.Rds", sep="/")
file.name.et  <- paste(work.dir, "knn-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  grid_knn <- expand.grid(k = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
  t1 <- proc.time()
  knn <- train(y = factor(Y.trainData), 
               x = X.trainData,
               method="knn",
               preProcess = c("center", "scale"),
               trControl=trctrl,
               tuneGrid = grid_knn)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(knn, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
knn 
summary(knn)
plot(varImp(knn), 
     main = paste0(model.name," Variable Importance"))

plot(knn, main = model.name)

# Let's use the model in test set
test_pred_knn <- predict(knn, newdata = X.testData)
test_pred_knn

res.knn <- confusionMatrix(test_pred_knn, Y.testData)
res.knn

print_confusionm(res.knn, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")


plot_results_by_type(test_pred_knn,
                     Y.testData,
                     main, testData)

# RMSE Resulting
(rmse_knn <- sqrt(mean((as.numeric(test_pred_knn)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_knn <- (as.numeric(test_pred_knn)-as.numeric(Y.testData)))
plot(r_knn, main = paste0(model.name," Residuals"),xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_knn_prob <- predict(knn, newdata = X.testData, type ="prob")[2]

res.knn.roc <- roc(as.numeric(Y.testData),
                   as.numeric(
                     as.matrix(test_pred_knn_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(knn$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.knn$overall[1],
                                "RMSE Test"= rmse_knn,
                                "ROC" = res.knn.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 12. Random Forest
############################################
## 12.1 Random Forest with Tuning Parameters (rf)
model.name <- "Random Forest with Tuning Parameters (rf)"
model.name12 <- model.name
file.name  <- paste(work.dir, "ramdonf.Rds", sep="/")
file.name.et  <- paste(work.dir, "randomf-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  grid_randomf <- expand.grid(mtry = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
  t1 <- proc.time()
  randomf <- train(y = factor(Y.trainData), 
                   x = X.trainData,
                   method="rf",
                   preProcess = c("center", "scale"),
                   trControl=trctrl, 
                   importance = TRUE,
                   tuneGrid = grid_randomf)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(randomf, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
randomf 
summary(randomf)
plot(varImp(randomf), main = paste0(model.name," Variable Importance"))
plot(varImp(randomf$finalModel), main = paste0(model.name," Variable Importance"))

plot(randomf, main = model.name)

# Let's use the model in test set
test_pred_randomf <- predict(randomf, newdata = X.testData)
test_pred_randomf

res.randomf <- confusionMatrix(test_pred_randomf, Y.testData)
res.randomf

print_confusionm(res.randomf, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")


plot_results_by_type(test_pred_randomf,
                     Y.testData,
                     main, testData)

# RMSE Resulting
(rmse_randomf <- sqrt(mean((as.numeric(test_pred_randomf)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_randomf <- (as.numeric(test_pred_randomf)-as.numeric(Y.testData)))
plot(r_randomf, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_randomf_prob <- predict(randomf, newdata = X.testData, type ="prob")[2]

res.randomf.roc <- roc(as.numeric(Y.testData),
                       as.numeric(
                         as.matrix(test_pred_randomf_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(randomf$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.randomf$overall[1],
                                "RMSE Test"= rmse_randomf,
                                "ROC" = res.randomf.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
## 12.2 Random Forest Boosted Tree (bstTree)
model.name <- "Random Forest Boosted Tree (bstTree)"
model.name13 <- model.name
file.name  <- paste(work.dir, "ramdonf2.Rds", sep="/") 
file.name.et  <- paste(work.dir, "randomf2-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  grid_randomf2 <- expand.grid(maxdepth = c(1,2,3), mstop = c(50,100,150,200), nu = c(0.1,0.2,0.3))
  randomf2 <- train(y = factor(Y.trainData), 
                    x = X.trainData,
                    method="bstTree",
                    preProcess = c("center", "scale"),
                    tuneGrid = grid_randomf2,
                    trControl=trctrl) 
  
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(randomf2, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
randomf2
summary(randomf2)
plot(varImp(randomf2), main = paste0(model.name," Variable Importance"))

plot(randomf2, main = model.name)

# Let's use the model in test set
test_pred_randomf2 <- predict(randomf2, newdata = X.testData)
test_pred_randomf2

res.randomf2 <- confusionMatrix(test_pred_randomf2, Y.testData)
res.randomf2

print_confusionm(res.randomf2, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")


plot_results_by_type(test_pred_randomf2, 
                     Y.testData,
                     main, testData) 

# RMSE Resulting
(rmse_randomf2 <- sqrt(mean((as.numeric(test_pred_randomf2)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_randomf2 <- (as.numeric(test_pred_randomf2)-as.numeric(Y.testData)))
plot(r_randomf2, main = model.name,xlab = "", ylab = "")

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(randomf2$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.randomf2$overall[1],
                                "RMSE Test"= rmse_randomf2,
                                "ROC" = NA,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
## 12.3 Random Forest Boost with Tuning Parameters (gbm)
model.name <- "Random Forest Boost with Tuning Parameters (gbm)"
model.name14 <- model.name
file.name  <- paste(work.dir, "ramdonf3.Rds", sep="/")
file.name.et  <- paste(work.dir, "randomf3-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  objControl <- trainControl(method='cv', number=10,  repeats = 10)
  grid_randomf3 <- expand.grid(interaction.depth =  c(1, 5, 9),
                               n.trees = (1:30)*50,
                               shrinkage = 0.1,
                               n.minobsinnode =10)
  
  t1 <- proc.time()
  randomf3 <- train(y = factor(Y.trainData), 
                    x = X.trainData,
                    method="gbm",
                    trControl=trctrl, 
                    tuneGrid = grid_randomf3,
                    verbose=FALSE)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name, " Executed ... time:", et))
  save(randomf3, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
randomf3
summary(randomf3)
plot(varImp(randomf3), 
     main = paste0(model.name," Variable Importance"))

plot(randomf3, main = model.name)

# Let's use the model in test set
test_pred_randomf3 <- predict(randomf3, newdata = X.testData)
test_pred_randomf3

res.randomf3 <- confusionMatrix(test_pred_randomf3, Y.testData)
res.randomf3

print_confusionm(res.randomf3, 
                 main = paste0("Confusion Matrix: ",model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")


plot_results_by_type(test_pred_randomf3,
                     Y.testData,
                     main, testData)

# RMSE Resulting
(rmse_randomf3 <- sqrt(mean((as.numeric(test_pred_randomf3)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_randomf3 <- (as.numeric(test_pred_randomf3)-as.numeric(Y.testData)))
plot(r_randomf3, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_randomf3_prob <- predict(randomf3, newdata = X.testData, type ="prob")[2]

res.randomf3.roc <- roc(as.numeric(Y.testData),
                        as.numeric(
                          as.matrix(test_pred_randomf3_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(randomf3$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.randomf3$overall[1],
                                "RMSE Test"= rmse_randomf3,
                                "ROC" = res.randomf3.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
## 12.4 Random Forest with Stochastic Gradient Boost (gbm)
model.name <- "Random Forest with Stochastic Gradient Boost (gbm)"
model.name15 <- model.name
file.name  <- paste(work.dir, "ramdonf4.Rds", sep="/")
file.name.et  <- paste(work.dir, "randomf4-et.Rds", sep="/")
Y.trainData.names <- factor(make.names(Y.trainData))
Y.testData.names <- factor(make.names(Y.testData))

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  grid_randomf4 <- expand.grid(interaction.depth =  c(1, 5, 9),
                               n.trees = (1:30)*50,
                               shrinkage = 0.1,
                               n.minobsinnode =10)
  trctrl2 <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          ## Estimate class probabilities
                          classProbs = TRUE,
                          ## Evaluate performance using
                          ## the following function
                          summaryFunction = twoClassSummary)
  
  t1 <- proc.time()
  randomf4 <- train(y = factor(Y.trainData.names), 
                    x = X.trainData,
                    method="gbm",
                    trControl=trctrl2, 
                    tuneGrid = grid_randomf4,
                    verbose=FALSE,
                    metric = "ROC")
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(randomf4, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
randomf4
summary(randomf4)
plot(varImp(randomf4), main = paste0(model.name," Variable Importance"))

plot(randomf4, main = model.name)

# Let's use the model in test set
test_pred_randomf4 <- predict(randomf4, newdata = X.testData)
test_pred_randomf4

res.randomf4 <- confusionMatrix(test_pred_randomf4, Y.testData.names)
res.randomf4

print_confusionm(res.randomf4, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_randomf4,
                     factor(Y.testData.names),
                     main, testData)

# RMSE Resulting
(rmse_randomf4 <- sqrt(mean((as.numeric(test_pred_randomf4)-as.numeric(Y.testData.names))^2)))

# Residuals Plot
(r_randomf4 <- (as.numeric(test_pred_randomf4)-as.numeric(Y.testData.names)))
plot(r_randomf4, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_randomf4_prob <- predict(randomf4, newdata = X.testData, type ="prob")[2]

res.randomf4.roc <- roc(as.numeric(Y.testData.names),
                        as.numeric(
                          as.matrix(test_pred_randomf4_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(randomf4$results$ROC, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.randomf4$overall[1],
                                "RMSE Test"= rmse_randomf4,
                                "ROC" = res.randomf4.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 13. Classification Tree (rpart)
model.name <- "Classification Tree (rpart)"
model.name16 <- model.name
heart.tree <- rpart(factor(Disease) ~ Age + Sex + CP + Trestbps + Chol + Fbs + Restecg + 
                      Thalach + Exang + Oldpeak + Slope + Ca + Thal, method = "class", 
                    data = trainData)

heart.tree
summary(heart.tree)

prp(heart.tree, extra = 100, main = model.name)

# Let's use the model in test set
test_pred_classtree <- predict(heart.tree,newdata = testData, type = "class")
test_pred_classtree

res.classtree <- confusionMatrix(test_pred_classtree, Y.testData)
res.classtree
print_confusionm(res.classtree, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_classtree,
                     Y.testData,
                     main, testData)

# RMSE Resulting
(rmse_classtree <- sqrt(mean((as.numeric(test_pred_classtree)-as.numeric(Y.testData))^2)))

# Residuals Plot
(r_classtree <- (as.numeric(test_pred_classtree)-as.numeric(Y.testData)))
plot(r_classtree, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_classtree_prob <- predict(heart.tree, newdata = X.testData, type ="prob")[,2]

res.classtree.roc <- roc(as.numeric(Y.testData),
                         as.numeric(
                           as.matrix(test_pred_classtree_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  NA,
                                "Prediction Accuracy in Test Set" =  
                                  res.classtree$overall[1],
                                "RMSE Test"= rmse_classtree,
                                "ROC" = res.classtree.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 14. Fast-and-Frugal Decision Trees (FFTrees)
model.name <- "Fast-and-Frugal Decision Trees (FFTrees)"
model.name17 <- model.name

trainDataF <- trainData
testDataF <- testData

# Make Disease Boolean
trainDataF$Disease <- ifelse(trainDataF$Disease == 1,TRUE,FALSE) 
testDataF$Disease <- ifelse(testDataF$Disease == 1,TRUE,FALSE)

# Create an FFTrees object called `heart_FFT`
heart_FFT <- FFTrees(formula = Disease ~ Age + Sex + CP + Trestbps + 
                       Chol + Fbs + Restecg + Thalach + Exang + 
                       Oldpeak + Slope + Ca + Thal,
                     data = trainDataF,
                     data.test = testDataF,
                     main = "ER Decisions",
                     decision.labels = c("Stable", "H Attack"))

# Print the summary statistics
heart_FFT

# Visualise the tree applied to the test data heart.test
plot(heart_FFT, data = "test")
plot(heart_FFT, stats = FALSE)
# Let's use the model in test set
test_pred_fft <- predict(heart_FFT,newdata = testDataF, type = "class")
test_pred_fft

res.fft <- confusionMatrix(test_pred_fft, testDataF$Disease)
res.fft

print_confusionm(res.fft, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_fft,
                     testDataF$Disease,
                     main, testDataF)

# RMSE Resulting
(rmse_fft <- sqrt(mean((as.numeric(test_pred_fft)-as.numeric(testDataF$Disease))^2)))

# Residuals Plot
(r_fft <- (as.numeric(test_pred_fft)-as.numeric(testDataF$Disease)))
plot(r_fft, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_fft_prob <- predict(heart_FFT, newdata = testDataF, type ="prob")[,2]

res.fft.roc <- roc(as.numeric(testDataF$Disease),
                   as.numeric(
                     as.matrix(test_pred_fft_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(heart_FFT$tree.stats$train$acc,na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  
                                  res.fft$overall[1],
                                "RMSE Test"= rmse_fft,
                                "ROC" = res.fft.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 15. Custom Fast-and-Frugal Decision Trees (FFTrees)
model.name <- "Custom Fast-and-Frugal Decision Trees (FFTrees)"
model.name18 <- model.name

# Create a Custom an FFTrees object called `custom__FFT`

custom_FFT <- FFTrees(formula = Disease ~ Age + Sex + CP + Trestbps + 
                        Chol + Fbs + Restecg + Thalach + Exang + 
                        Oldpeak + Slope + Ca + Thal,               
                      data = trainDataF,                    
                      data.test = testDataF,                
                      main = "Custom's Tree",                  
                      decision.labels = c("Stable", "Attack", 
                                          sens.w = 0.99,
                                          goal = "acc"))

# Print the summary statistics
custom_FFT

# Visualise the tree applied to the test data heart.test
plot(custom_FFT, data = "test")
plot(custom_FFT, stats = FALSE)
# Let's use the model in test set
test_pred_cfft <- predict(custom_FFT,newdata = testDataF, type = "class")
test_pred_cfft

res.cfft <- confusionMatrix(test_pred_cfft, testDataF$Disease)
res.cfft

print_confusionm(res.cfft, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_cfft,
                     testDataF$Disease,
                     main, testDataF)

# RMSE Resulting
(rmse_cfft <- sqrt(mean((as.numeric(test_pred_cfft)-as.numeric(testDataF$Disease))^2)))

# Residuals Plot
(r_cfft <- (as.numeric(test_pred_cfft)-as.numeric(testDataF$Disease)))
plot(r_cfft, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_cfft_prob <- predict(custom_FFT, newdata = testDataF, type ="prob")[,2]

res.cfft.roc <- roc(as.numeric(testDataF$Disease),
                    as.numeric(
                      as.matrix(test_pred_cfft_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(custom_FFT$tree.stats$train$acc,na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  
                                  res.cfft$overall[1],
                                "RMSE Test"= rmse_cfft,
                                "ROC" = res.cfft.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 16. Forest of Fast-and-Frugal Decision Trees (FFFOrest) 
model.name <- "Forest of Fast-and-Frugal Decision Trees (FFForest)"
model.name19 <- model.name
num_trees <- 100

file.name  <- paste(work.dir, "heart_fff.Rds", sep="/")
file.name.et  <- paste(work.dir, "heart_fff-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  # Create an FFForest object (can take a few minutes)
  heart_fff <- FFForest(formula = Disease ~ Age + Sex + CP + Trestbps + 
                          Chol + Fbs + Restecg + Thalach + Exang + 
                          Oldpeak + Slope + Ca + Thal,
                        data = rbind(trainDataF,testDataF),
                        ntree = num_trees, 
                        train.p = 2/3,
                        verbose = FALSE)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0(model.name," Executed ... time:", et))
  save(heart_fff, file = file.name)
  save(et, file = file.name.et)
}
# Visualise the tree applied to the test data heart.test
plot(heart_fff, data = "test")
plot(heart_fff, stats = FALSE)

plot(heart_fff)

# Let's choose the best tree

best <- 0
tree_id <- 0

print("Accuracy Results for Trees")
for (i in 1:num_trees) {
  
  fff_tree <- (heart_fff$fft.models[i])[[1]]
  
  test_pred_fff <- predict(fff_tree, data = testDataF)
  test_pred_fff
  
  res.fff <- confusionMatrix(test_pred_fff, testDataF$Disease)
  
  fff_tre_acc <- res.fff$overall[1]
  print(paste0("Tree:",i," Accuracy:",fff_tre_acc))
  
  
  if ( (best==0) || (fff_tre_acc > best) ) {
    
    best <- fff_tre_acc
    tree_id <- i
  }
}
print(paste0("Best Tree id:",tree_id, " accuracy:", fff_tre_acc))

# Select the best Tree for calculations
fff_tree <- (heart_fff$fft.models[tree_id])[[1]]

test_pred_fff <- predict(fff_tree, data = testDataF)
test_pred_fff

res.fff <- confusionMatrix(test_pred_fff, testDataF$Disease)
res.fff

print_confusionm(res.fff, 
                 main = paste0("Confusion Matrix: ", model.name," (Test Set)"), 
                 labels = c("Healthy", "Non-Healthy"))

main <- paste0(model.name," Classification/Misclassification in Test Set by\n",
               "Heart Disease Status: number of major vessels with >50% narrowing ",
               "(0=Healthy,1,2,3, or 4)")

plot_results_by_type(test_pred_fff,
                     testDataF$Disease,
                     main, testDataF)

# RMSE Resulting
(rmse_fff <- sqrt(mean((as.numeric(test_pred_fff)-as.numeric(testDataF$Disease))^2)))

# Residuals Plot
(r_fff <- (as.numeric(test_pred_fff)-as.numeric(testDataF$Disease)))
plot(r_fff, main = model.name,xlab = "", ylab = "")

# Let's check the probabilities and Calculate the ROC
test_pred_fff_prob <- predict(fff_tree, data = testDataF, type ="prob")[,2]

res.fff.roc <- roc(as.numeric(testDataF$Disease),
                   as.numeric(
                     as.matrix(test_pred_fff_prob)))$auc

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= model.name,
                                "Prediction Accuracy in Training Set" = 
                                  max(fff_tree$tree.stats$train$acc,na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  
                                  res.fff$overall[1],
                                "RMSE Test"= rmse_fff,
                                "ROC" = res.fff.roc,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
############################################
# 17. Optimizing Cost
model.name <- "Minimun Cost"

## Specifying Cost
heart.cue.cost <- data.frame("cue" = 
                               c("age", "sex", "cp", "trestbps", "chol",  "fbs", "restecg",
                                 "thalach", "exang", "oldpeak", "slope", "ca", "thal"),
                             "cost" = c(1, 1, 1, 1, 7.27, 5.2, 15.5, 102.9, 87.3, 87.3, 
                                        87.3, 100.9, 102.9))
# Specify the following costs for heart disease diagnosis:
# cost(Hit) = 0, cost(False Alarm) = 100, cost(Miss) = 200, cost(correct rejection) = 0
heart.cost.outcomes <- c(0, 500, 1000, 0)

heart.costA.fft <- FFTrees(formula = Disease ~.,
                           data = rbind(trainDataF,testDataF),
                           cost.outcomes = heart.cost.outcomes,
                           cost.cues = heart.cue.cost,
                           goal = "bacc",
                           goal.chase = "bacc")

summary(heart.costA.fft)$train[1,]

heart.costB.fft <- FFTrees(formula = Disease ~.,
                           data = rbind(trainDataF,testDataF),
                           cost.outcomes = heart.cost.outcomes,
                           cost.cues = heart.cue.cost,
                           goal = "cost",
                           goal.chase = "cost")

summary(heart.costB.fft)$train[1,]
## ############################################
## ############################################ RESULTS  ###################################
## ############################################
## 
############################################
## 18.1 Mining Association Rules  
############################################
plot(topRules_count, method="graph")
plot(topRules_1count, method="graph")
plot(topRules_lift, method="graph")
plot(topRules_1lift, method="graph")
plot(topRules_train_count, method="graph")
plot(topRules_1train_count, method="graph")
plot(topRules_train_lift, method="graph")
plot(topRules_1train_lift, method="graph")
plot(topRules_test_count, method="graph")
plot(topRules_1test_count, method="graph")
plot(topRules_test_lift, method="graph")
plot(topRules_1test_lift, method="graph")
############################################
## 17.2 All Prediction Models Results
rownames(results_all) <- NULL
kable(results_all)
############################################
### Confusion Matrix
############################################

par(mfrow=c(1,2))
print_confusionm(res.logistic, 
                 main = model.name1, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.logistic2, 
                 main = model.name2, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.logistic4, 
                 main = model.name4,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.glmStepAIC, 
                 main = model.name5,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.kmeans_pca, 
                 main = model.name6,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.kmeans, 
                 main = model.name7,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.svmradial, 
                 main = model.name8, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.svmlinear, 
                 main = model.name9,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.n_net, 
                 main = model.name10,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.knn, 
                 main = model.name11, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.randomf, 
                 main = model.name12,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.randomf2, 
                 main = model.name13,
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.randomf3, 
                 main = model.name14, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.randomf4, 
                 main = model.name15, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.classtree, 
                 main = model.name16, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.fft, 
                 main = model.name17, 
                 labels = c("Healthy", "Non-Healthy"))

print_confusionm(res.cfft, 
                 main = model.name18, 
                 labels = c("Healthy", "Non-Healthy"))
############################################
### Models Accuracy
############################################
par(mfrow=c(1,2))

plot(reg.logistic$finalModel, main = model.name1)
plot(reg.logistic2$finalModel, main = model.name2)
plot(reg.logistic4$finalModel, main = model.name4)

plot(glmStepAIC$finalModel, main = model.name5)

plot(svm_Radial_Grid, main = model.name8)
plot(svm_Linear_Grid, main = model.name9)

plot(n_net, main = model.name10)
plot(knn, main = model.name11)

plot(randomf, main = model.name12)
plot(randomf2, main = model.name13)
plot(randomf3, main = model.name14)
plot(randomf4, main = model.name15)
prp(heart.tree, extra = 100, main = model.name16)
plot(heart_FFT, main = model.name17)

plot(custom_FFT, main = model.name18)
############################################
### Models Residuals
############################################
par(mfrow=c(1,2))
plot(r_logistic, main = model.name1,  xlab = "", ylab = "")
plot(r_logistic2, main = model.name2,  xlab = "", ylab = "")
plot(r_logistic4, main = model.name4,  xlab = "", ylab = "")

plot(r_glmStepAIC, main = model.name5,  xlab = "", ylab = "")
plot(r_kmeans, main = model.name6, xlab = "", ylab = "")
plot(r_kmeans_pca, main = model.name7, xlab = "", ylab = "")
plot(r_svmradial, main = model.name8, xlab = "", ylab = "")
plot(r_svmlinear, main = model.name9, xlab = "", ylab = "")

plot(r_n_net, main = model.name10, xlab = "", ylab = "")
plot(r_knn, main = model.name11, xlab = "", ylab = "")
plot(r_randomf, main = model.name12, xlab = "", ylab = "")
plot(r_randomf2, main = model.name13, xlab = "", ylab = "")
plot(r_randomf3, main = model.name14, xlab = "", ylab = "")
plot(r_randomf4, main = model.name15, xlab = "", ylab = "")

plot(r_classtree, main = model.name16, xlab = "", ylab = "")
plot(r_fft, main = model.name17, xlab = "", ylab = "")

plot(r_cfft, main = model.name18, xlab = "", ylab = "") 
## ############################################
## ############################################ END  ###################################
## ############################################
## 
