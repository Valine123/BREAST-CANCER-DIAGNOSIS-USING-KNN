# Import dataset
wdbc <- read.csv("C:/Users/user/Downloads/wdbc.data", header=FALSE)
View(wdbc)

# View structure of the dataset
str(wdbc)

# Assign names to the variables
names(wdbc) <- c("ID", "Diagnosis", "radius1","texture1","perimeter1", "area1",
                 "smoothness1", "compactness1", "concavity1","concavepoints1",
                 "symmetry1","Fractaldimension1", "radius2", "texture2", "perimeter2",
                 "area2", "smoothness2", "compactness2", "concavity2", "concavepoints2",
                 "symmetry2", "fractaldimension2", "radius3", "texture3", "perimeter3",
                 "area3", "smoothness3", "compactness3", "concavity3", "concavepoints3",
                 "symmetry3", "fractaldimensions3")

# Exclude the ID variable
wdbc <- wdbc[-1]

#How many benign vs malignant cases do we have?
table(wdbc$Diagnosis)

# Code the target feature (diagnosis) as a factor and give B and more informative labels.
wdbc$Diagnosis<-factor(wdbc$Diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wdbc$Diagnosis))* 100, digits = 1)

# Taking a closer look at three numeric features, (for illustrative purpose)
summary(wdbc[c("radius1", "area1", "smoothness1")]) # We notice significant ranges about the mean that would significantly impact distance calculation

#scale the data values between the range of 0 to 1 to suppress the effects of outliers
library(caret)
wdbc_n <-preProcess(as.data.frame(wdbc[2:31]), method =  c("range"))
wdbc_n1 <- predict(wdbc_n,as.data.frame(wdbc[2:31]))
summary(wdbc_n1$area1)

#Split the wdbc_n1 data frame into wdbc_train and wdbc_test data frames
wdbc_train <- wdbc_n1[1:469,]
wdbc_test <- wdbc_n1[470:569,]

# For training the KNN model, we'll need to store these class labels in factor vectors
wdbc_train_labels <- wdbc[1:469,1]
wdbc_test_labels <- wdbc[470:569,1]

#Train a model on the data
library(class)
wdbc_pred <- knn(train = wdbc_train, test = wdbc_test, cl = wdbc_train_labels, k= 21)

install.packages("gmodels")
library(gmodels)
CrossTable(x=wdbc_test_labels, y=wdbc_pred, prop.chisq = FALSE)

