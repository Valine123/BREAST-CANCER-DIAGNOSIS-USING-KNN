# BREAST-CANCER-DIAGNOSIS-USING-KNN


# Import dataset
wdbc <- read.csv("C:/Users/user/Downloads/wdbc.data", header=FALSE)
View(wdbc)

# Assign names to the variables
names(wdbc) <- c("ID", "Diagnosis", "radius1","texture1","perimeter1", "area1",
                 "smoothness1", "compactness1", "concavity1","concavepoints1",
                 "symmetry1","Fractaldimension1", "radius2", "texture2", "perimeter2",
                 "area2", "smoothness2", "compactness2", "concavity2", "concavepoints2",
                 "symmetry2", "fractaldimension2", "radius3", "texture3", "perimeter3",
                 "area3", "smoothness3", "compactness3", "concavity3", "concavepoints3",
                 "symmetry3", "fractaldimensions3")

 # View structure of the dataset
str(wdbc)     

data.frame':	569 obs. of  31 variables:
 $ Diagnosis         : Factor w/ 2 levels "Benign","Malignant": 2 2 2 2 2 2 2 2 2 2 ...
 $ radius1           : num  18 20.6 19.7 11.4 20.3 ...
 $ texture1          : num  10.4 17.8 21.2 20.4 14.3 ...
 $ perimeter1        : num  122.8 132.9 130 77.6 135.1 ...
 $ area1             : num  1001 1326 1203 386 1297 ...
 $ smoothness1       : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
 $ compactness1      : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
 $ concavity1        : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
 $ concavepoints1    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
 $ symmetry1         : num  0.242 0.181 0.207 0.26 0.181 ...
 $ Fractaldimension1 : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
 $ radius2           : num  1.095 0.543 0.746 0.496 0.757 ...
 $ texture2          : num  0.905 0.734 0.787 1.156 0.781 ...
 $ perimeter2        : num  8.59 3.4 4.58 3.44 5.44 ...
 $ area2             : num  153.4 74.1 94 27.2 94.4 ...
 $ smoothness2       : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
 $ compactness2      : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
 $ concavity2        : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
 $ concavepoints2    : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
 $ symmetry2         : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
 $ fractaldimension2 : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
 $ radius3           : num  25.4 25 23.6 14.9 22.5 ...
 $ texture3          : num  17.3 23.4 25.5 26.5 16.7 ...
 $ perimeter3        : num  184.6 158.8 152.5 98.9 152.2 ...
 $ area3             : num  2019 1956 1709 568 1575 ...
 $ smoothness3       : num  0.162 0.124 0.144 0.21 0.137 ...
 $ compactness3      : num  0.666 0.187 0.424 0.866 0.205 ...
 $ concavity3        : num  0.712 0.242 0.45 0.687 0.4 ...
 $ concavepoints3    : num  0.265 0.186 0.243 0.258 0.163 ...
 $ symmetry3         : num  0.46 0.275 0.361 0.664 0.236 ...
 $ fractaldimensions3: num  0.1189 0.089 0.0876 0.173 0.0768 ...


# Exclude the ID variable
wdbc <- wdbc[-1]

#How many benign vs malignant cases do we have?
table(wdbc$Diagnosis)

Benign Malignant 
      357       212

# Code the target feature (diagnosis) as a factor and give B and more informative labels.
wdbc$Diagnosis<-factor(wdbc$Diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wdbc$Diagnosis))* 100, digits = 1)

Benign Malignant 
     62.7      37.3 

# Taking a closer look at three numeric features, (for illustrative purpose)
summary(wdbc[c("radius1", "area1", "smoothness1")]) # We notice significant ranges about the mean that would significantly impact distance calculation

   radius1           area1         smoothness1     
 Min.   : 6.981   Min.   : 143.5   Min.   :0.05263  
 1st Qu.:11.700   1st Qu.: 420.3   1st Qu.:0.08637  
 Median :13.370   Median : 551.1   Median :0.09587  
 Mean   :14.127   Mean   : 654.9   Mean   :0.09636  
 3rd Qu.:15.780   3rd Qu.: 782.7   3rd Qu.:0.10530  
 Max.   :28.110   Max.   :2501.0   Max.   :0.16340

#scale the data values between the range of 0 to 1 to suppress the effects of outliers
library(caret)
wdbc_n <-preProcess(as.data.frame(wdbc[2:31]), method =  c("range"))
wdbc_n1 <- predict(wdbc_n,as.data.frame(wdbc[2:31]))
summary(wdbc_n1$area1)

 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.1174  0.1729  0.2169  0.2711  1.0000

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

  Cell Contents
|-------------------------|
|                       N |
|           N / Row Total |
|           N / Col Total |
|         N / Table Total |
|-------------------------|

 
Total Observations in Table:  100 

 
                 | wdbc_pred 
wdbc_test_labels |    Benign | Malignant | Row Total | 
-----------------|-----------|-----------|-----------|
          Benign |        77 |         0 |        77 | 
                 |     1.000 |     0.000 |     0.770 | 
                 |     0.975 |     0.000 |           | 
                 |     0.770 |     0.000 |           | 
-----------------|-----------|-----------|-----------|
       Malignant |         2 |        21 |        23 | 
                 |     0.087 |     0.913 |     0.230 | 
                 |     0.025 |     1.000 |           | 
                 |     0.020 |     0.210 |           | 
-----------------|-----------|-----------|-----------|
    Column Total |        79 |        21 |       100 | 
                 |     0.790 |     0.210 |           | 
-----------------|-----------|-----------|-----------|

## INTERPRETATION:
In the top-left cell, are the true negative results. These 77 
of 100 values indicate cases where the mass was benign, and the kNN algorithm 
correctly identified it as such. The bottom-right cell, indicates the true 
positive results, where the classifier and the clinically determined label agree that 
the mass is malignant. A total of 21 of 100 predictions were true positives.

The cells falling on the other diagonal contain counts of examples where the kNN 
approach disagreed with the true label. The 2 examples in the lower-left FN cell are 
false negative results; in this case, the predicted value was benign but the tumor 
was actually malignant. Errors in this direction could be extremely costly, as they 
might lead a patient to believe that she is cancer-free, when in reality the disease 
may continue to spread. The top right cell would contain the false positive results, 
if there were any. These values occur when the model classifies a mass as malignant
when in reality it was benign. Although such errors are less dangerous than a false 
negative result, they should also be avoided as they could lead to additional financial 
burden on the health care system, or additional stress for the patient, as additional 
tests or treatment may have to be provided.

A total of 2 percent, that is, 2 out of 100 masses were incorrectly classified by the 
kNN approach, while 98 percent were accuratley classified, indicating an impressive accuracy of the KNN model.

