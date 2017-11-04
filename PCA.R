library(caTools) #to split into test and training data sets
library(caret) #to build principal components
library(e1071) #to build svm classifier

#read data
#setwd('Pick the folder that contains the Wine.csv file')
dataset = read.csv('Wine.csv')

#split dataset into training and test set
set.seed(123)
split = sample.split(Y = dataset$Customer_Segment,
                     SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#scale continuous features
training_set[,-14] = scale(training_set[,-14])
test_set[,-14] = scale(test_set[,-14])

#apply PCA to training and test data sets
pca = preProcess(x = training_set[,-14],
                 method = "pca",
                 pcaComp = 2)
training_set_pca = predict(pca, training_set)
test_set_pca = predict(pca, test_set)

#move the customer_segment to the last 
training_set_pca = training_set_pca[,c(2,3,1)]
test_set_pca = test_set_pca[,c(2,3,1)]

#fit classifier using linear SVM classifier
classifier_svm = svm(formula = Customer_Segment ~ .,
                     data = training_set_pca,
                     type = 'C-classification',
                     kernel = 'linear')

#predict the test data using linear svm classifier
y_pred = predict(classifier_svm,
                 newdata = test_set_pca[,-3])

#build confusion matrix
cm = table(test_set_pca[,3],y_pred)
cm
