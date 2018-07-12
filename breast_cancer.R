#dowloaded data
breast_cancer=read.csv("~alyonarodin/Desktop/R(knn)/breastcancer.csv", stringsAsFactors = FALSE)
#to see what variables we have
str(breast_cancer)
#deleted the first column
breast_cancer=breast_cancer[-1]
#omitted NA values
na.omit(breast_cancer)
#assigned labels Benign and Malignant
table(breast_cancer$diagnosis)
breast_cancer$diagnosis=factor(breast_cancer$diagnosis, levels=c("B","M"), labels=c("Benign","Malignant"))
round(prop.table(table(breast_cancer$diagnosis))*100, digits = 1)
#summary of few variables 
summary(breast_cancer[c("radius_mean","texture_mean","area_mean")])
#function to normalize the values. This function transforms values fall in a range between 0 and 1.
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#used the function to normalize the values 
breast_cancer_n=as.data.frame(lapply(breast_cancer[2:31], normalize))
breast_cancer_n[1:4,]
#transfered values to the data frame to be able to save it as .csv file to plot in Tableau
breast_cancer_new=data.frame(breast_cancer$diagnosis, breast_cancer_n)
#wrote the data into csv file 
write.csv(breast_cancer_new, file = "~alyonarodin/Desktop/R(knn)/breast_cancer_new.csv")
install.packages("class")
install.packages("plotly")
library("class")
library("plotly")
#another way to plot data - plotly - example 
y1=breast_cancer_new$area_mean
y2=breast_cancer_new$radius_mean
y3=breast_cancer_new$texture_mean
y4=breast_cancer_new$perimeter_mean

p <- plot_ly(type = 'box') %>%
add_boxplot(y = y1, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                               marker = list(color = 'rgb(7,40,89)'),
                               line = list(color = 'rgb(7,40,89)'),
                               name = "All Points") %>%
add_boxplot(y = y2, name = "Only Whiskers", boxpoints = FALSE,
                               marker = list(color = 'rgb(9,56,125)'),
                               line = list(color = 'rgb(9,56,125)')) %>%
add_boxplot(y = y3, name = "Suspected Outlier", boxpoints = 'suspectedoutliers',
                               marker = list(color = 'rgb(8,81,156)',
                               outliercolor = 'rgba(219, 64, 82, 0.6)',
                               line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                               outlierwidth = 2)),
                               line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y = y4, name = "Whiskers and Outliers", boxpoints = 'outliers',
                               marker = list(color = 'rgb(107,174,214)'),
                               line = list(color = 'rgb(107,174,214)')) %>%
layout(title = "Box Plot Styling Outliers")  
p
#prepared data as train and test
breast_cancer_new_train=breast_cancer_n[1:427,]
breast_cancer_new_test=breast_cancer_n[428:569,]
breast_cancer_new_train_labels=breast_cancer[1:427,1]
breast_cancer_new_test_labels=breast_cancer[428:569,1]
#k_NN model
breast_cancer_pred=knn(train=breast_cancer_new_train,test=breast_cancer_new_test,cl=breast_cancer_new_train_labels,k=21)
install.packages("gmodels")
library(gmodels)
#evaluating model performance using cross table
CrossTable(x=breast_cancer_new_test_labels, y=breast_cancer_pred,prop.chisq=FALSE)

