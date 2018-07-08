#dowloaded data
breast_cancer=read.csv("~alyonarodin/Desktop/R(knn)/breastcancer.csv", stringsAsFactors = FALSE)
#to see what variables we have
str(breast_cancer)
breast_cancer=breast_cancer[-1]
na.omit(breast_cancer)
table(breast_cancer$diagnosis)
breast_cancer$diagnosis=factor(breast_cancer$diagnosis, levels=c("B","M"), labels=c("Benign","Malignant"))
round(prop.table(table(breast_cancer$diagnosis))*100, digits = 1)
summary(breast_cancer[c("radius_mean","texture_mean","area_mean")])
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
breast_cancer_n=as.data.frame(lapply(breast_cancer[2:31], normalize))
breast_cancer_n[1:4,]
breast_cancer_new=data.frame(breast_cancer$diagnosis, breast_cancer_n)
write.csv(breast_cancer_new, file = "~alyonarodin/Desktop/R(knn)/breast_cancer_new.csv")
install.packages("class")
install.packages("plotly")
library("class")
library("plotly")
y1=breast_cancer_new$area_mean
y2=breast_cancer_new$radius_mean
y3=breast_cancer_new$texture_mean
y4=breast_cancer_new$perimeter_mean
y5=breast_cancer_new$smoothness_mean
y6=breast_cancer_new$compactness_mean
y7=breast_cancer_new$concavity_mean
y8=breast_cancer_new$concave.points_mean
y9=breast_cancer_new$symmetry_mean
y10=breast_cancer_new$fractal_dimension_mean
p <- plot_ly(type = 'box') %>%
#p=plot_ly(midwest,y=breast_cancer_new$area_mean, type='box')
add_boxplot(y=y1, name = "area_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y2, name = "radius_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y3, name = "texture_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y4, name = "perimeter_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y5, name = "smoothness_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y6, name = "compactness_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y7, name = "concavity_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y8, name = "concave.points_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y9, name = "symmetry_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y=y10, name = "fractal_dimension_mean",boxpoints = 'suspectedoutliers',
            marker = list(color = 'rgb(25,25,112)',
                          outliercolor = 'rgba(219, 64, 82, 0.6)',
                          line = list(outliercolor = 'rgba(128,0,0, 0.5)',
                                      outlierwidth = 2)),
            line = list(color = 'rgb(8,81,156)'))
p

breast_cancer_new_train=breast_cancer_n[1:427,]
breast_cancer_new_test=breast_cancer_n[428:569,]
breast_cancer_new_train_labels=breast_cancer[1:427,1]
breast_cancer_new_test_labels=breast_cancer[428:569,1]

breast_cancer_pred=knn(train=breast_cancer_new_train,test=breast_cancer_new_test,cl=breast_cancer_new_train_labels,k=21)
install.packages("gmodels")
library(gmodels)
CrossTable(x=breast_cancer_new_test_labels, y=breast_cancer_pred,prop.chisq=FALSE)
