setwd("C:\\Users\\user\\Documents\\DataScience\\LunchNLearn\\20181121")
install.packages("MASS")
library(MASS)

customer.date = read.csv("dummy_dataset_clustering.csv")

#in pet column(categorical varible) can be changed into the sum of revenue for these
# categories


# pre processing
cluster.data = customer.date[, -c(1,2,3,4,6,7,17,19)]

#cluster
group.1 = kmeans(cluster.data, centers = 3)


#scaling what is scaling?
# what is the arange of a variable?

# assignmenbers
cluster.data$ groups = group.1$cluster
group.1$size
group.1$centers


#check the discrimination

out.discr.1 = lda(groups~., data = cluster.data)
out.discr.1$scaling

plot(out.discr.1)
#second iteration
#including scaling

customer.date.scalled = as.data.frame(scale(cluster.data))
group.2 = kmeans(cluster.data, centers = 3)
group.2$size
customer.date.scalled