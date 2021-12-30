### Classification
rm(list = ls())
require(tidyverse)
require(DataExplorer)
require(corrplot)
require(caret)
require(GGally)
require(cvms)
require(rpart)
require(rpart.plot)

cancer_df <- read_delim('~/Documents/Salford/Fall21/ASDM/Course Work/data.csv', delim = ",",
                        col_names = T)

#cancer_df %>% View()
cancer_df %>% spec()
cancer_df %>% str()

## Drop ID variable

cancer_df <- cancer_df %>%
  select(-id)

## Count Missing observations
map_int(cancer_df, function(.x) sum(is.na(.x)))

## Check label distribution

cancer_df %>% select(diagnosis) %>% 
  table() %>% prop.table() %>% round(2)

"
We have two options:

- We can drop highly correlated variables
- We can do PCA

Imma stick with option 1
"


## Divide into Training Nd Testing
set.seed(1)
test_index <- createDataPartition(y = cancer_df$diagnosis, times = 1, p = 0.25, list = FALSE)
train_set <- cancer_df[-test_index, ]
test_set <- cancer_df[test_index, ]

## Reduce Dimensions Check for correlation

ytr <- train_set %>% 
  select(diagnosis) 


Xtr <- train_set %>% 
  select(-diagnosis)

cor_df <- cor(Xtr)

corrplot(cor_df, title = 'Correlation Matrix', order = "hclust", tl.cex = 1, addrect = 4)

## Drop Highly correlated variables
to_drop <- findCorrelation(cor_df, cutoff = 0.7)

new_df <-  Xtr %>% select(-names(Xtr)[to_drop])

## Examine the new df

train_df <- bind_cols(ytr, new_df)

test_df <- test_set %>% select(-names(Xtr)[to_drop])

## Exploring the training set

plot_str(train_df) ## Plot structure

introduce(train_df)

plot_missing(train_df) 

plot_intro(train_df)


plot_histogram(train_df)

## or

ggplot(gather(train_df %>% select(-diagnosis)), aes(value)) + 
  geom_histogram(bins = 10, color = 'black') + 
  facet_wrap(~key, scales = 'free_x')


plot_bar(train_df)

## Do it yourself

ggplot(train_df , aes(x = diagnosis, fill = diagnosis)) +
  geom_bar() + ggtitle("Distribution of Breast Cancer") + theme_bw() 

##
ggpairs(train_df, aes(colour=diagnosis)) + theme_bw()  ## You may break it into 2

## or
ggpairs(train_df, columns = 1: (ncol(train_set) - 1),
        ggplot2::aes(colour = diagnosis)) + theme_bw()

plot_correlation(train_df %>% select(-diagnosis))


train_df$diagnosis <- ifelse(train_df$diagnosis == 'M', 1, 0)
train_df$diagnosis <- as.factor(train_df$diagnosis)

test_df$diagnosis <- ifelse(test_df$diagnosis == 'M', 1, 0)
test_df$diagnosis <- as.factor(test_df$diagnosis)

## Scaling!

means <- apply(train_df %>% select(-diagnosis), 2, mean)
sds <- apply(train_df %>% select(-diagnosis), 2, sd)

scaled_train <- train_df %>% select(-diagnosis) %>% scale() 
scaled_train <- cbind(train_df['diagnosis'], scaled_train)


scaled_test <- test_df %>% select(-diagnosis) %>% scale(center = means, scale = sds) 
scaled_test <- cbind(test_df['diagnosis'], scaled_test)

## Logistic Regression
logit_model <- glm(diagnosis ~., data = scaled_train, family = binomial())

summary(logit_model)

prediction <- predict(logit_model, scaled_test[,-1], type = 'response')
prediction <- ifelse(prediction > 0.5,1,0)

conf <- caret::confusionMatrix(data = factor(prediction), reference = scaled_test[,1], positive = '1')
conf

## Wanna plot conf. mat?

conf_mat <- as_tibble(table(predicted = prediction, actual = scaled_test[,1]))

plot_confusion_matrix(conf_mat, 
                      target_col = "actual",
                      prediction_col = "predicted",
                      counts_col = 'n')

## Some Stepwise Regression

step_model <- MASS::stepAIC(logit_model, direction = 'both', trace = FALSE)
summary(step_model)

step_prediction <- predict(step_model, scaled_test[,-1], type = 'response')
step_prediction <- ifelse(step_prediction > 0.5, 1, 0)

conf2 <- caret::confusionMatrix(data = factor(step_prediction), reference = scaled_test[,1], positive = '1')
conf2


## Plot Variable Importance

importance <- data.frame(step_model$coefficients[-1])
colnames(importance) <- c("Importance")

importance %>%
  ggplot(aes(x = rownames(importance), 
             y = Importance)) +
  geom_bar(stat = "identity") +
  ylab("Importance") + xlab("Variable") +theme_bw() + coord_flip()



"
Recall this is a classification problem with classes 0 and 1. 
Notice that the coefficients are both positive and negative. 
The positive scores indicate a feature that predicts class 1, 
whereas the negative scores indicate a feature that predicts class 0.
"




### Cluster Analysis

require(NbClust)
require(factoextra)
require(flexclust)

## Scale Data
cluster_df <- cancer_df %>% select(-diagnosis) %>% 
  scale()

"
The NbClust package offers numerous indices for determining the best number of clusters
in a cluster analysis. There is no guarantee that they will agree with each other. In fact, they
probably won’t. But the results can be used as a guide for selecting possible candidate values
for K, the number of clusters. 
"


fviz_nbclust(cluster_df, kmeans, method = 'wss', k.max = 10)
fviz_nbclust(cluster_df, kmeans, method = 'silhouette') 
nc <- NbClust(cluster_df, min.nc=2, max.nc=10, method="kmeans", index = 'alllong') 
fviz_nbclust(nc)

set.seed(1)
fit.km <- kmeans(cluster_df, 2, nstart=50)
fit.km$size
fit.km$centers
aggregate(cancer_df %>% select(-diagnosis), by=list(cluster=fit.km$cluster), mean)


fviz_cluster(fit.km, data=cluster_df)

clusters <- data.frame(cluster = ifelse(fit.km$cluster == 1, "Cluster 1", "Cluster 2"))
clusters$category <- cancer_df$diagnosis

cluster_matrix <- table(clusters$cluster, clusters$category)

ggplot(data = reshape2::melt(cluster_matrix), aes(x = as.factor(Var2), y = value)) + geom_bar(stat = 'identity') +
  ylab("Frequency") + xlab("Category") + theme_bw() + facet_grid(.~ Var1)

randIndex(cluster_matrix)

means <- as.data.frame(fit.km$centers) 
means$cluster <- 1:nrow(means) 
plotdata <- gather(means, key="variable", value="value", -cluster) 
ggplot(plotdata, 
       aes(x=variable,
           y=value,
           fill=variable,
           group=cluster)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0),
        legend.position="none") +
  labs(x="", y="Standardized scores",
       title = "Mean Cluster Profiles")


## What if cluster was 3?
set.seed(1)
fit.km2 <- kmeans(cluster_df, 3, nstart=50)
fit.km2$size


clusters <- c()
for (j  in fit.km2$cluster) {
  if (j == 1){
    clusters <- c(clusters, "Cluster 1")
  }else if (j == 2){
    clusters <- c(clusters, "Cluster 2")
  }else{
    clusters <- c(clusters, "Cluster 3")
  }
}

cluster_df <- data.frame(cluster = clusters, category = cancer_df$diagnosis)
cluster_matrix <- table(cluster_df$cluster, cluster_df$category)

ggplot(data = reshape2::melt(cluster_matrix), aes(x = as.factor(Var2), y = value)) + geom_bar(stat = 'identity') +
  ylab("Frequency") + xlab("Category") + theme_bw() + facet_grid(.~ Var1)




means <- as.data.frame(fit.km2$centers) 
means$cluster <- 1:nrow(means) 
plotdata <- gather(means, key="variable", value="value", -cluster) 
ggplot(plotdata, 
       aes(x=variable,
           y=value,
           fill=variable,
           group=cluster)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0),
        legend.position="none") +
  labs(x="", y="Standardized scores",
       title = "Mean Cluster Profiles")

