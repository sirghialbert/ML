install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("e1071")
install.packages("cluster")
install.packages("factoextra")
install.packages("readxl")
install.packages("readr")
install.packages("caTools")
install.packages("ROCR")
install.packages("nnet")
install.packages("reshape2")
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(e1071)
library(cluster)
library(factoextra)
library(readxl)
library(readr)
library(ROCR)
library(nnet)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readxl)
library(FNN)
library(NeuralNetTools)
library(caTools)
df <- read.csv("heart.csv")



str(df)
cat("The shape of the dataset is :", dim(df), "\n")
categorical_cols <-
  c('sex', 'exng', 'caa', 'cp', 'fbs', 'restecg', 'slp', 'thall')


for (col in categorical_cols) {
  df[[col]] <- factor(df[[col]])
}

categorical_cols <-
  c('sex', 'exng', 'caa', 'cp', 'fbs', 'restecg', 'slp', 'thall')
continuous_cols <- c('age', 'trtbps', 'chol', 'thalachh', 'oldpeak')
target_var <- 'output'


unique_counts <-
  sapply(df[categorical_cols], function(x)
    length(unique(x)))
unique_counts_df <- as.data.frame(t(unique_counts))
colnames(unique_counts_df) <- "unique count"


continuous_summary <- summary(df[continuous_cols])


list(unique_counts_df = unique_counts_df,
     continuous_summary = continuous_summary)



continuous_summary <- summary(df[continuous_cols])


continuous_summary_transposed <- t(continuous_summary)


continuous_summary_transposed



missing_counts <- colSums(is.na(df))


missing_counts











categorical_cols <-
  c('sex', 'exng', 'caa', 'cp', 'fbs', 'restecg', 'slp', 'thall')


color_palette <-
  c("#800000", "#8000ff", "#6aac90", "#5833ff", "#da8829")


plot_list <- list()


for (col in categorical_cols) {
  plot <- ggplot(df, aes_string(x = col, fill = col)) +
    geom_bar(color = "black") +
    scale_fill_manual(values = color_palette) +
    labs(x = "", y = "Count", fill = col) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle(paste("Count plot for", col))
  
  
  plot_list[[col]] <- plot
}


multiplot <- grid.arrange(grobs = plot_list, nrow = 2)


multiplot




df_numeric <- df
df_numeric[] <-
  lapply(df_numeric, function(x)
    as.numeric(as.character(x)))
df_numeric <- df_numeric[complete.cases(df_numeric),]









corr_mat <- as.data.frame(as.table(cor(df_numeric)))


names(corr_mat) <- c("feature1", "feature2", "correlation")


heatmap_plot <-
  ggplot(corr_mat, aes(x = feature1, y = feature2, color = correlation)) +
  geom_point(size = 3) +
  scale_color_gradient2(
    low = "blue",
    high = "yellow",
    mid = "white",
    midpoint = 0,
    limits = c(-1, 1),
    space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  labs(x = "Features on X", y = "Features on Y",
       title = "Scatterplot Heatmap")


print(heatmap_plot)










plot1 <-     ggplot(df, aes(x = cp, fill = as.factor(output))) +
  geom_bar(position = "fill") +
  labs(title = "Chest Pain Distribution",
       x = "Chest Pain Type",
       y = "Proportion",
       fill = "Output") +
  scale_x_discrete(labels = c(
    "Typical Angina",
    "Atypical Angina",
    "Non-anginal Pain",
    "Asymptomatic"
  )) +
  theme_minimal()


plot2 <-
  ggplot(df, aes(x = as.factor(caa), fill = as.factor(output))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Number of Major Vessels",
       x = "Number of Major Vessels",
       y = "Proportion",
       fill = "Output") +
  theme_minimal()


plot3 <-
  ggplot(df, aes(x = factor(sex), fill = factor(output))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack according to sex") +
  theme_minimal()


plot4 <-
  ggplot(df, aes(x = as.factor(thall), fill = as.factor(output))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Thallium Test Results",
       x = "Thallium Test Result",
       y = "Proportion",
       fill = "Output") +
  theme_minimal()


plot5 <-
  ggplot(df, aes(
    x = as.factor(output),
    y = thalachh,
    fill = as.factor(output)
  )) +
  geom_boxplot() +
  labs(title = "Boxen Plot of Thalachh with respect to Outcome",
       x = "Outcome",
       y = "Thalachh",
       fill = "Outcome") +
  theme_minimal()


plot6 <-
  ggplot(df, aes(
    x = factor(exng),
    y = age,
    color = factor(output)
  )) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(title = "Strip Plot of exng vs age") +
  theme_minimal()

plot(plot1)
plot(plot2)
plot(plot3)
plot(plot4)
plot(plot5)
plot(plot6)

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6)





heart <- df

heart_numeric <- heart[, sapply(heart, is.numeric)]
result <-
  cmeans(
    heart_numeric,
    centers = 2,
    iter.max = 100,
    m = 2,
    method = "cmeans"
  )


print(result)


ggplot(data = heart, aes(
  x = age,
  y = thalachh,
  color = as.factor(result$cluster)
)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Fuzzy Clustering of Heart Dataset", x = "Age", y = "Max Heart Rate (thalachh)") +
  theme_minimal()


o <- order(result$cluster)
data.frame(heart[o,], Cluster = result$cluster[o])


head(result$membership, 3)


clustered_heart <- data.frame(heart, Cluster = result$cluster)

heart_scaled <- scale(heart_numeric)


res.fanny <- fanny(heart_scaled, 3)


data.frame(Cluster = res.fanny$clustering, heart)


fviz_cluster(
  res.fanny,
  ellipse.type = "norm",
  repel = TRUE,
  palette = "jco",
  ggtheme = theme_minimal(),
  legend = "right"
)


fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())


print(res.fanny$silinfo)








colnames(heart)







heart <- read_csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)


heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))
heart_encoded%>% head()

heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)

heart_processed$output <- heart$output


??sample.split
split <- sample.split(heart_processed$output, SplitRatio = 0.75)
training <- subset(heart_processed, split == TRUE)
testing <- subset(heart_processed, split == FALSE)

dim(training)
dim(testing)
logistic_model <-
  glm(output ~ ., data = training, family = binomial)


summary(logistic_model)

prob <-
  predict(logistic_model, newdata = testing, type = "response")


pred <- ifelse(prob > 0.5, 1, 0)


conf_matrix<-confusionMatrix(table(pred,testing$output))

conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy



logistic_model <-
  glm(
    output ~ cp.0 + sex.0 + slp.1 + caa.2 + thall.2 + thall.1 + exng.1,
    data = training,
    family = binomial
  )


summary(logistic_model)


prob <-
  predict(logistic_model, newdata = testing, type = "response")


pred <- ifelse(prob > 0.5, 1, 0)

conf_matrix<-confusionMatrix(table(pred,testing$output))
conf_matrix
conf_matrix <- table(Predicted = pred, Actual = testing$output)
conf_matrix


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
sensitivity
specificity


p <-
  predict(logistic_model, newdata = testing, type = "response")
pr <- prediction(p, testing$output)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")


roc_data <- data.frame(FPR = prf@x.values[[1]],
                       TPR = prf@y.values[[1]])


auc <- performance(pr, measure = "auc")
auc_value <- auc@y.values[[1]]


ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  theme_minimal() +
  labs(
    title = "ROC Curve",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    caption = paste("AUC =", round(auc_value, 4))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 15)
  )









set.seed(1)

###ARBORE DE CLASIFICARE
heart <- read.csv("heart.csv")
heart <- na.omit(heart)


heart$output <- as.factor(heart$output)



split <-
  createDataPartition(heart$output, p = 0.75, list = FALSE)
train <- heart[split, ]
test <- heart[-split, ]


grid <-
  expand.grid(cp = seq(0.0, 0.1, by = 0.01))


control <-
  trainControl(method = "cv", number = 5)
model <-
  train(
    output ~ .,
    data = train,
    method = "rpart",
    trControl = control,
    tuneGrid = grid
  )


plot(model)


best_cp <- model$bestTune$cp
best_cp


final_model <-
  rpart(
    output ~ .,
    data = train,
    method = "class",
    control = rpart.control(cp = best_cp)
  )


prp(final_model, extra = 1)


predictions <-
  predict(final_model, newdata = test, type = "class")


accuracy <- mean(predictions == test$output)
cat("Accuracy:", accuracy, "\n")


confusionMatrix <- confusionMatrix(predictions, test$output)
confusionMatrix
rpart.plot(final_model)


heatmap_plot


head(heart)














cor(cbind(heart$oldpeak,heart$output,heart$chol))



heart <- read.csv("heart.csv")
str(heart)


heart_scaled <-
  as.data.frame(scale(heart))




split <-
  createDataPartition(heart_scaled$oldpeak, p = 0.7, list = FALSE)
heart_train <- heart_scaled[split, ]
heart_test <- heart_scaled[-split, ]
head(heart_train)

tree_model_oldpeak <-
  rpart(oldpeak ~ slp, data = heart_train, method = "anova")


rpart.plot(tree_model_oldpeak, main = "Arbore de regresie initial pentru 'oldpeak' în functie de 'output' si 'slp'")



param_grid_oldpeak <- expand.grid(cp = seq(0.0, 0.1, by = 0.01))
cv <- trainControl(method = "cv",
                   number = 5,
                   verboseIter = TRUE)


grid_search_oldpeak <-
  train(
    oldpeak ~ slp + thall,
    data = heart_train,
    method = "rpart",
    trControl = cv,
    tuneGrid = param_grid_oldpeak
  )


best_cp_oldpeak <- grid_search_oldpeak$bestTune$cp
best_cp_oldpeak


tree_model_oldpeak_pruned <-
  rpart(
    oldpeak ~ output + slp,
    data = heart_train,
    method = "anova",
    control = rpart.control(cp = best_cp_oldpeak)
  )


predictions_oldpeak <-
  predict(tree_model_oldpeak_pruned, newdata = heart_test)


mse_oldpeak <-
  mean((predictions_oldpeak - heart_test$oldpeak) ^ 2)
mse_oldpeak

plot(
  y=heart_test$oldpeak,
  x=predictions_oldpeak,
  main = "Actual vs Predicted 'oldpeak'",
  xlab = "Actual oldpeak",
  ylab = "Predicted oldpeak",
  col = "blue"
)


abline(0, 1, col = "red")

rmse <-
  sqrt(mean((predictions_oldpeak - heart_test$oldpeak) ^ 2))
rmse









rpart.plot(tree_model_oldpeak_pruned, main = "Arbore de regresie pruned pentru 'oldpeak' în functie de 'output' si 'slp'")









set.seed(9902)


heart <- read.csv("heart.csv")


heart_numeric <- heart[, sapply(heart, is.numeric)]
heart_numeric <- na.omit(heart_numeric)


heart_numeric$output <- as.factor(heart_numeric$output)


split <-
  createDataPartition(heart_numeric$output, p = 0.7, list = FALSE)
train <- heart_numeric[split, ]
test <- heart_numeric[-split, ]


x_train <- train[, -which(names(train) == "output")]
y_train <- train$output
x_test <- test[, -which(names(test) == "output")]
y_test <- test$output


preProcValues <-
  preProcess(x_train, method = c("center", "scale"))
x_train <- predict(preProcValues, x_train)
x_test <- predict(preProcValues, x_test)


k_values <- seq(1, 20, by = 1)

accuracy_values <- sapply(k_values, function(k) {
  predictions <-
    knn(
      train = x_train,
      test = x_test,
      cl = y_train,
      k = k
    )
  confusion_matrix <- confusionMatrix(predictions, y_test)
  accuracy <- confusion_matrix$overall['Accuracy']
  return(accuracy)
})

heatmap_plot
best_k <- k_values[which.max(accuracy_values)]
cat("Cel mai bun k:", best_k, "\n")


calculate_accuracy <- function(k, x_train, y_train, x_test, y_test) {
  model <- knn(train = x_train, test = x_test, cl = y_train, k = k)
  mean(model == y_test)
}


accuracy_values <- sapply(k_values, calculate_accuracy, x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test)


accuracy_df <- data.frame(k = k_values, accuracy = accuracy_values)


ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Acuratețea Modelului KNN în Funcție de k",
       x = "Numărul de Vecini (k)",
       y = "Acuratețe") +
  theme_minimal()


confusion_matrix <- confusionMatrix(model, y_test)
cat("Matricea de confuzie:\n")
print(confusion_matrix)
cat("Acuratete:", confusion_matrix$overall['Accuracy'], "\n")


conf_matrix <- as.data.frame(confusion_matrix$table)


conf_matrix_melted <- melt(conf_matrix)

ggplot(data = conf_matrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ggtitle("Matricea de confuzie") +
  xlab("Valori prezise") +
  ylab("Valori reale")


accuracy_df <-
  data.frame(k = k_values, accuracy = accuracy_values)

ggplot(data = accuracy_df, aes(x = k, y = accuracy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Acuratetea pentru diferite valori k") +
  xlab("Valoarea k") +
  ylab("Acuratete") +
  scale_x_continuous(breaks = k_values)


heatmap_plot 


















set.seed(123)
heatmap_plot


data <- read.csv("heart.csv")

heart <- read.csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)


heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)




split <-
  createDataPartition(heart_processed$output, p = 0.7, list = FALSE)
train_data <- heart_processed[split, ]
test_data <- heart_processed[-split, ]
str(train_data)
train_data$output <- as.numeric(train_data$output)
test_data$output <- as.numeric(test_data$output)

k_values <- seq(1, 20)
rmse_values <- numeric(length(k_values))

for (i in k_values) {
  knn_pred <- knn(
    train = train_data[,-which(names(train_data) == "thalachh")],
    test = test_data[,-which(names(test_data) == "thalachh")],
    cl = train_data$thalachh,
    k = i
  )
  rmse_values[i] <-
    RMSE(as.numeric(as.character(knn_pred)), test_data$thalachh)
}


plot_df <- data.frame(k = k_values, RMSE = rmse_values)
ggplot(plot_df, aes(x = k, y = RMSE)) +
  geom_point() +
  geom_line() +
  labs(title = "RMSE pentru diferite valori ale lui k", x = "Valoarea lui k", y = "RMSE")


best_k <- k_values[which.min(rmse_values)]
final_knn_pred <- knn(
  train = train_data[,-which(names(train_data) == "thalachh")],
  test = test_data[,-which(names(test_data) == "thalachh")],
  cl = train_data$thalachh,
  k = best_k
)


final_rmse <-
  RMSE(as.numeric(as.character(final_knn_pred)), test_data$thalachh)
cat("Cel mai bun k:", best_k, "\n")
cat("RMSE pentru cel mai bun k:", final_rmse, "\n")


comparison_df <-
  data.frame(Real = test_data$thalachh,
             Predicted = as.numeric(as.character(final_knn_pred)))
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")





















set.seed(9902)


data <- read.csv("heart.csv")

heart <- read.csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)


heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)


(88)


split <- sample.split(heart_processed$output, SplitRatio = 0.75)
training <- subset(heart_processed, split == TRUE)
testing <- subset(heart_processed, split == FALSE)





model <- rpart(thalachh ~ ., data = training, method = "anova")


rpart.plot(model, main = "Arbore de regresie pentru estimarea thalachh")


predictions <- predict(model, testing)


rmse <- RMSE(predictions, testing$thalachh)
cat("RMSE al modelului de arbore de regresie:", rmse, "\n")


comparison_df <-
  data.frame(Real = testing$thalachh, Predicted = predictions)
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")


cp_values <- seq(0.001, 0.05, by = 0.001)
rmse_values <- numeric(length(cp_values))

for (i in 1:length(cp_values)) {
  model <-
    rpart(
      thalachh ~ .,
      data = training,
      method = "anova",
      control = rpart.control(cp = cp_values[i])
    )
  predictions <- predict(model, testing)
  rmse_values[i] <- RMSE(predictions, testing$thalachh)
}


plot_df <- data.frame(cp = cp_values, RMSE = rmse_values)
ggplot(plot_df, aes(x = cp, y = RMSE)) +
  geom_point() +
  geom_line() +
  labs(title = "RMSE pentru diferite valori ale parametrului cp", x = "Valoarea lui cp", y = "RMSE")


best_cp <- cp_values[which.min(rmse_values)]
final_model <-
  rpart(
    thalachh ~ .,
    data = training,
    method = "anova",
    control = rpart.control(cp = best_cp)
  )
rpart.plot(final_model, main = "Arbore de regresie final pentru estimarea thalachh")


final_predictions <- predict(final_model, testing)


final_rmse <- RMSE(final_predictions, testing$thalachh)
cat("Cel mai bun cp:", best_cp, "\n")
cat("RMSE pentru cel mai bun cp:", final_rmse, "\n")


final_comparison_df <-
  data.frame(Real = testing$thalachh, Predicted = final_predictions)
ggplot(final_comparison_df, aes(x = Real, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise (Model final)", x = "Valori reale", y = "Valori prezise")






















heart <- read_csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)


heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)
heart_processed$slp <- heart_numeric$slp
heart_processed <- heart_processed[, -19]
heart_processed <- heart_processed[, -19]
head(heart_processed)



split <- sample.split(heart_processed$output, SplitRatio = 0.75)
train_data <- subset(heart_processed, split == TRUE)
test_data <- subset(heart_processed, split == FALSE)
train_data$slp <- as.factor(train_data$slp)
test_data$slp <- as.factor(test_data$slp)

model <- multinom(slp ~ ., data = train_data)

summary(model)

head(training)
predictions <- predict(model, test_data)


conf_matrix <- confusionMatrix(predictions, test_data$slp)
cat("Matricea de confuzie:\n")
print(conf_matrix)


accuracy <- conf_matrix$overall['Accuracy']
cat("Acuratetea modelului de regresie multinomiala:", accuracy, "\n")


comparison_df <-
  data.frame(Real = test_data$slp, Predicted = predictions)
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")


test_data_prop <- prop.table(table(test_data$slp))
cat("Proportiile categoriilor în datele de testare:\n")
print(test_data_prop)


predictions_prop <- prop.table(table(predictions))
cat("Proportiile categoriilor în predictii:\n")
print(predictions_prop)


comparison_df <- data.frame(
  Category = levels(test_data$slp),
  Real = as.numeric(table(test_data$slp)),
  Predicted = as.numeric(table(predictions))
)

comparison_df <-
  melt(
    comparison_df,
    id.vars = "Category",
    variable.name = "Type",
    value.name = "Count"
  )

ggplot(comparison_df, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distributia valorilor reale si prezise", x = "Categorie", y = "Numar")





















data <- read.csv("heart.csv")

heart <- read_csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)



heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)


split <- sample.split(heart_processed$output, SplitRatio = 0.75)
train_data <- subset(heart_processed, split == TRUE)
test_data <- subset(heart_processed, split == FALSE)
train_data$output <- as.factor(train_data$output)
test_data$output <- as.factor(test_data$output)

tune_grid <-
  expand.grid(
    size = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    decay = c(0.01, 0.1, 0.5)
  )


train_control <- trainControl(method = "cv", number = 5)



nn_model <-
  train(
    output ~ .,
    data = train_data,
    method = "nnet",
    tuneGrid = tune_grid,
    trControl = train_control,
    linout = FALSE,
    trace = FALSE
  )


print(nn_model)


best_params <- nn_model$bestTune
cat("Cei mai buni parametri:\n")
print(best_params)
plotnet(nn_model, bias = TRUE)


predictions <- predict(nn_model, test_data)
predictions

table(test_data$output, predictions)

conf_matrix <- confusionMatrix(predictions, test_data$output)
cat("Matricea de confuzie:\n")
print(conf_matrix)


accuracy <- conf_matrix$overall['Accuracy']
cat("Acuratetea modelului de retea neuronala:", accuracy, "\n")


comparison_df <-
  data.frame(Real = test_data$output, Predicted = predictions)
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")


comparison_df <- data.frame(
  Category = levels(test_data$output),
  Real = as.numeric(table(test_data$output)),
  Predicted = as.numeric(table(predictions))
)

comparison_df <-
  melt(
    comparison_df,
    id.vars = "Category",
    variable.name = "Type",
    value.name = "Count"
  )

ggplot(comparison_df, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distributia valorilor reale si prezise", x = "Categorie", y = "Numar")

























set.seed(1234)




heart <- read_csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)



heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)


split <- sample.split(heart_processed$output, SplitRatio = 0.75)
train_data <- subset(heart_processed, split == TRUE)
test_data <- subset(heart_processed, split == FALSE)
train_data$output <- as.factor(train_data$output)
test_data$output <- as.factor(test_data$output)

cv <-
  trainControl(method = "cv",
               number = 5,
               verboseIter = TRUE)


tune_grid <- expand.grid(C = c(0.1, 0.001,0.005,0.01,0.05,0.5, 1),
                         sigma = c(0.001, 0.01, 0.1))



svm_model <-
  train(
    output ~ .,
    data = train_data,
    method = "svmRadial",
    tuneGrid = tune_grid,
    trControl = cv,
  )


print(svm_model)


best_params <- svm_model$bestTune
cat("Cei mai buni parametri:\n")
print(best_params)


predictions <- predict(svm_model, test_data)


conf_matrix <- confusionMatrix(predictions, test_data$output)
cat("Matricea de confuzie:\n")
print(conf_matrix)


accuracy <- conf_matrix$overall['Accuracy']
cat("Acuratetea modelului SVM:", accuracy, "\n")


comparison_df <-
  data.frame(Real = test_data$output, Predicted = predictions)
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")


comparison_df <- data.frame(
  Category = levels(test_data$output),
  Real = as.numeric(table(test_data$output)),
  Predicted = as.numeric(table(predictions))
)

comparison_df <-
  melt(
    comparison_df,
    id.vars = "Category",
    variable.name = "Type",
    value.name = "Count"
  )

ggplot(comparison_df, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distributia valorilor reale si prezise", x = "Categorie", y = "Numar")





















set.seed(1234)


heart <- read_csv("heart.csv")


categorical_vars <-
  c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
numerical_vars <-
  c("age", "trtbps", "chol", "thalachh", "oldpeak")


heart[categorical_vars] <-
  lapply(heart[categorical_vars], as.factor)



heart_encoded <- dummyVars(" ~ .", data = heart)
heart_encoded <-
  data.frame(predict(heart_encoded, newdata = heart))


heart_scaled <-
  as.data.frame(scale(heart_encoded[numerical_vars]))


heart_processed <-
  cbind(heart_scaled, heart_encoded[setdiff(names(heart_encoded), numerical_vars)])
head(heart_processed)


split <- sample.split(heart_processed$output, SplitRatio = 0.75)
train_data <- subset(heart_processed, split == TRUE)
test_data <- subset(heart_processed, split == FALSE)
train_data$output <- as.factor(train_data$output)
test_data$output <- as.factor(test_data$output)



rf_model <- train(
  output ~ .,
  data = train_data,
  method = "rf",
  trControl = cv,
  preProcess = c("center", "scale")
)


print(rf_model)


predictions <- predict(rf_model, test_data)


conf_matrix <- confusionMatrix(predictions, test_data$output)
cat("Matricea de confuzie:\n")
print(conf_matrix)


accuracy <- conf_matrix$overall['Accuracy']
cat("Acuratetea modelului Random Forest:", accuracy, "\n")


comparison_df <-
  data.frame(Real = test_data$output, Predicted = predictions)
ggplot(comparison_df, aes(x = Real, y = Predicted)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  labs(title = "Comparatie între valorile reale si cele prezise", x = "Valori reale", y = "Valori prezise")


comparison_df <- data.frame(
  Category = levels(test_data$output),
  Real = as.numeric(table(test_data$output)),
  Predicted = as.numeric(table(predictions))
)

comparison_df <-
  melt(
    comparison_df,
    id.vars = "Category",
    variable.name = "Type",
    value.name = "Count"
  )

ggplot(comparison_df, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distributia valorilor reale si prezise", x = "Categorie", y = "Numar")
dim(training)
