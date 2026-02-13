df_roc <- data.frame(ml_scores = c(0.01, 0.05, 0.13, 0.2, 0.6, 0.73, 0.8, 0.9, 0.95, 0.1, 0.3), 
                     ground_truth = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1))
par(mfrow = c(1, 2))

# ROC-curve via ROCit R package
library("ROCit")
my_roc<-rocit(df_roc$ml_scores,df_roc$ground_truth)
plot(my_roc$FPR, my_roc$TPR, col="blue", type="o", main = "ROC-curve via ROCit R package", 
     ylab="SENSITIVITY (TPR)", xlab="1-SPECIFISITY (FPR)", pch=19)
lines(c(0,1), c(0,1), col="red")


# ROC-curve from scratch in plain R
df_roc <- df_roc[order(df_roc$ml_scores), ]
fpr <- c(1, 1 - cumsum(df_roc$ground_truth == 0) / sum(df_roc$ground_truth == 0))
tpr <- c(1, 1 - cumsum(df_roc$ground_truth == 1) / sum(df_roc$ground_truth == 1))
plot(tpr ~ fpr, col="blue", main = "ROC-curve from scratch in plain R", 
     type="o", ylab="SENSITIVITY (TPR)", xlab="1-SPECIFISITY (FPR)", pch=19)
lines(c(0,1),c(0,1),col="red")
