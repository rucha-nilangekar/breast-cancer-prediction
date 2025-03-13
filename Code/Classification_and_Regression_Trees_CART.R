
# FUNCTIONS
sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}


rm(list=ls()); gc()
library(rpart); library(rpart.plot)

#Read in the data
dat = read.csv('wdbc_preped_scaled.csv', stringsAsFactors = T, head=T)
K = nrow(dat)


# Classification Tree with rpart
#A smaller cp leads to a more complex tree.
fit = rpart(Diagnosis ~ ., method="class", data=dat, minsplit=5 , cp = 0.001) # same as using all other variables as predictors



########################
### BEST PRUNED ###
########################

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
#pfit.bp = prune(fit, cp = 0.05)
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

## How to predict? I am taking best pruned tree as an example.
yhat = predict(pfit.bp, dat, type = "class") # replace "dat" by validation data if you have it
err.bp = mean(yhat != dat$Diagnosis)

# if you want to use a cutoff not equal to 0.5
prob1 = predict(pfit.bp, dat, type = "prob")[,2]
pred.class = as.numeric(prob1 > .8)
#ytest = dat$Diagnosis # Be careful! Check the variable type of your outcome
#err.bp.newCut = mean(pred.class != ytest)
#err.bp.newCut

########################

# Set a sequence of cutoff values
cutoff_values <- seq(0.1, 0.9, by = 0.1)

# Initialize vectors to store results
err_vec <- numeric(length(cutoff_values))
sen_vec <- numeric(length(cutoff_values))
spe_vec <- numeric(length(cutoff_values))
fpr_vec <- numeric(length(cutoff_values))
fnr_vec <- numeric(length(cutoff_values))

# Loop over cutoff values for the 
for (i in seq_along(cutoff_values)) {
  # Obtain predictions using the current cutoff
  pred.class <- as.numeric(prob1 > cutoff_values[i])
  
  # Calculate error and performance metrics
  err_vec[i] <- mean(pred.class != ytest)
  sen_vec[i] <- sen(ytest, pred.class)
  spe_vec[i] <- spe(ytest, pred.class)
  fpr_vec[i] <- fpr(ytest, pred.class)
  fnr_vec[i] <- fnr(ytest, pred.class)
}

# Create a data frame
result_df <- data.frame(
  Cutoff = cutoff_values,
  Error = err_vec,
  Sensitivity = sen_vec,
  Specificity = spe_vec,
  FPR = fpr_vec,
  FNR = fnr_vec
)

# Print the data frame
print(result_df)

#############################

# Create a line chart for Error with different cutoff values
plot(result_df$Cutoff, result_df$Error, type = "l", col = "blue", 
     main = "Error vs. Cutoff Values", xlab = "Cutoff Values", ylab = "Error")
#############################




########################
###### MIN ERROR TREE ###### 
########################

# Minimum Error Tree Model
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')


## Min Error tree Prediction
yhat_me = predict(pfit.me, dat, type = "class") # replace "dat" by validation data if you have it
err.me = mean(yhat_me != dat$Diagnosis)

# if you want to use a cutoff not equal to 0.5 (Min error)
prob2 = predict(pfit.me, dat, type = "prob")[,2]
pred.class = as.numeric(prob1 > .4)
ytest_2 = as.numeric(dat$Diagnosis)
err.me.newCut = mean(pred.class != ytest)

########################

# Set a sequence of cutoff values
cutoff_values <- seq(0.1, 0.9, by = 0.1)

# Initialize vectors to store results
err_vec <- numeric(length(cutoff_values))
sen_vec <- numeric(length(cutoff_values))
spe_vec <- numeric(length(cutoff_values))
fpr_vec <- numeric(length(cutoff_values))
fnr_vec <- numeric(length(cutoff_values))

# Loop over cutoff values for the 
for (i in seq_along(cutoff_values)) {
  # Obtain predictions using the current cutoff
  pred.class <- as.numeric(prob2 > cutoff_values[i])
  
  # Calculate error and performance metrics
  err_vec[i] <- mean(pred.class != ytest_2)
  sen_vec[i] <- sen(ytest_2, pred.class)
  spe_vec[i] <- spe(ytest_2, pred.class)
  fpr_vec[i] <- fpr(ytest_2, pred.class)
  fnr_vec[i] <- fnr(ytest_2, pred.class)
}

# Create a data frame
result_df <- data.frame(
  Cutoff = cutoff_values,
  Error = err_vec,
  Sensitivity = sen_vec,
  Specificity = spe_vec,
  FPR = fpr_vec,
  FNR = fnr_vec
)

# Print the data frame
print(result_df)

#############################

# Create a line chart for Error with different cutoff values
plot(result_df$Cutoff, result_df$Error, type = "l", col = "blue", 
     main = "Error vs. Cutoff Values", xlab = "Cutoff Values", ylab = "Error")
#############################