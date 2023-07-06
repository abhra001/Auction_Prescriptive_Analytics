setwd("C:/Users/HP/Downloads/ML_Trial")

list.of.packages <- c("glmnet", "rpart", "rpart.plot", "randomForest", "devtools", "tidyverse", "knitr", "SuperLearner", "caret", "xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

install_github('susanathey/causalTree')
library(causalTree)

select <- dplyr::select

set.seed(1)

my_data = read.csv("output.csv", stringsAsFactors = FALSE)
colnames(my_data)
my_data$bidfee<-ifelse(my_data$bidfee== 75,1,0)

#my_data <- my_data[60000:80000,]

#X1 <- my_data$bidfee
#Y1 <- my_data$placedbids
#W1 <- my_data %>% select( retail, bidincrement, flg_click_only, flg_beginnerauction, flg_fixedprice, flg_endprice)

my_data <- my_data[40000:100000,]
folds = createFolds(1:nrow(my_data), k=2)
colnames(my_data)

Y1 <- my_data[folds[[1]],11]
Y2 <- my_data[folds[[2]],11]

X1 <- my_data[folds[[1]],9]
X2 <- my_data[folds[[2]],9]

W1 <- my_data[folds[[1]],]
W1 <- W1 %>% select( retail, bidincrement, flg_click_only, flg_beginnerauction, flg_fixedprice, flg_endprice)
W2 <- my_data[folds[[2]],]
W2 <- W2 %>% select( retail, bidincrement, flg_click_only, flg_beginnerauction, flg_fixedprice, flg_endprice)

sl_lm = SuperLearner(Y = Y1, 
                     X = data.frame(X=X1, W1, W1*X1), 
                     family = binomial(), 
                     SL.library = "SL.lm", 
                     cvControl = list(V=0))

summary(sl_lm$fitLibrary$SL.lm_All$object)


#Trees
tree_fml <- as.formula(paste("Y", paste(names(W1), collapse = ' + '), sep = " ~ "))
tree_fml

causal_tree <- causalTree(formula = tree_fml,
                          data = data.frame(Y=Y1, W1),
                          treatment = X1,
                          split.Rule = "CT", #causal tree
                          split.Honest = F, #will talk about this next
                          split.alpha = 1, #will talk about this next
                          cv.option = "CT",
                          cv.Honest = F,
                          split.Bucket = T, #each bucket contains bucketNum treated and bucketNum control units
                          bucketNum = 5, 
                          bucketMax = 100, 
                          minsize = 250) # number of observations in treatment and control on leaf

rpart.plot(causal_tree, roundint = F)
tree_fml

#Honest Tress

honest_tree <- honest.causalTree(formula = tree_fml,
                                 data = data.frame(Y=Y1, W1),
                                 treatment = X1,
                                 est_data = data.frame(Y=Y2, W2),
                                 est_treatment = X2,
                                 split.alpha = 0.5,
                                 split.Rule = "CT",
                                 split.Honest = T,
                                 cv.alpha = 0.5,
                                 cv.option = "CT",
                                 cv.Honest = T,
                                 split.Bucket = T,
                                 bucketNum = 5,
                                 bucketMax = 100, # maximum number of buckets
                                 minsize = 250) # number of observations in treatment and control on leaf

rpart.plot(honest_tree, roundint = F)

opcpid <- which.min(honest_tree$cp[, 4]) 
opcp <- honest_tree$cp[opcpid, 1]
honest_tree_prune <- prune(honest_tree, cp = opcp)

rpart.plot(honest_tree_prune, roundint = F)

leaf2 <- as.factor(round(predict(honest_tree_prune,
                                 newdata = data.frame(Y=Y2, W2),
                                 type = "vector"), 4))

# Run linear regression that estimate the treatment effect magnitudes and standard errors
honest_ols_2 <- lm( Y ~ leaf + X * leaf - X -1, data = data.frame(Y=Y2, X=X2, leaf=leaf2, W2))

summary(honest_ols_2)

cate_honesttree <- predict(honest_tree_prune, newdata = data.frame(Y=Y2, W2), type = "vector")

# Causal forests 
causalforest <- causalForest(tree_fml,
                             data=data.frame(Y=Y1, W1), 
                             treatment=X1, 
                             split.Rule="CT", 
                             split.Honest=T,  
                             split.Bucket=T, 
                             bucketNum = 5,
                             bucketMax = 100, 
                             cv.option="CT", 
                             cv.Honest=T, 
                             minsize = 2, 
                             split.alpha = 0.5, 
                             cv.alpha = 0.5,
                             sample.size.total = floor(nrow(Y1) / 2), 
                             sample.size.train.frac = .5,
                             mtry = ceiling(ncol(W1)/3), 
                             nodesize = 5, 
                             num.trees = 10, 
                             ncov_sample = ncol(W1), 
                             ncolx = ncol(W1))

cate_causalforest <- predict(causalforest, newdata = data.frame(Y=Y2, W2), type = "vector")

het_effects <- data.frame(causal_tree = cate_honesttree, 
                          causal_forest = cate_causalforest)

# Set range of the x-axis
xrange <- range( c(het_effects[, 1], het_effects[, 2]))

# Set the margins (two rows, three columns)
#par(mfrow = c(2, 4))

hist(het_effects[, 1], main = "Causal tree", xlim = xrange)
hist(het_effects[, 2], main = "Causal forest", xlim = xrange)


# Summary statistics
summary_stats <- do.call(data.frame, 
                         list(mean = apply(het_effects, 2, mean),
                              sd = apply(het_effects, 2, sd),
                              median = apply(het_effects, 2, median),
                              min = apply(het_effects, 2, min),
                              max = apply(het_effects, 2, max)))

summary_stats
