## My Script ##

require(ggplot2)
require(scales)
require(reshape2)
require(corrplot)
require(glmnet)
require(caret)
# setwd("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Applied Data Analysis/A2/ADLA2")


udf_utils_MultiPlot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    
    #####################################################
    # Multiplot for ggplot2                             
    # REFERENCE: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/            
    #####################################################
    
    
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                         ncol = cols, nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

## Load the data set.
dt_ <- read.csv("Wine_training.csv")


# Let's look at the structure first.
str(dt_)
# Looks like all the features are numerics, and the target variable has two levels, good or bad.

# Let's look at the statistical summary.
summary(dt_)

udf_eda_MultBoxPlot <- function(numericDataFrame,ncol=11) {

    ## Description : This function plots ggplot boxplots with free scales for a numeric dataframe. 
    # Input : A dataframe with numeric values.
    # Output : ggplot2 object.

    require(reshape2)
    require(ggplot2)
    tmp__ <- numericDataFrame

    tmp__[, "idx"] <- row.names(tmp__)
    tmp__ <- melt(tmp__, id.vars = "idx")

    p_ <- ggplot(data = tmp__,aes(factor(1),value)) + geom_boxplot() + facet_wrap(~ variable,scales = "free",ncol = ncol) + theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())

    return(p_)
}


udf_eda_MultBoxPlot(dt_[,-ncol(dt_)])
# Seems like there is quite a large disparity of scales, e.g. SO2 has mean of 137, whilst Cl- of 0.04.


## Univariate Distributions.
# Let's investigate our target variable. Given that it's a factor we will look at it's proportion.

## Quality
ggplot(dt_,aes(dt_$quality)) + geom_bar() + scale_x_discrete(name = "Wine Quality") + scale_y_continuous(name = "Count",labels = scales::comma) + ggtitle("Proportion of Wine Quality In Training Set")



## Fixed acidity.
p1_ <- ggplot(dt_,aes(dt_$fixed.acidity)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_continuous(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Fixed Acidity", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Fixed Acidity Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$fixed.acidity)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Fixed Acidity") + ggtitle("Fixed Acidity Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

## Volatile Acidity
p1_ <- ggplot(dt_,aes(dt_$volatile.acidity)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Volatile Acidity", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Volatile Acidity Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$volatile.acidity)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Volatile Acidity") + ggtitle("Volatile Acidity Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)


## Citric Acid
p1_ <- ggplot(dt_,aes(dt_$citric.acid)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Citric Acid", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Citric Acid Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$citric.acid)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Citric Acid") + ggtitle("Citric Acid Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

## Residual Sugar
p1_ <- ggplot(dt_,aes(dt_$residual.sugar)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Residual Sugar", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Residual Sugar Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$residual.sugar)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Residual Sugar") + ggtitle("Residual Sugar Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

## Chlorides
p1_ <- ggplot(dt_,aes(dt_$chlorides)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Chlorides", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Chlorides Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$chlorides)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Chlorides") + ggtitle("Chlorides Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)


## Free Sulfur Dioxide
p1_ <- ggplot(dt_,aes(dt_$free.sulfur.dioxide)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Free Sulfur Dioxide", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Free Sulfur Dioxide Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$free.sulfur.dioxide)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Free Sulfur Dioxide") + ggtitle("Free Sulfur Dioxide Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)


## Total Sulfur Dioxide
p1_ <- ggplot(dt_,aes(dt_$chlorides)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Chlorides", labels = scales::comma) + scale_y_continuous(name = "Density") + ggtitle("Chlorides Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$chlorides)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Chlorides") + ggtitle("Chlorides Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

## Density
p1_ <- ggplot(dt_,aes(dt_$density)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Density", labels = scales::comma) + scale_y_continuous(name = "Observation Density") + ggtitle("Density Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$density)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Density") + ggtitle("Density Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

#pH 
p1_ <- ggplot(dt_,aes(dt_$pH)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "pH", labels = scales::comma) + scale_y_continuous(name = "pH") + ggtitle("pH Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$pH)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "pH") + ggtitle("pH Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

# Sulphates
p1_ <- ggplot(dt_,aes(dt_$sulphates)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Sulphates", labels = scales::comma) + scale_y_continuous(name = "Sulphates") + ggtitle("Sulphates Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$sulphates)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Sulphates") + ggtitle("Sulphates Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)

# Alcohol
p1_ <- ggplot(dt_,aes(dt_$alcohol)) + geom_histogram(bins = 50, colour = "black",aes(fill = ..count.., y = ..density..)) + geom_density() + scale_fill_gradient(low = "green",high = "red", name = "Count") + scale_x_continuous(name = "Alocohol", labels = scales::comma) + scale_y_continuous(name = "Alcohol") + ggtitle("Alcohol Histogram")



p2_ <- ggplot(dt_,aes(as.factor(""),dt_$alcohol)) + geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "Alcohol") + ggtitle("Alcohol Boxplot") + coord_flip()

udf_utils_MultiPlot(p1_,p2_,cols = 1)



## Multivariate distributions.
dt_Scaled <- scale(dt_[,-ncol(dt_)])
dt_Scaled <- as.data.frame(dt_Scaled)
dt_Scaled$id <- as.numeric(row.names(dt_Scaled))
dt_Scaled <- melt(dt_Scaled,id="id")

ggplot(dt_Scaled,aes(dt_Scaled$value,colour = dt_Scaled$variable)) + geom_density() + scale_colour_discrete(name = "Property") + scale_x_continuous(name = "Scaled Property Measure") + scale_y_continuous(name = "Observation Density") + ggtitle("Density of Dataset Properties")


# PCA
dt_pca <- scale(dt_[,-ncol(dt_)])
dt_pca <- prcomp(dt_pca)
summary(dt_pca)
# Sum of variances of the componetns (total variance explained by components)
sum((dt_pca$sdev)^2)
plot(dt_pca,type="lines",main = "PCA")

(dt_pca$sdev)^2
dt_pca <- as.data.frame(dt_pca$x[,1:4])

ggplot(dt_pca, aes(dt_pca$PC1, dt_pca$PC2)) + geom_point(aes(colour = dt_$quality)) + scale_colour_brewer(palette = "Set1", name = "Quality") + scale_y_continuous(name = "Priniciple Component 2") + scale_x_continuous(name = "Principle Component 1") + ggtitle("Wine Quality Labels in 2-Principle Components")

## Look at the histogram for good and bad with the principle component.
dt_pca <- cbind(dt_pca, dt_[, ncol(dt_)])
colnames(dt_pca)[ncol(dt_pca)] <- "Quality"


ggplot(dt_pca,aes(dt_pca$PC1)) + geom_density(aes(fill = dt_pca$Quality), alpha = 0.1) + scale_fill_brewer(palette = "Set1" , name = "Quality")  + scale_x_continuous(name = "Principle Component 1") + scale_y_continuous("Observation Density") + ggtitle("Distribution of Quality on Principle Component 1")

## Bivariate Visualisation
# Have a look at variable correlations.
corrplot(cor(dt_[,-ncol(dt_)]),method = "number")

## Fixed acidity
################

# fixed acidity histogram.
udf_eda_FeatTargetHist <- function(data=dt_,featName="fixed.acidity",targetName="quality",alpha = 0.5){
    require(ggplot2)
    
    camelize <- function(x) {
        
        x <- gsub("\\."," ",x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    
    tmp__ <- data[,c(featName,targetName)]
    
    plt_ <- ggplot(tmp__,aes(tmp__[,featName])) + geom_density(aes(fill = tmp__[,targetName]),alpha = alpha) + scale_fill_brewer(palette = "Set1", name = camelize(targetName)) + scale_x_continuous(name = camelize(featName)) + scale_y_continuous(name = "Observation Density") + ggtitle(paste0("Distribution of ",camelize(featName)," On ",camelize(targetName)))
    
    return(plt_)
}

udf_eda_FeatTargetBox <- function(data=dt_,featName="fixed.acidity",targetName="quality",alpha = 1){
    require(ggplot2)
    
    camelize <- function(x) {
        
        x <- gsub("\\."," ",x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    
    tmp__ <- data[,c(featName,targetName)]
    
    plt_ <- ggplot(tmp__,aes(tmp__[,targetName],tmp__[,featName])) + geom_boxplot(aes(fill = tmp__[,targetName]),alpha = alpha) + scale_x_discrete(name = camelize(targetName)) + scale_y_continuous(name = camelize(featName)) + scale_fill_brewer(palette = "Set1",name = camelize(targetName)) + ggtitle(paste0("Histogram of ",camelize(featName)," by ",camelize(targetName))) + coord_flip()
    
    return(plt_)
}

# udf_eda_FeatTargetHist(dt_,"fixed.acidity","quality")
udf_utils_MultiPlot(udf_eda_FeatTargetBox(dt_,"fixed.acidity","quality"),udf_eda_FeatTargetHist(dt_,"fixed.acidity","quality"),cols = 1)

udf_eda_CorrTargetScatter <- function(data=dt_,xData="fixed.acidity",yData="pH",targetName="quality",smooth=TRUE){
    
    require(ggplot2)
    
    camelize <- function(x) {
        
        x <- gsub("\\."," ",x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    
    tmp__ <- data[,c(xData,yData,targetName)]
    
    plt_ <- ggplot(tmp__,aes(tmp__[,xData],tmp__[,yData],colour = tmp__[,targetName])) + geom_point(shape = 16,size =2 , alpha = 0.5) + scale_color_brewer(palette = "Set1",name = camelize(targetName)) + scale_x_continuous(name = camelize(xData)) + scale_y_continuous(name = camelize(yData)) + ggtitle(paste0(camelize(xData)," ~ ",camelize(yData)," by ",camelize(targetName)))
    
    if(smooth){
        return(plt_ + geom_smooth(method="lm",se=FALSE))
    }
    else{
        return(plt_)
    }
}



udf_eda_CorrTargetScatter(dt_,"fixed.acidity","pH","quality",TRUE)


## Volatile acidity
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "volatile.acidity"),udf_eda_FeatTargetHist(featName = "volatile.acidity"))
udf_eda_CorrTargetScatter(xData = "volatile.acidity",yData = "citric.acid")


## Citric acid
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "citric.acid"),udf_eda_FeatTargetHist(featName = "citric.acid"))
udf_eda_CorrTargetScatter(xData = "citric.acid",yData = "fixed.acidity")


## Residual sugar
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "residual.sugar"),udf_eda_FeatTargetHist(featName = "residual.sugar"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "residual.sugar",yData = "density"),udf_eda_CorrTargetScatter(xData = "residual.sugar",yData = "total.sulfur.dioxide"),udf_eda_CorrTargetScatter(xData = "residual.sugar",yData = "alcohol"))


## Chlorides
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "chlorides"),udf_eda_FeatTargetHist(featName = "chlorides"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "chlorides",yData = "alcohol"),udf_eda_CorrTargetScatter(xData = "chlorides",yData = "density"))

## Free sulfur dioxide
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "free.sulfur.dioxide"),udf_eda_FeatTargetHist(featName = "free.sulfur.dioxide"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "free.sulfur.dioxide",yData = "total.sulfur.dioxide"),udf_eda_CorrTargetScatter(xData = "free.sulfur.dioxide",yData = "density"))

## Total sulfur dioxide
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "total.sulfur.dioxide"),udf_eda_FeatTargetHist(featName = "total.sulfur.dioxide"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "total.sulfur.dioxide",yData = "alcohol"),udf_eda_CorrTargetScatter(xData = "total.sulfur.dioxide",yData = "density"))


## Density
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "density"),udf_eda_FeatTargetHist(featName = "density"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "density",yData = "fixed.acidity"),udf_eda_CorrTargetScatter(xData = "density",yData = "free.sulfur.dioxide"))

## pH
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "pH"),udf_eda_FeatTargetHist(featName = "pH"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "pH",yData = "citric.acid"),udf_eda_CorrTargetScatter(xData = "pH",yData = "residual.sugar"))


## Sulphates
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "sulphates"),udf_eda_FeatTargetHist(featName = "sulphates"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "sulphates",yData = "total.sulfur.dioxide"),udf_eda_CorrTargetScatter(xData = "sulphates",yData = "pH"))


## Alcohol
udf_utils_MultiPlot(udf_eda_FeatTargetBox(featName = "alcohol"),udf_eda_FeatTargetHist(featName = "alcohol"))
udf_utils_MultiPlot(udf_eda_CorrTargetScatter(xData = "alcohol",yData = "chlorides"),udf_eda_CorrTargetScatter(xData = "alcohol",yData = "free.sulfur.dioxide"))


### modelling
#require(glmnet)

### Logistic regression
#lreg_ <- glm(quality ~ .,data = dt_, family = "binomial")

### Step function
#step_ <- stepAIC(lreg_,direction = "both")

#preds_ <- ROCR::prediction(predict(step_,dt_[,-ncol(dt_)],type = "response"),dt_$quality)

#roc_perf <- ROCR::performance(preds_,measure = "tpr",x.measure = "fpr")
#plot(roc_perf,add = TRUE,colorize =TRUE)



mtx_ <- as.matrix(dt_[, - ncol(dt_)])
mty_ <- as.matrix(dt_[,ncol(dt_)])
mdl.glm <- glmnet(mtx_,mty_, family = "binomial")

plot(mdl.glm, xvar = "dev", label = TRUE)


## Stepwise selection

# define training schema to 10 Fold CV
swtrc_ <- trainControl(method = "cv", number = 10)

swmdl_ <- train(quality ~ ., data = dt_, trainControl = swtrc_, method = "glmStepAIC", family = "binomial",direction = "forward",trace = 0)

# Check the summary.
summary(swmdl_)

# Anova 
anova(swmdl_$finalModel)

# predict
#####
swpreds_ <- predict(swmdl_,newdata = mtx_,class = "class")
table(swpreds_,mty_)

# ROC
x_ <- predict(swmdl_$finalModel,newx = mtx_,type = "response")
pred <- prediction(x_,mty_)
perf_ <- performance(pred,"tpr","fpr")
plot(perf_, colorize = FALSE, col = rainbow(10),main = "ROC")
########
## LASSO

fit_ <- glmnet(mtx_, mty_, family = "binomial") # fit non regularised.

predict(fit_,newx= mtx_[1:5,],type = "class",s = c(0.05,0.01))


lassoFit_ <- cv.glmnet(mtx_, mty_, family = "binomial", alpha = 1, type.measure = "class")
plot(lassoFit_)

# find the best lambda.

bestLambda_ <- lassoFit_$lambda.min

x2_ <- predict(lassoFit_, newx = mtx_, s = 'lambda.min', type = "response")

pred2 <- prediction(x2_,mty_)
perf2_ <- performance(pred2,"tpr","fpr")
plot(perf2_)
## RIDGE

ridgeFit_ <- cv.glmnet(mtx_, mty_, family = "binomial", alpha = 0, type.measure = "class")

# find best lambda.
b0Lambda_ <- ridgeFit_$lambda.min

x3_ <- predict(ridgeFit_, newx = mtx_[1:10,], s = 'lambda.min', type = "response")
pred2 <- prediction(x3_,mtyy_)
perf3_ <- performance(pred2,"tpr","fpr")
plot(perf3_)

anova(ridgeFit_)


plot(ridgeFit_)
