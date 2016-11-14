## My Script ##

require(ggplot2)
require(scales)
require(reshape2)
require(corrplot)


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

# Seems like there is quite a large disparity of scales, e.g. SO2 has mean of 137, whilst Cl- of 0.04.


## Univariate Distributions.
# Let's investigate our target variable. Given that it's a factor we will look at it's proportion.

## Quality
ggplot(dt_,aes(dt_$quality)) + geom_bar(colour  = "black", aes(fill= ..count..)) + scale_fill_gradient(low = "green", high= "red") + scale_x_discrete(name = "Wine Quality") + scale_y_continuous(name = "Count",labels = scales::comma) + ggtitle("Proportion of Wine Quality In Training Set") + guides(fill=FALSE)



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
plot(dt_pca,type="lines")

(dt_pca$sdev)^2
dt_pca <- as.data.frame(dt_pca$x[,1:4])

ggplot(dt_pca,aes(dt_pca$PC1,dt_pca$PC2)) + geom_point(aes(colour = dt_$quality)) + scale_colour_brewer(palette = "Set1",name = "Quality") + scale_y_continuous(name = "Priniciple Component 2") + scale_x_continuous(name = "Principle Component 1") + ggtitle("Wine Quality Labels in 2-Principle Components")

## Bivariate Visualisation
# Have a look at variable correlations.
corrplot(cor(dt_[,-ncol(dt_)]))


