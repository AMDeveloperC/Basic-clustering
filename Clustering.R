setwd(".")
library(proxy);

# Execute the hierarchical clustering method specified
# dataset: it's a labeled matrix having n row and m column from a dataset of n documents and m words extracted
# distanceMethod: it's a string specifying the distance method (e.g. "cosine")
# clusterMethod: it's a string specifying the clustering method (e.g. "average")
# clustersNumber: it's an integer specifying how many cluster in output are required
# outPath: it's the folder where the confusion matrix output is stored
clusterizing <- function(dataset, distanceMethod, clusterMethod, clustersNumber, outPath) {
    # Executing clustering
    distanceMatrix <- dist(dataset[,1:ncol(dataset)-1], method=distanceMethod, diag=TRUE, upper=TRUE);
    hlm <- hclust(distanceMatrix, method=clusterMethod);
 
    # Composing output file name
    numberOfFeatures <- ncol(dataset)-1;
    name <- paste("Features", numberOfFeatures, sep="_")
    name <- paste(outPath, name, sep="");
    fileName <- paste(name, ".txt", sep="");

    # Building confusion matrix
    groups <- cutree(hlm, k=clustersNumber);
    cm <- table(dataset[,ncol(dataset)], groups);
    write.table(cm, fileName, sep=" ");

    return(hlm);
}

# Demo
if (interactive()) {
    # Assumptions:
    # - you have labeled matrixes in the parent folder
    # - the output will be stored in the parent folder

    pathToData <- "..";
    files <- list.files(path=pathToData);
    for (i in 1:length(files)) {
        nameFile <- paste(pathToData, files[i], sep=""); # DO NOT DELETE!
        data <- read.table(nameFile, header=T, sep="");
	clusterizing(data , "cosine", "average", 4, "..");
    }
}
