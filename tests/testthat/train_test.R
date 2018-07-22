## helper script to test suite
## this creates a train/test dataset split based on the iris dataset


i.columns = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
## training set
i.train.indexes = c(1:20, 51:70, 101:120)
i.train = iris[i.train.indexes, i.columns]
i.train.labels = iris[i.train.indexes, "Species"]
rownames(i.train) = paste0("X", 1:nrow(i.train))
i.train.u = umap(i.train, n_neighbors=10, random_state=123)

## testing set (for prediction)
i.test.indexes = c(46:50, 96:100, 146:150) 
i.test = iris[i.test.indexes, i.columns]
i.test.labels = iris[i.test.indexes, "Species"]
rownames(i.test) = paste0("Test", 1:nrow(i.test))

