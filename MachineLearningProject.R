library(randomForest)

# Download the training and test datasets into the current working directory.
urlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urlTrain, 'training_dataset.csv', method = 'curl', quiet = T)
download.file(urlTest, 'testing_dataset.csv', method = 'curl', quiet = T)

# Start the clock!
start_time <- proc.time()[3]

#Read the data from the training and test datasets.
training <- read.csv('training_dataset.csv')
testing <- read.csv('testing_dataset.csv')

# By quickliy skimming the testing data, it becomes obvious that many of the
# features (variables) contain nothing but NAs, and therefore cannot be useful in prediction.
# Therefore we conclude that only those variables in the test set that are not fully NAs 
# would be useful in building the model. Below we extract the indices for such useful variables
# (those having at least one non-NA value) and select those variables for our modeling
# in the training set. The first line just takes the indices where NOT all values are NA.
# Finally, we select the useful indices in the testing set as well, for use in our prediction later.
useful_indices <- colSums(is.na(testing))/nrow(testing) != 1
useful_training <- training[,useful_indices]
useful_testing <- testing[,useful_indices]

# Then we also need to take out book-keeping variables from our training dataset,
# since they are not useful in training the model. These include columns with no
# actual measurement data, from 'X' to 'num_window'.
useful_training <- select(useful_training,-(X:num_window))

# Build a random forest model taking as predictors all features (all columns of
# the dataset, except the last one, which is our outcome variable).
# We would also like to measure the time it takes R to build our model.
start_time_RF <- proc.time()[3]
RF <- randomForest(useful_training[,-ncol(useful_training)], useful_training[,ncol(useful_training)])
modeling_time <- round(unname(unclass(proc.time()[3] - start_time_RF)), digits = 0)

# Prediction
answers <- predict(RF, useful_testing)

# Function for writing the answers into 20 separate files:
# (This function is given as instructions on the project submission page.)
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

# Write answers to 20 files.
pml_write_files(answers)

# Stop the clock
total_time <- round(unname(unclass(proc.time()[3] - start_time)), digits = 0)

# Report total run time, as well as proportion of time spent only in modeling.
print(paste("Running the entire code - except downloading datasets - took", total_time, "seconds."))
print(paste("Building the Random Forest model took", modeling_time, "seconds, which constitutes", round(modeling_time/total_time, digits = 2)*100, "percent of total run time."))
