# Apple Health's step counter dataset prediction.
# Download at: https://bit.ly/2S9vzMD

debugSource('main.R')

df <- read.csv('dataset/step-counts-ts-dataset-master/Steps.csv')
data <- as.matrix(df$value)

#debug(run_test)
run_test(data)

