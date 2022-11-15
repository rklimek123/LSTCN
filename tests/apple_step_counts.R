# Health data of one individual were extracted from the
# Apple Health application in the period from 2015 to 2021.
# Download at: https://bit.ly/2S9vzMD
source('main.R')

df <- read.csv('dataset/step-counts-ts-dataset-master/Steps.csv')
data <- as.matrix(df$value)

run_test(data, 100)
