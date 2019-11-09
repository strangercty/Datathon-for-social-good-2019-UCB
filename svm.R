library(dplyr)
library(zoo)
library(epitools)
library(e1071) 
library(plsgenomics)

data = read.csv('data.csv')

# Generate data for predict
df = data[c('FIRE_SIZE', 'STATE', 'LATITUDE', 'LONGITUDE', 
            'FIRE_YEAR', 'DISCOVERY_DATE', 'CONT_DATE', 
            'STAT_CAUSE_CODE', 'DISCOVERY_TIME', 'DISCOVERY_DOY')]

# Cleaning and generating covariates
df = df %>% mutate(WORK_DAY = CONT_DATE - DISCOVERY_DATE, 
                    ) %>%
                   filter(WORK_DAY >= 0 & WORK_DAY <= 14, STATE == 'CA') %>%
                    mutate(
                        LATITUDE = scale(LATITUDE),
                        LONGITUDE = scale(LONGITUDE),
                        FIRE_YEAR = factor(FIRE_YEAR),
                        STAT_CAUSE_CODE = factor(STAT_CAUSE_CODE),
                        DISCOVERY_DOY = scale(DISCOVERY_DOY),
                        DISCOVERY_TIME = scale(DISCOVERY_TIME)
                    )

df = df[complete.cases(df),]

# Sampling
ind = sample(nrow(df), replace = FALSE, size = nrow(df)*0.8)
data_train = df[ind, ]
data_test = df[-ind, ]

# SVM
classifier = svm(formula = WORK_DAY ~ LATITUDE + LONGITUDE + FIRE_YEAR + FIRE_SIZE + DISCOVERY_DOY + DISCOVERY_TIME + STAT_CAUSE_CODE, 
                 data = data_train, 
                 type = 'C-classification', 
                 kernel = 'linear') 

x = predict(classifier, newdata = data_test)
tb = table(data_test[,11], x)
m = mean(data_test[,11] == x)

matrix.heatmap(tb)

