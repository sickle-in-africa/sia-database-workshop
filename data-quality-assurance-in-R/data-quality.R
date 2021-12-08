#
#  DATA QUALITY ASSURANCE IN R
#  ===========================

# import an example data set on heart disease
heart_disease_data <- read.csv(
    file='https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data', 
    header=FALSE)

# peak at the data set
head(heart_disease_data)
str(heart_disease_data)
# -- it looks as though most of these variables have been classes either as "num" (number) or "char" (character)
# -- but this can't be right; if we look at the data dictionary [here](https://archive.ics.uci.edu/ml/datasets/heart+disease)
# -- then we see that some of these variables are categorical. 
# -- we can also even see some variables that R should have interpreted as "num" have been classed as "cahr". Why?
# -- we will come back to this.

# give columns meaningful names
colnames(heart_disease_data) <- c(
    'age',
    'sex',
    'cp',
    'trestbps',
    'chol',
    'fbs',
    'restecg',
    'thalach',
    'exang',
    'oldpeak',
    'slope',
    'ca',
    'thal',
    'num')

# find all missing values and map all missing value markers to NA
# -- if we scan through the columns we find no obvious missing values,
# -- but if we look closely we see a mysterious "?" value in the "ca"
# -- and "thal" columns. So it seems that missing values in the raw 
# -- data here have been coded as "?" not NA. We need to fix this. 
heart_disease_data$ca[heart_disease_data$ca=="?"] <- NA
heart_disease_data$thal[heart_disease_data$thal=="?"] <- NA
# -- note that NA and "NA" are not the same in R!

# change variable types to factors where necessary
heart_disease_data$sex <- as.factor(ifelse(heart_disease_data$sex==1,"M","F"))
heart_disease_data$cp <- as.factor(heart_disease_data$cp)
heart_disease_data$fbs <- as.factor(heart_disease_data$fbs)
heart_disease_data$restecg <- as.factor(heart_disease_data$restecg)
heart_disease_data$exang <- as.factor(heart_disease_data$exang)
heart_disease_data$slope <- as.factor(heart_disease_data$slope)
heart_disease_data$ca <- as.integer(as.character(heart_disease_data$ca))
heart_disease_data$thal <- droplevels(as.factor(as.integer(as.character(heart_disease_data$thal))))
# -- why do we bother with this? because R functions are "smart", in the sense
# -- that if we try to compute things like means or standard deviations, but 
# -- our chosen variable is of "char" type, then it will throw an error. 
# -- More importantly, the more advanced regression or chi-square test tools for
# -- example behave differently depending on the types of their input. This
# -- is *very important* for the validity of your downstream analysis. 

# check for outliers in continuous variables
boxplot(heart_disease_data$age)
boxplot(heart_disease_data$thalach)
boxplot(heart_disease_data$trestbps)
boxplot(heart_disease_data$chol)

# normality tests - are any variables roughly normally distributed?
hist(heart_disease_data$age)
qqnorm(heart_disease_data$age)
qqline(heart_disease_data$age)
# -- looks like age is approximately normally distributed!
# -- it therefore makes sense to calculate the mean and 
# -- standard deviation
mean(heart_disease_data$age)
sd(heart_disease_data$age)

hist(heart_disease_data$oldpeak)
qqnorm(heart_disease_data$oldpeak)
qqline(heart_disease_data$oldpeak)
# -- but oldpeak is not normally distributed.
# -- we had better calculate the median and 
# -- interquartile range instead
median(heart_disease_data$oldpeak)
quantile(heart_disease_data$oldpeak)

# we can also summarise some of the discrete variables in tables
table(heart_disease_data$sex)

# create new columns to faciliate analysis
heart_disease_data$heart_disease <- as.factor(ifelse(heart_disease_data$num >= 1, 'yes', 'no'))

# look at your clean data set!
head(heart_disease_data)
str(heart_disease_data)

# does sex influence your new outcome variable?
table(x=heart_disease_data$sex,y=heart_disease_data$heart_disease)
chisq.test(x=heart_disease_data$sex,y=heart_disease_data$heart_disease)
