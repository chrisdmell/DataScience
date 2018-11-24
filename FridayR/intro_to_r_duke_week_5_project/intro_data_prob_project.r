##plotting histogram of height 

ggplot(data= brfss2013, aes(x = weight2))  + geom_bar()  +xlab("Weights") +
  scale_x_continuous(limits = as.character(0, 300))

hist(as.integer(brfss2013$weight2))
typeof(list(brfss2013$weight2))

random_sample = sample(brfss2013, size = 100, replace = TRUE, prob = NULL)
#Error: Discrete value supplied to continuous scale
#Error: Cannot use `+.gg()` with a single argument. Did you accidentally put + on a new line?
#Error: StatBin requires a continuous x variable: the x variable is discrete. Perhaps you want stat="count"?

str(brfss2013$vegeda1_)
summary(brfss2013$vegeda1_)


##### stratified sampling ####
set.seed(9850)
new_dateset = brfss2013 %>% select('X_state', 'genhlth', 'hlthpln1', 'X_bmi5cat', 'exerany2')
#hlthpln1 - Have Any Health Care Coverage
#exerany2 - Exercise In Past 30 Days
nrow(new_dateset)
unique(new_dateset$genhlth)
unique(new_dateset$X_bmi5cat)
str(new_dateset)
library(data.table)
new_dateset = na.omit(as.data.table(new_dateset))
nrow(new_dateset)
# new_dateset %>% summarise(new_dateset$X_bmi5cat)
new_dateset
library("Hmisc")
describe(new_dateset)
summary (new_dateset)
table   (new_dateset$X_bmi5cat)
table   (new_dateset$hlthpln1, new_dateset$X_bmi5cat)
#if you see this table there is sharp difference between ppl who are healthy and people who think they are healthy. 
prop.table(table(new_dateset$X_bmi5cat))

yes = new_dateset %>% filter(hlthpln1 == "Yes")
No  = new_dateset %>% filter(hlthpln1 == "No")

## barplot side by side
barplot(list(yes$genhlth), list(No$genhlth))
plot(yes$genhlth)
barplot(table(new_dateset$hlthpln1, new_dateset$X_bmi5cat), beside = TRUE,
        col = c("red","green"),
        xlab = "plot heading", 
        main = "heading")
legend("topleft",
       c("Yes","No"),
       fill = c("red","green"))



barplot(table(new_dateset$hlthpln1))
barplot(table(yes$hlthpln1, No$hlthpln1))
nrow(yes)
nrow(No)
table(No$exerany2)
table(yes$hlthpln1, No$hlthpln1)

#####################3
table(exercise_yes$X_bmi5cat,exercise_no$X_bmi5cat )
exercise = new_dateset %>% filter( hlthpln1 == "Yes")
barplot(table(exercise$exerany2,exercise$X_bmi5cat ), beside = TRUE,
        xlab = "plot heading", 
        col = c("red","green"),
        main = "heading")
legend("topleft",
       c("exercies_Yes","exercise_No"),
       fill = c("red","green"))
####################################################3
table(exercise$exerany2,exercise$X_bmi5cat )
exercise = new_dateset %>% filter( hlthpln1 == "Yes", genhlth = "Very good")
barplot(table(exercise$exerany2,exercise$X_bmi5cat ), beside = TRUE,
        xlab = "plot heading", 
        col = c("red","green"),
        main = "ppl say good health and genhealth good but bmi")
legend("topleft",
       c("exercies_Yes","exercise_No"),
       fill = c("red","green"))


###########################33
#histogram of height and weight

ggplot(data = brfss2013, aes(x = height3)) +
  geom_histogram(binwidth = 15)

hist(brfss2013$height3)

hist(brfss2013$height3, xlim = c(300,700), breaks=1000)
#########################################33
#solving getting in ft and inches
heights = na.omit(as.list(brfss2013$height3))

p <- function(..., sep='') {
  +    paste(..., sep=sep, collapse=sep)}

sep = ""
paste(substring(x, 1,1), ".", substring(heights[1],2), sep=sep, collapse=sep)

mapping <- function(x)  {
  x = as.character(x)
  sep = ""
  x = paste(substring(x, 1,1), ".", substring(heights[1],2), sep=sep, collapse=sep)
  return(x)
}
heights[2]

mapping(heights[1])

new_height = lapply(heights, mapping)
new_height_1 = as.numeric(new_height)
table(new_height_1)
new_height_2 = new_height_1 * 30.48
new_height_3 = lapply(new_height_2, function(x) x[!is.na(x)])
table(new_height_2)
hist(new_height_2 , breaks = 100)
#############################33
table(brfss2013$weight2)

hist(as.numeric(brfss2013$weight2))

hist(as.numeric(brfss2013$weight2), breaks = 500, xlim = c(0, 200))

ggplot(data = brfss2013, aes(x = as.numeric(brfss2013$weight2))) +
  geom_histogram(bins = 150)
  

ggplot(data = brfss2013, aes(x = as.numeric(brfss2013$weight2))) +
  geom_histogram( aes(y =..density..),     breaks=seq(20, 50, by = 2), 
                       col="red", 
                       fill="green", 
                       alpha=.2, bins = 150) +
  geom_density(col=100)

x = brfss2013[,brfss2013$weight2 == 1]
nrow(x)
x = brfss2013[brfss2013$weight2 == 0, ]
nrow(x)
#Error: StatBin requires a continuous x variable: the x variable is discrete. Perhaps you want stat="count"?
#make numeric

#################################################3
w = table(new_dateset$genhlth)
w
# table() will give the count of catetegorical variables in the DF.
w1 = table(new_dateset, "genhlth")


w2 = table(new_dateset$genhlth, new_dateset$X_state, dnn = c("Health Status", "State"))

##sample 
##replicate ()
##apply

x  = sample(brfss2013$sleptim1, 100)
x1 = replicate(100, sample(brfss2013$sleptim1, 100) )
x2 = apply(replicate(100, sample(brfss2013$sleptim1, 100)),2, FUN = mean)
mean(x2)


########################################################################################
##question one ##
##sugar and how it is related to diabetis
#ssbsugar: How Often Do You Drink Regular Soda Or Pop?
#ssbfrut2: How Often Did You Drink Sugar-Sweetened Drinks?

#diabage2: Age When Told Diabetic
#insulin: Now Taking Insulin
#bldsugar: How Often Check Blood For Glucose

#cvdinfr4: Ever Diagnosed With Heart Attack
#cvdcrhd4: Ever Diagnosed With Angina Or Coronary Heart Disease
#cvdstrk3: Ever Diagnosed With A Stroke

new_dataset_2 = brfss2013 %>% select('ssbsugar', 'ssbfrut2', 'diabage2', 
                                     'insulin', 'bldsugar', 'cvdinfr4', 
                                     'cvdcrhd4', 'cvdstrk3')

# the data is really sparse for sugar data. 
# gotta look at something else

##################################################################
#sleptim1: How Much Time Do You Sleep
#cvdinfr4: Ever Diagnosed With Heart Attack
#cvdcrhd4: Ever Diagnosed With Angina Or Coronary Heart Disease
#cvdstrk3: Ever Diagnosed With A Stroke


new_dataset_3 = brfss2013 %>% select('sleptim1', 'cvdinfr4', 
                                     'cvdcrhd4', 'cvdstrk3')
### need more variables to explore 

###############################################################
#alcday5: Days In Past 30 Had Alcoholic Beverage
#avedrnk2: Avg Alcoholic Drinks Per Day In Past 30
#drnk3ge5: Binge Drinking
#maxdrnks: Most Drinks On Single Occasion Past 30 Days

###veterans and binge drinking
###############################################################
#fruitju1:   How Many Times Did You Drink 100 Percent Pure Fruit Juices?
#fruit1:     How Many Times Did You Eat Fruit?
#fvbeans:    How Many Times Did You Eat Beans Or Lentils?
#fvgreen:    How Many Times Did You Eat Dark Green Vegetables?
#fvorang:    How Many Times Did You Eat Orange-Colored Vegetables?
#vegetab1:   How Many Times Did You Eat Other Vegetables?

## can talk about fruit trends 

fruit_set = brfss2013 %>% select( 'fruitju1', 'fruit1', 'fvbeans', 'fvgreen',
                                  'fvorang', 'vegetab1')

nrow(fruit_set)

fruit_set_2 = na.omit(fruit_set)
nrow(fruit_set_2)

ggplot(data = fruit_set_2, aes ( fruitju1)) + geom_histogram(bins = 10)


############################################################

#fruitju1: How Many Times Did You Drink 100 Percent Pure Fruit Juices?
#  fruit1: How Many Times Did You Eat Fruit?
#  fvbeans: How Many Times Did You Eat Beans Or Lentils?
#  fvgreen: How Many Times Did You Eat Dark Green Vegetables?
#  fvorang: How Many Times Did You Eat Orange-Colored Vegetables?
#  vegetab1: How Many Times Did You Eat Other Vegetables?

#Can talk about exercise trends

################################################################
#Exercise 
#exerany2: Exercise In Past 30 Days
#exract11: Type Of Physical Activity
#exeroft1: How Many Times Walking, Running, Jogging, Or Swimming
#exerhmm1: Minutes Or Hours Walking, Running, Jogging, Or Swimming
#exract21: Other Type Of Physical Activity Giving Most Exercise During Past Month
#exeroft2: How Many Times Walking, Running, Jogging, Or Swimming
#exerhmm2: Minutes Or Hours Walking, Running, Jogging, Or Swimming
#strength: How Many Times Did You Do Physical Activities Or Exercises To Strengthen Your Mu

exercise_set = brfss2013 %>% select ('exerany2', 'exract11', 'exeroft1', 'exerhmm1', 
                                     'exract21', 'exeroft2', 'exerhmm2', 'strength')
nrow(na.omit(brfss2013 %>% select ('exerany2') %>% filter ( exerany2 != 0)))
nrow(na.omit(brfss2013 %>% select ('exract11') %>% filter ( exract11 != 0)))
barplot(x, names.arg = c ("nameofcol", "ff"))

col_names = c('exerany2', 'exract11', 'exeroft1', 'exerhmm1', 
              'exract21', 'exeroft2', 'exerhmm2', 'strength')

row_num <- function(x) {
  return (as.numeric(nrow(na.omit(brfss2013 %>% select (x) %>% filter ( x != 0)))))
}

nrows_needed = lapply(col_names, row_num)

barplot(unlist(nrows_needed), names.arg = col_names,  cex.names = .6,
        horiz=F, las =2, col = "skyblue")
########################################################################3
#Create new dataset that you need
#Change the variables as you need it - ( like numeric/ character etc)
#
