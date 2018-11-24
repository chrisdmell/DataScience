##GGPLOT
ggplot(data = soomenae, aes(x = soenone)) + geom_hist

##summary 
summary is used to get 5 point summary. It also shows the "NA" count of the same. 

## select
df = some_data %>% select( 'colname', 'colname1')
#colnames are in inverted commas. 

##omit na from the df
omit the "NA" values from the data-table
na.omit( data-table)

##to get data as table to plot in histogram 
table(df$column)

## using filter 
new_df = df %>% filter (column == "" )

##side by side barplot 
barplot(table(new_dateset$hlthpln1, new_dateset$X_bmi5cat), beside = TRUE,
        col = c("red","green"),
        xlab = "plot heading", 
        main = "heading")
legend("topleft",
       c("Yes","No"),
       fill = c("red","green"))
	
	
##histogram
hist(brfss2013$height3, xlim = c(300,700), breaks=1000)

ggplot(data = brfss2013, aes(x = height3)) +
  geom_histogram(binwidth = 15)
  
  