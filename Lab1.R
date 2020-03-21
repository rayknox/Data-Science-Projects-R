###########################  STAT 410 LAB 1   #######################################

#####  Please save this script file with your group members' last names added in before you get started.  
#####  You will upload a script file to canvas for submission.  Code and output must be accompanied by
#####  a commented description/English answer to the question.  You should use code to fully find the solutions
#####  (if the question asks how many or which product, you shouldn't be figuring that out yourself
#####   from a printout)
###### NAMES: Raymond Knox, Ricardo Huerta, Samantha Hollifield, Nicole Miller


##   All problems come from the course textbook.
##   The problems use the Forbes2000 data set in the textbook R package HSAUR3.
library(HSAUR3) ##importing from library
data(package = "HSAUR3")
Forbes2000

# Ex 1.1: Calculate the median profit for the companies in the US and 
# the median profit for the companies in the UK, France, and Germany.  
# That is report
# -median profit individually for companies in each of US, UK, France, and Germany
# -median profit for companies in UK, France, and Germany combined

US = subset(Forbes2000, country %in% "United States") ##making subsets for all countries
US
median(US$profits, na.rm = T)

UK = subset(Forbes2000, country %in% "United Kingdom")
UK
median(UK$profits, na.rm = T)

Fr = subset(Forbes2000, country %in% "France")
Fr
median(Fr$profits, na.rm = T)

Ge = subset(Forbes2000, country %in% "Germany")
Ge
median(Ge$profits, na.rm = T)

Foreignp = subset(Forbes2000, country %in% c("United Kingdom", "France", "Germany")) ##creates subset of most foreign profits
Foreignp
median(Foreignp$profits, na.rm = T)

# Ex 1.2: Find all German companies with negative profit.  Print the company names accompanied by their profits.

genegprof = Ge[Ge$profits < 0,] ##finding profits that are negative
genegprof

compprof = genegprof[,c("name","profits")] ##creating vector with company name and product
compprof

# Ex 1.3: To which business category do most of the Bermuda island companies belong?
# Hint: use subset(), table(), levels(), and which.max() functions
Be = subset(Forbes2000, country %in% ("Bermuda")) ##bermuda subset
Be

Becat = Be[,c("name", "category")] ##selects specific columns
Becat
table(factor(Becat$category)) ##tabelizes the factor
which.max(table(factor(Becat$category))) ##asks for max factor number in table
##ANSWER 1.3 It would seem that most companies in Bermuda fall under the "INSURANCE" category

# Ex 1.4: For the 50 companies in the Forbes data set with the highest profits,
# plot sales against assets, suitably transformed (hint: log), labeling each point with the 
# appropriate country name which may need to be abbreviated (using abbreviate()) to
# avoid making the plot look too 'messy'.
# Hint: the following will identify the top 50 most profitable companies using
# the subset() function.
Forbes14 = subset(Forbes2000, Forbes2000$profits %in% rev(sort(Forbes2000$profits))[1:50])
Forbes14

plot(log(Forbes14$assets), log(Forbes14$sales))
### To add text to a plot you've already made, use the command text(x,y,labels)
text(x = log(Forbes14$assets), y = log(Forbes14$sales), labels = abbreviate(Forbes14$country)) ##adds text and organization statements

# Ex 1.5: Find the average value of sales for the companies in each country 
# in the Forbes data set and find the number of companies in each country 
# with profits above 5 billion US dollars
# Hint: use tapply().  cbind() can also be used to make results easier to read.
#table(factor(Forbes2000$country))
tapply(Forbes2000$sales, Forbes2000$country, mean) ##average value of sales per country

Forbes5mill = Forbes2000[Forbes2000$profits > 5,]#cbind("name", "country", "profits", "category")]
Forbes5mill
cbind(table(Forbes5mill$country))

# Ex 1.6: List all the products made by companies in the UK.
# Hint: use table()
UK
UKfac = factor(UK$category) #factorizes the table to count like row values
UKtab = table(UKfac)
UKtab

# Ex 1.7: Plot log sales against log market value for companies in the UK and in Germany
# using different plotting symbols for the two countries.
UKGe = rbind(UK, Ge) ##combines already separated UK and Germany data sets
UKGe

plot(log(UKGe$sales), log(UKGe$marketvalue), pch = c(19, 12))

# Ex 1.8: For the ten companies in the UK with the greatest profits construct
# a bar chart of profits labeled with the companies' name.
# Hint: use barplot with the names.arg option.
UKtop10 = subset(UK, UK$profits %in% rev(sort(UK$profits))[1:10]) ##reverse sorts the top 10 profit values from descending order
UKtop10

barplot(UKtop10$profits, names.arg = abbreviate(UKtop10$name))
# Ex 1.9: How many of the 20 companies with the greatest market value are from 
# the US and how many are from the UK?
# Hint: sort(Forbes2000$marketvalue, decreasing=TRUE)[1:20] identifies 
#       the top 20 market values.  Then can use subset().
Forbestop20 = subset(Forbes2000, Forbes2000$marketvalue %in% rev(sort(Forbes2000$marketvalue))[1:20])
Forbestop20 ##reverse order top 20 matrket value 

table(factor(Forbestop20$country)) ##tabelizes factor for numerical understanding

# Ex 1.10: Construct a histogram of profits for all companies in Germany with assets 
# above 3 billion dollars; how many such companies are there?  And which product does the company with the greatest profit make?
Gecos = subset(Forbes2000, country %in% c("Germany"))##returns assets over 3 billion
Getopcos = subset(Gecos, assets >= 3)
hist(Getopcos$profits)

Ge3bill = Ge[Ge$assets > 3,] ##returns assets over 3 billion
Ge3bill

table(factor(Ge3bill$country)) ##how many there are
table(factor(Ge3bill$category)) ## category?
table(factor(Ge3bill$name)) ## name

Ge3billmax = subset(Ge3bill, Ge3bill$profits %in% rev(sort(Ge3bill$profits))[1:1]) ##reverse sorts and returns one row with highest profit
Ge3billmax