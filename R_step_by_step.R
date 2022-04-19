#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output: 
#'   html_document: 
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
library(ggplot2); # visualization
library(dplyr); # data processing
library(GGally); # plotting
library(rio); # formatting for importing and exporting files
library(pander); # formats tables
library(printr);
library(broom);
library(dplyr); # adding dplyr library 
options(max.print=42); # how many lines of results are printed
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};
# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment, Variables, and Data types.
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo <- 15;
bar <- foo <- 15;
bar <- foo;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#'
#' ## Simple data types
#'
#' Numeric. You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
foo <- 5 + 5; foo;
foo <- exp(5); foo;
foo <- 5 - 5; foo;
foo <- 5/5; foo;
foo <-5*5; foo;
foo <- 5^5; foo;
foo <- log(5); foo;
foo <- sqrt(5); foo; 
foo <- log(((5^2)*(2*5)/3)-5)
baz <- "Do Not Panic"
bat <- 'Do Not Panic'
caz <- "Don't Panic"
daz <- 'The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive' 
dat <- '42'
whatisthis(foo);
#' Character strings. Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.

#+ assignment_string

#' Logical values are `TRUE` or `FALSE`. Can also be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`

#+ assignment_logical
One <- TRUE
Zero <- FALSE
baz==bat
baz==caz
dat>foo
baz>=bat

#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime
today <- "2/8/2022"
Sys.Date()
Sys.time()
as.Date(32768, origin = "1900-01-01")
#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
print(lit <- c(5, 10, 15, 20, 25, 30))
print(lib <- c(-1, -3, -6, 18, 45, 63))
#'
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c

#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(lit)
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
55:73
-10:15
98:80
seq_len(20)

#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators
lit+12
lit+lib
lib+lit
lit/lib
lit>=12
lib<=35
boo <- lib<=35
c(lit,lib)
c(lit,lib,"76")
#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1
print(jar <- c(a="cat",best="dog",c="fish", slow="turtle"))
jar["best"]
jar[c("best","c")]
#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(jar)
names(jar) <- c("libby","beau","bob","bob2") #renaming character vector
jar
names(jar)[3]
names(jar)[3] <-"milo" #renaming only third character vector
jar
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
lit[3]

#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
lit[c(1,2,3)]
lit[c(1:3,5:6)]
#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore
summary(lit)
summary(jar)
table(lit) #frequency table 
bat <- sample(1:10,30,replace=TRUE)*1000 #how to generate 30 integers between 1 and 10
table(bat)
bat
head(bat) #fist several values of bat
tail(bat) #last several values of bat 
diff(bat) #difference between two consecutive values
sum(bat) #sum of all the values in bat (vector)
seq_along(bat) #generate ID numbers unique to bat
sum(bat,na.rm = TRUE)
head(jar)
#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.

#+ vectors_aggregate
quantile(bat)
quantile(bat,na.rm = T)
#' ## Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()` and (for
#' small datasets) `plot()`.

#+ df_explore
dim(iris) #dimensions of the data frame
nrow(iris) #number of rows in the data frame
ncol(iris) #number of columns in the data frame 
names(iris) #names of the columes in the data frame 
head(iris) #fist several values of iris database
tail(iris) #last several values of iris database 
head(iris,20)

#' How to select rows in a dataset 
#+ df_subset 
iris[1:20,] #rows 1 through 20 in iris dataset 
iris[c(1:3,7,10,12:15,55:73,95,140),] #showing specific rows using numbers
iris[-c(3:20),] #skipping rows 3 through 20
seq_len(nrow(iris))
sample(seq_len(nrow(iris)), 20) #sampling without replacement, limited to the size of the population
sample(seq_len(nrow(iris)), 300, replace=TRUE) #sampling with replacement
flower <- iris[sample(seq_len(nrow(iris)), 20),]

#' How to select columns in a dataset 
#+ df_columns, error=TRUE, results="hide"
iris[,1:3] #columns 1 through 2, leaving rows blank
iris[,c("Petal.Length", "Petal.Width")]
PreVar <- c("Petal.Length", "Petal.Width") #predefined variables 
iris[ , PreVar]
iris[ , c(PreVar, "Species")]

iris$Species #looking for a specific column within the dataset 
Outcome <- "Species"
iris$Outcome
iris[[Outcome]]
iris[["Species"]]

#' How to select columns and rows simultaneously
#+ df_columnsrows
iris[4:10,PreVar]

#' ## Linear Models 
#' 
#+ linear_models
head(mtcars) #view of the mtcars dataset
lm(mpg~hp+wt+qsec,mtcars) #linear model of columns in the dataset 
Performance <- lm(mpg~hp+wt+qsec,mtcars) #saving lm to an object
summary(Performance)
summary(Performance)$coefficient #print or summary with summarize the data
glance(Performance)
tidy(Performance)
lm(mpg~hp+wt+qsec,mtcars) %>%tidy() %>% select(c("estimate","p.value"))
Performance %>%tidy() %>% select(c("estimate","p.value"))
whatisthis(Performance)
Performance %>%tidy() %>% select(c("estimate","p.value")) %>% slice(-1)
#select is to select columns
#slice is to select rows 

#' ## Multiple Comparisons
#' 
#+ Multiple comparisons
Performance %>%tidy() %>% select(c("p.value")) %>% slice(-1)
Performance %>%tidy() %>% select(c("p.value")) %>% slice(-1) %>% unlist() %>% p.adjust()

#' ## Comments
#'
#' `#` This is an ordinary comment. Everything after it on the same line is not
#' executed.
#'
#' `#'` This indicates that this line should be formatted as text. It must be
#' the first two characters in that line in order to work.
#'
#' `#+` This indicates that the following lines (until the next #' or #+ )
#' should be treated as a "code chunk". I.e. the next lines (but not this one)
#' will be run, the code will be displayed according to your settings and the
#' results will be displayed according to your settings.
#'
#' ## Working with Datasets
#'
#+ Define location and import your files 
list.files #This allows you to search your desktop for a specific file after they are downloaded
list.files("/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data/") 
#This allows you to find the Sample Data file
Example1 <- list.files("/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data/", 
                       full.names = TRUE) %>% sapply(import) #Renaming as a vector, example1
#Piping the previous command to sapply(whatever function you want it to do to
#all the files in sample data). And normalizes the names. In this case we are
#importing
# View(Example1) Allows you to view the files saved as example one (ie. all the files in the sample data folder)
list.files("/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data/", 
           full.names = TRUE) %>% sapply(import) %>% names #listing out file names

# Importing files into R
birthweight <- import("/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data/Birthweight.sav")

#+ Debugging 
Example1$`/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data//Birthweight.sav` %>% head() #selecting one dataset or file within the folder
# Example1$`/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data//Birthweight.sav` %>% View #viewing the dataset
Example1 <- list.files("/Users/Cece/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data/", 
                       full.names = TRUE) %>% sapply(import) %>%setNames(., basename(names(.)))
#Standardizing names for files. Removes Users/CeCe/Desktop/UTSA MFM fellowship/MSCI-TS/Sample Data

#+ Working with a specific dataset 
Example2 <- Example1$Birthweight.sav #Saving birthweight data only as example2 
View(Example2) #viewing the birthweight dataset 

#' ## Introduction to dplyr
#' 
#+ mutate(birthweight)
mutate(birthweight,AGE=AGE*12) %>% View # Converting age to age in months
mutate(birthweight,AGEMonths=AGE*12) %>% head # Adding age in months as a last column
mutate(birthweight,AGEMonths=AGE*12, AGEDays=AGEMonths*30.4) %>% head
table(birthweight$RACE) #extracting different variables from one column
with(birthweight, case_when(RACE == 1 ~ "Caucasian", RACE == 2 ~ "Asian",
                            RACE == 3 ~ "African American/Black", TRUE ~ as.character(RACE))) %>% table
# Assigning descriptive values to race 
mutate(birthweight,AGEMonths=AGE*12, AGEDays=AGEMonths*30.4
       , RACEName=case_when(RACE == 1 ~ "Caucasian"
                   , RACE == 2 ~ "Asian"
                   , RACE == 3 ~ "African American/Black"
                   , TRUE ~ as.character(RACE))) %>% head
summary(birthweight$BWT)

#' ## The Summary Function
#' 
#+ summary(birthweight)
summary(birthweight)

#+ summarize(birthweight)
summarize(birthweight, Age=median(AGE)) #getting the median AGE
summarize(birthweight, Age=mean(AGE))
summarize(birthweight, MedianAge=median(AGE), Height=median(HT), Birthweight=median(BWT), MeanAge=mean(AGE)) 
# building the summarize function out

#+ group_by(dataset, variables)
group_by(birthweight, SMOKE) #shows dataframe by smoking, 2 groups in SMOKE
group_by(birthweight, SMOKE)%>%summarize(MedianAge=median(AGE), Height=median(HT), Birthweight=median(BWT), MeanAge=mean(AGE))
group_by(birthweight, SMOKE)%>%summarize(MedianAge=median(AGE), Height=median(HT), Birthweight=median(BWT), MeanAge=mean(AGE))
group_by(birthweight, SMOKE)%>% summarize(across(where(is.numeric), mean))
# showing the mean for all variables in a dataset
group_by(birthweight, SMOKE)%>% summarize(across(where(is.numeric), mean, .names = '{.col}_mean')
                                          ,across(where(is.numeric), sd, .names = '{.col}_sd'))
                                               