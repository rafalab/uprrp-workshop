# Big Data in Biology: From Genes to the Biosphere

Material used in the classes and workshop by Rafael Irizarry

## Downloading the data
You can download all the data in the repository by clicking the green button on the top right of this page that says __Clone or download__. You can also clone the repository by using __git clone__. Here are the steps to do that:

1. Open the terminal or command line in your computer and set the directory to where you want to save the repo (e.g, the Desktop or Document folders)
2. Copy and paste the url of this web page
3. Type __git clone url__ into your terminal or command line. The word __url__ is a place holder for the url of this webpage
4. After a few seconds you should have all the data in this repo


## Outline and learning objectives

The course intermixes short lectures with assessments. The two main topics of this week are data visualization and data wrangling. For the assessments we will be answering biologically relevant questions and preparing publication ready figures using R code. The assessments will be carried out on a table with over 500 rows, which we use to illustrate important principles that can then be extrapolated to "Big Data".

1. Basic data manipulation. After a 15 minute lecture and 15 minute lab, students will be able to:

    * Identify if data is tidy or not.
    * Import a csv file into data frame in R.
    * Select, filter and change columns on data frame with __dplyr__ functions.
    * Stratify and summarize data with __dplyr__ functions.
  
  
2. Data visualization principles. After a 30 minute lecutre and 30 minute lab students will be able to

    * Create publication ready figures using __ggplot__. 
    * Determine the more appropriate type of plot to convey a biological result.
    * Avoid commonly made plots that ofuscate or distract.
  
3. Data wrangling. The coding approach we present in the previous two sections work best with tidy data. In Biology it is common to receive data that is not in tidy format. In fact, often the majority of data analysis time is spent of _wrangling_ the data into a format that facilitates analysis. We will describe some of the tools that are available to do this.

    * Wrangle a jagged array stored in an excel file into a tidy data table.
    * Join tables to combine data from multiple datasets into one table.
    


