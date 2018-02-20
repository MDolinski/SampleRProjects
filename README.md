# Description
This repository contains a couple of sample reports containing snippets with code in R. 

Author: Mateusz Doliński

Current projects: 
1.  VariableSelection.md is a sample report comparing various supervised variable selection methods on Wisconsin Diagnostic Breast Cancer Dataset.
2. PFAImplementation.R is the implementation of Principle Feature Analysis algorithm proposed in Lu, Y., Cohen, I., Zhou, X. S., & Tian, Q. (2007)
3. Sample Shiny application visualizing data of car prices. The data set used in this project was downloaded from: http://web.sgh.waw.pl/~atoroj/ In order to run the application use the following command in RStudio:
```{r}
if (!require(shiny)) install.packages('shiny')
require(shiny)
runUrl("https://github.com/MDolinski/SampleRProjects/archive/master.tar.gz",
       subdir = "/SampleShinyApp/")
```
