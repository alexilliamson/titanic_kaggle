---
title: "Titanic"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.3
```

```
## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.2     v dplyr   0.7.4
## v tidyr   0.8.0     v stringr 1.3.0
## v readr   1.1.1     v forcats 0.3.0
```

```
## Warning: package 'tibble' was built under R version 3.4.3
```

```
## Warning: package 'tidyr' was built under R version 3.4.3
```

```
## Warning: package 'readr' was built under R version 3.4.3
```

```
## Warning: package 'purrr' was built under R version 3.4.3
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## Warning: package 'stringr' was built under R version 3.4.3
```

```
## Warning: package 'forcats' was built under R version 3.4.3
```

```
## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```


### Data  


```r
train_df <- read_csv('data/train.csv', col_types = cols(
        PassengerId = col_integer(),
        Survived = col_integer(),
        Pclass = col_integer(),
        Name = col_character(),
        Sex = col_character(),
        Age = col_double(),
        SibSp = col_integer(),
        Parch = col_integer(),
        Ticket = col_character(),
        Fare = col_double(),
        Cabin = col_character(),
        Embarked = col_character()
))

submission_df <- read_csv('data/test.csv', col_types = cols(
        PassengerId = col_integer(),
        Survived = col_integer(),
        Pclass = col_integer(),
        Name = col_character(),
        Sex = col_character(),
        Age = col_double(),
        SibSp = col_integer(),
        Parch = col_integer(),
        Ticket = col_character(),
        Fare = col_double(),
        Cabin = col_character(),
        Embarked = col_character()
))
```

```
## Warning: The following named parsers don't match the column names: Survived
```

### Determine Predictors  

Which fields are present in both datasets?  


```r
combined_data <- submission_df %>% 
        mutate(IsTrain = FALSE) %>%
        bind_rows(mutate(train_df, IsTrain = TRUE))

combined_data %>%
        group_by(IsTrain) %>%
        summarise_all(~sum(is.na(.x)))
```

```
## # A tibble: 2 x 13
##   IsTrain PassengerId Pclass  Name   Sex   Age SibSp Parch Ticket  Fare
##   <lgl>         <int>  <int> <int> <int> <int> <int> <int>  <int> <int>
## 1 FALSE             0      0     0     0    86     0     0      0     1
## 2 TRUE              0      0     0     0   177     0     0      0     0
## # ... with 3 more variables: Cabin <int>, Embarked <int>, Survived <int>
```

Select the non-missing features that would make sense as predictors


```r
predictors <- combined_data %>%
        select_if(~sum(is.na(.x)) == 0) %>%
        select(-PassengerId,-Name,-Ticket, -IsTrain) %>%
        names()

predictors
```

```
## [1] "Pclass" "Sex"    "SibSp"  "Parch"
```


