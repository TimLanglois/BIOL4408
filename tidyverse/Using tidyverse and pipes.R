
# Using the tidyverse package

library(tidyr)
library(dplyr)

# Pipes in R----
# purpose to simplify the notation for the application of several functions to a single data frame 

# R is a functional language, which means that your code often contains a lot of parenthesis, ( and ). When you have complex code, this often will mean that you will have to nest those parentheses together. This makes your R code hard to read and understand. 



# # Pipe option (or making a pizza in the tidyverse)

pizza <- pie %>%
  dress_with(sauce) %>%
  dress_with(oil) %>%
  dress_with(break(cheese)) %>%
  put_in(oven) %>%
  take_out(oven)

# And the pizza is done. Compare to
 

# # Multiple Object Option:

pie_tmp1 <- dress_with(pie, sauce)
pie_tmp2 <- dress_with(pie_tmp2, oil)
pie_tmp3 <- dress_with(pie_tmp3, break(cheese))
pie_tmp4 <- put_in(pie_tmp4, oven)
pizza <- take_out(pie_tmp, oven)


# Nested option:

pizza <- take_out(put_in(dress_with(dress_with(dress_with(pie, sauce), oil), break(cheese), oven), oven)

  
                  
# Take, for example, following code chunk and read it aloud:

iris %>%
  subset(Sepal.Length > 5) %>%
  aggregate(. ~ Species, ., mean)


# "you take the Iris data, then you subset the data and then you aggregate the data".


glimpse(iris)


# Nested option:
arrange(
  summarize(
    group_by(
      filter(iris, Petal.Length > 1),
      Species
    ),
    Avg_Sepal.Length = mean(Sepal.Length)
  ),
  desc(Avg_Sepal.Length)
)


# Multiple Object Option:
a <- filter(iris, Petal.Length > 1)
b <- group_by(a, Species)
c <- summarise(b, Avg_Sepal.Length = mean(Sepal.Length))
d <- arrange(c, desc(Avg_Sepal.Length))
print(d)


# %>% Option:
iris %>%
  filter(Petal.Length > 1) %>%
  group_by(Species) %>%
  summarise(Avg_Sepal.Length = mean(Sepal.Length)) %>%
  arrange(desc(Avg_Sepal.Length))%>%
  glimpse()

