
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




# Nested option:
arrange(
  summarize(
    group_by(
      filter(mtcars, carb > 1),
      cyl
    ),
    Avg_mpg = mean(mpg)
  ),
  desc(Avg_mpg)
)


# Multiple Object Option:
a <- filter(mtcars, carb > 1)
b <- group_by(a, cyl)
c <- summarise(b, Avg_mpg = mean(mpg))
d <- arrange(c, desc(Avg_mpg))
print(d)


# %>% Option:
mtcars %>%
  filter(carb > 1) %>%
  group_by(cyl) %>%
  summarise(Avg_mpg = mean(mpg)) %>%
  arrange(desc(Avg_mpg))%>%
  glimpse()

