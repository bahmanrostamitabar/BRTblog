library(tidyverse) # Load the package tidyverse
gapminder <- read_csv("data/gapminder.csv")# Import data into R
gapminder_filtered <- gapminder %>% # Filter data to show only observation for 2007
  filter(year == 2007)
ggplot(data = gapminder_filtered,#Create a plot
       mapping = aes(x = gdpPercap, y = lifeExp, #display variable gdpPercap in x-axis 
                     #and lifeExp in y-axis from gapminder_filtered data 
                     size = pop, color = continent)) +# assign different point size based on population of countries and assign different colors to different countries  
  geom_point()+#create a scatter plot
  labs(x = "GDP per capita", y = "Life expectancy",# cosmatic part pf the graph
       title = "Health and wealth are strongly related",
       subtitle = "142 countries; 2007 only", caption = "Source: The Gapminder Project",
       color = "Continent", size = "Population") +
  theme_classic()

