
# Pivot and unpivot data (short or long format in R) ----------------------
# https://python-bloggers.com/2022/07/how-to-unpivot-a-dataset-in-excel-power-query-vs-r-vs-python/

library(tidyverse)

# Read in dataset as csv

wholesale <- read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv')

head(wholesale)
# it is still small size table
#   Channel Region Fresh  Milk Grocery Frozen Detergents_Paper
#     <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl>            <dbl>
# 1       2      3 12669  9656    7561    214             2674
# 2       2      3  7057  9810    9568   1762             3293
# 3       2      3  6353  8808    7684   2405             3516
# 4       1      3 13265  1196    4221   6404              507
# 5       2      3 22615  5410    7198   3915             1777
# 6       2      3  9413  8259    5126    666             1795

# pivot or long table
wholesale_pivot <- wholesale %>% 
  pivot_longer(cols=c('Fresh':'Delicassen'),
               values_to = 'Sales',
               names_to = 'Category')

head(wholesale_pivot)
#   Channel Region Category         Sales
#     <dbl>  <dbl> <chr>            <dbl>
# 1       2      3 Fresh            12669
# 2       2      3 Milk              9656
# 3       2      3 Grocery           7561
# 4       2      3 Frozen             214
# 5       2      3 Detergents_Paper  2674
# 6       2      3 Delicassen        1338