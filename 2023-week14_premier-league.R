# Info: <https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-04-04>

# Load packages
library(tidyverse)

# Read in the data manually
df_premier <- read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv")

# Let's take a look at the data
glimpse(df_premier)

# RQ: Which Premier League referees had the highest ratio of home and away wins 
# in the 2021/22 season

# Select only the relevant variables to work more efficiently
df_premier_ref <- df_premier |> 
  select(
    # Full time result
    ft_result = FTR,
    # Referee of the match
    referee = Referee
  )

# Let's take a look at how many matches officiated each referee
df_premier_ref |> 
  count(referee) |>
  # Arrange the results in a descending order
  arrange(desc(x = n)) |>
  # Print all rows of the resulting tibble in the console
  print(n = Inf) 

# To take into account:
# 
# Not all referees officiate the same amount of matches
# 
# Note on referees (source: <https://www.premierleague.com/news/102389>):
# The appointment of match officials is traditionally announced each Monday. If 
# there is a Premier League fixture on a Monday then the appointments of match 
# officials will be announced on the Tuesday. Select Group referees officiate 
# Premier League matches.
# Appointments are made by the Professional Game Match Officials Board. They 
# take into account several factors, including the referee's overall experience, 
# their current form, how often they have refereed the clubs involved, which 
# team the referee supports and any forthcoming international appointments.
# 
# The fewer matches a referee has officiated, the more uncertain our answer to 
# the RQ will be

# As a very sketchy way to determine when a referee favors the home team more 
# than it should, we can set as a threshold the percentage of home wins for the 
# whole season 
threshold <- df_premier_ref |> 
  count(ft_result) |> 
  mutate(p = (n / sum(n)) * 100) |> 
  filter(ft_result == "H") |> 
  pull(p)

# Calculate the proportion of home and away wins per referee
df_premier_ref_results <- df_premier_ref |> 
  group_by(referee) |> 
  # Count the number of matches per result
  count(ft_result) |> 
  # Add the percentages
  mutate(p = (n / sum(n)) * 100) |> 
  # Do not forget to ungroup!
  ungroup()
  
# Prepare the data to present the results in a table
tbl_premier_ref_results <- df_premier_ref_results |> 
  # For visualization purposes, we want a column with the unique referees, so we
  # need to pivot the counts and percentages to additional columns and thus make
  # the tibble wider
  pivot_wider(
    # Take the names for the new columns from the full time result variable
    names_from = ft_result,
    # And the values to populate the new columns from the counts and percentages
    values_from = c(n, p)
  ) |> 
  # With this pivoting we introduced 2 NA values in our data. We can replace 
  # them with zeros
  replace_na(list(n_D = 0, p_D = 0)) |> 
  # Add a column with the total number of matches officiated
  # To do so, we need to operate across rows, no columns
  rowwise() |> 
  mutate(n_matches = sum(c_across(n_A:n_H))) |> 
  ungroup() |> 
  # Arrange the results by our variable of interest
  arrange(desc(p_H)) |> 
  # Relocate columns to facilitate interpretation
  select(
    referee,
    n_H,
    n_D,
    n_A,
    p_H,
    p_D,
    p_A,
    n_matches
  ) 

# Present the results in a table
tbl_premier_ref_results |> 
  filter(
    # Here I'm adding a 10% error to the threshold
    p_H > threshold + 10,
    # Remove referees with a low count of matches officiated
    n_matches > 9
  ) 
