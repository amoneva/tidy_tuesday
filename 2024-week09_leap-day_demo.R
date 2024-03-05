# ==============================================================================
# Title: NSC-R TidyTuesday
# Author: Asier Moneva
# Affiliation: 
#   - Netherlands Institute for the Study of Crime and Law Enforcement (NCSR)
#   - Center of Expertise Cyber Security, The Hague University of Applied 
#   Sciences
# Date: 2024-03-05
# Contributors: Wim Bernasco, Jens Branum
# ==============================================================================

# Load necessary libraries
library(tidyverse)      # For data manipulation and visualization
library(tidytuesdayR)   # For loading TidyTuesday datasets

# Load the data for TidyTuesday Week 9 of 2024
data_tt_w9 <- tidytuesdayR::tt_load(x = 2024, week = 9)

# Extract the 'births' dataset from the loaded data
df_births <- data_tt_w9$births

# Question 1
# Alternative 1a: Group by birth year, count occurrences, and sort them in 
# descending order
df_births %>% 
  group_by(year_birth) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

# Alternative 1b: Same as Alternative 1a, but with different function chaining
arrange(ungroup(count(group_by(df_births, year_birth))), desc(n))

# Alternative 2 (Wim): Count occurrences of births by year, sorted in descending
# order
df_births %>% count(year_birth, sort = TRUE)

# Question 2
# Analyze birth counts by year, considering lagged and leading counts, and 
# identify 'surprisingly underrepresented' entries
df_births_ <- df_births %>% 
  count(year_birth) %>% 
  arrange(desc(year_birth)) %>% 
  mutate(
    n_lagged = lag(n),
    n_led = lead(n),  # Find the "next" value in the `n` vector
    surp_under = if_else(
      condition = n < n_lagged - 5 | n < n_led - 5,
      true = 1,
      false = 0
    )
  ) 

# Explore the distribution of birth years using a histogram
df_births %>% 
  ggplot(mapping = aes(
    x = year_birth
  )) +
  geom_histogram()

# Final plot based on the analysis of birth counts and surprisingly 
# underrepresented entries
df_births_ %>% 
  ggplot(mapping = aes(
    x = year_birth,
    y = n,
    fill = surp_under
  )) +
  geom_col() +
  scale_fill_continuous(breaks = c(0, 1))

# Alternative 2 (Jens): Plot using ggplot with filtered surprisingly 
# underrepresented events
ggplot(
  data = filter(df_births_, !is.na(surp_under)),
  mapping = aes(
    x = year_birth,
    y = n,
    fill = surp_under
  )
) +
  geom_col() +
  scale_fill_continuous(breaks = c(0, 1))
