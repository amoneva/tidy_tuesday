# ==============================================================================
# Title: NSC-R TidyTuesday
# Author: Asier Moneva
# Affiliation: 
#   - Netherlands Institute for the Study of Crime and Law Enforcement (NCSR)
#   - Center of Expertise Cyber Security, The Hague University of Applied 
#   Sciences
# Date: 2024-02-24
# ==============================================================================

# Install the {tidytuesdayR} package
# install.packages(c("tidytuesdayR", "tidyverse"))

# Load the packages
library (here)
library (tidytuesdayR)
library (tidyverse)

# Load data for TidyTuesday (TT) week 9
data_tt_w9 <- tt_load(
  x = 2024, 
  week = 9
)

# Store each data frame as a separate object
df_events <- data_tt_w9$events
df_births <- data_tt_w9$births
df_deaths <- data_tt_w9$deaths

# Question: Which cohort of leap day births is most represented in ----
# Wikipedia's data?

# Using the births data frame
df_births |> 
  # Group the observations by year
  group_by(year_birth) |> 
  # Count them, and
  count() |> 
  # (Good practice: ungroup the data)
  ungroup() |> 
  # Arrange them in descending order
  arrange(desc(n)) |> 
  # Extract only the observation(s) with the highest number of entries
  slice_max(
    order_by = n, 
    n = 1
  )

# Answer: The 1984 cohort is the most represented in Wikipedia's data, with 8 
# entries.

# Question: Are any years surprisingly underrepresented compared to nearby ----
# years? 

# `Surprisingly underrepresented`: a year with at least three fewer entries than 
# nearby years.
# `Nearby year`: a year that is one year away from the reference year.

# Re-using part of the previous code
df_births_ <- df_births |> 
  group_by(year_birth) |> 
  count() |> 
  ungroup() |> 
  # Identify `surprisingly underrepresented` years
  # Right join a new data frame with all the missing years in the data
  right_join(
    y = tibble(year_birth = full_seq(
      x = df_births |> pull(year_birth), 
      period = 1
    )),
    by = join_by(year_birth)
  ) |> 
  # Fill in the newly added years with zeroes
  replace_na(list(n = 0)) |> 
  # Arrange the data in descendant order of years
  arrange(desc(year_birth)) |> 
  # Lag and lead `n` to be able to compare its value with the immediately 
  # preceding and following one
  mutate(
    n_lagged = lag(n),
    n_led = lead(n),
    surp_under = if_else(
      condition = n < n_lagged - 3 | n < n_led - 3,
      true = 0,
      false = NA
    )
  )

# Extract the surprisingly underrepresented years as a character vector
v_surp_under_years <- df_births_ |> 
  filter(surp_under == 0) |> 
  pull(year_birth)

# Answer: Yes, there are 26 surprisingly underrepresented years compared 
# to nearby years, all from the 1900s: 1993, 1991, 1989, 1987, 1985, 1983, 1981, 
# 1979, 1977, 1975, 1973, 1971, 1969, 1967, 1945, 1943, 1937, 1935, 1933, 1931, 
# 1929, 1927, 1921, 1919, 1909, and 1907.

# Visualize the data
fig_tt_w9 <- df_births_ |> 
  ggplot(mapping = aes(x = year_birth)) +
  # Add a column geom for the count of entries
  geom_col(mapping = aes(y = n)) +
  # Add a point geom for the surprisingly underrepresented years
  geom_point(
    mapping = aes(y = surp_under), 
    # NSCR pink!
    color = "#B81E61",
    na.rm = TRUE,
    size = 0.25
  ) +
  # Annotate the answer
  annotate(
    geom = "text",
    x = 1800,
    y = 5,
    label = str_wrap(
      string = paste(v_surp_under_years, collapse = ", "),
      width = 30
    ),
    color = "#B81E61",
    size = 3
  ) +
  # Break the x-axis every 100 years
  scale_x_continuous(breaks = seq(
    from = round(min(df_births_$year_birth), -2), 
    to = round(max(df_births_$year_birth), -2), 
    by = 100
  )) +
  # Plot text
  labs(
    title = "Wikipedia birth entries for February 29th",
    subtitle = "There are 26 years that are 'surprisingly underrepresented' compared to nearby years:",
    x = "Birth year",
    y = "Number\nof entries",
    caption = "Plot created using the #TidyTuesday dataset for week 9 of 2024: Leap Day\nLink: https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-02-27\nAsier Moneva (@crimoneva)"
  ) +
  theme_classic() +
  theme(
    # Color the subtitle in the same way as the answer
    plot.subtitle = element_text(color = "#B81E61"),
    # Rotate the title of the y-axis vertically
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5
    )
  )

# Save the plot
ggsave(
  filename = "2024-week9.png",
  plot = fig_tt_w9,
  device = "png",
  path = here("scripts", "tidy-tuesdays", "plots"),
  width = 8,
  height = 3,
  dpi = 300
)

# (Bonus) What other patterns can you find in the data? ----


