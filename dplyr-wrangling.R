#------- Section 1: Filter -------#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

# Look for an exact match: ==

penguins_biscoe <- penguins %>% filter(island == "Biscoe")
# use unique(penguins_biscoe$island) to check that only Biscoe observations are included

penguins_2007 <- penguins %>% filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie" & island == "Torgersen")
# Alternative: penguins %>% filter(species == "Adelie", island == "Torgersen")

# Create a subset from penguins that only contains Gentoo penguins observed in 2008
gentoo_2008 <- penguins %>% filter(species == "Gentoo", year == 2008)

# Create a subset that contains Gentoos and Adelies
gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

# Create a subset that contains observations where the island is Dream OR the year is 2009
Dream_or_2009 <- penguins %>% filter(island == "Dream" | year == 2009)

# Create ggplot chart with water temp on x-axis, crab size on y-axis
ggplot(pie_crab, aes(x = water_temp, y = size)) + 
  geom_point()

# Keep observations for NIB, ZI, DB, JC
# We can use the %in% operator to ask: Does the value in our column match ANY of the values IN this vector?

pie_sites <- pie_crab %>% filter(site %in% c("NIB", "ZI", "DB", "JC"))

# Run a line of code in the console to confirm that only the sites above remain in the new subset you created
# unique(pie_sites$site)

sites <- c("CC", "BB", "PIE")

pie_sites2 <- pie_crab %>% filter(site %in% sites)

# Create a subset using the %in% operator that includes sites PIE, ZI, NIB, BB, and CC
sites_3 <- pie_crab %>% filter(site %in% c("PIE", "ZI", "NIB", "BB", "CC"))

# Excluding filter statements
# != (asks is this NOT equal to this value?)

exclude_zi <- pie_crab %>% filter(site != "ZI")

# What if I want to exclude sites "BB", "CC", and "PIE"

exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB", "CC", "PIE"))

# Create a subset from pie_crab that only contains observations from NIB, CC, ZI for crabs with carapace size exceeding 13mm

large_crabs <- pie_crab %>% filter(size > 13 & 
                                     site %in% c("NIB", "CC", "ZI"))

#------- Selecting columns -------#

# Select individual columns by name, separate them by a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# Select a range of columns using:
crabs_subset2 <- pie_crab %>% select(site:air_temp)

# Select a range and an individual column
crabs_subset3 <- pie_crab %>% select(date:water_temp, name)

# Can reorder! 
pie_crab %>% select(name, water_temp, size)


#-------- Mutate! -------#

# Use dplyr::mutate() to add or update a column while keeping all existing columns

crabs_cm <- pie_crab %>% 
  mutate(size_cm = size / 10)

# What happens if I use mutate to add a new column containing the mean of the size column?

crabs_mean <- pie_crab %>% 
  mutate(size_mean = mean(size, na.rm = TRUE))

# mutate will overwrite a column if you use an already existing name
crabs_awesome <- pie_crab %>% 
  mutatue(name = "Teddy is Awesome")


# Reminder: group_by and summarize
mean_size_by_site <- pie_crab %>% 
  group_by(site) %>% 
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))

# What about a group_by and then mutate?
group_mutate <- pie_crab %>% 
  group_by(site) %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))


# What if I want to create a new column in pie_crab that contains "giant" if the size is greater than 20, or "not giant" if the size is less than or equal to 35?

# Use dplyr::case_when() to write if-else statements more easily
crabs_bin <- pie_crab %>% 
  mutate(size_bin = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))


sites_binned <- pie_crab %>% 
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High" # this is a catch-all meaning make everything else be high
  ))
