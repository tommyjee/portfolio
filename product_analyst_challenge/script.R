# ----------------------------------------------------------------------------------------
# Importing data
# ----------------------------------------------------------------------------------------

# Load dependencies
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Export tables in challenge database
account <- read_rds("data/account.rds")
account_variant <- read_rds("data/account_variant.rds")

# Main data of interest
dat <- account %>% left_join(account_variant, by = c("id" = "account_id")) %>%
  select(id, is_mobile, country_id, entered_cc, balance, started_video,
         finished_video, source, variant_id) %>%
  # Create two binary variables: usa and is_balance denoting whether user is from USA
  # and whether user has balance
  mutate(usa = ifelse(country_id == 1, 1, 0),
         is_balance = ifelse(balance != 0, 1, 0)) %>%
  # Remove observations where user did not start video, but finished the video
  filter(!(started_video == 0 & finished_video == 1))

# ----------------------------------------------------------------------------------------
# High-level analysis: Control (not showing video) is best
# ----------------------------------------------------------------------------------------

# Compare variants: 411 is control, 412 is top, 413 is bottom
temp <- dat %>% group_by(variant_id) %>%
  summarize(start_counseling_rate = mean(entered_cc))

# Create bar plot
ggplot(data = temp, aes(x = variant_id, y = start_counseling_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_cartesian(ylim = c(0.09, 0.1)) +
  ggtitle("Conversion rate by experiment variant")

# ----------------------------------------------------------------------------------------
# Program to find which combinations of categorical variables, when coupled with variant_id
# result in higher conversion rate for variants 412 or 413 than for 411
# ----------------------------------------------------------------------------------------

# Categorical variables to be coupled with variant_id
cat_vars <- c("is_mobile", "is_balance", "usa", "source")
num_vars <- length(cat_vars)
num_res <- 0
max_df <- data.frame()
max_diff <- 0

for (num in 1:num_vars) {
  # All subsets of size less than or equal to num_vars
  combn <- combn(cat_vars, num, simplify = FALSE)
  for (i in 1:length(combn)) {
    # Getting all combinations of variables and values
    grouping_vars <- c("variant_id", combn[[i]])
    comparison_tab <- dat %>% 
      group_by_at(vars(one_of(grouping_vars))) %>%
      summarize(start_counseling_rate = mean(entered_cc)) %>%
      ungroup()
    
    # Number of comparisons across variant groups
    num_row <- nrow(comparison_tab)
    compare_num <- num_row / 3
    
    for (j in 1:compare_num) {
      # Compute differences
      diff_412 <- comparison_tab[["start_counseling_rate"]][j] - 
        comparison_tab[["start_counseling_rate"]][j + compare_num]
      diff_413 <- comparison_tab[["start_counseling_rate"]][j] -
        comparison_tab[["start_counseling_rate"]][j + 2*compare_num]
      
      # Save variable combinations of interest
      print_df <- comparison_tab[j, ] %>%
        select(-one_of("variant_id", "start_counseling_rate")) %>%
        ungroup()
      
      # Top is better
      if (diff_412 < 0 && diff_412 < diff_413) {
        print("Show video at the TOP when")
        print(print_df)
        # browser()
        cat("\n\n")
        num_res <- num_res + 1
        
        # Finding biggest difference to include in slides
        if (max_diff < diff_412) {
          max_diff <- max_diff
        }
        else {
          max_diff <- diff_412
          max_df <- print_df
        }
      }
      
      # Bottom is better
      else if (diff_413 < 0 && diff_413 < diff_412) {
        print("Show video at the BOTTOM when")
        print(print_df)
        # browser()
        cat("\n\n")
        num_res <- num_res + 1
        
        # Finding biggest difference to include in slides
        if (max_diff < diff_413) {
          max_diff <- max_diff
        }
        else {
          max_diff <- diff_413
          max_df <- print_df
        }
      }
    }
  }
}

print("Otherwise, do not show the video at all.")
