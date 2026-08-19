# 1. Extract fixed-effect coefficients from your model
# (Replace 'm_vigor' with whatever you named this specific lmer object)
beta_m <- fixef(pupil_state_reward)

# 2. Create a prediction grid for plotting the model slopes
grid_vigor <- expand.grid(
  rt = seq(
    min(rt_sub_data$rt, na.rm = TRUE),
    max(rt_sub_data$rt, na.rm = TRUE),
    length.out = 100
  ),
  reward_type = c(1, 2) # Level 1 = No Reward, Level 2 = Reward
)

# 3. Calculate predicted vigor_wc based on the model formula
grid_vigor <- grid_vigor %>%
  mutate(
    reward_label = ifelse(reward_type == 2, "Reward (2)", "No Reward (1)"),
    vigor_pred = case_when(
      reward_type == 1 ~ beta_m["(Intercept)"] + 
        (beta_m["rt"] * rt),
      
      reward_type == 2 ~ (beta_m["(Intercept)"] + beta_m["reward_type2"]) + 
        ((beta_m["rt"] + beta_m["reward_type2:rt"]) * rt)
    )
  )

# 4. Generate the publication-ready scatterplot with model fits
ggplot(rt_sub_data, aes(x = rt, y = vigor_wc, color = factor(reward_type))) +
  # Raw data points with transparency to avoid overplotting
  geom_point(alpha = 0.15, size = 1) +
  # Model prediction lines
  geom_line(data = grid_vigor, aes(x = rt, y = vigor_pred, color = reward_label), linewidth = 1.2) +
  scale_color_manual(
    values = c("No Reward (1)" = "#2b5c8f", "Reward (2)" = "#d95f02"),
    name = "Condition"
  ) +
  labs(
    title = "Relationship Between Reaction Time and Motor Vigor",
    subtitle = "Higher RT (delayed commitment) strongly predicts higher vigor across conditions.",
    x = "Reaction Time (rt)",
    y = "Vigor (vigor_wc)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )
