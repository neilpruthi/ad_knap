# Description --------------------------------------------------------------------------------------
# Generate plots for CS590.2 final writeup.
# BE on 2018-12-10

# Setup --------------------------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggthemes)
theme_set(
  theme_tufte() +
    theme(panel.background = element_rect(fill = "grey90"))
)

# Figure 1 -----------------------------------------------------------------------------------------
uniform_probs <- read.csv("data/uniform_probs.csv")

uniform_probs %>% 
  gather(var, val, -X) %>% 
  mutate(var = factor(var, levels = c("V1", "V2", "V3"), labels = c("U[-1, 1]", "U[-2, 2]", "U[-a, a]"))) %>%
  ggplot(aes(X, val, fill = var, colour = var)) +
  geom_line() +
  xlim(0, 100) +
  scale_colour_brewer(palette = "Set1", name = "Distribution") +
  xlab("a") + ylab("Probability of Winning (%)")
ggsave("plots/final/uniform_probs.png", width = 6, height = 3)
