# Libraries
library(ggplot2)
library(tidyverse)

# Defining the data
seal_presence <- tribble(
  ~class,             ~month, ~presence,
  "Weanlings",        "Jan", "X",
  "Weanlings",        "May", "X",
  "Weanlings",        "Jun", "X",
  "Weanlings",        "Jul", "X",
  "Weanlings",        "Aug", "X",
  "Weanlings",        "Sep", "X",
  "Weanlings",        "Oct", "X",
  "Weanlings",        "Nov", "X",
  "Weanlings",        "Dec", "X",
  
  "Yearlings",        "May", "X",
  "Yearlings",        "Jun", "X",
  "Yearlings",        "Jul", "X",
  "Yearlings",        "Aug", "X",
  "Yearlings",        "Sep", "X",
  "Yearlings",        "Nov", "M",
  "Yearlings",        "Dec", "M",
  
  "Juveniles",        "Jan", "M",
  "Juveniles",        "Feb", "M",
  "Juveniles",        "May", "X",
  "Juveniles",        "Jun", "X",
  "Juveniles",        "Jul", "X",
  "Juveniles",        "Aug", "X",
  "Juveniles",        "Sep", "X",
  "Juveniles",        "Nov", "M",
  "Juveniles",        "Dec", "M",
  
  "Sub-adult males",  "Feb", "M",
  "Sub-adult males",  "Mar", "M",
  "Sub-adult males",  "Oct", "B",
  "Sub-adult males",  "Nov", "B",
  "Sub-adult males",  "Dec", "B",
  
  "Adult males",      "Feb", "M",
  "Adult males",      "Mar", "M",
  "Adult males",      "Apr", "M",
  "Adult males",      "Oct", "B",
  "Adult males",      "Nov", "B",
  "Adult males",      "Dec", "B",
  
  "Adult females",    "Jan", "M",
  "Adult females",    "Feb", "M",
  "Adult females",    "Mar", "M",
  "Adult females",    "Oct", "B",
  "Adult females",    "Nov", "B",
  "Adult females",    "Dec", "B"
)


# Format factors
seal_presence <- seal_presence %>%
  mutate(
    month = factor(month, levels = month.abb),
    class = factor(class, levels = rev(c(
      "Weanlings", "Yearlings", "Juveniles",
      "Sub-adult males", "Adult males", "Adult females"
    )))
  )

# Colors!!
presence_colors <- c(
  "X" = "#A9DFBF",  # Green = General presence
  "M" = "#85C1E9",  # Blue = Molting
  "B" = "#F5B7B1"   # Red = Breeding
)

# Plot
ggplot(seal_presence, aes(x = month, y = class, fill = presence)) +
  geom_tile(color = "white", linewidth = 0.7, height = 0.9) +
  scale_fill_manual(values = presence_colors) +
  theme_minimal() +
  labs(
    title = "Elephant Seal Shore Presence by Class",
    subtitle = "Observed monthly presence by life stage",
    caption = "B = Breeding; M = Molting; X = General presence",
    x = "Month", y = NULL, fill = "Activity"
  ) +
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    legend.position = "bottom"
  )


