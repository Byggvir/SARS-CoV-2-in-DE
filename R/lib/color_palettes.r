# Color blind friendly palettes

# Source: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/#use-a-colorblind-friendly-palette
# # To use for fills, add
# bp + scale_fill_manual(values = cbp1)
# 
# # To use for line and point colors, add
# sp + scale_colour_manual(values=cbp1)

library("ggsci")

# The palette with grey:

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

