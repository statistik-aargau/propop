# # Create a package logo / hex sticker
# nolint start
# library(ggplot2)
# library(hexSticker)
# library(dplyr)
# library(showtext)
#
# ## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("inter")
#
# ## Automatically use showtext to render text for future devices
# showtext_auto()
#
# # Sample data
# x <- c(
#   1.00, 1.14, 1.28, 1.42, 1.57, 1.71, 1.85, 1.99, 2.13, 2.27, 2.41, 2.55, 2.69,
#   2.84, 2.98, 3.12, 3.26, 3.40, 3.54, 3.68, 3.82, 3.96, 4.11, 4.25, 4.39, 4.53,
#   4.67, 4.81, 4.95, 5.09, 5.23, 5.38, 5.52, 5.66, 5.80, 5.94, 6.08, 6.22, 6.36,
#   6.50, 6.64, 6.79, 6.93, 7.07, 7.21, 7.35, 7.49, 7.63, 7.77, 7.91, 8.06, 8.20,
#   8.34, 8.48, 8.62, 8.76, 8.90, 9.04, 9.18, 9.33, 9.47, 9.61, 9.75, 9.89, 10.03,
#   10.17, 10.31, 10.45, 10.60, 10.74, 10.88, 11.02, 11.16, 11.30, 11.44, 11.58,
#   11.72, 11.87, 12.01, 12.15, 12.29, 12.43, 12.57, 12.71, 12.85, 12.99, 13.14,
#   13.28, 13.42, 13.56, 13.70, 13.84, 13.98, 14.12, 14.26, 14.41, 14.55, 14.69,
#   14.88, 15
# )
# y <- c(
#   0.00, 0.14, 0.28, 0.43, 0.58, 0.74, 0.90, 1.07, 1.24, 1.41, 1.59, 1.77, 1.95,
#   2.13, 2.31, 2.49, 2.67, 2.84, 3.01, 3.18, 3.34, 3.50, 3.65, 3.80, 3.94, 4.08,
#   4.21, 4.34, 4.46, 4.58, 4.69, 4.80, 4.91, 5.01, 5.11, 5.20, 5.29, 5.38, 5.46,
#   5.54, 5.62, 5.69, 5.76, 5.82, 5.88, 5.94, 5.99, 6.04, 6.09, 6.13, 6.17, 6.21,
#   6.25, 6.28, 6.31, 6.34, 6.37, 6.39, 6.42, 6.44, 6.46, 6.48, 6.50, 6.52, 6.54,
#   6.56, 6.58, 6.59, 6.60, 6.61, 6.62, 6.63, 6.64, 6.65, 6.66, 6.67, 6.68, 6.69,
#   6.70, 6.71, 6.72, 6.73, 6.74, 6.75, 6.76, 6.77, 6.78, 6.79, 6.8, 6.81, 6.82,
#   6.83, 6.84, 6.85, 6.86, 6.87, 6.88, 6.89, 6.9, 6.95
# )
#
# data <- data.frame(
#   x,
#   y,
#   ci_low = y * 0.65,
#   ci_high = y * 1.25
# )
#
#
# # Plot line chart with confidence interval
# p <-
#   ggplot(data, aes(x = x)) +
#
#
#    # upper ci band
#   ## top
#   geom_ribbon(
#     aes(
#       ymin = ci_low * 1.81, ymax = ci_high * 1.01
#     ),
#     alpha = 1, fill = "#96D4FF"
#   ) +
#   ## middle
#   geom_ribbon(
#     aes(
#       ymin = ci_low * 1.67, ymax = ci_high * 0.943
#     ),
#     alpha = 0.75, fill = "#96D4FF"
#   ) +
#
#
#    ## centre line
#   geom_ribbon(aes(
#     ymin = ci_low * 1.4, ymax = ci_high * 0.87
#   ), alpha = 0.3, fill = "#96D4FF") +
#
#
#    # lower ci band
#   ## middle
#   geom_ribbon(aes(
#     ymin = ci_low * 1.25, ymax = ci_high * 0.729
#   ), alpha = 0.75, fill = "#96D4FF") +
#   ## bottom
#   geom_ribbon(aes(
#     ymin = ci_low * 1.11, ymax = ci_high * 0.655
#   ), alpha = 1, fill = "#96D4FF") +
#
#
#   geom_smooth(aes(y = y), linewidth = 0.7, colour = "white", method = "gam") +
#   ylim(-5, 20) +
#   xlim(-8.5, 16) +
#   theme_void() +
#   theme_transparent() +
#   annotate(
#     "text",
#     x = 8, y = 10, label = "propop", color = "white",
#     family = "inter", size = 23.5,
#     # fontface = "bold.italic",
#     angle = 0
#   ) +
#   annotate(
#     "text",
#     x = 10.7, y = 0.77, label = "Project population growth in Switzerland",
#     color = "white", family = "inter", size = 4, angle = 30
#   ) +
#   annotate(
#     "text",
#     x = 11, y = 0.37, label = "using the cohort component method",
#     color = "white", family = "inter", size = 4, angle = 30
#   )
#
#
# # Create and save sticker
# hexSticker::sticker(
#   p,
#   package = "",
#   p_size = 26,
#   p_color = "white",
#   p_x = .8,
#   p_y = 1.35,
#   s_x = 0.5,
#   s_y = 1.25,
#   s_width = 3.3,
#   s_height = 3.6,
#   h_fill = "#004774",
#   h_color = "#c0c0c0",
#   h_size = 2.3,
#   white_around_sticker = TRUE,
#   spotlight = FALSE,
#   l_x = 0.75,
#   l_y = 1.25,
#   l_alpha = .8,
#   l_width = 4,
#   p_family = "inter",
#   filename = "man/figures/logo.png"
# )
# nolint end
