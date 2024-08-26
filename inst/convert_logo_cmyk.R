## Export logo for printing
# nolint start
# library(magick)
#
# propop_logo <- image_read("man/figures/logo.png")
# image_info(propop_logo)
#
# logo_cmyk_iso <- image_convert(
#   propop_logo,
#   colorspace = 'cmyk',
#   profile = "man/figures/ISOcoated_v2_300_eci.icc"
# )
#
# image_info(logo_cmyk_iso)
#
# image_write(image = logo_cmyk_iso,
#             path = "man/figures/logo_cmyk_iso.png",
#             density = 300)
#
# nolint end
