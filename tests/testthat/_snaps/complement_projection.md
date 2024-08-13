# snapshot test complement_projection()

    Code
      constructive::construct(output_table)
    Output
      tibble::tibble(
        age = 0:9,
        sex = factor(rep("m", 10L), levels = c("m", "f")),
        nat = factor(rep("ch", 10L), levels = c("ch", "int")),
        year = rep(2018L, 10L),
        spatial_levels = factor(rep("Aargau", 10L)),
        n = c(2506, 2589, 2568, 2635, 2710, 2608, 2569, 2611, 2634, 2512),
      )

