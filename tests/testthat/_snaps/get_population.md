# get population snapshots - 1 region 1 year

    Code
      constructive::construct(get_population_snapshot)
    Output
      tibble::tibble(
        year = rep("2022", 404L),
        spatial_unit = rep("......4232 Geltwil", 404L),
        nat = rep(c("ch", "int"), each = 202L),
        sex = rep(rep(c("m", "f"), 2), each = 101L),
        age = rep(seq(0, 100, by = 1), 4),
        n = rep(
          c(
            0, 2, 0, 3, 0, 2, 1, 0, 2, 0, 2, 1, 0, 2, 0, 1, 0, 2, 1, 0, 2, 0, 1, 0, 2, 1,
            2, 4, 1, 2, 1, 4, 1, 3, 1, 3, 2, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 0, 2, 0, 1, 0,
            2, 1, 0, 2, 1, 0, 3, 1, 4, 3, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 4, 0, 1, 0, 1, 3,
            4, 1, 0, 2, 1, 4, 1, 0, 1, 2, 3, 1, 2, 0, 2, 1, 0, 1, 3, 2, 4, 1, 3, 2, 0, 1,
            2, 1, 0, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 1, 0, 1, 0
          ),
          c(2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 1L, 1L, 3L, 2L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
          1L, 2L, 1L, 4L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 3L, 4L, 1L, 1L, 1L, 
          1L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 1L, 1L, 18L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 2L, 2L, 1L, 4L, 1L, 1L, 1L, 1L, 3L, 4L, 2L, 2L, 1L, 
          1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 4L, 
          1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 5L, 12L, 
          1L, 12L, 1L, 11L, 1L, 6L, 1L, 1L, 1L, 4L, 1L, 2L, 2L, 1L, 2L, 
          2L, 1L, 1L, 1L, 11L, 1L, 6L, 2L, 45L, 1L, 11L, 1L, 13L, 1L, 1L, 
          1L, 2L, 2L, 2L, 1L, 7L, 1L, 8L, 1L, 42L)
        ),
      )

# get population snapshots - 2 regions & 2 years

    Code
      constructive::construct(get_population_snapshot2)
    Output
      tibble::tibble(
        year = rep("2022", 404L),
        spatial_unit = rep("......4232 Geltwil", 404L),
        nat = rep(c("ch", "int"), each = 202L),
        sex = rep(rep(c("m", "f"), 2), each = 101L),
        age = rep(seq(0, 100, by = 1), 4),
        n = rep(
          c(
            0, 2, 0, 3, 0, 2, 1, 0, 2, 0, 2, 1, 0, 2, 0, 1, 0, 2, 1, 0, 2, 0, 1, 0, 2, 1,
            2, 4, 1, 2, 1, 4, 1, 3, 1, 3, 2, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 0, 2, 0, 1, 0,
            2, 1, 0, 2, 1, 0, 3, 1, 4, 3, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 4, 0, 1, 0, 1, 3,
            4, 1, 0, 2, 1, 4, 1, 0, 1, 2, 3, 1, 2, 0, 2, 1, 0, 1, 3, 2, 4, 1, 3, 2, 0, 1,
            2, 1, 0, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 1, 0, 1, 0
          ),
          c(2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 1L, 1L, 3L, 2L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
          1L, 2L, 1L, 4L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 3L, 4L, 1L, 1L, 1L, 
          1L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 1L, 1L, 18L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 2L, 2L, 1L, 4L, 1L, 1L, 1L, 1L, 3L, 4L, 2L, 2L, 1L, 
          1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 4L, 
          1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 5L, 12L, 
          1L, 12L, 1L, 11L, 1L, 6L, 1L, 1L, 1L, 4L, 1L, 2L, 2L, 1L, 2L, 
          2L, 1L, 1L, 1L, 11L, 1L, 6L, 2L, 45L, 1L, 11L, 1L, 13L, 1L, 1L, 
          1L, 2L, 2L, 2L, 1L, 7L, 1L, 8L, 1L, 42L)
        ),
      )

