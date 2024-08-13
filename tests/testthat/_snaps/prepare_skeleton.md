# skeleton snapshots

    Code
      constructive::construct(skeleton_snapshot)
    Output
      tibble::tibble(
        age = rep(0:100, 16),
        sex = factor(rep(rep(c("m", "f"), 8), each = 101L), levels = c("m", "f")),
        nat = factor(rep(rep(c("ch", "int"), 4), each = 202L)),
        year = rep(2019:2022, each = 404L),
        spatial_unit = factor(rep("Canton", 1616L)),
      ) |>
        structure(
          out.attrs = list(
            dim = c(age = 101L, sex = 2L, nat = 2L, year = 4L, spatial_unit = 1L),
            dimnames = list(
              age = c(
                "age=  0", "age=  1", "age=  2", "age=  3", "age=  4", "age=  5", "age=  6",
                "age=  7", "age=  8", "age=  9", "age= 10", "age= 11", "age= 12", "age= 13",
                "age= 14", "age= 15", "age= 16", "age= 17", "age= 18", "age= 19", "age= 20",
                "age= 21", "age= 22", "age= 23", "age= 24", "age= 25", "age= 26", "age= 27",
                "age= 28", "age= 29", "age= 30", "age= 31", "age= 32", "age= 33", "age= 34",
                "age= 35", "age= 36", "age= 37", "age= 38", "age= 39", "age= 40", "age= 41",
                "age= 42", "age= 43", "age= 44", "age= 45", "age= 46", "age= 47", "age= 48",
                "age= 49", "age= 50", "age= 51", "age= 52", "age= 53", "age= 54", "age= 55",
                "age= 56", "age= 57", "age= 58", "age= 59", "age= 60", "age= 61", "age= 62",
                "age= 63", "age= 64", "age= 65", "age= 66", "age= 67", "age= 68", "age= 69",
                "age= 70", "age= 71", "age= 72", "age= 73", "age= 74", "age= 75", "age= 76",
                "age= 77", "age= 78", "age= 79", "age= 80", "age= 81", "age= 82", "age= 83",
                "age= 84", "age= 85", "age= 86", "age= 87", "age= 88", "age= 89", "age= 90",
                "age= 91", "age= 92", "age= 93", "age= 94", "age= 95", "age= 96", "age= 97",
                "age= 98", "age= 99", "age=100"
              ),
              sex = c("sex=m", "sex=f"),
              nat = c("nat=ch", "nat=int"),
              year = c("year=2019", "year=2020", "year=2021", "year=2022"),
              spatial_unit = "spatial_unit=Canton"
            )
          )
        )

