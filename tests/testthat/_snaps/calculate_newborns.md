# calculate newborns test

    Code
      constructive::construct(calculate_newborns_default)
    Output
      tibble::tibble(
        year = rep(2024L, 4L),
        scen = rep("reference", 4L),
        nat = rep(c("ch", "int"), each = 2L),
        sex = factor(rep(c("m", "f"), 2), levels = c("m", "f")),
        age = numeric(4),
        spatial_unit = rep("Aargau", 4L),
        births = c(2633.2254515827535, 2507.8337634121463, 1228.2781170922676, 1169.7886829450167),
        birthrate = numeric(4),
        int_mothers = rep(0.2499218016890835, 4L),
        mor = c(0.003395, 0.002766, 0.003226, 0.002525),
        emi_int = c(0.002279, 0.00319, 0.008958, 0.004274),
        emi_nat = c(0.007976, 0.008772, 0.004886, 0.003419),
        acq = c(0, 0, 0.001629, 0.002564),
        imm_int_n = c(5, 4, 17, 20),
        imm_nat_n = c(47, 57, 20, 19),
        n_jan = rep(NA, 4L),
        emi_int_n = c(6.001120804157096, 7.999989705284747, 11.002915372912534, 4.999676830907002),
        emi_nat_n = c(21.002606201824044, 21.998717772651347, 6.001366880112819, 3.9995075069890125),
        acq_n = c(2.000865052743304, 2.9993381830710226, -2.000865052743304, -2.9993381830710226),
        mor_n = c(9.000903930569287, 6.999365352618264, 3.9968229418272823, 2.9941200255843694),
        n_dec = c(2651.2216856989467, 2534.835028764663, 1242.2761468446715, 1193.7960403984653),
      )

---

    Code
      constructive::construct(calculate_newborns_net)
    Output
      tibble::tibble(
        year = rep(2024L, 4L),
        scen = rep("reference", 4L),
        nat = rep(c("ch", "int"), each = 2L),
        sex = factor(rep(c("m", "f"), 2), levels = c("m", "f")),
        age = numeric(4),
        spatial_unit = rep("Aargau", 4L),
        births = c(2633.2254515827535, 2507.8337634121463, 1228.2781170922676, 1169.7886829450167),
        birthrate = numeric(4),
        int_mothers = rep(0.2499218016890835, 4L),
        mor = c(0.003395, 0.002766, 0.003226, 0.002525),
        emi_int = c(0.002279, 0.00319, 0.008958, 0.004274),
        emi_nat = c(0.007976, 0.008772, 0.004886, 0.003419),
        acq = c(0, 0, 0.001629, 0.002564),
        imm_int_n = c(5, 4, 17, 20),
        imm_nat_n = c(47, 57, 20, 19),
        mig_sub = rep(1, 4L),
        n_jan = rep(NA, 4L),
        emi_int_n = c(6.001120804157096, 7.999989705284747, 11.002915372912534, 4.999676830907002),
        emi_nat_n = c(21.002606201824044, 21.998717772651347, 6.001366880112819, 3.9995075069890125),
        acq_n = c(2.000865052743304, 2.9993381830710226, -2.000865052743304, -2.9993381830710226),
        mor_n = c(9.000903930569287, 6.999365352618264, 3.9968229418272823, 2.9941200255843694),
        n_dec = c(2652.2216856989467, 2535.835028764663, 1243.2761468446715, 1194.7960403984653),
      )

---

    Code
      constructive::construct(calculate_newborns_rate)
    Output
      tibble::tibble(
        year = rep(2024L, 4L),
        scen = rep("reference", 4L),
        nat = rep(c("ch", "int"), each = 2L),
        sex = factor(rep(c("m", "f"), 2), levels = c("m", "f")),
        age = numeric(4),
        spatial_unit = rep("Aargau", 4L),
        births = c(2633.2254515827535, 2507.8337634121463, 1228.2781170922676, 1169.7886829450167),
        birthrate = numeric(4),
        int_mothers = rep(0.2499218016890835, 4L),
        mor = c(0.003395, 0.002766, 0.003226, 0.002525),
        emi_int = c(0.002279, 0.00319, 0.008958, 0.004274),
        emi_nat = c(0.007976, 0.008772, 0.004886, 0.003419),
        acq = c(0, 0, 0.001629, 0.002564),
        imm_int_n = c(5, 4, 17, 20),
        imm_nat_n = c(47, 57, 20, 19),
        mig_sub = rep(1, 4L),
        emi_sub = rep(0.7, 4L),
        imm_sub = rep(0.3, 4L),
        n_jan = rep(NA, 4L),
        emi_int_n = c(6.001120804157096, 7.999989705284747, 11.002915372912534, 4.999676830907002),
        emi_nat_n = c(21.002606201824044, 21.998717772651347, 6.001366880112819, 3.9995075069890125),
        acq_n = c(2.000865052743304, 2.9993381830710226, -2.000865052743304, -2.9993381830710226),
        mor_n = c(9.000903930569287, 6.999365352618264, 3.9968229418272823, 2.9941200255843694),
        n_dec = rep(NA_real_, 4L),
        emi_sub_n = rep(NA_real_, 4L),
        emi_sub_n_total = rep(NA_real_, 4L),
        imm_sub_n = rep(NA_real_, 4L),
      )

# calculate newborns test only Swiss NA

    Code
      constructive::construct(calculate_newborns_na)
    Output
      tibble::tibble(
        year = rep(2024L, 4L),
        scen = rep("reference", 4L),
        nat = rep(c("ch", "int"), each = 2L),
        sex = factor(rep(c("m", "f"), 2), levels = c("m", "f")),
        age = numeric(4),
        spatial_unit = rep("Aargau", 4L),
        births = c(2633.2254515827535, 2507.8337634121463, 1228.2781170922676, 1169.7886829450167),
        birthrate = rep(NA_real_, 4L),
        int_mothers = rep(NA_real_, 4L),
        mor = rep(NA_real_, 4L),
        emi_int = rep(NA_real_, 4L),
        emi_nat = rep(NA_real_, 4L),
        acq = rep(NA_real_, 4L),
        imm_int_n = rep(NA_real_, 4L),
        imm_nat_n = rep(NA_real_, 4L),
        n_jan = rep(NA, 4L),
        emi_int_n = rep(NA_real_, 4L),
        emi_nat_n = rep(NA_real_, 4L),
        acq_n = rep(NA_real_, 4L),
        mor_n = rep(NA_real_, 4L),
        n_dec = rep(NA_real_, 4L),
      )

