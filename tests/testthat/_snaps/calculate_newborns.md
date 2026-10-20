# calculate newborns test

    Code
      dput(calculate_newborns_default)
    Output
      structure(list(year = c(2025, 2025, 2025, 2025), scen = c("reference", 
      "reference", "reference", "reference"), spatial_unit = c("Aargau", 
      "Aargau", "Aargau", "Aargau"), nat = c("ch", "ch", "int", "int"
      ), sex = c("m", "f", "m", "f"), births = c(2633.22545158275, 
      2507.83376341215, 1228.27811709227, 1169.78868294502), age = c(0, 
      0, 0, 0), birthrate = c(0, 0, 0, 0), int_mothers = c(0.249921801689084, 
      0.249921801689084, 0.249921801689084, 0.249921801689084), mor = c(0.003395, 
      0.002766, 0.003226, 0.002525), emi_int = c(0.002279, 0.00319, 
      0.008958, 0.004274), emi_nat = c(0.007976, 0.008772, 0.004886, 
      0.003419), acq = c(0, 0, 0.001629, 0.002564), imm_int_n = c(5, 
      4, 17, 20), imm_nat_n = c(47, 57, 20, 19), n_jan = c(NA, NA, 
      NA, NA), mor_n = c(8.99637530599991, 6.99383457300868, 4.00112963903672, 
      2.99917322759254), emi_int_n = c(6.0011208041571, 7.99998970528475, 
      11.0029153729125, 4.999676830907), emi_nat_n = c(21.002606201824, 
      21.9987177726513, 6.00136688011282, 3.99950750698901), acq_n = c(2.0008650527433, 
      2.99933818307102, -2.0008650527433, -2.99933818307102), n_dec = c(2651.22621432352, 
      2534.84055954427, 1242.27184014746, 1193.79098719646)), row.names = c(NA, 
      -4L), class = c("tbl_df", "tbl", "data.frame"))

---

    Code
      calculate_newborns_default
    Output
      # A tibble: 4 x 21
         year scen      spatial_unit nat   sex   births   age birthrate int_mothers
        <dbl> <chr>     <chr>        <chr> <chr>  <dbl> <dbl>     <dbl>       <dbl>
      1  2025 reference Aargau       ch    m      2633.     0         0       0.250
      2  2025 reference Aargau       ch    f      2508.     0         0       0.250
      3  2025 reference Aargau       int   m      1228.     0         0       0.250
      4  2025 reference Aargau       int   f      1170.     0         0       0.250
      # i 12 more variables: mor <dbl>, emi_int <dbl>, emi_nat <dbl>, acq <dbl>,
      #   imm_int_n <dbl>, imm_nat_n <dbl>, n_jan <lgl>, mor_n <dbl>,
      #   emi_int_n <dbl>, emi_nat_n <dbl>, acq_n <dbl>, n_dec <dbl>

---

    Code
      dput(calculate_newborns_net)
    Output
      structure(list(year = c(2025, 2025, 2025, 2025), scen = c("reference", 
      "reference", "reference", "reference"), spatial_unit = c("Aargau", 
      "Aargau", "Aargau", "Aargau"), nat = c("ch", "ch", "int", "int"
      ), sex = c("m", "f", "m", "f"), births = c(2633.22545158275, 
      2507.83376341215, 1228.27811709227, 1169.78868294502), age = c(0, 
      0, 0, 0), birthrate = c(0, 0, 0, 0), int_mothers = c(0.249921801689084, 
      0.249921801689084, 0.249921801689084, 0.249921801689084), mor = c(0.003395, 
      0.002766, 0.003226, 0.002525), emi_int = c(0.002279, 0.00319, 
      0.008958, 0.004274), emi_nat = c(0.007976, 0.008772, 0.004886, 
      0.003419), acq = c(0, 0, 0.001629, 0.002564), imm_int_n = c(5, 
      4, 17, 20), imm_nat_n = c(47, 57, 20, 19), mig_sub = c(1, 1, 
      1, 1), n_jan = c(NA, NA, NA, NA), mor_n = c(8.99637530599991, 
      6.99383457300868, 4.00112963903672, 2.99917322759254), emi_int_n = c(6.0011208041571, 
      7.99998970528475, 11.0029153729125, 4.999676830907), emi_nat_n = c(21.002606201824, 
      21.9987177726513, 6.00136688011282, 3.99950750698901), acq_n = c(2.0008650527433, 
      2.99933818307102, -2.0008650527433, -2.99933818307102), n_dec = c(2652.22621432352, 
      2535.84055954427, 1243.27184014746, 1194.79098719646)), row.names = c(NA, 
      -4L), class = c("tbl_df", "tbl", "data.frame"))

---

    Code
      calculate_newborns_net
    Output
      # A tibble: 4 x 22
         year scen      spatial_unit nat   sex   births   age birthrate int_mothers
        <dbl> <chr>     <chr>        <chr> <chr>  <dbl> <dbl>     <dbl>       <dbl>
      1  2025 reference Aargau       ch    m      2633.     0         0       0.250
      2  2025 reference Aargau       ch    f      2508.     0         0       0.250
      3  2025 reference Aargau       int   m      1228.     0         0       0.250
      4  2025 reference Aargau       int   f      1170.     0         0       0.250
      # i 13 more variables: mor <dbl>, emi_int <dbl>, emi_nat <dbl>, acq <dbl>,
      #   imm_int_n <dbl>, imm_nat_n <dbl>, mig_sub <dbl>, n_jan <lgl>, mor_n <dbl>,
      #   emi_int_n <dbl>, emi_nat_n <dbl>, acq_n <dbl>, n_dec <dbl>

---

    Code
      dput(calculate_newborns_rate)
    Output
      structure(list(year = c(2025, 2025, 2025, 2025), scen = c("reference", 
      "reference", "reference", "reference"), spatial_unit = c("Aargau", 
      "Aargau", "Aargau", "Aargau"), nat = c("ch", "ch", "int", "int"
      ), sex = c("m", "f", "m", "f"), births = c(2633.22545158275, 
      2507.83376341215, 1228.27811709227, 1169.78868294502), age = c(0, 
      0, 0, 0), birthrate = c(0, 0, 0, 0), int_mothers = c(0.249921801689084, 
      0.249921801689084, 0.249921801689084, 0.249921801689084), mor = c(0.003395, 
      0.002766, 0.003226, 0.002525), emi_int = c(0.002279, 0.00319, 
      0.008958, 0.004274), emi_nat = c(0.007976, 0.008772, 0.004886, 
      0.003419), acq = c(0, 0, 0.001629, 0.002564), imm_int_n = c(5, 
      4, 17, 20), imm_nat_n = c(47, 57, 20, 19), mig_sub = c(1, 1, 
      1, 1), emi_sub = c(0.7, 0.7, 0.7, 0.7), imm_sub = c(0.3, 0.3, 
      0.3, 0.3), n_jan = c(NA, NA, NA, NA), mor_n = c(8.99637530599991, 
      6.99383457300868, 4.00112963903672, 2.99917322759254), emi_int_n = c(6.0011208041571, 
      7.99998970528475, 11.0029153729125, 4.999676830907), emi_nat_n = c(21.002606201824, 
      21.9987177726513, 6.00136688011282, 3.99950750698901), acq_n = c(2.0008650527433, 
      2.99933818307102, -2.0008650527433, -2.99933818307102), n_dec = c(1363.13599414796, 
      1307.70149917877, 641.386356947657, 621.318193077386), emi_sub_n = c(1840.12888596508, 
      1753.05580052214, 858.407833142578, 817.818277312959), emi_sub_n_total = c(1840.12888596508, 
      1753.05580052214, 858.407833142578, 817.818277312959), imm_sub_n = c(552.038665789525, 
      525.916740156643, 257.522349942774, 245.345483193888)), row.names = c(NA, 
      -4L), class = c("tbl_df", "tbl", "data.frame"))

---

    Code
      calculate_newborns_rate
    Output
      # A tibble: 4 x 27
         year scen      spatial_unit nat   sex   births   age birthrate int_mothers
        <dbl> <chr>     <chr>        <chr> <chr>  <dbl> <dbl>     <dbl>       <dbl>
      1  2025 reference Aargau       ch    m      2633.     0         0       0.250
      2  2025 reference Aargau       ch    f      2508.     0         0       0.250
      3  2025 reference Aargau       int   m      1228.     0         0       0.250
      4  2025 reference Aargau       int   f      1170.     0         0       0.250
      # i 18 more variables: mor <dbl>, emi_int <dbl>, emi_nat <dbl>, acq <dbl>,
      #   imm_int_n <dbl>, imm_nat_n <dbl>, mig_sub <dbl>, emi_sub <dbl>,
      #   imm_sub <dbl>, n_jan <lgl>, mor_n <dbl>, emi_int_n <dbl>, emi_nat_n <dbl>,
      #   acq_n <dbl>, n_dec <dbl>, emi_sub_n <dbl>, emi_sub_n_total <dbl>,
      #   imm_sub_n <dbl>

# calculate newborns test only Swiss NA

    Code
      dput(calculate_newborns_na)
    Output
      structure(list(year = c(2025, 2025, 2025, 2025), scen = c("reference", 
      "reference", "reference", "reference"), spatial_unit = c("Aargau", 
      "Aargau", "Aargau", "Aargau"), nat = c("ch", "ch", "int", "int"
      ), sex = c("m", "f", "m", "f"), births = c(2633.22545158275, 
      2507.83376341215, 1228.27811709227, 1169.78868294502), age = c(0, 
      0, 0, 0), birthrate = c(NA_real_, NA_real_, NA_real_, NA_real_
      ), int_mothers = c(NA_real_, NA_real_, NA_real_, NA_real_), mor = c(NA_real_, 
      NA_real_, NA_real_, NA_real_), emi_int = c(NA_real_, NA_real_, 
      NA_real_, NA_real_), emi_nat = c(NA_real_, NA_real_, NA_real_, 
      NA_real_), acq = c(NA_real_, NA_real_, NA_real_, NA_real_), imm_int_n = c(NA_real_, 
      NA_real_, NA_real_, NA_real_), imm_nat_n = c(NA_real_, NA_real_, 
      NA_real_, NA_real_), n_jan = c(NA, NA, NA, NA), mor_n = c(NA_real_, 
      NA_real_, NA_real_, NA_real_), emi_int_n = c(NA_real_, NA_real_, 
      NA_real_, NA_real_), emi_nat_n = c(NA_real_, NA_real_, NA_real_, 
      NA_real_), acq_n = c(NA_real_, NA_real_, NA_real_, NA_real_), 
          n_dec = c(NA_real_, NA_real_, NA_real_, NA_real_)), row.names = c(NA, 
      -4L), class = c("tbl_df", "tbl", "data.frame"))

---

    Code
      calculate_newborns_na
    Output
      # A tibble: 4 x 21
         year scen   spatial_unit nat   sex   births   age birthrate int_mothers   mor
        <dbl> <chr>  <chr>        <chr> <chr>  <dbl> <dbl>     <dbl>       <dbl> <dbl>
      1  2025 refer~ Aargau       ch    m      2633.     0        NA          NA    NA
      2  2025 refer~ Aargau       ch    f      2508.     0        NA          NA    NA
      3  2025 refer~ Aargau       int   m      1228.     0        NA          NA    NA
      4  2025 refer~ Aargau       int   f      1170.     0        NA          NA    NA
      # i 11 more variables: emi_int <dbl>, emi_nat <dbl>, acq <dbl>,
      #   imm_int_n <dbl>, imm_nat_n <dbl>, n_jan <lgl>, mor_n <dbl>,
      #   emi_int_n <dbl>, emi_nat_n <dbl>, acq_n <dbl>, n_dec <dbl>

