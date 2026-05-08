# calculate_emi_rate snapshot - mean, age_group=5, binational, two_sex

    Code
      print(tail(result, 100))
    Output
      # A tibble: 100 x 12
          year spatial_unit   age age_group nat   sex   n_jan births n_base emi_n
         <int> <chr>        <dbl> <chr>     <chr> <chr> <int>  <int>  <int> <int>
       1  2025 3                1 age_0_4   int   f       113      0    113    13
       2  2025 3                2 age_0_4   int   f       114      0    114     6
       3  2025 3                3 age_0_4   int   f       126      0    126    11
       4  2025 3                4 age_0_4   int   f       139      0    139     6
       5  2025 3                5 age_5_9   int   f       123      0    123     8
       6  2025 3                6 age_5_9   int   f       134      0    134     3
       7  2025 3                7 age_5_9   int   f       130      0    130     5
       8  2025 3                8 age_5_9   int   f       126      0    126     1
       9  2025 3                9 age_5_9   int   f       132      0    132     0
      10  2025 3               10 age_10_14 int   f       128      0    128     1
      # i 90 more rows
      # i 2 more variables: emi_rate <dbl>, method <chr>

# calculate_emi_rate snapshot - median, age_group=7, 2023 & 2025, one sex

    Code
      print(tail(result2, 100))
    Output
      # A tibble: 100 x 11
          year spatial_unit   age age_group nat   n_jan births n_base emi_n emi_rate
         <int> <chr>        <dbl> <chr>     <chr> <int>  <int>  <int> <int>    <dbl>
       1  2025 3                1 age_0_6   int     113      0    113    13   0.0555
       2  2025 3                2 age_0_6   int     114      0    114     6   0.0555
       3  2025 3                3 age_0_6   int     126      0    126    11   0.0555
       4  2025 3                4 age_0_6   int     139      0    139     6   0.0555
       5  2025 3                5 age_0_6   int     123      0    123     8   0.0555
       6  2025 3                6 age_0_6   int     134      0    134     3   0.0555
       7  2025 3                7 age_7_13  int     130      0    130     5   0.0175
       8  2025 3                8 age_7_13  int     126      0    126     1   0.0175
       9  2025 3                9 age_7_13  int     132      0    132     0   0.0175
      10  2025 3               10 age_7_13  int     128      0    128     1   0.0175
      # i 90 more rows
      # i 1 more variable: method <chr>

