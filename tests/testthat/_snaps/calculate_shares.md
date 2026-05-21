# calculate_shares snapshot1 - mean, age_group=5, binational, two_sex

    Code
      print(tail(result, 100))
    Output
      # A tibble: 100 x 9
         spatial_unit   age age_group nat   sex   sum_imm_n total_imm_n imm_share
         <chr>        <dbl> <chr>     <chr> <chr>     <int>       <int>     <dbl>
       1 3                1 age_0_4   int   f            53         557    0.0952
       2 3                2 age_0_4   int   f            53         557    0.0952
       3 3                3 age_0_4   int   f            53         557    0.0952
       4 3                4 age_0_4   int   f            53         557    0.0952
       5 3                5 age_5_9   int   f            43         395    0.109 
       6 3                6 age_5_9   int   f            43         395    0.109 
       7 3                7 age_5_9   int   f            43         395    0.109 
       8 3                8 age_5_9   int   f            43         395    0.109 
       9 3                9 age_5_9   int   f            43         395    0.109 
      10 3               10 age_10_14 int   f            38         365    0.104 
      # i 90 more rows
      # i 1 more variable: method <chr>

# calculate_shares snapshot2 - median, age_group=7, 2023 & 2025, one sex

    Code
      print(tail(result2, 100))
    Output
      # A tibble: 100 x 8
         spatial_unit   age age_group nat   sum_imm_n total_imm_n imm_share method    
         <chr>        <dbl> <chr>     <chr>     <int>       <int>     <dbl> <chr>     
       1 3                1 age_0_6   int         157        1472     0.107 share 202~
       2 3                2 age_0_6   int         157        1472     0.107 share 202~
       3 3                3 age_0_6   int         157        1472     0.107 share 202~
       4 3                4 age_0_6   int         157        1472     0.107 share 202~
       5 3                5 age_0_6   int         157        1472     0.107 share 202~
       6 3                6 age_0_6   int         157        1472     0.107 share 202~
       7 3                7 age_7_13  int          99         965     0.103 share 202~
       8 3                8 age_7_13  int          99         965     0.103 share 202~
       9 3                9 age_7_13  int          99         965     0.103 share 202~
      10 3               10 age_7_13  int          99         965     0.103 share 202~
      # i 90 more rows

