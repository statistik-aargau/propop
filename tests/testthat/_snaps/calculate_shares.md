# calculate_shares snapshot1 - mean, age_group=5, binational, two_sex

    Code
      print(tail(result, 100))
    Output
      # A tibble: 100 x 10
          year spatial_unit   age age_group nat   sex   imm_n sum_imm_n imm_share
         <int> <chr>        <dbl> <chr>     <chr> <chr> <int>     <int>     <dbl>
       1  2025 3                1 age_0_4   int   f        11        53    0.208 
       2  2025 3                2 age_0_4   int   f         8        53    0.151 
       3  2025 3                3 age_0_4   int   f         4        53    0.0755
       4  2025 3                4 age_0_4   int   f         7        53    0.132 
       5  2025 3                5 age_5_9   int   f         9        43    0.209 
       6  2025 3                6 age_5_9   int   f         3        43    0.0698
       7  2025 3                7 age_5_9   int   f         4        43    0.0930
       8  2025 3                8 age_5_9   int   f         1        43    0.0233
       9  2025 3                9 age_5_9   int   f         2        43    0.0465
      10  2025 3               10 age_10_14 int   f         3        38    0.0789
      # i 90 more rows
      # i 1 more variable: method <chr>

# calculate_shares snapshot2 - median, age_group=7, 2023 & 2025, one sex

    Code
      print(tail(result2, 100))
    Output
      # A tibble: 100 x 9
          year spatial_unit   age age_group nat   imm_n sum_imm_n imm_share method    
         <int> <chr>        <dbl> <chr>     <chr> <int>     <int>     <dbl> <chr>     
       1  2025 3                1 age_0_6   int      11       157    0.0701 share 202~
       2  2025 3                2 age_0_6   int       8       157    0.0510 share 202~
       3  2025 3                3 age_0_6   int       4       157    0.0255 share 202~
       4  2025 3                4 age_0_6   int       7       157    0.0446 share 202~
       5  2025 3                5 age_0_6   int       9       157    0.0573 share 202~
       6  2025 3                6 age_0_6   int       3       157    0.0191 share 202~
       7  2025 3                7 age_7_13  int       4        99    0.0404 share 202~
       8  2025 3                8 age_7_13  int       1        99    0.0101 share 202~
       9  2025 3                9 age_7_13  int       2        99    0.0202 share 202~
      10  2025 3               10 age_7_13  int       3        99    0.0303 share 202~
      # i 90 more rows

