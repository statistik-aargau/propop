#' Project population development (raw results)
#'
#' @description Core function that uses the cohort component method and matrix
#' algebra to project population development. The function can be used for
#' different spatial levels (e.g., cantons, municipalities) and for one scenario
#' at a time.
#'
#' This function provides projections in a \bold{raw} version in which
#' key information is missing (e.g., which age groups the rows represent).
#' To conveniently obtain an enriched, more informative output,
#' use the \bold{wrapper function} `propop::propop()` (which internally uses
#' `propop::project_raw()`).
#'
#' The parameters and start populations for different spatial levels
#' can be obtained from the Swiss Federal Statistical Office (FSO).
#' For instructions on how to download this information from
#' [STAT-TAB](https://www.bfs.admin.ch/bfs/en/home/services/recherche/stat-tab-online-data-search.html),
#' see
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' The projection parameters need to be passed on as a single data frame to
#' `project_raw` with (with the parameters as columns). The column types, names,
#' and factor levels need to match those specified below.
#'
#' The method used to calculate the projections is a 'cohort-component
#' analysis' implemented with matrices due to programming performance benefit
#' compared to data frames. In a nutshell, the starting population ('n') is
#' multiplied by the survival rate to obtain the number of people which
#' transition into the projected next year (year + 1). Then, the absolute
#' number of people immigrating from outside Switzerland and the migration saldo
#' for people from outside the respective canton is added to the surviving
#' population. This results in the starting population for projection the next
#' year. Newborn children are added aeparately to the new starting population
#' of each year.
#'
#' The starting population is clustered in 404 groups: 101 age groups times
#' two nationalities times 2 genders. The survival rate is calculated in the
#' function 'create_transition_matrix()' resulting in the matrix 'L'. We use the
#' rates for mortality, emigration towards countries outside Switzerland and the
#' rate for the acquisition of the Swiss citizenship by the foreign population
#' to calculate survival rates. The model from the FSO also includes the rate of
#' emigration to other cantons in the survival rate. In contrast, we include the
#' immi- and emigration from and to other cantons by adding the migration
#' balance (German = 'saldo') (immigration + emigration) afterwards.
#'
#' Steps in this function:
#' 1) Checks: Checking input data and parameter settings for correct formats.
#' 2) Data preparation: Preparing vectors e.g. for the projection time frame and
#'    creating empty vectors to be filled with data later on.
#' 3) Loop over years for calculating the projections
#'    - Subsetting parameters: Depending on the selected projection year and on
#'      the demographic unit, the parameters for mortality, fertility, acquisition
#'      of the Swiss citizenship as well as migration parameters are subset by
#'      demographic group.
#'    - Create matrices: Matrices are build for the survival rate, mortality,
#'      fertility and for calculating the number of newborn babies.
#'    - Creating vectors: Vectors are built.
#'    - Projection: The transition matrix 'L' is multiplied by the starting
#'      population for the next year. Migrating people are added in absolute
#'      numbers. People that are 100 years old and older are clustered into one
#'      age group (age = 100). The newborn babies are added to the resulting
#'      starting population for the next projection year.
#' 4) Aggregating the data: All projected years are aggregated into one data
#'    frame. The function 'propop()', in which this function is contained,
#'    automatically adds relevant meta data to the results.
#'
#'
#' @param parameters data frame containing the FSO rates and numbers to run the
#' projection for a specific spatial level (e.g., canton, municipality).
#'    * `year`: projection year.
#'    * `spatial_unit`: ID of spatial entity (e.g., canton, municipality) for
#'       which to run the projections.
#'    * `scen`: projection scenario, used to subset data frames with multiple
#'       scenarios (r = reference, l = low growth, h = high growth scenario).
#'    * `nat`: nationality (ch = Swiss; int = foreign / international).
#'    * `sex`: sex (f = female, m = male).
#'    * `age`: age classes; typically ranging from 0 to 100 (incl. >100).
#'    * `birth_rate`: number of children per year.
#'    * `births_int_ch` proportion of children with Swiss nationality born to
#'       non-Swiss mothers.
#'    * `mor`: prospective mortality rate (probability of death).
#'    * `acq`: rate of acquisition of Swiss citizenship.
#'    * `emi`: rate of people emigrating abroad.
#'    * `mig_ch`: national / inter-cantonal net migration
#'       (number of immigrants - number of emigrants).
#'    * `imm_int`: number of people immigrating from abroad.
#'    * `mig_sub`: within canton net migration. Useful to account for movements
#'       between different subregions (e.g., municipalities). This argument is
#'       \bold{optional.}
#'
#' @param year_first numeric, first year to be projected.
#' @param year_last numeric, last year to be projected.
#' @param age_groups numeric, number of age classes. Creates a vector with
#'        1-year age classes running from `0` to (`age_groups` - 1). Defaults to
#'        `101` (FSO standard number of age groups).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#' @param n number of people per demographic group and year; should be
#'        the year before `year_first`. Typically extracted from data frame
#'        created with `propop::get_population()`.
#' @param subregional boolean, TRUE indicates that subregional migration
#'        patterns (e.g., movement between municipalities within a canton)
#'        are part of the projection.
#'
#' @returns Returns an unformatted and unlabeled data frame. It includes the
#'      number of people for each demographic group per year (starting year and
#'      projected years. The number of rows corresponds to the product of years
#'      and demographic groups (e.g., nationality (2) X sex (2) X age groups (101) = 404).
#'      Variables included in the output:
#'      \item{n}{number of people per demographic group.}
#'      \item{IMM_INT}{number of immigrants from other countries.}
#'      \item{MIG_CH}{number of people migrating from / to other superordinate
#'      spatial units (typically cantons).}
#'      \item{MIG_SUB}{number of migrants within the superordinate spatial unit
#'      (typically a canton).}
#'      \item{MOR}{number of deaths (among people older than 0).}
#'      \item{EMI_INT}{number of emigrants to other countries.}
#'      \item{ACQ}{number of foreigners who acquire Swiss citizenship
#'      (naturalisations).}
#'      \item{BIRTHS}{number of births.}
#'
#' @seealso [propop()]
#'
#' @autoglobal
#'
#' @export
#'
#' @examples
#' # load package data
#' data(fso_parameters)
#' data(fso_population)
#'
#' # run projection
#' project_raw(
#'   parameters = fso_parameters,
#'   year_first = 2019,
#'   year_last = 2019,
#'   n = fso_population |> dplyr::pull(n),
#'   subregional = FALSE
#' ) |>
#'   head(10)
project_raw <-
  function(parameters,
           year_first,
           year_last,
           age_groups = 101,
           fert_first = 16,
           fert_last = 50,
           share_born_female = 100 / 205,
           n,
           subregional) {

    # Check input ----
    ## Only 1 value in scenario
    assertthat::assert_that(
      n_distinct(parameters$scen) == 1,
      msg = "The 'scen' column in the 'parameters' data frame must contain the
    identical value in all rows (either reference, high, or low)."
    )

    ## Presence of mandatory parameters ----
    assertthat::assert_that("scen" %in% names(parameters),
      msg = "column `scen` is missing in parameters"
    )
    assertthat::assert_that("nat" %in% names(parameters),
      msg = "column `nat` is missing in parameters"
    )
    assertthat::assert_that("sex" %in% names(parameters),
      msg = "column `sex` is missing in parameters"
    )
    assertthat::assert_that("age" %in% names(parameters),
      msg = "column `age` is missing in parameters"
    )
    assertthat::assert_that("year" %in% names(parameters),
      msg = "column `year` is missing in parameters"
    )
    assertthat::assert_that("birth_rate" %in% names(parameters),
      msg = "column `birth_rate` is missing in parameters"
    )
    assertthat::assert_that("births_int_ch" %in% names(parameters),
      msg = "column `births_int_ch` is missing in parameters"
    )
    assertthat::assert_that("mor" %in% names(parameters),
      msg = "column `mor` is missing in parameters"
    )
    assertthat::assert_that("emi" %in% names(parameters),
      msg = "column `emi` is missing in parameters"
    )
    assertthat::assert_that("acq" %in% names(parameters),
      msg = "column `acq` is missing in parameters"
    )
    assertthat::assert_that("imm_int" %in% names(parameters),
      msg = "column `imm_int` is missing in parameters"
    )
    assertthat::assert_that("mig_ch" %in% names(parameters),
      msg = "column `mig_ch` is missing in parameters"
    )
    assertthat::assert_that("spatial_unit" %in% names(parameters),
      msg = "column `spatial_unit` is missing in parameters"
    )
    # Check existence of optional parameter when required
    if (subregional == TRUE) {
      assertthat::assert_that("mig_sub" %in% names(parameters),
        msg = "column `mig_sub` is missing in parameters"
      )
    }

    ## Check other arguments ----
    # convert input in years to integer, results in error if not possible
    year_first <- vctrs::vec_cast(year_first, integer())
    year_last <- vctrs::vec_cast(year_last, integer())
    fert_first <- vctrs::vec_cast(fert_first, integer())
    fert_last <- vctrs::vec_cast(fert_last, integer())

    assertthat::assert_that(is.integer(year_first),
      dplyr::between(year_first, 2018, 2050),
      msg = paste0(
        "`year_first` must be an integer or a numeric value without decimals",
        " between 2018 and 2050"
      )
    )
    assertthat::assert_that(is.integer(year_last),
      dplyr::between(year_last, 2018, 2050),
      msg = paste0(
        "`year_last` must be an integer or a numeric value without decimals",
        " between 2018 and 2050"
      )
    )
    assertthat::assert_that(is.integer(year_first),
      is.integer(year_last), year_first <= year_last,
      msg = "year_first must be smaller than or equal to year_last"
    )
    assertthat::assert_that(is.vector(age_groups),
      all(sapply(age_groups, is.numeric)),
      all(!is.na(age_groups)),
      msg = paste0(
        "The argument 'age_groups' must be a vector containing only",
        " numeric values and no `NA` values."
      )
    )
    assertthat::assert_that(is.integer(fert_first),
      msg = paste0(
        "The argument 'fert_first' must be an integer or a numeric value",
        " without decimals"
      )
    )
    assertthat::assert_that(is.integer(fert_last),
      msg = paste0(
        "The argument 'fert_last' must be an integer or a numeric value",
        " without decimals"
      )
    )
    assertthat::assert_that(is.integer(fert_first),
      is.integer(fert_last), fert_first <= fert_last,
      msg = "fert_first must be smaller than or equal to fert_last"
    )
    assertthat::assert_that(is.numeric(share_born_female),
      msg = "The argument 'share_born_female' must be numeric"
    )
    assertthat::assert_that(is.vector(n),
      all(sapply(n, is.numeric)),
      all(!is.na(n)),
      msg = paste0(
        "The argument 'n' must be a vector containing only numeric values",
        " and no `NA` values."
      )
    )
    assertthat::assert_that(is.logical(subregional),
      msg = "The argument 'subregional' must either be `TRUE` or `FALSE`"
    )

    ## Progress feedback
    cli::cli_text("Running projection for: {.val { parameters |>",
                  "dplyr::select(spatial_unit) |> dplyr::distinct()}}")


    ## Data preparation ----
    # Compute fertility length (fertile age range for females)
    fert_length <- (fert_last - fert_first + 1)
    assertthat::assert_that(fert_length > 0,
      msg = "Fertility length must be > 0 years."
    )

    # Compute projection length
    proj_length <- (year_last - year_first + 1)
    assertthat::assert_that(proj_length > 0,
      msg = "Projection length must be > 0 years."
    )

    # Length of the population vector, calculated as:
    # 101 age_groups * 2 nationalities * 2 genders = 404 groups
    length_pop_vec <- length(n)
    assertthat::assert_that(length_pop_vec == age_groups * 2 * 2,
      msg = paste0(
        "The length of the population vector is not equal to the size of",
        " demographic groups."
      )
    )

    # Placeholder vector for newborn children (0 years old)
    zeros <- rep(0, age_groups - 1)
    assertthat::assert_that(length(zeros) == age_groups - 1,
      msg = paste0(
        "Placeholder values for the vector to calculate children aged zero",
        " must be equal to (age groups - 1)"
      )
    )

    # Create empty population vector for ages 1-100
    empty_vector_NA <-
      create_empty_vector(
        empty_val = NA,
        proj_length = proj_length,
        length_pop_vec = length_pop_vec
      )
    assertthat::assert_that(length(empty_vector_NA) > 0,
      msg = "Empty population vector must be > 0."
    )

    # Create empty population vector for newborns
    empty_vector_0 <-
      create_empty_vector(
        empty_val = 0,
        proj_length = proj_length,
        length_pop_vec = length_pop_vec
      )
    assertthat::assert_that(length(empty_vector_0) > 0,
      msg = "Empty population vector for children aged zero must be > 0."
    )

    # Prepare empty population vectors for each projection parameter (once for
    # people aged 1-100 and once for newborns)
    # International immigrants
    IMM_INT <- empty_vector_NA
    IMM_INT0 <- empty_vector_NA

    # Cantonal net migration
    MIG_CH <- empty_vector_NA
    MIG_CH0 <- empty_vector_NA

    # Internal net migration
    MIG_SUB <- empty_vector_0
    MIG_SUB0 <- empty_vector_0

    # international emigrants
    EMI_INT <- empty_vector_NA
    EMI_INT0 <- empty_vector_NA

    # naturalization
    ACQ <- empty_vector_NA
    ACQ0 <- empty_vector_NA

    # Vectors for mortality and births
    # death
    MOR <- empty_vector_NA

    # birth
    BIRTHS <- empty_vector_NA


    ## Loop over years ----
    for (i in seq_len(proj_length)) {
      # Define current projection year
      yr <- year_first + i - 1
      assertthat::assert_that(yr >= year_first,
        msg = "Projection year must be equal or greater than the start year."
      )

      # Provide progress information
      cli::cli_alert_success("Year: {.val { yr }}")

      # Determine positions for the starting population within the vector
      temp_pos <- (i - 1) * length_pop_vec
      first_pos <- temp_pos + 1
      last_pos <- i * length_pop_vec


      ### Subset input data -----
      # Annual proportion of Swiss children born to foreign women by year
      births_int_ch <- parameters |>
        dplyr::filter(
          year == yr,
          age %in% (fert_first + 1):(fert_last + 1),
          !is.na(births_int_ch)
        ) |>
        dplyr::select(births_int_ch) |>
        dplyr::distinct() |>
        dplyr::pull()
      assertthat::assert_that(births_int_ch > 0, !is.na(births_int_ch),
        length(births_int_ch) == 1,
        msg = "Value for `births_int_ch` is either < 0, NA or longer than 1."
      )


      # Format data
      parameters_subset <- parameters |>
        # subset data by projection year
        dplyr::filter(year == yr) |>
        # remove unused columns
        dplyr::select(-c(year, spatial_unit, scen, births_int_ch)) |>
        # pivot parameters into long format
        tidyr::pivot_longer(
          cols = c(birth_rate:last_col()),
          names_to = "parameter",
          values_to = "value"
        ) |>
        # remove non-existing combinations (birth rate of males and acquisition
        # of the Swiss citizenship for females)
        dplyr::filter(
          !(parameter == "birth_rate" & sex == "m"),
          !(parameter == "acq" & nat == "ch")
        ) |>
        # pivot parameters into wide format
        tidyr::pivot_wider(
          names_from = age,
          values_from = value
        ) |>
        # clean data
        mutate(id_col = paste(parameter, nat, sex, sep = "_")) |>
        tibble::column_to_rownames(var = "id_col") |>
        dplyr::select(-c(nat, sex, parameter)) |>
        t() |>
        as.data.frame()
      assertthat::assert_that(nrow(parameters_subset) == (length_pop_vec / 4),
        msg = paste0(
          "Length of data frame `parameters_subset` must be equal to",
          " `length_pop_vec` / 4 (demographic groups)"
        )
      )


      # Create list of vectors
      vectors_parameters <- as.list(parameters_subset)


      # Subset data into vectors
      ## Birth rate vectors (fertility age = 16 to 50)
      birth_rate_int <-
        vectors_parameters$birth_rate_int_f[(fert_first + 1):(fert_last + 1)]
      birth_rate_ch <-
        vectors_parameters$birth_rate_ch_f[(fert_first + 1):(fert_last + 1)]


      ## Vectors for mortality, emigration and acquisition of citizenship
      # Vectors for newborns
      # Mortality
      mor_ch_f_0 <- vectors_parameters$mor_ch_f[1]
      mor_int_f_0 <- vectors_parameters$mor_int_f[1]
      mor_ch_m_0 <- vectors_parameters$mor_ch_m[1]
      mor_int_m_0 <- vectors_parameters$mor_int_m[1]
      # Emigration
      emi_ch_f_0 <- vectors_parameters$emi_ch_f[1]
      emi_int_f_0 <- vectors_parameters$emi_int_f[1]
      emi_ch_m_0 <- vectors_parameters$emi_ch_m[1]
      emi_int_m_0 <- vectors_parameters$emi_int_m[1]
      # Acquisition of the Swiss citizenship
      acq_int_f_0 <- vectors_parameters$acq_int_f[1]
      acq_int_m_0 <- vectors_parameters$acq_int_m[1]
      assertthat::assert_that(
        !any(
          sapply(list(
            mor_ch_f_0, mor_int_f_0, mor_ch_m_0, mor_int_m_0, emi_ch_f_0,
            emi_int_f_0, emi_ch_m_0, emi_int_m_0, acq_int_f_0, acq_int_m_0
          ), function(x) is.null(x) || length(x) == 0)
        ),
        msg = paste0(
          "One or more vectors for mortality, emigration and acquisition",
          " of citizenship for children aged zero years are empty"
        )
      )

      # Vectors for people older than 0 years
      # Mortality
      mor_ch_f <- vectors_parameters$mor_ch_f[c(2:age_groups, age_groups)]
      mor_int_f <- vectors_parameters$mor_int_f[c(2:age_groups, age_groups)]
      mor_ch_m <- vectors_parameters$mor_ch_m[c(2:age_groups, age_groups)]
      mor_int_m <- vectors_parameters$mor_int_m[c(2:age_groups, age_groups)]
      # Emigration
      emi_ch_f <- vectors_parameters$emi_ch_f[c(2:age_groups, age_groups)]
      emi_int_f <- vectors_parameters$emi_int_f[c(2:age_groups, age_groups)]
      emi_ch_m <- vectors_parameters$emi_ch_m[c(2:age_groups, age_groups)]
      emi_int_m <- vectors_parameters$emi_int_m[c(2:age_groups, age_groups)]
      # Acquisition of the Swiss citizenship
      acq_int_f <- vectors_parameters$acq_int_f[c(2:age_groups, age_groups)]
      acq_int_m <- vectors_parameters$acq_int_m[c(2:age_groups, age_groups)]
      assertthat::assert_that(
        !any(
          sapply(list(
            mor_ch_f, mor_int_f, mor_ch_m, mor_int_m, emi_ch_f,
            emi_int_f, emi_ch_m, emi_int_m, acq_int_f, acq_int_m
          ), function(x) is.null(x) || length(x) == 0)
        ),
        msg = paste0(
          "One or more vectors for mortality, emigration and acquisition",
          " of citizenship for people older than 0 years are empty"
        )
      )


      ## Vectors for migration
      # Vectors for newborns
      # International immigration
      imm_int_ch_f_0 <- vectors_parameters$imm_int_ch_f[1]
      imm_int_ch_m_0 <- vectors_parameters$imm_int_ch_m[1]
      imm_int_int_f_0 <- vectors_parameters$imm_int_int_f[1]
      imm_int_int_m_0 <- vectors_parameters$imm_int_int_m[1]
      # Intercantonal net-migration
      mig_ch_ch_f_0 <- vectors_parameters$mig_ch_ch_f[1]
      mig_ch_ch_m_0 <- vectors_parameters$mig_ch_ch_m[1]
      mig_ch_int_f_0 <- vectors_parameters$mig_ch_int_f[1]
      mig_ch_int_m_0 <- vectors_parameters$mig_ch_int_m[1]
      assertthat::assert_that(
        !any(
          sapply(list(
            imm_int_ch_f_0, imm_int_ch_m_0, imm_int_int_f_0, imm_int_int_m_0,
            mig_ch_ch_f_0, mig_ch_ch_m_0, mig_ch_int_f_0, mig_ch_int_m_0
          ), function(x) is.null(x) || length(x) == 0)
        ),
        msg = paste0(
          "One or more vectors for international immigration or intercantonal",
          " net-migration for people older than 0 years are empty"
        )
      )

      # Vectors for people older than 0 years
      # International immigration
      imm_int_ch_f <- vectors_parameters$imm_int_ch_f[2:age_groups]
      imm_int_ch_m <- vectors_parameters$imm_int_ch_m[2:age_groups]
      imm_int_int_f <- vectors_parameters$imm_int_int_f[2:age_groups]
      imm_int_int_m <- vectors_parameters$imm_int_int_m[2:age_groups]

      # Intercantonal net-migration
      mig_ch_ch_f <- vectors_parameters$mig_ch_ch_f[2:age_groups]
      mig_ch_ch_m <- vectors_parameters$mig_ch_ch_m[2:age_groups]
      mig_ch_int_f <- vectors_parameters$mig_ch_int_f[2:age_groups]
      mig_ch_int_m <- vectors_parameters$mig_ch_int_m[2:age_groups]
      assertthat::assert_that(
        !any(
          sapply(list(
            imm_int_ch_f, imm_int_ch_m, imm_int_int_f, imm_int_int_m,
            mig_ch_ch_f, mig_ch_ch_m, mig_ch_int_f, mig_ch_int_m
          ), function(x) is.null(x) || length(x) == 0)
        ),
        msg = paste0(
          "One or more vectors for international immigration or intercantonal",
          " net-migration for people older than 0 years are empty"
        )
      )

      ## Optional: migration on subregional level
      ## (e.g., municipalities within canton)
      if (subregional == TRUE) {
        # Create vectors for newborns
        mig_sub_ch_f_0 <- vectors_parameters$mig_sub_ch_f[1]
        mig_sub_ch_m_0 <- vectors_parameters$mig_sub_ch_m[1]
        mig_sub_int_f_0 <- vectors_parameters$mig_sub_int_f[1]
        mig_sub_int_m_0 <- vectors_parameters$mig_sub_int_m[1]
        # Create vectors for people older than 0 years
        mig_sub_ch_f <- vectors_parameters$mig_sub_ch_f[2:age_groups]
        mig_sub_ch_m <- vectors_parameters$mig_sub_ch_m[2:age_groups]
        mig_sub_int_f <- vectors_parameters$mig_sub_int_f[2:age_groups]
        mig_sub_int_m <- vectors_parameters$mig_sub_int_m[2:age_groups]
        assertthat::assert_that(
          !any(
            sapply(list(
              mig_sub_ch_f_0, mig_sub_ch_m_0, mig_sub_int_f_0, mig_sub_int_m_0,
              mig_sub_ch_f, mig_sub_ch_m, mig_sub_int_f, mig_sub_int_m
            ), function(x) is.null(x) || length(x) == 0)
          ),
          msg =
            "One or more vectors for migration on subregional level are empty"
        )
      }


      ### Build matrices ----
      #### Transition matrix ----
      # The transition matrix (L = "Leslie-Matrix") contains survival rates for
      # each demographic unit. Survival rates are calculated in the function
      # 'create_transition_matrix()' based on mortality, emigration and
      # citizenship-acquisition rates.
      L <-
        create_transition_matrix(
          n_age_class = age_groups,
          mor_ch_f = mor_ch_f,
          mor_ch_m = mor_ch_m,
          mor_int_f = mor_int_f,
          mor_int_m = mor_int_m,
          emi_ch_f = emi_ch_f,
          emi_ch_m = emi_ch_m,
          emi_int_f = emi_int_f,
          emi_int_m = emi_int_m,
          acq_int_f = acq_int_f,
          acq_int_m = acq_int_m
        )
      assertthat::assert_that(unique(dim(L)) == length_pop_vec,
        msg = paste0(
          "Transition matrix dimensions are not equal to the length of",
          " the population vector."
        )
      )


      #### Fertility matrix ----
      # The fertility matrix 'O' is needed to calculate the number of newborns
      O <-
        create_fertility_matrix(
          fert_first = fert_first,
          fert_last = fert_last,
          fert_length = fert_length,
          n_age_class = age_groups,
          share_born_female = share_born_female,
          birth_rate_ch = birth_rate_ch,
          birth_rate_int = birth_rate_int,
          births_int_ch = births_int_ch,
          mor_ch_f_0 = mor_ch_f_0,
          mor_ch_m_0 = mor_ch_m_0,
          mor_int_f_0 = mor_int_f_0,
          mor_int_m_0 = mor_int_m_0,
          emi_ch_f_0 = emi_ch_f_0,
          emi_ch_m_0 = emi_ch_m_0,
          emi_int_f_0 = emi_int_f_0,
          emi_int_m_0 = emi_int_m_0,
          acq_int_f_0 = acq_int_f_0,
          acq_int_m_0 = acq_int_m_0
        )
      assertthat::assert_that(unique(dim(O)) == length_pop_vec,
        msg = paste0(
          "Fertility matrix dimensions are not equal to the length of",
          " the population vector."
        )
      )

      ### Build vectors ----
      #### Mortality ----
      # Vector for 1-100
      MOR_vec <-
        c(
          mor_ch_m,
          mor_ch_f,
          mor_int_m,
          mor_int_f
        )
      assertthat::assert_that(length(MOR_vec) == length_pop_vec,
        msg = paste0(
          "Mortality vector `MOR_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(MOR_vec)),
        msg = "Mortality vector `MOR_vec` contains NAs."
      )

      # Vector for newborns
      MOR0_vec <-
        c(
          mor_ch_m_0, zeros,
          mor_ch_f_0, zeros,
          mor_int_m_0, zeros,
          mor_int_f_0, zeros
        )
      assertthat::assert_that(length(MOR0_vec) == length_pop_vec,
        msg = paste0(
          "Mortality vector `MOR0_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(MOR0_vec)),
        msg = "Mortality vector `MOR0_vec` contains NAs."
      )

      #### Death ----
      # Vector for newborns
      D0_vec <-
        c(
          mor_ch_m_0 - (emi_ch_m_0 * ((2 / 3) * mor_ch_m_0)), zeros,
          mor_ch_f_0 - (emi_ch_f_0 * ((2 / 3) * mor_ch_f_0)), zeros,
          mor_int_m_0 - ((emi_int_m_0 + acq_int_m_0) * ((2 / 3) * mor_int_m_0)),
          zeros,
          mor_int_f_0 - ((emi_int_f_0 + acq_int_f_0) * ((2 / 3) * mor_int_f_0)),
          zeros
        )
      assertthat::assert_that(length(D0_vec) == length_pop_vec,
        msg = paste0(
          "`D0_vec` vector length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(D0_vec)),
        msg = "Death vector `D0_vec` contains NAs."
      )

      #### Migration ----
      # Vector for 1-100
      EMI_vec <-
        c(
          emi_ch_m,
          emi_ch_f,
          emi_int_m,
          emi_int_f
        )
      assertthat::assert_that(length(EMI_vec) == length_pop_vec,
        msg = paste0(
          "Migration vector `EMI_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(
        unique(!is.na(EMI_vec)),
        msg = "Migration vector `EMI_vec` contains NAs."
      )

      # Vector for newborns
      EMI0_vec <-
        c(
          emi_ch_m_0, zeros,
          emi_ch_f_0, zeros,
          emi_int_m_0, zeros,
          emi_int_f_0, zeros
        )
      assertthat::assert_that(length(EMI0_vec) == length_pop_vec,
        msg = paste0(
          "Migration vector `EMI0_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(EMI0_vec)),
        msg = "Migration vector `EMI0_vec` contains NAs."
      )

      #### Acquisition of the Swiss citizenship ----
      # Vector for 1-100
      ACQ_vec <-
        c(
          rep(0, 2 * age_groups),
          acq_int_m,
          acq_int_f
        )
      assertthat::assert_that(length(ACQ_vec) == length_pop_vec,
        msg = paste0(
          "Emigration vector `ACQ_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(ACQ_vec)),
        msg = "Emigration vector `ACQ_vec` contains NAs."
      )

      # Vector for newborns
      ACQ0_vec <-
        c(
          rep(0, 2 * age_groups),
          acq_int_m_0, zeros,
          acq_int_f_0, zeros
        )
      assertthat::assert_that(length(ACQ0_vec) == length_pop_vec,
        msg = paste0(
          "Emigration vector `ACQ0_vec` length is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(unique(!is.na(ACQ0_vec)),
        msg = "Emigration vector `ACQ0_vec` contains NAs."
      )

      #### International immigration ----
      # Vector for 1-100
      IMM_INT[first_pos:last_pos] <-
        c(
          0, imm_int_ch_m,
          0, imm_int_ch_f,
          0, imm_int_int_m,
          0, imm_int_int_f
        )
      assertthat::assert_that(length(IMM_INT) == length(empty_vector_NA),
        msg = paste0(
          "Vector `IMM_INT` length is not equal to the length of",
          " the pre-defined empty vector."
        )
      )

      # Vector for newborns
      IMM_INT0[first_pos:last_pos] <-
        c(
          imm_int_ch_m_0, zeros,
          imm_int_ch_f_0, zeros,
          imm_int_int_m_0, zeros,
          imm_int_int_f_0, zeros
        )
      assertthat::assert_that(length(IMM_INT0) == length(empty_vector_0),
        msg = paste0(
          "Vector `IMM_INT0` length is not equal to the length of",
          " the pre-defined empty vector."
        )
      )

      #### Cantonal net migration ----
      # (persons migrating from and to other cantons)
      # Vector for 1-100
      MIG_CH[first_pos:last_pos] <-
        c(
          0, mig_ch_ch_m,
          0, mig_ch_ch_f,
          0, mig_ch_int_m,
          0, mig_ch_int_f
        )
      assertthat::assert_that(length(MIG_CH) == length(empty_vector_NA),
        msg = paste0(
          "Vector `MIG_CH` lentgh is not equal to the length of",
          " the pre-defined empty vector."
        )
      )

      # Vector for newborns
      MIG_CH0[first_pos:last_pos] <-
        c(
          mig_ch_ch_m_0, zeros,
          mig_ch_ch_f_0, zeros,
          mig_ch_int_m_0, zeros,
          mig_ch_int_f_0, zeros
        )
      assertthat::assert_that(length(MIG_CH0) == length(empty_vector_0),
        msg = paste0(
          "Vector `MIG_CH0` length is not equal to the length of",
          " the pre-defined empty vector."
        )
      )

      #### Optional: migration on subregional level -----
      # (e.g., municipalities within canton)
      if (subregional == TRUE) {
        # Vector for 1-100
        MIG_SUB[first_pos:last_pos] <-
          c(
            0, mig_sub_ch_m,
            0, mig_sub_ch_f,
            0, mig_sub_int_m,
            0, mig_sub_int_f
          )
        assertthat::assert_that(length(MIG_SUB) == length(empty_vector_NA),
          msg = paste0(
            "Vector `MIG_SUB` length is not equal to the length of",
            " the pre-defined empty vector."
          )
        )

        # Vector for newborns
        MIG_SUB0[first_pos:last_pos] <-
          c(
            mig_sub_ch_m_0, zeros,
            mig_sub_ch_f_0, zeros,
            mig_sub_int_m_0, zeros,
            mig_sub_int_f_0, zeros
          )
        assertthat::assert_that(length(MIG_SUB0) == length(empty_vector_0),
          msg = paste0(
            "Vector `MIG_SUB0` length is not equal to the length of",
            " the pre-defined empty vector."
          )
        )
      }


      ### Projection ----
      # Population vector at time step n excluding the newborns:
      # Multiplying the survival rate ('L') with the starting population ('n')
      # and adding people from migration ('IMM_INT', 'MIG_CH' and optionally
      # 'MIG_SUB').
      Nn100 <-
        L %*% n[first_pos:last_pos] +
        IMM_INT[first_pos:last_pos] * (1 - (MOR_vec / 2)) +
        MIG_CH[first_pos:last_pos] * (1 - (MOR_vec / 2)) +
        MIG_SUB[first_pos:last_pos]
      assertthat::assert_that(length(Nn100) == length_pop_vec,
        msg = paste0(
          "Formula 1 first equation output is not equal to the length of",
          " the population vector."
        )
      )
      assertthat::assert_that(any(!is.na(Nn100)),
        msg = "Result matrix `Nn100` contains NAs."
      )

      # Population vector at time step n-1 excluding the newborns
      Nminus1 <-
        c(
          0,
          n[(first_pos):(temp_pos + age_groups - 1)],
          0,
          n[(temp_pos + age_groups + 1):(temp_pos + age_groups * 2 - 1)],
          0,
          n[(temp_pos + age_groups * 2 + 1):(temp_pos + age_groups * 3 - 1)],
          0,
          n[(temp_pos + age_groups * 3 + 1):(temp_pos + age_groups * 4 - 1)]
        )
      assertthat::assert_that(length(Nminus1) == length_pop_vec,
        msg = paste0(
          "Population vector `Nminus1` at time step n-1 is not equal",
          " to the length of the population vector."
        )
      )
      assertthat::assert_that(any(!is.na(Nminus1)),
        msg = "Result matrix `Nminus1` contains NAs."
      )

      # Population vector at time step n for newborns (p_n,i = 0, with i > 0)
      Nn0 <-
        O %*% ((1 / 2) * (Nn100 + Nminus1)) +
        IMM_INT0[first_pos:last_pos] * (1 - ((2 / 3) * MOR0_vec)) +
        MIG_CH0[first_pos:last_pos] * (1 - ((2 / 3) * MOR0_vec)) +
        MIG_SUB0[first_pos:last_pos]
      assertthat::assert_that(length(Nn0) == length_pop_vec,
        msg = paste0(
          "Formula 1 second equation output `Nn0` is not equal",
          " to the length of the population vector."
        )
      )
      assertthat::assert_that(any(!is.na(Nn0)),
        msg = "Result matrix `Nn0` contains NAs."
      )

      # Complete population vector at time step n with newborns and people older
      # than 1 year
      n[(last_pos + 1):((i + 1) * length_pop_vec)] <-
        Nn100 + Nn0
      assertthat::assert_that(any(!is.na(n)),
        msg = "Result matrix `n` contains NAs."
      )
    }

    # Feedback if non-standard values are used
    ## Create vector with non-FSO conform parameters
    non_conform <- c()

    # Check each condition and add variable name to vector if it fails
    if (age_groups != 101) {
      non_conform <- c(non_conform, "age_groups")
    }

    if (fert_first != 16) {
      non_conform <- c(non_conform, "fert_first")
    }

    if (fert_last != 50) {
      non_conform <- c(non_conform, "fert_last")
    }

    if (share_born_female != 100 / 205) {
      non_conform <- c(non_conform, "share_born_female")
    }

    ## Feedback
    if (!is.null(non_conform)) {
      # cli::cli_rule()
      cli::cli_text(cli::col_red("Warning message:"))
      cli::cli_text("Some of the provided parameters do not correspond to the
                  standard values suggested by the Federal Statistical Office:")
      cli::cli_alert_warning(cli::col_magenta(
        paste(non_conform, collapse = ", ")
      ))
      cli::cli_text("This may lead to incomplete, unexpected, or wrong results.")
      cli::cli_rule()
    }

    ## Final data set ----
    data.frame(
      # pop size
      N = n,
      # nbr international immigrants
      IMM_INT = IMM_INT + IMM_INT0,
      # cantonal migration saldo
      MIG_CH = MIG_CH + MIG_CH0,
      # intracantonal migration saldo
      MIG_SUB = MIG_SUB + MIG_SUB0,
      # nbr of death older than 0
      MOR = MOR,
      # international emmigrants
      EMI_INT = EMI_INT + EMI_INT0,
      # nbr of naturalisation
      ACQ = ACQ + ACQ0,
      # BIRTHS
      BIRTHS = BIRTHS
    )
  }
