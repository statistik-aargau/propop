#' This function runs projections with tables instead of matrices
#' (work-in-progress)
#'
#' @description
#' This function is applied with `purrr::reduce()` to iterate across years.
#' An example is currently in development; see script: dev/reduce_function.R
#'
#' @param population data frame including the starting population of each
#' demographic group; either the initial population or the previous projected
#' year. Possible identifiers are the same as in `parameters` (apart from year).
#' The data frame only includes one year, usually the one preceding the first
#' projected year.
#'    * `year` numeric
#'    * `spatial_unit` character.
#'    * `nat` character.
#'    * `sex` character.
#'    * `age` numeric.
#'    * `n_dec` numeric, number of people per demographic group in December.
#' @param parameters list, data frames containing the FSO rates and numbers to
#' run the projection for a specific spatial level (e.g., canton, municipality).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#'
#' @return
#' Returns a data frame that includes the number of people for each demographic
#'      group per year (for projected years) and spatial unit. The number of
#'      rows is the product of all years times all demographic groups times
#'      all spatial units.
#'      The output includes several \bold{identifiers} that indicate to which
#'      demographic group, year, and spatial unit the results in the rows refer
#'      to:
#'      \item{year}{integer, indicating the projected years.}
#'      \item{spatial_unit}{factor, spatial units for which the projection
#'            was run (e.g., canton, districts, municipalities).}
#'      \item{age}{integer, ranging from `0`n to `100` (including those older
#'      than 100).}
#'      \item{sex}{factor, female (f) and male (m).}
#'      \item{nat}{factor, indicates if the nationality is Swiss (ch) or
#'      international / foreign (int). This variable is only returned if
#'      `binational = TRUE`.}
#'      The output also includes columns related to the \bold{size and change
#'      of the population:}
#'      \item{n_jan}{numeric, start-of-year population per demographic group.}
#'      \item{n_dec}{numeric, end-of-year population per demographic group.}
#'      The \bold{components} that are used to project the development of the
#'      population are also included in the output:
#'      \item{births}{numeric, number of births (non-zero values are only
#'      available for age = 0).}
#'      \item{mor_n}{numeric, number of deaths.}
#'      \item{emi_int}{numeric, number of people who emigrate
#'      to other countries.}
#'      \item{emi_nat_n}{numeric, number of people who emigrate
#'      to other cantons.}
#'      \item{imm_int_n}{numeric, number of people who immigrate
#'      from other countries.}
#'      \item{imm_nat_n}{numeric, number of people who immigrate
#'      from other cantons.}
#'      \item{acq_n}{numeric, number of people who acquire Swiss citizenship.}
#'
#' @export
apply_parameters <- function(
    population,
    parameters,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205) {

  browser()

  # `pop_year` helps to identify the progress of the iteration. For the first
  # iteration, pop_year returns a single year. For all following iterations,
  # `pop_year` contains two years.
  pop_year <- unique(population$year)

  # If the length of `pop_year` is 1, the initial population is used here.
  # Otherwise, if the length of `pop_year` is larger than one, the population
  # is retrieved from the previous iterations' result.
  if (isTRUE(length(pop_year) != 1)) {
    population_prev <- population |>
      # filter for year from parameters
      filter(year == parameters$year - 1)
  } else {
    population_prev <- population
  }

  population_aged <- population_prev |>
    # advance the population by one year: increase the age by one year,
    # aggregate people aged 100 and older.
    advance_population() |>
    # adapt year to parameters
    mutate(year = unique(parameters$year))

  # Prepare the population from the previous iteration to calculate the
  # projection of the next year
  population_aged_prep <- population_aged |>
    # determine factor levels
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    # arrange data
    arrange(year, nat, sex, age) |>
    # select identifier columns and population
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "n_dec", "births"
    ))) |>
    # the population in december of year t becomes the population in January
    # of year t+1
    rename(n_jan = n_dec)


  # Calculate the projection
  population_new <- parameters |>
    # exclude newborn children
    dplyr::filter(age > 0) |>
    # join population and parameters
    left_join(
      population_aged_prep,
      by = c("year", "spatial_unit", "nat", "sex", "age")
    ) |>
    # calculate projection results
    calc_proj_tables() |>
    # bind results of year t and year t+1
    bind_rows(population)


  # calculate newborns
  newborns <- calc_newborns(
    population = population_new,
    parameters = parameters,
    fert_first = fert_first,
    fert_last = fert_last,
    share_born_female = share_born_female
  ) |>
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    # arrange data
    arrange(year, nat, sex, age) |>
    # apply FSO method for projections
    mutate(
      # births = 0,
      # rates (resulting people are subtracted from the population) -----

      # international emigration -----
      emi_int_n = case_when(
        # ages 1-100
        # age > 0 ~ n_jan * (emi_int_rate * (1 - (mor_rate / 2))),
        # newborns
        # age == 0 ~ births * (emi_int_rate * (1 - (mor_rate / 2))),
        # Using emigration international function:
        age == 0 ~ births * emi_int_rate ,
        TRUE ~ NA
      ),
      # emigration to other cantons -----
      emi_nat_n = case_when(
        # ages 1-100
        # age > 0 ~ n_jan * (emi_nat_rate * (1 - (mor_rate / 2))),
        # newborns
        # age == 0 ~ births * (emi_nat_rate * (1 - (mor_rate / 2))),
        # Using function for intercatonal emmigration
        age == 0 ~ births * emi_nat_rate ,
        TRUE ~ NA
      ),
      # surviving newborns -----
      # birth_balance = case_when(
      #   age == 0 ~ births - mor_n - emi_int_n - emi_nat_n, TRUE ~ NA
      # ),
      #
      # birth_balance = 0,
      # absolute numbers (are added to the population) -----
      ## acquisition of Swiss citizenship -----
      acq_n = case_when(
        # ages 1-100
        # age > 0 ~ n_jan * (acq_rate * (1 - (mor_rate / 2))),
        # newborns
        # age == 0 ~ birth_balance * (acq_rate * (1 - (mor_rate / 2))),
        # USing aquisition function
        age == 0 ~ births * acq_rate,
        TRUE ~ NA
      ),

      # Stopped here ----
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, 2 ), -acq_n),
      ## international immigration -----
      imm_int_nn = imm_int_n,
      ## immigration from other cantons
      imm_nat_nn = imm_nat_n,
      # calculate the new population in December of the respective year -----
      # population balance
      # n_dec = n_jan - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
      #   imm_nat_n,
      # add newborns

      ## mortality -----
      mor_n = case_when(
        # ages 1-100
        # age > 0 ~ n_jan - (n_jan * (1 - mor_rate)),
        # newborns
        # age == 0 ~ births - (births * (1 - mor_rate)),
        # Using death function:
        age == 0 ~ mor_rate*(births*(1-(2/3)*(emi_int_rate+acq_rate+emi_nat_rate))+(2/3)*(imm_int_n+acq_n+imm_nat_n )),
        TRUE ~ NA
      ),
      #STOPPED HERE ----
      n_dec = case_when(
        # age > 0 ~ n_jan - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
        # imm_nat_n,
        age == 0 ~ births - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
          imm_nat_n,
        TRUE ~ NA)
    )



  # Bind results of year t and year t+1
  population_out <- bind_rows(population_new, newborns) |>
    # clean the data
    select(any_of(c(
      "year", "spatial_unit", "scen", "nat", "sex", "age", "n_jan", "births",
      "mor_n", "emi_int_n", "emi_nat_n", "imm_int_n", "imm_nat_n", "acq_n",
      "mig_sub", "n_dec"
    ))) |>
    mutate(
      sex = factor(sex, levels = c("m", "f")),
      nat = factor(nat, levels = c("ch", "int"))
    ) |>
    arrange(nat, desc(sex), age, year, spatial_unit)

  return(population_out)
}
