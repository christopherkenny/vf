#' Read FL voter file
#'
#' @param path_detail path to FL detail file or directory
#' @param path_history path to FL history file or directory
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_fl <- function(path_detail, path_history, widen_history = TRUE) {

  # check inputs ----
  has_detail <- !missing(path_detail)
  has_history <- !missing(path_history)

  if (!has_detail && !has_history) {
    cli::cli_abort('Must specify at least one of {.arg path_detail} or {.arg path_history}.')
  }

  if (has_detail) {
    if (any(fs::is_dir(path_detail))) {
      if (!all(fs::is_dir(path_detail))) {
        cli::cli_abort('{.arg path_detail} must be either (1) all files or (2) one directory and no files.')
      }
      path_detail <- fs::dir_ls(path_detail, glob = '*.txt')
    }
  }
  if (has_history) {
    if (any(fs::is_dir(path_history))) {
      if (!all(fs::is_dir(path_history))) {
        cli::cli_abort('{.arg path_history} must be either (1) all files or (2) one directory and no files.')
      }
      path_history <- fs::dir_ls(path_history, glob = '*.txt')
    }
  }

  if (has_detail && has_history && !widen_history) {
    cli::cli_warn('{.arg widen_history} will be set to {.val TRUE} to join outputs from {.arg path_detail} and {.arg path_history}.')
  }

  # read detail ----
  if (has_detail) {
    details <- purrr::map_dfr(path_detail, function(f) {
      readr::read_tsv(
        file = f,
        col_names = fl_detail_names,
        col_types = readr::cols(
          county_code                = readr::col_character(),
          voter_id                   = readr::col_double(),
          name_last                  = readr::col_character(),
          name_suffix                = readr::col_character(),
          name_first                 = readr::col_character(),
          name_middle                = readr::col_character(),
          requested_public_exemption = readr::col_character(),
          residence_address_line_1   = readr::col_character(),
          residence_address_line_2   = readr::col_character(),
          residence_city             = readr::col_character(),
          residence_state            = readr::col_character(),
          residence_zipcode          = readr::col_character(),
          mailing_address_line_1     = readr::col_character(),
          mailing_address_line_2     = readr::col_character(),
          mailing_address_line_3     = readr::col_character(),
          mailing_city               = readr::col_character(),
          mailing_state              = readr::col_character(),
          mailing_zipcode            = readr::col_character(),
          mailing_country            = readr::col_character(),
          gender                     = readr::col_character(),
          race                       = readr::col_double(),
          birth_date                 = readr::col_character(),
          registration_date          = readr::col_character(),
          party_affiliation          = readr::col_character(),
          precinct                   = readr::col_character(),
          precinct_group             = readr::col_character(),
          precinct_split             = readr::col_character(),
          precinct_suffix            = readr::col_character(),
          voter_status               = readr::col_character(),
          congressional_district     = readr::col_character(),
          house_district             = readr::col_character(),
          senate_district            = readr::col_character(),
          county_commission_district = readr::col_character(),
          school_board_district      = readr::col_character(),
          daytime_area_code          = readr::col_character(),
          daytime_phone_number       = readr::col_character(),
          daytime_phone_extension    = readr::col_character(),
          email_address              = readr::col_character(),
          .default                   = readr::col_character()
        )
      )
    })
  }

  # read history ----
  if (has_history) {
    history <- purrr::map_dfr(path_history, function(f) {
      readr::read_tsv(
        file = f,
        col_names = fl_history_names,
        col_types = readr::cols(
          county_code   = readr::col_character(),
          voter_id      = readr::col_double(),
          election_date = readr::col_character(),
          election_type = readr::col_character(),
          history_code  = readr::col_character()
        )
      )
    }) |>
      dplyr::mutate(
        election_date = stringr::str_replace_all(lubridate::mdy(election_date), '-', '_')
      )

    if (widen_history) {
      history <- history |>
        tidyr::unite(
          'record', election_type, election_date, sep = '_'
        ) |>
        tidyr::pivot_wider(
          id_cols = c(county_code, voter_id),
          names_from = record,
          values_from = history_code,
          values_fn = \(x) paste0(sort(x), collapse = ''))
    }


  }

  # return ----
  if (has_detail && has_history) {
    dplyr::left_join(details, history, by = c('county_code', 'voter_id'))
  } else if (has_detail) {
    details
  } else if (has_history) {
    history
  } else {
    cli::cli_abort('Something went wrong. Please open an issue at {.url https://github.com/christopherkenny/vf/issues}.')
  }
}

fl_detail_names <- c(
  'county_code',
  'voter_id',
  'name_last',
  'name_suffix',
  'name_first',
  'name_middle',
  'requested_public_exemption',
  'residence_address_line_1',
  'residence_address_line_2',
  'residence_city',
  'residence_state',
  'residence_zipcode',
  'mailing_address_line_1',
  'mailing_address_line_2',
  'mailing_address_line_3',
  'mailing_city',
  'mailing_state',
  'mailing_zipcode',
  'mailing_country',
  'gender',
  'race',
  'birth_date',
  'registration_date',
  'party_affiliation',
  'precinct',
  'precinct_group',
  'precinct_split',
  'precinct_suffix',
  'voter_status',
  'congressional_district',
  'house_district',
  'senate_district',
  'county_commission_district',
  'school_board_district',
  'daytime_area_code',
  'daytime_phone_number',
  'daytime_phone_extension',
  'email_address'
)

fl_history_names <- c(
  'county_code',
  'voter_id',
  'election_date',
  'election_type',
  'history_code'
)
