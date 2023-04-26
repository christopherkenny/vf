#' Read WA voter file
#'
#' @param path_detail path to WA detail file or directory
#' @param path_history path to WA history file or directory
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_wa <- function(path_detail, path_history, widen_history = TRUE) {
  # check inputs ----
  has_detail <- !missing(path_detail)
  has_history <- !missing(path_history)

  if (!has_detail && !has_history) {
    cli::cli_abort('Must specify at least one of {.arg path_detail} or {.arg path_history}.')
  }

  # read detail ----
  if (has_detail) {
    detail <- readr::read_delim(
      path_detail, delim = '|',
      col_types =   readr::cols(
        StateVoterID          = readr::col_double(),
        FName                 = readr::col_character(),
        MName                 = readr::col_character(),
        LName                 = readr::col_character(),
        NameSuffix            = readr::col_character(),
        Birthdate             = readr::col_date(format = ''),
        Gender                = readr::col_character(),
        RegStNum              = readr::col_character(),
        RegStFrac             = readr::col_character(),
        RegStName             = readr::col_character(),
        RegStType             = readr::col_character(),
        RegUnitType           = readr::col_character(),
        RegStPreDirection     = readr::col_character(),
        RegStPostDirection    = readr::col_character(),
        RegStUnitNum          = readr::col_character(),
        RegCity               = readr::col_character(),
        RegState              = readr::col_character(),
        RegZipCode            = readr::col_character(),
        CountyCode            = readr::col_character(),
        PrecinctCode          = readr::col_character(),
        PrecinctPart          = readr::col_character(),
        LegislativeDistrict   = readr::col_double(),
        CongressionalDistrict = readr::col_double(),
        Mail1                 = readr::col_character(),
        Mail2                 = readr::col_character(),
        Mail3                 = readr::col_character(),
        Mail4                 = readr::col_character(),
        MailCity              = readr::col_character(),
        MailZip               = readr::col_character(),
        MailState             = readr::col_character(),
        MailCountry           = readr::col_character(),
        Registrationdate      = readr::col_date(format = ''),
        AbsenteeType          = readr::col_character(),
        LastVoted             = readr::col_date(format = ''),
        StatusCode            = readr::col_character()
      )
    ) |>
      setNames(wa_detail_names)
  }

  if (has_history) {
    history <- readr::read_tsv(
      file = path_history
    )
  }

  detail
}

wa_detail_names <- c(
  'voter_id',
  'name_first',
  'name_middle',
  'name_last',
  'name_suffix',
  'birth_date',
  'gender',
  'residence_address_number',
  'residence_street_fraction',
  'residence_street_name',
  'residence_street_type',
  'residence_address_unit_type',
  'residence_address_pre_direction',
  'residence_address_post_direction',
  'residence_address_unit_number',
  'residence_city',
  'residence_state',
  'residence_zip',
  'county',
  'precinct',
  'precinct_split',
  'legislative_district',
  'congressional_district',
  'mailing_address_line_1',
  'mailing_address_line_2',
  'mailing_address_line_3',
  'mailing_address_line_4',
  'mailing_city',
  'mailing_zip',
  'mailing_state',
  'mailing_country',
  'registration_date',
  'absentee_type',
  'last_voted_date',
  'voter_status'
)

wa_county_codes <- tibble::tribble(
  ~county, ~county_code,
  'AD', 'Adams',
  'AS', 'Asotin',
  'BE', 'Benton',
  'CH', 'Chelan',
  'CM', 'Clallam',
  'CR', 'Clark',
  'CU', 'Columbia',
  'CZ', 'Cowlitz',
  'DG', 'Douglas',
  'FE', 'Ferry',
  'FR', 'Franklin',
  'GA', 'Garfield',
  'GR', 'Grant',
  'GY', 'Grays Harbor',
  'IS', 'Island',
  'JE', 'Jefferson',
  'KI', 'King',
  'KP', 'Kitsap',
  'KS', 'Kittitas',
  'KT', 'Klickitat',
  'LE', 'Lewis',
  'LI', 'Lincoln',
  'MA', 'Mason',
  'OK', 'Okanogan',
  'PA', 'Pacific',
  'PE', 'Pend Oreille',
  'PI', 'Pierce',
  'SJ', 'San Juan',
  'SK', 'Skagit',
  'SM', 'Skamania',
  'SN', 'Snohomish',
  'SP', 'Spokane',
  'ST', 'Stevens',
  'TH', 'Thurston',
  'WK', 'Wahkiakum',
  'WL', 'Walla Walla',
  'WM', 'Whatcom',
  'WT', 'Whitman',
  'YA', 'Yakima'
)

wa_gender_codes <- tibble::tribble(
  ~gender, ~gender_code,
  'Male', 'M',
  'Female', 'F',
  'Unknown', 'U'
)

wa_voter_status <- tibble::tribble(
  ~voter_status, ~voter_status_code,
  'Active', 'A',
  'Inactive', 'I'
)
