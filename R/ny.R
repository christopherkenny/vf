#' Read NY voter file
#'
#' @param path path to FL detail file or directory
#' @param ... additional arguments passed to `readr::read_csv()`
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_ny <- function(path, ...) {

  if (missing(path)) {
    cli::cli_abort('Must specify {.arg path}.')
  }

  readr::read_csv(
    file = path,
    col_names = ny_names,
    ...
  )
}

ny_names <- c(
  'name_last',
  'name_first',
  'name_middle',
  'name_suffix',
  'residence_address_number',
  'residence_address_half_code',
  'residence_address_direction',
  'residence_address_unit_type',
  'residence_address_unit_number',
  'residence_address_nonstandard',
  'residence_city',
  'residence_zipcode',
  'residence_zipcode_plus4',
  'mailing_address_line_1',
  'mailing_address_line_2',
  'mailing_address_line_3',
  'mailing_address_line_4',
  'birth_date',
  'gender',
  'party_affiliation',
  'other_party_affiliation',
  'county',
  'election_district',
  'legislative_district',
  'city',
  'ward',
  'congressional_district',
  'senate_district',
  'assembly_district',
  'last_voted_date',
  'last_voted_county',
  'last_voted_address',
  'last_voted_name',
  'county_voter_registration_number',
  'registration_date',
  'voter_application_source',
  'voter_id_required',
  'voter_id_requirement_met',
  'voter_status',
  'voter_status_reason',
  'inactive_date',
  'purge_date',
  'voter_id',
  'voter_history'
)
