#' Read OH voter file
#'
#' @param path_registration path to OH registration file or directory
#' @param ... additional arguments passed to `readr::read_csv()`
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_oh <- function(path_registration, ...) {

  if (missing(path_registration)) {
    cli::cli_abort('Must specify {.arg path_registration}.')
  }

  if (any(fs::is_dir(path_registration))) {
    if (!all(fs::is_dir(path_registration))) {
      cli::cli_abort('{.arg path_registration} must be either (1) all files or (2) one directory and no files.')
    }
    path_registration <- fs::dir_ls(path_registration, glob = '*.txt')
  }
  if (any(fs::path_ext(path_registration) == 'gz')) {
    cli::cli_abort(c('{.arg path_registration} may not contain gzip files (files ending in {.val .gz}).',
                     'i' = 'Please unzip the files, possibly with {.fn R.utils::gunzip}.'))
  }

  purrr::map(path_registration, function(path) {
    out <- readr::read_csv(
      path,
      col_types = readr::cols(
        SOS_VOTERID                   = readr::col_character(),
        COUNTY_NUMBER                 = readr::col_character(),
        COUNTY_ID                     = readr::col_double(),
        LAST_NAME                     = readr::col_character(),
        FIRST_NAME                    = readr::col_character(),
        MIDDLE_NAME                   = readr::col_character(),
        SUFFIX                        = readr::col_character(),
        DATE_OF_BIRTH                 = readr::col_date(format = ""),
        REGISTRATION_DATE             = readr::col_date(format = ""),
        VOTER_STATUS                  = readr::col_character(),
        PARTY_AFFILIATION             = readr::col_character(),
        RESIDENTIAL_ADDRESS1          = readr::col_character(),
        RESIDENTIAL_SECONDARY_ADDR    = readr::col_character(),
        RESIDENTIAL_CITY              = readr::col_character(),
        RESIDENTIAL_STATE             = readr::col_character(),
        RESIDENTIAL_ZIP               = readr::col_double(),
        RESIDENTIAL_ZIP_PLUS4         = readr::col_character(),
        RESIDENTIAL_COUNTRY           = readr::col_character(),
        RESIDENTIAL_POSTALCODE        = readr::col_character(),
        MAILING_ADDRESS1              = readr::col_character(),
        MAILING_SECONDARY_ADDRESS     = readr::col_character(),
        MAILING_CITY                  = readr::col_character(),
        MAILING_STATE                 = readr::col_character(),
        MAILING_ZIP                   = readr::col_double(),
        MAILING_ZIP_PLUS4             = readr::col_character(),
        MAILING_COUNTRY               = readr::col_character(),
        MAILING_POSTAL_CODE           = readr::col_character(),
        CAREER_CENTER                 = readr::col_character(),
        CITY                          = readr::col_character(),
        CITY_SCHOOL_DISTRICT          = readr::col_character(),
        COUNTY_COURT_DISTRICT         = readr::col_logical(),
        CONGRESSIONAL_DISTRICT        = readr::col_character(),
        COURT_OF_APPEALS              = readr::col_character(),
        EDU_SERVICE_CENTER_DISTRICT   = readr::col_character(),
        EXEMPTED_VILL_SCHOOL_DISTRICT = readr::col_character(),
        LIBRARY                       = readr::col_logical(),
        LOCAL_SCHOOL_DISTRICT         = readr::col_character(),
        MUNICIPAL_COURT_DISTRICT      = readr::col_character(),
        PRECINCT_NAME                 = readr::col_character(),
        PRECINCT_CODE                 = readr::col_character(),
        STATE_BOARD_OF_EDUCATION      = readr::col_character(),
        STATE_REPRESENTATIVE_DISTRICT = readr::col_double(),
        STATE_SENATE_DISTRICT         = readr::col_character(),
        TOWNSHIP                      = readr::col_character(),
        VILLAGE                       = readr::col_character(),
        WARD                          = readr::col_character(),
        .default                      = readr::col_character()
      )
                           ) |>
      dplyr::rename_with(.fn = function(x) {
        x |>
          stringr::str_to_lower() |>
          stringr::str_replace_all('-', '_') |>
          stringr::str_replace_all('/', '_')
      })
    names(out)[1:46] <- oh_registration_names
    out
  })

}

oh_county_codes <- tibble::tibble(
  county = c(
    'Adams', 'Allen', 'Ashland', 'Ashtabula', 'Athens', 'Auglaize',
    'Belmont', 'Brown', 'Butler', 'Carroll', 'Champaign', 'Clark',
    'Clermont', 'Clinton', 'Columbiana', 'Coshocton', 'Crawford',
    'Cuyahoga', 'Darke', 'Defiance', 'Delaware', 'Erie', 'Fairfield',
    'Fayette', 'Franklin', 'Fulton', 'Gallia', 'Geauga', 'Greene',
    'Guernsey', 'Hamilton', 'Hancock', 'Hardin', 'Harrison', 'Henry',
    'Highland', 'Hocking', 'Holmes', 'Huron', 'Jackson', 'Jefferson',
    'Knox', 'Lake', 'Lawrence', 'Licking', 'Logan', 'Lorain', 'Lucas',
    'Madison', 'Mahoning', 'Marion', 'Medina', 'Meigs', 'Mercer',
    'Miami', 'Monroe', 'Montgomery', 'Morgan', 'Morrow', 'Muskingum',
    'Noble', 'Ottawa', 'Paulding', 'Perry', 'Pickaway', 'Pike', 'Portage',
    'Preble', 'Putnam', 'Richland', 'Ross', 'Sandusky', 'Scioto',
    'Seneca', 'Shelby', 'Stark', 'Summit', 'Trumbull', 'Tuscarawas',
    'Union', 'Van Wert', 'Vinton', 'Warren', 'Washington', 'Wayne',
    'Williams', 'Wood', 'Wyandot'
  ),
  county_code = 1:88
)

oh_registration_names <- c(
  'sos_voterid',
  'county_number',
  'county_id',
  'last_name',
  'first_name',
  'middle_name',
  'suffix',
  'date_of_birth',
  'registration_date',
  'voter_status',
  'party_affiliation',
  'residential_address1',
  'residence_secondary_addr',
  'residence_city',
  'residence_state',
  'residence_zip',
  'residence_zip_plus4',
  'residence_country',
  'residence_postalcode',
  'mailing_address_line_1',
  'mailing_secondary_address',
  'mailing_city',
  'mailing_state',
  'mailing_zip',
  'mailing_zip_plus4',
  'mailing_country',
  'mailing_postal_code',
  'career_center',
  'city',
  'city_school_district',
  'county_court_district',
  'congressional_district',
  'court_of_appeals',
  'edu_service_center_district',
  'exempted_vill_school_district',
  'library',
  'local_school_district',
  'municipal_court_district',
  'precinct_name',
  'precinct_code',
  'state_board_of_education',
  'state_representative_district',
  'state_senate_district',
  'township',
  'village',
  'ward'
)
