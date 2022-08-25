#' Read NC voter file
#'
#' @param path_registration path to NC registration file or directory
#' @param path_history path to NC history file or directory
#' @param ... additional arguments passed to `readr::read_csv()`
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_nc <- function(path_registration, path_history, ...) {

  has_registration <- !missing(path_registration)
  has_history <- !missing(path_history)

  if (!has_registration && !has_history) {
    cli::cli_abort('Must specify at least one of {.arg path_registration} or {.arg path_history}.')
  }

  if (has_registration) {
    if (any(fs::is_dir(path_registration))) {
      if (!all(fs::is_dir(path_registration))) {
        cli::cli_abort('{.arg path_registration} must be either (1) all files or (2) one directory and no files.')
      }
      path_registration <- fs::dir_ls(path_registration, glob = '*.txt')
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

  if (has_registration) {
    registration <- purrr::map_dfr(path_registration, function(f) {
      readr::read_tsv(
        file = f,
        col_names = nc_registration_names,
        col_types = readr::cols(
          county_code                  = readr::col_character(),
          county                       = readr::col_character(),
          county_voter_reg_num         = readr::col_character(),
          ncid                         = readr::col_character(),
          name_last                    = readr::col_character(),
          name_first                   = readr::col_character(),
          name_middle                  = readr::col_character(),
          name_suffix                  = readr::col_character(),
          status_cd                    = readr::col_character(),
          voter_status_description     = readr::col_character(),
          reason_cd                    = readr::col_character(),
          voter_status_reason          = readr::col_character(),
          residence_address            = readr::col_character(),
          residence_city               = readr::col_character(),
          residence_state              = readr::col_character(),
          residence_zipcode            = readr::col_character(),
          mailing_address_line_1       = readr::col_character(),
          mailing_address_line_2       = readr::col_character(),
          mailing_address_line_3       = readr::col_character(),
          mailing_address_line_4       = readr::col_character(),
          mailing_city                 = readr::col_character(),
          mailing_state                = readr::col_character(),
          mailing_zipcode              = readr::col_character(),
          phone_number                 = readr::col_character(),
          confidential_ind             = readr::col_character(),
          registration_date            = readr::col_character(),
          race                         = readr::col_character(),
          ethnicity                    = readr::col_character(),
          party_affiliation            = readr::col_character(),
          gender                       = readr::col_character(),
          birth_year                   = readr::col_character(),
          age_at_year_end              = readr::col_character(),
          birth_state                  = readr::col_character(),
          drivers_lic                  = readr::col_character(),
          precinct_abb                 = readr::col_character(),
          precinct                     = readr::col_character(),
          municipality_abb             = readr::col_character(),
          municipality                 = readr::col_character(),
          ward_abb                     = readr::col_character(),
          ward                         = readr::col_character(),
          congressional_district       = readr::col_character(),
          superior_court               = readr::col_character(),
          judicial_district            = readr::col_character(),
          senate_district              = readr::col_character(),
          house_district               = readr::col_character(),
          county_commission_abb        = readr::col_character(),
          county_commission            = readr::col_character(),
          township_abb                 = readr::col_character(),
          township                     = readr::col_character(),
          school_district_abb          = readr::col_character(),
          school_district              = readr::col_character(),
          fire_district_abb            = readr::col_character(),
          fire_district                = readr::col_character(),
          water_district_abb           = readr::col_character(),
          water_district               = readr::col_character(),
          sewer_district_abb           = readr::col_character(),
          sewer_district               = readr::col_character(),
          sanit_district_abb           = readr::col_character(),
          sanit_district               = readr::col_character(),
          rescue_district_abb          = readr::col_character(),
          rescue_district              = readr::col_character(),
          munic_district_abb           = readr::col_character(),
          munic_district               = readr::col_character(),
          presecutorial_district_1_abb = readr::col_character(),
          presecutorial_district_1     = readr::col_character(),
          vtd_abb                      = readr::col_character(),
          vtd                          = readr::col_character()
        )
      )
    })
  }


  registration
}

nc_registration_names <- c(
  'county_code',
  'county',
  'county_voter_reg_num',
  'ncid',
  'name_last',
  'name_first',
  'name_middle',
  'name_suffix',
  'status_cd',
  'voter_status_description',
  'reason_cd',
  'voter_status_reason',
  'residence_address',
  'residence_city',
  'residence_state',
  'residence_zipcode',
  'mailing_address_line_1',
  'mailing_address_line_2',
  'mailing_address_line_3',
  'mailing_address_line_4',
  'mailing_city',
  'mailing_state',
  'mailing_zipcode',
  'phone_number',
  'confidential_ind',
  'registration_date',
  'race',
  'ethnicity',
  'party_affiliation',
  'gender',
  'birth_year',
  'age_at_year_end',
  'birth_state',
  'drivers_lic',
  'precinct_abb',
  'precinct',
  'municipality_abb',
  'municipality',
  'ward_abb', 'ward',
  'congressional_district',
  'superior_court',
  'judicial_district',
  'senate_district',
  'house_district',
  'county_commission_abb',
  'county_commission',
  'township_abb',
  'township',
  'school_district_abb',
  'school_district',
  'fire_district_abb',
  'fire_district',
  'water_district_abb',
  'water_district',
  'sewer_district_abb',
  'sewer_district',
  'sanit_district_abb',
  'sanit_district',
  'rescue_district_abb',
  'rescue_district',
  'munic_district_abb',
  'munic_district',
  'presecutorial_district_1_abb',
  'presecutorial_district_1',
  'vtd_abb',
  'vtd'
)

nc_history_names <- c(
  'county_code',
  'county',
  'county_voter_reg_num',
  'election_date_label',
  'election_description',
  'voting_method',
  'voted_party_code',
  'voted_party',
  'precinct_label',
  'precinct',
  'ncid',
  'voted_county_code',
  'voted_county',
  'vtd_label',
  'vtd'
)

nc_county_codes <- tibble::tribble(
  ~county, ~county_code,
  'ALAMANCE', '1',
  'ALEXANDER', '2',
  'ALLEGHANY', '3',
  'ANSON', '4',
  'ASHE', '5',
  'AVERY', '6',
  'BEAUFORT', '7',
  'BERTIE', '8',
  'BLADEN', '9',
  'BRUNSWICK', '10',
  'BUNCOMBE', '11',
  'BURKE', '12',
  'CABARRUS', '13',
  'CALDWELL', '14',
  'CAMDEN', '15',
  'CARTERET', '16',
  'CASWELL', '17',
  'CATAWBA', '18',
  'CHATHAM', '19',
  'CHEROKEE', '20',
  'CHOWAN', '21',
  'CLAY', '22',
  'CLEVELAND', '23',
  'COLUMBUS', '24',
  'CRAVEN', '25',
  'CUMBERLAND', '26',
  'CURRITUCK', '27',
  'DARE', '28',
  'DAVIDSON', '29',
  'DAVIE', '30',
  'DUPLIN', '31',
  'DURHAM', '32',
  'EDGECOMBE', '33',
  'FORSYTH', '34',
  'FRANKLIN', '35',
  'GASTON', '36',
  'GATES', '37',
  'GRAHAM', '38',
  'GRANVILLE', '39',
  'GREENE', '40',
  'GUILFORD', '41',
  'HALIFAX', '42',
  'HARNETT', '43',
  'HAYWOOD', '44',
  'HENDERSON', '45',
  'HERTFORD', '46',
  'HOKE', '47',
  'HYDE', '48',
  'IREDELL', '49',
  'JACKSON', '50',
  'JOHNSTON', '51',
  'JONES', '52',
  'LEE', '53',
  'LENOIR', '54',
  'LINCOLN', '55',
  'MACON', '56',
  'MADISON', '57',
  'MARTIN', '58',
  'MCDOWELL', '59',
  'MECKLENBURG', '60',
  'MITCHELL', '61',
  'MONTGOMERY', '62',
  'MOORE', '63',
  'NASH', '64',
  'NEWHANOVER', '65',
  'NORTHAMPTON', '66',
  'ONSLOW', '67',
  'ORANGE', '68',
  'PAMLICO', '69',
  'PASQUOTANK', '70',
  'PENDER', '71',
  'PERQUIMANS', '72',
  'PERSON', '73',
  'PITT', '74',
  'POLK', '75',
  'RANDOLPH', '76',
  'RICHMOND', '77',
  'ROBESON', '78',
  'ROCKINGHAM', '79',
  'ROWAN', '80',
  'RUTHERFORD', '81',
  'SAMPSON', '82',
  'SCOTLAND', '83',
  'STANLY', '84',
  'STOKES', '85',
  'SURRY', '86',
  'SWAIN', '87',
  'TRANSYLVANIA', '88',
  'TYRRELL', '89',
  'UNION', '90',
  'VANCE', '91',
  'WAKE', '92',
  'WARREN', '93',
  'WASHINGTON', '94',
  'WATAUGA', '95',
  'WAYNE', '96',
  'WILKES', '97',
  'WILSON', '98',
  'YADKIN', '99',
  'YANCEY', '00'
)
