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
    readr::read_csv(path)
  })

}

oh_county_codes <- tibble::tibble(
  county = c("Adams", "Allen", "Ashland", "Ashtabula", "Athens", "Auglaize",
              "Belmont", "Brown", "Butler", "Carroll", "Champaign", "Clark",
              "Clermont", "Clinton", "Columbiana", "Coshocton", "Crawford",
              "Cuyahoga", "Darke", "Defiance", "Delaware", "Erie", "Fairfield",
              "Fayette", "Franklin", "Fulton", "Gallia", "Geauga", "Greene",
              "Guernsey", "Hamilton", "Hancock", "Hardin", "Harrison", "Henry",
              "Highland", "Hocking", "Holmes", "Huron", "Jackson", "Jefferson",
              "Knox", "Lake", "Lawrence", "Licking", "Logan", "Lorain", "Lucas",
              "Madison", "Mahoning", "Marion", "Medina", "Meigs", "Mercer",
              "Miami", "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum",
              "Noble", "Ottawa", "Paulding", "Perry", "Pickaway", "Pike", "Portage",
              "Preble", "Putnam", "Richland", "Ross", "Sandusky", "Scioto",
              "Seneca", "Shelby", "Stark", "Summit", "Trumbull", "Tuscarawas",
              "Union", "Van Wert", "Vinton", "Warren", "Washington", "Wayne",
              "Williams", "Wood", "Wyandot"),
  county_code = 1:88
)
