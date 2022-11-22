#' Read OK voter file
#'
#' @param path_registration path to OK registration file or directory
#' @param ... additional arguments passed to `readr::read_csv()`
#'
#' @return tibble
#' @export
#'
#' @examples
#' # TODO
vf_read_ok <- function(path_registration, ...) {

  if (missing(path_registration)) {
    cli::cli_abort('Must specify {.arg path_registration}.')
  }

  if (any(fs::is_dir(path_registration))) {
    if (!all(fs::is_dir(path_registration))) {
      cli::cli_abort('{.arg path_registration} must be either (1) all files or (2) one directory and no files.')
    }
    path_registration <- fs::dir_ls(path_registration, glob = '*.csv')
  }
  if (any(fs::path_ext(path_registration) == 'zip')) {
    cli::cli_abort(c('{.arg path_registration} may not contain zip files (files ending in {.val .zip}).',
                     'i' = 'Please unzip the files, possibly with {.fn utils::unzip} or {.fn zip::unzip}.'))
  }

  purrr::map(path_registration, function(path) {
    out <- readr::read_csv(
      path
    )
  })

}
