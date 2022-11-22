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

  if (has_registration) {
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
  }

  purrr::map(path_registration, function(path) {
    readr::read_csv(path)
  })

}
