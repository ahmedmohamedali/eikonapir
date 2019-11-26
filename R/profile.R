#' set_app_key
#'
#' Use this function to set your application key.
#' The application key should be set before calling functions to retrieve data.
#'
#' @param appKey string.
#'
#' @examples
#' set_app_key('YOUR_APP_KEY')
#' @export
set_app_key <- function(appKey) {
  options('eikon_application_key' = appKey)
}


#' get_app_key
#'
#' Use this function to get back the application key you have set earlier with set_app_key.
#'
#' @return A string.
#'
#' @examples
#' my_app_key = get_app_key()
#' @export
get_app_key <- function() {
  app_key <- getOption('eikon_application_key', default = NULL)
  if (is.null(app_key)) {
    warning('INFO: Eikon application key is NULL')
  }
  return(app_key)
}


#' set_app_id
#'
#' Use this function to set your application id.
#' The application id should be set before calling functions to retrieve data.
#' DEPRECATED.
#'
#' @param appId string.
#'
#' @examples
#' set_app_id('YOUR_APP_KEY')
#' @export
set_app_id <- function(appId) {
  set_app_key(appId)
}


#' get_app_id
#'
#' Use this function to get back the application id you have set earlier with set_app_id.
#' DEPRECATED.
#'
#' @examples
#' my_app_key = get_app_id()
#' @export
get_app_id <- function() {
  get_app_key()
}


#' set_proxy_port
#'
#' By default the library will try to connect to the proxy default port 9000.
#' Use this function if the proxy is listening on another port than 9000.
#'
#' @param port integer.
#'
#' @examples
#' set_proxy_port(37009L)
#' @export
set_proxy_port <- function(port) {
  options('eikon_proxy_port' = port)
}

#' get_proxy_port
#'
#' Use this function to get back the proxy port the library will connect to.
#'
#' @return An integer.
#'
#' @examples
#' prox_port = get_proxy_port()
#' @export
get_proxy_port <- function() {
  getOption('eikon_proxy_port', default = 9000L)
}
