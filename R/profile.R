#'
#' init
#'
#' Initialize the library context
#'
#'
#' @examples
#'--------
#' library(eikon_r)
#' init()

init <- function()
{

  if (exists("requestInfo") == FALSE)
  {
    requestInfo <<- new("RequestInfo")
  }


}

#'
#' set_app_id
#'
#' Use this function to set your application id.
#' The application id should be set before calling functions to retrieve data
#' @param appId: string
#'
#'
#' @examples
#'--------
#' library(eikon_r)
#' set_app_id('YOUR_APP_ID')

set_app_id <- function(appId) {
  init()
  requestInfo@application_id <<- appId
}


#'
#' get_app_id
#'
#' Use this function to get back the application id you have set earlier with set_app_id.
#'
#'
#' @examples
#'--------
#' library(eikon_r)
#' my_app_id = get_app_id()

get_app_id <- function(appId) {
  init()
  return (requestInfo@application_id)
}


#'
#' set_proxy_port
#'
#' By default the library will try to connect to the proxy default port 9000.
#' Use this function if the proxy is listening on another port than 9000
#' @param port: integer
#'
#'
#' @examples
#'--------
#' library(eikon_r)
#' set_proxy_port(37009L)
set_proxy_port <- function(port) {
  init()
  requestInfo@proxy_port <<- port
}

#'
#' get_proxy_port
#'
#' Use this function to get back the proxy port the library will connect to
#'
#'
#' @examples
#'--------
#' library(eikon_r)
#' prox_port = get_proxy_port()

get_proxy_port <- function(port) {
  init()
  return (requestInfo@proxy_port)
}




RequestInfo <- setClass(
  # Set the name for the class
  "RequestInfo",

  # Define the slots
  slots = c(
    application_id  = "character",
    proxy_port            = "integer",
    url             = "character"
  ),

  prototype=list(application_id="",proxy_port=9000L,url=""),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    #if(sum(object@velocity^2)>100.0) {
      #return("The velocity level is out of bounds.")
    #}
    return(TRUE)
  }


)






