set_app_id <- function(appId) {
  requestInfo <<- RequestInfo(appId)
}

get_app_id <- function() {
  return(requestInfo@application_id)
}

get_request_info <- function() {
  return(requestInfo)
}



RequestInfo <- setClass(
  # Set the name for the class
  "RequestInfo",

  # Define the slots
  slots = c(
    application_id  = "character",
    port            = "integer",
    url             = "character",
    streaming_url   = "character"
  ),

  prototype=list(application_id="",port=36036L,url="",streaming_url=""),

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



RequestInfo <- function(appId)  {

  port_number <- 36036L
  request_url <- paste('http://localhost:',port_number,'/api/v1/data',sep='')
  request_streaming_url <- paste('ws://localhost:',port_number,'/?',sep='')
  new("RequestInfo",application_id=appId, port=36036L,url=request_url,streaming_url= request_streaming_url)

}


