#
#' get_timeseries
#'
#' Returns historical data for one or several RICs
#'
#' @param rics: string or list of strings.
#' Single RIC or List of RICs to retrieve historical data for
#' @param fields: string or list of strings
#' Use this parameter to filter the returned fields set.
#  List of available fields: ['TIMESTAMP', 'VALUE', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE', 'COUNT']
#  By default all fields are returned.
#
#' @param start_date: string. The start date of the historical range
#' string format is: %Y-%m-%dT%H:%M:%S. ex: 2016-01-20T15:04:05.
#' Default: current date - 100 days
#' @param end_date: string. The end date of the historical range.
#  string format is: %Y-%m-%dT%H:%M:%S. ex: 2016-01-20T15:04:05.
#  Default: current date
#' @param interval: string. the data interval.
#' Possible values: 'tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly' (Default 'daily')
#' @param count: int. The maximum number of data points you want tp retrieve.
#' @param calendar: string. Possible values: 'native', 'tradingdays', 'calendardays'.
#' @param corax: string. Possible values are : 'adjusted', 'unadjusted'
#' @param raw_output: boolean
#' Set this parameter to TRUE to get the data in json format
#' if set to FALSE, the function will return a data frame which shape is defined by the parameter normalize
#' The default value is False
#' @param normalize: boolean
#' if set to True, the function will return a normalized data frame with the following columns 'Date','Security','Field'
#' If the value of this parameter is False the returned data frame shape will have a different column for each field and a column
#' for the security
#' The default value is False
#' Remark: This parameter has a less precedence than the parameter rawOutput i.e. if rawOutput is set to True, the returned data will be the raw data and this parameter will be ignored
#' @param debug: bool
#' When set to True, the json request and response are printed.
#'
#'
#
#' @examples
#'
#' > eikonapir.set_app_id('YOUR_APP_ID')
#' > df = get_timeseries(list("MSFT.O","VOD.L","IBM.N"),list("*"),"2016-01-01T15:04:05","2016-01-10T15:04:05","daily")
#' > print(df)


get_timeseries <- function(rics,fields='*', start_date=NULL, end_date=NULL, interval='daily', normalize=FALSE, count=NULL,
                   calendar=NULL, corax=NULL,raw_output=FALSE, debug=FALSE)
{



  TimeSeries_endpoint <- 'TimeSeries'
  Calendar_Values <- list('native', 'tradingdays', 'calendardays')
  Corax_Values <- list('adjusted', 'unadjusted')


  if (is.character(rics))
  {
    rics <- list(trimws(rics))
  }


  payload = list('rics'= rics)



  if ( '*' %in% fields)
  {
    fields <- list('*')
  }
  else
  {
    append(fields, 'TIMESTAMP')
  }



  if (!is.character(interval))
  {
    print('get_timeseries error: The interval parameter should be of character string')
    return(NULL)
  }


  # set start_date / end_date in the payload
  if (is.null(start_date))
  {
    # Get the current date/time - 100 days with the timezone.
    start_date <- strftime(Sys.time() - 86400*100, "%Y-%m-%dT%H:%M:%S%z")
    start_date <- paste0(substr(start_date, 1, nchar(start_date)-2),":", substr(start_date, nchar(start_date)-1, nchar(start_date)))
  }

  if (is.null(end_date))
  {
    # Get the current date/time with the timezone.
    end_date <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    end_date <- paste0(substr(end_date, 1, nchar(end_date)-2),":", substr(end_date, nchar(end_date)-1, nchar(end_date)))
  }



  payload <- list('rics'= rics, 'fields'= fields, 'interval'= interval, 'startdate' = start_date,'enddate'= end_date)

  if (!set_integer_property(payload,'count',count))
  {
    print('get_timeseries error: count must be an integer')
    return (NULL)
  }

  if (!set_string_property(payload,'calendar',calendar))
  {
    print('get_timeseries error: calendar must be a string')
    return (NULL)
  }

  if (!set_string_property(payload,'corax',corax))
  {
    print('get_timeseries error: corax must be a string')
    return (NULL)
  }


  json_data = send_json_request(TimeSeries_endpoint, payload, debug=debug)

  if (raw_output == TRUE)
  {
    return (json_data)
  }

  data = jsonlite::fromJSON(json_data)

  if (normalize == TRUE)
  {
    return(get_normalized_data_frame(data))
  }

  return (get_formatted_data_frame(data))


}

get_normalized_data_frame <- function(data)
{
  input_data_frame = data$timeseriesData
  data_frames <- list()
  for (i in 1:nrow(input_data_frame))
  { current_row = input_data_frame[i,]
      fields  = current_row$fields[[1]]$name
      data_frame  = as.data.frame(current_row$dataPoints[[1]],stringsAsFactors = FALSE)
      names(data_frame) <- fields
      data_fields_count = length(fields) - 1 # do not account for TIMESTAMP field
      timestamps = data_frame[,"TIMESTAMP"]
      timestamps_column = rep(timestamps,data_fields_count)
      normalized_frame <- data.frame(Date=timestamps_column)
      ric = current_row$ric
      ric_column = rep(ric,nrow(data_frame))
      ric_column = rep(ric_column,length(fields) - 1)
      normalized_frame <- cbind(normalized_frame, Security=ric_column)
      values_columns = data_frame[,!(names(data_frame) %in% c('TIMESTAMP'))]
      values_columns = stack(values_columns)
      values_columns = data.frame(Field = values_columns[,2],Value=values_columns[,1])
      normalized_frame <- cbind(normalized_frame, values_columns)
      data_frames[[i]] = normalized_frame

  }

  combined_data_frame = do.call("rbind", data_frames)
  combined_data_frame = combined_data_frame[order( combined_data_frame[,'Security'],combined_data_frame[,'Date']) ,]
  rownames(combined_data_frame) <- NULL#seq(length=nrow(combined_data_frame))
  return (combined_data_frame)

}




get_formatted_data_frame  <- function(data)
{

  input_data_frame = data$timeseriesData
  data_frames <- list()
  for (i in 1:nrow(input_data_frame))
  { current_row = input_data_frame[i,]
    current_fields  = current_row$fields[[1]]$name
    ric_column = rep( current_row$ric,nrow(input_data_frame[i,]))
    data_frame  <- as.data.frame(current_row$dataPoints[[1]],stringsAsFactors = FALSE)
    data_frame <- cbind(data_frame, Security=ric_column)
    names(data_frame) <- current_fields
    data_frames[[i]] = data_frame
  }
  return (do.call("rbind", data_frames))

}





### Utility functions
set_integer_property <- function(list_object,property,value)
{
  # set the count in the payload
  if (!is.null(value))
  {
    if (!is.integer(value))
    {
      print('get_timeseries error: count must be an integer')
      return(FALSE)
    }

    list_object[property] = value

  }

  return(TRUE);
}


set_string_property <- function(list_object,property,value)
{
  if (!is.null(value))
  {
    if (is.character(value))
    {
      payload[property] = value
      return (TRUE)
    }
    else
    {

      return (FALSE)
    }
  }

  return (TRUE)


}

