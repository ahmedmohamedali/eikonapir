DataGrid_UDF_endpoint = 'DataGrid'


#
#' TR_Field
#'
#' This is a helper function to build the field for the get_data function
#' Returns a dictionary that can directly passed to get_data
#'
#' @param field_name: string
#' @param params: dictionary containing the parameters for the field passed in the argument field_name
#' @param sort_dir: string. Indicate the sort direction. Possible values are 'asc' or 'desc'. The default value is 'asc'
#' @param sort_priority: integer. Gives a piority to the field for the sorting. The highest priority is 0 (zero). the default value is None
#'
#' @examples
#' > TR_Field('tr.revenue')
#' > TR_Field('tr.open','asc',1)
#' > TR_Field('TR.GrossProfit',list('Scale': 6, 'Curn': 'EUR'),'asc',0)

TR_Field  <- function (field_name,params = NULL,sort_dir ='asc',sort_priority=NULL)
{

  if(!is.null(params) && !is_named_list(params))
  {
    print('TR_Field error: The argument params must be a named list')
    return (NULL)
  }

  if (is_named_list(params) && length(params) == 0)
  {
    print('TR_Field error: The argument params must be a non empty named list or NULL (default)')
    return (NULL)
  }

  field <- list()
  field[[field_name]] <- list()

  if (!is.null(params)) { field[[field_name]][['params']] = params }
  field[[field_name]][['sort_dir']] = sort_dir
  if (!is.null(sort_priority)) { field[[field_name]][['sort_priority']] = sort_priority }

  return (field)
}


#'
#'get_data
#'
#' Retreives fields for instruments with the sepcified parameters and sort criteria. The result is returned in a data frame
#'
#' @param instruments: string or list. A single instrument or list of instruments to request.
#'
#' @param fields: string, named list or list of string and/or named list
#'   - Single field as a string: 'TR.PriceClose'
#'   - Single field in a named list for providing parameters and/or sort option:
#'     ex:
#'       list('TR.GrossProfit'=list('params'=list( 'Scale'= 6, 'Curn'='EUR')))
#'       list('TR.GrossProfit'= list('params'= list('Scale'= 6, 'Curn'= 'EUR'),sort_dir='desc'))
#'   -  Multiple fields could be set in a non named list. Items could be strings and/or named lists
#'     ex:
#'       list('TR.PriceClose','TR.PriceOpen')
#'       list(list('TR.PriceClose'= list('sort_dir'='asc','sort_priority'=1)),list('TR.PriceOpen'=list('sort_dir'='asc','sort_priority'=0)))
#'
#'   You can use the function TR_Field to build the fields more easily:
#'      ex:
#'      fields = list(TR_Field('tr.revenue'),TR_Field('tr.open',NULL,'asc',1),TR_Field('TR.GrossProfit',list('Scale'=6, 'Curn'='EUR'),'asc',0))
#'      data_frame = get_data(list("IBM","MSFT.O"),fields)
#'
#'  Tips: You can launch the Data Item Browser to discover fields and parameters
#'  or copy field names and parameters from TR Eikon - MS Office formulas
#'
#' @param parameters: string or named list. Single global parameter or a list of key=value in the named list.
#'
#' @param raw_output: boolean. By default the output is a data frame. Set raw_output=True to get data in Json format.
#'
#' @param debug: bool. When set to True, the json request and response are printed.
#'
#'
#' @examples
#' --------

#' > set_app_id('set your app id here')
#' > data_frame = get_data(list("IBM", "GOOG.O", "MSFT.O"), list("TR.PriceClose", "TR.Volume", "TR.PriceLow"))
#' > data_frame = get_data("IBM", list('TR.Employees', list('TR.GrossProfit'= list('params'=list('Scale'= 6, 'Curn'= 'EUR'),'sort_dir'='asc'))))
#' > fields = list(TR_Field('tr.revenue'),TR_Field('tr.open',NULL,'asc',1),TR_Field('TR.GrossProfit',list('Scale'=6, 'Curn'='EUR'),'asc',0))
#' > data_frame = get_data(list("IBM","MSFT.O"),fields)


get_data <- function(instruments, fields, parameters=NULL, raw_output=FALSE, debug=FALSE)
{

    instruments = parse_instrument(instruments)
    if (is.null(instruments))
    {
      return(NULL)
    }

    fields = parse_fields(fields)
    if (is.null(fields))
    {
      return(NULL)
    }

    fields_for_request <- list()

    for( i in 1:length(fields))
    {
      f = fields[[i]]
      field_name = names(f)[1]
      field_info = f[[field_name]]
      field <- list('name'=field_name)
      if ('sort_dir' %in% names(field_info)) { field[['sort']] = field_info[['sort_dir']]}
      if ('sort_priority' %in% names(field_info)) { field[['sortPriority']] = field_info[['sort_priority']]}
      if ('params' %in% names(field_info)) { field[['parameters']] = field_info[['params']]}
      fields_for_request[[i]] <- field

    }

    parameters = parse_parameters(parameters)


    payload = list('instruments'= instruments,'fields'= fields_for_request)
    if (!is.null(parameters))
    {
      payload[['parameters']] = parameters
    }

    json_data = send_json_request(DataGrid_UDF_endpoint, payload, debug=debug)

    if (raw_output)
    {
      return(json_data)
    }

    result = jsonlite::fromJSON(json_data)
    data_frame = data.frame(result$data)
    names(data_frame) <- result$headers[[1]]$displayName

    return(data_frame)
}


parse_instrument <- function(instruments)
{
  if(is_string(instruments))
  {
    instruments = trimws(instruments)
    return(list(instruments))
  }
  else if (is_non_named_list(instruments))
  {
    instrument_list = lapply(instruments, is_string)
    if (length(instrument_list) != length(instruments))
    {
      print("All the symbols in the instruments parameter should have the data type string")
      return(NULL)
    }

    return(instruments)
  }

  print("The parameter instruments should be a single string or a non named list of strings")
  return (NULL)
}



parse_fields <- function (fields)
{

  if(is_string(fields))
  {
    fields = trimws(fields)
    new_list <- list()
    new_list[[fields]] <- list()
    return(list(new_list))
  }

  if (!is.list(fields))
  {
    print('get_data error: the fields parameter should be a string, a named list , or a list of strings or/and named list')
    return (NULL)
  }


  if(is_named_list(fields)) {

    if (length(fields) != 1)
    {
      print('get_data error: the named list in the fields parameter cannot have more than one element')
      return (NULL)
    }

    field_info = fields[[1]]

    if (length(field_info) > 0 && !is_named_list(field_info))
    {
      print('get_data error: The field set in the fields parameter has an incorrect format. Please refer to the documentation ')
      return (NULL)
    }

    return(list(fields))
  }

  new_list <- list()
  for( i in 1:length(fields)) {
      f = fields[[i]]
      if (is_string(f)) {
          l <- list()
          l[[f]] <- list()
          new_list[[i]] <- l}
      else if (is_named_list(f)) {new_list[[i]] <- f}
      else {
        print('get_data error: the parameter fields should be of type string or a named list ')
        return (NULL)
      }
   }

   return(new_list)

}


parse_parameters <- function(parameters)
{
  if (is.null(parameters))
  {
    return(parameters)
  }

  if(is_string(parameters))
  {
    parameters = jsonlite::fromJSON(parameters)
    return (parameters)
  }

  if (is_named_list(parameters))
  {
    return (parameters)
  }


  print('get_data error: the argument parameters should be a string containing a json of key/value pairs or a named list')
  return (NULL)

}


