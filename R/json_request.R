
send_json_request <- function(entity, request_data, debug=FALSE)
{

  if (typeof(request_data) == 'character')
  {
    payload = jsonlite::fromJSON(request_data)
  }
  else
  {
    payload = request_data;
  }

  url = paste('http://localhost:',get_proxy_port(),'/api/v1/data',sep='')
  request <- list('Entity'= list('E'= entity, 'W'= payload))
  response <- httr::POST(url, httr::add_headers('Content-Type'='application/json','x-tr-applicationid'=get_app_id()),body=request,encode = "json")
  response_data <- httr::content(response, "text")
  response_status <- response$status_code

  if (debug)
  {
    print("Request *************************************")
    print(jsonlite::toJSON(request, auto_unbox = TRUE))
    print("Response *************************************")
    print(response_data)
    print("Response status *************************************")
    print(response_status)

  }

  if (response$status_code == 200)
   {
      return (response_data)
   }
   else
   {
     print("HTTP Error, code= ", response$status_code, sep="")
     return (NULL)
   }

}


# def get_data_value(value):
#   if is_string_type(value):
#   return value
# elif value is dict:
#   return value['value']
# else:
#   return value
#
#
# def get_data_frame(data_dict):
#   headers = [header['displayName'] for header in data_dict['headers'][0]]
# data = np.array([[get_data_value(value) for value in row] for row in data_dict['data']])
# df = pd.DataFrame(data, columns=headers)
# df = df.apply(pd.to_numeric, errors='ignore')
# errors = get_json_value(data_dict, 'error')
# return df, errors


