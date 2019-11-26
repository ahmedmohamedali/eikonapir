#' get_symbology
#'
#' Returns a list of instrument names converted into another instrument code.
#' For example, convert SEDOL instrument names to RIC names.
#'
#' @param symbol string or list of strings.
#'   Single instrument or list of instruments to convert.

#' @param from_symbol_type string.
#'   Instrument code to convert from.
#'   Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker' (Default 'RIC')

#' @param to_symbol_type string or list
#'   Instrument code to convert to.
#'   Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker'

#' @param raw_output boolean.
#'   Set this parameter to `TRUE` to get the data in json format;
#'   if set to `FALSE`, the function will return a data frame.
#'   The default value is `FALSE`.
#'
#' @param debug boolean.
#'   When set to `TRUE`, the json request and response are printed.
#'
#' @return A data frame containing the converted symbols
#'   or a raw output JSON string.
#'
#' @examples
#' \dontrun{
#' get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"), from_symbol_type="RIC", to_symbol_type="ISIN")
#' }
#' @export
get_symbology <- function(symbol, from_symbol_type='RIC', to_symbol_type=NULL, raw_output=FALSE, debug=FALSE)
{
  Symbology_endpoint = 'SymbologySearch'
  if (is.character(to_symbol_type))
  {
    to_symbol_type <- list(to_symbol_type)
  }

  if (is.character(symbol))
  {
    symbol <- list(symbol)
  }

  if (!is.character(from_symbol_type))
  {
    print("get_symbology error - the parameter from_symbol_type should be a string")
    return(NULL)
  }

  payload <- list('symbols'=symbol,'from'=from_symbol_type,'to'=to_symbol_type,'bestMatchOnly'=TRUE)
  json_data = send_json_request(Symbology_endpoint,payload,debug)
  if (raw_output == TRUE)
  {
    return (json_data)
  }
  data = jsonlite::fromJSON(json_data)
  return (data.frame("Symbol"=data$mappedSymbols$symbol,data$mappedSymbols$bestMatch))

}

