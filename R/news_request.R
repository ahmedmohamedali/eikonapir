#' get_news_headlines
#'
#' Returns a data frame contianing the news headlines
#'
#' @param query string, optional. Search criteria for the news headlines
#' The query can contain RIC codes, company names, country names and
#' operators (AND, OR, NOT, IN, parentheses and quotes for explicit search…).
#' Tip: Append 'R:' in front of RIC names to improve performance.
#' Default: Top News written in English
#'
#' @param count int, optional. The maximum number of headlines to retrieve.
#' Value Range: [1-100].
#' Default: 10
#'
#' @param date_from string. Beginning of date range.
#' String format is:'\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#'
#' @param date_to string. End of date range.
#' String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. "2016-01-20T15:04:05".
#'
#' @param raw_output boolean. Set this parameter to True to get the data in json format
#' if set to False, the function will return a data frame
#' The default value is False
#'
#' @param debug boolean. If this parameter is set to True, the json request and response are printed.
#'
#'
#' @examples
#' \dontrun{
#' set_app_id('set your app id here')
#' headlines = get_news_headlines("R:MSFT.O", 2L)
#' headlines = get_news_headlines("R:MSFT.O IN FRANCE")
#' headlines = get_news_headlines("R:MSFT.O IN FRANCE IN ENGLISH", count=5L)
#' headlines = get_news_headlines("OBA* OR CLINTON IN ENGLISH", count=5L)
#' }
#' @export
get_news_headlines <- function (query='Topic:TOPALL and Language:LEN', count=10L, date_from=NULL,
                       date_to=NULL, raw_output=FALSE, debug=FALSE)
{


  News_Headlines_endpoint = "News_Headlines"

  if (!is.character(query))
  {
    print('get_news_headlines error: query must be a string');
    return (NULL);
  }

  if (!is.integer(count))
  {
    print('get_news_headlines error: count must be an integer');
    return (NULL);
  }
  else if (count < 0)
  {
    print('get_news_headlines error: count must be equal or greater than 0');
    return (NULL);
  }


  # build the payload
   payload = list('number'= toString(count), 'query'= query, 'productName'=get_app_id(), 'attributionCode'= '')

  if (!is.null(date_from))
  {
    payload['dateFrom'] = date_from
  }

  if (!is.null(date_to))
  {
    payload['dateTo'] = date_to
  }


  json_data = send_json_request(News_Headlines_endpoint, payload, debug)

  if (raw_output)
  {
    return(json_data)
  }
  result = jsonlite::fromJSON(json_data)
  headlines <- result$headlines
  data_frame = data.frame (headlines$firstCreated,headlines$versionCreated,headlines$text,headlines$storyId,headlines$sourceCode)
  names(data_frame) <- c('firstCreated','versionCreated','text','storyId','sourceCode')
  return(data_frame)
}


#' get_news_story
#'
#' Return a single news story corresponding to the identifier provided in story_id
#'
#' @param story_id  The story id. The story id is a field you will find in every headline retrieved with the function get_news_headlines
#'
#' @param raw_output boolean. Set this parameter to True to get the data in json format
#' The default value is False
#
#' @param debug bool. When set to True, the json request and response are printed.
#'
#' @examples
#' \dontrun{
#' set_app_id('set your app id here')
#' headlines = get_news_headlines('IBM')
#' for (story_id in headlines$storyId) {
#'   story = get_news_story(story_id)
#'   print (story)
#' }
#' }
#' @export
get_news_story <- function(story_id,raw_output=FALSE, debug=FALSE)
{

  News_Story_endpoint = "News_Story"
  payload <- list('attributionCode'='', 'productName'= get_app_id() , 'storyId' =  story_id)
  json_data = send_json_request(News_Story_endpoint, payload, debug)

  if (raw_output)
  {
    return(json_data)
  }

  result = jsonlite::fromJSON(json_data)
  return (result$story$storyHtml)
}
