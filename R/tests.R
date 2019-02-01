#test symbology
test_symbology <- function()
{
  result <- get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"),"RIC",list("ISIN"),raw_ouput = FALSE,debug=FALSE)
  print(result)

}


#test time series
test_timeSeries <- function(normalize)
{
    result <- get_timeseries(list("MSFT.O","VOD.L","IBM.N"),list("*"),"2016-01-01T15:04:05","2016-01-10T15:04:05","daily",normalize)
 # result <- get_timeseries(list("MSFT.O"),"2016-01-01T15:04:05","2016-01-29T15:04:05","daily",list("*"))

 return (result)

}






#test news headlines
test_news_headlines <- function()
{
  #result <- get_news_headlines("OBA* OR CLINTON IN ENGLISH", count=5L)
  result <- get_news_headlines("IBM", count=5L)
  print (result[,c('firstCreated','storyId','sourceCode')])
  return (result)
}



test_news_story <- function()
{

   headlines <- get_news_headlines("IBM", count=5L)
   for (story_id in headlines$storyId)
   { story = get_news_story(story_id)
     print (story)
   }

}

test_data_grid <- function()
{
  get_data (list("IBM"), 'TR.Employees')
}
