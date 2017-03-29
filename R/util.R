is_string <- function (value)
{
  if(is.character(value) && length(value) == 1)
  {
    return(TRUE)
  }
  return(FALSE)
}

is_named_list <- function (value)
{
  if(is.list(value) && !is.null(names(value)))
  {
    return(TRUE)
  }

  return(FALSE)
}



is_non_named_list <- function (value)
{
  if(is.list(value) && is.null(names(value)))
  {
    return(TRUE)
  }

  return(FALSE)
}
