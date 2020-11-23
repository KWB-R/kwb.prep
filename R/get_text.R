# create_text_getter -----------------------------------------------------------

#' Create a get_text() Function
#' 
#' @param raw_strings list of string definitions (key = value) pairs
#' @return a function that can be used to lookup the string constant(s)
#' @export
#' @examples
#' get_text <- create_text_getter(
#'   list(hello_en = "good morning", hello_de = "sch<oe>ne Gr<ue><ss>e")
#' )
#' 
#' get_text("hello_en")
#' get_text("hello_de")
#' #get_text("no_such_key") # error with clear error message
#' 
create_text_getter <- function(raw_strings)
{
  user_strings <- check_user_strings(raw_strings)
  
  raw_strings <- get_raw_strings(user_strings)
    
  function(key = NULL, ...) {
    get_text(key, ..., raw_strings = raw_strings)
  }
}

# get_text ---------------------------------------------------------------------

#' Get Text Constant
#' 
#' @param key identifier
#' @param \dots additional arguments passed to \code{\link{sprintf}}
#' @param raw_strings list with raw string definitions as key = value pairs
#' @return if \code{key} is \code{NULL}) a list with all text constants or the
#'   text constant looked up for the given key
#' @export
#' @importFrom kwb.utils repeated resolve selectElements
#' 
get_text <- function(key = NULL, ..., raw_strings = get_raw_strings())
{
  strings <- kwb.utils::resolve(
    raw_strings,
    nl = "\n",
    tab = kwb.utils::repeated(" ", 3L),
    ae = "\u00E4",
    oe = "\u00F6",
    ue = "\u00FC",
    ss = "\u00DF",
    Ae = "\u00C4",
    Oe = "\u00D6",
    Ue = "\u00DC"
  )
  
  if (is.null(key)) {
    return(strings)
  }
  
  string <- kwb.utils::selectElements(strings, key)
  
  if (! length(list(...))) {
    return(string)
  }
  
  sprintf(string, ...)
}

# get_raw_strings --------------------------------------------------------------
get_raw_strings <- function(user_strings = get_user_strings())
{
  c(read_string_definition(), user_strings)
}

# read_string_definition -------------------------------------------------------
read_string_definition <- function(file = NULL)
{
  yaml::read_yaml(file = kwb.utils::defaultIfNULL(file, config_file(
    "text_constants.yml", in_package = TRUE
  )))
}
