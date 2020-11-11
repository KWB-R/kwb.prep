globals <- new.env()

# get_global -------------------------------------------------------------------
get_global <- function(x, default = NULL)
{
  if (! x %in% ls(envir = globals)) {
    return(default)
  }
  
  get(x, envir = globals)
}

# set_global -------------------------------------------------------------------
set_global <- function(x, value)
{
  assign(x, value, envir = globals)
}
