# is_required_field ------------------------------------------------------------
is_required_field <- function(field_config)
{
  is_required <- function(x) kwb.utils::defaultIfNULL(x[["required"]], TRUE)
  sapply(field_config, is_required)
}
