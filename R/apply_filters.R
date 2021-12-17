#' Apply Groups of Filter Criteria from Configuration
#' 
#' @param data data frame
#' @param groups names of filter criteria groups defined in list returned by
#'   \code{kwb.prep:::read_filter_criteria}
#' @param length_column name of column in \code{data} containing lengths (to be
#'   summed up for the overview that is returned)
#' @param id_columns names of column(s) in \code{data} that uniquely identify
#'   the records. This column / these columns are returned in order to report
#'   about the records that have been removed
#' @return data, filtered according to the specified criteria. The returned data
#'   frame has an attribute \code{filter_info} being a list with as many
#'   elements as there are \code{groups}. The elements are named according to
#'   the values given in \code{groups}. Each list element is a list with one
#'   element \code{overview} (being a data frame with one row per filter
#'   criterion) and further elements \code{removed_<i>} being data frames with
#'   only \code{id_columns} that represent the records that have been removed in
#'   the according filter step \code{i}.
#' @export
#' @importFrom kwb.utils getAttribute removeAttributes
#' @importFrom stats setNames
#' @examples 
#' 
#' # Define filter criteria
#' criteria <- list(
#'   sepal = c(
#'     "sepal short" = "Sepal.Length < 5",
#'     "sepal narrow" = "Sepal.Width < 3"
#'   ),
#'   petal = c(
#'     "petal short" = "Petal.Length < 5",
#'     "petal narrow" = "Petal.Width < 3"
#'   )
#' )
#' 
#' # Write criteria to temporary yaml file
#' tdir <- tempdir()
#' yaml::write_yaml(criteria, file.path(tdir, "filter_criteria.yml"))
#' 
#' # Set path to temporary "config" folder so that kwb.prep knows about it
#' kwb.prep:::set_global("user_config_dir", tdir)
#' 
#' # Apply filter groups "sepal" and "petal" to the iris dataset
#' result <- apply_filters(iris, c("sepal", "petal"))
#' 
#' # Have a look at the result
#' str(result)
#' 
apply_filters <- function(
  data, groups, length_column = NULL, id_columns = names(data)[1L]
)
{
  filter_info <- list()
  
  for (group in groups) {
    
    # group <- "g1"
    
    filtered <- apply_filter(
      x = data, 
      element = group, 
      length_column = length_column, 
      name = group
    )
    
    overview <- kwb.utils::getAttribute(filtered, "details.filter")
    matches <- kwb.utils::getAttribute(filtered, "matches/details")
    
    ids_removed <- apply(matches, 2L, function(kept) {
      kwb.utils::resetRowNames(data[! kept, id_columns, drop = FALSE])
    })
    
    filter_info[[group]] <- c(
      list(overview = overview),
      stats::setNames(ids_removed, paste0("removed_", seq_along(ids_removed)))
    )
    
    data <- filtered
  }
  
  structure(
    kwb.utils::removeAttributes(filtered, c("details.filter", "matches")),
    filter_info = stats::setNames(filter_info, groups)
  )
}
