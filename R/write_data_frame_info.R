# write_data_frame_info --------------------------------------------------------
write_data_frame_info <- function(
  x, level = 3L, name = deparse(substitute(x)),
  wide = get_option("table_structure_wide"),
  top_n = get_option("table_structure_top_n")
)
{
  write_markdown_chapter(
    c(
      get_text("table_dimesion", nrow(x), ncol(x)),
      kable_data_frame_structure(x, wide = wide, top_n = top_n)
    ),
    caption = get_text("structure_of", name),
    level = level
  )
  
  invisible(x)
}
