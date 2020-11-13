#
# Provide text constants in a yaml file
#

# Define paths -----------------------------------------------------------------
paths <- kwb.utils::resolve(list(
  config = kwb.utils::createDirectory("./inst/extdata/config"),
  yml = "<config>/text_constants.yml"
))

# Define text constants --------------------------------------------------------
string_definition <- list(
  hello = "sch<oe>ne Gr<ue>sse",
  key_columns = "Schl<ue>sselspalte(n)",
  left_joining = "Tabellen verbinden (Left Join)",
  left_table = "Linke Tabelle",
  n_rows = "Anzahl Zeilen",
  name = "Name",
  new_line = "<nl>",
  nrow_x = "Zeilen links",
  nrow_y = "Zeilen rechts",
  result_table = "Ergebnistabelle" ,
  right_table = "Rechte Tabelle", 
  select_columns = "Spalten ausw<ae>hlen",
  select_columns_from = "Spalten ausw<ae>hlen aus '%s'",
  unique_rows = "Duplikatzeilen entfernen"
)

# Write yaml file and dictionary file ------------------------------------------
if (TRUE)
{
  # Write yaml file
  yaml::write_yaml(string_definition, paths$yml)

  file.copy(paths$yml, "~/tmp/kwb.prep/config", overwrite = TRUE)
}

# Check if the original list of strings can be reproduced ----------------------
if (FALSE)
{
  strings <- kwb.prep:::read_string_definition()
  
  stopifnot(identical(strings, string_definition))
}

