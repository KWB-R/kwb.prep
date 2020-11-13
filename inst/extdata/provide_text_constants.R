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
  applying_filter = "Filterschritte '%s' anwenden",
  applying_filter_to = "Filterschritte '%s' anwenden auf '%s'",
  by = "verkn<ue>pft",
  calculating_new_columns = "Berechnen neuer Spalten",
  calculating_new_columns_in = "Berechnen neuer Spalten in '%s'",
  columns_removed = paste0(
    "Die folgenden %d Spalten wurden aus '%s' entfernt:<nl><nl>%s<nl>"
  ),
  columns_removed_reason = paste0(
    "Die folgenden %d Spalten wurden aus '%s' entfernt (%s):<nl><nl>%s<nl>"
  ),
  column_x = "Spalte links",
  column_y = "Spalte rechts",
  common_columns = paste0(
    "The following columns are in '%s' as well as in '%s':<nl>- %s"
  ),
  creating_missing_column = "Creating missing column '%s'",
  deleting_constant_columns = "Deleting 'constant' columns",
  hello = "sch<oe>ne Gr<ue>sse",
  key_columns = "Schl<ue>sselspalte(n)",
  left_joining = "Tabellen verbinden (Left Join)",
  left_table = "Linke Tabelle",
  merging = "Verbinden zweier Tabellen: '%s' x '%s'",
  merging_failed = "Merging %s with %s led to %d more rows!",
  n_rows = "Anzahl Zeilen",
  name = "Name",
  new_line = "<nl>",
  nrow_x = "Zeilen links",
  nrow_y = "Zeilen rechts",
  no_columns_removed = "Es wurden keine Spalten entfernt.",
  no_duplicates = "Keine Duplikate in (Wertekombinationen der) Spalte(n): %s",
  reading_args = "Argumente f<ue>r Funktion %s() lesen aus: '%s'",
  reading_filter_criteria = "Reading filter criteria from",
  removing_columns = "Spalten entfernen",
  removing_columns_from = "Spalten aus '%s' entfernen",
  renaming_columns = "Spalten in '%s' umbenennen:<nl>",
  replacements_invalid_csv = paste0(
    "Ersetzungen gem<ae><ss> replace_invalid.csv, Gruppe '%s'"
  ),
  result_table = "Ergebnistabelle" ,
  right_table = "Rechte Tabelle", 
  select_columns = "Spalten ausw<ae>hlen",
  select_columns_from = "Spalten ausw<ae>hlen aus '%s'",
  select_rename_columns = "Spalten ausw<ae>hlen (und umbenennen)",
  select_rename_columns_from = paste0(
    "Spalten ausw<ae>hlen (und umbenennen) aus '%s'"
  ),
  structure_of = "Struktur von '%s'",
  table_dimesion = "Tabelle mit %d Zeilen und %d Spalten<nl>",
  type_x = "Typ links",
  type_y = "Typ rechts",
  unique_rows = "Duplikatzeilen entfernen",
  writing_csv = "Schreiben von `%s`"
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

