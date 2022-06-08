# [kwb.prep 0.3.0](https://github.com/KWB-R/kwb.prep/releases/tag/v0.3.0) <small>2022-06-09</small>

* Add argument "to.numeric" to `doRegroupings()`, `applyRegrouping()`,
  `regroupedValues()`, `regroup()`, `typeConverted()`
* Let `replaceByCondition()` ignore lines starting with "#" in config file
* Document and export `fieldSummary()`

# [kwb.prep 0.2.0](https://github.com/KWB-R/kwb.prep/releases/tag/v0.2.0) <small>2022-01-24</small> 

Add private functions:

* `get_user_config_dir()`
* `set_user_config_dir()`
* `round_numeric()`

`writeStandardCsv()`: 

* Use `kwb.utils::argsCsv()`
* TODO: use consistent sep/dec combination

format_overview_table():

* round numeric (and non-integer) columns to two digits

write_data_frames():

* add argument "lng" to set sep/dec

# [kwb.prep 0.1.0](https://github.com/KWB-R/kwb.prep/releases/tag/v0.1.0) <small>2022-01-21</small> 

First release

# kwb.prep 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`
