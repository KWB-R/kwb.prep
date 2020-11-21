if (FALSE)
{
  store <- kwb.utils::createStorage("~/tmp/store")
  a=store$load("gis_001");b=store$load("gis_005")

  kwb.prep:::compare_data_frames(a, b)
  kwb.prep:::compare_data_frames(a, b, level = 1L)
  kwb.prep:::compare_data_frames(a, b, level = 2L)
}
