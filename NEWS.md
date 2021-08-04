
# home2park (development version)


## Bug fixes 
* Helper function `raster_class_area()`. The use of `terra::extract()` may output an additional third `area` column that is unused. Only keep first 2 columns of output.

* Fix error in `rasterise_buildings()`. Getting the files for land use rasters resulted in an error (e.g. multiple files). Syntax for re-naming for the output using the `glue::glue()` function also did not work within package environment.

* Add removal of invalid building polygons in `get_buildings_osm()`, which remained in spite of `st_make_valid()`. Related to https://github.com/r-spatial/sf/issues/1649

<br>

# home2park 0.1.0

* Finish development of main functions
* Prepare for journal submission
