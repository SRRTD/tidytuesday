temp_dir <- tempdir()

temp <- tempfile(tmpdir = temp_dir)

download.file("https://www.medellin.gov.co/apigeomedellin/atributos/archivos/openDataExt/Gis/open_data/OD398/geojson_arbol_urbano.zip", temp)

unzip(temp, files = "arbol_urbano.geojson", exdir = temp_dir)

arbol_urbano <- sf::read_sf(file.path(temp_dir, "arbol_urbano.geojson"))

unlink(temp)

