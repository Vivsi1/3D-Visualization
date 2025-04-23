
libs <- c(
    "giscoR", "terra", "elevatr",
    "png", "rayshader", "magick", "rgl"
)


country_sf <- giscoR::gisco_get_countries(
    country = "SI",
    resolution = "1"
)

options(timeout = 600)
url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E000N60/E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

forest_type <- terra::rast(
    basename(url)
)

vals <- terra::values(
    forest_type,
    dataframe = T
)

names(vals)
names(vals)[1] <- "value"
unique(vals$value)


crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_forest_type <- terra::crop(
    forest_type,
    terra::vect(country_sf),
    snap = "in",
    mask = T
) |>
terra::project(crs_lambert)

terra::plot(country_forest_type)


cols <- c(
    "#A9B0B3", 
    "#2A5D3F",
    "#A0B94C", 
    "#6F8F5F"
)

from <- c(0:1, 4:5)
to <- t(col2rgb(
    cols
))

forest_terra <- na.omit(
    country_forest_type
)

forest_type_image <- terra::subst(
    forest_terra,
    from,
    to,
    names = cols
)

terra::plotRGB(forest_type_image)

img_file <- "slovenia-forest-image.png"
terra::writeRaster(
    forest_type_image,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

elev <- elevatr::get_elev_raster(
    locations = country_sf, 
    z = 8, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

bio1_tile_path <- "wc2.1_2.5m_bio_1.tif"  

if (!file.exists(bio1_tile_path)) {
  stop("BIO1 tile not found. Please download and place it in your working directory.")
}

bio1_rast <- terra::rast(bio1_tile_path)

bio1_vals <- terra::values(bio1_rast, na.rm = TRUE)
temp_min <- floor(min(bio1_vals))
temp_max <- ceiling(max(bio1_vals))
bio1_vals <- bio1_norm <- (bio1_crop - temp_min) / (temp_max - temp_min) # convert from tenths of °C to °C


n_colors <- 256
temp_colors <- colorRampPalette(c("#313695", "#74add1", "#fdae61", "#d73027"))(n_colors)

png("temperature_legend.png", width = 800, height = 100)
par(mar = c(2, 2, 2, 2), family = "mono")


plot(
  c(0, 1), c(0, 1),
  type = "n", axes = FALSE, xlab = "", ylab = "",
  xaxs = "i", yaxs = "i", bty = "n"
)
rasterImage(
  as.raster(matrix(temp_colors, ncol = n_colors)),
  xleft = 0.1, ybottom = 0.4,
  xright = 0.9, ytop = 0.6
)

label_vals <- round(seq(temp_min, temp_max, length.out = 5))
label_pos <- seq(0.1, 0.9, length.out = 5)

text(label_pos, rep(0.25, 5), paste0(label_vals, "°C"), cex = 1.2)
text(0.5, 0.8, "Annual Mean Temperature", cex = 1.5, font = 2)

dev.off()

bio1_proj <- terra::project(bio1_rast, crs_lambert)
country_sf_proj <- terra::project(terra::vect(country_sf), crs_lambert)

bio1_crop <- terra::crop(bio1_proj, country_sf_proj) |>
  terra::mask(country_sf_proj)

bio1_vals <- terra::values(bio1_crop, na.rm = TRUE)
temp_min <- min(bio1_vals)
temp_max <- max(bio1_vals)

bio1_norm <- (bio1_crop - temp_min) / (temp_max - temp_min)

temp_colors <- colorRampPalette(c("#313695", "#74add1", "#fdae61", "#d73027"))(256)

temp_png_file <- "temp_overlay.png"
png(temp_png_file, width = 1225, height = 842, bg = "white", res = 300)
par(mar = c(0, 0, 0, 0))
terra::plot(bio1_norm, col = temp_colors, axes = FALSE, legend = FALSE, main = "")
dev.off()

temp_overlay_img <- image_read(temp_png_file)

temp_overlay_scaled <- image_scale(temp_overlay_img, "1640x1125") 

temp_overlay_adjusted <- image_crop(temp_overlay_scaled, "1225x842+0+0", gravity = "center")

image_write(temp_overlay_adjusted, "adjusted_temp_overlay.png")

elmat_forest <- elmat


h <- 842
w <- 1225


rgl::clear3d()

elmat_forest |>
  rayshader::height_shade(texture = colorRampPalette("white")(512)) |>
  rayshader::add_overlay(img, alphalayer = 0.9) |>
  rayshader::add_shadow(rayshader::lamb_shade(elmat_forest, zscale = 50, sunaltitude = 90, sunangle = 315), max_darken = 0.25) |>
  rayshader::add_shadow(rayshader::texture_shade(elmat_forest, detail = 0.95, brightness = 90, contrast = 80), max_darken = 0.1) |>
  rayshader::plot_3d(
    elmat_forest,
    zscale = 5,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(w / 5, h / 5),
    zoom = .5,
    phi = 85,
    theta = 0
  )


rayshader::render_highquality(
  filename = "forest-type-3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = "air_museum_playground_4k.hdr",
  intensity_env = 2,
  rotate_env = 90,
  interactive = FALSE,
  parallel = TRUE,
  width = w, height = h
)


temp_overlay <- png::readPNG("adjusted_temp_overlay.png")

rgl::clear3d()

elmat_temp <- elmat

elmat_temp |>
  rayshader::height_shade(texture = colorRampPalette("white")(512)) |>
  rayshader::add_overlay(temp_overlay, alphalayer = 0.9) |>
  rayshader::add_shadow(rayshader::lamb_shade(elmat_temp, zscale = 50, sunaltitude = 90, sunangle = 315), max_darken = 0.25) |>
  rayshader::add_shadow(rayshader::texture_shade(elmat_temp, detail = 0.95, brightness = 90, contrast = 80), max_darken = 0.1) |>
  rayshader::plot_3d(
    elmat_temp,
    zscale = 5,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(w / 5, h / 5),
    zoom = .5,
    phi = 85,
    theta = 0
  )

# 7B. RENDER TEMPERATURE IMAGE
#-----------------------------

rayshader::render_highquality(
  filename = "temperature-3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = "C:\\Users\\vivaa\\Downloads\\air_museum_playground_4k.hdr",
  intensity_env = 2,
  rotate_env = 90,
  interactive = FALSE,
  parallel = TRUE,
  width = w, height = h
)



png("my_legend.png")
par(family = "mono")
plot(
    NULL, xaxt = "n",
    yaxt = "n", bty = "n",
    ylab = "", xlab = "",
    xlim = 0:1, ylim = 0:1,
    xaxs = "i", yaxs = "i"
)
legend(
    "center",
    legend = c(
        "Unknown",
        "Evergreen needle leaf",
        "Deciduous broad leaf",
        "Mixed"
    ),
    pch = 16,
    pt.cex = 3,
    cex = 1.5,
    bty = "n",
    col = cols
)
dev.off()

forest_img <- magick::image_read(
    "forest-type-3d.png"
)

my_legend <- magick::image_read(
    "my_legend.png"
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
    my_legend, "none"), 2000
) |>
magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        forest_img,
        "x4000"
    ),
    my_legend_scaled,
    offset = "+100+0"
)

magick::image_write(
    p,
    "final-map.png"
)
