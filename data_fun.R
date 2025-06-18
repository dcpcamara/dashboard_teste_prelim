
# Loading data --------------------------------------------------------------------------------

# dengue.df <- vroom::vroom("data/cases.csv.gz")
# spatial.tbl <- vroom::vroom("data/spatial.tbl.csv")

# dengue.df <- dengue.df |>
#   left_join(spatial.tbl |>
#               select(geocode, uf, macroregional, macroregional_geocode),
#             by = c("municipio_geocodigo"="geocode") )

# dados.macro <- dengue.df |>
#   prepare.data(suspected_cases = F)

# observed <- dados.macro |>
#   rename(season = year) |>
#   mutate(year.s.first = as.numeric(str_sub(season, 1, 4))) |>
#   filter(season >= 2022)

# rio::export(observed, "data/observed.csv.gz")

observed <- vroom::vroom("data/observed.csv.gz")

# Wrangling Brazil data -----------------------------------------------------------------------

# df4BRplot <- function(obj, ano) {
# 
#   observed.tmp <- observed |>
#     filter(year.s.first == ano) |>
#     group_by(week, season, year.s.first, date) |>
#     summarise(cases = sum(cases, na.rm = TRUE))
# 
#   temp <- obj |>
#     group_by(week, samples) |>
#     summarise(values = sum(values)) |>
#     ungroup() |>
#     group_by(week) |>
#     summarise(q50 = quantile(values, probs = 0.5),
#               q75 = quantile(values, probs = 0.75),
#               q90 = quantile(values, probs = 0.9),
#               q100 = Inf)
# 
#   tmp1 <- temp |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'maxvalues'
#     )
# 
#   tmp2 <- temp |>
#     mutate(q100 = q90,
#            q90 = q75,
#            q75 = q50,
#            q50 = 0) |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'minvalues'
#     )
# 
#   tmp <- tmp1 |>
#     left_join(tmp2, by = c('week', 'quantile')) |>
#     ungroup() |>
#     left_join(observed.tmp, by = 'week') |>
#     mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12),
#            epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
#            date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
# 
#   tmp$quantile <- factor(tmp$quantile,
#                          levels = c('q50','q75','q90','q100'),
#                          labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
#                                     'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
#   return(tmp)
# }

# t1br <- df4BRplot(df.prob.22_23, ano = 2022)
# t2br <- df4BRplot(df.prob.23_24, ano = 2023)
# t3br <- df4BRplot(df.prob.24_25, ano = 2024)
# 
# rio::export(t1br, "data/t1br.csv.gz")
# rio::export(t2br, "data/t2br.csv.gz")
# rio::export(t3br, "data/t3br.csv.gz")
# 
# t1br <- vroom::vroom("data/t1br.csv.gz")
# t2br <- vroom::vroom("data/t2br.csv.gz")
# t3br <- vroom::vroom("data/t3br.csv.gz")

# Wrangling UF data ---------------------------------------------------------------------------

# df4UFplot <- function(obj, ano) {
# 
#   observed.tmp <- observed |>
#     filter(year.s.first == ano) |>
#     group_by(week, season, year.s.first, uf) |>
#     summarise(cases = sum(cases, na.rm = TRUE))
# 
#   temp <- obj |>
#     group_by(week, samples, uf) |>
#     summarise(values = sum(values)) |>
#     ungroup() |>
#     group_by(week, uf) |>
#     summarise(q50 = quantile(values, probs = 0.5),
#               q75 = quantile(values, probs = 0.75),
#               q90 = quantile(values, probs = 0.9),
#               q100 = Inf)
# 
#   tmp1 <- temp |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'maxvalues'
#     )
# 
#   tmp2 <- temp |>
#     mutate(q100 = q90,
#            q90 = q75,
#            q75 = q50,
#            q50 = 0) |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'minvalues'
#     )
# 
#   tmp <- tmp1 |>
#     left_join(tmp2, by = c('week', 'uf', 'quantile')) |>
#     ungroup() |>
#     left_join(observed.tmp, by = c('week', 'uf')) |>
#     mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12),
#            epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
#            date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
# 
#   tmp$quantile <- factor(tmp$quantile,
#                          levels = c('q50','q75','q90','q100'),
#                          labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
#                                     'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
#   return(tmp)
# }

# t1uf <- df4UFplot(df.prob.22_23, ano = 2022)
# t2uf <- df4UFplot(df.prob.23_24, ano = 2023)
# t3uf <- df4UFplot(df.prob.24_25, ano = 2024)
# 
# rio::export(t1uf, "data/t1uf.csv.gz")
# rio::export(t2uf, "data/t2uf.csv.gz")
# rio::export(t3uf, "data/t3uf.csv.gz")
# 
# t1uf <- vroom::vroom("data/t1uf.csv.gz")
# t2uf <- vroom::vroom("data/t2uf.csv.gz")
# t3uf <- vroom::vroom("data/t3uf.csv.gz")


# Wrangling Health District data --------------------------------------------------------------

# df4HDplot <- function(obj, ano) {
# 
#   observed.tmp <- observed |>
#     filter(year.s.first == ano) |>
#     group_by(week, season, year.s.first, uf, macroregional, macroregional_geocode) |>
#     summarise(cases = sum(cases, na.rm = TRUE))
# 
#   temp <- obj |>
#     group_by(week, samples, uf, macrocode) |>
#     summarise(values = sum(values)) |>
#     ungroup() |>
#     group_by(week, uf, macrocode) |>
#     summarise(q50 = quantile(values, probs = 0.5),
#               q75 = quantile(values, probs = 0.75),
#               q90 = quantile(values, probs = 0.9),
#               q100 = Inf)
# 
#   tmp1 <- temp |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'maxvalues'
#     )
# 
#   tmp2 <- temp |>
#     mutate(q100 = q90,
#            q90 = q75,
#            q75 = q50,
#            q50 = 0) |>
#     pivot_longer(
#       cols = c(q50, q75, q90, q100),
#       names_to = 'quantile',
#       values_to = 'minvalues'
#     )
# 
#   tmp <- tmp1 |>
#     left_join(tmp2, by = c('week', 'uf', 'quantile', 'macrocode')) |>
#     ungroup() |>
#     left_join(observed.tmp, by = c('week', 'uf', 'macrocode' = 'macroregional_geocode')) |>
#     mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12),
#            epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
#            date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
# 
#   tmp$quantile <- factor(tmp$quantile,
#                          levels = c('q50','q75','q90','q100'),
#                          labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
#                                     'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
#   return(tmp)
# }

# t1hd <- df4HDplot(df.prob.22_23, ano = 2022)
# t2hd <- df4HDplot(df.prob.23_24, ano = 2023)
# t3hd <- df4HDplot(df.prob.24_25, ano = 2024)
# 
# rio::export(t1hd, "data/t1hd.csv.gz")
# rio::export(t2hd, "data/t2hd.csv.gz")
# rio::export(t3hd, "data/t3hd.csv.gz")
# 
# t1hd <- vroom::vroom("data/t1hd.csv.gz")
# t2hd <- vroom::vroom("data/t2hd.csv.gz")
# t3hd <- vroom::vroom("data/t3hd.csv.gz")


# Maps ----------------------------------------------------------------------------------------

# spatial.tbl <- import("data/spatial.tbl.csv")
# 
# shape <- read_sf('data/shapefile_macro.json') |>
#   left_join(spatial.tbl |>
#               select(uf, macroregional, macroregional_geocode) |>
#               unique() |>
#               rename(uf_name = uf), by = c('code_macro' = 'macroregional_geocode'))
# 
# ufshape <- read_state(
#   year = 2020,
#   showProgress = FALSE,
#   simplified = TRUE
# )
# 
# ufshape <- ufshape |>
#   st_crop(xmin = -73.9904, ymin = -33.7511, xmax = -33.2476, ymax = 5.2718)
# 
# ufshape <- st_transform(ufshape, st_crs(shape))
# 
# regions_en <- tibble(
#   name_region = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro Oeste'),
#   region_en = c("North", "Northeast", "Southeast", "South", "Mid-West")
# )
# 
# ufshape <- ufshape |>
#   left_join(regions_en, by = "name_region") |>
#   mutate(name_state = if_else(name_state == 'Amazônas', true = 'Amazonas', false = name_state))
# 
# shape <- shape |>
#   left_join(ufshape |>
#               st_drop_geometry() |>
#               select(code_state, region_en), by = c('uf' = 'code_state'))
# 
# # Paleta de cores semelhante à do seu mapa
# region_colors <- c(
#   "North" = "#3CAACF",
#   "Northeast" = "#D4EC88",
#   "Mid-West" = "#66C07E",
#   "Southeast" = "#A383D9",
#   "South" = "#DC8E4B"
# )
# 
# pal <- colorFactor(palette = region_colors, domain = shape$region_en)
# 
# uf_labels <- ufshape |>
#   mutate(centroid = st_centroid(geom)) |>
#   st_drop_geometry() |>
#   bind_cols(st_coordinates(st_centroid(ufshape))) |>
#   select(name_state, abbrev_state, X, Y)
# 
# st_write(obj = shape, dsn = "data/gpkg_macro.gpkg", layer = "macro_shapes", driver = "GPKG", delete_dsn = TRUE)
# st_write(obj = ufshape, dsn = "data/gpkg_uf.gpkg", layer = "uf_shapes", driver = "GPKG", delete_dsn = TRUE)
# 
# shape <- read_sf("data/gpkg_macro.gpkg")
# ufshape <- read_sf("data/gpkg_uf.gpkg")
# 
# shape_simples <- ms_simplify(shape, keep = 0.05, keep_shapes = TRUE)
# st_write(obj = shape_simples, dsn = "data/shape_macro_simp.gpkg", layer = "macro_shapes", driver = "GPKG", delete_dsn = TRUE)
# 
# ufshape_simples <- ms_simplify(ufshape, keep = 0.05, keep_shapes = TRUE)
# st_write(obj = ufshape_simples, dsn = "data/shape_uf_simp.gpkg", layer = "macro_shapes", driver = "GPKG", delete_dsn = TRUE)

write_rds(shape, "data/shape_macro_simp.rds")
write_rds(ufshape, "data/shape_uf_simp.rds")

# shape <- read_sf("data/shape_macro_simp.gpkg")
# ufshape <- read_sf("data/shape_uf_simp.gpkg")
