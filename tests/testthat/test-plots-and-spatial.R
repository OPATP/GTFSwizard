test_that("spatial conversions round-trip standard columns", {
  feed <- minimal_feed()
  shapes <- get_shapes_sf(feed$shapes)
  expect_s3_class(shapes, "sf")
  expect_equal(sf::st_crs(shapes)$epsg, 4326)
  shape_table <- get_shapes_df(shapes)
  expect_true(all(diff(shape_table$shape_dist_traveled) >= 0))

  stops <- get_stops_sf(feed$stops)
  expect_s3_class(stops, "sf")
  expect_equal(sf::st_crs(latlon2epsg(stops))$IsGeographic, FALSE)
})

test_that("all-missing shape distances remain missing without warnings", {
  feed <- minimal_feed()
  feed$shapes$shape_dist_traveled <- NA_real_
  expect_no_warning(shapes <- get_shapes_sf(feed$shapes))
  expect_true(all(is.na(shapes$shape_dist_traveled)))
  expect_false(any(is.infinite(shapes$shape_dist_traveled)))
})

test_that("all public static plots return ggplot objects", {
  feed <- minimal_feed()
  expect_s3_class(plot(feed), "ggplot")
  expect_s3_class(plot_frequency(feed), "ggplot")
  expect_s3_class(plot_routefrequency(feed), "ggplot")
  expect_s3_class(plot_headways(feed), "ggplot")
  expect_s3_class(plot_calendar(feed), "ggplot")
  expect_s3_class(plot_servicespan(feed), "ggplot")
  expect_s3_class(plot_serviceheatmap(feed), "ggplot")
  expect_s3_class(plot_routeduration(feed), "ggplot")
  expect_s3_class(plot_servicesupply(feed), "ggplot")
  expect_s3_class(plot_hubs(feed, i = 1), "ggplot")
  expect_s3_class(plot_corridor(feed, i = 1, min.length = 0), "ggplot")
})

test_that("hub counts are computed before identifier list-columns", {
  hubs <- get_hubs(minimal_feed())
  expect_equal(hubs$n_trip, rep(2L, 3))
  expect_equal(hubs$n_routes, rep(1L, 3))
  expect_true(all(lengths(hubs$trip_id) == 2L))
  expect_true(all(lengths(hubs$route_id) == 1L))
})

test_that("map plots use readable layer order, labels, and legends", {
  feed <- minimal_feed()
  system_plot <- plot(feed)
  expect_true(all(sf::st_geometry_type(system_plot$layers[[1]]$data) == "POINT"))
  expect_true(all(sf::st_geometry_type(system_plot$layers[[2]]$data) == "LINESTRING"))
  expect_equal(system_plot$layers[[1]]$aes_params$size, 0.9)

  route_plot <- plot_routefrequency(feed)
  expect_equal(route_plot$theme$legend.position, "none")
  selected_route_plot <- plot_routefrequency(feed, route = "R")
  expect_false(identical(selected_route_plot$theme$legend.position, "none"))

  corridors <- get_corridor(feed, i = 1, min.length = 0)
  expect_equal(corridors$corridor, "Corridor 1")

  hubs <- plot_hubs(feed, i = 1)
  expect_lte(nrow(hubs$layers[[2]]$data), 40)
})

test_that("planning plots identify their observational units", {
  feed <- minimal_feed()
  expect_match(plot_servicespan(feed)$labels$subtitle, "Each line")
  expect_match(plot_serviceheatmap(feed)$labels$subtitle, "Each tile")
  expect_match(plot_routeduration(feed)$labels$subtitle, "Each observation")
  expect_match(plot_servicesupply(feed)$labels$subtitle, "Each bar")
})

test_that("explorer requires an explicit feed outside interactive sessions", {
  skip_if(interactive())
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  expect_error(explore_gtfs(), "required in non-interactive sessions")
})

test_that("summary returns a summary object", {
  result <- summary(minimal_feed())
  expect_s3_class(result, "summary.wizardgtfs")
  expect_equal(result$routes, 1)
  expect_snapshot(print(result))
})
