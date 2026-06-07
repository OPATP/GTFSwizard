test_that("creation validates and preserves GTFS service-day times", {
  feed <- minimal_feed()
  expect_s3_class(feed, "wizardgtfs")
  expect_true(any(feed$stop_times$arrival_time == "24:50:00"))
  expect_equal(nrow(feed$dates_services), 7)
  expect_equal(
    gtfs_time_to_seconds(c("0:0:0", "24:50:00", "120:05:09", "bad")),
    c(0, 89400, 432309, NA_real_)
  )
})

test_that("stop and time filters retain partial trips and valid references", {
  feed <- minimal_feed()
  feed$transfers <- data.frame(
    from_stop_id = "S1", to_stop_id = "S3", transfer_type = 0
  )
  filtered <- filter_stop(feed, "S2")
  expect_equal(nrow(filtered$stop_times), 2)
  expect_equal(unique(filtered$stop_times$stop_id), "S2")
  expect_equal(nrow(filtered$trips), 2)
  expect_equal(nrow(filtered$transfers), 0)

  timed <- filter_time(feed, "24:00:00", "25:00:00")
  expect_equal(nrow(timed$trips), 2)
  expect_equal(nrow(timed$stop_times), 5)
  expect_true(all(
    gtfs_time_to_seconds(timed$stop_times$arrival_time) >= 24 * 3600
  ))
})

test_that("time edits propagate without wrapping at midnight", {
  feed <- minimal_feed()
  delayed <- delay_trip(feed, "T1", 900)
  expect_equal(
    delayed$stop_times$arrival_time[
      delayed$stop_times$trip_id == "T1" &
        delayed$stop_times$stop_sequence == 3
    ],
    "24:35:00"
  )

  dwell <- set_dwelltime(feed, 120, trips = "T1", stops = "S2")
  t1 <- dwell$stop_times[dwell$stop_times$trip_id == "T1", ]
  expect_equal(t1$arrival_time[2], "24:05:00")
  expect_equal(t1$departure_time[2], "24:07:00")
  expect_equal(t1$arrival_time[3], "24:21:00")
})

test_that("merge and split update identifiers and references", {
  feed <- minimal_feed()
  merged <- merge_gtfs(feed, feed)
  expect_equal(anyDuplicated(merged$trips$trip_id), 0L)
  expect_true(all(merged$stop_times$trip_id %in% merged$trips$trip_id))
  expect_true(all(merged$stop_times$stop_id %in% merged$stops$stop_id))

  split <- split_trip(feed, "T1", split = 1L)
  expect_true(all(c("T1.part1", "T1.part2") %in% split$trips$trip_id))
  expect_false("T1" %in% split$trips$trip_id)
  expect_true(all(split$stop_times$trip_id %in% split$trips$trip_id))
})

test_that("frequency rows are expanded with an exclusive end time", {
  feed <- minimal_feed()
  feed$frequencies <- data.frame(
    trip_id = "T1", start_time = "24:00:00",
    end_time = "25:00:00", headway_secs = 1200, exact_times = 0
  )
  frequency <- get_frequency(feed, "by.route")
  expect_equal(sum(frequency$daily.frequency), 4)
})

test_that("selection groups records without altering GTFS tables", {
  feed <- minimal_feed()
  grouped <- selection(feed, route_id, direction_id)
  groups <- attr(grouped, "selection")$groups

  expect_s3_class(grouped, "wizardgtfs_selected")
  expect_equal(attr(grouped, "selection")$group_vars, c("route_id", "direction_id"))
  expect_equal(groups$n_stop_calls, 6)
  expect_identical(grouped$stop_times, feed$stop_times)

  selected <- selection(grouped, stop_id %in% "S2", add = TRUE)
  expect_equal(attr(selected, "selection")$groups$n_stop_calls, 2)
  expect_equal(attr(selected, "selection")$stops, "S2")
  expect_identical(unselection(selected)$stop_times, feed$stop_times)
})

test_that("selection supports computed groups and rejects invalid results", {
  feed <- minimal_feed()
  grouped <- selection(
    feed,
    route_direction = paste(route_id, direction_id, sep = "-")
  )
  expect_equal(
    attr(grouped, "selection")$groups$route_direction,
    "R-0"
  )
  expect_error(
    selection(feed, rep(route_id, 2)),
    "returned 12 values"
  )
})

test_that("selection evaluates package spatial predicates in caller contexts", {
  feed <- minimal_feed()
  area <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -38.523, ymin = -3.733, xmax = -38.520, ymax = -3.729),
    crs = sf::st_crs(4326)
  ))
  selected <- selection(feed, geometry %intersects% area)
  expect_s3_class(selected, "wizardgtfs_selected")
  expect_true(length(attr(selected, "selection")$stops) >= 1)
})

test_that("write and read preserve a standards-compliant feed", {
  feed <- minimal_feed()
  path <- tempfile(fileext = ".zip")
  write_gtfs(feed, path)
  restored <- read_gtfs(path)

  expect_equal(nrow(restored$stop_times), nrow(feed$stop_times))
  expect_equal(restored$stop_times$stop_id, feed$stop_times$stop_id)
  expect_true(all(restored$stop_times$trip_id %in% restored$trips$trip_id))
})
