# empty gtfs ----
# full
gtfs_meta <-
  tidytransit:::gtfs_meta

empty_gtfs_full <- list()

for (i in 1:length(gtfs_meta)) {

  colnames <- gtfs_meta[[i]]$field
  coltypes <- paste0(gtfs_meta[[i]]$coltype, "()")
  table <- purrr::map2_dfc(colnames, coltypes, ~ setNames(list(eval(parse(text = .y))), .x))
  empty_gtfs_full[[i]] <- table
  names(empty_gtfs_full)[i] <- names(gtfs_meta)[i]
    
}

empty_gtfs_full %>% GTFSwizard::as_wizardgtfs()

# minimum
req <- 
lapply(gtfs_meta, function(x) {x$file_spec == 'req'}) %>% unlist %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('table', 'required')) %>% 
  as_tibble() %>% 
  filter(required == TRUE)

gtfs_meta_minimum <-
  gtfs_meta[1:6]

empty_gtfs_minimum <- list()

for (i in 1:length(gtfs_meta_minimum)) {
  
  field_spec <- gtfs_meta[[i]]$field_spec == 'req'
  colnames <- gtfs_meta[[i]]$field[field_spec]
  coltypes <- paste0(gtfs_meta[[i]]$coltype[field_spec], "()")
  table <- purrr::map2_dfc(colnames, coltypes, ~ setNames(list(eval(parse(text = .y))), .x))
  empty_gtfs_minimum[[i]] <- table
  names(empty_gtfs_minimum)[i] <- names(gtfs_meta_minimum)[i]
  
}

empty_gtfs_minimum
empty_gtfs_minimum %>% GTFSwizard::as_wizardgtfs()

