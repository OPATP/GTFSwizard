

get_transfer_clusters <- function(obj){
  UseMethod('get_transfer_clusters')
}

# get_transfer_clusters.wizardgtfs <- function(obj){
#   transfers <- get_high_transfer_stops(obj)
#   get_transfer_clusters(transfers)
# }


get_transfer_clusters.wzd_transfers <- function(obj){
  transfers_lst <- obj %>%
    dplyr::group_by(service_pattern,hour) %>%
    dplyr::group_split()

  transfer_clusters <- lapply(transfers_lst, function(tran){

    if(nrow(tran)>4){

      tran_low <- tran %>%
        dplyr::filter(n_routes<=quantile(n_routes,.75)) %>%
        dplyr::mutate(cluster=1)

      tran <- tran %>%
        dplyr::filter(n_routes>quantile(n_routes,.75))

      tran$cluster <- stats::kmeans(tran$n_routes,centers = 4)$cluster

      tran %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(mean_n_routes) %>%
        dplyr::group_by(mean_n_routes) %>%
        dplyr::mutate(cluster = dplyr::cur_group_id()) %>%
        dplyr::bind_rows(tran_low) %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        return()
    }else{
      tran %>%
        dplyr::arrange(n_routes) %>%
        dplyr::mutate(cluster = 1:dplyr::n()) %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        return()
    }




  }) %>% data.table::rbindlist()

  class(transfer_clusters) <- c('wzd_transfercluster',class(transfer_clusters))

  attr(transfer_clusters,'stop_position') <- attr(obj,'stop_position')

  return(transfer_clusters)
}
