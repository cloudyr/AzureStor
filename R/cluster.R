#' Parallelise multiple file transfers in the background
#'
#' @param max_concurrent_transfers The maximum number of concurrent file transfers to support, which translates into the number of background R processes to create. Each concurrent transfer requires a separate R process, so limit this is you are low on memory.
#' @param restart For `start_cluster`, whether to terminate an already running cluster first. If this is FALSE (the default) and a cluster exists, the function does nothing.
#'
#' @details
#' AzureStor can parallelise transferring files by deploying a pool of R processes in the background. This can lead to significant speedups when transferring multiple small files. The pool is deployed the first time that a multiple file transfer is begun, and remains persistent for the session or until terminated by `stop_cluster`.
#'
#' `start_cluster` starts a cluster, or restarts an existing one if a cluster is already running. `clean_cluster` removes any artifacts from previous file transfers; it is called by the multiupload/download functions. `stop_cluster` terminates the cluster, freeing the memory used.
#'
#' @rdname cluster
#' @export
start_cluster <- function(max_concurrent_transfers=10, restart=FALSE)
{
    if(restart && exists("clus", envir=.AzureStor))
    {
        parallel::stopCluster(.AzureStor$clus)
        rm(clus, envir=.AzureStor$clus)
    }

    if(!exists("clus", envir=.AzureStor))
    {
        message("Starting background pool")
        .AzureStor$clus <- parallel::makeCluster(max_concurrent_transfers)
        parallel::clusterEvalQ(.AzureStor$clus, loadNamespace("AzureStor"))
    }

    invisible(NULL)
}

#' @rdname cluster
#' @export
clean_cluster <- function()
{
    if(!exists("clus", envir=.AzureStor))
        return()
    
    parallel::clusterEvalQ(.AzureStor$clus, rm(list=ls(all.names=TRUE)))
    invisible(NULL)
}

#' @rdname cluster
#' @export
stop_cluster <- function()
{
    if(!exists("clus", envir=.AzureStor))
        return()
    
    message("Stopping background pool")
    parallel::stopCluster(.AzureStor$clus)
}
