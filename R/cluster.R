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
        .AzureStor$clus <- parallel::makeCluster(max_concurrent_transfers)
        parallel::clusterEvalQ(.AzureStor$clus, loadNamespace("AzureStor"))
    }

    invisible(NULL)
}

#' @export
clean_cluster <- function()
{
    if(!exists("clus", envir=.AzureStor))
        return()
    
    parallel::clusterEvalQ(.AzureStor$clus, rm(list=ls(all.names=TRUE)))
    invisible(NULL)
}

#' @export
stop_cluster <- function()
{
    if(!exists("clus", envir=.AzureStor))
        return()
    
    parallel::stopCluster(.AzureStor$clus)
}
