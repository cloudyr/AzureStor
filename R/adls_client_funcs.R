#' @rdname adls_filesystem
#' @export
adls_filesystem <- function(endpoint, ...)
{
    UseMethod("adls_filesystem")
}

#' @rdname adls_filesystem
#' @export
adls_filesystem.character <- function(endpoint, key=NULL, sas=NULL,
                                      api_version=getOption("azure_storage_api_version"),
                                      ...)
{
    do.call(adls_filesystem, generate_endpoint_container(endpoint, key, sas, api_version))
}

#' @rdname adls_filesystem
#' @export
adls_filesystem.adls_endpoint <- function(endpoint, name, ...)
{
    obj <- list(name=name, endpoint=endpoint)
    class(obj) <- "adls_filesystem"
    obj
}

#' @rdname adls_filesystem
#' @export
print.adls_filesystem <- function(x, ...)
{
    cat("Azure Data Lake Storage Gen2 filesystem '", x$name, "'\n", sep="")
    cat(sprintf("URL: %s\n", paste0(x$endpoint$url, x$name)))
    if(!is_empty(x$endpoint$key))
        cat("Access key: <hidden>\n")
    else cat("Access key: <none supplied>\n")
    if(!is_empty(x$endpoint$sas))
        cat("Account shared access signature: <hidden>\n")
    else cat("Account shared access signature: <none supplied>\n")
    cat(sprintf("Storage API version: %s\n", x$endpoint$api_version))
    invisible(x)
}

