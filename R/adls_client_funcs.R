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



#' @rdname adls_filesystem
#' @export
list_adls_filesystems <- function(endpoint, ...)
{
    UseMethod("list_adls_filesystems")
}

#' @rdname adls_filesystem
#' @export
list_adls_filesystems.character <- function(endpoint, key=NULL, sas=NULL,
                                            api_version=getOption("azure_adls_api_version"),
                                            ...)
{
    do.call(list_adls_filesystems, generate_endpoint_container(endpoint, key, sas, api_version))
}

#' @rdname adls_filesystem
#' @export
list_adls_filesystems.adls_endpoint <- function(endpoint, ...)
{
    lst <- do_storage_call(endpoint$url, "/", options=list(resource="account"),
                           key=endpoint$key, sas=endpoint$sas, api_version=endpoint$api_version)

    lst <- lapply(lst$filesystems, function(cont) adls_filesystem(endpoint, cont$Name[[1]]))
    named_list(lst)
}



#' @rdname adls_filesystem
#' @export
create_adls_filesystem <- function(endpoint, ...)
{
    UseMethod("create_adls_filesystem")
}

#' @rdname adls_filesystem
#' @export
create_adls_filesystem.character <- function(endpoint, key=NULL, sas=NULL,
                                             api_version=getOption("azure_adls_api_version"),
                                             ...)
{
    endp <- generate_endpoint_container(endpoint, key, sas, api_version)
    create_adls_filesystem(endp$endpoint, endp$name, ...)
}

#' @rdname adls_filesystem
#' @export
create_adls_filesystem.adls_endpoint <- function(endpoint, name, ...)
{
    obj <- adls_filesystem(endpoint, name)
    do_container_op(obj, options=list(resource="filesystem"), http_verb="PUT")
    obj
}
