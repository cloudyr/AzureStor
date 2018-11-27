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

    sapply(lst$filesystems$name, function(fs) adls_filesystem(endpoint, fs), simplify=FALSE)
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
create_adls_filesystem.adls_filesystem <- function(endpoint, ...)
{
    create_adls_filesystem(endpoint$endpoint, endpoint$name)
}

#' @rdname adls_filesystem
#' @export
create_adls_filesystem.adls_endpoint <- function(endpoint, name, ...)
{
    obj <- adls_filesystem(endpoint, name)
    do_container_op(obj, options=list(resource="filesystem"), http_verb="PUT")
    obj
}



#' @rdname adls_filesystem
#' @export
delete_adls_filesystem <- function(endpoint, ...)
{
    UseMethod("delete_adls_filesystem")
}

#' @rdname adls_filesystem
#' @export
delete_adls_filesystem.character <- function(endpoint, key=NULL, sas=NULL,
                                             api_version=getOption("azure_adls_api_version"),
                                             ...)
{
    endp <- generate_endpoint_container(endpoint, key, sas, api_version)
    delete_adls_filesystem(endp$endpoint, endp$name, ...)
}

#' @rdname adls_filesystem
#' @export
delete_adls_filesystem.adls_filesystem <- function(endpoint, ...)
{
    delete_adls_filesystem(endpoint$endpoint, endpoint$name, ...)
}

#' @rdname adls_filesystem
#' @export
delete_adls_filesystem.adls_endpoint <- function(endpoint, name, confirm=TRUE, ...)
{
    if(confirm && interactive())
    {
        path <- paste0(endpoint$url, name)
        yn <- readline(paste0("Are you sure you really want to delete the filesystem '", path, "'? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    obj <- adls_filesystem(endpoint, name)
    do_container_op(obj, options=list(resource="filesystem"), http_verb="DELETE")
}




#' @rdname adls
#' @export
list_adls_files <- function(filesystem, dir="/", info=c("all", "name"),
                            recursive=FALSE)
{
    info <- match.arg(info)

    opts <- list(recursive=tolower(as.character(recursive)), resource="filesystem")
    opts <- c(opts, directory=as.character(dir))

    lst <- do_container_op(filesystem, "", options=opts)
    if(info == "all")
        lst$paths[c("isDirectory", "permissions", "contentLength", "lastModified", "name")]
    else lst$paths$name
}


#' @rdname adls
#' @export
upload_adls_file <- function(filesystem, src, dest, blocksize=2^24, lease=NULL)
{
    con <- if(inherits(src, "textConnection"))
        rawConnection(charToRaw(paste0(readLines(src), collapse="\n")))
    else file(src, open="rb")
    on.exit(close(con))

    # create the file
    content_type <- mime::guess_type(src)
    headers <- list(`x-ms-content-type`=content_type)
    #if(!is.null(lease))
        #headers[["x-ms-lease-id"]] <- as.character(lease)
    do_container_op(filesystem, dest, options=list(resource="file"), headers=headers, http_verb="PUT")

    # transfer the contents
    blocklist <- list()
    pos <- 0
    while(1)
    {
        print(pos)
        body <- readBin(con, "raw", blocksize)
        thisblock <- length(body)
        if(thisblock == 0)
            break

        headers <- list(
            `content-type`="application/octet-stream",
            `content-length`=sprintf("%.0f", thisblock)
        )
        opts <- list(action="append", position=sprintf("%.0f", pos))

        do_container_op(filesystem, dest, options=opts, headers=headers, body=body, http_verb="PATCH")
        pos <- pos + thisblock
    }

    # flush contents
    do_container_op(filesystem, dest,
        options=list(action="flush", position=pos),
        http_verb="PATCH")
}


#' @rdname adls
#' @export
delete_adls_file <- function(filesystem, file, confirm=TRUE)
{
    if(confirm && interactive())
    {
        endp <- filesystem$endpoint
        path <- paste0(endp$url, filesystem$name, "/", file)
        yn <- readline(paste0("Are you sure you really want to delete '", path, "'? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    opts <- list(recursive=tolower(as.character(FALSE)))
    do_container_op(filesystem, file, options=opts, http_verb="DELETE")
}



#' @rdname adls
#' @export
create_adls_dir <- function(filesystem, dir)
{
    do_container_op(filesystem, dir, options=list(resource="directory"), http_verb="PUT")
}


#' @rdname adls
#' @export
delete_adls_dir <- function(filesystem, dir, confirm=TRUE, recursive=FALSE)
{
    if(confirm && interactive())
    {
        endp <- filesystem$endpoint
        path <- paste0(endp$url, filesystem$name, "/", dir)
        yn <- readline(paste0("Are you sure you really want to delete directory '", path, "'? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    opts <- list(recursive=tolower(as.character(recursive)))
    do_container_op(filesystem, dir, options=opts, http_verb="DELETE")
}

