multiupload_adls_file_internal <- function(filesystem, src, dest, blocksize=2^22, lease=lease, retries=5,
                                           max_concurrent_transfers=10)
{
    src_files <- glob2rx(basename(src))
    src_dir <- dirname(src)
    src <- dir(src_dir, pattern=src_files, full.names=TRUE)

    if(length(src) == 0)
        stop("No files to transfer", call.=FALSE)
    if(length(src) == 1)
        return(upload_adls_file(filesystem, src, dest, blocksize=blocksize, lease=lease, retries=retries))

    init_pool(max_concurrent_transfers)

    parallel::clusterExport(.AzureStor$pool,
        c("filesystem", "dest", "blocksize"),
        envir=environment())
    parallel::parLapply(.AzureStor$pool, src, function(f)
    {
        dest <- sub("//", "/", file.path(dest, basename(f))) # API too dumb to handle //'s
        AzureStor::upload_adls_file(filesystem, f, dest, blocksize=blocksize, lease=lease, retries=retries)
    })
    invisible(NULL)
}


upload_adls_file_internal <- function(filesystem, src, dest, blocksize=2^24, lease=NULL, retries=5)
{
    con <- if(inherits(src, "textConnection"))
        rawConnection(charToRaw(paste0(readLines(src), collapse="\n")))
    else if(inherits(src, "rawConnection"))
        src
    else file(src, open="rb")
    on.exit(close(con))

    # create the file
    content_type <- if(inherits(src, "connection"))
        "application/octet-stream"
    else mime::guess_type(src)
    headers <- list(`x-ms-content-type`=content_type)
    #if(!is.null(lease))
        #headers[["x-ms-lease-id"]] <- as.character(lease)
    do_container_op(filesystem, dest, options=list(resource="file"), headers=headers, http_verb="PUT")

    # transfer the contents
    blocklist <- list()
    pos <- 0
    while(1)
    {
        body <- readBin(con, "raw", blocksize)
        thisblock <- length(body)
        if(thisblock == 0)
            break

        headers <- list(
            `content-type`="application/octet-stream",
            `content-length`=sprintf("%.0f", thisblock)
        )
        opts <- list(action="append", position=sprintf("%.0f", pos))

        for(r in seq_len(retries + 1))
        {
            res <- tryCatch(
                do_container_op(filesystem, dest, headers=headers, body=body, options=opts, http_verb="PATCH"),
                error=function(e) e
            )
            if(retry_transfer(res))
                message(retry_upload_message(src))
            else break 
        }
        if(inherits(res, "error"))
            stop(res)

        pos <- pos + thisblock
    }

    # flush contents
    do_container_op(filesystem, dest,
        options=list(action="flush", position=sprintf("%.0f", pos)),
        http_verb="PATCH")
}


multidownload_adls_file_internal <- function(filesystem, src, dest, overwrite=FALSE, retries=5,
                                             max_concurrent_transfers=10)
{
    src_dir <- dirname(src)
    if(src_dir == ".")
        src_dir <- "/"

    files <- list_adls_files(filesystem, src_dir, info="name")
    src <- grep(glob2rx(src), files, value=TRUE) # file listing on ADLS includes directory name

    if(length(src) == 0)
        stop("No files to transfer", call.=FALSE)
    if(length(src) == 1)
        return(download_adls_file(filesystem, src, dest, overwrite=overwrite, retries=retries))

    init_pool(max_concurrent_transfers)

    parallel::clusterExport(.AzureStor$pool,
        c("filesystem", "dest", "overwrite"),
        envir=environment())
    parallel::parLapply(.AzureStor$pool, src, function(f)
    {
        dest <- file.path(dest, basename(f))
        AzureStor::download_adls_file(filesystem, f, dest, overwrite=overwrite, retries=retries)
    })
    invisible(NULL)
}


download_adls_file_internal <- function(filesystem, src, dest, overwrite=FALSE, retries=5)
{
    file_dest <- is.character(dest)
    null_dest <- is.null(dest)
    conn_dest <- inherits(dest, "rawConnection")

    if(!file_dest && !null_dest && !conn_dest)
        stop("Unrecognised dest argument", call.=FALSE)

    # if dest is NULL or a raw connection, return the transferred data in memory as raw bytes
    config <- if(file_dest)
        httr::write_disk(dest, overwrite)
    else list()
    handler <- if(file_dest) "stop" else "pass"

    for(r in seq_len(retries + 1))
    {
        res <- tryCatch({
            response <- do_container_op(filesystem, src, config=config, progress="down", http_status_handler=handler)
            if(!file_dest)
                httr::stop_for_status(response, storage_error_message(response))
            else response
        }, error=function(e) e)

        if(retry_transfer(res))
            message("Error downloading file ", src, ", retrying...")
        else break 
    }
    if(inherits(res, "error"))
        stop(res)

    if(conn_dest)
    {
        writeBin(httr::content(res, as="raw"), dest)
        seek(dest, 0)
    }

    if(null_dest)
        httr::content(res, as="raw")
    else invisible(NULL)
}
