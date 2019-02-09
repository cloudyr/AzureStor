# define container

#' @export
storage_container <- function(endpoint, ...)
UseMethod("storage_container")

#' @export
storage_container.blob_endpoint <- function(endpoint, name, ...)
blob_container(endpoint, name, ...)

#' @export
storage_container.file_share <- function(endpoint, name, ...)
file_share(endpoint, name, ...)

#' @export
storage_container.adls_filesystem <- function(endpoint, name, ...)
adls_filesystem(endpoint, name, ...)

#' @export
storage_container.character <- function(endpoint, key=NULL, token=NULL, sas=NULL, ...)
{
    lst <- parse_storage_url(endpoint)
    endpoint <- storage_endpoint(lst[[1]], key=key, token=token, sas=sas, ...)
    storage_container(endpoint, lst[[2]], ...)
}


# create container

#' @export
create_storage_container <- function(endpoint, ...)
UseMethod("create_storage_container")

#' @export
create_storage_container.blob_endpoint <- function(endpoint, name, ...)
create_blob_container(endpoint, name, ...)

#' @export
create_storage_container.file_endpoint <- function(endpoint, name, ...)
create_file_share(endpoint, name, ...)

#' @export
create_storage_container.adls_endpoint <- function(endpoint, name, ...)
create_adls_filesystem(endpoint, name, ...)

#' @export
create_storage_container.storage_container <- function(endpoint, ...)
create_storage_container(endpoint$endpoint, endpoint$name, ...)

#' @export
create_storage_container.character <- function(endpoint, key=NULL, token=NULL, sas=NULL, ...)
{
    lst <- parse_storage_url(endpoint)
    endpoint <- storage_endpoint(lst[[1]], key=key, token=token, sas=sas, ...)
    create_storage_container(endpoint, lst[[2]], ...)
}


# delete container

#' @export
delete_storage_container <- function(endpoint, ...)
UseMethod("delete_storage_container")

#' @export
delete_storage_container.blob_endpoint <- function(endpoint, name, ...)
delete_blob_container(endpoint, name, ...)

#' @export
delete_storage_container.file_endpoint <- function(endpoint, name, ...)
delete_file_share(endpoint, name, ...)

#' @export
delete_storage_container.adls_endpoint <- function(endpoint, name, ...)
delete_adls_filesystem(endpoint, name, ...)

#' @export
delete_storage_container.storage_container <- function(endpoint, ...)
delete_storage_container(endpoint$endpoint, endpoint$name, ...)

#' @export
delete_storage_container.character <- function(endpoint, key=NULL, token=NULL, sas=NULL, ...)
{
    lst <- parse_storage_url(endpoint)
    endpoint <- storage_endpoint(lst[[1]], key=key, token=token, sas=sas, ...)
    delete_storage_container(endpoint, lst[[2]], ...)
}


# list containers

#' @export
list_storage_containers <- function(endpoint, ...)
UseMethod("list_storage_containers")

#' @export
list_storage_containers.blob_endpoint <- function(endpoint, ...)
list_blob_containers(endpoint, ...)

#' @export
list_storage_containers.file_endpoint <- function(endpoint, ...)
list_file_shares(endpoint, ...)

#' @export
list_storage_containers.adls_endpoint <- function(endpoint, ...)
list_adls_filesystems(endpoint, ...)

#' @export
list_storage_containers.character <- function(endpoint, key=NULL, token=NULL, sas=NULL, ...)
{
    lst <- parse_storage_url(endpoint)
    endpoint <- storage_endpoint(lst[[1]], key=key, token=token, sas=sas, ...)
    list_storage_containers(endpoint, lst[[2]], ...)
}


# upload

#' @export
storage_upload <- function(container, ...)
UseMethod("storage_upload")

#' @export
storage_upload.blob_container <- function(container, src, dest, ...)
upload_blob(container, src, dest, ...)

#' @export
storage_upload.file_share <- function(container, src, dest, ...)
upload_azure_file(container, src, dest, ...)

#' @export
storage_upload.adls_filesystem <- function(container, src, dest, ...)
upload_adls_file(container, src, dest, ...)

#' @export
storage_multiupload <- function(container, ...)
UseMethod("storage_multiupload")

#' @export
storage_multiupload.blob_container <- function(container, src, dest, ...)
multiupload_blob(container, src, dest, ...)

#' @export
storage_multiupload.file_share <- function(container, src, dest, ...)
multiupload_azure_file(container, src, dest, ...)

#' @export
storage_multiupload.adls_filesystem <- function(container, src, dest, ...)
multiupload_adls_file(container, src, dest, ...)


# download

#' @export
storage_download <- function(container, ...)
UseMethod("storage_download")

#' @export
storage_download.blob_container <- function(container, src, dest, ...)
download_blob(container, src, dest, ...)

#' @export
storage_download.file_share <- function(container, src, dest, ...)
download_azure_file(container, src, dest, ...)

#' @export
storage_download.adls_filesystem <- function(container, src, dest, ...)
download_adls_file(container, src, dest, ...)

#' @export
storage_multidownload <- function(container, ...)
UseMethod("storage_multidownload")

#' @export
storage_multidownload.blob_container <- function(container, src, dest, ...)
multidownload_blob(container, src, dest, ...)

#' @export
storage_multidownload.file_share <- function(container, src, dest, ...)
multidownload_azure_file(container, src, dest, ...)

#' @export
storage_multidownload.adls_filesystem <- function(container, src, dest, ...)
multidownload_adls_file(container, src, dest, ...)


# list files

#' @export
list_storage_files <- function(container, ...)
UseMethod("list_storage_files")

#' @export
list_storage_files.blob_container <- function(container, ...)
list_blobs(container, ...)

#' @export
list_storage_files.file_share <- function(container, ...)
list_azure_files(container, ...)

#' @export
list_storage_files.adls_filesystem <- function(container, ...)
list_adls_files(container, ...)


# create directory

#' @export
create_storage_dir <- function(container, ...)
UseMethod("create_storage_dir")

#' @export
create_storage_dir.blob_container <- function(container, ...)
stop("Blob storage does not support directories")

#' @export
create_storage_dir.file_share <- function(container, dir, ...)
create_azure_dir(container, dir, ...)

#' @export
create_storage_dir.adls_filesystem <- function(container, dir, ...)
create_adls_dir(container, dir, ...)


# delete directory

#' @export
delete_storage_dir <- function(container, ...)
UseMethod("delete_storage_dir")

#' @export
delete_storage_dir.blob_container <- function(container, ...)
stop("Blob storage does not support directories")

#' @export
delete_storage_dir.file_share <- function(container, dir, ...)
delete_azure_dir(container, dir, ...)

#' @export
delete_storage_dir.adls_filesystem <- function(container, dir, ...)
delete_adls_dir(container, dir, ...)


# delete file

#' @export
delete_storage_file <- function(container, ...)
UseMethod("delete_storage_file")

#' @export
delete_storage_file.blob_container <- function(container, file, ...)
delete_blob(container, file, ...)

#' @export
delete_storage_file.file_share <- function(container, file, ...)
delete_azure_file(container, file, ...)

#' @export
delete_storage_file.adls_filesystem <- function(container, file, ...)
delete_adls_file(container, file, ...)

