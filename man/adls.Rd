% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adls_client_funcs.R
\name{list_adls_files}
\alias{list_adls_files}
\alias{multiupload_adls_file}
\alias{upload_adls_file}
\alias{multidownload_adls_file}
\alias{download_adls_file}
\alias{delete_adls_file}
\alias{create_adls_dir}
\alias{delete_adls_dir}
\alias{adls_file_exists}
\alias{adls_dir_exists}
\title{Operations on an Azure Data Lake Storage Gen2 filesystem}
\usage{
list_adls_files(filesystem, dir = "/", info = c("all", "name"),
  recursive = FALSE)

multiupload_adls_file(filesystem, src, dest, recursive = FALSE,
  blocksize = 2^22, lease = NULL, put_md5 = FALSE, use_azcopy = FALSE,
  max_concurrent_transfers = 10)

upload_adls_file(filesystem, src, dest = basename(src), blocksize = 2^24,
  lease = NULL, put_md5 = FALSE, use_azcopy = FALSE)

multidownload_adls_file(filesystem, src, dest, recursive = FALSE,
  blocksize = 2^24, overwrite = FALSE, check_md5 = FALSE,
  use_azcopy = FALSE, max_concurrent_transfers = 10)

download_adls_file(filesystem, src, dest = basename(src), blocksize = 2^24,
  overwrite = FALSE, check_md5 = FALSE, use_azcopy = FALSE)

delete_adls_file(filesystem, file, confirm = TRUE)

create_adls_dir(filesystem, dir)

delete_adls_dir(filesystem, dir, recursive = FALSE, confirm = TRUE)

adls_file_exists(filesystem, file)

adls_dir_exists(filesystem, dir)
}
\arguments{
\item{filesystem}{An ADLSgen2 filesystem object.}

\item{dir, file}{A string naming a directory or file respectively.}

\item{info}{Whether to return names only, or all information in a directory listing.}

\item{recursive}{For the multiupload/download functions, whether to recursively transfer files in subdirectories. For \code{list_adls_files}, and \code{delete_adls_dir}, whether the operation should recurse through subdirectories. For \code{delete_adls_dir}, this must be TRUE to delete a non-empty directory.}

\item{src, dest}{The source and destination paths/files for uploading and downloading. See 'Details' below.}

\item{blocksize}{The number of bytes to upload/download per HTTP(S) request.}

\item{lease}{The lease for a file, if present.}

\item{put_md5}{For uploading, whether to compute the MD5 hash of the file(s). This will be stored as part of the file's properties.}

\item{use_azcopy}{Whether to use the AzCopy utility from Microsoft to do the transfer, rather than doing it in R.}

\item{max_concurrent_transfers}{For \code{multiupload_adls_file} and \code{multidownload_adls_file}, the maximum number of concurrent file transfers. Each concurrent file transfer requires a separate R process, so limit this if you are low on memory.}

\item{overwrite}{When downloading, whether to overwrite an existing destination file.}

\item{check_md5}{For downloading, whether to verify the MD5 hash of the downloaded file(s). This requires that the file's \code{Content-MD5} property is set. If this is TRUE and the \code{Content-MD5} property is missing, a warning is generated.}

\item{confirm}{Whether to ask for confirmation on deleting a file or directory.}
}
\value{
For \code{list_adls_files}, if \code{info="name"}, a vector of file/directory names. If \code{info="all"}, a data frame giving the file size and whether each object is a file or directory.

For \code{download_adls_file}, if \code{dest=NULL}, the contents of the downloaded file as a raw vector.

For \code{adls_file_exists}, either TRUE or FALSE.
}
\description{
Upload, download, or delete a file; list files in a directory; create or delete directories; check file existence.
}
\details{
\code{upload_adls_file} and \code{download_adls_file} are the workhorse file transfer functions for ADLSgen2 storage. They each take as inputs a \emph{single} filename as the source for uploading/downloading, and a single filename as the destination. Alternatively, for uploading, \code{src} can be a \link{textConnection} or \link{rawConnection} object; and for downloading, \code{dest} can be NULL or a \code{rawConnection} object. If \code{dest} is NULL, the downloaded data is returned as a raw vector, and if a raw connection, it will be placed into the connection. See the examples below.

\code{multiupload_adls_file} and \code{multidownload_adls_file} are functions for uploading and downloading \emph{multiple} files at once. They parallelise file transfers by using the background process pool provided by AzureRMR, which can lead to significant efficiency gains when transferring many small files. There are two ways to specify the source and destination for these functions:
\itemize{
\item Both \code{src} and \code{dest} can be vectors naming the individual source and destination pathnames.
\item The \code{src} argument can be a wildcard pattern expanding to one or more files, with \code{dest} naming a destination directory. In this case, if \code{recursive} is true, the file transfer will replicate the source directory structure at the destination.
}

\code{upload_adls_file} and \code{download_adls_file} can display a progress bar to track the file transfer. You can control whether to display this with \code{options(azure_storage_progress_bar=TRUE|FALSE)}; the default is TRUE.

\code{adls_file_exists} and \code{adls_dir_exists} test for the existence of a file and directory, respectively.
\subsection{AzCopy}{

\code{upload_azure_file} and \code{download_azure_file} have the ability to use the AzCopy commandline utility to transfer files, instead of native R code. This can be useful if you want to take advantage of AzCopy's logging and recovery features; it may also be faster in the case of transferring a very large number of small files. To enable this, set the \code{use_azcopy} argument to TRUE.

Note that AzCopy only supports SAS and AAD (OAuth) token as authentication methods. AzCopy also expects a single filename or wildcard spec as its source/destination argument, not a vector of filenames or a connection.
}
}
\examples{
\dontrun{

fs <- adls_filesystem("https://mystorage.dfs.core.windows.net/myfilesystem", key="access_key")

list_adls_files(fs, "/")
list_adls_files(fs, "/", recursive=TRUE)

create_adls_dir(fs, "/newdir")

upload_adls_file(fs, "~/bigfile.zip", dest="/newdir/bigfile.zip")
download_adls_file(fs, "/newdir/bigfile.zip", dest="~/bigfile_downloaded.zip")

delete_adls_file(fs, "/newdir/bigfile.zip")
delete_adls_dir(fs, "/newdir")

# uploading/downloading multiple files at once
multiupload_adls_file(fs, "/data/logfiles/*.zip")
multidownload_adls_file(fs, "/monthly/jan*.*", "/data/january")

# you can also pass a vector of file/pathnames as the source and destination
src <- c("file1.csv", "file2.csv", "file3.csv")
dest <- paste0("uploaded_", src)
multiupload_adls_file(share, src, dest)

# uploading serialized R objects via connections
json <- jsonlite::toJSON(iris, pretty=TRUE, auto_unbox=TRUE)
con <- textConnection(json)
upload_adls_file(fs, con, "iris.json")

rds <- serialize(iris, NULL)
con <- rawConnection(rds)
upload_adls_file(fs, con, "iris.rds")

# downloading files into memory: as a raw vector, and via a connection
rawvec <- download_adls_file(fs, "iris.json", NULL)
rawToChar(rawvec)

con <- rawConnection(raw(0), "r+")
download_adls_file(fs, "iris.rds", con)
unserialize(con)

}
}
\seealso{
\link{adls_filesystem}, \link{az_storage}, \link{storage_download}, \link{call_azcopy}
}
