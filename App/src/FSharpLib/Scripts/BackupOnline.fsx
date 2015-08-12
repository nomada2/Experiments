﻿#light
//#load "Library1.fs"
//#r "Ionic.Zip.dll"

#r @"C:\Git\Easj360Git\Easj360\bin\Debug\Ionic.Zip.dll"

open System
open System.IO
open System.Security
open System.Net
open Ionic.Zip

let username = "SateFtp"
let password = "Jocker74!"
let ftpUrl = "ftp://www.satepiscine.com"  // my site
let url = "https://www.dropbox.com/sh/xalp3kq6nip6uw7/uwKWUeSxjc?m"     // dropbox

let getFiles path filter =
    let rec getFilesRec path =
        seq {   for file in Directory.GetFiles(path, filter) do yield file
                for dir in Directory.GetDirectories(path) do yield! getFilesRec dir }
    getFilesRec path              
                
let compressFiles (files:string seq) (destinationZipFile:string) pwd =
    use zip = new ZipFile()
    //zip.set_Password(pwd)
    //zip.Encryption <- EncryptionAlgorithm.WinZipAes128
    zip.CompressionLevel <- Ionic.Zlib.CompressionLevel.BestCompression
    zip.StatusMessageTextWriter <- System.Console.Out
    zip.AddFiles(files)
    zip.Save(destinationZipFile)

let decompressFiles (zipToUnpack:string) (unpackDirectory:string) pwd =
    use zip = ZipFile.Read(zipToUnpack)
//    zip.set_Password(pwd)
//    zip.Encryption <- EncryptionAlgorithm.WinZipAes128
    zip.StatusMessageTextWriter <- System.Console.Out
    for file in zip do
       file.Extract(unpackDirectory, ExtractExistingFileAction.OverwriteSilently)
    
let getFtpRequest filePath reqMethod = 
    let request = FtpWebRequest.Create(ftpUrl + "/" + filePath) :?> FtpWebRequest
    request.Method <- reqMethod // WebRequestMethods.Ftp
    request.Credentials <- new NetworkCredential(username, password)
    request.UsePassive <- true
    request.UseBinary <- true
    request.KeepAlive <- false
    request

let checkIfFileExists filename =
    let request = getFtpRequest filename WebRequestMethods.Ftp.GetFileSize
    try
        use response = request.GetResponse()
        let size = response.ContentLength
        true
    with
    |_ -> false
 
let deleteFile filePath =
     if checkIfFileExists filePath then
        try
            let request = getFtpRequest filePath WebRequestMethods.Ftp.DeleteFile
            use response = request.GetResponse()
            response.Close()
            true
        with
        |_ -> false 
     else
        false         

                                   
let uploadFile fileNameSource fileNameDestination = 
    if checkIfFileExists fileNameDestination then
        deleteFile fileNameDestination |> ignore
    let request = getFtpRequest fileNameDestination WebRequestMethods.Ftp.UploadFile
    use reqStream = request.GetRequestStream()
    use fileStream = new FileStream(fileNameSource, FileMode.Open, FileAccess.Read, FileShare.Read)
    fileStream.CopyTo(reqStream)
        
let downloadFile fileNameSource fileNameDestination =
    if checkIfFileExists fileNameSource then
        let request = getFtpRequest fileNameSource WebRequestMethods.Ftp.DownloadFile
        use response = request.GetResponse() :?> FtpWebResponse
        use stream = response.GetResponseStream()
        use fileStream = new FileStream(fileNameDestination, FileMode.Create, FileAccess.Write, FileShare.Write)
        stream.CopyTo(fileStream)     
        stream.Close()
        fileStream.Close()                 
        
let uploadZippedFolder folderSource filePathDestination =
    let files = getFiles folderSource "*.*"
    compressFiles files filePathDestination "password"
    uploadFile filePathDestination (Path.GetFileName(filePathDestination))
             
let downloadUnzippedFolder fileNameSource folderPathDestination =
    let fileNameDownloaded = folderPathDestination + "\\" + fileNameSource
    downloadFile fileNameSource fileNameDownloaded
    decompressFiles fileNameDownloaded folderPathDestination "password"
     
uploadZippedFolder "C:\Temp\ObjectPool" "test.zip"
downloadUnzippedFolder "test.zip" @"c:\temp\test"

getFiles "C:\Temp\ObjectPool" "*.*"
