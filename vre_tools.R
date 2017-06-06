###########WORKSPACE INTERACTION FUNCTIONS####################



workspaceurl <- "http://node76.p.d4science.research-infrastructures.eu:8080/home-library-webapp"
#list folder elements

getCredentials<-function(){
  machinename<-Sys.info()[['nodename']]
  g<-regexpr(".*(-d).*", machinename)

  if (g[1]>0){
    workspaceurl<<-"http://node11.d.d4science.research-infrastructures.eu:8080/home-library-webapp"
  }else{workspaceurl<<-"https://workspace-repository.d4science.org/home-library-webapp"}

  configfile<-"userconfig.csv"
  configexists<-F
  if (file.exists(configfile)){
    csvfile<-read.csv(configfile,check.names=FALSE,sep=";")
    if (length(csvfile)>8){
      username<-dimnames(csvfile[9])[[2]];
      token<-dimnames(csvfile[10])[[2]];
      configexists<-T
      assign("username", username, envir = environment(downloadWS))
      assign("token", token, envir = environment(downloadWS))
      assign("configexists", configexists, envir = environment(downloadWS))
    }
  }
}


corrFolder<-function(folder){
  if (substr(folder,nchar(folder),nchar(folder))=="/"){return(substr(folder,0,nchar(folder)-1))}
  else{return(folder)}
}

mngVREFolders<-function(link){
  substf<-gsub("/MySpecialFolders", "/VRE Folders", link)
  return(substf)
}

invmngVREFolders<-function(link){
  substf<-gsub("/VRE Folders", "/MySpecialFolders", link)
  return(substf)
}

uuidGen<-function(){
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")

  uuid<-paste(
    substr(baseuuid,1,8),
    "-",
    substr(baseuuid,9,12),
    "-",
    "4",
    substr(baseuuid,13,15),
    "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18),
    "-",
    substr(baseuuid,19,30),
    sep="",
    collapse=""
  )

  return(uuid)
}

listHomeWS<-function(){
  getCredentials()
  home=paste("/Home/",username,"/Workspace",sep="")
  homeFolders<-listWS(home)
  }

listWS<-function(path){
  getCredentials()
  path<-invmngVREFolders(path)
  path<-corrFolder(path)
  getRoot<-paste(workspaceurl,"/rest/List?absPath=",URLencode(path),"/",sep="")
  cat("Retrieving the list of folders and files in",path,"\n")
  #cat(getRoot)
  got<-GET(getRoot,authenticate(username,token), timeout(1*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  #print(xmltop)
  foldersListxpath = xpathSApply(xmltop, "//map/entry/string")
  foldersList<-(rapply(foldersListxpath, function(x) head(x, 1)))
  if (length(foldersList)>0){
    cat("Found",length(foldersList),"elements in the folder\n")
    foldersList<-foldersList[seq(3,length(foldersList),by=3)]
    folders<-as.character(foldersList)
    folders<-paste(path,"/",folders,sep="")
  }else{cat("Found no elements in the folder\n"); folders<-""}
  folders<-mngVREFolders(folders)
  return(folders)
}

deleteWS<-function(file){
  getCredentials()
  file<-invmngVREFolders(file)
  getRoot<-paste(workspaceurl,"/rest/Delete?absPath=",URLencode(file),sep="")
  cat("Deleting remote file",file,"\n")
  got<-GET(getRoot,authenticate(username,token), timeout(1*3600))
  #closeAllConnections()
  if (got$status_code==200){
    cat("Delete OK\n")
    return(T)
  }else {cat("Delete KO\n"); return(F)}
}

downloadFileWS<-function(element){
  downloadWS(element,T)
}

downloadFolderWS<-function(element){
  downloadWS(element,F)
}

downloadWS<-function(element,isfile){
  getCredentials()
  element<-invmngVREFolders(element)
  authent<-paste("https://",username,":",token,"@",sep="")
  authurl<-gsub("https://", authent, workspaceurl)
  getRoot<-paste(authurl,"/rest/Download?absPath=",URLencode(element),sep="")
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)

  zip<-F
  if (g[1]==nchar(element) || !isfile){
    cat("The entire folder will be downloaded\n")
    filename<-paste(uuidGen(),".zip",sep="")
    zip<-T
  } else{filename<-substring(element,g[1]+1,nchar(element))}
  cat("Downloading...\n")
  download.file(getRoot, filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  cat("...Finished Dowloading\n")
  if (zip){
    cat("Unzipping",filename,"to",getwd(),"\n")
    unzip(filename,exdir=getwd())
    if (file.exists(filename)){cat("Cleaning the zip file...\n"); file.remove(filename)}
  }
  #closeAllConnections()
  cat("All done.\n")
}

uploadWS<-function(path,file,overwrite){
  uploadWSManager(path,file,overwrite,F)
}

uploadWSManager<-function(path, file, overwrite, archive){
  getCredentials()
  path <- invmngVREFolders(path)
  name <- basename(file)
  description = name
  mimetype = ""
  path <- corrFolder(path)

  if (archive) {
    getUrl<-paste(workspaceurl,"/rest/Unzip?name=",URLencode(name),"&description=",URLencode(description),"&parentPath=",URLencode(path),"/&replace=true&hardReplace=true",sep="")
  } else {
    getUrl<-paste(workspaceurl,"/rest/Upload?name=",URLencode(name),"&description=",URLencode(description),"&parentPath=",URLencode(path),"/",sep="")
  }

  cat("Uploading",file,"to",path,"\n")
  out<-POST(url = getUrl, config = authenticate(username, token, type = "basic"),
            body = upload_file(file, type=mimetype), encode = c("multipart"), handle = NULL, timeout(1*3600))

  res <- xml2::as_list(content(out))$body$string

  output <- T
  if (length(res)) {
    if (length(grep("Exception", res[[1]]))>0){
      if (overwrite){
        cat("Overwriting\n")
        todel<-paste(path,"/",name, sep="")
        del <- deleteWS(todel)
        if(del){
          cat("Updating..\n")
          return(uploadWSManager(path,file,overwrite,archive))
        }else{cat("Could not delete file",todel,"\n"); output<-F}
      }else{cat("Error, file already present in the folder\n"); output<-F}
    }
  }else {cat("Service error\n"); output<-F}

  #closeAllConnections()
  cat("All done.\n")
  return(output)
}

uploadAllWS<-function(path){
  overwrite=T
  home=corrFolder(paste("/Home/",username,"/Workspace",sep=""))
  pathverification = corrFolder(path)
  if (pathverification==home){
    cat("Cannot upload to the Home folder directly\n")
    return()
  }
  getCredentials()
  zipfile<-"all.zip"

  if (file.exists(zipfile)){cat("Cleaning...\n"); file.remove(zipfile)}
  cat("Preparing workspace package\n")

  wd<-username
  exclude<-paste("-x ",wd,"/.**\\* ",wd,"/R/**\\*",sep="")
  zip <- paste("cd .. && zip -r ",wd,"/all.zip ",wd,"/ ",exclude,sep="")
  system(zip, intern = FALSE)

  returns<-F
  if (file.exists(zipfile)){
    cat("Ready to upload to",path,"\n")
    q<-uploadWSManager(path,zipfile,overwrite,T)
    cat("Cleaning...\n")
    if (file.exists(zipfile)){file.remove(zipfile)}
    cat("All done.\n")
    returns<-q
  }else{cat("Error - could not create zip package\n")}
  return(returns)
}

getPublicFileLinkWS<-function(remotefile){
  getCredentials()
  remotefile<-invmngVREFolders(remotefile)
  link<-""
  cat("Retrieving public URL for",remotefile,"\n")
  getRoot<-paste(workspaceurl,"/rest/GetPublicLink?absPath=",URLencode(remotefile),"&shortUrl=false",sep="")
  got<-GET(getRoot,authenticate(username,token), timeout(1*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  fxpath = xpathSApply(xmltop, "//string")
  flist<-(rapply(fxpath, function(x) head(x, 1)))
  if (length(flist)>0){
    flist<-flist[seq(3,length(flist),by=3)]
    link<-flist[[1]]
    cat("Public URL retrieved",link,"\n")
  }else{cat("Impossible to produce public link for file",remotefile,"\n"); link<-""}
  return(link)
}

###########END - WORKSPACE INTERACTION FUNCTIONS####################


downloadURI <- function(element) {
  getCredentials()
  element <- invmngVREFolders(element)
  authent <- paste0("https://", username, ":", token, "@")
  authurl <- gsub("https://", authent, workspaceurl)

  paste0(authurl, "/rest/Download?absPath=", URLencode(element))
}
