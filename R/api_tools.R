##### Temporal Folder #####
#' Create temporal folder in main directory
#'
#' This function create a temporal folder in local directory
#' @param reset Reset all the tmp directory
#' @export
#' @examples
#' tmp_creator()
#'
tmp_creator = function(reset=FALSE){
  tmpdir = "tmp"
  if(reset){
    message("Remove files in tmp")
    unlink(tmpdir, recursive=TRUE)
  }

  if(!dir.exists(tmpdir)){
  message("Temporal folder created")
  dir.create(tmpdir)
  }
  return(tmpdir)
}

tmp_creator()

#### SHAPE ####
#' Read the Shape data from API
#'
#' This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
#' @param client Client name
#' @param farm email of user
#' @param email Email of custome user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE.
#' @param reset reset temporal folder
#' @keywords vectorial data as sf object
#' @export
#' @examples
#' read_shape(client='clientexample', farm='farm1example',email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_ke)
#'
read_shape = function(client,farm,email,api_key,dash_param=FALSE,reset=FALSE){
  tmp = tmp_creator(reset = reset)
  id_farm = paste(client,farm,collapse = "_")

  file = file.path(tmp,paste0(id_farm,".Rdata"))

  if(file.exists(file)){
    message("Reading shape: ", id_farm)
    load(file)

  }else{
    message("Downloading shape: ", client,"-",farm)

    farm_req = ASAPI::asapi_farm_get(client = client, farm = farm,
                                     email=email, api_key=api_key, dash_param=dash_param)
    save(farm_req,file=file)
  }
  return(farm_req)
}

#### RASTER ####
#' Read the Raster data from API
#'
#' This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
#' @param client Client name
#' @param farm email of user
#' @param sensor satellite sensor
#' @param index Api Key obtain from /auth
#' @param date URL for dev purpose
#' @param email Email of custome user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE.
#' @param reset reset temporal folder
#' @keywords read raster and keep in local
#' @export
#' @examples
#' read_rst(client='clientexample', farm='farm1example', sensor = "S2SR", index = "NDVI", date = '2021-02-07', email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)
#'
read_rst = function(client,farm,sensor,index,date,email,api_key,dash_param=FALSE,reset=FALSE){
  tmp = tmp_creator(reset = reset)
  id_rst = paste(client,farm,sensor,index,date,sep="_")
  file = file.path(tmp, paste0(id_rst,".grd"))
  if(file.exists(file)){
    message("Reading image: ", id_rst)
    if(index=="RGB"){
      rst = raster::brick(file)
    }else{
      rst = raster::raster(file)
    }

  }else{
    message("Downloading image: ",id_rst)
    rst = ASAPI::asapi_image(client = client, farm = farm,
                             sensor=sensor, index=index,
                             date=date, email=email,
                             api_key=api_key, dash_param=dash_param)
    message("Print image desp: ",rst)

    rst = raster::writeRaster(rst$rst,
                              filename=file.path(tmp, paste0(id_rst,".grd")),
                              bandorder='BIL', overwrite=TRUE)
  }
  return(list(rst=rst,file=id_rst))
}



#### IMAGE ####
#' Plot RGB
#'
#' This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
#' @param client Client name
#' @param farm email of user
#' @param date URL for dev purpose
#' @param email Email of custome user
#' @param api_key Api Key obtain from /auth
#' @keywords plot rgb raster and keep in local
#' @export
#' @examples
#' post_rgb_plot(client='clientexample', farm='farm1example', date = '2021-02-07', email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)
#'
post_rgb_plot = function(client, farm, date,email, height = 200,width = 200,api_key, url = "https://api.agrospace.cl"){
  param_query = list(client = client, farm = farm, date = date,height=height, width=width,email = email, api_key = api_key)
  res = httr::POST(url = asapi_url(url = url, endpoint = "/plot"),
                   query = param_query)

  res = httr::content(res, as = "text", encoding = "UTF-8")
  base::message(res)

  return(list(res))
}

#### IMAGE ####
#' Plot RGB
#'
#' This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
#' @param client Client name
#' @param farm email of user
#' @param date URL for dev purpose
#' @param email Email of custome user
#' @param api_key Api Key obtain from /auth
#' @param path Address where the image will be saved. Default "getwd()"
#' @param name Name that the image will take. Default "client_farm_dare_RGB"
#' @keywords plot rgb raster and keep in local
#' @export
#' @examples
#' get_rgb_plot(client='clientexample', farm='farm1example', date = '2021-02-07', email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)
#'
get_rgb_plot = function(client, farm, date,email,api_key, url = "https://api.agrospace.cl",
                        path=getwd(), name=NULL){
  param_query = list(client = client, farm = farm, date = date,email = email, api_key = api_key)
  res = httr::GET(url = asapi_url(url = url, endpoint = "/plot"),
                  query = param_query)


  if (res$status_code == 200) {
    res = httr::content(res, encoding = "UTF-8")
    if(is.null(name)){filename = paste0(path, paste(client,farm,date,"RGB",sep = "_"),".png")}
    else{filename = paste0(path, name,".png")}
    download.file(res$link[[1]]  ,filename, mode = 'wb')
  } else {
    res = httr::content(res, encoding = "UTF-8")
    filename=NULL
    base::message(res)
  }

  return(list(res,filename=filename))
}


