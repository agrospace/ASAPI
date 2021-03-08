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
#' @param reset reset temporal folder
#' @keywords vectorial data as sf object
#' @export
#' @examples
#' read_shape(client='clientexample', farm='farm1example',email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_ke)
#'
read_shape = function(client,farm,email,api_key,reset=FALSE){
  tmp = tmp_creator(reset = reset)
  id_farm = paste0(client,farm,collapse = "-")

  file = file.path(tmp,paste0(id_farm,".Rdata"))

  if(file.exists(file)){
    message("Reading shape: ", id_farm)
    load(file)

  }else{
    message("Downloading shape: ", client,"-",farm)

    farm_req = ASAPI::asapi_farm_get(client = client, farm = farm,
                                     email=email, api_key=api_key)
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
#' @param reset reset temporal folder
#' @keywords read raster and keep in local
#' @export
#' @examples
#' read_rst(client='clientexample', farm='farm1example', sensor = "S2SR", index = "NDVI", date = '2021-02-07', email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)
#'
read_rst = function(client,farm,sensor,index,date,email,api_key,reset=FALSE){
  tmp = tmp_creator(reset = reset)
  id_rst = paste0(client,farm,sensor,index,date,collapse = "_")
  file = file.path(tmp, paste0(id_rst,".grd"))
  if(file.exists(file)){
    message("Reading image: ", id_rst)
    if(index=="RGB"){
      rst = raster::brick(file)
    }else{
      rst = raster::raster(file)
    }

  }else{
    message("Downloading image: ", index,"-",date)
    rst = ASAPI::asapi_image(client = client, farm = farm,
                             sensor=sensor, index=index,
                             date=date, email=email,
                             api_key=api_key)

    rst = raster::writeRaster(rst$rst,
                              filename=file.path(tmp, paste0(id_rst,".grd")),
                              bandorder='BIL', overwrite=TRUE)
  }
  return(rst)
}
