#' @export setPath
#'
#' @title sets custom output path
#' @description
#' This function allows the user to set the output dirrectory path
#' @name setPath
#' @rdname setPath
#' @aliases setPath
#' @usage
#' setPath(MACSQuant,path=NULL)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @param path user defined path, default is tempd
#' @examples
#' filepath <- system.file("extdata", "SingleDrugs.xlsx",
#'     package = "MACSQuantifyR")
#' #MACSQuant = load_MACSQuant(filepath)
#' user_path="."
#' #MACSQuant = setPath(MACSQuant,path=user_path)
#' @return object of class MACSQuant with updated path

setPath = function(MACSQuant,path=NULL)
{
    if (!is.null(path))
    {
    MACSQuant@param.output$path=path
    }
    else
    {
    message('You did not specify any path for the ouput directory yet')
    message(paste('Files will be stored at a temporary path: ',
                      tempdir(),'/outputMQ',sep=""))
    message('If you want to retreive output files specify a path')
    }
return(MACSQuant)
}


#' @export rData
#'
#' @title accessor function to access raw data
#' @description
#' This function allows the user to access raw data table
#' @name rData
#' @rdname rData
#' @aliases rData
#' @usage
#' rData(MACSQuant)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @examples
#' filepath <- system.file("extdata", "SingleDrugs.xlsx",
#'     package = "MACSQuantifyR")
#' #MACSQuant = load_MACSQuant(filepath)
#'
#' #rData(MACSQuant)
#' @return the raw data table

rData = function(MACSQuant)
{
    return(MACSQuant@my_data)
}

#' @export sorted
#'
#' @title accessor function to access sorted data
#' @description
#' This function allows the user to access sorted data table
#' @name sorted
#' @rdname sorted
#' @aliases sorted
#' @usage
#' sorted(MACSQuant)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @examples
#' filepath <- system.file("extdata", "SingleDrugs.xlsx",
#'     package = "MACSQuantifyR")
#' #MACSQuant = load_MACSQuant(filepath)
#'
#' #sorted(MACSQuant)
#' @return the raw data table

sorted = function(MACSQuant)
{
    return(MACSQuant@my_data_sorted)
}
