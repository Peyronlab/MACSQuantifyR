#' @export load_MACSQuant
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom methods new
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @title Load xls file from maxQuant 96-well-plate device
#' @name load_MACSQuant
#' @rdname load_MACSQuant
#' @aliases load_MACSQuant
#' @usage
#' load_MACSQuant(filepath,sheet_name= NULL,MACSQuant.obj= NULL)
#' @param filepath path of the excel file
#' @param sheet_name Name of the sheet to load
#' (optional, first sheet is default)
#' @param MACSQuant.obj object of class MACSQuant
#' @examples
#' filepath <- system.file("extdata", "SingleDrugs.xlsx",
#'     package = "MACSQuantifyR")
#' # load_MACSQuant(filepath)
#' @return An object called MACSQuant of class MACSQuant containing
#' variable my_data that corresponds to the data of the excel file in R

# source("./R/function_toolbox.R")

load_MACSQuant <- function(filepath, sheet_name = NULL, MACSQuant.obj = NULL) {
    ext <- c("xls", "xlsx","htm")
    # check for correct extention type and file

    if (!file.exists(filepath)) {
        stop("File or variable not found")
    } else if (!file_ext(filepath) %in% ext) {
        stop("File compatibility error, try xls, xlsx or htm files")
    } else {
        my_data <- tryCatch(
            {
                message('ok')
                tmp_data=read_excel(filepath, sheet_name)
                tmp_data=as.data.frame(tmp_data)
            },
            error=function(cond) {
                file_htm=read_html(filepath)
                htm_data=html_table(file_htm,header=TRUE)
                names_col=names(htm_data[[1]])

                list_paths=unlist(htm_data[[1]][,1])

                tmp_data=data.frame(list_paths,
                                unlist(htm_data[[1]][,2]),
                                unlist(htm_data[[1]][,3]),
                                unlist(htm_data[[1]][,4]),
                                stringsAsFactors = FALSE)
                names(tmp_data)=names_col
                return(tmp_data)
            }
        )
        # data <- data.frame(my_data,stringsAsFactors = FALSE)
        # names(data)[1]="Full path"
        # names(data)[3]='Pct'
        # names(data)[4]='Count/mL'
        data=my_data

        blank_lines=which(data$WID=="---")
        if (length(blank_lines)>0)
        {
            data=data[-c(blank_lines),]
        }
        for (i in c(3,4)){

            data[,i]=gsub(",",".",data[,i])
            data[,i]=gsub("\\s","",data[,i])
            data[,i]=iconv( data[,i], 'utf-8', 'ascii', sub='')
            data[,i]=as.numeric(data[,i])
        }
        if (dim(data)[2] != 4) {
            # is the data a MQ file?
            stop(paste(
                "Check your data file, column missing.",
                "Columns shoud be 'Full path','WID','%-#','Count/mL'"
            ),
            sep = " "
            )
        }
        if (sum((names(data) == c("Full path", "WID",'%-#', "Count/mL"))) !=
            4) {
            stop(paste("Check your data file,",
                "column names shoud be 'Full path','WID','%-#','Count/mL'",
                sep = " "
            ))
        }
        message("--> Done: data loaded")
        message("--> Done: data stored in variable MACSQuant@my_data")
        message(paste("...You can now run",
            "on_plate_selection(MACSQuant,num_replicates,number_of_conditions)",
            "with your replicates and conditions numbers...",
            sep = " "
        ))
        if (is.null(MACSQuant.obj)) {
            MACSQuant <- new_class_MQ()
        } else {
            MACSQuant <- MACSQuant.obj
        }

        MACSQuant@my_data <- data

        # assign("my_data", data, envir = .GlobalEnv)
        # asssigning data to my_data
    }
    return(MACSQuant)
}

load_MACSQuant.internal <- function(filepath, sheet_name = NULL, MACSQuant.obj = NULL) {
  ext <- c("xls", "xlsx","htm")
  # check for correct extention type and file

  if (!file.exists(filepath)) {
    stop("File or variable not found")
  } else if (!file_ext(filepath) %in% ext) {
    stop("File compatibility error, try xls, xlsx or htm files")
  } else {
    my_data <- tryCatch(
      {
        message('ok')
        tmp_data=read_excel(filepath, sheet_name)
        tmp_data=as.data.frame(tmp_data)
      },
      error=function(cond) {
        file_htm=read_html(filepath)
        htm_data=html_table(file_htm,header=TRUE)
        names_col=names(htm_data[[1]])

        list_paths=unlist(htm_data[[1]][,1])

        tmp_data=data.frame(list_paths,
                            unlist(htm_data[[1]][,2]),
                            unlist(htm_data[[1]][,3]),
                            unlist(htm_data[[1]][,4]),
                            stringsAsFactors = FALSE)
        names(tmp_data)=names_col
        return(tmp_data)
      }
    )
    # data <- data.frame(my_data,stringsAsFactors = FALSE)
    # names(data)[1]="Full path"
    # names(data)[3]='Pct'
    # names(data)[4]='Count/mL'
    data=my_data

    blank_lines=which(data$WID=="---")
    if (length(blank_lines)>0)
    {
      data=data[-c(blank_lines),]
    }
    for (i in c(3,4)){

      data[,i]=gsub(",",".",data[,i])
      data[,i]=gsub("\\s","",data[,i])
      data[,i]=iconv( data[,i], 'utf-8', 'ascii', sub='')
      data[,i]=as.numeric(data[,i])
    }
    if (dim(data)[2] != 4) {
      # is the data a MQ file?
      stop(paste(
        "Check your data file, column missing.",
        "Columns shoud be 'Full path','WID','%-#','Count/mL'"
      ),
      sep = " "
      )
    }
    if (sum((names(data) == c("Full path", "WID",'%-#', "Count/mL"))) !=
        4) {
      stop(paste("Check your data file,",
                 "column names shoud be 'Full path','WID','%-#','Count/mL'",
                 sep = " "
      ))
    }
    message("--> Done: data loaded")
    message("--> Done: data stored in variable MACSQuant@my_data")
    message(paste("...You can now run",
                  "on_plate_selection(MACSQuant,num_replicates,number_of_conditions)",
                  "with your replicates and conditions numbers...",
                  sep = " "
    ))
    if (is.null(MACSQuant.obj)) {
      MACSQuant <- new_class_MQ()
    } else {
      MACSQuant <- MACSQuant.obj
    }

    MACSQuant@my_data <- data

    # assign("my_data", data, envir = .GlobalEnv)
    # asssigning data to my_data
  }
  return(MACSQuant)
}
