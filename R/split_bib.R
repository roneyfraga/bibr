#' Split fields and return a data frame
#' 
#' Split fields as Author Address, Authors, Keywords
#' and return a data frame.
#' 
#' 
#' @param \code{x} a vector with text
#' @param \code{by} the character used to separate the fields
#' @param \code{address} can be 'institution', 'country' or 'whole field'  
#' @return a data frame from the divided string 
#' @author Roney Fraga Souza, Winicius Sabino
#' @details 
#' Write the details about the functions here!
#' Can be in more that one line.
#' @examples
#' \dontrun{Split the author address from Web of Science (WoS)}
#' c1 <- split_bib(p$AuthorAddress, by=';', address='whole field')
# 
#' # get the country 
#' country <- split_bib(p$AuthorAddress, by=';', address='country')
#'
#' # get the institution of research of the authors
#' inst <- split_bib(p$AuthorAddress, by=';', address='institution')
#' @export

split_bib <- function(x, by=';', address=NULL){

    if(!is.vector(x)){
        stop('"x" is not a vector')
    }

    id <- 1:length(x)

    s <- strsplit(as.character(x), split = by)
    dt <- data.frame(id = rep(id, sapply(s, length)), field = unlist(s))

    if(!is.null(address)){
        if(address=='institution'){
            # remove everything between [ ]
            x1 <- gsub('\\[.*?\\]','',as.character(x))
            s <- strsplit(as.character(x1), split = by)
            # keep only the elements before the first comma
            s <- lapply(s, function(lista){gsub(',.*$','',lista)})
            # remove spaces before and after the string
            s <- lapply(s, function(lista){gsub('^[[:space:]]+|[[:space:]]+$','',lista)})
            dt <- data.frame(id = rep(id, sapply(s, length)), field = unlist(s))
        }
        if(address=='country'){
            # remove everything between [ ]
            x1 <- gsub('\\[.*?\\]','',as.character(x))
            s <- strsplit(as.character(x1), split = by)
            # remove everything until the last coma
            s <- lapply(s, function(lista){gsub('^.*(,)[^,]','',lista)})
            # remove spaces before and after the string
            s <- lapply(s, function(lista){gsub('^[[:space:]]+|[[:space:]]+$','',lista)})
            dt <- data.frame(id = rep(id, sapply(s, length)), field = unlist(s))
        }
        if(address=='whole field'){
            # keep the whole intact field
            s <- strsplit(as.character(x), split = '; \\[', perl=FALSE)
            dt <- data.frame(id = rep(id, sapply(s, length)), field = unlist(s))
        }
    } else { 
        stop('the option for "address" is not available')
    }
    return(dt)
}
