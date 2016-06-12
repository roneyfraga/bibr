#' Read bibliographic data
#' 
#' Read data from Web of Science 'WoS' and Scopus. It's possible read one o more
#' files and set the colunm names.
#' 
#' @param database \code{WoS} or \code{Scopus} data base.
#' @param extension \code{txt} or \code{csv} extension.
#' @param directory the path to find the files, if the files are in a diferrent directory.
#' @param col.names logical, if TRUE return optional name of column to the data.
#' @param col.names.style \code{uppercase}, \code{lower} and \code{good4read}. 
#' @return a data frame with the data imported.
#' @author Roney Fraga Souza, Winicius Sabino
#' @details 
#' Write the details about the functions here!
#' Can be in more that one line.
#' @seealso \code{read.table}.
#' @examples
#' \dontrun{Import data from Web of Science (WoS)}
#' p <- read_bib(
#'              database='WoS', 
#'              extension='txt',
#'              directory='/Users/roney/data/WoS tab-delimited Windows',
#'              col.names=TRUE,
#'              col.names.style='good4read'
#'              )
#'names(p);dim(p)
#' \dontrun{or for a scopus csv data}
#' p <- read_bib( database='Scopus', extension='csv', col.names=TRUE, col.names.style='good4read')
#'names(p);dim(p)
#' @export

read_bib <- function(
                     database = NULL, 
                     extension = NULL,
                     directory = NULL, 
                     col.names = FALSE,
                     col.names.style = NULL,
                     fileEncoding = 'utf-8'
                     ){

    if(is.null(database))
        stop('there is no defined data base')

    if(is.null(extension))
        stop('there is no defined extension')

    options(warn=-1) 

    getwd()
    pasta.atual <- getwd()


    # list the files
    if (is.null(directory)) {
        ext <- paste0('+.*',extension)
        filenames <- list.files(pattern=ext)
    } else {
        setwd(directory)
        ext <- paste0('+.*',extension)
        filenames <- list.files(pattern=ext)
    }

    if(length(filenames) == 0L)
        stop('no file was found')


    # import the files
    if(database=='WoS' & extension=='txt'){
        paper <- do.call("rbind",
                         lapply(filenames, 
                                function(x) read.delim(x,quote = '',  nrows = FALSE, 
                                                       stringsAsFactors=FALSE, fileEncoding = fileEncoding 
                                                       )))
    }

    if(database=='Scopus' & extension=='csv'){
        paper <- do.call("rbind",
                         lapply(filenames, 
                                function(x) read.csv(x, stringsAsFactors=FALSE)))
    }

    setwd(pasta.atual)


    # change colunm names if WoS
    if(database=='WoS' & col.names==TRUE){
        code.name  <- data.frame( name = c("PublicationType","Authors","BookAuthors","Editors","BookGroupAuthors","AuthorFullName","BF","GroupAuthors",
                                           "DocumentTitle","PublicationName",
                                           "BookSerieTitle","BookSeriesSubtitle","Language","DocumentType","ConferenceTitle","ConferenceDate",
                                           "ConferenceLocation","ConferenceSponsors","ConferenceHost","AuthorKeywords","KeywordsPlus","Abstract",
                                           "AuthorAddress","ReprintAddress","EmailAddress","RI","OI","FundingAgencyandGrantNumber","FundingText",
                                           "CitedReferences","CitedReferenceCount","TimesCited","Z9","U1","U2","Publisher","PublisherCity",
                                           "PublisherAddresss","ISSN","EI","ISBN","29-CharacterSourceAbbreviation","ISOsourceAbbreviation",
                                           "PublicationDate","YearPublished","Volume","Issue","PartNumber","Supplement","SpecialIssue","MA","BeginningPage",
                                           "EndingPage","ArticleNumber","DigitalObjectIdentifier","D2","PageCount","WebOfScienceCategories","ResearchAreas",
                                           "DocumentDeliveryNumber","UniqueArticleIdentifier","PM"),
                                 code = c("PT", "AU", "BA", "BE", "GP", "AF", "BF", "CA", "TI", "SO", "SE", "BS", "LA", "DT", "CT", "CY", "CL", "SP", "HO",
                                          "DE", "ID", "AB", "C1", "RP", "EM", "RI", "OI", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "PI", "PA",
                                          "SN", "EI", "BN", "J9", "JI", "PD", "PY", "VL", "IS", "PN", "SU", "SI", "MA", "BP", "EP", "AR", "DI", "D2", "PG",
                                          "WC", "SC", "GA", "UT", "PM")
                                 )

        code.name$name <- as.character(code.name$name)
        code.name$code <- as.character(code.name$code)

        orig.table <- data.frame(code=names(paper), one=1)
        orig.table$code <- as.character(orig.table$code)

        orig.names.full <- merge(orig.table, code.name, by='code', all.x=TRUE, all.y=FALSE, sort = FALSE)

        names(paper) <- as.character(orig.names.full$name)

        # change the style of the column names
        if(!is.null(col.names.style)){
            if(col.names.style=='upper'){
                names(paper) <- toupper(names(paper))
            }
            if(col.names.style=='tolower'){
                names(paper) <- tolower(names(paper)) 
            }
            if(col.names.style=='good4read')
                names(paper) <- as.character(orig.names.full$name)
        } else {   
            stop('pattern incorrec to col.names.style')
        }
    }


    # change colunm names if Scopus
    if(database=='Scopus' & col.names==TRUE){
        sn <- data.frame(name= c("Authors" ,"Title" ,"Year" ,"SourceTitle" ,"Volume" ,"Issue" ,"ArtNo" ,"PageStart" ,"PagEnd" ,"PageCount" ,
                                 "CitedBy" ,"DOI" ,"Link" ,"Affiliations" ,"AuthorsWithAffiliations" ,"Abstract" ,"AuthorKeywords" ,
                                 "IndexKeywords" ,"MolecularSequenceNumbers" ,"ChemicalsCAS" ,"Tradenames" ,"Manufacturers" ,"FundingDetails" ,
                                 "References" ,"CorrespondenceAddress" ,"Editors" ,"Sponsors" ,"Publisher" ,"ConferenceName" ,"ConferenceDate" ,
                                 "ConferenceLocation" ,"ConferenceCode" ,"ISSN" ,"ISBN" ,"CODEN" ,"PubMedID" ,"LanguageOfOriginalDocument" ,
                                 "AbbreviatedSourceTitle" ,"DocumentType" ,"Source" ,"EID"),
                         code = c("Authors" ,"Title" ,"Year" ,"Source.title" ,"Volume" ,"Issue" ,"Art..No." ,"Page.start" ,"Page.end" ,"Page.count" ,
                                  "Cited.by" ,"DOI" ,"Link" ,"Affiliations" ,"Authors.with.affiliations" ,"Abstract" ,"Author.Keywords" ,"Index.Keywords" ,
                                  "Molecular.Sequence.Numbers" ,"Chemicals.CAS" ,"Tradenames" ,"Manufacturers" ,"Funding.Details" ,"References" ,"Correspondence.Address" ,
                                  "Editors" ,"Sponsors" ,"Publisher" ,"Conference.name" ,"Conference.date" ,"Conference.location" ,"Conference.code" ,"ISSN" ,"ISBN" ,"CODEN"
                                  ,"PubMed.ID" ,"Language.of.Original.Document" ,"Abbreviated.Source.Title" ,"Document.Type" ,"Source" ,"EID"))

        sn$name <- as.character(sn$name)
        sn$code <- as.character(sn$code)

        orig.table <- data.frame(code=names(paper), one=1)
        orig.table$code <- as.character(orig.table$code)

        orig.names.full <- merge(orig.table, sn, by='code', all.x=TRUE, all.y=FALSE, sort = FALSE)

        names(paper) <- as.character(orig.names.full$name)

        # change the style of the column names
        if(!is.null(col.names.style)){
            if(col.names.style=='upper'){
                names(paper) <- toupper(names(paper))
            }
            if(col.names.style=='tolower'){
                names(paper) <- tolower(names(paper)) 
            }
            if(col.names.style=='good4read')
                names(paper) <- as.character(orig.names.full$name)
        } else {   
            stop('pattern incorrec to col.names.style')
        }
    }

    return(paper)
}