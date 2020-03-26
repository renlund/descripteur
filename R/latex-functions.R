##' LaTeX code for a figure
##'
##' generate the LaTeX code needed to produce a figure, with caption and label. A
##'     caption and label will be generated if unspecified
##' @param path path to figure
##' @param caption caption of figure
##' @param label label of figure
##' @param cat concatenate and print the code, else return as character
##' @param lab.prefix prefix for label
##' @param where argument for placement of figure
##' @return printed coded or character
##' @export
latex_figure <- function(path, caption = NULL, label = NULL,
                        cat = TRUE, lab.prefix = "fig:", where = "htb"){
    if(is.null(label)){
        label <- paste0(sample(x = c(letters, LETTERS), size = 6),
                        collapse = "")
    }
    if(is.null(caption)){
        item <- gsub(pattern = "(.*/)([^\\.]*)((\\..*)?)",
                     replacement = "\\2", path)
        item <- gsub(pattern = "_", replacement = "\\_", item, fixed = TRUE)
        caption <- paste0("Plot of: ", item)
    }
    label <- if(!is.null(lab.prefix)) paste0(lab.prefix, label)
    r <- paste0("\n\\begin{center}\\begin{figure}[", where, "]\n",
                "\\includegraphics{", path, "}\n",
                "\\caption{", caption, "}\n",
                "\\label{", label, "}\n",
                "\\end{figure}\\end{center}\n")
    if(cat) cat(r) else r
}
