
if ("PIA_plots.RData" %in% list.files()){
    load("PIA_plots.RData")
} else {
    source("plots.R")
}

fl_name_gen <- function(x){
    tmp <- gsub("\\.","_",x)
    tmp <- gsub("wd","wind",tmp)
    tmp <- gsub("td","tide",tmp)
    tmp <- gsub("pr","pressure",tmp)
    tmp <- gsub("SLP","sea-level pressure",tmp)
    tmp <- gsub("STP","station pressure",tmp)
    tmp <- gsub("lf","low_freq",tmp)
    tmp <- gsub("^[t|p]_","",tmp)
    tmp <- gsub("(clean)|(hr)","hourly",tmp)
    tmp <- gsub("dy","daily",tmp)
    tmp <- gsub("dist","distribution",tmp)
    paste0(tmp,".png")
}

png_plot <- function(x){
    # doesn't matter if the input is a character vector or an object
    if (is.character(x)){
        tmp.txt <- x
        tmp.obj <- get(x)
    }else{
        tmp.txt <- deparse(substitute(x))
        tmp.obj <- x
    }
    fl.name=fl_name_gen(tmp.txt)
    png(filename =paste0("output/", fl.name),width = 12,height = 8,units="in",res=100)
    plot.new()
    plot(tmp.obj)
    par(omi=c(0,0,1,0),oma=c(0,0,1,0),cex.main=1)
    title(main=fl.name,outer = T,font.main=4,col.main="blue")
    dev.off()
}

plots=grep("(^p\\.)",ls(),value=T)
lapply(plots,png_plot)

graphics.off()

