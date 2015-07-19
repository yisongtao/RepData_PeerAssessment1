for (i in 1:17568){
    if (as.integer(data[i,"interval"]) < 10) {
        data[i, "interval"] = paste ("000", as.character(data[i, "interval"]), sep="")
    }
    else if (as.integer(data[i,"interval"]) < 100) {
        data[i, "interval"] = paste ("00", as.character(data[i, "interval"]), sep="")
    }
    else if (as.integer(data[i,"interval"]) < 1000) {
        data[i, "interval"] = paste ("0", as.character(data[i, "interval"]), sep="")
    }
    else {
        data[i, "interval"] =as.character(data[i, "interval"])
    }
}
