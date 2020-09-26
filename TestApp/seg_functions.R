freq_range <- function(filetitle, min_time, max_time) {
    wav <- readWave(filetitle, from = min_time, to = max_time, units = "seconds")
    spec <- round(data.frame(spec(wav,dB = "max0", plot = FALSE)),3)%>% filter (y > -35)
    minfreq <- min(spec$x)
    maxfreq <- max(spec$x)
    maxamp <- max(spec$y)
    peakfreq <- as.numeric(spec %>% filter(y == maxamp) %>% select(x) %>% head(1))
    list <- c(minfreq, maxfreq, peakfreq)
    return(matrix(list))
}

segment_box <- function(df, input_cols){
    x_min <- (df %>% pull("Start (ms)"))/1000
    y_min <- df %>% pull("Min Frequency")
    x_max <- (df %>% pull("End (ms)"))/1000
    y_max <- df %>% pull("Max Frequency")
    for (i in 1:nrow(df)){
        rect(x_min[i], y_min[i], x_max[i], y_max[i], border = "red")
    }
    if (length(input_cols)) {
        seg <- df %>% filter(Segment %in% input_cols) 
        x_min <- (seg %>% pull("Start (ms)"))/1000
        y_min <- seg %>% pull("Min Frequency")
        x_max <- (seg %>% pull("End (ms)"))/1000
        y_max <- seg %>% pull("Max Frequency")
        for (i in 1:length(input_cols)){
            rect(x_min[i], y_min[i], x_max[i], y_max[i], border = "red", col = rgb(0,0,1.0,alpha=0.1))
        }
    }
    
}