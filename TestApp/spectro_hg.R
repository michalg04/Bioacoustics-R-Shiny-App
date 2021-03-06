spectro_hg <- function (wave, f, wl = 512, wn = "hanning", zp = 0, ovlp = 0, 
          complex = FALSE, norm = TRUE, correction = "none", fftw = FALSE, 
          dB = "max0", dBref = NULL, plot = TRUE, flog = FALSE, grid = TRUE, 
          osc = FALSE, scale = TRUE, cont = FALSE, collevels = NULL, 
          palette = spectro.colors, contlevels = NULL, colcont = "black", 
          colbg = "white", colgrid = "black", colaxis = "black", collab = "black", 
          cexlab = 1, cexaxis = 1, tlab = "Time (s)", flab = "Frequency (kHz)", 
          alab = "Amplitude", scalelab = "Amplitude\n(dB)", main = NULL, 
          scalefontlab = 1, scalecexlab = 0.75, axisX = TRUE, axisY = TRUE,
          tlim = NULL, trel = TRUE, flim = NULL, flimd = NULL, widths = c(6, 
                                                                          1), heights = c(3, 1), oma = rep(0, 4), listen = FALSE, 
          ...) 
{
    if (wl%%2 == 1) 
        stop("'wl' has to be an even number.")
    if (!is.null(dB) && all(dB != c("max0", "A", "B", "C", "D"))) 
        stop("'dB' has to be one of the following character strings: 'max0', 'A', 'B', 'C' or 'D'")
    if (complex) {
        if (plot) {
            plot <- FALSE
            warning("\n'plot' was turned to 'FALSE'")
        }
        if (norm) {
            norm <- FALSE
            warning("\n'norm' was turned to 'FALSE'")
        }
        if (!is.null(dB)) {
            dB <- NULL
            warning("\n'dB' was turned to 'NULL'")
        }
    }
    input <- inputw(wave = wave, f = f)
    if (!is.null(tlim) && trel && osc) {
        wave <- wave0 <- input$w
    }
    else {
        wave <- input$w
    }
    f <- input$f
    rm(input)
    if (!is.null(tlim)) 
        wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
    if (!is.null(flimd)) {
        mag <- round((f/2000)/(flimd[2] - flimd[1]))
        wl <- wl * mag
        if (ovlp == 0) 
            ovlp <- 100
        ovlp <- 100 - round(ovlp/mag)
        flim <- flimd
    }
    n <- nrow(wave)
    step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
    z <- stdft(wave = wave, f = f, wl = wl, zp = zp, step = step, 
               wn = wn, fftw = fftw, scale = norm, complex = complex, 
               correction = correction)
    if (!is.null(tlim) && trel) {
        X <- seq(tlim[1], tlim[2], length.out = length(step))
    }
    else {
        X <- seq(0, n/f, length.out = length(step))
    }
    xat <- xlabel <- pretty(X)
    if (is.null(flim)) {
        Y <- seq(0, (f/2) - (f/(wl + zp)), by = f/(wl + zp))/1000
    }
    else {
        fl1 <- flim[1] * nrow(z) * 2000/f
        fl2 <- flim[2] * nrow(z) * 2000/f
        z <- z[(fl1:fl2) + 1, ]
        Y <- seq(flim[1], flim[2], length.out = nrow(z))
    }
    yat <- ylabel <- pretty(Y)
    if (flog) {
        Y <- log(Y + 1)
        yat <- log(yat + 1)
    }
    if (!is.null(dB)) {
        if (is.null(dBref)) {
            z <- 20 * log10(z)
        }
        else {
            z <- 20 * log10(z/dBref)
        }
        if (dB != "max0") {
            if (dB == "A") 
                z <- dBweight(Y * 1000, dBref = z)$A
            if (dB == "B") 
                z <- dBweight(Y * 1000, dBref = z)$B
            if (dB == "C") 
                z <- dBweight(Y * 1000, dBref = z)$C
            if (dB == "D") 
                z <- dBweight(Y * 1000, dBref = z)$D
        }
    }
    Z <- t(z)
    if (plot) {
        if (!isTRUE(norm) && isTRUE(scale)) 
            stop("dB colour scale cannot be plot when 'norm' is FALSE")
        maxz <- round(max(z, na.rm = TRUE))
        if (!is.null(dB)) {
            if (is.null(collevels)) 
                collevels <- seq(maxz - 30, maxz, by = 1)
            if (is.null(contlevels)) 
                contlevels <- seq(maxz - 30, maxz, by = 10)
        }
        else {
            if (is.null(collevels)) 
                collevels <- seq(0, maxz, length = 30)
            if (is.null(contlevels)) 
                contlevels <- seq(0, maxz, length = 3)
        }
        Zlim <- range(Z, finite = TRUE, na.rm = TRUE)
        if (osc & scale) {
            layout(matrix(c(1, 3, 2, 0), ncol = 1, byrow = TRUE), 
                   widths = widths, heights = heights)
            par(las = 0, oma = oma, col = "white", col = colaxis, 
                col.lab = collab, cex.lab = cexlab, cex.axis = cexaxis, 
                bg = colbg)
            par(mar = c(0, 1, 4.5, 3))
            dBscale(collevels = collevels, palette = palette, 
                    fontlab = scalefontlab, cexlab = scalecexlab, 
                    collab = collab, textlab = scalelab, colaxis = colaxis)
            par(mar = c(5, 4.1, 0, 0))
            if (!is.null(tlim) && trel) {
                wave <- wave0
                from <- tlim[1]
                to <- tlim[2]
            }
            else {
                from <- FALSE
                to <- FALSE
            }
            soscillo(wave = wave, f = f, bty = "u", from = from, 
                     to = to, collab = collab, colaxis = colaxis, 
                     colline = colaxis, ylim = c(-max(abs(wave)), 
                                                 max(abs(wave))), tickup = max(abs(wave), na.rm = TRUE), 
                     tlab = tlab, alab = alab, cexlab = cexlab, cexaxis = cexaxis, 
                     xaxt = {
                         if (!axisX) {
                             "n"
                         }
                     }, ...)
            par(mar = c(0, 4.1, 1, 0), las = 1, cex.lab = cexlab + 
                    0.2)
            filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                  nlevels = 20, plot.title = title(main = main, 
                                                                   xlab = "", ylab = flab), plot.axes = {
                                                                       if (axisY) {
                                                                           axis(2, at = yat, labels = ylabel)
                                                                       }
                                                                       else {
                                                                           NULL
                                                                       }
                                                                   }, color.palette = palette)
            if (grid) 
                abline(h = yat, col = colgrid, lty = "dotted")
            if (cont) {
                contour(X, Y, Z, add = TRUE, levels = contlevels, 
                        nlevels = 5, col = colcont, ...)
            }
            if (colaxis != colgrid) 
                abline(h = 0, col = colaxis)
            else abline(h = 0, col = colgrid)
        }
        if (osc == FALSE & scale) {
            layout(matrix(c(1, 2), ncol = 1, byrow = TRUE), widths = widths, heights = heights)
            par(mar = c(0, 4.1, 4.5, 1), oma = oma, las = 0, bg = colbg)
            dBscale(collevels = collevels, palette = palette, 
                    fontlab = scalefontlab, cexlab = scalecexlab, 
                    collab = collab, textlab = scalelab, colaxis = colaxis, side = 3)
            par(mar = c(5, 4.1, 1, 1), las = 1, cex = 1, col = colaxis, 
                col.axis = colaxis, col.lab = collab, bg = colbg, 
                cex.lab = cexlab + 0.2)
            filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                  nlevels = 20, plot.title = title(main = main, 
                                                                   xlab = tlab, ylab = flab), plot.axes = {
                                                                       if (axisX) {
                                                                           axis(1, at = xat, labels = xlabel)
                                                                       }
                                                                       if (axisY) {
                                                                           axis(2, at = yat, labels = ylabel)
                                                                       }
                                                                   }, color.palette = palette)
            if (grid) 
                abline(h = yat, col = colgrid, lty = "dotted")
            if (colaxis != colgrid) 
                abline(h = 0, col = colaxis)
            else abline(h = 0, col = colgrid)
            if (cont) {
                contour(X, Y, Z, add = TRUE, levels = contlevels, 
                        nlevels = 5, col = colcont, ...)
            }
        }
        if (osc & scale == FALSE) {
            layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
            par(mar = c(5.1, 4.1, 0, 2.1), las = 0, oma = oma, 
                bg = colbg)
            if (!is.null(tlim) && trel) {
                wave <- wave0
                from <- tlim[1]
                to <- tlim[2]
            }
            else {
                from <- FALSE
                to <- FALSE
            }
            soscillo(wave = wave, f = f, bty = "u", from = from, 
                     to = to, collab = collab, colaxis = colaxis, 
                     colline = colaxis, tickup = max(abs(wave), na.rm = TRUE), 
                     ylim = c(-max(abs(wave)), max(abs(wave))), tlab = tlab, 
                     alab = alab, cexlab = cexlab, cexaxis = cexaxis, 
                     xaxt = {
                         if (!axisX) {
                             "n"
                         }
                     }, ...)
            par(mar = c(0, 4.1, 2.1, 2.1), las = 1, cex.lab = cexlab)
            filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                  nlevels = 20, plot.title = title(main = main, 
                                                                   xlab = "", ylab = flab), color.palette = palette, 
                                  plot.axes = {
                                      if (axisY) {
                                          axis(2, at = yat, labels = ylabel)
                                      }
                                      else {
                                          NULL
                                      }
                                  }, col.lab = collab, colaxis = colaxis, ...)
            if (grid) 
                abline(h = yat, col = colgrid, lty = "dotted")
            if (cont) {
                contour(X, Y, Z, add = TRUE, levels = contlevels, 
                        nlevels = 5, col = colcont, ...)
            }
            if (colaxis != colgrid) 
                abline(h = 0, col = colaxis)
            else abline(h = 0, col = colgrid)
        }
        if (osc == FALSE & scale == FALSE) {
            par(las = 1, col = colaxis, col.axis = colaxis, col.lab = collab, 
                bg = colbg, cex.axis = cexaxis, cex.lab = cexlab, 
                ...)
            filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                  nlevels = 20, plot.title = title(main = main, 
                                                                   xlab = tlab, ylab = flab), plot.axes = {
                                                                       if (axisX) {
                                                                           axis(1, at = xat, labels = xlabel)
                                                                       }
                                                                       if (axisY) {
                                                                           axis(2, at = yat, labels = ylabel)
                                                                       }
                                                                   }, color.palette = palette, col.lab = collab, 
                                  colaxis = colaxis)
            if (grid) 
                abline(h = yat, col = colgrid, lty = "dotted")
            if (cont) {
                contour(X, Y, Z, add = TRUE, levels = contlevels, 
                        nlevels = 5, col = colcont, ...)
            }
            if (colaxis != colgrid) 
                abline(h = 0, col = colaxis)
            else abline(h = 0, col = colgrid)
        }
        if (listen) {
            listen(wave, f = f)
        }
        invisible(list(time = X, freq = Y, amp = z))
    }
    else return(list(time = X, freq = Y, amp = z))
}



# #### dBscale Function Revised ####
# input <- inputw(wave = wave, f = f)
# if (!is.null(tlim) && trel && osc) {
#     wave <- wave0 <- input$w
# } else {
#     wave <- input$w
# }
# f <- input$f
# rm(input)
# if (!is.null(tlim)) 
#     wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
# if (!is.null(flimd)) {
#     mag <- round((f/2000)/(flimd[2] - flimd[1]))
#     wl <- wl * mag
#     if (ovlp == 0) 
#         ovlp <- 100
#     ovlp <- 100 - round(ovlp/mag)
#     flim <- flimd
# }
# n <- nrow(wave)
# step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
# z <- stdft(wave = wave, f = f, wl = wl, zp = zp, step = step, 
#            wn = wn, fftw = fftw, scale = norm, complex = complex, 
#            correction = correction)
# 
# z <- 20 * log10(z)
# collevels <- seq(maxz - 30, maxz, by = 1)
# 
# dBscale_hg <- function (collevels, palette = spectro.colors, side = 4, textlab = "Amplitude\n(dB)", 
#           cexlab = 0.75, fontlab = 1, collab = "black", colaxis = "black", 
#           ...) 
# {
#     plot.new()
#     levels <- collevels
#     col <- palette(length(collevels) - 1)
#     par(las = 1)
#     if (side == 2 | side == 4) {
#         plot.window(xlim = c(0, 1), ylim = range(collevels), 
#                     xaxs = "i", yaxs = "i")
#         mtext(textlab, side = 3, outer = FALSE, line = 1.5, adj = 0, 
#               font = fontlab, cex = cexlab, col = collab)
#         rect(xleft = 0, ybottom = levels[-length(levels)], xright = 0.95, 
#              ytop = levels[-1], col = col, lty = 0, border = TRUE)
#         segments(x0 = 0, y0 = max(collevels), x1 = 0.95, y1 = max(collevels), 
#                  col = colaxis)
#         segments(x0 = 0, y0 = min(collevels), x1 = 0.95, y1 = min(collevels), 
#                  col = colaxis)
#         abline(v = c(0, 0.95), col = colaxis)
#         if (side == 2) 
#             axis(2, col = colaxis, col.axis = colaxis, ...)
#         if (side == 4) 
#             axis(4, pos = 0.95, col = colaxis, col.axis = colaxis, 
#                  ...)
#     }
#     if (side == 1 | side == 3) {
#         plot.window(xlim = range(collevels), ylim = c(0, 1), 
#                     xaxs = "i", yaxs = "i")
#         mtext(textlab, side = 3, outer = FALSE, line = 1.5, adj = 0, 
#               font = fontlab, cex = cexlab, col = collab)
#         rect(xleft = levels[-length(levels)], ybottom = 0, xright = levels[-1], 
#              ytop = 0.95, col = col, lty = 0)
#         segments(x0 = min(collevels), y0 = 0, x1 = min(collevels), 
#                  y1 = 0.95, col = colaxis)
#         segments(x0 = max(collevels), y0 = 0, x1 = max(collevels), 
#                  y1 = 0.95, col = colaxis)
#         abline(h = c(0, 0.95), col = colaxis)
#         if (side == 1) 
#             axis(1, col = colaxis, col.axis = colaxis)
#         if (side == 3) 
#             axis(3, pos = 0.95, col = colaxis, col.axis = colaxis)
#     }
# }
