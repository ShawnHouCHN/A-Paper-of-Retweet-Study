# convert felm result into a data frame with rhs | coef | se | xmin | xmax | xmid | ci.l | ci.h,
# replacing Inf and -Inf with in xmin and xmax on the edge bins with the graphically spaced bound
## felm.est: result from felm() call (lm will probably work too) with RHS factor variable like cuttmax or bins
## plotvar: string of variable to plot: e.g. 'cuttemp'
## breaks: bin size
## omit: omitted bin

binned.plot <- function(felm.est, plotvar, breaks, omit, noci=FALSE,
                        xlimit=c(min(dt$xmid), max(dt$xmid)), ylimit=c(min(dt$ci.l) - (max(dt$ci.h) - min(dt$ci.l))/10,max(dt$ci.h)),
                        panel="panel", panel.x=min(xlimit), panel.y=min(ylimit), panel.size=15,
                        hist.text.size=6, ylabel="y-label", xlabel="x=label",
                        roundx=5,
                        y.axis.theme=element_text(size=20, angle=0, face="bold"), y.axis.line=element_line(),
                        y.axis.ticks=element_line(),
                        linecolor='darkorchid4', errorfill='gray40', pointfill='gold', pointcolor='darkorchid4') {

    dt <- as.data.frame(summary(felm.est)$coefficients[, 1:2]) # extract from felm summary (use df to keep coefficient names)
    dt <- dt[grepl(plotvar, rownames(dt)), ] # extract variable name (e.g., tmax.cut, ppt.cut, ...), limit to matches
    dt$rhs <- str_split_fixed(rownames(dt), "   ", n=2)[,1]
    dt <- as.data.table(dt)
    dt[, rhs := gsub(paste0(plotvar), "", rhs)]
    dt[, rhs := gsub("\\(|]", "", rhs)]
    dt[, rhs := gsub(".neg.", "-", rhs)]
    dt[, rhs := gsub(".pos.", "", rhs)]
    dt[, rhs := gsub(".diff", "", rhs)]
    dt[grepl(",", rhs), rhs := tstrsplit(rhs, ',')[2]] # if a factor variable, grab second column (upper limit)
    dt[, rhs := as.numeric(rhs)]
    dt <- dt[!is.na(rhs)]
    dt[, xmin := rhs - breaks]
    dt[, xmax := rhs]
    dt[, rhs := NULL]
    setnames(dt, c("coef", "se", "xmin", "xmax"))
    dt[, xmin.lead := data.table::shift(xmin, type="lead")]
    dt[, xmax.lag := data.table::shift(xmax, type="lag")]
    dt[xmin==-Inf, xmin := xmin.lead - breaks]
    dt[xmax==-Inf, xmax := xmin.lead]
    dt[xmin==Inf, xmin := xmax.lag]
    dt[xmax==Inf, xmax := xmin + breaks]
    dt[, c('xmin.lead','xmax.lag') := NULL]
    # identify omitted category, add 0.
    dt <- rbind(dt, data.table(coef=0, se=0, xmin=omit[1], xmax=omit[2]))
    dt[, ':='(xmid=(xmin+xmax)/2, ci.l=coef - se*1.96, ci.h=coef + se*1.96)] # add some stuff for plotting
    dt[, range := paste(xmin, xmax, sep="-")] # create range variable for x labels
    dt[xmax == min(xmax), range := paste0("<=",min(xmax))] # set bottom of range
    dt[xmin == max(xmin), range := paste0(">",min(xmin))] # set top of range

    plot <- ggplot() +
        geom_hline(aes(yintercept=0), linetype=2, color='black') +
        geom_ribbon(data=dt, aes(x=xmid, ymin=ci.l, ymax=ci.h), fill=errorfill, alpha=0.25) +
        geom_line(data=dt, aes(x=xmid, y=ci.l), linetype="dotted", color=linecolor, size=0.75, alpha=1) +
        geom_line(data=dt, aes(x=xmid, y=ci.h), linetype="dotted", color=linecolor, size=0.75, alpha=1) +
        geom_line(data=dt, aes(x=xmid, y=coef), color=linecolor, size=2, alpha=1) +
        geom_point(data=dt, aes(x=xmid, y=coef), size=2, fill=linecolor, color='black', pch=21, alpha=1) +
        ylab(ylabel) +
        xlab(xlabel) +
        coord_cartesian(xlim=xlimit, ylim=ylimit, expand=TRUE) +
        scale_x_continuous(breaks = unique(round_any(dt$xmid, roundx)), labels = unique(round_any(dt$xmid, roundx))) +
        theme(plot.title = element_text(face="bold", size=40),
              axis.title.x = element_text(size=15, angle=0, face="bold", vjust=1.1),
              axis.title.y = element_text(size=15, angle=90, face="bold", vjust=-0),
              axis.text.y = y.axis.theme,
              axis.text.x = element_text(size=15, face="bold"),
              axis.ticks.y = y.axis.ticks,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(),
              axis.line.y = y.axis.line,
              legend.title=element_blank()
        )
    return(plot)
}
