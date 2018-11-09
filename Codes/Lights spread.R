
len <- 60
wid <- 40

lightspread<-9

qtylen <- round(len/lightspread, digits=0)
qtywid <- round(wid/lightspread, digits=0)

# qtylen
# qtywid

corn <- lightspread/2

difflen = len-(corn+(qtylen-1)*lightspread)
startlen = (difflen+corn)/2

diffwid = wid - (corn+(qtywid-1)*lightspread)
startwid = (diffwid+corn)/2

lenvec = c(startlen, startlen + 1:(qtylen-1)*lightspread)
# lenvec

widvec = c(startwid, startwid + 1:(qtywid-1)*lightspread)
# widvec

xyg <- expand.grid(x=lenvec, y=widvec, KEEP.OUT.ATTRS = FALSE)
# xyg

plot(xyg, xlim = c(0,len), ylim = c(0,wid))

totlights = qtylen*qtywid

title(main=c("Total Lights = ", totlights),col.main="red", font.main=3)
title(xlab="Length")
title(ylab="Width")
totlights
startlen
startwid
lightspread

