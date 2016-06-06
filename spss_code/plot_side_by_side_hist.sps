* Encoding: UTF-8.
DATASET ACTIVATE DataSet3.
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=rating text_type MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: rating=col(source(s), name("rating"))
  DATA: text_type=col(source(s), name("text_type"), unit.category())
  COORD: transpose(mirror(rect(dim(1,2))))
  GUIDE: axis(dim(1), label("rating"))
  GUIDE: axis(dim(1), opposite(), label("rating"))
  GUIDE: axis(dim(2), label("Frequency"))
  GUIDE: axis(dim(3), label("text_type"), opposite(), gap(0px))
  GUIDE: legend(aesthetic(aesthetic.color), null())
  ELEMENT: interval(position(summary.count(bin.rect(rating*1*text_type))), 
    color.interior(text_type))
END GPL.

