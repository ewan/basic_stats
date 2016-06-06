* Encoding: UTF-8.

SET DECIMAL DOT
GET DATA  /TYPE=TXT
  /FILE="/Users/emd/Work/events/kent_stats_0616/data/text_ratings/text_ratings_simple.csv"
  /ENCODING='UTF-8'
  /DELCASE=LINE
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  rating F18.0
  text_type A6.
CACHE.
EXECUTE.
DATASET NAME DataSet3 WINDOW=FRONT.
