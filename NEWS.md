# sftime (development version)

* Add methods to convert `sftime` objects from:
  + Objects from the `spatstat` package classes (`ppp`, `ppplist`, `psp`, `lpp`)
  + `sftrack` and `sftraj` objects from the `sftrack` package.

* Bug fix in `st_time<-.sftime`:  
  + Still contained references to the old `tc`class.
  + Did not allow to give the active time column a character vector as value.

# version 0.2-0

* initial CRAN submission
