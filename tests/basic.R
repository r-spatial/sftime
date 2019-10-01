library(sftime)

sfc = st_sfc(st_point(c(0,1)), st_point(c(2,1)))
time = as.Date("2019-10-01") + 0:1
sf = st_sf(a = 1:2, time = time, geom = sfc)

sft = sf_time(sf, "time")
sft0 = sf_time(sf)
all.equal(sft, sft0)
class(sft)
sft
print(sft, n = 1)
