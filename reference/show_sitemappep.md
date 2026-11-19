# Map water quality data for a selected year

Map water quality data for a selected year

## Usage

``` r
show_sitemappep(
  dat,
  yrsel = NULL,
  mosel = NULL,
  param = c("chla", "sd", "tn"),
  bay_segment = c("1a", "1b", "2", "3"),
  maxrel = 0.99,
  relative = FALSE
)
```

## Arguments

- dat:

  data frame of data returned by
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

- yrsel:

  numeric for years to plot, see details

- mosel:

  numeric for months to plot, see details

- param:

  chr string indicating which water quality value to plot, one of "chla"
  for chlorophyll, "sd" for secchi depth, or "tn" for total nitrogen

- bay_segment:

  chr string for the bay segment, one or all of "1a", "1b", "2", or "3"

- maxrel:

  numeric for the maximum quantile value for scaling if `relative = T`,
  this prevents outliers from skewing the scale

- relative:

  logical indicating if sizes and colors are relative to the entire
  water quality data base, otherwise scaling is relative only for the
  points on the map

## Value

A [`leaflet`](https://rstudio.github.io/leaflet/reference/leaflet.html)
object

## Details

Year estimates for the selected parameter are based on median
observations across months. All twelve months are used if `mosel = NULL`
(default). Monthly estimates for the selected parameter are based on
median observations across years. All years are used if `yrsel = NULL`
(default).

The color ramp and size scaling of points are reversed for Secchi
observations.

Three ESRI basemaps can be selected from the layer control on the map:
World Gray (default), World Imagery, and World Topo Map.

## Examples

``` r

# 2018, all months
show_sitemappep(rawdat, yrsel = 2018)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldGrayCanvas",null,"World Gray",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"World Imagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,"World Topo Map",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addLayersControl","args":[["World Gray","World Imagery","World Topo Map"],[],{"collapsed":true,"autoZIndex":true,"position":"topright"}]},{"method":"addCircleMarkers","args":[[40.924067,40.921667,40.92935,40.913883,40.938333,40.922783,40.943617,40.936383,40.900217,40.911667,41.00195,41.008883,40.975833,40.942783,40.955267,41.027717,41.052717,41.059117,40.9941,40.99625,41.05,41.076383,41.082217,41.013333,41.045283,41.063583,41.0055,41.00778,41.106267,41.077983,41.010367,41.0275,41.1144,41.081383,41.13445,41.02055,41.098017,41.025,41.010283,41.064167],[-72.63225,-72.61945,-72.6199,-72.61499999999999,-72.618617,-72.587217,-72.57083299999999,-72.51221700000001,-72.45908300000001,-72.443617,-72.460283,-72.460283,-72.42945,-72.414717,-72.399483,-72.433367,-72.414,-72.414233,-72.367833,-72.349817,-72.376667,-72.40055,-72.4025,-72.34444999999999,-72.347217,-72.359317,-72.3074,-72.2955,-72.354867,-72.311617,-72.252517,-72.26000000000001,-72.317483,-72.27305,-72.271383,-72.18195,-72.203667,-72.139167,-72.05416700000001,-71.92778300000001],[12.0036866359447,9.481566820276498,9.775115207373272,8.85852534562212,17,9.014285714285716,8.421198156682028,4.994470046082949,5.881105990783411,5.71336405529954,7.013364055299538,8.073732718894011,6.048847926267282,5.497695852534562,6.264516129032258,4.389400921658986,7.606451612903227,9.30783410138249,4.946543778801844,5.976958525345623,5.958986175115207,4.95852534562212,6.456221198156681,5.71336405529954,5.288018433179723,5.994930875576037,5.186175115207373,5.413824884792627,5.473732718894009,4.934562211981567,4.455299539170507,4.443317972350231,5.821198156682028,4.143778801843318,5.833179723502305,4.251612903225807,4.652995391705069,4,4,4.503225806451614],null,null,{"interactive":true,"className":"","stroke":true,"color":"black","weight":1,"opacity":0.5,"fill":true,"fillColor":["#FED889","#E8F6EC","#EFF8DF","#D2ECF4","#A50026","#D9EFF6","#C1E3EF","#4266AD","#5B8EC1","#5586BD","#86BBD9","#B2DCEB","#6195C5","#4D7DB8","#699ECA","#39499E","#9FCFE4","#E3F4F3","#4164AC","#5F92C3","#5E91C3","#4164AC","#6FA7CE","#5586BD","#4574B4","#5F92C4","#446FB1","#4A7AB7","#4C7CB8","#4163AB","#3A4CA0","#3A4BA0","#598BC0","#343D98","#598BC0","#36429B","#3D56A5","#313695","#313695","#3B4EA1"],"fillOpacity":0.8},null,null,null,null,["Bay station  60250 ,  chl-a (ug/L)   7.86","Bay station  60240 ,  chl-a (ug/L)   5.76","Bay station  60230 ,  chl-a (ug/L)   6","Bay station  60210 ,  chl-a (ug/L)   5.23","Bay station  60220 ,  chl-a (ug/L)   12.03","Bay station  60170 ,  chl-a (ug/L)   5.36","Bay station  60101 ,  chl-a (ug/L)   4.87","Bay station  60130 ,  chl-a (ug/L)   2.01","Bay station  60290 ,  chl-a (ug/L)   2.75","Bay station  60148 ,  chl-a (ug/L)   2.61","Bay station  60102 ,  chl-a (ug/L)   3.69","Bay station  60103 ,  chl-a (ug/L)   4.58","Bay station  60113 ,  chl-a (ug/L)   2.89","Bay station  60104 ,  chl-a (ug/L)   2.43","Bay station  60300 ,  chl-a (ug/L)   3.07","Bay station  60105 ,  chl-a (ug/L)   1.5","Bay station  60106 ,  chl-a (ug/L)   4.19","Bay station  60107 ,  chl-a (ug/L)   5.61","Bay station  60310 ,  chl-a (ug/L)   1.97","Bay station  60320 ,  chl-a (ug/L)   2.83","Bay station  60114 ,  chl-a (ug/L)   2.82","Bay station  60109 ,  chl-a (ug/L)   1.98","Bay station  60340 ,  chl-a (ug/L)   3.23","Bay station  60121 ,  chl-a (ug/L)   2.61","Bay station  60124 ,  chl-a (ug/L)   2.26","Bay station  60119 ,  chl-a (ug/L)   2.84","Bay station  60127 ,  chl-a (ug/L)   2.17","Bay station  60126 ,  chl-a (ug/L)   2.36","Bay station  60111 ,  chl-a (ug/L)   2.41","Bay station  60122 ,  chl-a (ug/L)   1.96","Bay station  60131 ,  chl-a (ug/L)   1.56","Bay station  60118 ,  chl-a (ug/L)   1.55","Bay station  60115 ,  chl-a (ug/L)   2.7","Bay station  60116 ,  chl-a (ug/L)   1.3","Bay station  60330 ,  chl-a (ug/L)   2.71","Bay station  60132 ,  chl-a (ug/L)   1.39","Bay station  60137 ,  chl-a (ug/L)   1.73","Bay station  60133 ,  chl-a (ug/L)   1.18","Bay station  60134 ,  chl-a (ug/L)   1.18","Bay station  60135 ,  chl-a (ug/L)   1.6"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#313695 , #4266AC 7.55760368663595%, #95C7DF 25.9907834101383%, #EFF8DF 44.4239631336406%, #FFD282 62.8571428571429%, #F0663F 81.2903225806452%, #A60226 99.7235023041475%, #A50026 "],"labels":["2","4","6","8","10","12"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"bottomright","type":"numeric","title":"Median chl-a (ug/L)<\/br>2018, Jan-Dec","extra":{"p_1":0.07557603686635946,"p_n":0.9972350230414747},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[40.900217,41.13445],"lng":[-72.63225,-71.92778300000001]}},"evals":[],"jsHooks":[]}
# 2018, July only
show_sitemappep(rawdat, yrsel = 2018, mosel = 7)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldGrayCanvas",null,"World Gray",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"World Imagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,"World Topo Map",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addLayersControl","args":[["World Gray","World Imagery","World Topo Map"],[],{"collapsed":true,"autoZIndex":true,"position":"topright"}]},{"method":"addCircleMarkers","args":[[40.924067,40.921667,40.92935,40.913883,40.938333,40.922783,40.943617,40.936383,40.900217,40.911667,41.00195,41.008883,40.975833,41.052717,41.059117,40.99625,41.05,41.076383,41.082217,41.013333,41.045283,41.063583,41.13445,41.02055,41.025,41.010283,41.064167],[-72.63225,-72.61945,-72.6199,-72.61499999999999,-72.618617,-72.587217,-72.57083299999999,-72.51221700000001,-72.45908300000001,-72.443617,-72.460283,-72.460283,-72.42945,-72.414,-72.414233,-72.349817,-72.376667,-72.40055,-72.4025,-72.34444999999999,-72.347217,-72.359317,-72.271383,-72.18195,-72.139167,-72.05416700000001,-71.92778300000001],[11.29085673146148,9.241180705543556,11.68394528437725,10.85097192224622,17,8.988480921526278,7.107271418286537,6.049676025917926,6.817134629229662,4,8.164866810655148,8.810655147588193,7.013678905687545,7.519078473722102,9.166306695464362,10.49532037437005,7.322534197264218,5.796976241900648,6.012239020878329,8.904247660187185,7.593952483801296,11.86177105831533,5.179265658747299,8.417566594672426,6.536357091432685,5.843772498200144,6.068394528437725],null,null,{"interactive":true,"className":"","stroke":true,"color":"black","weight":1,"opacity":0.5,"fill":true,"fillColor":["#FFECA2","#E1F3F6","#FEE394","#FFF7B2","#A50026","#D8EFF6","#8ABEDA","#6195C5","#7EB4D5","#313695","#B6DEEC","#D0EBF4","#86BBD9","#9BCCE2","#DFF2F8","#FFFFBF","#93C5DE","#588ABF","#6093C4","#D4EDF5","#9ECEE3","#FEDE8E","#446FB1","#C0E3EF","#72AAD0","#5A8CC0","#6296C5"],"fillOpacity":0.8},null,null,null,null,["Bay station  60250 ,  chl-a (ug/L)   8.62","Bay station  60240 ,  chl-a (ug/L)   6.43","Bay station  60230 ,  chl-a (ug/L)   9.04","Bay station  60210 ,  chl-a (ug/L)   8.15","Bay station  60220 ,  chl-a (ug/L)   14.72","Bay station  60170 ,  chl-a (ug/L)   6.16","Bay station  60101 ,  chl-a (ug/L)   4.15","Bay station  60130 ,  chl-a (ug/L)   3.02","Bay station  60290 ,  chl-a (ug/L)   3.84","Bay station  60148 ,  chl-a (ug/L)   0.83","Bay station  60102 ,  chl-a (ug/L)   5.28","Bay station  60103 ,  chl-a (ug/L)   5.97","Bay station  60113 ,  chl-a (ug/L)   4.05","Bay station  60106 ,  chl-a (ug/L)   4.59","Bay station  60107 ,  chl-a (ug/L)   6.35","Bay station  60320 ,  chl-a (ug/L)   7.77","Bay station  60114 ,  chl-a (ug/L)   4.38","Bay station  60109 ,  chl-a (ug/L)   2.75","Bay station  60340 ,  chl-a (ug/L)   2.98","Bay station  60121 ,  chl-a (ug/L)   6.07","Bay station  60124 ,  chl-a (ug/L)   4.67","Bay station  60119 ,  chl-a (ug/L)   9.23","Bay station  60330 ,  chl-a (ug/L)   2.09","Bay station  60132 ,  chl-a (ug/L)   5.55","Bay station  60133 ,  chl-a (ug/L)   3.54","Bay station  60134 ,  chl-a (ug/L)   2.8","Bay station  60135 ,  chl-a (ug/L)   3.04"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#313695 , #436BAF 8.42332613390929%, #84B9D8 22.8221742260619%, #D2ECF4 37.2210223182145%, #FFFAB7 51.6198704103672%, #FEC274 66.0187185025198%, #F36B42 80.4175665946724%, #BF1D27 94.816414686825%, #A50026 "],"labels":["2","4","6","8","10","12","14"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"bottomright","type":"numeric","title":"Median chl-a (ug/L)<\/br>2018, Jul","extra":{"p_1":0.08423326133909287,"p_n":0.9481641468682505},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[40.900217,41.13445],"lng":[-72.63225,-71.92778300000001]}},"evals":[],"jsHooks":[]}
# July only, all years
show_sitemappep(rawdat, mosel = 7)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldGrayCanvas",null,"World Gray",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"World Imagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,"World Topo Map",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addLayersControl","args":[["World Gray","World Imagery","World Topo Map"],[],{"collapsed":true,"autoZIndex":true,"position":"topright"}]},{"method":"addCircleMarkers","args":[[40.917283,40.917,40.9176,40.91776,40.917933,40.924067,40.921667,40.92935,40.913883,40.938333,40.922783,40.943617,40.936383,40.900217,40.911667,41.00195,41.008883,40.975833,40.942783,40.955267,41.027717,41.052717,41.059117,40.9941,40.99625,41.05,41.076383,41.082217,41.013333,41.045283,41.063583,41.0055,41.00778,41.106267,41.077983,41.010367,41.0275,41.1144,41.081383,41.13445,41.02055,41.098017,41.025,41.010283,41.064167],[-72.655567,-72.65115,-72.645583,-72.63891,-72.631917,-72.63225,-72.61945,-72.6199,-72.61499999999999,-72.618617,-72.587217,-72.57083299999999,-72.51221700000001,-72.45908300000001,-72.443617,-72.460283,-72.460283,-72.42945,-72.414717,-72.399483,-72.433367,-72.414,-72.414233,-72.367833,-72.349817,-72.376667,-72.40055,-72.4025,-72.34444999999999,-72.347217,-72.359317,-72.3074,-72.2955,-72.354867,-72.311617,-72.252517,-72.26000000000001,-72.317483,-72.27305,-72.271383,-72.18195,-72.203667,-72.139167,-72.05416700000001,-71.92778300000001],[6.871129326047359,4.789313904068003,13.90095628415301,8.73095021250759,8.66188524590164,7.088190649666059,8.357670512042096,8.775349119611414,6.745727079538555,17,6.022616879174256,6.918488160291439,4.746148299939284,5.038737097753491,4.1474043715847,5.492296599878567,6.472624468731025,5.068139040680024,4.863312082574378,5.343412264723741,4.828779599271403,5.011308439587128,5.689723740133577,5.198770491803279,4.907316332726169,4.596918639951427,4.143063145112325,4.578271098967821,4.591985428051002,4.866272009714633,7.25961976320583,5.117668488160291,4.759714632665452,4.784380692167577,4.434122647237401,4.562386156648452,4.399392835458409,4.591985428051002,4,4.330722525804493,4.142865816636308,4.155396174863388,4.552519732847601,4.2565270188221,4.335458409228901],null,null,{"interactive":true,"className":"","stroke":true,"color":"black","weight":1,"opacity":0.5,"fill":true,"fillColor":["#80B6D6","#3F5CA8","#F8874E","#CDEAF3","#CAE8F2","#89BDDA","#BEE2EE","#CFEAF3","#7AB2D4","#A50026","#6094C4","#82B8D7","#3F5AA7","#4268AE","#343D99","#4D7DB8","#70A7CE","#436AAE","#4060AA","#4777B5","#405EA9","#4267AD","#5485BD","#4470B2","#4162AB","#3C53A3","#343D98","#3C52A3","#3C53A3","#4060AA","#90C3DD","#436CB0","#3F5BA7","#3F5CA8","#3A4B9F","#3C51A2","#39499F","#3C53A3","#313695","#38469D","#343D98","#343E99","#3C51A2","#36429B","#38469D"],"fillOpacity":0.8},null,null,null,null,["Bay station  60280 ,  chl-a (ug/L)   8.12","Bay station  60275 ,  chl-a (ug/L)   3.9","Bay station  60270 ,  chl-a (ug/L)   22.37","Bay station  60265 ,  chl-a (ug/L)   11.89","Bay station  60260 ,  chl-a (ug/L)   11.75","Bay station  60250 ,  chl-a (ug/L)   8.56","Bay station  60240 ,  chl-a (ug/L)   11.13","Bay station  60230 ,  chl-a (ug/L)   11.98","Bay station  60210 ,  chl-a (ug/L)   7.87","Bay station  60220 ,  chl-a (ug/L)   28.65","Bay station  60170 ,  chl-a (ug/L)   6.4","Bay station  60101 ,  chl-a (ug/L)   8.22","Bay station  60130 ,  chl-a (ug/L)   3.81","Bay station  60290 ,  chl-a (ug/L)   4.41","Bay station  60148 ,  chl-a (ug/L)   2.6","Bay station  60102 ,  chl-a (ug/L)   5.32","Bay station  60103 ,  chl-a (ug/L)   7.31","Bay station  60113 ,  chl-a (ug/L)   4.47","Bay station  60104 ,  chl-a (ug/L)   4.05","Bay station  60300 ,  chl-a (ug/L)   5.02","Bay station  60105 ,  chl-a (ug/L)   3.98","Bay station  60106 ,  chl-a (ug/L)   4.35","Bay station  60107 ,  chl-a (ug/L)   5.73","Bay station  60310 ,  chl-a (ug/L)   4.73","Bay station  60320 ,  chl-a (ug/L)   4.14","Bay station  60114 ,  chl-a (ug/L)   3.51","Bay station  60109 ,  chl-a (ug/L)   2.59","Bay station  60340 ,  chl-a (ug/L)   3.47","Bay station  60121 ,  chl-a (ug/L)   3.5","Bay station  60124 ,  chl-a (ug/L)   4.06","Bay station  60119 ,  chl-a (ug/L)   8.91","Bay station  60127 ,  chl-a (ug/L)   4.57","Bay station  60126 ,  chl-a (ug/L)   3.84","Bay station  60111 ,  chl-a (ug/L)   3.89","Bay station  60122 ,  chl-a (ug/L)   3.18","Bay station  60131 ,  chl-a (ug/L)   3.44","Bay station  60118 ,  chl-a (ug/L)   3.11","Bay station  60115 ,  chl-a (ug/L)   3.5","Bay station  60116 ,  chl-a (ug/L)   2.3","Bay station  60330 ,  chl-a (ug/L)   2.97","Bay station  60132 ,  chl-a (ug/L)   2.59","Bay station  60137 ,  chl-a (ug/L)   2.62","Bay station  60133 ,  chl-a (ug/L)   3.42","Bay station  60134 ,  chl-a (ug/L)   2.82","Bay station  60135 ,  chl-a (ug/L)   2.98"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#313695 , #4676B5 10.2459016393443%, #A7D5E7 29.2197935640559%, #FAFDCA 48.1936854887674%, #FEBC6E 67.1675774134791%, #E24A31 86.1414693381906%, #A50026 "],"labels":["5","10","15","20","25"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"bottomright","type":"numeric","title":"Median chl-a (ug/L)<\/br>1990-2024, Jul","extra":{"p_1":0.1024590163934426,"p_n":0.8614146933819065},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[40.900217,41.13445],"lng":[-72.655567,-71.92778300000001]}},"evals":[],"jsHooks":[]}
```
