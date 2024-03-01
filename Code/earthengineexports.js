// Google Earth Engine script for exporting images

// function to add NBR band
var addNBR = function(image) {
  var nbr = image.normalizedDifference(['B8', 'B12'])
    .rename('NBR')
    .multiply(1000)
    .int16();
  return image.addBands(nbr);
};

// function to add NDVI band
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4'])
    .rename('NDVI')
    .multiply(1000)
    .int16();
  return image.addBands(ndvi);
};

// load in datasets
var s2SrX = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED');
var s2CloudsX = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY');

// set parameters depending on what you wish to export
var START_DATE = ee.Date('2022-12-01');
var END_DATE = ee.Date('2023-03-01');
var START_M = 1;
var END_M = 12;
var MAX_CLOUD_PROBABILITY = 50;
var region = geometry;

// function to mask clouds
function maskClouds(img) {
  var clouds = ee.Image(img.get('cloud_mask')).select('probability');
  var isNotCloud = clouds.lt(MAX_CLOUD_PROBABILITY);
  return img.updateMask(isNotCloud);
}

// The masks for the 10m bands sometimes do not exclude bad data at
// scene edges, so we apply masks from the 20m and 60m bands as well.
// Example asset that needs this operation:
// COPERNICUS/S2_CLOUD_PROBABILITY/20190301T000239_20190301T000238_T55GDP
function maskEdges(s2_img) {
  return s2_img.updateMask(
      s2_img.select('B8A').mask().updateMask(s2_img.select('B9').mask()));
}

// Filter input collections by desired data range and region.
var criteria = ee.Filter.and(
    ee.Filter.bounds(region), ee.Filter.date(START_DATE, END_DATE), ee.Filter.calendarRange(START_M, END_M, 'month'));
var s2Sr = s2SrX.filter(criteria).map(maskEdges);
var s2Clouds = s2CloudsX.filter(criteria);

// Join S2 SR with cloud probability dataset to add cloud mask.
var s2SrWithCloudMask = ee.Join.saveFirst('cloud_mask').apply({
  primary: s2Sr,
  secondary: s2Clouds,
  condition:
      ee.Filter.equals({leftField: 'system:index', rightField: 'system:index'})
});

// Apply cloud mask
var s2CloudMasked = ee.ImageCollection(s2SrWithCloudMask).map(maskClouds);

// add NDVI band and select it
var ndviC = s2CloudMasked.map(addNDVI).select('NDVI');

// Create 10th percentile
var N10 = ndviC.reduce(ee.Reducer.percentile([10])).int16();

// add NBR band and select it
var nbrC = s2CloudMasked.map(addNBR).select('NBR');

// create mean and standard deviation rasters
var Nm = nbrC.reduce(ee.Reducer.mean()).int16();
var Nsd = nbrC.reduce(ee.Reducer.stdDev()).int16();

// use this for summer median after changing time periods above
var Nmedian = nbrC.median().int16();

// median across all bands
var median = s2CloudMasked.select('B2','B3','B4','B5','B6','B7','B8','B8A','B11','B12').median().int16();

// export images  
Export.image.toDrive({
    image: Nm,
    description: 'nbr_mean_2023_summer',
    folder: 'melb_water',
    scale: 10,
    fileFormat: 'GeoTIFF',
    crs: 'EPSG:28355', //wgs84 zone 55
    maxPixels: 1e10,
    region: geometry
  });
  
Export.image.toDrive({
    image: Nsd,
    description: 'nbr_sd_2019_2023_summer',
    folder: 'melb_water',
    scale: 10,
    fileFormat: 'GeoTIFF',
    crs: 'EPSG:28355', //wgs84 zone 55
    maxPixels: 1e10,
    region: geometry
  });
  
Export.image.toDrive({
    image: N10,
    description: 'ndvi_2020_21_p10_sr',
    folder: 'melb_water',
    scale: 10,
    fileFormat: 'GeoTIFF',
    crs: 'EPSG:28355',
    maxPixels: 1e10,
    region: geometry
  });
