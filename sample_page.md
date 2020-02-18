West London, encompassing neighbourhoods like Notting Hill, Chelsea, Knightsbridge, and Kensington, is one of the most economically advantaged areas in the UK. However, within the boroughs of Hammersmith and Fulham, and Kensignton and Chelsea, there are also pockets of serious disadvantage - often times located only a few streets over from some of the richest postcodes in the country. To capture the diversity of this area, I've [mapped](http://bl.ocks.org/bsuthersan/cebdec7bcd4ecc606bcd849ff0f31b24) Income Deprivation Affecting Children (IDACI) deciles by West London neighbourhood. IDACI deciles relate to the numbers of children aged 16 and below who live in low-income households, and are nationally representative.

### Data processing in R

To begin, I downloaded the LSOA shapefiles for London from [here](https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london), and the IDACI data, which matches IDACI decile with LSOA, from [here](http://imd-by-postcode.opendatacommunities.org/).

First, the `london` shapefiles are read into the system using the `readOGR` function from the `rgdal` package. 

```python
london <- rgdal::readOGR("~/Downloads/statistical-gis-boundaries-london/MapInfo", layer = "LSOA_2011_London_gen_MHW", stringsAsFactors = FALSE)
```
Next, the IDACI data is read into the system. The relevant columns are LSOA code, and IDACI Decile, so we will select only these two columns for merging.

```python
IDACI <- read_csv("~/Downloads/IDACData.csv)

IDACI <- IDACI %>%
  dplyr::select(`LSOA code`, `IDACI Decile`)
```

Using the `merge` function from `sp` package, which allows you to merge dataframes with spatial data, join the IDACI data with the london shapefiles.

```python
london <-
 sp::merge(london, IDACI, by = ("`LSOA code`" = "LSOA11CD"))
```

Filter the shapefiles to include only the two boroughs of Hammersmith and Fulham, and Kensington and Chelsea.
 
```python
london <- london %>%
  dplyr::filter(LAD11NM=="Hammersmith and Fulham" | LAD11NM=="Kensington and Chelsea")
```

Finally, convert and save `london` to a GeoJSON file, suitable for mapping. For mapping purposes, we'll use the latest LSOAs, which are from 2011, and labelled "LSOA11CD" in the london shapefiles. The `convert_wgs84` function ensures that the input is converted to the standard GeoJSON reference sysem, For the purposes of mapping in Leaflet, you'll want to ensure that `convert_wgs84` is set to TRUE.

```python
london <- geojsonio::geojson_json(london, geometry = "polygon", convert_wgs84 = TRUE)
```

### Mapping using Leaflet

[Leaflet](https://leafletjs.com/) is an open-source Javascript library which allows you to quickly build and deploy interactive maps. Following the [tutorials](https://leafletjs.com/examples.html), I found it really easy to use and deploy, especially becuase the map can be built layer-by-layer. 

First, we set the map view by calling the coordinates that we want the map to centre on (51.5131224, -0.21997120000003179), as well as the default zoom (12.4, in this case).

```javascript
    var map = L.map('mapid2').
    setView([51.5131224, -0.21997120000003179], 12.4);
```
Then we add functions for setting the colour for the legend. Here, I've spit the data into quintiles. 

```javascript
function getColor(d) {
    return d > 80  ? '#99CC00' :
           d > 60 ? 'steelblue' :
           d > 40   ? 'purple' :
           d > 20   ? 'grey' :
           d > 0   ? 'black' :
                      '#99CC00';}
```
As well as functions to style each of the areas:

```javascript
function style(feature) {
return {
        fillColor: getColor(feature.properties.Decile),
        weight: 1,
        opacity: 1,
        color: 'grey',
        dashArray: '3',
        fillOpacity: 0.7};
```

And a function to style each of these areas, on mouse hover:


```javascript
function highlightFeature(e) {
		var layer = e.target;
		layer.setStyle({
			weight: 5,
			color: '#666',
			dashArray: '',
			fillOpacity: 0.7});
  ```
  
Once these functions are set we can call them, linking them with behaviour for mouseover and mouseout. I've also added a zoom for when a user clicks on a particular area.
  
  ```javascript
  	function resetHighlight(e) {
		geojson.resetStyle(e.target);
		info.update();}

	function zoomToFeature(e) {
		map.fitBounds(e.target.getBounds());}

	function onEachFeature(feature, layer) {
		layer.on({
			mouseover: highlightFeature,
			mouseout: resetHighlight,
			click: zoomToFeature
		});}
  ```

Next, we call the geojson file, which contains all the relevant data, and apply our new functions to this data.

```javascript
geojson = L.geoJson(Data, {
	style: style,
	onEachFeature: onEachFeature
	}).addTo(map);
```

And, for the final step, add the legend to the map.

```javascript
var legend = L.control({position: 'bottomright'});

	legend.onAdd = function (map) {

		var div = L.DomUtil.create('div', 'info legend'),
			grades = [0, 20, 40, 60, 80],
			labels = [],
			from, to;

		for (var i = 0; i < grades.length; i++) {
			from = grades[i];
			to = grades[i + 1];

			labels.push(
				'<i style="background:' + getColor(from + 1) + '"></i> ' +
				from + (to ? '&ndash;' + to : '+'));
		}

		div.innerHTML = labels.join('<br>');
		return div;
	};

	legend.addTo(map);
```
The [final map](http://bl.ocks.org/bsuthersan/cebdec7bcd4ecc606bcd849ff0f31b24) is clean and easy to use.
