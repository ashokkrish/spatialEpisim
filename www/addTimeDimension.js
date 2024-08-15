function addTimeDimensionToLeaflet(map, data) {

    var uniqueDates = [...new Set(data.features.map(function(feature) {
        return feature.properties.time.split("T")[0];
    }))].sort();

    console.log("Unique Dates with Features: ", uniqueDates);

    var timeDimension = new L.TimeDimension({
        times: uniqueDates,
        currentTime: Date.parse(uniqueDates[0] + "T00:00:00Z"),
        period: "P1D" 
    });

    map.timeDimension = timeDimension; 

    var player = new L.TimeDimension.Player({
        transitionTime: 1000, 
        loop: false,
        startOver: true
    }, timeDimension);

    var timeDimensionControlOptions = {
        player: player,
        timeDimension: timeDimension,
        position: 'bottomleft',
        autoPlay: true,
        minSpeed: 10,
        maxSpeed: 30,
        timeSliderDragUpdate: true
    };
    
    var timeDimensionControl = new L.Control.TimeDimension(timeDimensionControlOptions);
    map.addControl(timeDimensionControl);

    var circlesData = data;
    console.log(circlesData);

    // Custom layer for accumulating circles
    L.TimeDimension.Layer.AccumulatingCircleLayer = L.TimeDimension.Layer.GeoJson.extend({
        _getFeatureTimes: function(feature) {
            if (feature.properties && feature.properties.time) {
                return [feature.properties.time];
            }
            return [];
        },

        _updateTimeDimension: true,

        _getFeatureBetweenDates: function(feature, minTime, maxTime) {
            var featureTime = new Date(feature.properties.time);
            if (featureTime >= minTime && featureTime <= maxTime) {
                return feature;
            }
            return null;
        }
    });

    L.timeDimension.layer.accumulatingCircleLayer = function(layer, options) {
        return new L.TimeDimension.Layer.AccumulatingCircleLayer(layer, options);
    };

    var geoJsonLayer = L.geoJson(circlesData, {
        pointToLayer: function (feature, latlng) {
            var circle = L.circleMarker(latlng, {
                radius: 5,
                fillColor: feature.properties.color,
                color: "#000",
                weight: 0,
                opacity: 1,
                fillOpacity: 0.6
            });

            circle.bindTooltip(feature.properties.label, {
                permanent: false,
                direction: 'top'
            });

            return circle;
        }
    });

    var timeLayer = L.timeDimension.layer.accumulatingCircleLayer(geoJsonLayer, {
        updateTimeDimension: true,
        duration: 'P2D', 
    });

    timeLayer.addTo(map);
}
