"use strict"

var errorsDom = document.getElementById("errors");
var ccFileDom = document.getElementById("ccFile");

var margin = {top: 20, right: 20, bottom: 30, left: 50};

// Define the div for the tooltip
var divTooltip = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0).style("background","lightsteelblue")
    .style("padding","2px").style("border-radius","8px");

var svg = d3.select("svg"),
    g = svg.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var bisectTime = d3.bisector(function(d) { return d.time; }).right;

var renderData = function (data) {
    var width = svg.node().clientWidth - margin.left - margin.right,
	height = svg.node().clientHeight - margin.top - margin.bottom;

    g.selectAll("*").remove();

    var x = d3.scaleLinear().range([0, width]),
	y = d3.scaleLinear().range([height, 0]),
	z = d3.scaleOrdinal(d3.schemeCategory20);

    var max_cc = d3.max(data, function(d) { return d.max; });

    x.domain([0, d3.max(data, function(d) { return d.time; })]).nice();
    y.domain([0, d3.max(data, function(d) { return d.total; })]).nice();
    z.domain([0, max_cc]);

    var stack = d3.stack().keys(d3.range(max_cc))
	.value(function (d,v) {
	    var out = 0;
	    if (d.values[v]) out = v * d.values[v];
	    return out;});

    var area = d3.area().curve(d3.curveStepAfter)
        .x(function(d) { return x(d.data.time); })
        .y0(function(d) { return y(d[0]); })
        .y1(function(d) { return y(d[1]); });

    var layer = g.selectAll(".layer")
        .data(stack(data))
        .enter().append("path")
        .attr("class", "area")
        .style("fill", function(d) { return z(d.key); })
        .attr("d", area);

    layer.on("mouseover", function(d) {
	divTooltip.transition()
	    .duration(200)
	    .style("opacity", .9);
    })
        .on("mousemove", function(d) {
	    var tt = x.invert(d3.mouse(this)[0]),
	    nb = data[bisectTime(data,tt)].values[d.key];
	    divTooltip.html("t: "+tt.toFixed(2)+"<br/>nb: "+nb+"<br/>length: "+d.key)
		.style("left", (d3.event.pageX + 3) + "px")
		.style("top", (d3.event.pageY - 28) + "px");
	})
        .on("mouseout", function(d) {
	    divTooltip.transition()
	        .duration(500)
	        .style("opacity", 0);
	});

    g.append("g")
        .attr("class", "axis")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

    g.append("g")
        .attr("class", "axis")
        .call(d3.axisLeft(y));
}

var dealWithFiles = function (files) {
    if (files.length === 1) {
	var file = files[0], ka = new FileReader();
	ka.onloadend = function(e){
	    d3.request(e.target.result)
	        .mimeType("text/csv")
	        .response(function(xhr) {
		    return d3.csvParseRows(xhr.responseText, function(d, i) {
			var t = 0, m = 0;
			for (i = 0; i < d.length-1; ++i) {
			    t += i * d[i+1];
			    if (+ d[i+1] > 0) m = i;
			}
			return {
			    time: +d[0],
			    values: d.slice(1),
			    max: m,
			    total: t
			};
		    }); })
	        .get(function (error,data) {
		if (error) errorsDom.innerHTML = error;
		else {
		    errorsDom.innerHTML = null;
		    renderData(data);
		}
	    })
	}
	ka.readAsDataURL(file);
    }
}

ccFileDom.onchange = function() {dealWithFiles(ccFileDom.files)};
