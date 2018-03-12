function render(fileName, className) {
    var margin = {top: 20, right: 20, bottom: 130, left: 50}
        , width = 960 - margin.left - margin.right
        , height = 300 - margin.top - margin.bottom;

    var xScale = d3.scaleTime().range([0, width]);
    var yScale = d3.scaleLinear().range([height, 0]);

    var line = d3.line().x(function (d) {
        return xScale(d.t);
    }).y(function (d) {
        return yScale(d.v);
    });


    d3.text(fileName, function (text) {
        var dataset = d3.csvParseRows(text).map(function (row) {
            return {t: +row[0], v: +row[1]};
        });

        xScale.domain(d3.extent(dataset, function (d) {
            return d.t;
        }));
        yScale.domain([0, d3.max(dataset, function (d) {
            return d.v;
        })]);

        var svg = d3.select(className).append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(xScale).tickFormat(d3.timeFormat("%Y-%m-%d %H:%M")))
            .selectAll("text")
            .style("text-anchor", "end")
            .attr("dx", "-.8em")
            .attr("dy", ".15em")
            .attr("transform", "rotate(-65)");

        svg.append("g")
            .attr("class", "y axis")
            .call(d3.axisLeft(yScale));

        svg.append("path")
            .datum(dataset)
            .attr("class", "line")
            .attr("d", line);

        svg.selectAll(".dot")
            .data(dataset)
            .enter().append("circle") // Uses the enter().append() method
            .attr("class", "dot") // Assign a class for styling
            .attr("cx", function(d) { return xScale(new Date(d.t)) })
            .attr("cy", function(d) { return yScale(d.v) })
            .attr("r", 5);

    });
};