namespace BioFSharp.Vis

open System
open System.IO
open System.Reflection

module Chord =
    
    let private chord2 () =
        let assembly = Assembly.GetExecutingAssembly()
        let resourceName = "chord2.js"
        let stream = assembly.GetManifestResourceStream("BioFSharp.Vis.Resources." + resourceName)
        let reader = new System.IO.StreamReader(stream)
        reader.ReadToEnd()


//    let private chord2css () =
//        let assembly = Assembly.GetExecutingAssembly()
//        let resourceName = "chart.css"
//        let stream = assembly.GetManifestResourceStream(resourceName)
//        let reader = new System.IO.StreamReader(stream)
//        reader.ReadToEnd()


    let private chord2Scaffold data labels labelColors k width height =
        sprintf """<!DOCTYPE html>
                        <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=10" >
                        <style>

                        body {
                          font: 10px sans-serif;
                        }

                        .chord path {
                          fill-opacity: .67;
                          stroke: #000;
                          stroke-width: .2px;
                        }

                        </style>
                        <body>
                        <script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
                        <script> %s </script>
                        <script>

                        connections = %s 
                        labels = %s
                        labelColors = %s

                        var chord = d3.chord2()
                            .padding(.05)
                            .connections(connections);

                        var width = %i,
                            height = %i,
                            innerRadius = Math.min(width, height) * .30,
                            outerRadius = innerRadius * 1.1;

                        var fill = d3.scale.ordinal()
                            .domain(d3.range(4))
                            .range(labelColors);

                        var svg = d3.select("body").append("svg")
                            .attr("width", width)
                            .attr("height", height)
                          .append("g")
                            .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

                        svg.append("g").selectAll("path")
                            .data(chord.groups)
                          .enter().append("path")
                            .style("fill", function(d) { return fill(d.index); })
                            .style("stroke", function(d) { return fill(d.index); })
                            .attr("d", d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius))
                            .on("mouseover", fade(.1))
                            .on("mouseout", fade(1));

                        var ticks = svg.append("g").selectAll("g")
                            .data(chord.groups)
                          .enter().append("g").selectAll("g")
                            .data(groupTicks)
                          .enter().append("g")
                            .attr("transform", function(d) {
                              return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                                  + "translate(" + outerRadius + ",0)";
                            });

                        ticks.append("line")
                            .attr("x1", 1)
                            .attr("y1", 0)
                            .attr("x2", 5)
                            .attr("y2", 0)
                            .style("stroke", "#000");

                        ticks.append("text")
                            .attr("x", 8)
                            .attr("dy", ".35em")
                            .attr("transform", function(d) { return d.angle > Math.PI ? "rotate(180)translate(-16)" : null; })
                            .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
                            .text(function(d) { return d.label; });

                        var labels = svg.append("g").selectAll("g")
                            .data(chord.groups)
                          .enter().append("g").selectAll("g")
                            .data(groupLabels)
                          .enter().append("g")
                            .attr("transform", function(d) {
                              return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                                  + "translate(" + (outerRadius + 20) + ",0)";
                            });

                        labels.append("text")
                            .attr("x", 8)
                            .attr("dy", ".35em")
                            .attr("transform", function(d) { return d.angle > Math.PI ? "rotate(180)translate(-16)" : null; })
                            .style("fill", "#000")
                            .style("font-size", 20 )
                            .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
                            .text(function(d) { return d.label; });

                        svg.append("g")
                            .attr("class", "chord")
                          .selectAll("path")
                            .data(chord.chords)
                          .enter().append("path")
                            .attr("d", d3.svg.chord().radius(innerRadius))
                            .style("fill", function(d) { return fill(d.target.index); })
                            .style("opacity", 1);

                        // Returns an array of tick angles and labels, given a group.
                        function groupTicks(d) {
                          var k = (d.endAngle - d.startAngle) / d.value;
                          return d3.range(0, d.value, %i).map(function(v, i) {
                            return {
                              angle: v * k + d.startAngle,
                              label: i %s 5 ? null : v 
                            };
                          });
                        }

                            function groupLabels(d) {
                              return [ {angle: (d.startAngle + d.endAngle) / 2, label: labels[d.index]} ];
                            }

                            // Returns an event handler for fading a given chord group.
                            function fade(opacity) {
                              return function(g, i) {
                                svg.selectAll(".chord path")
                                    .filter(function(d) { return !((i + '') in d.groups); })
                                  .transition()
                                    .style("opacity", opacity);
                              };
                            }

                        </script>
                        </body>"""  (chord2 ())  data  labels labelColors width height k "%"


    //data labels labelColors
    let fromLabel (labels:array<'a>) = 
        let tmp =
            labels
            |> Array.mapi (fun i l -> sprintf "%i: %A" i l)
            |> String.concat ", "
        sprintf "{%s}" tmp

    let fromColorLabel (clabels:array<string>) = 
        let tmp =
            clabels
            |> Array.map (fun s -> sprintf "\"%s\"" s)            
            |> String.concat ", "
        sprintf "[%s]" tmp

    let show tickInterval width height labels clabels data =
        let labelString = fromLabel labels
        let colorLabelString = fromColorLabel clabels
        chord2Scaffold data labelString colorLabelString tickInterval width height
        |> ViewContainer.showHTMLWith (float width + 25.) (float height + 25.)


    let showInBrowser tickInterval width height labels clabels data =
        let labelString = fromLabel labels
        let colorLabelString = fromColorLabel clabels        
        
        let guid = System.Guid.NewGuid().ToString()
        let html = chord2Scaffold data labelString colorLabelString tickInterval width height
        let tempPath = Path.GetTempPath()
        let file = sprintf "%s.html" guid
        let path = Path.Combine(tempPath, file)
        File.WriteAllText(path, html)
        System.Diagnostics.Process.Start(path) |> ignore
        

    let saveHtmlAs path tickInterval width height labels clabels data =
        let labelString = fromLabel labels
        let colorLabelString = fromColorLabel clabels        
        let guid = System.Guid.NewGuid().ToString()
        let html = chord2Scaffold data labelString colorLabelString tickInterval width height
        File.WriteAllText(path, html)


