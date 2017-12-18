(function () {

d3.chord2 = function() {
    var chord = {},
        chords,
        groups,
        connections,
        padding = 0,
        sortGroups,
        sortSubgroups,
        sortChords,
        width = 960,
        height = 900,
        innerRadius = Math.min(width, height) * .30,
        outerRadius = innerRadius * 1.1,
        fontsize = 15,
        colorschemeArcs = ["#d9d9d9"]

    function chord2(selection){
        selection.each(function (d, i) {
            var selection = d3.select(this);

            connections = d.connections;

            selection.append("g")
                .attr("class", "arc")
                .selectAll("path")
                .data(chord2.groups)
              .enter().append("path")
                .style("fill", 
                       function(d) {
                           return fillArcs(colorschemeArcs)(d.index %
                                                            colorschemeArcs.length);
                       })
                .attr("d", d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius))
                .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
                .on("mouseover", fade(selection, .1))
                .on("mouseout", fade(selection, 1));

            selection.append("g")
                .attr("class", "chord")
                .selectAll("path")
                .data(chord2.chords)
              .enter().append("path")
                .attr("d", d3.svg.chord().radius(innerRadius))
                .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
                .style("fill", 
                       function(d) {
                           return fillChords(d.polygon %
                                             fillChords.range().length);
                       })
                .style("opacity", 1)
                .style("fill-opacity", .67)
                .style("stroke", "#000")
                .style("stroke-width", ".5px");

            var labels = d.labels || (d3.range(chord2.groups().length)
                                      .map(function(n) {return "group" + n;}));
            var groupLabels = function (d) {
                return {angle: (d.startAngle + d.endAngle) / 2,
                        label: labels[d.index]};
            };

            selection.append("g").selectAll("g")
                .data(chord2.groups().map(groupLabels))
              .enter()
                .append("g")
                .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
                .append("g")
                .attr("transform", function(d) {
                    return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                        + "translate(" + (outerRadius + 5) + ",0)";})
                .append("text")
                .attr("x", 8)
                .attr("dy", ".35em")
                .attr("transform",
                      function(d) { return d.angle > Math.PI ? 
                                    "rotate(180)translate(-16)" : null; })
                .style("fill", "#000")
                .style("font-size", fontsize )
                .style("text-anchor",
                       function(d) { return d.angle > Math.PI ? 
                                     "end" : null; })
                .text(function(d) { return d.label; });

        });
    };

    function fade(selection, opacity) {
        return function(g, i) {
            selection.selectAll(".chord path")
                .filter(function(d) { return !((i + '') in d.groups); })
                .transition()
                .style("opacity", opacity);
        };
    }

    function relayout() {
        var subgroups     = [],
            groupSums     = {},
            subgroupIndex = [],
	    polygons      = [],
	    poly          = {edges:    [],
		             vertices: {}},
            samebase      = [],
            ngroups       = 0,
            groupIndex,
            pt1, pt2,
            pt, ep,
            k, x, x0, i, j, h;

        chords = [];
        groups = [];

        k = 0, i = -1; while (++i < connections.length) {
            j = -1; while (++j < connections[i].length) {
	        ep = connections[i][j].group;
	        if (ep in groupSums) {
	            groupSums[ep] += connections[i][j].value;
	        } else {
	            groupSums[ep] = connections[i][j].value;
	            ++ngroups;
	        }
	        k += connections[i][j].value;
            }
        }
        
        groupIndex = d3.range(ngroups);
      
        if (sortGroups) {
            groupIndex.sort(function(a, b) {
                return sortGroups(groupSums[a], groupSums[b]);
            });
        }

        k = (2 * Math.PI - padding * ngroups) / k;
        
        i = -1; while (++i < connections.length) {
            // If the current connections element is a singleton,
            // skip this altogether: no chord, it's empty space in the segment.
            if (connections[i].length > 1) {
	        j = 0; while (++j < connections[i].length) {
	            // the polygon keeps track of links which share groups.
	            poly.edges.push({
			source: connections[i][j-1],
			target: connections[i][j]
		    });
	            // the only purpose of poly.vertices is later lookup,
	            // so I use it as a set. I convert vertices ID to string with + ''
	            poly.vertices[connections[i][j-1].group + ''] = '';
	        }
	        poly.vertices[connections[i][j-1].group + ''] = '';
	        // close the polygon, unless it has only one side.
	        if (poly.edges.length > 1) {
	            poly.edges.push({
        		source: connections[i][0],
			target: connections[i][j-1]
		    });
	        }
	        polygons.push(poly);
	        poly = {edges: [],
		        vertices: {}};
            }
        };
        
        i = -1; while (++i < ngroups) {
            subgroups[i] = [];
            j = -1; while (++j < polygons.length) {
	        samebase = {'ribbons': [],
		            'basevalue': 0};
                h = -1; while (++h < polygons[j].edges.length) {
	            if (polygons[j].edges[h].source.group === i) {
	                samebase.ribbons.push(polygons[j].edges[h]);
	                samebase.basevalue = polygons[j].edges[h].source.value;
	            } else if (polygons[j].edges[h].target.group === i) {
	                samebase.ribbons.push(polygons[j].edges[h]);
	                samebase.basevalue = polygons[j].edges[h].target.value;
	            }
	        }
	        subgroups[i].push(samebase);
            }
        }

        // Now I handle the empty spaces, i.e. singletons in connections
        i = -1; while (++i < connections.length) {
            if (connections[i].length === 1) {
	        subgroups[connections[i][0].group]
                    .push({'ribbons': [],
		           'basevalue': connections[i][0].value
		          });
            }
        }

        // last pass on subgroups to create indices
        i = -1; while (++i < ngroups) {
            subgroupIndex.push(d3.range(subgroups[i].length));
        }

        // Sort subgroups…
        if (sortSubgroups) {
            subgroupIndex.forEach(function(d, i) {
                d.sort(function(a, b) {
                    return sortSubgroups(subgroups[i][a].basevalue,
			                 subgroups[i][b].basevalue);
                });
            });
        }

        x = 0, i = -1; while (++i < ngroups) {
            var di = groupIndex[i];
            x0 = x, j = -1; while (++j < subgroupIndex[di].length) {
                var dj = subgroupIndex[di][j],
                    // take numerical ID as subgroup key
	            v = subgroups[di][dj].basevalue,
	            a0 = x,
                    a1 = x += v * k;
                // here you should directly modify the "edges",
	        // then access them back via polygons
	        // I now extend polygons elements with new properties.
	        h = -1; while(++h < subgroups[di][dj].ribbons.length) {
	            // pick the right end of the edge to be augmented
	            pt1 = subgroups[di][dj].ribbons[h].source;
	            pt2 = subgroups[di][dj].ribbons[h].target;
	            if (pt1.group === di) { pt = pt1; }
	            else { pt = pt2; }
	            // Only one of the two groups per iteration
	            // is augmented with the 'geometry' property.
	            // I will read this object back from the 'polygons' object.
	            pt['geometry'] = {
	                index: di,
	                subindex: dj,
	                startAngle: a0,
	                endAngle: a1,
	                value: v
                    };
	        }
            }
            groups[di] = {
	        index: di,
                startAngle: x0,
                endAngle: x,
                value: (x - x0) / k
            };
            x += padding;
        }

        // Generate chords for each (non-empty) subgroup-subgroup link.
        i = -1; while (++i < polygons.length) {
            j = -1; while (++j < polygons[i].edges.length) {
                var source = polygons[i].edges[j].source.geometry,
                    target = polygons[i].edges[j].target.geometry;
                if (source.value || target.value) {
                    chords.push(source.value < target.value
                                ? {source: target, 
                                   target: source, 
                                   groups: polygons[i].vertices,
                                   polygon: i}
                                : {source: source, 
                                   target: target, 
                                   groups: polygons[i].vertices,
                                   polygon: i});
                }
            }
        }

        if (sortChords) resort();
    }

    function resort() {
        chords.sort(function(a, b) {
            return sortChords(
                (a.source.value + a.target.value) / 2,
                (b.source.value + b.target.value) / 2);
        });
    }

    var fillArcs = function(x) {
        return d3.scale.ordinal()
            .domain(d3.range(x.length))
            .range(x);
    };

    var fillChords = d3.scale.category20();

    chord2.colorschemeArcs = function(x) {
        if (!arguments.length) return colorschemeArcs;
        colorschemeArcs = x;
        chords = groups = null;
        return chord2;
    };
    
    chord2.width = function(x) {
        if (!arguments.length) return width;
        width = x;
        innerRadius = Math.min(width, height) * .30;
        outerRadius = innerRadius * 1.1;
        chords = groups = null;
        return chord2;
    };

    chord2.height = function(x) {
        if (!arguments.length) return height;
        height = x;
        innerRadius = Math.min(width, height) * .30;
        outerRadius = innerRadius * 1.1;
        chords = groups = null;
        return chord2;
    };

    chord2.fontsize = function(x) {
        if (!arguments.length) return fontsize;
        fontsize = x;
        return chord2;
    };

    chord2.innerRadius = function(x) {
        if (!arguments.length) return innerRadius;
        innerRadius = x;
        outerRadius = innerRadius * 1.1,
        chords = groups = null;
        return chord2;
    };

    chord2.connections = function(x) {
        if (!arguments.length) return connections;
        connections = x;
        chords = groups = null;
        return chord2;
    };

    chord2.padding = function(x) {
        if (!arguments.length) return padding;
        padding = x;
        chords = groups = null;
        return chord2;
    };

    chord2.sortGroups = function(x) {
        if (!arguments.length) return sortGroups;
        sortGroups = x;
        chords = groups = null;
        return chord2;
    };

    chord2.sortSubgroups = function(x) {
        if (!arguments.length) return sortSubgroups;
        sortSubgroups = x;
        chords = null;
        return chord2;
    };

    chord2.sortChords = function(x) {
        if (!arguments.length) return sortChords;
        sortChords = x;
        if (chords) resort();
        return chord2;
    };

    chord2.chords = function() {
        if (!chords) relayout();
        return chords;
    };

    chord2.groups = function() {
        if (!groups) relayout();
        return groups;
    };

    return chord2;
}

})();
