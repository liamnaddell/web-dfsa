var Dracula = require('graphdracula')


var Graph = Dracula.Graph
var Renderer = Dracula.Renderer.Raphael
var Layout = Dracula.Layout.Tree

var g = new Graph();
g.addNode("4")
g.addNode("1")
g.addNode("2")
g.addNode("3")
g.addEdge("1","2",{directed: true,label:'0'})
g.addEdge("1","3",{directed: true,label:'1'})
g.addEdge("3","4",{directed: true,label:'0'})
g.addEdge("2","4",{directed: true,label: '0'})
var layouter = new Dracula.Layout.Spring(g);
var renderer = new Renderer('#paper', g, 800, 400);
layouter.layout();
renderer.draw();
