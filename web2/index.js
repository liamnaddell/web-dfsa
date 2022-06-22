var Dracula = require('graphdracula')


var Graph = Dracula.Graph
var Renderer = Dracula.Renderer.Raphael
var Layout = Dracula.Layout.Tree

var g = new Graph();
g.addNode("1")
g.addNode("2")
g.addNode("3")
g.addNode("4")
g.addNode("5")
g.addNode("6")
g.addEdge("1","2",{directed: true,label:'0'})
g.addEdge("2","2",{directed: true,label:'0'})
g.addEdge("2","3",{directed: true,label:'1'})
g.addEdge("3","4",{directed: true,label:'1'})
g.addEdge("4","5",{directed: true,label:'1'})
g.addEdge("5","6",{directed: true,label:'1'})
var layouter = new Dracula.Layout.TournamentTree(g,topologicalSort(g));
var renderer = new Renderer('#paper', g, 800, 200);
layouter.layout();
renderer.draw();
