function renderDot(dot) {
	hpccWasm.graphvizSync().then(graphviz => {
	    const div = document.getElementById("placeholder");
	    // Synchronous call to layout
	    div.innerHTML = graphviz.layout(dot, "svg", "dot");
	});
}
/*const mydot = `
   digraph g{
	  rankdir=LR;
	  "1" -> "2" [label="0"]
	  "2" -> "2" [label="0"]
	  "2" -> "3" [label="1"]
	  "3" -> "4" [label="1"]
	  "4" -> "5" [label="1"]
	  "5" -> "6" [label="1"]
   }
`;

renderDot(mydot)*/
