window.onload = (function () {

  var plots = document.getElementsByClassName('current-bench_plot');
  console.log('plots', plots);
  var layout = {
    title: false,
    height: 150,
    margin: {
      l: 100,
      r: 20,
      b: 20,
      t: 20,
      pad: 5

    }
  };

  var config = {
    displayModeBar: false,
    responsive: true
  };

  for (var i = 0; i < plots.length; ++i) {
    var plot = plots[i];
    console.log('plot', plot);
    var json = plot.textContent;
    console.log('json', json);
    var json = JSON.parse(json);
    console.log('json', json);

    Plotly.newPlot(plot.previousSibling, json, layout, config);


  }
  
});

// Plotly.newPlot('mydiv', data, layout);
