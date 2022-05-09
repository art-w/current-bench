window.onload = (function () {

  var plots = document.getElementsByClassName('current-bench_plot');
  console.log('plots', plots);
  var layout = {
    title: false,
    height: 150,
    margin: {
      l: 50,
      r: 50,
      b: 50,
      t: 5,
      pad: 5
    },

    xaxis: {
      gridcolor: 'rgb(255,255,255)',
      showgrid: true,
      showline: false,
      showticklabels: true,
      tickcolor: 'rgb(125,125,125)',
      ticks: 'outside',
      zeroline: false
    },

    yaxis: {
      showgrid: true,
      showline: false,
      tickcolor: 'rgb(125,125,125)',
      gridcolor: 'rgba(220,220,220,0.5)',
      ticks: 'outside',
      zeroline: false
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
