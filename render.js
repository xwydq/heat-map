var page = require('webpage').create();

                  page.viewportSize = { width: 1280, height: 800 };

                  page.open('/home/xuwy/R_test/remap/html/ID_20161227130307_833811.html', function() {

                  window.setTimeout(function () {

                  page.render('/home/xuwy/R_test/remap/png/Rplot_069.png');

                  phantom.exit();

                  }, 100);

                  });
