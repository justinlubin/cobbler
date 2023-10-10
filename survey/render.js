var system = require("system")

var basename = system.args[1];

var page = require("webpage").create();
page.viewportSize = { width: 3000, height: 2000 };

page.open(basename + ".html", function() {
    page.render(basename + ".untrimmed.png");
    phantom.exit();
});
