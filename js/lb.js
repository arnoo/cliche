//TODO: closure wrapping, allow non-window viewport
var settings = { 'delay' : 200, 'bufferPages' : 1 , 'placeholder' : "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAAAANSURBVBhXYzh8+PB/AAffA0nNPuCLAAAAAElFTkSuQmCC", 'srcTransform' : function (x) { return "/thumbs/home/"+x } };
var cols;
var imgs;
var pending = {};
var failed = {};
var done = {};
var updateTimeoutHandle;
var preloadTimeoutHandle;
var container, viewport;

function deferUpdate() {
    if (updateTimeoutHandle!==undefined)
	window.clearTimeout(updateTimeoutHandle);
    updateTimeoutHandle = window.setTimeout(update, settings.delay);
    return true;
}

function load(imgIdx) {
    var img = imgs[imgIdx];
    var src = img.getAttribute("data-src");
    console.log("loading "+src);
    if (failed[imgIdx]!==undefined || done[imgIdx]!==undefined || pending[imgIdx]!==undefined) { return };
    img.setAttribute("src", settings.srcTransform ? settings.srcTransform(img.getAttribute("data-src")) : img.getAttribute("data-src"));
    pending[src] = true;
    img.addEventListener('load', function () {delete(pending[imgIdx]); done[imgIdx] = true});
    img.addEventListener('error', function () {failed[imgIdx]=true});
}

function unload(imgIdx) {
    var img = imgs[imgIdx];
    var src = img.getAttribute("data-src");
    console.log("unloading "+src);
    delete(done[imgIdx]);
    img.setAttribute("src", settings.placeholder);
}

function update() {
    var firstPicBox = container.querySelector("div:first-child");
    var picWidth = firstPicBox.offsetWidth;
    var picHeight = firstPicBox.offsetHeight;
    cols = Math.floor(container.offsetWidth/picWidth);
    var linesPerPage = Math.floor(viewport.offsetHeight/picHeight);

    var firstVisibleLine = Math.floor(-container.getBoundingClientRect().top/picHeight);
    var lastVisibleLine = Math.floor((viewport.innerHeight-container.getBoundingClientRect().top)/picHeight);

    for (i in pending) {if (pending[i]) unload(i)};

    if (preloadTimeoutHandle!==undefined)
	window.clearTimeout(preloadTimeoutHandle);

    maplines(firstVisibleLine, lastVisibleLine, load);

    preloadTimeoutHandle = window.setTimeout(function () {
						maplines(lastVisibleLine, lastVisibleLine+linesPerPage*settings.bufferPages, load);
						maplines(firstVisibleLine-linesPerPage*settings.bufferPages, firstVisibleLine, load);
						}, settings.delay);
    
    maplines(0, firstVisibleLine-linesPerPage*settings.bufferPages, unload);
    maplines(firstVisibleLine-linesPerPage*settings.bufferPages, unload);
}

function maplines(from, to, fn) {
    if (from>=to) return;
    for (var i=from*cols; i<=Math.min(to*cols, imgs.length-1); i++) {
	fn(i);
    }
}

function updatelb() {
    imgs = container.querySelectorAll("img");
    for (var i=0; i<imgs.length; i++) {
	imgs[i].setAttribute("src", settings.placeholder);
    }
    pending = {};
    failed = {};
    done = {};
}

function initlb(_container, _viewport) {
    container = _container.jQuery!==undefined ? _container : _container.get(0);
    viewport = _viewport.jQuery!==undefined ? _viewport : _viewport.get(0);
    viewport.addEventListener('scroll', deferUpdate);
    updatelb();
    window.addEventListener('resize', deferUpdate);
    deferUpdate();
}
