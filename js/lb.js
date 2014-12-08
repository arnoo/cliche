var settings = { 'delay' : 200, 'bufferPages' : 1 , 'placeholder' : "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAAAANSURBVBhXYzh8+PB/AAffA0nNPuCLAAAAAElFTkSuQmCC", 'srcTransform' : function (x) { return "/thumbs/home/"+x } };
var cols;
var pending = {};

var timeoutHandle;

function deferUpdate() {
    if (timeoutHandle!==undefined)
	window.clearTimeout(timeoutHandle);
    timeoutHandle = window.setTimeout(update, settings.delay);
    return true;
}

function load() {
    var self = this;
    var img = $(self);
    console.log("loading"+img.data("src"));
    img.attr("src", settings.srcTransform ? settings.srcTransform(img.data("src")) : img.data("src"));
    pending[self] = true;
    img.load(function () {delete(pending[self])});
}

function unload(img) {
    if (!img) { img = $(this); }
    console.log("unloading"+img.data("src"));
    img.attr("src", settings.placeholder);
}

function update() {
    var viewport = $("#thumbs");
    var firstPicBox = viewport.find("div:first-child");
    var picWidth = firstPicBox.width();
    var picHeight = firstPicBox.width();
    cols = Math.floor(viewport.width()/picWidth);
    var linesPerPage = Math.floor(viewport.height()/picHeight);

    var firstVisibleLine = Math.floor(viewport.scrollTop()/picHeight);
    var lastVisibleLine = Math.floor((viewport.scrollTop()+viewport.height())/picHeight);

    for (i in pending) {if (pending[i]) unload($(i))};

    maplines(firstVisibleLine, lastVisibleLine, load);
    maplines(lastVisibleLine, lastVisibleLine+linesPerPage*settings.bufferPages, load);
    maplines(firstVisibleLine-linesPerPage*settings.bufferPages, firstVisibleLine, load);
    
    maplines(0, firstVisibleLine-linesPerPage*settings.bufferPages, unload);
    maplines(firstVisibleLine-linesPerPage*settings.bufferPages, unload);

}

function maplines(from, to, fn) {
    if (from>=to) return;
    var fromSelector = from===0 ? "" : ":gt("+(from*cols-1)+")";
    $("#thumbs img"+fromSelector+":lt("+to*cols+")").each(fn);
}

$("#thumbs img").attr("src", settings.placeholder);
$("#thumbs").scroll(deferUpdate);
$("window").resize(deferUpdate);
update();
