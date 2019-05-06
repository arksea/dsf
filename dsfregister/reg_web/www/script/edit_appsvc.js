document.getElementById("body").onkeydown="return (event.keyCode!=8)";
function set_selected(s,v) {
    for (var i=0;i<s.length;++i) {
        if (s[i].text == v) {
            s[i].selected = "selected";
        } else {
            s[i].selected = "";
        }
    }
}
var s = document.getElementById("protocol");
var v = document.getElementById("protocol_value").value;
set_selected(s,v);

s = document.getElementById("route");
v = document.getElementById("route_value").value;
set_selected(s,v);

s = document.getElementById("failed");
v = document.getElementById("failed_value").value;
set_selected(s,v);
