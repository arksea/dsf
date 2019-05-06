// Get an Element
function $() { return document.getElementById(arguments[0]); }
//// Get the value of an Element
//function $F() { return document.getElementById(arguments[0]).value; }

var WS = false;
if (window.WebSocket) WS = WebSocket;
if (!WS && window.MozWebSocket) WS = MozWebSocket;
if (WS)
    $('status_information').textContent = "";

var domain = document.domain;
var url = "ws://"+domain+":8080/websock_service.yaws?view=objmeta_table";

var ws = new WebSocket(url);
var send_fun = function(){
    table = $('table_name').textContent;
    ws.send(table);
    setTimeout(arguments.callee,5000);
}

ws.onopen = function()
{
    table = $('table_name').textContent;
    ws.send(table);
    setTimeout(send_fun,5000);
    start_curve();
}

ws.onmessage = function(m)
{
    var objs = eval("("+m.data+")");
    for(key in objs)
    {
        $('objvalue_'+key).textContent = objs[key];
    }
}

ws.onclose = function()
{

}

function show_curve()
{
    var c = document.getElementById("container");
    c.setAttribute("style","");
}