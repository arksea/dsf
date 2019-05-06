// Draw a sine curve at time t
var start;
function animate (t)
{
    var container = document.getElementById('container');
    var style = container.getAttribute("style");
    if(style == "")
    {
        var data,graph,offset,i;
        data = [];
        offset = 2 * Math.PI * (t - start) / 10000;
        // Sample the sine function
        for (i = 0; i < 4 * Math.PI; i += 0.2)
        {
            data.push([i, Math.sin(i - offset)]);
        }

        // Draw Graph
        graph = Flotr.draw(container, [ data ], {
                yaxis : {
                  max : 2,
                  min : -2
                }
            });
    }
    // Animate
    setTimeout(function () {
        animate((new Date).getTime());
        }, 1000);
}

function start_curve ()
{
    start = (new Date).getTime();
    animate(start);
}
