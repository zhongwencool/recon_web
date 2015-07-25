
var process_count = 0;
var run_queue = 0;
var error_logger_queue_len = 0;

var reductions = 0;
var memory_total = 0;
var memory_process = 0;
var memory_atom = 0;
var memory_bin = 0;
var memory_ets = 0;

var scheduler_usage = new Array();

var bytes_in = 10;
var bytes_out =10;

var memory_count  = {};
var bin_memory_count = {};
var reductions_count = {};
var total_heap_size_count = {};

var port_summary = "";
var inet_count_sent_cnt = {};
var inet_count_recv_cnt = {};
var inet_count_sent_oct = {};
var inet_count_recv_oct = {};

var session_count = 1;

var alloc_used = 1;
var alloc_unused = 0;
var alloc_allocated_types = {};
var alloc_allocated_instances = {};
var cache_hit = {};

// replace the url for  yourself
var socket =  io.connect('http://127.0.0.1:8080'); 
socket.on('connect', function() {
    output('<span class="connect-msg">Client has connected to the server!</span>','');
});
socket.on('message', function(data) {
    output('<span class="username-msg">' + data.userName + ':</span> ' , data);
});
socket.on('disconnect', function() {
    output('<span class="disconnect-msg">The client has disconnected!</span>','');
});

function sendDisconnect() {
    socket.disconnect();
}
function sendMessage() {
    var message = $('#msg').val();
    $('#msg').val('');

    // don't forget to define type field '@class'
    // it should equals to class name which used
    // to deserialize object on server side
    // via ...addJsonObjectListener() method
    //
    // TIP: you can customize type name field
    // via Configuration.jsonTypeFieldName property

    var jsonObject = {userName: userName,
      message: message};
      socket.json.send(jsonObject);
  }
  function output(message, data) {
    var currentTime = "<span class='time'>" +  moment().format('HH:mm:ss') + "</span>";
    <!--JSON.stringify(message) -->
    var element = $("<div>" + currentTime + " " + JSON.stringify(data)  + "</div>");
    if ( data instanceof Array ) {
        update_every_heartbeat(data);
    }else if(data.system_version != undefined){
        first_msg_from_server(data);
    };
//    $('#console').prepend(element);
};

function first_msg_from_server(data) {
    $("#title").html(data.system_version);
    jQuery.each(data, function(attr, val) { 
      $("#" + attr).html(""+val);})
    create_scheduler_chart(data.logical_processors);
};

function update_every_heartbeat(data){   
    process_count = data[0].process_summary.process_count;
    run_queue = data[0].process_summary.run_queue;
    error_logger_queue_len = data[0].process_summary.error_logger_queue_len;
    //        alert(JSON.stringify(data[2]));
    memory_total = data[0].process_summary.memory_total;
    memory_process = data[0].process_summary.memory_procs;
    memory_atom = data[0].process_summary.memory_atoms;
    memory_bin = data[0].process_summary.memory_bin;
    memory_ets = data[0].process_summary.memory_ets;
    reductions = data[0].mem_summary.reductions;
    jQuery.each(data[0].mem_summary.scheduler_usage, function(index, val){
        scheduler_usage[index] = Math.round(val * 10000)/100;
    });

    bytes_in = data[0].mem_summary.bytes_in+bytes_in;
    bytes_out = data[0].mem_summary.bytes_out+bytes_out;

    memory_count = data[1].proc_count.memory;
    bin_memory_count = data[1].proc_count.bin_memory;
    reductions_count = data[1].proc_count.reductions;
    total_heap_size_count = data[1].proc_count.total_heap_size;

    port_summary = JSON.stringify(data[2]);
    $("#port_summary").html(port_summary);

    inet_count_sent_oct = data[3].inet_count.sent_oct;
    inet_count_recv_oct = data[3].inet_count.recv_oct;
    inet_count_sent_cnt = data[3].inet_count.sent_cnt;
    inet_count_recv_cnt = data[3].inet_count.recv_cnt;

    session_count = data[4].session_count;
    $("#circle").text("" + session_count);

    alloc_used = Math.round(data[5].used *100)/100;
    alloc_unused = Math.round(data[5].unused *100)/100;
    alloc_allocated_types = data[5].allocated_types;
    alloc_allocated_instances = data[5].allocated_instances;
    create_alloc_instance(Object.keys(alloc_allocated_instances).length);

    cache_hit = data[6].cache_hit_rates;
    create_cache_hit(cache_hit);
};

function create_scheduler_chart(logical_processors){
 if($('div').hasClass('scheduler')){}
    else{
        var is_left = 1;
        var div;
        for(var i=logical_processors; i > 0; i--){
            var div_left = '<div class = scheduler id="scheduler_usage' + i + '"style="min-width: 250px; height: 250px; margin: 0 auto; float:left"></div>';
            var div_right = '<div class = scheduler id="scheduler_usage' + i + '"style="min-width: 250px; height: 250px; margin: 0 auto; float:right"></div>';
            if(is_left ==1||is_left ==2 ){div = div_left; is_left++}else if(is_left ==3){div = div_right; is_left++}else if(is_left == 4){div = div_right; is_left = 1};
            $('#memory').after(div);
        };
        $(document).ready(function (){
            for(var j = 1; j< logical_processors +1; j++){        
                scheduler_name = 'scheduler_usage' + j;
                $('#' + scheduler_name).highcharts({
                    chart: {
                        type: 'gauge',
                        plotBackgroundColor: null,
                        plotBackgroundImage: null,
                        plotBorderWidth: 0,
                        plotShadow: false
                    },
                    credits:{
                        text: "recon_web",
                        href: "https://github.com/zhongwencool/recon_web"
                    },

                    title: {
                        text: scheduler_name
                    },

                    pane: {
                        startAngle: -150,
                        endAngle: 150,
                        background: [{
                            backgroundColor: {
                                linearGradient: { x1: 0, y1: 0, x2: 0, y2: 1 },
                                stops: [
                                [0, '#FFF'],
                                [1, '#333']
                                ]
                            },
                            borderWidth: 0,
                            outerRadius: '109%'
                        }, {
                            backgroundColor: {
                                linearGradient: { x1: 0, y1: 0, x2: 0, y2: 1 },
                                stops: [
                                [0, '#333'],
                                [1, '#FFF']
                                ]
                            },
                            borderWidth: 1,
                            outerRadius: '107%'
                        }, {
                // default background
            }, {
                backgroundColor: '#DDD',
                borderWidth: 0,
                outerRadius: '105%',
                innerRadius: '103%'
            }]
        },

        // the value axis
        yAxis: {
            min: 0,
            max: 100,

            minorTickInterval: 'auto',
            minorTickWidth: 1,
            minorTickLength: 10,
            minorTickPosition: 'inside',
            minorTickColor: '#666',

            tickPixelInterval: 30,
            tickWidth: 2,
            tickPosition: 'inside',
            tickLength: 10,
            tickColor: '#666',
            labels: {
                step: 2,
                rotation: 'auto'
            },
            title: {
                text: '%'
            },
            plotBands: [{
                from: 0,
                to: 80,
                color: '#55BF3B' // green
            }, {
                from: 80,
                to: 90,
                color: '#DDDF0D' // yellow
            }, {
                from: 90,
                to: 100,
                color: '#DF5353' // red
            }]
        },

        series: [{
            name: j,
            data: [80],
            tooltip: {
                valueSuffix: ' %'
            }
        }]

    },
        // Add some life
        function (chart) {
            if (!chart.renderer.forExport) {
                setInterval(function () {
                    var point = chart.series[0].points[0];
                    point.update(scheduler_usage[chart.series[0].name]);

                }, 6050);
            }
        })};
})};
};
//cup live second 

$(function () {
    $(document).ready(function () {
        Highcharts.setOptions({
            global: {
                useUTC: false
            }
        });

        $('#process').highcharts({
            chart: {
                type: 'spline',
                animation: Highcharts.svg, // don't animate in old IE
                marginRight: 10,
                events: {
                    load: function () {

                        // set up the updating of the chart each second
                        var series_process_count = this.series[0];
                        var series_run_queue = this.series[1];                        
                        var series_error_logger_queue_len = this.series[2]; 
                        setInterval(function () {
                            var x = (new Date()).getTime(); // current time
                            series_process_count.addPoint([x, process_count], true, true);
                            series_run_queue.addPoint([x, run_queue], true, true);
                            series_error_logger_queue_len.addPoint([x, error_logger_queue_len], true, true);
                        }, 6020);
                    }
                }
            },
            title: {
                text: 'Process Information'
            },
            credits:{
                text: "recon_web",
                href: "https://github.com/zhongwencool/recon_web"
            },
            xAxis: {
                type: 'datetime',
                tickPixelInterval: 150
            },
            yAxis: {
                title: {
                    text: 'Count'
                },
                plotLines: [{
                    value: 0,
                    width: 1,
                    color: '#808080'
                }]
            },
            tooltip: {
                shared: false,
                formatter: function () {
                    return '<b>' + this.series.name + '</b><br/>' +
                    Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) + '<br/>' +
                    Highcharts.numberFormat(this.y, 2);
                }
            },
            legend: {
                enabled: true
            },
            exporting: {
                enabled: false
            },
            series: [{
                name: 'process_count',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;
                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 10
                        });
                    }
                    return data;
                }())
            },
            {
                name: 'run_queue',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;

                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 15
                        });
                    }
                    return data;
                }())
            },
            {
                name: 'error_logger_queue_len',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;
                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 20
                        });
                    }
                    return data;
                }())}
                ]
            });
});
});

// memory second live
$(function () {
    $(document).ready(function () {
        Highcharts.setOptions({
            global: {
                useUTC: false
            }
        });

        $('#memory').highcharts({
            chart: {
                type: 'spline',
                animation: Highcharts.svg, // don't animate in old IE
                marginRight: 10,
                events: {
                    load: function () {
                        // set up the updating of the chart each second
                        var series_mem_total = this.series[0];
                        var series_mem_procs = this.series[1];                        
                        var series_mem_atom = this.series[2]; 
                        var series_mem_bin = this.series[3];
                        var series_mem_ets = this.series[4];
                        setInterval(function () {
                            var x = (new Date()).getTime(); // current time
                            series_mem_total.addPoint([x, memory_total], true, true);
                            series_mem_procs.addPoint([x, memory_process], true, true);
                            series_mem_atom.addPoint([x, memory_atom], true, true);
                            series_mem_bin.addPoint([x, memory_bin], true, true);
                            series_mem_ets.addPoint([x, memory_ets], true, true);                            
                        }, 6100);
                    }
                }
            },
            title: {
                text: 'Memory Information'
            },
            credits:{
                text: "recon_web",
                href: "https://github.com/zhongwencool/recon_web"
            },
            xAxis: {
                type: 'datetime',
                tickPixelInterval: 150
            },
            yAxis: {
                title: {
                    text: 'Byte'
                },
                plotLines: [{
                    value: 0,
                    width: 1,
                    color: '#808080'
                }]
            },
            tooltip: {
                shared: false,
                formatter: function () {
                    return '<b>' + this.series.name + '</b><br/>' +
                    Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) + ' byte<br/>' +
                    Highcharts.numberFormat(this.y, 2);
                }
            },
            legend: {
                enabled: true
            },
            exporting: {
                enabled: false
            },
            series: [{
                name: 'total_memory',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;
                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 100
                        });
                    }
                    return data;
                }())
            },
            {
                name: 'process_memory',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;

                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 90
                        });
                    }
                    return data;
                }())
            },
            {
                name: 'atom_memory',
                data: (function () {
                    var data = [],
                    time = (new Date()).getTime(),
                    i;

                    for (i = -19; i <= 0; i += 1) {
                        data.push({
                            x: time + i * 1000,
                            y: 80
                        });
                    }
                    return data;
                }())},
                {
                    name: 'bin_memory',
                    data: (function () {
                        var data = [],
                        time = (new Date()).getTime(),
                        i;

                        for (i = -19; i <= 0; i += 1) {
                            data.push({
                                x: time + i * 1000,
                                y: 70
                            });
                        }
                        return data;
                    }())},

                    {
                        name: 'ets_memory',
                        data: (function () {
                            var data = [],
                            time = (new Date()).getTime(),
                            i;

                            for (i = -19; i <= 0; i += 1) {
                                data.push({
                                    x: time + i * 1000,
                                    y: 60
                                });
                            }
                            return data;
                        }())}
                        ]
                    });
});
});

$(function () {
    $(document).ready(function () {
        $('#byte').highcharts({
            chart: {
                type: 'bar'
            },
            title: {
                text: 'Summary of bytes in out from open this web'
            },
            xAxis: {
                categories: ['Byte'],
                title: {
                    text: null
                }
            },
            yAxis: {
                min: 0,
                title: {
                    text: 'Byte',
                    align: 'high'
                },
                labels: {
                    overflow: 'justify'
                }
            },
            tooltip: {
                valueSuffix: ' bit'
            },
            plotOptions: {
                bar: {
                    dataLabels: {
                        enabled: true
                    }
                }
            },
            legend: {
                layout: 'vertical',
                align: 'right',
                verticalAlign: 'top',
                x: -40,
                y: 80,
                floating: true,
                borderWidth: 1,
                backgroundColor: ((Highcharts.theme && Highcharts.theme.legendBackgroundColor) || '#FFFFFF'),
                shadow: true
            },
            credits: {
                enabled: false
            },
            series: [{
                name: 'Byte In',
                data: [107]
            }, {
                name: 'Byte Out',
                data: [133]
            }
            ]
        },function (chart) {
            if (!chart.renderer.forExport) {
                setInterval(function () {
                    chart.series[0].data[0].update(bytes_in);
                    chart.series[1].data[0].update(bytes_out);

                }, 6010)}});
})});

// proc_count
$(function () {
    $(document).ready(function () {
        var proc_count_name = ['memory', 'bin_memory', 'reduction', 'total_heap_size'];
        for(var num = 0 ;num < proc_count_name.length; num++){ 
            var name = proc_count_name[num];        
            $('#' + name +'_count').highcharts({
                chart: {
                    type: 'column'
                },
                title: {
                    text:  name
                },
                credits:{
                    text: "recon_web",
                    href: "https://github.com/zhongwencool/recon_web"
                },
                xAxis: {
                    type: 'category',
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '13px',
                            fontFamily: 'Verdana, sans-serif'
                        }
                    }
                },
                yAxis: {
                    min: 0,
                    title: {
                        text: 'Kb'
                    }
                },
                legend: {
                    enabled: false
                },
                tooltip: {
                    pointFormat: ': <b>{point.y:.1f} Kb</b>'
                },
                series: [{
                    name: name,
                    data: [
                    ['pid1', 23.7],
                    ['pid2', 16.1],
                    ['pid3', 14.2],
                    ['pid4', 14.0],
                    ['pid5', 12.5],
                    ['pid6', 12.1],
                    ['pid7', 11.8],
                    ['pid8', 11.7],
                    ['pid9', 11.1],
                    ['pid10', 11.1]            
                    ],
                    dataLabels: {
                        enabled: true,
                        rotation: -90,
                        color: '#FFFFFF',
                        align: 'right',
                format: '{point.y:.1f}', // one decimal
                y: 10, // 10 pixels down from the top
                style: {
                    fontSize: '13px',
                    fontFamily: 'Verdana, sans-serif'
                }
            }
        }]
    },function (chart) {
        setInterval(function () {
            var pid = 0;
            var counts = "";
            if(chart.series[0].name == 'memory') {
                counts = memory_count;
            }else if(chart.series[0].name == 'bin_memory'){
                counts = bin_memory_count;
            }else if(chart.series[0].name == 'reduction'){
                counts = reductions_count;
            }else if(chart.series[0].name == 'total_heap_size'){
                counts = total_heap_size_count;
            };
            jQuery.each(counts, function(attr, value){
                chart.series[0].data[pid++].update([attr, Math.round(value /(1024)* 100)/100]);
            });

        }, 6010)}
        )}})});

// inet_count
$(function () {
    $(document).ready(function () {
        var proc_count_name = ['sent_oct', 'recv_oct', 'sent_cnt', 'recv_cnt'];
        for(var num = 0 ;num < proc_count_name.length; num++){ 
            var name = proc_count_name[num];        
            $('#inet_' + name).highcharts({
                chart: {
                    type: 'column'
                },
                title: {
                    text:  name
                },
                credits:{
                    text: "recon_web",
                    href: "https://github.com/zhongwencool/recon_web"
                },
                xAxis: {
                    type: 'category',
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '13px',
                            fontFamily: 'Verdana, sans-serif'
                        }
                    }
                },
                yAxis: {
                    min: 0,
                    title: {
                        text: 'Kb'
                    }
                },
                legend: {
                    enabled: false
                },
                tooltip: {
                    pointFormat: ': <b>{point.y:.1f} Kb</b>'
                },
                series: [{
                    name: name,
                    data: [
                    ['port1', 0],
                    ['port2', 0],
                    ['port3', 0],
                    ['port4', 0],
                    ['port5', 0],
                    ['port6', 0],
                    ['port7', 0],
                    ['port8', 0],
                    ['port9', 0],
                    ['port10', 0]            
                    ],
                    dataLabels: {
                        enabled: true,
                        rotation: -90,
                        color: '#FFFFFF',
                        align: 'right',
                format: '{point.y:.1f}', // one decimal
                y: 10, // 10 pixels down from the top
                style: {
                    fontSize: '13px',
                    fontFamily: 'Verdana, sans-serif'
                }
            }
        }]
    },function (chart) {
        setInterval(function () {
            var port = 0;
            var counts = {};
            if(chart.series[0].name == 'sent_oct') {
                counts = inet_count_sent_oct;
            }else if(chart.series[0].name == 'recv_oct'){
                counts = inet_count_recv_oct;
            }else if(chart.series[0].name == 'sent_cnt'){
                counts = inet_count_sent_cnt;
            }else if(chart.series[0].name == 'recv_cnt'){
                counts = inet_count_recv_cnt;
            };
            jQuery.each(counts, function(attr, value){
                chart.series[0].data[port++].update([attr, Math.round(value /(1024)* 100)/100]);
            });

        }, 6030)}
        )}})});

//alloc memory usage
$(function () {
    $(document).ready(function () {
        $('#alloc_memory').highcharts({
            chart: {
                plotBackgroundColor: null,
                plotBorderWidth: null,
                plotShadow: false
            },
            title: {
                text: 'alloc usaged'
            },
            credits: {
                enabled: false
            },
            tooltip: {
                pointFormat: '{series.name}: <b>{point.percentage:.2f}%</b>'
            },
            plotOptions: {
                pie: {
                    allowPointSelect: true,
                    cursor: 'pointer',
                    dataLabels: {
                     enabled: true,
                     format: '<b>{}</b>{point.y:.2f} Mb'
                 },
                 showInLegend: true
             }
         },
         series: [{
            type: 'pie',
            name: 'memory usage',
            data: [ 
            ['used',   45.0],
            ['unused',   45.0]
            ]
        }]
    },function(chart){
        if (!chart.renderer.forExport) {
            setInterval(function () {
                chart.series[0].data[0].update(alloc_used);
                chart.series[0].data[1].update(alloc_unused);
            }, 6010)}});
});
});

//alloc allocated_types
$(function () {
    $(document).ready(function () {
        $('#allocated_types').highcharts({
            chart: {
                plotBackgroundColor: null,
                plotBorderWidth: null,
                plotShadow: false
            },
            title: {
                text: 'allocated_types'
            },
            credits: {
                enabled: false
            },
            tooltip: {
                pointFormat: '{}: <b>{point.percentage:.2f}%</b>'
            },
            plotOptions: {
                pie: {
                    allowPointSelect: true,
                    cursor: 'pointer',
                    dataLabels: {
                     enabled: true,
                     format: '<b>{point.name}</b>:{point.y:.2f} Mb'
                 },
                 showInLegend: true
             }
         },
         series: [{
            type: 'pie',
            name: 'allocated_types',
            data: [ 
            ['binary',   1.157],
            ['driver',   0.157],
            ['eheap',    5.950],
            ['ets',      1.041],
            ['fix',      0.657],
            ['ll',      20.000],
            ['sl',       0.157],
            ['std',      0.891],
            ['temp',     0.626]
            ]
        }]
    },function(chart){
        if (!chart.renderer.forExport) {
            setInterval(function () {
                chart.series[0].data[0].update(Math.round(alloc_allocated_types.binary_alloc * 10000)/10000);
                chart.series[0].data[1].update(Math.round(alloc_allocated_types.driver_alloc * 10000)/10000);
                chart.series[0].data[2].update(Math.round(alloc_allocated_types.eheap_alloc * 10000)/10000);
                chart.series[0].data[3].update(Math.round(alloc_allocated_types.ets_alloc * 10000)/10000);
                chart.series[0].data[4].update(Math.round(alloc_allocated_types.fix_alloc * 10000)/10000);
                chart.series[0].data[5].update(Math.round(alloc_allocated_types.ll_alloc * 10000)/10000);
                chart.series[0].data[6].update(Math.round(alloc_allocated_types.sl_alloc * 10000)/10000);
                chart.series[0].data[5].update(Math.round(alloc_allocated_types.std_alloc * 10000)/10000);
                chart.series[0].data[6].update(Math.round(alloc_allocated_types.temp_alloc * 10000)/10000);
            }, 6015)}});
});
});

//alloc allocated_instances
function create_alloc_instance(instance_num){
 if($('div').hasClass('already_create_instance')){}else {
    $('#allocated_instances').highcharts({
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: null,
            plotShadow: false
        },
        title: {
            text: 'allocated_instances'
        },
        credits: {
            enabled: false
        },
        tooltip: {
            pointFormat: '{series.name}: <b>{point.percentage:.2f}%</b>'
        },
        plotOptions: {
            pie: {
                allowPointSelect: true,
                cursor: 'pointer',
                dataLabels: {
                 enabled: true,
                 format: '<b>{point.name}</b>:{point.y:.2f} '
             },
             showInLegend: true
         }
     },
     series: [{
        type: 'pie',
        name: 'allocated_instances',
        data: (function () {
                    var data = [],
                    i;
                    for (i = 0; i < instance_num; i ++) {
                        data.push({
                            x: 8.923324584960938,
                            y: i,
                            name: "ins_" + i
                        });
                    }
                    return data;
                }())
    }]
},function(chart){
    if (!chart.renderer.forExport) {
        setInterval(function () {
            var pid = 0;
            jQuery.each(alloc_allocated_instances, function(attr, value){
                chart.series[0].data[pid++].update(value);
            });
        }, 6015)}});
};
$('#allocated_instances').addClass("already_create_instance");
};

//alloc allocated_instances
function create_cache_hit(instance){
 if($('div').hasClass('already_create_cache_hit')){}else {
    $('#cache_hit_chart').highcharts({
        chart: {
            type: 'bar'
        },
        title: {
            text: 'cache_hit_rates'
        },
        xAxis: {
            categories:  (function () {
                    var data = [];
                    jQuery.each(instance, function(Attr,Value){
                        data.push(Attr);
                    });
                    return data;
                }())
        },
        yAxis: {
            min: 0,
            title: {
                text: 'Hit'
            }
        },
        legend: {
            reversed: false
        },
        tooltip: {
                pointFormat: '{series.name}: <b>{point.percentage:.2f}%</b>'
            },
        plotOptions: {
            series: {
                stacking: 'normal'
            }
        },
        series:         
        [{
            name: 'NotHit',
            data: 
            (function () {
                    var data = [];
                    jQuery.each(instance, function(attr,value){
                        data.push(value[1] - value[0]);
                    });
                    return data;
                }())
            
        }, 
        {
            name: 'Hit',
            data: 
            (function () {
                    var data = [];
                    jQuery.each(instance, function(attr,value){
                        data.push(value[0]);
                    });
                    return data;
                }())

        }]
    });
    $('#cache_hit_chart').addClass("already_create_cache_hit");
}}



