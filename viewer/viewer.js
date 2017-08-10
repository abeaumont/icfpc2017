// from stackoverflow: https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript
function getParameterByName(name, url) {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}


var gameName = getParameterByName("game") || "game";
if (gameName.charAt(gameName.length - 1) == "/") {
// workaround for a bug on kadoban's setup, browsers/server are adding
// a forward slash at the end, screwing up the load
    gameName = gameName.slice(0, -1);
}
var testGame = "../output/" + gameName + ".json";
var TURNS, CURRENT_TURN;
var sWrap;
var gameInfo;
var mines;
var rivers = {};

console.log("GAME NAME",gameName);
$("input#file").val(gameName);

//var colors = ["#00cc00", "#0000cc", "#aaaa00", "#aa00aa", "#00aaaa"];
//var colors = ["#00cc00", "#0000cc", "aaaa00", "aa00aa", "00aaaa"];
//var colors = ["#00ff00", "#0000ff", "aaaa00", "aa00aa", "00aaaa"];
var colors = ["#1f77b4", "#aec7e8", "#aaaa00", "#aa00aa", "#00aaaa"];
//var edgeColor = '#000';
var edgeColor = '#009';//blue uncl river
var mineColor = '#f00';//red mine
var siteColor = '#fff';//white site

function initGame(players) {
    rivers = {};
    for (var i in players) {
        var row =
            $("<tr>")
                .append($("<td>").append($("<font>").attr("color", colors[i]).text(players[i])))
                .append($("<td>").text(0));
        $("#scoreboard").after(row);
    }
}

function claim(player, source, target) {
    var river = rivers[source + ";" + target] || rivers[target + ";" + source];
    var edge = sWrap.graph.edges(river);
    edge.color = colors[player + 1];
    if (player === null) {
        edge.color = edgeColor;
        edge.size = 2;
    } else {
        edge.color = colors[player];
        edge.size = 10;
    }
    sWrap.refresh();
}

function mapToSigma(obj) {
    var res = {"nodes": obj.sites, "edges": obj.rivers};
    mines = new Set(obj.mines);
    for (var n of res.nodes) {
        n.label = String(n.id);
        if (mines.has(n.id)) {
            n.size = 10;
            n.color = mineColor;
        } else {
            n.size = 3;
            n.color = siteColor;
        }
    }
    for (var i in res.edges) {
        var edge = res.edges[i];
        //edge.size = 0.5;
        edge.size = 2;
        edge.color = edgeColor;
        edge.id = i;
        edge.label = edge.source + ";" + edge.target
        rivers[edge.label] = i;
    }
    return res;
}

function loadGraph(path) {
    $.getJSON({
        url: path,
        success: function(response) {
            sWrap.graph.clear();
            sWrap.graph.read(mapToSigma(response));
            sWrap.refresh();
        }
    });
}

function loadGame(path) {
    $("#graph-container").empty();
    sWrap = new sigma("graph-container");
    sWrap.settings({"minNodeSize": 1, "maxNodeSize": 8,
                    "minEdgeSize": 0.1, "maxEdgeSize": 5,
                    "defaultLabelColor": "#aaaaaa", "labelHoverColor": "#ffffff",
                    "labelThreshold": 0});
    sWrap.cameras[0].goTo({ x: 0, y: 0, angle: 0, ratio: 2 });

    $.getJSON({
        url: path,
        success: function(game) {
            var players = [];
            for (var i = 0; i < game.num_players; ++i)
                players[i] = "Punter" + (i + 1);
            initGame(players);
            TURNS = game.turns;
            gameInfo = game;
            CURRENT_TURN = 0;
            $("#log").text("The game was loaded");

            console.log("GAME IS", game);

            if (game.map) {
              sWrap.graph.read(mapToSigma(game.map));
            }

            sWrap.refresh();
        }
    });
}

function prevTurn() {
    if (CURRENT_TURN == 0)
        return;
    CURRENT_TURN--;
    turn = TURNS[CURRENT_TURN];
    if (turn.claim) {
        var data = turn.claim;
        claim(null, data.source, data.target);
    }

    if (turn.option) {
      var data = turn.option;
      claim(null, data.source, data.target);

    }
    $("#turn" + CURRENT_TURN).remove();
}

function getPunterName(punter) {
  if (gameInfo.players) {
    return gameInfo.players[punter] || punter;
  }

  return punter;
}

function getScore(CURRENT_TURN, punter, scores) {

}

function nextTurn() {
    if (CURRENT_TURN == TURNS.length) {
        return;
    }
    var logEl = $("#log");
    turn = TURNS[CURRENT_TURN];
    if (turn.claim) {
        var data = turn.claim;
        claim(data.punter, data.source, data.target);
        logEl.append($("<div>")
          .attr("id", "turn" + CURRENT_TURN)
          .text(getPunterName(data.punter) + " claimed " + data.source + "->" + data.target));

        var score = getScore(CURRENT_TURN, data.punter);

        if (score) {
          logEl.append(score);
        }


    } else if (turn.pass) {

        $("#log").append($("<div>")
          .attr("id", "turn" + CURRENT_TURN)
          .text(getPunterName(turn.pass.punter) + " passed"));
    } else if (turn.option) {
      var data = turn.option;
      claim(data.punter, data.source, data.target);
      $("#log").append($("<div>")
        .attr("id", "turn" + CURRENT_TURN)
        .text(getPunterName(data.punter) + " optioned " + data.source + "->" + data.target));


    }
    CURRENT_TURN++;
}

$(function() {
    $(':button').on('click', function() {
        loadGame("../output/" + $("#file").val() + ".json");
    });

    $("#back").click(prevTurn);
    $("#forward").click(nextTurn);

    $(document).keydown(function(e) {
        switch (e.keyCode) {
            case 37: prevTurn(); break;
            case 39: nextTurn(); break;
        }
    });

    loadGame(testGame);
});

