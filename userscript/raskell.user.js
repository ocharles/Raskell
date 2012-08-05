// ==UserScript==
// @match http://hackage.haskell.org/*
// ==/UserScript==

function addJQuery(callback) {
  var script = document.createElement("script");
  script.setAttribute("src", "//code.jquery.com/jquery-latest.min.js");
  script.addEventListener('load', function() {
    var script = document.createElement("script");
    script.textContent = "(" + callback.toString() + ")();";
    document.body.appendChild(script);
  }, false);
  document.body.appendChild(script);
}

addJQuery(function() {
    var activateFor = new RegExp('\/package\/[^\/]+\/?$');
    $.ajax('http://raskell.ocharles.org.uk/ws/user-overview', {
        data: { user: "{{raskell-hash}}" },
        dataType: 'json',
        success: function(data) {
            var insertQueue = [];

            $('a[href^="/package"]').each(function() {
                var a = $(this);
                if (!activateFor.test(a.attr('href'))) {
                    return true;
                }

                insertQueue.push(function() {
                    var project = a.text();
                    var overview = data[project];

                    var button = $('<button>')
                        .addClass('raskell-plus-one')
                        .data('project', project);

                    if (overview && overview.ratings) {
                        button.html(overview.ratings);
                        button.addClass('raskell-rated');
                    }
                    else {
                        button.html('++');
                    }

                    if (overview && overview.rated) {
                        button.addClass('raskell-i-rated');
                    }

                    a.after(button);
                });
            });

            var addButton = function() {
                if (insertQueue.length > 0) {
                    (insertQueue.shift())();
                    setTimeout(addButton, 0);
                }
            };

            setTimeout(addButton, 0);
        }
    });

    $('body')
        .append('<link rel="stylesheet" type="text/css" \
                       href="http://raskell.ocharles.org.uk/raskell.css" />');

    $('button.raskell-plus-one').live('click', function() {
        var button = $(this);
        $.ajax('http://raskell.ocharles.org.uk/ws/toggle-rating', {
            data: { project: $(this).data('project'), user: '{{raskell-hash}}' },
            dataType: 'json',
            success: function(data) {
                console.log(button);
                console.log(data);
                button.html(data.ratings > 0 ? data.ratings : '++');
                button.toggleClass('raskell-i-rated', data.rated);
                button.toggleClass('raskell-rated', data.ratings > 0);
            }
        });
    });
});

console.log('Raskell user script changed');
