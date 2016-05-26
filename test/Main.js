"use strict";

exports.getImpl = function(opts) {
    return function(done) {
        return function() {
            require('http').request(opts, function(res) {
                var body = '';
                res.setEncoding('utf8');
                res.on('data', function (s) {
                    body += s;
                });
                res.on('end', function () {
                    done(body)();
                });
            }).end();
        };
    };
};
