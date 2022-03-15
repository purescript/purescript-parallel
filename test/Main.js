import http from "http";

export function getImpl(opts) {
    return function(done) {
        return function() {
            http.request(opts, function(res) {
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
}
