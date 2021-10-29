const http = require('http');
const connect = require('connect');


async function attachVite(handler) {
  console.log("Attaching handler to vite: ", handler);
  const app = connect();
  const vite = await require('vite').createServer({
    server: {
      middlewareMode: 'html',
      watch: {
        // During tests we edit the files too fast and sometimes chokidar
        // misses change events, so enforce polling for consistency
        usePolling: true,
        interval: 100
      }
    }
  });

  // server.addListener('request', function(req, res){
  //     console.log(req.headers);

  //     vite.middlewares(req, res);
  // });

  // vite.middlewares.use((req, res, next) => {
  //   console.log("REQUEST: ", req.requestUrl);
  //   next();
  // })
  // vite.middlewares.use(handler);
  // console.log("Handler attached");

  app.use(handler);
  app.use(vite.middlewares);


  // await vite.listen()
  http.createServer(app).listen(3000);
}

exports.connector = function(handler){
  return function(){
    attachVite(handler)
  }
}
