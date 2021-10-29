// const { createServer } = require('vite')

// ;(async () => {
//   const server = await createServer({
//     // any valid user config options, plus `mode` and `configFile`
//     configFile: false,
//     root: __dirname,
//     server: {
//       port: 1337
//     }
//   })
//   await server.listen()
// })()

const Main = require("./output/Server.Main")
Main.main();

// // console.log("MAin", Main)
// exports.server = Main.server;

// // Main.main();

// async function attachVite(server) {
//   const vite = await require('vite').createServer({
//     server: {
//       middlewareMode: 'ssr',
//       watch: {
//         // During tests we edit the files too fast and sometimes chokidar
//         // misses change events, so enforce polling for consistency
//         usePolling: true,
//         interval: 100
//       }
//     }
//   });

//   server.addListener('request', function(req, res){
//       console.log(req.headers);

//       vite.middlewares(req, res);
//   });

//   // server.use(vite.middlewares);
// }

// Main.main((result) => () => {
//   const server = result.value0.value0;
//   // console.log("sever: ", server);
//   attachVite(server);
// })();

// // Main.main2();