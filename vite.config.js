import { viteCommonjs } from '@originjs/vite-plugin-commonjs';
// import { VitePluginNode } from 'vite-plugin-node';
import nodePolyfills from 'rollup-plugin-polyfill-node';
import nodeGlobals from 'rollup-plugin-node-globals';
import replace from '@rollup/plugin-replace';


export default {
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:3001',
        changeOrigin: true,
        // rewrite: (path) => path.replace(/^\/api/, '')
      }
    }
  },
  build: {
    // generate manifest.json in outDir
    manifest: true,
    // rollupOptions: {
    //   // overwrite default .html entry
    //   // input: '/path/to/main.js'
    //   plugins: []
    // }
  },
  plugins: [
    // {
    //   ...nodeGlobals({
    //     process: false,
    //     global: true,
    //     buffer: false,
    //     dirname: false,
    //     filename: false,
    //     baseDir: false,
    //   }),
    //   enforce: "pre"
    // },
    {
      ...replace({
        'global': 'window'
      }),
      enforce: "pre"
    },
    viteCommonjs(),
    {
      ...nodePolyfills({
        include: ["path"]
      }),
      enforce: "pre"
    },
    // ...VitePluginNode({
    //   adapter: function(app, req, res) {
    //     console.log("app: ", app)
    //     app(res, res)
    //   }, 
    //   appPath: './index.js',
    //   exportName: 'server',
    // })
  ]
}