const path = require('path');

module.exports = {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: [/node_modules/, /elm-stuff/],
                loader: 'file-loader?name=[name].[ext]'
            }
        ]
    },
    entry: { index: path.resolve(__dirname, "webpack-entry.js") },
    output: { path: __dirname, filename: "elm.js" }
};