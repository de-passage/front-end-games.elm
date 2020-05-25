const path = require('path');

module.exports = {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: [/node_modules/, /elm-stuff/],
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname
                        }
                    }
                ]
            }]
    },
    mode: "development",
    entry: { index: path.resolve(__dirname, "index.js") },
    output: { path: __dirname, filename: "elm.js" },
    devServer: {
        contentBase: '.',
    },
};