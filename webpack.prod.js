const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge.smart(common, {
    mode: 'production',
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            optimize: true
                        }
                    }
                ]
            }
        ]
    }
});