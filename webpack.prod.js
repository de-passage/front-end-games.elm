const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
    mode: 'production',
    module: {
        rules: [
            {
                test: /\.elm$/,
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