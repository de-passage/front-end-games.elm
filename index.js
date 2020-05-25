'use strict';

require('./index.html');
const {Elm} = require('./src/Main.elm');

Elm.Main.init({node: document.getElementById('main')});