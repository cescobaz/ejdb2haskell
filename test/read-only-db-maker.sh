#!/bin/bash

curl http://localhost:9191/plants -d '{"name":"pinus","isTree":true,"year":1753,"description":"wow 🌲","ratio":null,"insects":["ant"], "theLeaf": {"color":"green", "size": 420}, "ids" : [42, 23, 35]}'
curl http://localhost:9191/plants -d '{"name":"gentiana brentae","isTree":false,"year":2008,"description":"violet 🌺flower", "theLeaf":null, "leaf":null}'
curl http://localhost:9191/plants -d '{"name":"leontopodium","isTree":false,"year":1817,"description":"tipical alpine flower"}'
curl http://localhost:9191/plants -d '{"name":"leucanthemum vulgare","isTree":false,"year":1778,"description":"very common flower in Italy 🍕","ratio":1.618}'
