# server for Hmail

A Clojure + ClojureScript app to bring a Gmail like interface to your own IMAP 
mail server.

## Usage

Start the server
lein run -m server.core/-main <port>

Open the browser on the host and port where the server is running.
Provide username, password and IMAP server to login

For now it pulls the 10 latest emails, fetching subject, sender and date first.
Gets the message bodies in separate async threads and pushes them through websocket to the client.

Navigate with the usual Gmail keyboard shortcuts: j, k, u, <enter>
Select messages with x and apply 'mark as un/read' with U and I.

## License

Copyright © 2014 Harry Binnendijk

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
