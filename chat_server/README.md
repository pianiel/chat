## A single room chat server

This is a simple chat server written with Erlang and OTP. The main modules are:
  - `chat_server` - contains the logic of the server, stores users and routes messages
  - `tcp_listener` - takes care of accepting new connections and creating new sockets for incoming clients
  - `chat_client_handler` - many processes with a common supervisor `chat_client_sup` listen to what clients say, and hide TCP communication with them

This chat server lets clients exchange messages and see who is joining or leaving the conversation.

Communication is achieved by means of exchanging serialized Erlang terms, so it naturally is easier to write a client that is also running on the Erlang VM.

### Author

Piotr Anielski