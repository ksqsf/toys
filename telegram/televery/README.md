# Televery

Televery is a hassle-free 2-step verification service, or more
accurately, server.

1. Televery server listens for verification requests via a TCP socket.
2. Some client asks for verification.
3. Televery sends you a verification message, which comprises
   essential information and two inline buttons "Pass" and "Deny".
   Meanwhile, the client waits for reply from Televery.
4. If you click "Pass", the client will be informed that the
   verification succeeded.
5. If you click "Deny" or it's timed out, the client will be noticed
   that the verification failed.

Televery is very user-friendly and easy-to-use. It just needs you to
incorporate it into your systems.

## Usage

Setting up Televery roughly takes you 3 steps.

### Create your own Telegram bot

Create a bot just for you! Refer to [the official documentation here](https://core.telegram.org/bots#3-how-do-i-create-a-bot).

### Configure and run Televery server

Consult `config.sample.yml` and write yours.

### Incorporate Televery into your system

That's it! Now you can teach your apps to use the the very simple
Televery protocol.

## Protocol

Televery protocol is a line-based (LF w/o CR) ASCII-encoded query
protocol.  Right now, only one method `REQ` is supported.

First, a client sends
```
REQ <appname>\n
```
and receives either "ALLOW" or "DENY".

* `ALLOW`: the request is passed.
* `DENY`: the request is denied.

However, due to the current implementation, you should not assume
there's always a response. Always pack up with time-outs.
