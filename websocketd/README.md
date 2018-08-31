# websocketd

websocketd is a very simple WebSocket server implementation using
Tokio.  It makes heavy use of tokio-codec.

websocketd can pipe a WebSocket connection to some program, and send
program output back to the WebSocket connection.

## Testing

``` shell
$ cargo run -- ./echo.py
```
