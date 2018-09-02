# websocketd

websocketd is a very simple WebSocket server implementation using
Tokio.  It makes heavy use of tokio-codec.

websocketd can pipe a WebSocket connection to some program, and send
program output back to the WebSocket connection.

## Testing

``` shell
$ cargo run -- ./echo.py
```

## Performance

Very simple stress test and benchmarking shows that my websocketd
performs as well as (if not better) another
[websocketd](https://github.com/joewalnes/websocketd) which has nearly
10k stars.
