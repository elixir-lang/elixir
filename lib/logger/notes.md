## Distributed Logger Proposal

### Issue Addressed

As Elixir apps are deployed across dozens of nodes, it becomes harder to
manage and inspect all the independent log files. Some, like Bleacher
Report, resort to [third party solutions](https://dev.bleacherreport.com/elixir-phoenix-centralized-http-logging-aa50efe3105b).

This seems unnecessary, as this kind of distributed solution is square
in Elixir's sweet spot.

### Proposal

1. Alter the `console` logger backend to accept a new configuration
   parameter, `known_as`. This would publish the a name for this logger.
   The name could be an atom, is which case `global` is used, or it
   could be a `:via` tuple.

2. Create a new logger backend, `forward`. This will be configured to
   take the name of the logger created in (1), along with

   * an optional identifying string (defaults to the node name)
   * a minimum log level to send (defaults to `error`). This is
     distinct from the logger's own error level, and controls the
     amount of traffic sent over the network.

### In Use

1. The `forward` backend will typically be configured in parallel with
   an existing backend (such as console). That way the logger on a
   client machine will continue to log locally even if the logging
   server goes down. (maybe: If the server does go down, the forward backend
   will enter an exponential backoff poll for it to come back.)

2. We'd recommend that folks run the logging server in a separate
   top-level application, isolating it from failures of application
   level components.

3. On the logging server, messages received via `forward` from another
   node will be logged normally, and hence will follow the logging
   server's log configuration. (Note that this even permits a hierarchy
   of loggers...)
