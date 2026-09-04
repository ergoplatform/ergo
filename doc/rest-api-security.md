# REST API configuration migration

The bundled REST API now binds to `127.0.0.1` (port `9053` on mainnet,
`9052` otherwise). Protected routes return HTTP 403 until the operator configures
an API key hash. Public routes remain accessible on the configured interface.

A node can now start with `scorex.restApi.apiKeyHash = null`, leaving protected
routes disabled. An omitted key inherits the same default. Empty or malformed
hashes cannot authenticate a request. The former bundled `hello` credential is
rejected even if its hash remains in an existing configuration.

## Configure authenticated access

Choose a unique, high-entropy API key. Set `scorex.restApi.apiKeyHash` to its
64-character, lowercase hexadecimal Blake2b256 hash in the operator's node
configuration. Send the original key in the `api_key` request header. Store the
key securely and do not reuse credentials from examples or integration tests.

Wallet, scan, node administration, block submission and other routes that
already require authentication now enforce this configuration requirement.
Existing clients with a correctly configured, non-default key keep working.
Clients that relied on `null` or `hello` must configure a new key before using
protected routes. P2P networking and consensus rules are unchanged.

## Configure remote or container access

An existing explicit `scorex.restApi.bindAddress` continues to override the
bundled default. Operators who require remote REST access must set that address
explicitly, configure an API key, and restrict access with network controls.
Use an encrypted connection for API credentials, such as a TLS reverse proxy
or a secured tunnel; the bind address and API key do not provide encryption.

In a container, a host port mapping cannot reach a service bound only to the
container's loopback interface. Set the container's REST bind address explicitly
(for example, `0.0.0.0:9053` on mainnet), then limit the published host port to a
trusted interface and apply the same authentication and transport controls.

The shipped local node profiles with `apiKeyHash = null` also require an
operator-supplied hash for protected routes. Public health and query routes
remain available without a key.
