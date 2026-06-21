# Serving the node REST API over HTTPS

By default the Ergo node serves its REST API over plain HTTP. Anyone able to
observe the traffic between a client and the node can read the `api_key` header
and any other sensitive data. To protect the API in transit you can either:

1. **Terminate TLS at a reverse proxy** (nginx, Caddy, Traefik, …) in front of
   the node — recommended for public deployments, and
2. **Let the node terminate TLS itself** via the built-in HTTPS support
   described below — convenient when you don't want to run a proxy.

The node loads its certificate and private key from a Java keystore (PKCS12 or
JKS). This guide covers both approaches.

## Reverse proxy (TLS termination)

Recommended for public deployments: run nginx/Caddy/Traefik in front of the node,
let it terminate TLS, and forward requests to the node over loopback.

The critical step is to **stop exposing the node's plain HTTP API directly**. The
default `bindAddress = "0.0.0.0:9053"` listens on every interface, so even with a
proxy in place, anyone who can reach the node's host on that port bypasses TLS and
can intercept the `api_key`. Bind the API to loopback so only the proxy (running
on the same host) can reach it:

```hocon
scorex {
  restApi {
    # Only the local reverse proxy can reach the plain HTTP API.
    bindAddress = "127.0.0.1:9053"
    apiKeyHash  = "<blake2b256 hash of your API key>"

    # https stays disabled — the proxy terminates TLS.
    # Advertise the proxy's public HTTPS endpoint:
    publicUrl = "https://node.example.com"
  }
}
```

Additionally:

- If the proxy runs on a different host, bind to the private interface the proxy
  uses (not `0.0.0.0`) and firewall the API port so it is unreachable from the
  public internet — e.g. `ufw deny 9053` / a security-group rule — allowing only
  the proxy's address.
- Configure the proxy to pass the request through unchanged (it forwards the
  caller's `api_key` header to `http://127.0.0.1:9053`).

## Native TLS in the node

Convenient when you don't want to run a proxy: the node terminates TLS itself.

```hocon
scorex {
  restApi {
    bindAddress = "0.0.0.0:9053"
    apiKeyHash  = "<blake2b256 hash of your API key>"

    https {
      enabled          = true
      keyStorePath     = "/etc/ergo/ergo-api.p12"
      # Prefer an environment variable over storing the password in the file:
      keyStorePassword = ${?ERGO_API_KEYSTORE_PASSWORD}
      keyStoreType     = "PKCS12"   # or "JKS"
    }

    # Advertise the HTTPS endpoint so peers/clients discover the node over TLS.
    publicUrl = "https://node.example.com:9053"
  }
}
```

Notes:

- When `https.enabled = false` (or the `https { }` block is absent) the API is
  served over plain HTTP exactly as before — fully backward compatible.
- `keyStoreType` defaults to `PKCS12` if omitted.
- The keystore password is also used as the key (entry) password, so create the
  keystore with a single password for both (the default for PKCS12).
- A misconfigured keystore (missing file, wrong password/type) aborts node
  startup with a clear configuration error before the rest of the node starts.
- If `https.enabled = true` but `publicUrl` advertises a non-`https://` address,
  the node **refuses to start** — clients discovering it would otherwise keep
  sending the API key over plain HTTP. Set `publicUrl` to the matching `https://`
  address, or, if you really intend the insecure combination, opt out explicitly
  with `https.allowInsecurePublicUrl = true` (the node then only logs a warning).
  TLS terminated by a reverse proxy is unaffected: leave `https.enabled = false`
  and point `publicUrl` at the proxy's `https://` endpoint.
- If `https.enabled = true` and `publicUrl` is unset, the node logs a warning
  (clients won't discover the HTTPS endpoint) but still starts.

## Creating a keystore

### Self-signed certificate (development / private use)

```bash
keytool -genkeypair \
  -alias ergo-api -keyalg RSA -keysize 2048 -validity 365 \
  -storetype PKCS12 -keystore ergo-api.p12 \
  -storepass "$ERGO_API_KEYSTORE_PASSWORD" \
  -dname "CN=localhost" \
  -ext "SAN=dns:localhost,ip:127.0.0.1"
```

The Subject Alternative Name (`-ext SAN=…`) is **required** by most modern
clients and browsers — a certificate with only a `CN` is rejected. List every
hostname and IP clients will use, e.g.
`-ext "SAN=dns:node.example.com,dns:localhost,ip:127.0.0.1"`.

### From a CA / Let's Encrypt certificate (production)

Let's Encrypt (certbot) emits PEM files (`fullchain.pem`, `privkey.pem`).
Convert them to a PKCS12 keystore with OpenSSL:

```bash
openssl pkcs12 -export \
  -in  /etc/letsencrypt/live/node.example.com/fullchain.pem \
  -inkey /etc/letsencrypt/live/node.example.com/privkey.pem \
  -name ergo-api \
  -out ergo-api.p12 \
  -passout env:ERGO_API_KEYSTORE_PASSWORD
```

Re-run this conversion (and restart the node) on each certificate renewal.

## Trusting the certificate from clients

- **CA-issued certificate:** clients (curl, browsers, SDKs) trust it
  automatically — no extra steps.
- **Self-signed certificate:** clients will reject it unless they trust it
  explicitly. Do **not** disable certificate verification in production (e.g.
  `curl -k`), as that re-opens the interception risk this feature prevents.
  Instead, distribute the certificate and add it to the client's trust store:

  ```bash
  # Export the certificate from the keystore:
  keytool -exportcert -rfc -alias ergo-api \
    -keystore ergo-api.p12 -storetype PKCS12 \
    -storepass "$ERGO_API_KEYSTORE_PASSWORD" -file ergo-api.crt

  # Then pin/trust it from the client, e.g. with curl:
  curl --cacert ergo-api.crt https://node.example.com:9053/info
  ```
