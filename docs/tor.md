# Running an Ergo Node Behind Tor

This guide explains how to run an Ergo node with its network traffic routed through
[Tor](https://www.torproject.org/), providing network-level privacy for node operators.

It covers threat modeling, deployment options, Tor Hidden Services, and critical
operational security (OpSec) considerations specific to the Ergo JVM/Scorex/Akka stack.

> **Reference:** This guide addresses [issue #970](https://github.com/ergoplatform/ergo/issues/970)
> and is modeled after Bitcoin Core's [`doc/tor.md`](https://github.com/bitcoin/bitcoin/blob/master/doc/tor.md).

---

## Table of Contents

1. [Why Run Behind Tor?](#1-why-run-behind-tor)
2. [⚠️ CRITICAL: The JVM SOCKS Proxy Trap](#2--critical-the-jvm-socks-proxy-trap)
3. [Prerequisites](#3-prerequisites)
4. [Option A — Docker Network Isolation (Recommended)](#4-option-a--docker-network-isolation-recommended)
5. [Option B — Whonix / Tor Gateway VM](#5-option-b--whonix--tor-gateway-vm)
6. [Option C — iptables Transparent Proxy (Advanced)](#6-option-c--iptables-transparent-proxy-advanced)
7. [Tor Hidden Service (Onion Address)](#7-tor-hidden-service-onion-address)
8. [Ergo-Specific Configuration Reference](#8-ergo-specific-configuration-reference)
9. [Security Considerations](#9-security-considerations)
10. [Troubleshooting](#10-troubleshooting)
11. [Further Reading](#11-further-reading)

---

## 1. Why Run Behind Tor?

Running an Ergo node behind Tor provides several benefits:

- **IP Privacy:** Your node's real IP address is hidden from peers, preventing observers
  from linking your node to your physical location or identity.
- **Censorship Resistance:** In jurisdictions that restrict blockchain network traffic,
  Tor can bypass ISP-level filtering.
- **Eclipse Attack Mitigation:** When used in combination with clearnet connections
  (dual-stack), Tor makes it harder for an attacker to isolate your node.

> **Note:** Tor adds latency to network communications (typically 200–800 ms per hop).
> For **mining nodes** that need minimal latency for block propagation, this can lead to
> stale blocks and is **not recommended**. For non-mining full nodes and wallet nodes,
> the latency impact is negligible.

---

## 2. ⚠️ CRITICAL: The JVM SOCKS Proxy Trap

### Do NOT use `-DsocksProxyHost`

You may encounter guides, scripts, or even the official
[`ergo-installer.sh --tor`](https://github.com/ergoplatform/ergo/blob/master/ergo-installer.sh#L149)
that inject JVM system properties:

```bash
# ⚠️ THIS DOES NOT WORK FOR P2P TRAFFIC — DO NOT USE ⚠️
java -DsocksProxyHost=127.0.0.1 -DsocksProxyPort=9050 -jar ergo.jar
```

**Why this leaks your IP:**

Ergo uses the Scorex network framework, which builds on **Akka IO TCP**
(`akka.io.Tcp`). Under the hood, Akka IO operates entirely on **Java NIO**
(`java.nio.channels.SocketChannel`).

The JVM SOCKS proxy properties (`-DsocksProxyHost`, `-DsocksProxyPort`) only apply to
the legacy blocking socket API (`java.net.Socket` via `SocksSocketImpl`). Java NIO
channels bypass this implementation entirely and connect directly via native OS system
calls (`sun.nio.ch.Net.connect`).

**Result:** If you use `-DsocksProxyHost`, your REST API HTTP calls may be proxied
(they use `java.net.URL`), but **100% of your P2P gossip traffic — blocks, transactions,
peer discovery — goes out in plaintext over the clearnet**, exposing your real IP to
every peer you connect to.

This is not a bug in Ergo or Akka; it is a well-documented fundamental limitation of
the JDK NIO architecture. See:
[JDK-8033696](https://bugs.openjdk.org/browse/JDK-8033696),
[Oracle NIO Proxy Docs](https://docs.oracle.com/javase/8/docs/technotes/guides/net/proxies.html).

### Do NOT use `torsocks`

You may also find suggestions to use `torsocks` (an `LD_PRELOAD`-based interceptor):

```bash
# ⚠️ UNRELIABLE WITH THE JVM — DO NOT USE ⚠️
torsocks java -jar ergo.jar
```

`torsocks` works by hooking libc network calls (`connect()`, `sendto()`, etc.) via
`LD_PRELOAD`. However, the JVM frequently bypasses libc through JNI native code and
direct system calls, making `torsocks` unreliable:

- The JVM may silently bypass the hooked calls, leaking traffic.
- `LD_PRELOAD` conflicts can cause crashes, deadlocks, or segmentation faults.
- `torsocks` cannot intercept raw syscalls that bypass libc entirely.

### The Only Safe Approach: Network-Level Isolation

To securely route Ergo traffic through Tor, you must enforce proxying **below** the
JVM — at the network/OS level. The three proven approaches are:

| Method | Complexity | Security | Platform |
|--------|------------|----------|----------|
| **Docker network isolation** | Low | High | Linux, macOS, Windows |
| **Whonix / Tor Gateway VM** | Medium | Very High | Any (VM) |
| **iptables transparent proxy** | High | Very High | Linux only |

All three methods ensure that **every packet** leaving the Ergo process is routed
through Tor, regardless of which socket API the JVM uses internally.

---

## 3. Prerequisites

| Component           | Version    | Notes                                    |
|---------------------|------------|------------------------------------------|
| Ergo node           | ≥ 5.0      | Any recent release `.jar` or Docker image |
| Tor                 | ≥ 0.4.x    | System package or containerized          |
| Docker + Compose    | ≥ 20.10    | For Option A (recommended)               |

---

## 4. Option A — Docker Network Isolation (Recommended)

This is the recommended approach. The Ergo container **shares the network namespace**
of the Tor container using `network_mode: "service:tor"`. This means both processes
share the same network stack — iptables `OUTPUT` rules in that namespace apply directly
to all traffic generated by the JVM, making bypass impossible.

> **Why not a separate bridge network?** In a standard Docker bridge setup, the Ergo
> container's default gateway is the Docker bridge host IP (e.g., `172.28.0.1`), not
> the Tor container. TCP traffic from Ergo would route through the bridge directly to
> the internet, completely bypassing the Tor container. The shared-namespace pattern
> eliminates this leak by design.

### Architecture

```
┌─────────────────────────────────────────────────┐
│         Shared Network Namespace (Pod)           │
│                                                  │
│  ┌─────────────────┐  ┌──────────────────────┐   │
│  │   Tor daemon    │  │   Ergo JVM           │   │
│  │  (uid: tor)     │  │   (uid: ergo)        │   │
│  │  TransPort:9040 │  │   P2P:9030           │   │
│  │  DNSPort:5353   │  │   API:9053           │   │
│  └─────────────────┘  └──────────────────────┘   │
│                                                  │
│  iptables OUTPUT rules (order matters!):         │
│  ✓ uid=tor  → RETURN (direct internet)           │
│  ✓ DNS:53   → REDIRECT to DNSPort :5353          │
│  ✓ loopback → RETURN (local API works)           │
│  ✓ TCP      → REDIRECT to TransPort :9040        │
│  ✓ ESTAB    → ACCEPT (API responses)             │
│  ✗ ALL ELSE → DROP (leak prevention)             │
│  ✗ IPv6     → DROP (ip6tables kill-switch)       │
└──────────┬───────────────────────────────────────┘
           │
           ▼ (Tor circuit to the internet)
```

### `torrc`

```
# Tor configuration for transparent proxying
SocksPort 0.0.0.0:9050
TransPort 0.0.0.0:9040
DNSPort 0.0.0.0:5353
AutomapHostsOnResolve 1
VirtualAddrNetworkIPv4 10.192.0.0/10

# Optional: Hidden Service (uncomment to enable)
# HiddenServiceDir /var/lib/tor/ergo-node/
# HiddenServicePort 9030 127.0.0.1:9030
```

### `docker-compose.yml`

```yaml
version: "3.8"

services:
  tor:
    image: alpine:latest
    cap_add:
      - NET_ADMIN
    command: >
      sh -c "
        rm -f /tmp/fw_locked &&
        apk add --no-cache tor iptables su-exec &&
        chown -R tor:tor /var/lib/tor &&

        # --- IPv4 NAT Table (connection rewriting) ---

        # 1. Allow the Tor process itself to reach the internet directly
        iptables -t nat -A OUTPUT -m owner --uid-owner tor -j RETURN &&

        # 2. Redirect DNS BEFORE loopback exclusion
        #    (Docker uses 127.0.0.11 as DNS — without this, DNS leaks to clearnet)
        iptables -t nat -A OUTPUT -p udp --dport 53 -j REDIRECT --to-ports 5353 &&
        iptables -t nat -A OUTPUT -p tcp --dport 53 -j REDIRECT --to-ports 5353 &&

        # 3. Exclude loopback (allows local REST API access on 127.0.0.1:9053)
        iptables -t nat -A OUTPUT -d 127.0.0.0/8 -j RETURN &&

        # 4. Redirect ALL other TCP (i.e., Ergo P2P) to Tor TransPort
        iptables -t nat -A OUTPUT -p tcp -j REDIRECT --to-ports 9040 &&

        # --- IPv4 Filter Table (packet security) ---

        # 5. Allow responses to inbound connections (fixes REST API drop via bridge IP)
        iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT &&

        # 6. Allow Tor and loopback
        iptables -A OUTPUT -m owner --uid-owner tor -j ACCEPT &&
        iptables -A OUTPUT -d 127.0.0.0/8 -j ACCEPT &&

        # 7. DROP everything else (IPv4)
        iptables -A OUTPUT -j DROP &&

        # --- IPv6 Kill-Switch ---
        # 8. Block ALL IPv6 output (prevents JVM from bypassing IPv4 rules)
        ip6tables -A OUTPUT -j DROP || true &&

        # CRITICAL: Signal that the firewall is fully locked down
        touch /tmp/fw_locked &&

        # Start Tor via su-exec (Alpine standard for PID 1 privilege drop)
        exec su-exec tor tor -f /etc/tor/torrc
      "
    restart: unless-stopped
    volumes:
      - tor-data:/var/lib/tor
      - ./torrc:/etc/tor/torrc:ro
    ports:
      # Ports declared HERE because Ergo shares this network namespace
      - "127.0.0.1:9053:9053"
    healthcheck:
      # Gate: Ergo will not start until this passes (prevents startup IP leak)
      test: ["CMD", "test", "-f", "/tmp/fw_locked"]
      interval: 1s
      timeout: 2s
      retries: 30

  ergo:
    image: ergoplatform/ergo:latest
    # CRITICAL: Share the Tor container's network namespace.
    # All Ergo traffic goes through the same iptables rules as Tor.
    network_mode: "service:tor"
    restart: unless-stopped
    depends_on:
      tor:
        # CRITICAL: Wait until the iptables firewall is fully active.
        # Without this, Ergo's JVM boots faster than the iptables setup,
        # causing a 2-3 second IP leak on every container restart.
        condition: service_healthy
    environment:
      MAX_HEAP: "4G"
      # Force IPv4 stack — prevents JVM from opening IPv6 sockets that bypass iptables
      _JAVA_OPTIONS: "-Djava.net.preferIPv4Stack=true"
    volumes:
      - ergo-data:/home/ergo/.ergo
      - ./ergo.conf:/home/ergo/ergo.conf:ro
    command: ["--mainnet", "-c", "/home/ergo/ergo.conf"]

volumes:
  tor-data:
  ergo-data:
```

### `ergo.conf`

```hocon
ergo {
  node {
    mining = false
    utxo {
      # Fast sync recommended for Tor (reduces sync from days to ~1 hour)
      utxoBootstrap = true
      storingUtxoSnapshots = 0
    }
    nipopow {
      nipopowBootstrap = true
      p2pNipopows = 2
    }
  }
}

scorex {
  restApi {
    # Shared namespace — bind to 0.0.0.0 is safe (iptables DROP prevents leaks)
    bindAddress = "0.0.0.0:9053"
    # IMPORTANT: Change this! Generate your own with: curl -s http://127.0.0.1:9053/utils/hash/blake2b -d '"YOUR_SECRET"'
    # Default below is Blake2b256("hello") — NOT SECURE for production use.
    apiKeyHash = "324dcf027dd4a30a932c441f365a25e86b173defa4b8e58948253471b81b72cf"
  }

  network {
    bindAddress = "0.0.0.0:9030"

    # CRITICAL: Disable UPnP — it would expose your real IP via your router
    upnpEnabled = no

    # Do NOT set declaredAddress (see Section 7 for .onion limitations)
    # declaredAddress = ""
  }
}
```

### Launch

```bash
docker compose up -d
docker compose logs -f ergo
```

### Verification

```bash
# Verify iptables rules are active inside the shared namespace
docker compose exec tor iptables -t nat -L OUTPUT -n --line-numbers

# Verify Ergo cannot reach the internet directly (should be blocked by DROP)
docker compose exec tor ping -c1 8.8.8.8  # Should FAIL

# Verify the REST API is accessible locally on the host
curl -s http://127.0.0.1:9053/info
```

---

## 5. Option B — Whonix / Tor Gateway VM

For maximum security, run the Ergo node inside a
[Whonix Workstation](https://www.whonix.org/) VM. Whonix enforces all traffic through
a separate Tor Gateway VM at the hypervisor level, making IP leaks architecturally
impossible — even if the JVM makes direct syscalls.

### Setup

1. Install [Whonix](https://www.whonix.org/wiki/Download) (VirtualBox or KVM).
2. Inside the **Whonix-Workstation** VM, install Java and download the Ergo `.jar`.
3. Run the node normally — no special proxy flags needed (add `-Djava.net.preferIPv4Stack=true`
   to prevent IPv6 leaks, though Whonix blocks these at the gateway level as well):
   ```bash
   java -Xmx4G -Djava.net.preferIPv4Stack=true -jar ergo-*.jar --mainnet -c ergo.conf
   ```
4. Use the same `ergo.conf` as Option A, but with `bindAddress = "0.0.0.0:..."` since
   Whonix handles network isolation at the VM level.

Whonix is considered the gold standard for Tor-enforced network isolation and is the
approach recommended by the Tor Project for high-security use cases.

---

## 6. Option C — iptables Transparent Proxy (Advanced)

> **⚠️ OPSEC WARNING:** This bare-metal UID-filtering approach is **strictly inferior**
> to Docker Network Namespaces (Option A) or Whonix (Option B). It suffers from two
> severe Linux architectural edge cases:
>
> 1. **DNS IPC Leaks:** If your OS uses `systemd-resolved` (Ubuntu/Debian default),
>    the JVM delegates DNS resolution to the OS via a local Unix socket (D-Bus IPC).
>    The actual DNS packet will be sent by the `systemd-resolve` user, bypassing
>    your `ergonode` UID iptables rules entirely and leaking to the clearnet.
>    **Mitigation:** Disable `systemd-resolved` and use a static `/etc/resolv.conf`
>    pointing to `127.0.0.1` (your local Tor DNSPort).
> 2. **Ephemeral Rules:** `iptables` rules vanish on reboot. If you do not persist
>    them (e.g., `sudo apt install iptables-persistent && sudo netfilter-persistent save`),
>    your node will leak 100% of its traffic after a server restart.
>
> **Option A is strongly recommended instead.**

On a dedicated Linux host, you can use `iptables` to create a transparent proxy that
forces all traffic from the Ergo process through Tor. This is the bare-metal equivalent
of the Docker approach.

### Step 1 — Configure Tor

Edit `/etc/tor/torrc`:

```
TransPort 9040
DNSPort 5353
AutomapHostsOnResolve 1
VirtualAddrNetworkIPv4 10.192.0.0/10
```

Restart Tor: `sudo systemctl restart tor`

### Step 2 — Create a dedicated user

```bash
sudo useradd -r -s /usr/sbin/nologin ergonode
```

### Step 3 — iptables rules

> **Rule order matters.** In Netfilter, the `nat` table is evaluated before the
> `filter` table. After `REDIRECT` changes the destination to `127.0.0.1:9040`, the
> packet passes through the `filter OUTPUT` chain. Without a loopback exemption, the
> `DROP` rule would kill the redirected packets, breaking all connectivity.
>
> Additionally, DNS must be redirected **before** the loopback exclusion. On many
> Linux systems, `systemd-resolved` listens on `127.0.0.53` — if the loopback rule
> fires first, DNS queries escape to the clearnet via the system resolver.

```bash
# --- NAT table (connection rewriting) ---

# 1. Redirect DNS BEFORE loopback exclusion (catches systemd-resolved at 127.0.0.53)
sudo iptables -t nat -A OUTPUT -m owner --uid-owner ergonode \
  -p udp --dport 53 -j REDIRECT --to-ports 5353
sudo iptables -t nat -A OUTPUT -m owner --uid-owner ergonode \
  -p tcp --dport 53 -j REDIRECT --to-ports 5353

# 2. Exclude loopback traffic from redirection (preserves local REST API access)
sudo iptables -t nat -A OUTPUT -m owner --uid-owner ergonode \
  -d 127.0.0.0/8 -j RETURN

# 3. Redirect all TCP from ergonode through Tor's TransPort
sudo iptables -t nat -A OUTPUT -m owner --uid-owner ergonode \
  -p tcp -j REDIRECT --to-ports 9040

# --- FILTER table (packet acceptance/rejection) ---

# 4. Allow responses to inbound connections (required for REST API via LAN IP)
sudo iptables -A OUTPUT -m owner --uid-owner ergonode \
  -m state --state ESTABLISHED,RELATED -j ACCEPT

# 5. Allow loopback traffic (required for REDIRECT'd packets and local API)
sudo iptables -A OUTPUT -m owner --uid-owner ergonode \
  -d 127.0.0.0/8 -j ACCEPT

# 6. Block all other outgoing clearnet traffic (safety net against UDP/ICMP leaks)
sudo iptables -A OUTPUT -m owner --uid-owner ergonode -j DROP

# --- IPv6 Kill-Switch ---

# 7. Prevents JVM from bypassing IPv4 rules via IPv6 sockets
sudo ip6tables -A OUTPUT -m owner --uid-owner ergonode -j DROP
```

### Step 4 — Run Ergo as the isolated user

```bash
sudo -u ergonode java -Xmx4G -Djava.net.preferIPv4Stack=true \
  -jar ergo-*.jar --mainnet -c ergo.conf
```

All TCP traffic from this user is transparently redirected through Tor via the
`TransPort`. DNS is intercepted before the loopback exemption to prevent clearnet
resolver leaks. Loopback traffic (REST API on `127.0.0.1:9053`) is preserved.
IPv6 is killed at both the JVM and kernel level. All remaining traffic is dropped.

---

## 7. Tor Hidden Service (Onion Address)

A Hidden Service allows other Tor-enabled Ergo nodes to connect **to** your node via a
`.onion` address. This makes your node a full participant in the Tor overlay network.

### Important Limitation

> **⚠️ Scorex does not natively gossip `.onion` addresses.** The Ergo P2P protocol
> serializes peer addresses as IPv4 (4 bytes) or IPv6 (16 bytes). It cannot gossip
> `.onion` hostnames over the network.
>
> Furthermore, setting a `.onion` address in `declaredAddress` will cause Java's
> `InetAddress.getAllByName()` to throw `UnknownHostException` at startup, because
> `.onion` TLDs are not resolvable via standard DNS.
>
> **You must leave `declaredAddress` unset.** Peers who wish to connect to your onion
> node must add your `.onion:9030` manually to their `knownPeers`.

### Configuration

In your Tor configuration (`torrc` or the Docker volume):

```
HiddenServiceDir /var/lib/tor/ergo-node/
HiddenServicePort 9030 127.0.0.1:9030
```

Retrieve your `.onion` address:

```bash
# Native install
sudo cat /var/lib/tor/ergo-node/hostname

# Docker
docker compose exec tor cat /var/lib/tor/ergo-node/hostname
```

For peers to connect to you, they must add your onion address to their config:

```hocon
scorex.network.knownPeers = [
  "<YOUR_ONION_ADDRESS>.onion:9030"
]
```

### Transparent Outbound `.onion` Connectivity (Emergent Property)

An important consequence of the transparent proxy architecture (Options A & C) is
that Ergo can **connect outbound to `.onion` peers without any code changes**.

When a peer's `.onion` address is added to `knownPeers`, the following chain occurs
automatically:

1. Java resolves the `.onion` hostname via a DNS query.
2. The `iptables` DNS redirect sends this query to Tor's `DNSPort`.
3. Tor's `AutomapHostsOnResolve` assigns a virtual IP (e.g., `10.192.0.5`) from the
   `VirtualAddrNetworkIPv4` subnet and returns it to Java.
4. Java initiates a TCP connection to `10.192.0.5:9030`.
5. The `iptables` TCP redirect sends this connection to Tor's `TransPort`.
6. Tor recognizes the virtual IP as a mapped `.onion` address and routes the
   connection through the Tor network to the destination Hidden Service.

This means Ergo gains transparent `.onion` peer connectivity at the network level,
with zero awareness of Tor at the application layer. The Scorex P2P protocol sees
ordinary IPv4 addresses throughout.

> **Note:** This only works for **outbound** connections (your node connecting to a
> peer's `.onion`). For **inbound** connections (other nodes reaching you), you must
> configure a Hidden Service as shown above.

---

## 8. Ergo-Specific Configuration Reference

### Port Reference

| Port   | Protocol   | Default Bind     | Purpose                         |
|--------|------------|------------------|---------------------------------|
| `9030` | TCP (P2P)  | `0.0.0.0:9030`  | Peer-to-peer network (mainnet)  |
| `9053` | HTTP (API) | `0.0.0.0:9053`  | REST API (mainnet)              |
| `9020` | TCP (P2P)  | `0.0.0.0:9020`  | Peer-to-peer network (testnet)  |
| `9052` | HTTP (API) | `0.0.0.0:9052`  | REST API (testnet)              |

### Critical Settings for Tor

| Setting                         | Tor Value               | Why                                                  |
|---------------------------------|-------------------------|------------------------------------------------------|
| `scorex.network.bindAddress`    | `"127.0.0.1:9030"` *   | Prevents listening on public interfaces               |
| `scorex.restApi.bindAddress`    | `"127.0.0.1:9053"` *   | API must never be exposed publicly                    |
| `scorex.network.declaredAddress`| *(unset)*               | Setting a clearnet IP defeats Tor; `.onion` will crash |
| `scorex.network.upnpEnabled`   | `no`                    | UPnP would expose your real IP via your router        |

\* Use `0.0.0.0` in Docker/VM setups where network isolation is enforced at the
container/hypervisor level.

### Fast Sync (Recommended for Tor)

Syncing the full blockchain over Tor is slow. Use UTXO bootstrap + NiPoPoW proofs for
a fast initial sync (~1 hour instead of days):

```hocon
ergo.node.utxo.utxoBootstrap = true
ergo.node.utxo.storingUtxoSnapshots = 0
ergo.node.nipopow.nipopowBootstrap = true
ergo.node.nipopow.p2pNipopows = 2
```

---

## 9. Security Considerations

### API Key — Change the Default!

Generate your own Blake2b256 hash after starting the node:

```bash
curl -s http://127.0.0.1:9053/utils/hash/blake2b \
  -H "Content-Type: application/json" \
  -d '"YOUR_SECRET_API_KEY"'
```

Replace the `apiKeyHash` value in your `ergo.conf` with the output, then restart.

### Sybil / Eclipse Attacks

Running exclusively over Tor increases exposure to Sybil attacks, since an adversary
can cheaply spin up many Tor relays. Mitigations:

- **Dual-stack (recommended):** Run one node on clearnet and a second on Tor.
  A single honest clearnet peer is enough to prevent an eclipse attack.
- **Trusted seed peers:** Hardcode known-good peers in your config:
  ```hocon
  scorex.network.knownPeers = [
    "213.239.193.208:9030",
    "159.65.11.55:9030",
    "165.227.26.175:9030"
  ]
  ```

### Hidden Service Security

- **Intersection attacks:** If your ISP cuts your internet and the hidden service goes
  offline simultaneously, your identity may be correlated.
- **Uptime fingerprinting:** A hidden service with consistent uptime patterns may be
  fingerprintable.
- **Key protection:** Guard the contents of your `HiddenServiceDir`.

### UPnP Auto-Doxxing

While `upnpEnabled` defaults to `no` in Ergo's `application.conf`, always verify it is
explicitly disabled in your Tor config. If UPnP is enabled, the node will attempt to
punch a port through your router and expose your real IP to the P2P network.

---

## 10. Troubleshooting

### Node cannot find peers

1. Verify Tor is running and accessible from the Ergo container/process.
2. Ensure `knownPeers` contains reachable seed nodes.
3. For Docker setups, verify the Tor `TransPort` is listening:
   ```bash
   docker compose exec tor ss -tlnp | grep 9040
   ```

### Extremely slow sync

This is expected for initial full-chain sync over Tor. Use UTXO bootstrap mode
(see [Section 8](#fast-sync-recommended-for-tor)) to reduce sync time from days to
approximately 1 hour.

### "Connection refused" on API

Ensure `scorex.restApi.bindAddress` matches the port you're trying to reach. For Docker
deployments, the API is available on the host at `127.0.0.1:9053`.

### Hidden Service not reachable

1. Check that Tor created the hostname file (see [Section 7](#configuration)).
2. Verify the `HiddenServicePort` matches the node's `bindAddress` port.
3. Check Tor logs: `sudo journalctl -u tor -f` or `docker compose logs tor`

### Node crashes with `UnknownHostException`

You likely set a `.onion` address in `declaredAddress`. Remove it — see
[Section 7](#important-limitation).

---

## 11. Further Reading

- [Tor Project — Documentation](https://support.torproject.org/)
- [Bitcoin Core — `doc/tor.md`](https://github.com/bitcoin/bitcoin/blob/master/doc/tor.md)
- [Whonix — Anonymous Operating System](https://www.whonix.org/)
- [Ergo Node — Configuration Reference](https://docs.ergoplatform.com/node/conf/)
- [JDK-8033696 — NIO Proxy Support (Open)](https://bugs.openjdk.org/browse/JDK-8033696)
