TODO: ALL OF THEM

# Dokan
Dokan is a simple local reverse proxy for development.

Dokan runs separate listeners:
- HTTP on port 8080
- HTTPS on port 8443 (fixed)

Since ports 80 and 443 are typically reserved by the operating system, Dokan uses 8080 and 8443. To use standard URLs (e.g., http://a.b.com), it is recommended to use a browser extension like SwitchyOmega to forward traffic to these ports.

## Behavior and Restrictions
- **TLS Termination**: Dokan acts as a TLS terminator. All backends are assumed to be plain HTTP endpoints.
- **Independent Entry Points**: HTTP and HTTPS are treated as independent entry points.
- **Header Rewriting**: Dokan rewrites the `Host` header to the backend address by default.
- **Forwarding Headers**: The original host is preserved in the `X-Forwarded-Host` header. The header `X-Forwarded-Proto: http` is also added to requests.

## NOTES
- HTTPS routing is based on the SNI hostname.
- HTTP routing uses the `Host` header.

Traffic Flow:
[Browser (via forwarder)] -> [Dokan (http 8080/https 8443)] -> [Local Backend Server]

# Prerequisites
A trusted certificate authority must be registered on the system. A self-signed development CA is acceptable if it is installed into the OS or browser trust store. Dokan does not obtain certificates automatically.

# Usage
```bash
# Dokan reads "./dokan.yaml" as default.
dokan

# Or you can specify any file as routing mapper.
dokan /path/to/your-route.yaml
```

# Configuration
Dokan requires a configuration of routing which names `dokan.yaml` as default.

Example:

```yaml
dns:
  defaultAddress: 127.0.0.1
hosts:
  - names:
      - a.b.io
      - x.y.io
    tls:
      cert: ./certs/cert.pem
      key: ./certs/cert-key.pem
    proxy:
      upstream: http://localhost:3000
    dns:
      address: 127.0.0.2
  - names:
      - "*.m.n.net"
    tls:
      cert: ./certs/wild.pem
      key: ./certs/wild-key.pem
    proxy:
      upstream: http://localhost:8081
```

## DNS

Dokan has simple DNS layer to avoid modifying your `/etc/hosts`.

You can manage Dokan's simple DNS layer setting by `dns` property.

| key | type | value | necessity |
| --- | --- | --- | --- |
| defaultAddress | string | Any request to a name in hosts will return this IP when queried via DNS. Capable for IPv4 and IPv6. | required |

## Hosts

Dokan routes following `hosts` property settings.

| key | type | value | necessity |
| --- | --- | --- | --- |
| names | string[] | Hostnames to be routed to proxy. Wildcard hostname must be matched only single section subdomain. | required |
| tls | object | Setting for tls resolution. | optional |
| tls.cert | string | Certificate file path. A value is relative then Dokan assumes entrypoint is current directory. | required |
| tls.key | string | Certificate file path. A value is relative then Dokan assumes entrypoint is current directory. | required |
| proxy | object | Setting for proxy. | required |
| proxy.upstream | string | Upstream server address. | required |
| dns | object | Setting for overwriting default dns. | optional |
| dns.address | string | An IP address to overwrite default one. Capable for IPv4 and IPv6. | required|

According to above example is mapped as like follow:

- `https://a.b.io` (SNI)    -> `http://localhost:3000`
- `http://a.b.io`           -> `http://localhost:3000`
- `https://x.y.io` (SNI)    -> `http://localhost:3000`
- `http://x.y.io`           -> `http://localhost:3000`
- `https://l.m.n.net` (SNI) -> `http://localhost:8081`
- `http://l.m.n.net`        -> `http://localhost:8081`

If `tls` property is not provided, Dokan only handle http request against that hostname.

# Acknowledge
Backends may be any local service or another reverse proxy such as Nginx.
