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

# Routing mapper
Example configuration (`dokan.yaml`):
```yaml
tls:
  - cert: certs/cert.pem
    key: certs/key.pem
    hosts:
      - a.b.com
      - x.y.net
  - cert: certs/wildcard.pem
    key: certs/wildcard-key.pem
    hosts:
      - "*.z.com"
hosts:
  a.b.com: localhost:3000
  x.y.net: localhost:8081
  p.z.com: localhost:8082
  q.z.com: localhost:8083
```

With this config, traffic resolves as follows:
- `https://a.b.com` (SNI) -> `http://localhost:3000`
- `https://x.y.net` (SNI) -> `http://localhost:8081`
- `http://p.z.com` (Host Header) -> `http://localhost:8082`
- `http://q.z.com` (Host Header) -> `http://localhost:8083`

# Acknowledge
Backends may be any local service or another reverse proxy such as Nginx.
