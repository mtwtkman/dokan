TODO: ALL OF THEM

# Dokan
Dokan is a simple local reverse proxy for development.

Dokan runs separate listeners:
- HTTP on port 8080
- HTTPS on port 8443(fixed)

Since 80 and 443 ports are reserved, Dokan use 8080 and 8443 instead.

Dokan has the following restritcions:
- HTTP and HTTPS are treated as independent entry points
- Dokan does not modify, normalize, or reconcile protocol-specific behavior
- Application-level behavioral differences are the responseibility of the backend

## NOTES
- HTTPS routing is based on SNI hostname
- HTTP routing uses the Host header

Traffic Flow:
[Browser (domain-based routing)] -request-> [Dokan] -resolve-> [local server]

# Prerequisites
A trusted certificate authority must be registered on the system.

A self-signed development CA is acceptable if it is installed into the OS or browser trust store.

Dokan does not obtain certificates automatically.

# Usage
```bash
# Dokan reads "./dokan.yaml" as default.
dokan

# Or you can specify any file as routing mapper.
dokan /path/to/your-route.yaml
```

# Routing mapper
When Dokan reads like:
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
it routes to:
- https://a.b.com -> http://localhost:3000
- https://x.y.net -> http://localhost:8081
- http://p.z.com  -> http://localhost:8082
- http://q.z.com  -> http://localhost:8083

Backends are assumed to be plain HTTP endpoints.

A backend may itself be a reverse proxy such as nginx.

# Acknowledge
Dokan rewrites the Host header to the backend address by default like nginx.

The original host is forwarded using X-Forwarded-Host.

Preserving the original Host header is available as an opt-in feature.
