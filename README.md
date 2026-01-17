TODO: ALL OF THEM

# Dokan
Dokan is a simple local reverse proxy for development.

Dokanr runs separate listeners:
- HTTP on port 80
- HTTPS on port 443(fixed)


Dokan has the following restritcions:
- HTTP and HTTPS are treated as independent entry points
- Dokan does not modify, normalize, or reconcile protocol-specific behavior
- Application-level behavioral differences are the responseibility of the backend

--
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
You must create `route.yaml` first, then start Dokan using `docker` or `dokan /path/to/route.yaml`.

# route.yaml
When Dokan reads like:
```yaml
https:
  my.develop.host:
    localhost:3000
  my.another.host:
    localhost:8080
http:
  my.develop.host:
    localhost:3000
  my.host:
    localhost:80
```
,it routes like:
- https://my.develop.host -> http://localhost:3000
- https://my.another.host -> http://localhost:8080
- http://my.develop.host  -> http://localhost:3000
- http://my.host          -> http://localhost:80

Backends are assumed to be plain HTTP endpoints.

A backend may itself be a reverse proxy such as nginx.
