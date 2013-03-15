# Attrib Types

This resource provides access to all the Crowbar *attrib_types*, eg:

1. List attrib_types
1. Retrieve a specific attrib_type

## List attrib_types

### Request

```
GET /api/v2/attrib_types
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 8786e1c250445b852872288fcdda1afb
X-Runtime: 0.021875
ETag: "ab3d4dfa3accd5d6918c9783327e9541"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
    "name": "web_port",
    "updated_at": "2013-03-11T14:59:23Z",
    "description": "crowbar Created Attribute",
    "created_at": "2013-03-11T14:59:23Z",
    "order": 10000,
    "id": 1,
    "_links": {
      "self": { "href": "/api/v2/attrib_types/1" },
      "attribs": [
        { "href": "/api/v2/attribs/42" },
        { "href": "/api/v2/attribs/36" }
      ]
    }
  },
  {
    "name": "instances",
    "updated_at": "2013-03-11T14:59:23Z",
    "description": "crowbar Created Attribute",
    "created_at": "2013-03-11T14:59:23Z",
    "order": 10000,
    "id": 2,
    "_links": {
      "self": { "href": "/api/v2/attrib_types/2" },
      "attribs": [
        { "href": "/api/v2/attribs/22" }
      ]
    }
  },
  ...
  {
    "name": "status",
    "updated_at": "2013-03-11T15:04:44Z",
    "description": "System Assigned Attrib for Role-Node membership",
    "created_at": "2013-03-11T15:04:44Z",
    "order": 999999,
    "id": 40,
    "_links": {
      "self": { "href": "/api/v2/attrib_types/40" },
      "attribs": [
        { "href": "/api/v2/attribs/99" },
        { "href": "/api/v2/attribs/33" },
        { "href": "/api/v2/attribs/11" }
      ]
    }
  }
]
```

## Retrieve a specific attrib_type

### Request

```
GET /api/v2/attrib_types/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 8786e1c250445b852872288fcdda1afb
X-Runtime: 0.021875
ETag: "ab3d4dfa3accd5d6918c9783327e9541"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "name": "web_port",
  "updated_at": "2013-03-11T14:59:23Z",
  "description": "crowbar Created Attribute",
  "created_at": "2013-03-11T14:59:23Z",
  "order": 10000,
  "id": 1,
  "_links": {
    "self": { "href": "/api/v2/attrib_types/1" },
    "attribs": [
      { "href": "/api/v2/attribs/42" },
      { "href": "/api/v2/attribs/36" }
    ]
  }
}
```
