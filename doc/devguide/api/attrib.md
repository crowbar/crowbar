# Attribs

This resource provides access to all the Crowbar *attribs*. It can be used to:

+ List attribs
+ Retrieve a specific attrib

## List attribs

### Request

```
GET /api/v2/attribs
```

### Parameters

+ filter -- specifies one or more properties that selected attribs must match
+ created -- selects attribs created after specified datetime
+ updated -- selects attribs updated after specified datetime

### Response

Headers:

```http
HTTP/1.1 200 OK
Cache-Control: max-age=0, private, must-revalidate
X-Runtime: 0.139598
X-Request-Id: 4957a1203d60a953350f0401d90f32c0
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
ETag: "d494b9ec2f2b084386a940e95e71b22a"
Transfer-Encoding: chunked
Link: <http://localhost/api/v2/attribs?page=2&per_page=128>; rel="next"
```

Body:

```json
[
  {
    "name": "status",
    "state": 999,
    "node_id": 1,
    "attrib_type_id": 40,
    "description": "This is an Attrib",
    "value": "empty",
    "updated_at": "2013-03-11T15:43:38Z",
    "order": 999999,
    "id": 47
    "created_at": "2013-03-11T15:43:38Z",
    "_links": {
      "self": { "href": "/api/v2/attribs/47" },
      "attrib_type": { "href": "/api/v2/attrib_types/42" }
    }
  },
  {
    "name": "status",
    "state": 999,
    "node_id": 1,
    "attrib_type_id": 40,
    "description": "This is another Attrib",
    "value": "empty",
    "updated_at": "2013-03-11T15:04:44Z",
    "order": 999999,
    "id": 46,
    "created_at": "2013-03-11T15:04:44Z"
    "_links": {
      "self": { "href": "/api/v2/attribs/46" },
      "attrib_type": { "href": "/api/v2/attrib_types/28" }
    }
  },
  ...
  {
    "name": "forwarders",
    "state": "empty",
    "node_id": null,
    "attrib_type_id": 36,
    " escription": "dns Created Attribute",
    "value": null,
    "updated_at": "2013-03-11T14:59:33Z",
    "order": 10000,
    "id": 42,
    "created_at": "2013-03-11T14:59:33Z",
    "_links": {
      "self": { "href": "/api/v2/atttribs/42" },
      "attrib_type": { "href": "/api/v2/attrib_types/36" }
    }
  }
]
```

## Retrieve a specific attrib

### Request

```
GET /api/v2/attribs/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: ce79f8c672719a693891f537bd747d8a
X-Runtime: 0.012809
ETag: "5a1469c57221b78c1df21f0a6b67e431"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "name": "forwarders",
  "attrib_type_id": 36,
  "node_id": null,
  "updated_at": "2013-03-11T14:59:33Z",
  "value": null,
  "description": "dns Created Attribute",
  "state": "empty",
  "order": 10000,
  "created_at": "2013-03-11T14:59:33Z",
  "id": 42,
  "_links": {
    "self": { "href": "/api/v2/attribs/42" },
    "attrib_type": { "href": "/api/v2/attrib_types/36" }
  }
}
```
