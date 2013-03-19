# Jigs

This resource provides access to Crowbar *jigs*. It can be used to:

+ List jigs
+ Retrieve a specific jig

## List jigs

### Request

```
GET /api/v2/jigs
```

### Response

Headers:

```http
HTTP/1.1 200 OK
X-Request-Id: 1380a1dbde1767805afcd0ed9b7d85e2
X-Runtime: 0.011635
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
ETag: "59bd92a16eb7f1055ff940db8318db68"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
    "id": 1,
    "type": "BarclampCrowbar::Jig",
    "name": "test",
    "created_at": "2013-03-12T16:04:34Z",
    "description": null,
    "updated_at": "2013-03-12T16:04:34Z",
    "order": 9999,
    "_links": {
      "self": { "href": "/api/v2/jigs/1" }
    }
  },
  {
    "id": 2,
    "type": "BarclampChef::Jig",
    "name": "admin_chef",
    "created_at": "2013-03-12T16:04:41Z",
    "description": null,
    "updated_at": "2013-03-12T16:04:41Z",
    "order": 100,
    "_links": {
      "self": { "href": "/api/v2/jigs/2" }
    }
  },
  {
    "id": 3,
    "type": "BarclampChef::Jig",
    "name": "chef",
    "created_at": "2013-03-12T16:04:41Z",
    "description": null,
    "updated_at": "2013-03-12T16:04:41Z",
    "order": 200,
    "_links": {
      "self": { "href": "/api/v2/jigs/3" }
    }
  },
  {
    "id": 4,
    "type": "BarclampCrowbar::Jig",
    "name": "bddjig",
    "created_at": "2013-03-12T16:11:23Z",
    "description": "BDD Testing Only - should be automatically removed",
    "updated_at": "2013-03-12T16:11:23Z",
    "order": 100,
    "_links": {
      "self": { "href": "/api/v2/jigs/4" }
    }
  }
]
```

## Retrieve a specific jig

### Request

```
GET /api/v2/jigs/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
X-Request-Id: a0589084434f31b3bb6e343b4007638b
X-Runtime: 0.011998
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
ETag: "0acc1bf46308b8011437b5b3dd3e1084"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "id": 4,
  "type": "BarclampCrowbar::Jig",
  "name": "bddjig",
  "created_at": "2013-03-12T16:11:23Z",
  "description": "BDD Testing Only - should be automatically removed",
  "updated_at": "2013-03-12T16:11:23Z",
  "order": 100,
  "_links": {
    "self": { "href": "/api/v2/jigs/4" }
  }
}
```
