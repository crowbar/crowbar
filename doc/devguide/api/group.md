# Groups

This resource provides access to all the Crowbar *groups*. It can be used to:

+ List groups
+ Retrieve a specific group

## List groups

### Request

```
GET /api/v2/groups
```

### Response

Headers:

```http`
HTTP/1.1 200 OK
X-Request-Id: 6f0404a4f974ebf889086b2e77acb396
X-Runtime: 0.006447
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
ETag: "0582b058b9b234b6f995b5e18417a8b0"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
    "name": "not_set",
    "created_at": "2013-03-12T16:09:50Z",
    "description": "Not set",
    "updated_at": "2013-03-12T16:09:50Z",
    "category": "ui",
    "id": 1,
    "order": 10000,
    "_links": {
      "self": { "href": "/api/v2/groups/1" }
    }
  },
  {
    "name": "bddthings",
    "created_at": "2013-03-12T16:11:15Z",
    "description": "BDD Testing Only - should be automatically removed",
    "updated_at": "2013-03-12T16:11:15Z",
    "category": "ui",
    "id": 2,
    "order": 100,
    "_links": {
      "self": { "href": "/api/v2/groups/2" }
    }
  },
  {
    "name": "bdddelete",
    "created_at": "2013-03-12T16:11:16Z",
    "description": "BDD Testing Only - should be automatically removed",
    "updated_at": "2013-03-12T16:11:16Z",
    "category": "ui",
    "id": 3,
    "order": 200,
    "_links": {
      "self": { "href": "/api/v2/groups/3" }
    }
  }
]
```

## Retrieve a specific group

### Request

```
GET /api/v2/groups/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
X-Request-Id: 46401724f8afc9c12e23a74530e2fae3
X-Runtime: 0.010442
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
ETag: "b2a9513490c67f2058271daf7bc74922"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "name": "bdddelete",
  "created_at": "2013-03-12T16:11:16Z",
  "description": "BDD Testing Only - should be automatically removed",
  "updated_at": "2013-03-12T16:11:16Z",
  "category": "ui",
  "id": 3,
  "order": 200,
  "_links": {
    "self": { "href": "/api/v2/groups/3" }
  }
}
```

