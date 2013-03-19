# Nodes

This resoure provides access to Crowbar *nodes*. It can be used to:

+ List nodes
+ Retrieve a specific node

## List nodes

### Request

```
GET /api/v2/nodes
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 221938bcbba8cda724aa727bc1a43c55
X-Runtime: 0.019898
X-UA-Compatible: IE=Edge,chrome=1
ETag: "fc52a8eafec72c878f29bcfdb14574a9"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
   "created_at": "2013-03-18T15:10:05Z",
   "name": "global-node.testing.com",
   "alias": "global-node",
   "updated_at": "2013-03-18T15:10:05Z",
   "description": "GlobalBDD Testing Only - should be automatically removed",
   "order": 100,
   "state": "empty",
   "id": 1,
   "allocated": false,
   "os_id": null,
   "admin": false,
   "_links": {
     "self": { "href": "/api/v2/nodes/1" }
   }
  },
  {
   "created_at": "2013-03-18T15:10:33Z",
   "name": "dashboard1.example.com",
   "alias": "dashboard1",
   "updated_at": "2013-03-18T15:10:33Z",
   "description": "BDD Testing Only - should be automatically removed",
   "order": 100,
   "state": "empty",
   "id": 2,
   "allocated": false,
   "os_id": null,
   "admin": false,
   "_links": {
     "self": { "href": "/api/v2/nodes/2" }
   }
  },
  {
   "created_at": "2013-03-18T15:11:22Z",
   "name": "group1.node.test",
   "alias": "group1",
   "updated_at": "2013-03-18T15:11:22Z",
   "description": "BDD Testing Only - should be automatically removed",
   "order": 100,
   "state": "empty",
   "id": 4,
   "allocated": false,
   "os_id": null,
   "admin": false,
   "_links": {
     "self": { "href": "/api/v2/nodes/4" }
   }
  },
  {
   "created_at": "2013-03-18T15:11:53Z",
   "name": "bdd1.example.com",
   "alias": "bdd1",
   "updated_at": "2013-03-18T15:11:53Z",
   "description": "BDD Testing Only - should be automatically removed",
   "order": 100,
   "state": "empty",
   "id": 6,
   "allocated": false,
   "os_id": null,
   "admin": false,
   "_links": {
     "self": { "href": "/api/v2/nodes/6" }
   }
  }
]
```

## Retrieve a specific node

### Request

```
GET /api/v2/nodes/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 9b3f0ea191adbdd0a31357bd64df76ad
X-Runtime: 0.015017
X-UA-Compatible: IE=Edge,chrome=1
ETag: "4b2a8f03178a28da428123cfc46da865"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "created_at": "2013-03-18T15:11:53Z",
  "name": "bdd1.example.com",
  "alias": "bdd1",
  "updated_at": "2013-03-18T15:11:53Z",
  "description": "BDD Testing Only - should be automatically removed",
  "order": 100,
  "state": "empty",
  "id": 6,
  "allocated": false,
  "os_id": null,
  "admin": false,
   "_links": {
     "self": { "href": "/api/v2/nodes/6" }
   }
}
```
