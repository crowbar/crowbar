# Role Types

This resource provides access to Crowbar *role_types*. It can be used to:

+ List role types
+ Retrieve a specific node

## List role types

### Request

```
GET /api/v2/role_types
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 2fc91b765c045887d729e68f9e71af48
X-Runtime: 0.013939
X-UA-Compatible: IE=Edge,chrome=1
ETag: "ef8436a949e33358b7f6a2bee8119c13"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
   "created_at": "2013-03-18T15:07:04Z",
   "updated_at": "2013-03-18T15:07:04Z",
   "description": "Private Role for Internal Data",
   "name": "private",
   "id": 1,
   "order": 1,
   "_links": {
     "self": { "href": "/api/v2/role_types/1" }
   }
  },
  {
   "created_at": "2013-03-18T15:07:04Z",
   "updated_at": "2013-03-18T15:07:04Z",
   "description": "Crowbar template Created Role",
   "name": "crowbar",
   "id": 2,
   "order": 9999,
   "_links": {
     "self": { "href": "/api/v2/role_types/2" }
   }
  },
  ...
  {
   "created_at": "2013-03-18T15:10:32Z",
   "updated_at": "2013-03-18T15:10:32Z",
   "description": "User Defined Attributes",
   "name": "user_defined",
   "id": 35,
   "order": 999990,
   "_links": {
     "self": { "href": "/api/v2/role_types/35" }
   }
  }
]
```

## Retrieve a specific role type

### Request

```
GET /api/v2/role_types/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: abe15e55f14b941b61c301a06e073dc2
X-Runtime: 0.011858
X-UA-Compatible: IE=Edge,chrome=1
ETag: "e618177c7f419eba322e8d0caa1f86bb"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "created_at": "2013-03-18T15:07:04Z",
  "updated_at": "2013-03-18T15:07:04Z",
  "description": "Ganglia template Created Role",
  "name": "ganglia-client",
  "id": 3,
  "order": 9999,
  "_links": {
    "self": { "href": "/api/v2/role_types/3" }
  }
 }
```

