# Roles

This resource provides access to Crowbar *roles*. It can be used to:

+ List roles
+ Retrieve a specific role

## List nodes

### Request

```
GET /api/v2/roles
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: c94d84f01c79ab2e90590d9d51b34369
X-Runtime: 0.171903
X-UA-Compatible: IE=Edge,chrome=1
ETag: "5cbe8547a13a83b530027f49423b3e38"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
   "id": 1,
   "states": "all",
   "created_at": "2013-03-18T15:07:04Z",
   "updated_at": "2013-03-18T15:07:04Z",
   "description": null,
   "snapshot_id": 1,
   "name": "private",
   "order": 1,
   "role_type_id": 1,
   "run_order": -1,
   "_links": {
     "self": { "href": "/api/v2/roles/1" }
   }
  },
  {
   "id": 2,
   "states": "all",
   "created_at": "2013-03-18T15:07:04Z",
   "updated_at": "2013-03-18T15:07:04Z",
   "description": "Imported from /tmp/crowbar-dev-test/opt/dell/barclamps/crowbar/bc-template-crowbar.json",
   "snapshot_id": 1,
   "name": "crowbar",
   "order": 100,
   "role_type_id": 2,
   "run_order": 0,
   "_links": {
     "self": { "href": "/api/v2/roles/2" }
   }
  },
  {
   "id": 3,
   "states": "all",
   "created_at": "2013-03-18T15:07:04Z",
   "updated_at": "2013-03-18T15:07:04Z",
   "description": null,
   "snapshot_id": 2,
   "name": "private",
   "order": 1,
   "role_type_id": 1,
   "run_order": -1,
   "_links": {
     "self": { "href": "/api/v2/roles/3" }
   }
  },
  ...
  {
   "id": 75,
   "states": "all",
   "created_at": "2013-03-18T15:10:38Z",
   "updated_at": "2013-03-18T15:10:38Z",
   "description": null,
   "snapshot_id": 19,
   "name": "logging",
   "order": 9999,
   "role_type_id": 28,
   "run_order": 9999,
   "_links": {
     "self": { "href": "/api/v2/roles/75" }
   }
  }
]
```

## Retrieve a specific role

### Request

```
GET /api/v2/roles/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: d119a0a3be6bc3b7beabb77a1779b45a
X-Runtime: 0.006579
X-UA-Compatible: IE=Edge,chrome=1
ETag: "506a9998adf549acde2c2abfaa07acac"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "id": 75,
  "states": "all",
  "created_at": "2013-03-18T15:10:38Z",
  "updated_at": "2013-03-18T15:10:38Z",
  "description": null,
  "snapshot_id": 19,
  "name": "logging",
  "order": 9999,
  "role_type_id": 28,
  "run_order": 9999,
  "_links": {
    "self": { "href": "/api/v2/roles/75" }
  }
 }
```

