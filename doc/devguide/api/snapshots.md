# Snapshots

This resource provides access to Crowbar *snapshots*. It can be used to:

+ List snapshots
+ Retrieve a specific snapshot

## List snapshots

### Request

```
GET /api/v2/snaphsots
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: f284a8393fa8c98110fbc1cc2de9d3b0
X-Runtime: 0.023323
X-UA-Compatible: IE=Edge,chrome=1
ETag: "99bc4a55f90359bde684be4e1aac8032"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
   "created_at": "2013-03-18T15:07:04Z",
   "deployment_id": null,
   "name": "Crowbar template",
   "barclamp_id": 1,
   "status": 1,
   "order": 10000,
   "updated_at": "2013-03-18T15:07:04Z",
   "description": "Imported from /tmp/crowbar-dev-test/opt/dell/barclamps/crowbar/crowbar.yml",
   "failed_reason": null,
   "element_order": "[[\"crowbar\"]]",
   "id": 1,
   "_links": {
     "self": { "href": "/api/v2/snapshots/1" }
   }
  },
  {
   "created_at": "2013-03-18T15:07:04Z",
   "deployment_id": null,
   "name": "Ganglia template",
   "barclamp_id": 2,
   "status": 1,
   "order": 10000,
   "updated_at": "2013-03-18T15:07:04Z",
   "description": "Imported from /tmp/crowbar-dev-test/opt/dell/barclamps/ganglia/crowbar.yml",
   "failed_reason": null,
   "element_order": "[[\"ganglia-server\",\"ganglia-client\"]]",
   "id": 2,
   "_links": {
     "self": { "href": "/api/v2/snapshots/2" }
   }
  },
  ...
  {
   "created_at": "2013-03-18T15:10:38Z",
   "deployment_id": 7,
   "name": "solid",
   "barclamp_id": 11,
   "status": 1,
   "order": 10000,
   "updated_at": "2013-03-18T15:10:38Z",
   "description": "Imported from /tmp/crowbar-dev-test/opt/dell/barclamps/logging/crowbar.yml",
   "failed_reason": null,
   "element_order": "[[\"logging-server\",\"logging-client\"]]",
   "id": 19,
   "_links": {
     "self": { "href": "/api/v2/snapshots/19" }
   }
  }
]
```

## Retrieve a specific snapshot

### Request

```
GET /api/v2/snapshots/:id
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: fab7f475170330b7552662c54c8d0115
X-Runtime: 0.011233
X-UA-Compatible: IE=Edge,chrome=1
ETag: "de3db0b378e972b97c43cf7b99943e48"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
{
  "created_at": "2013-03-18T15:07:04Z",
  "deployment_id": null,
  "name": "Crowbar template",
  "barclamp_id": 1,
  "status": 1,
  "order": 10000,
  "updated_at": "2013-03-18T15:07:04Z",
  "description": "Imported from /tmp/crowbar-dev-test/opt/dell/barclamps/crowbar/crowbar.yml",
  "failed_reason": null,
  "element_order": "[[\"crowbar\"]]",
  "id": 1,
  "_links": {
     "self": { "href": "/api/v2/snapshots/1" }
  }
}
```
