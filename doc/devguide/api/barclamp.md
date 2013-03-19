# Barclamp API

This resource provides access to all the Crowbar *barclamps*

+ List barclamps
+ Retrieve a specific barclamp

## List barclamps

### Request

```
GET /api/v2/barclamps
```

### Response

Headers:

```http
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 5f7a7e9a28bcc3dfab26b522a17016d9
X-Runtime: 0.021580
ETag: "7f7e86fb0b2538e200852a3af7f55c81"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Body:

```json
[
  {
    "description": "Self-referential barclamp enabling other barclamps",
    "order": 0,
    "api_version": "v2",
    "transition_list": "all",
    "proposal_schema_version": 2,
    "commit": "unknown",
    "online_help": "https://crowbar.github.com/",
    "run_order": 0,
    "allow_multiple_deployments": false,
    "transitions": false,
    "display": "Crowbar",
    "user_managed": true,
    "layout": 2,
    "build_on": null,
    "mode": "full",
    "name": "crowbar",
    "source_path": "../barclamps/crowbar",
    "license": "apache2",
    "version": 0,
    "api_version_accepts": "v2",
    "template_id": 1,
    "jig_order": 0,
    "copyright": "Dell, Inc 2013",
    "created_at": "2013-03-11T14:59:23Z",
    "updated_at": "2013-03-11T14:59:23Z",
    "id": 1,
    "_links": {
      "self": { "href": "/api/v2/barclamps/1"}
    }
  },
  {
    "description": "Ganglia",
    "order": 100,
    "api_version": "v2",
    "transition_list": "all",
    "proposal_schema_version": 2,
    "run_order": 70,
    "allow_multiple_deployments": false,
    "transitions": false,
    "display": "Ganglia",
    "user_managed": true,
    "layout": 1,
    "build_on": null,
    "mode": "full",
    "name": "ganglia",
    "source_path": "../barclamps/ganglia",
    "license": "apache2",
    "version": 0,
    "api_version_accepts": "v2",
    "template_id": 2,
    "jig_order": 1040,
    "copyright": "Dell, Inc 2013",
    "created_at": "2013-03-11T14:59:24Z",
    "updated_at": "2013-03-11T14:59:24Z",
    "id": 2
    "_links": {
      "self": { "href": "/api/v2/barclamps/2"}
    }
  },
  ...
  {
    "description": "Dns",
    "order": 30,
    "api_version": "v2",
    "transition_list": "all",
    "proposal_schema_version": 2,
    "commit": "unknown",
    "online_help": null,
    "run_order": 30,
    "allow_multiple_deployments": false,
    "transitions": false,
    "display": "DNS",
    "user_managed": true,
    "layout": 1,
    "build_on": null,
    "mode": "full",
    "name": "dns",
    "source_path": "../barclamps/dns",
    "license": "apache2",
    "version": 0,
    "api_version_accepts": "v2",
    "template_id": 13,
    "jig_order": 30,
    "copyright": "Dell, Inc 2013",
    "created_at": "2013-03-11T14:59:32Z",
    "updated_at": "2013-03-11T14:59:33Z",
    "id": 12
    "_links": {
      "self": { "href": "/api/v2/barclamps/12"}
    }
  }
]
```

## Retrieve a specific barclamp

### Request

```
GET /api/v2/barclamps/:id
```

#### Response

Headers:

```http 
HTTP/1.1 200 OK
Content-Type: application/vnd.crowbar+json; version=1.9; charset=utf-8
X-Request-Id: 5f7a7e9a28bcc3dfab26b522a17016d9
X-Runtime: 0.021580
ETag: "7f7e86fb0b2538e200852a3af7f55c81"
Cache-Control: max-age=0, private, must-revalidate
Transfer-Encoding: chunked
```

Response:

```json
{
  "description": "Dns",
  "order": 30,
  "api_version": "v2",
  "transition_list": "all",
  "proposal_schema_version": 2,
  "commit": "unknown",
  "online_help": null,
  "run_order": 30,
  "allow_multiple_deployments": false,
  "transitions": false,
  "display": "DNS",
  "user_managed": true,
  "layout": 1,
  "build_on": null,
  "mode": "full",
  "name": "dns",
  "source_path": "../barclamps/dns",
  "license": "apache2",
  "version": 0,
  "api_version_accepts": "v2",
  "template_id": 13,
  "jig_order": 30,
  "copyright": "Dell, Inc 2013",
  "created_at": "2013-03-11T14:59:32Z",
  "updated_at": "2013-03-11T14:59:33Z",
  "id": 12,
  "_links": {
    "self": { "href": "/api/v2/barclamps/12"}
  }
}
```
