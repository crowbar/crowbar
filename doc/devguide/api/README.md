# Crowbar REST API

## Overview

The Crowbar API "contract" consists of URIs, resources, the structure and
content of resource representations, their formats, and the HTTP methods for
each resource.

Crowbar URIs have one of two forms:

1. `http(s)://<HOSTNAME>/api/<VERSION>`. These URIs provide access to Crowbar
   framework resources, such as attribs, attrib_types, barclamps, deployments,
   groups, jigs, nodes, roles, role_types, and snapshots.
2. `http(s)://<HOSTNAME>/<BARCLAMP>/<VERSION>`. These provide access to
   barclamp specific resources.

The content returned to the client has a media type of
`application/vnd.crowbar+json; version=X.Y` unless otherwise noted. (*a little
googling turned up: http://tools.ietf.org/html/draft-kelly-json-hal-05 "a
generic media type with with Web APIs can be developed and exposed as a series
of links", perhaps we should use it rather than defining our own. Regardless,
we should steal their link syntax.*)

## Client Errors

There are three possible types of client errors on API calls that receive
request bodies:

1. Sending invalid JSON will result in a `400 Bad Request` response.
2. Sending the wrong type of JSON values will result in a `400 Bad Request`
   response.
3. Sending invalid fields will result in a `422 Unprocessable Entity` response.

## HTTP Verbs

Where possible the Crowbar API strives to use the appropriate HTTP verbs for
each action, eg:

| Verb   | Description |
|--------|-------------|
| HEAD   | Used against any resource to get just the HTTP header information. |
| GET    | Used for retrieving resources. |
| POST   | Used for creating resources, or performing custom actions. |
| PATCH  | Used for updating resources with partial JSON data. |
| PUT    | Used for replacing resources or collection. Requests should always set `Content-Length` header to zero. |
| DELETE | Used for deleting resources. |

## Hypermedia Controls

Crowbar API clients should be able to navigate with a bare minimum of a priori
knowledge. `Links` are included in the content returned to clients that allow
the client to progress through the application. The `hal+json` media type
specifies links in the following format:

```json
{
  "_links": {
    "self": {
      "href": "/api/v2/nodes/42"
    },
    ...
  },
  "id":     "xyzzy",
  "status": "outstanding!"
  ...
}
```

The reserved property `_links` contains links to other resources of the form:
`reltype` : `href`. Where `reltype` is the relation type and `href` is a
relative or absolute URI. The `self` link should always be provided, further
links are included depending on the type of the content.

## Pagination

Requests that return multiple items are paginated by 128 items by default. You
can specify further pages with the `?page` parameter.

The pagination info is included in the `Link` header. It is important to follow
these `Link` header values instead of constructing your own URIs. A `Link`
header has the form:

```
Link: <http://your.crowbar.host/api/v2/attribs?page=3&per_page=128>; rel="next", <http://your.crowbar.host/api/v2/attribs?page=12&per_page=128>; rel="last"
```

The possible values for the `rel` attribute are:

| Value | Description |
|-------|-------------|
| next  | URI of the immediate next page of results. |
| last  | URI of the last page of results. |
| first | URI of the first page of results. |
| prev  | URI of the immediate previous page of results. |
