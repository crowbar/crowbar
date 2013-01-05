### Status APIs

Status APIs are used to provide lists of objects in optimized formats.

The general pattern for the Status API calls is:

> `status/2.0/object/[:id]`

#### Node Status 

Returns JSON for node status.  Includes hash of all nodes to help detect changes.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /status/2.0/node </td><td> none </td><td> All nodes </td><td> Used by Dashboard </td><td> 
<tr><td> GET  </td><td> /status/2.0/node/[id] </td><td> id is the node ID or name. Used by Node details </td><td> - </td><tr>
<table>

**Output:**

    {
      "state":{"1":null},
      "sum":-1881043387,
      "i18n":{"ready":"Ready"},
      "groups":{
        "0":{"failed":0,"ready":0,"building":0,"pending":0,"unready":1,"name":"all","unknown":0}
      },
      "count":1,
      "status":{"1":"unready"}
    }

Details:

* Format - json
* i18n - the localized versions of the status strings for display.
* state - ?
* groups - ?
* status - ?
* count - ?
* sum - Hashed value of the nodes included to identify state changes for refresh
