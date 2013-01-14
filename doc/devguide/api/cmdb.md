### CMDB APIs

CMDB APIs are used to manage configuration management databases.  

> WARNING: You cannot simply add a new CMDB type via the API!  CMDB object types must have a matching cmdb class override!  The primary function of this API is to manage the related CMDB subobjects.  You can have multiple CMDBs of the same type.

#### CMDB CRUD

List, Create, Read, Delete actions for CMDBs

> There is no update at this time!

##### List

Returns list of CMDB id:names in the system

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/cmdb </td>
  <td> - </td>
  <td> - </td></tr>
</table>


**Output:**

    {
      1:"chef",
      2:"puppet",
      4:"test"
    }

Details:

* id - CMDB id
* name - CMDB name

##### Read

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/cmdb/[id] </td>
  <td> id is the cmdb ID or name. </td>
  <td> -  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"chef",
      "description":null,
      "order":10000,
      "type":"CmdbChef",
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - json
* id - CMDB id
* name - CMDB name
* all Node properties serialized

##### CMDB CRUD: Create

Creates a new CMDB

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td>
  <td> /2.0/crowbar/2.0/cmdb/ </td>
  <td> json definition (see CMDB Show) </td>
  <td> must be a legal object </td></tr>
</table>

**Input:**

    { 
      "name":"chef",
      "description":"description",
      "order":10000,
      "type":"CmdbChef"
    }

Details:

* name (required) - CMDB name (must be letters - numbers and start with a letter)
* description - optional (default null)
* type (required) - name of the object that manages the CMDB calls
* order - optional (default 10000) 

> The type must match an existing class in the system

##### CMDB CRUD: Delete 

Deletes a CMDB

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td>
  <td> /2.0/crowbar/2.0/cmdb/[id] </td>
  <td> Database ID or name </td>
  <td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - CMDB name or database ID



