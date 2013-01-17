### Jig APIs

Jig APIs are used to manage configuration management databases.  

> WARNING: You cannot simply add a new Jig type via the API!  Jig object types must have a matching jig class override!  The primary function of this API is to manage the related Jig subobjects.  You can have multiple Jigs of the same type.

#### Jig CRUD

List, Create, Read, Delete actions for Jigs

> There is no update at this time!

##### List

Returns list of Jig id:names in the system

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/jig </td>
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

* id - Jig id
* name - Jig name

##### Read

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/jig/[id] </td>
  <td> id is the jig ID or name. </td>
  <td> -  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"chef",
      "description":null,
      "order":10000,
      "type":"JigChef",
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - json
* id - Jig id
* name - Jig name
* all Node properties serialized

##### Jig CRUD: Create

Creates a new Jig

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td>
  <td> /2.0/crowbar/2.0/jig/ </td>
  <td> json definition (see Jig Show) </td>
  <td> must be a legal object </td></tr>
</table>

**Input:**

    { 
      "name":"chef",
      "description":"description",
      "order":10000,
      "type":"JigChef"
    }

Details:

* name (required) - Jig name (must be letters - numbers and start with a letter)
* description - optional (default null)
* type (required) - name of the object that manages the Jig calls
* order - optional (default 10000) 

> The type must match an existing class in the system

##### Jig CRUD: Delete 

Deletes a Jig

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td>
  <td> /2.0/crowbar/2.0/jig/[id] </td>
  <td> Database ID or name </td>
  <td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Jig name or database ID



