### Attribute (aka Attrib) APIs

Attribute APIs are used to manage attributes tracked by the CMDB system

> To prevent Rails name collisions, Crowbar uses 'Attrib' instead of Attribute.

#### Attrib CRUD

List, Create, Read, Delete actions for Attribute

> There is no update at this time!

##### List

Returns list of Attrib id:names in the system

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/attrib </td>
  <td> - </td>
  <td> - </td></tr>
</table>


**Output:**

    {
      1:"ram",
      2:"cpu",
      4:"nics"
    }

Details:

* id - Attrib id
* name - Attrib name

##### Read

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/attrib/[id] </td>
  <td> id is the Attrib ID or name. </td>
  <td> -  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"ram",
      "description":null,
      "order":10000,
      "barclamp_id":40,
      "hint":null,
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - json
* id - Attrib id
* name - Attrib name
* barclamp_id - relation with barclamp (attribute only has 1)
* hint - helps the barclamp figure out how to populate the attribute.  should be assigned by the attribute

##### Attrib CRUD: Create

Creates a new Attrib

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td>
  <td> /2.0/crowbar/2.0/attrib/ </td>
  <td> json definition (see Attrib Show) </td>
  <td> must be a legal object </td></tr>
</table>

**Input:**

    { 
      "name":"chef",
      "description":"description",
      "order":10000,
    }

Details:

* name (required) - Attrib name (must be letters - numbers and start with a letter)
* description - optional (default null)
* order - optional (default 10000) 

##### Attrib CRUD: Delete 

Deletes an Attrib

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td>
  <td> /2.0/crowbar/2.0/attrib/[id] </td>
  <td> Database ID or name </td>
  <td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Attrib name or database ID



