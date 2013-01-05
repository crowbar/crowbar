### Node APIs

Node APIs are used to manage nodes (servers) within the Crowbar system

#### Node Show (all)

**Input:**



<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /2.0/crowbar/2.0/node</td><td> no options </td><td> Specialized return </td></tr>
</table>


**Output:**

    { 
      4:"greg.example.com",
      5:"rob.example.com"
    }

Details:

* json format is id: Node name

#### Node Show (single)

**Input:**



<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /2.0/crowbar/2.0/node/[id]</td><td> id is the node ID or name. </td><td>   </td></tr>
</table>


**Output:**

    {
      "id":4,
      "fingerprint":-1224971211,
      "state":null,
      "name":"greg.example.com",
      "description":null,
      "order":10000,
      ...
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* id - Node id
* name - Node name
* all Node properties serialized

#### Node Create (API only)

Creates a new node

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td> /2.0/crowbar/2.0/node </td><td> json definition (see Node Show) </td><td> must be a legal object </td></tr>
</table>

**Input:**

    {
      "name":"fqdn.example.com",
      "description":"description",
      "order":10000,
    }

Details:

* name - Node name (must be FQDN)
* description - optional (default null)
* order - optional (default 10000) 

#### Node Delete (API only)

Deletes a node

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td><td> /2.0/crowbar/2.0/node/[id] </td><td> Database ID or name </td><td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Node name or database ID (must be FQDN)

### Node Attributes

Node Attributes API is used to retrieve data about attributes that have been associated with a Node.

Typically, attribute data is populated by the CMDB system(s) based on the associations established using this API.

#### Associations

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET </td>
  <td> /2.0/crowbar/2.0/node/[id]/attribute </td>
  <td> none </td>
  <td> List of attribute IDs and names assigned to node</td>
  <td></td></tr>
<tr><td> GET </td>
  <td> /2.0/crowbar/2.0/node/[id]/attribute/[id] </td>
  <td> none </td>
  <td> Last 100 readings (Event ID + Value) </td>
  <td></td></tr>
<tr><td> POST </td>
  <td> /2.0/crowbar/2.0/node/[id]/attribute/[id] </td>
  <td> none </td>
  <td> Link Attribute to Node </td>
  <td></td></tr>
<tr><td> PUT </td>
  <td> /2.0/crowbar/2.0/node/[id]/attribute/[id] </td>
  <td> none </td>
  <td> 405 error - Not enabled </td>
  <td></td></tr>
<tr><td> DELETE </td>
  <td> /2.0/crowbar/2.0/node/[id]/attribute/[id] </td>
  <td> none </td>
  <td> Break association and remove data </td>
  <td></td></tr>
</table>



