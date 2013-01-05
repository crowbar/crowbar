### Group APIs

Group APIs are used to manage groups.  Groups are used to organized things

#### Group CRUD

Create, Read, Update, Delete actions for Groups

##### List

Returns list of group id:names in the system

> Note: Category is not included in this list

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/group </td>
  <td> - </td>
  <td> - </td></tr>
</table>


**Output:**

    {
      1:"not_set",
      2:"rock_n_role",
      4:"group_of_wrath"
    }

Details:

* id - Node id
* name - Node name

##### Read

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/group/[id] </td>
  <td> id is the group ID or name. </td>
  <td> -  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"greg.example.com",
      "description":null,
      "order":10000,
      "category":"ui",
      ...
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - json
* id - Node id
* name - Node name
* category - one of the allowed categories in lowercase: ui, rack 
* all Node properties serialized

##### Group CRUD: Create

Creates a new group

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td>
  <td> /2.0/crowbar/2.0/node/ </td>
  <td> json definition (see Node Show) </td>
  <td> must be a legal object </td></tr>
</table>

**Input:**

    { 
      "id":1
      "name":"fqdn.example.com",
      "description":"description",
      "category":"ui"
      "order":10000,
    }

Details:

* name - group name (must be letters - numbers and start with a letter)
* description - optional (default null)
* category - (default = ui) determines the collection of groups.  Allowed categories are
  * ui
  * rack
* order - optional (default 10000) 

##### Group CRUD: Delete 

Deletes a group

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td>
  <td> /2.0/crowbar/2.0/group/[id] </td>
  <td> Database ID or name </td>
  <td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Group name or database ID

#### Node Actions on Groups 

These actions are for showing adding, removing, or moving nodes in groups

On success, They all return the same result as the Show method

> _Note_: This these methods are used by the UI for drag and drop group management

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/group/[group-id]/node </td>
  <td> none </td>
  <td> json list of nodes (see below) </td>
  <td> Shows nodes that below to group </td></tr>
<tr><td> POST </td>
  <td> /2.0/crowbar/2.0/group/[group-id]/node/[node-id] </td>
  <td> none </td>
  <td> json definition (see Node Show) </td>
  <td> Add node to group </td></tr>
<tr><td> PUT  </td>
  <td> /2.0/crowbar/2.0/group/[group-id]/node/[node-id] </td>
  <td> none </td>
  <td> json definition (see Group Node Show) </td>
  <td> Move Node from Group 1 to Group 2 </td></tr>
<tr><td> DELETE </td>
  <td> /2.0/crowbar/2.0/group/[group-id]/node/[node-id] </td>
  <td> none </td>
  <td> json definition (see Node Show) </td>
  <td> Removes a node from an existing group </td></tr>
</table>

> _Note_: Move a node from an existing group to an another group _in the same category_.  This is effectively a combined delete/add action.


Details:

* All data is contained in the URL (no body required)
* group-id: id of the group (can be name)
* node-id: id if the node (can be name) 

**Output:**

    {
      "id": #
      "nodes": {"[group_id#]":"[group_name]"},
      "name":"[group_name]",
      "category":"[group_category]"}
    }

**Errors:**

* 404 if node requested is not found
* 404 if group requested is not found


