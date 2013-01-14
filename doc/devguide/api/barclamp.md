### Barclamp APIs

Barclamps are the core modulization for Crowbar.  For that reason, the API for barclamps is more limited because changes to barclamps can cause breaking changes to the framework.

#### Barclamp List

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET </td><td> /2.0/crowbar/2.0/barclamp  </td><td> none  </td><td> All barclamps </td><td> - </td></tr> 
</table>

**Output:**


    {
      1:"crowbar",
      2:"deployer",
      3:"impi"
    }

#### Barclamp Get

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET </td><td> /2.0/crowbar/2.0/barclamp/[:id]  </td><td> none  </td><td> Barclamp </td><td> - </td></tr> 
</table>

**Output:**


    {
      "id":1,
      "name":"crowbar",
      "description":"this is about the barclamp"
    }


#### Not Supported

Barclamp API does not support create, update and delete operations.
