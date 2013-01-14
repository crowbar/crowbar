### Proposal APIs

#### Proposal Status

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET   </td><td> /2.0/crowbar/2.0/proposal/status  </td><td> none  </td><td> All proposals  </td><td> Used by Barclamp List  </td></tr> 
<tr><td> GET </td><td> 2.0/crowbar/2.0/proposal/status/2.0/[id]  </td><td> id is the proposal ID.  </td><td> Used by Proposal Views  </td></tr>
</table>

**Output:**


    {
      "i18n":{"unknown":"Unknown, requesting status...","ready":"Active"},
      "proposals":{"5":"ready","11":"ready" },
      "count":14,
      "error":""
    }

Details:

* Format - json
* i18n - the localized versions of the status strings for display.  Unknown is _always_ included
* proposals - the proposals (by database ID) requested in the API call with their current status
* count - the number of proposals returned, count < 0 indicates an error
* error - provides error information if the call returns an error
