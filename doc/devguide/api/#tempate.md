### Template API Doc

#### SAMPLE - REFERENCE ONLY

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET </td><td> /barclamp/status/2.0  </td><td> none  </td><td> All proposals  </td><td> Used by Barclamp List  </td></tr> 
<tr><td> GET   </td><td> /barclamp/status/2.0/[id]  </td><td> id is the proposal ID.  </td><td> Used by Proposal Views  </td></tr>
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
* i18n - ?


