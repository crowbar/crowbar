## Contributing to Crowbar

Before [submitting pull requests]
(https://help.github.com/articles/using-pull-requests), please ensure you are
covered by a [CLA](CLA.md).

#### Guidelines for Pull Requests

   * Must be Apache 2 license
   * For bugs & minor items (20ish lines), we can accept the request at our
     discretion
   * UI strings are localized (only EN file needs to be updated)
   * Does not inject vendor information (Name or Product) into Crowbar expect
     where relevant to explain utility of push (e.g.: help documentation &
     descriptions).
   * Passes code review by Dell Crowbar team reviewer
   * Does not degrade the security model of the product
   * Items requiring more scrutiny, including signing CLA
      * Major changes
      * New barclamps
      * New technology

#### Timing

   * Accept no non-bug fix push requests within 2 weeks of a release fork
   * No SLA - code accepted at Dell's discretion. No commitment to accept
     changes.

#### Coding Expectations [PRELIMINARY / PROPOSED 8/31]

   * Copyright & License header will be included in files that can tolerate
     headers
   * At least 1 line comments as header for all methods
   * Unit tests for all models concurrent with pull request
   * BDD tests for all API calls and web pages concurrent with pull request
   * Documentation for API calls concurrent with pull request
   * Adhere to the community [Ruby style guide]
     (https://github.com/bbatsov/ruby-style-guide)
   * Adhere to the community [Rails style guide]
     (https://github.com/bbatsov/rails-style-guide/)

#### Testing/ Validation

   * For core functions, push will be validated to NOT break build or deploy or
     our commercial products
   * For product suites (OpenStack, Hadoop, etc), push will be validated to NOT
     break build or deploy our commercial products
   * For operating systems that are non-core, we will _not_ validate on the
     target OS for the push (e.g.: not testing SUSE install at this point)
   * For non-DellCloudEdge barclamps &ndash; no rules!
   * Eventually, we would expect that a pull request would be built and tested
     in our CI system before the push can be accepted

#### Feature Progression

The following table shows the progression of new feature additions to Crowbar.
The purpose of this list is to help articulate how new features appear in
Crowbar and when they are considered core.

<table border=1>
  <thead>
    <tr>
      <th align="center"><em>Phase   </em></th>
      <th align="center"><em>Comments</em></th>
      <th align="center"><em>Roadmap </em></th>
      <th align="center"><em>Support </em></th>
      <th align="center"><em>On Trunk</em></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td align="center">Proposed</td>
      <td align="center">Conceptual ideas and suggestions for Crowbar
        functionality that have not been implemented as code</td>
      <td align="center">May be shown</td>
      <td align="center">None</td>
      <td align="center">N/A</td>
    </tr>
    <tr>
      <td align="center">Proof of Concept</td>
      <td align="center">Initial code showing partial functionality for
        feature</td>
      <td align="center">Optionally Identified</td>
      <td align="center">Negative Test (Does not impact core)</td>
      <td align="center">No (on branch)</td>
    </tr>
    <tr>
      <td align="center">Incubated</td>
      <td align="center">Base functional code allowing use of feature to
        demonstrate value</td>
      <td align="center">Identified</td>
      <td align="center">Negative Test (Does not impact core)</td>
      <td align="center">No (on branch)</td>
    </tr>
    <tr>
      <td align="center">Stable</td>
      <td align="center">Base functional code is available for use in select
        builds</td>
      <td align="center">Included</td>
      <td align="center">Validated by QA, No Support</td>
      <td align="center">Yes</td>
    </tr>
    <tr>
      <td align="center">Core</td>
      <td align="center">Feature code integrated into operations of Crowbar in
        fundamental way</td>
      <td align="center">Central</td>
      <td align="center">Validated by QA, Current Version Support</td>
      <td align="center">Yes</td>
    </tr>
    <tr>
      <td align="center">Supported</td>
      <td align="center">Same as core, but available with commercial
        support</td>
      <td align="center">Central</td>
      <td align="center">Backwards Support via Patches</td>
      <td align="center">Yes &amp; Maintained on Branches</td>
    </tr>
  </tbody>
</table>

Note: Features are NOT required to progress through all these phases!
Architectural changes may skip ahead based on their level of impact and
disruption.
