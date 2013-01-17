### Barclamp Meta Data

Every barclamps has meta data that tells Crowbar how to display and manage the barclamp within the Crowbar framework.

In Crowbar 1.x, this meta data was tracked in the Crowbar.yml file.  This file still exists, but has a much more limited function.

In Crowbar 2.x, barclamp meta data is injected into Crowbar in the barclamps `YYYYMMDDHHMMSSnn_barclamp_import_[barclamp].rb` migration file stored in the barclamp's `crowbar_framework/db/migrate` directory.

This migration is used to populate (or update) the barclamp information used by Crowbar.  

> _Note:_ For legacy barclamps, you can use `Barclamp.import_1x 'barclamp'` to simply import the information from the 1.x barclamp.yml into the new schema.  This is not recommended for new barclamps.

* name (must be unique)
* description
* display
* version - (default 2)
* online_help - not required, URL to online help
* proposal_schema_version (default 2)
* layout - version of layout (default 2)
* order - display order in UI (default 0)
* run_order
* jig_order
* commit - automatically generated flag
* build_on - automatically generated flag
  
  