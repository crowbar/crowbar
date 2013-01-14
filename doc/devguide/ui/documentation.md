### Adding Documentation

Crowbar uses a composite documentation system that allows each barclamp to add documentation specific to its function while still building a single comprehensive documentation set.

> _Tip:_ Use http://www.ctrlshift.net/project/markdowneditor/ to WYSIWYG edit markdown!

This information is available as a video! see http://youtu.be/eWHeEWiOEvo

#### Composite Documentation

It is vital to understand that the Crowbar documentation system is _composite documentation._  That means that the information is assembled from multiple barclamps on the fly.  This is required because the Crowbar framework is really a collection of barclamps and each barclamp has it's own capabilities and features.

The design of the documentation system allows each barclamp to contribute parts to the overall whole _and also_ allows parts to cross reference each other.

For example, each barclamp is expected to contribute "barclamp" and "license" information.  These pages only refer to the individual barclamp's data; however, they are rolled up under the barclamp and license sections of the documentation.  For Crowbar suite barclamps, they are further grouped under the master Crowbar set.  That means that the Deployer license information depends on the Crowbar Meta information.

While this adds complexity for the documentation author, it greatly simplify the documentation reading experience for the user.  It also allows developers to isolate documentation changes.

#### Table of Contents - Directory Tree Layout

By design, the table of contents generally follows the directory structure of the documentation.  This is intentional because it simplifies composition.

Each subdirectory should be paired with a matching topic document that functions as the index for the items in the subdirectory.

For example, 

    devguide.md
    devguide/
        api.md
        api/
            node.md
            group.md
        testing.md
        testing/

In the above example, the `devguide` topic layout out general information for the developer guide.  The `api` and `testing` sections would be shown as sections of the Developer Guide.  Individual API topics `node` and `group` are subsections of the API topic.

> The convention of having an the index topic name (`api.md`) match the subfolder (`api/`) makes it easier for Crowbar to assemble to table of contents without extra meta data.

If no hints (see index below) are given, Crowbar will automatically scan the subdirectory tree and build the table of contents based on the file path naming convention.

#### Index
There is a documentation tree index (`[barclamp].yml`) for each barclamp.  The index is a yml file which builds the documentation tree by deep merging all the barclamp indexes together.

> The index _must_ be named the same as the barclamp.  So each barclamp provides "barclamp.yml" in the `doc` directory.  These files are merged together during the barclamp install. 

The index has an entry for each topic page that follows the following pattern: `barclamp/topic`.  The `/` is required!

> You can comment out a page from being automatically index by prefixing it's name w/ `#`

It is acceptable for a barclamp to reference topics in another barclamp so that the correct parent topics are used to build an integrated set.

The index file should be nested so that topics have correct parents.

> It is strongly encouraged (but not required) to keep the index path the same as the file path.

##### Manual Index

With the addition of automatic indexing, the number of pages manually indexed has dropped dramatically.  Manual indexing is still required if you want to control the order of page rendering or add additional meta data.

Each level of the index can have meta data overrides, the meta data values are:

* order - defaults to alpha if omitted.  Range is 0 to 999999, default is 9999
* author - who edited this document last
* license - the license the document is distributed under
* date - last edit date
* copyright - copyright requirements for the document
* url - link to part of Crowbar this information relates to

An example index file looks like this:

    # these are the default meta_data values
    license: Apache 2
    copyright: 2012 by Dell, Inc
    author: Dell CloudEdge Team
    date: July 29, 2012
    crowbar/userguide:
      order: 1000
      crowbar/ui-general:
        order: 10
        - crowbar+ui-nodes
      crowbar/ui-utilities:
        order: 100
        url: '/utils'
        crowbar/utils-export:
          url: '/utils/index'
          

#### Topic Documentation

Markdown is the current format.

Path is `doc/default/barclamp/file.md`

It is allowable (recommended!) to use additional subdirectories!  Simply add another `/` to the same to indicate another level of depth on the file path.

> The naming of topics will match the file paths!

The name of the topic is expected to start with a # title flag.  This title is used by the generator to create the displayed title.

It is recommended to use # for top level (book), ## for second level (section), ### for third level (topic) and so on.
