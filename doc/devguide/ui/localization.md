### Adding Localizations (i18n)

Crowbar uses Rails I18N library.  Please refer to the documentation (http://http://guides.rubyonrails.org/i18n.html) for usage hints that can help you reduce coding and add nifty features like Interpolation.

Each barclamp is expected to add its own localization (i18n) file.  Please do _not_ add your localizations into another barclamps i18n file, it’s not friendly!  However, you also need to be careful not to create duplicate entries.  That’s just too confusing for Crowbar and makes the bunny angry.

Add your localization file (`en.yml` is the default) into the `crowbar_framework/config/locales/[barclamp]` directory.  You know this but I’ve got to tell the n00bs: you need to replace [barclamp] with the name of your barclamp.

If you are supporting multiple languages, replace `en` with the target language code.  Like `kl.yml` if you want provide Klingon translations.

Inside the i18n file, you’ll provide a simple YML hash for translations.
    en:
      # Layout
      nav:
        nodes: Nodes
        nodes_description: Infrastructure Components

Reminder: encode your translations in quotes if you need to use : or ‘ marks!

#### Crowbar 1.0 note
We no longer support storing localization strings in the crowbar.yml meta data file.  This was not scaling so we dropped it like a rotten tomato in the Heinz ketchup factory. 
