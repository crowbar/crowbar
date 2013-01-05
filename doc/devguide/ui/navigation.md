### Adding Menu Items

You add menu items into Crowbar using database migrations that insert into the `navs` table using the `Nav` object.

You must add the migration to the `crowbar_framework/db/migrate` directory and follow the Rails migration naming convention of `YYYYMMDDHHMMSS_barclamp_navs.rb`.

Inside the migration, use the `Nav.find_or_create_by_item` to populate the information for the menu item:

*	item = the id of the item
*	parent_item = the id of the top level menu you want to use (`root` creates a top level menu)
*	name = the i18n path to the menu text
*	description = the i18n path to the menu hover information
*	path = the Rails path you want to follow.  Unless it starts with http, `eval` will be applied to the path.
*	order = the display order of the menu item

Remember:

*	to provide a `self.down` that removes your menu item!  It’s just more sanitary that way.
*	to create matching entries in your barclamps i18n files

#### Example from the Network barclamp:
    class NetworkNavs < ActiveRecord::Migration
      def self.up
        Nav.find_or_create_by_item :item=>'switches', :parent_item=>'network', :name=>'nav.switch', :description=>'nav.switch_description', :path=>"switch_path", :order=>500
      end
    
      def self.down
        Nav.delete_by_item 'switches'
        Nav.delete_by_item 'vlan'
      end
    end

#### Crowbar 1.0 note

We no longer support storing navigation strings in the crowbar.yml meta data file.  This approach made difficult to upgrade and maintain.  The new approach is also difficult to maintain but easier to upgrade.
