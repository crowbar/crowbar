# Interact with Chef #

First you must sync local date and time with that on the target system. Especially if you run in a development environment and did not do so on installation (chef protects agains replay attacks and needs a correct time setting) 

Get your local time 

    echo \'date -us \"`date -u "+%Y-%m-%d %H:%M"`\"\' 
    'date -us "2012-10-10 18:07"'

and set it on all nodes (from the crowbar admin machine as root)

    knife ssh node:* -- 'date -us "2012-10-10 18:07"'

Next copy your `/etc/chef/webui.pem` file from the admin machine to `.chef/webui.pem`

Copy the template knife.rb file and set the server url to the correct value

    cp .chef/knife.rb.example .chef/knife.rb 

copy the template `.rvmrc` in place and trust it (or install the corresponding ruby)

    cp dot-rvmrc .rvmrc
    rvm rvmrc trust .
    rvm rvmrc load

install chef with the Gemfile 

    bundle install

finally you can work with knife from your local maschine:

    $ knife status
    6 minutes ago, admin.v1.cr0wbar.de, admin.v1.cr0wbar.de, 10.124.0.10, ubuntu 12.04.
    4 minutes ago, dc0-ff-ee-00-00-02.v1.cr0wbar.de, dc0-ff-ee-00-00-02.v1.cr0wbar.de, 10.124.3.2, ubuntu 12.04.
    4 minutes ago, dc0-ff-ee-00-00-01.v1.cr0wbar.de, dc0-ff-ee-00-00-01.v1.cr0wbar.de, 10.124.3.3, ubuntu 12.04.
    3 minutes ago, dc0-ff-ee-00-00-03.v1.cr0wbar.de, dc0-ff-ee-00-00-03.v1.cr0wbar.de, 10.124.3.1, ubuntu 12.04. 