
actions :add, :remove

attribute :name, :kind_of => String, :name_attribute => true
attribute :hostname, :kind_of => String
attribute :macaddress, :kind_of => String
attribute :ipaddress, :kind_of => String
attribute :group, :kind_of => String
attribute :options, :kind_of => Array, :default => []

