
actions :add, :remove

attribute :subnet, :kind_of => String, :name_attribute => true
attribute :broadcast, :kind_of => String
attribute :netmask, :kind_of => String
attribute :routers, :kind_of => Array, :default => []
attribute :options, :kind_of => Array, :default => []

