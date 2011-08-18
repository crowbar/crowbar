ENV["RAILS_ENV"] ||= ENV["RACK_ENV"]
require "#{::File.expand_path('config/environment')}"
use Rails::Rack::Static
run ActionController::Dispatcher.new
