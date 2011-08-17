require 'json'

Given /^there is a node called "([^"]*)"$/ do |name|
  visit path_to("update node \"#{name}\" to \"TESTING\"")
end

When /^a node registers with "([^"]*)"$/ do |name|
  visit path_to("update node \"#{name}\" to \"TESTING\"")
end

When /^I update node "([^"]*)" to "([^"]*)"$/ do |name, state|
  visit path_to("update node \"#{name}\" to \"#{state}\"")
end

Then /^the "([^"]*)" state should be "([^"]*)"$/ do |name, state|
  visit path_to("node list page")
  result = JSON.parse(page.body)
  result["nodes"][name].should == state
end
