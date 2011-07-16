require 'json'

Then /^the "([^"]*)" entry should be at least "([^"]*)"$/ do |key, value|
  result = JSON.parse(page.body)
  assert result[key] >= value.to_i
end

Then /^the "([^"]*)":"([^"]*)" entry should be "([^"]*)"$/ do |key1, key2, value|
  result = JSON.parse(page.body)
  result[key1][key2].should == value
end
